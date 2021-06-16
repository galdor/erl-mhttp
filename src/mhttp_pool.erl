%% Copyright (c) 2020-2021 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(mhttp_pool).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([process_name/1, start_link/2, stop/1,
         send_request/2, send_request/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([name/0, ref/0, options/0]).

-type name() :: et_gen_server:name().
-type ref() :: et_gen_server:ref().

-type options() ::
        #{client_options => mhttp_client:options(),
          max_connections_per_key => pos_integer(),
          use_netrc => boolean()}.

-type state() ::
        #{id := mhttp:pool_id(),
          options := options(),
          clients := #{pid() := client()},
          free_clients := #{mhttp:client_key() := [pid()]}}.

-type client() ::
        #{key := mhttp:client_key(),
          pid := pid(),
          acquisition_time => integer(), % millisecond timestamp
          request => mhttp:request(),
          request_context => request_context()}.

-type request_context() ::
        #{source := pid(),
          tag := term()}.

-spec process_name(mhttp:pool_id()) -> atom().
process_name(Id) ->
  Name = <<"mhttp_pool_", (atom_to_binary(Id))/binary>>,
  binary_to_atom(Name).

-spec start_link(mhttp:pool_id(), options()) -> et_gen_server:start_ret().
start_link(Id, Options) ->
  Name = process_name(Id),
  gen_server:start_link({local, Name}, ?MODULE, [Id, Options], []).

-spec stop(mhttp:pool_id()) -> ok.
stop(Id) ->
  Name = process_name(Id),
  gen_server:stop(Name).

-spec send_request(ref(), mhttp:request()) ->
        mhttp:result(mhttp:response_result()).
send_request(Ref, Request) ->
  send_request(Ref, Request, #{}).

-spec send_request(ref(), mhttp:request(), mhttp:request_options()) ->
        mhttp:result(mhttp:response_result()).
send_request(Ref, Request, Options) ->
  gen_server:call(Ref, {send_request, Request, Options}, infinity).

-spec init(list()) -> et_gen_server:init_ret(state()).
init([Id, Options]) ->
  logger:update_process_metadata(#{domain => [mhttp, pool, Id]}),
  process_flag(trap_exit, true),
  State = #{id => Id,
            options => Options,
            clients => #{},
            free_clients => #{}},
  {ok, State}.

-spec handle_call(term(), {pid(), et_gen_server:request_id()}, state()) ->
        et_gen_server:handle_call_ret(state()).
handle_call({send_request, Request, Options}, {Source, Tag}, State) ->
  try
    Context = #{source => Source, tag => Tag},
    {Result, State2} = handle_send_request(Request, Context, Options, State),
    {reply, {ok, Result}, State2}
  catch
    throw:{error, Reason} ->
      {reply, {error, Reason}, State};
    exit:{Reason, _Trace} ->
      %% Happens if the client failed (error or exit) during call processing
      {reply, {error, Reason}, State}
  end;
handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

-spec handle_cast(term(), state()) -> et_gen_server:handle_cast_ret(state()).
handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

-spec handle_info(term(), state()) -> et_gen_server:handle_info_ret(state()).
handle_info({'EXIT', Pid, normal}, State) ->
  {noreply, delete_client(Pid, State)};
handle_info({'EXIT', Pid, Reason}, State) ->
  ?LOG_ERROR("client ~p exited:~n~tp", [Pid, Reason]),
  {noreply, delete_client(Pid, State)};
handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec handle_send_request(mhttp:request(), request_context(),
                          mhttp:request_options(), state()) ->
        {mhttp:response_result(), state()}.
handle_send_request(Request, Context, Options, State) ->
  MaxNbRedirections = maps:get(max_nb_redirections, Options, 5),
  send_request_1(Request, Context, Options, MaxNbRedirections, State).

-spec send_request_1(mhttp:request(), request_context(),
                     mhttp:request_options(),
                     NbRedirectionsLeft :: non_neg_integer(), state()) ->
        {mhttp:response_result(), state()}.
send_request_1(_Request, _Context, _Options, 0, _State) ->
  throw({error, too_many_redirections});
send_request_1(Request, Context, Options, NbRedirectionsLeft, State) ->
  PreparedRequest = prepare_request(Request, State),
  {ClientPid, State2} = get_or_create_client(PreparedRequest, Context, State),
  %% The final request is just the prepared request with the normalized target
  %% (i.e. the path, query and fragment).
  FinalRequest = finalize_request(PreparedRequest),
  case mhttp_client:send_request(ClientPid, FinalRequest, Options) of
    {ok, Response} when is_map(Response) ->
      State3 = release_client(ClientPid, State2),
      case redirection_uri(Response, Options) of
        undefined ->
          {Response, State3};
        URI ->
          %% Redirection requires the full target
          NextRequest = mhttp_request:redirect(PreparedRequest, Response, URI),
          send_request_1(NextRequest, Context, Options, NbRedirectionsLeft-1,
                         State3)
      end;
    {ok, {upgraded, Response, Pid}} ->
      %% Clients stop after returning an {upgraded, _, _} response, so we are
      %% going to receive an EXIT signal and remove the process from the busy
      %% pool.
      {{upgraded, Response, Pid}, State2};
    {error, Reason} ->
      throw({error, Reason})
  end.

-spec redirection_uri(mhttp:response(), mhttp:request_options()) ->
        uri:uri() | undefined.
redirection_uri(Response, Options) ->
  case maps:get(follow_redirections, Options, true) of
    true ->
      case mhttp_response:is_redirection(Response) of
        {true, URI} ->
          URI;
        false ->
          undefined;
        {error, Reason} ->
          throw({error, Reason})
      end;
    false ->
      undefined
  end.

-spec get_or_create_client(mhttp:request(), request_context(), state()) ->
        {pid(), state()}.
get_or_create_client(Request, Context,
                     State = #{options := Options,
                               clients := Clients,
                               free_clients := FreeClients}) ->
  Key = request_key(Request),
  case maps:find(Key, FreeClients) of
    {ok, Pids} when Pids =/= [] ->
      N = rand:uniform(length(Pids)),
      {Before, [Pid | After]} = lists:split(N-1, Pids),
      Pids2 = Before ++ After,
      ?LOG_DEBUG("acquiring client ~p for ~p", [Pid, Key]),
      Client = maps:get(Pid, Clients),
      Client2 = Client#{acquisition_time => os:system_time(millisecond),
                        request => Request,
                        request_context => Context},
      {Pid, State#{clients => Clients#{Pid => Client2},
                   free_clients => FreeClients#{Key => Pids2}}};
    _ ->
      _MaxConns = maps:get(max_connections_per_key, Options, 1),
      %% TODO Respect MaxConns
      Pid = create_client(Key, State),
      ?LOG_DEBUG("adding new client ~p for ~p", [Pid, Key]),
      Client = #{key => Key,
                 pid => Pid,
                 acquisition_time => os:system_time(millisecond),
                 request => Request,
                 request_context => Context},
      {Pid, State#{clients => Clients#{Pid => Client}}}
  end.

-spec create_client(mhttp:client_key(), state()) -> pid().
create_client({Host, Port, Transport}, #{id := Id, options := Options}) ->
  CACertificateBundlePath =
    persistent_term:get(mhttp_ca_certificate_bundle_path),
  ClientOptions0 = maps:merge(#{ca_certificate_bundle_path =>
                                  CACertificateBundlePath},
                              maps:get(client_options, Options, #{})),
  ClientOptions = ClientOptions0#{host => Host,
                                  port => Port,
                                  transport => Transport},
  case mhttp_client:start_link(Id, ClientOptions) of
    {ok, Pid} ->
      Pid;
    {error, Reason} ->
      throw({error, Reason})
  end.

-spec release_client(pid(), state()) -> state().
release_client(Pid, State = #{clients := Clients,
                              free_clients := FreeClients}) ->
  (Client = #{key := Key}) = maps:get(Pid, Clients),
  Pids = maps:get(Key, FreeClients, []),
  Client2 = maps:without([acquisition_time, request, request_context], Client),
  State#{clients => Clients#{Pid => Client2},
         free_clients => FreeClients#{Key => [Pid | Pids]}}.

-spec delete_client(pid(), state()) -> state().
delete_client(Pid, State = #{clients := Clients,
                             free_clients := FreeClients}) ->
  case maps:find(Pid, Clients) of
    {ok, #{key := Key}} ->
      Pids = maps:get(Key, FreeClients),
      State#{clients => maps:remove(Pid, Clients),
             free_clients => FreeClients#{Key => lists:delete(Pid, Pids)}};
    error ->
      State
  end.

-spec request_key(mhttp:request()) -> mhttp:client_key().
request_key(Request) ->
  Target = mhttp_request:target_uri(Request),
  Host = mhttp_uri:host(Target),
  Port = mhttp_uri:port(Target),
  Transport = mhttp_uri:transport(Target),
  {Host, Port, Transport}.

-spec prepare_request(mhttp:request(), state()) -> mhttp:request().
prepare_request(Request0, State) ->
  Funs = [fun canonicalize_target/2,
          fun maybe_apply_netrc_entry/2],
  lists:foldl(fun (F, Request) ->
                  F(Request, State)
              end, Request0, Funs).

-spec canonicalize_target(mhttp:request(), state()) -> mhttp:request().
canonicalize_target(Request, _State) ->
  %% Make sure the target is an URI map with a scheme and host (the host is
  %% required to know which server connect to, the scheme is required for URI
  %% reference resolution in redirections).
  case mhttp_request:canonicalize_target(Request) of
    {ok, Request2} ->
      Request2;
    {error, Reason} ->
      throw({error, Reason})
  end.

-spec maybe_apply_netrc_entry(mhttp:request(), state()) -> mhttp:request().
maybe_apply_netrc_entry(Request, State) ->
  case netrc_entry(Request, State) of
    {ok, Entry} ->
      apply_netrc_entry(Request, Entry);
    error ->
      Request
  end.

-spec apply_netrc_entry(mhttp:request(), netrc:entry()) ->
        mhttp:request().
apply_netrc_entry(Request0, Entry) ->
  Funs = [fun maybe_apply_netrc_entry_port/2,
          fun maybe_apply_netrc_entry_credentials/2],
  lists:foldl(fun (F, Request) ->
                  F(Request, Entry)
              end, Request0, Funs).

-spec maybe_apply_netrc_entry_port(mhttp:request(), netrc:entry()) ->
        mhttp:request().
maybe_apply_netrc_entry_port(Request, Entry) ->
  case mhttp_request:target_uri(Request) of
    #{port := _} ->
      %% Do not override an explicit port in the target URI
      Request;
    Target ->
      case Entry of
        #{port := Port} when is_integer(Port) ->
          Request#{target => Target#{port => Port}};
        #{port := Port} when is_binary(Port) ->
          case string:to_lower(binary_to_list(Port)) of
            "http" ->
              Request#{target => Target#{port => 80}};
            "https" ->
              Request#{target => Target#{port => 443}};
            _ ->
              ?LOG_WARNING("unknown port '~ts' for machine ~ts in netrc file",
                           [Port, mhttp_uri:host(Target)]),
              Request
          end;
        _ ->
          Request
      end
  end.

-spec maybe_apply_netrc_entry_credentials(mhttp:request(), netrc:entry()) ->
        mhttp:request().
maybe_apply_netrc_entry_credentials(Request, #{login := Login,
                                               password := Password}) ->
  Header = mhttp_request:header(Request),
  Header2 = mhttp_header:add_basic_authorization(Header, Login, Password),
  Request#{header => Header2}.

-spec finalize_request(mhttp:request()) -> mhttp:request().
finalize_request(Request) ->
  Target = mhttp_request:target_uri(Request),
  Target2 = maps:without([scheme, userinfo, host, port], Target),
  Request#{target => mhttp_uri:path(Target2)}.

-spec netrc_entry(mhttp:request(), state()) -> {ok, netrc:entry()} | error.
netrc_entry(Request, #{options := Options}) ->
  case maps:get(use_netrc, Options, false) of
    true ->
      Target = mhttp_request:target_uri(Request),
      Host = mhttp_uri:host(Target),
      case mhttp_netrc:lookup(Host) of
        {ok, Entry} ->
          {ok, Entry};
        error ->
          error
      end;
    false ->
      error
  end.
