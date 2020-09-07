%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(mhttp_pool).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([process_name/1,
         start_link/1, start_link/2, stop/1,
         send_request/2, send_request/3]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([pool_name/0, pool_ref/0, options/0]).

-type pool_name() :: mhttp:gen_server_name().
-type pool_ref() :: mhttp:gen_server_ref().

-type options() :: #{client_options => mhttp_client:options()}.

-type state() :: #{options := options(),
                   clients_by_key := ets:tid(),
                   clients_by_pid := ets:tid()}.

-spec process_name(mhttp:pool_id()) -> atom().
process_name(Id) ->
  Name = <<"mhttp_pool_", (atom_to_binary(Id))/binary>>,
  binary_to_atom(Name).

-spec start_link(pool_name() | options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Options) when is_map(Options) ->
  gen_server:start_link(?MODULE, [Options], []);
start_link(Name) ->
  start_link(Name, #{}).

-spec start_link(pool_name(), options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Name, Options) ->
  gen_server:start_link(Name, ?MODULE, [Options], []).

-spec stop(pool_ref()) -> ok.
stop(Ref) ->
  gen_server:stop(Ref).

-spec send_request(pool_ref(), mhttp:request()) ->
        {ok, mhttp:response()} | {error, term()}.
send_request(Ref, Request) ->
  send_request(Ref, Request, #{}).

-spec send_request(pool_ref(), mhttp:request(),
                   mhttp:request_options()) ->
        {ok, mhttp:response()} | {error, term()}.
send_request(Ref, Request, Options) ->
  gen_server:call(Ref, {send_request, Request, Options}, infinity).

init([Options]) ->
  logger:update_process_metadata(#{domain => [mhttp, pool]}),
  process_flag(trap_exit, true),
  ClientsByKey = ets:new(mhttp_pool_clients_by_key, [set]),
  ClientsByPid = ets:new(mhttp_pool_clients_by_pid, [set]),
  State = #{options => Options,
            clients_by_key => ClientsByKey,
            clients_by_pid => ClientsByPid},
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

handle_call({get_client, Key}, _From, State) ->
  try
    Client = get_or_create_client(State, Key),
    {reply, {ok, Client}, State}
  catch
    error:Reason ->
      {reply, {error, Reason}, State};
    exit:{Reason, Call} ->
      {reply, {error, {client_failure, Reason, Call}}, State}
  end;

handle_call({send_request, Request, Options}, _From, State) ->
  MaxNbRedirections = maps:get(max_nb_redirections, Options, 5),
  try
    {State, Response} = do_send_request(State, Request, Options,
                                        MaxNbRedirections),
    {reply, {ok, Response}, State}
  catch
    error:Reason:Trace ->
      {reply, {error, {Reason, Trace}}, State};
    exit:{Reason, Call} ->
      {reply, {error, {client_failure, Reason, Call}}, State}
  end;

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {noreply, State}.

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
  ?LOG_DEBUG("client ~p exited (~p)", [Pid, Reason]),
  delete_client(State, Pid),
  {noreply, State};

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec get_or_create_client(state(), mhttp:client_key()) ->
        mhttp_client:client_ref().
get_or_create_client(State = #{clients_by_key := ClientsByKey,
                               clients_by_pid := ClientsByPid},
                     Key) ->
  case ets:lookup(ClientsByKey, Key) of
    [{_, Pid}] ->
      Pid;
    [] ->
      Pid = create_client(State, Key),
      ets:insert(ClientsByKey, {Key, Pid}),
      ets:insert(ClientsByPid, {Pid, Key}),
      ?LOG_DEBUG("added new client ~p (~p)", [Key, Pid]),
      Pid
  end.

-spec create_client(state(), mhttp:client_key()) ->
        mhttp_client:client_ref().
create_client(#{options := Options}, {Host, Port, Transport}) ->
  ClientOptions0 = maps:get(client_options, Options, #{}),
  ClientOptions = ClientOptions0#{host => Host, port => Port,
                                  transport => Transport},
  case mhttp_client:start_link(ClientOptions) of
    {ok, Pid} ->
      Pid;
    {error, Reason} ->
      error({client_failure, Reason})
  end.

-spec delete_client(state(), pid()) -> ok.
delete_client(#{clients_by_key := ClientsByKey,
                clients_by_pid := ClientsByPid}, Pid) ->
  %% Note that we can try to delete a client from tables after receiving an
  %% exit signal because the client crashed during its initialization (e.g. if
  %% the connection failed); in that case, the client will not be registered
  %% in tables.
  case ets:lookup(ClientsByPid, Pid) of
    [{_, Key}] ->
      ets:delete(ClientsByKey, Key),
      ets:delete(ClientsByPid, Pid),
      ok;
    [] ->
      ok
  end.

-spec request_target_and_key(mhttp:request()) ->
        {mhttp:target(), mhttp:client_key()}.
request_target_and_key(Request) ->
  Target = mhttp_request:target_uri(Request),
  Key = {mhttp_uri:host(Target), mhttp_uri:port(Target),
         mhttp_uri:transport(Target)},
  Target2 = maps:without([scheme, userinfo, host, port], Target),
  Target3 = Target2#{path => mhttp_uri:path(Target2)},
  {Target3, Key}.

-spec do_send_request(state(), mhttp:request(), mhttp:request_options(),
                      NbRedirectionsLeft :: non_neg_integer()) ->
        {state(), mhttp:response()}.
do_send_request(_State, _Request, _Options, 0) ->
  error(too_many_redirections);
do_send_request(State, Request, Options, NbRedirectionsLeft) ->
  {Target, Key} = request_target_and_key(Request),
  Client = get_or_create_client(State, Key),
  Request2 = Request#{target => Target},
  case mhttp_client:send_request(Client, Request2, Options) of
    {ok, Response} ->
      case maps:get(follow_redirections, Options, false) of
        true ->
          case mhttp_response:is_redirection(Response) of
            {true, URI} ->
              Status = maps:get(status, Response),
              NextRequest = mhttp_request:redirect(Request, Status, URI),
              do_send_request(State, NextRequest, Options,
                              NbRedirectionsLeft-1);
            false ->
              {State, Response}
          end;
        false ->
          {State, Response}
      end;
    {error, Reason} ->
      error(Reason)
  end.
