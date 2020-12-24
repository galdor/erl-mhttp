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

-export([process_name/1, start_link/2, stop/1,
         send_request/2, send_request/3]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([name/0, ref/0, options/0]).

-type name() :: et_gen_server:name().
-type ref() :: et_gen_server:ref().

-type options() :: #{client_options => mhttp_client:options(),
                     max_connections_per_key => pos_integer()}.

-type state() :: #{id := mhttp:pool_id(),
                   options := options(),
                   clients_by_key := ets:tid(),
                   clients_by_pid := ets:tid()}.

-spec process_name(mhttp:pool_id()) -> atom().
process_name(Id) ->
  Name = <<"mhttp_pool_", (atom_to_binary(Id))/binary>>,
  binary_to_atom(Name).

-spec start_link(mhttp:pool_id(), options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Id, Options) ->
  Name = process_name(Id),
  gen_server:start_link({local, Name}, ?MODULE, [Id, Options], []).

-spec stop(mhttp:pool_id()) -> ok.
stop(Id) ->
  Name = process_name(Id),
  gen_server:stop(Name).

-spec send_request(ref(), mhttp:request()) ->
        {ok, mhttp:response()} | {error, term()}.
send_request(Ref, Request) ->
  send_request(Ref, Request, #{}).

-spec send_request(ref(), mhttp:request(), mhttp:request_options()) ->
        {ok, mhttp:response()} | {error, term()}.
send_request(Ref, Request, Options) ->
  gen_server:call(Ref, {send_request, Request, Options}, infinity).

-spec init(list()) -> et_gen_server:init_ret(state()).

init([Id, Options]) ->
  logger:update_process_metadata(#{domain => [mhttp, pool, Id]}),
  process_flag(trap_exit, true),
  ClientsByKey = ets:new(ets_table_name(<<"clients_by_key">>, Id), [bag]),
  ClientsByPid = ets:new(ets_table_name(<<"clients_by_pid">>, Id), [set]),
  State = #{id => Id,
            options => Options,
            clients_by_key => ClientsByKey,
            clients_by_pid => ClientsByPid},
  {ok, State}.

-spec ets_table_name(Table :: binary(), mhttp:pool_id()) -> atom().
ets_table_name(Table, Id) ->
  Bin = <<"mhttp_pool_", (atom_to_binary(Id))/binary, "__", Table/binary>>,
  binary_to_atom(Bin).

-spec terminate(et_gen_server:terminate_reason(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec handle_call(term(), {pid(), et_gen_server:request_id()}, state()) ->
        et_gen_server:handle_call_ret(state()).

handle_call({send_request, Request0, Options}, _From, State) ->
  case mhttp_request:canonicalize_target(Request0) of
    {ok, Request} ->
      MaxNbRedirections = maps:get(max_nb_redirections, Options, 5),
      try
        {State, Response} = do_send_request(State, Request, Options,
                                            MaxNbRedirections),
        {reply, {ok, Response}, State}
      catch
        throw:{error, Reason} ->
          {reply, {error, Reason}, State};
        exit:{Reason, _MFA} ->
          {reply, {error, {client_error, Reason}}, State}
      end;
    {error, Reason} ->
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

handle_info({'EXIT', Pid, Reason}, State) ->
  ?LOG_DEBUG("client ~p exited (~p)", [Pid, Reason]),
  delete_client(State, Pid),
  {noreply, State};

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec get_or_create_client(state(), mhttp:client_key()) ->
        mhttp_client:ref().
get_or_create_client(State = #{options := Options,
                               clients_by_key := ClientsByKey,
                               clients_by_pid := ClientsByPid},
                     Key) ->
  MaxConns = maps:get(max_connections_per_key, Options, 1),
  case ets:lookup(ClientsByKey, Key) of
    Entries when length(Entries) < MaxConns ->
      Pid = create_client(State, Key),
      ets:insert(ClientsByKey, {Key, Pid}),
      ets:insert(ClientsByPid, {Pid, Key}),
      ?LOG_DEBUG("added new client ~p (~p)", [Key, Pid]),
      Pid;
    Entries ->
      {_, Pid} = lists:nth(rand:uniform(length(Entries)), Entries),
      Pid
  end.

-spec create_client(state(), mhttp:client_key()) -> mhttp_client:ref().
create_client(#{id := Id, options := Options}, {Host, Port, Transport}) ->
  ClientOptions0 = maps:get(client_options, Options, #{}),
  ClientOptions = ClientOptions0#{host => Host, port => Port,
                                  transport => Transport,
                                  pool => Id},
  case mhttp_client:start_link(ClientOptions) of
    {ok, Pid} ->
      Pid;
    {error, Reason} ->
      throw({error, {client_error, Reason}})
  end.

-spec delete_client(state(), pid()) -> ok.
delete_client(#{clients_by_key := ClientsByKey,
                clients_by_pid := ClientsByPid}, Pid) ->
  case ets:lookup(ClientsByPid, Pid) of
    [{Pid, Key}] ->
      ets:delete_object(ClientsByKey, {Key, Pid}),
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
  throw({error, too_many_redirections});
do_send_request(State, Request, Options, NbRedirectionsLeft) ->
  {Target, Key} = request_target_and_key(Request),
  Client = get_or_create_client(State, Key),
  Request2 = Request#{target => Target},
  case mhttp_client:send_request(Client, Request2, Options) of
    {ok, Response} ->
      case maps:get(follow_redirections, Options, true) of
        true ->
          case mhttp_response:is_redirection(Response) of
            {true, URI} ->
              Status = maps:get(status, Response),
              NextRequest = mhttp_request:redirect(Request, Status, URI),
              do_send_request(State, NextRequest, Options,
                              NbRedirectionsLeft-1);
            false ->
              {State, Response};
            {error, Reason} ->
              throw({error, Reason})
          end;
        false ->
          {State, Response}
      end;
    {error, Reason} ->
      throw({error, Reason})
  end.
