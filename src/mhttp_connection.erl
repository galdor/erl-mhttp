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

-module(mhttp_connection).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([options/0]).

%% XXX We need to keep track of the server id for request logging. Using
%% options to store the server id is a hack, we need a better way.
-type options() :: #{server_pid := pid(),
                     error_handler := mhttp:error_handler(),
                     idle_timeout := pos_integer(),
                     address => inet:ip_address(),
                     port => inet:port_number(),
                     log_requests => boolean(),
                     server => mhttp:server_id()}.

-type state() :: #{options := options(),
                   socket => inet:socket(),
                   parser := mhttp_parser:parser(),
                   idle_timer => reference()}.

-spec start_link(options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).

-spec init(list()) -> et_gen_server:init_ret(state()).
init([Options]) ->
  logger:update_process_metadata(#{domain => log_domain()}),
  State = #{options => Options,
            parser => mhttp_parser:new(request)},
  {ok, State}.

-spec terminate(et_gen_server:terminate_reason(), state()) -> ok.
terminate(Reason, State = #{socket := Socket}) ->
  gen_tcp:close(Socket),
  terminate(Reason, maps:remove(socket, State));
terminate(_Reason, _State) ->
  ok.

-spec handle_call(term(), {pid(), et_gen_server:request_id()}, state()) ->
        et_gen_server:handle_call_ret(state()).

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {noreply, State}.

-spec handle_cast(term(), state()) -> et_gen_server:handle_cast_ret(state()).

handle_cast({socket, Socket}, State) ->
  State2 = State#{socket => Socket},
  set_socket_active(State2, 1),
  {noreply, schedule_idle_timeout(State2)};

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

-spec handle_info(term(), state()) -> et_gen_server:handle_info_ret(state()).

handle_info(idle_timeout, State) ->
  ?LOG_INFO("connection idle, exiting"),
  {stop, normal, State};

handle_info({tcp, _Socket, Data}, State = #{parser := Parser}) ->
  State2 = schedule_idle_timeout(State),
  case mhttp_parser:parse(Parser, Data) of
    {ok, Request, Parser2} ->
      State3 = handle_request(Request, State#{parser => Parser2}),
      set_socket_active(State3, 1),
      {noreply, State3};
    {more, Parser2} ->
      set_socket_active(State2, 1),
      {noreply, State#{parser => Parser2}};
    {error, Reason} ->
      ?LOG_ERROR("invalid data: ~p", [Reason]),
      Response0 = #{status => 400},
      Response = finalize_response(State2, Response0),
      send_response(Response, State2),
      {stop, normal, State2}
  end;

handle_info({tcp_closed, _Socket}, State) ->
  ?LOG_DEBUG("connection closed"),
  {stop, normal, State};

handle_info({tcp_passive, _Socket}, State) ->
  {noreply, State};

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec handle_request(mhttp:request(), state()) -> state().
handle_request(Request, State = #{options := Options}) ->
  Now = erlang:system_time(microsecond),
  RequestId = case mhttp_request:request_id(Request) of
                {ok, Id} -> Id;
                error -> ksuid:generate_string()
              end,
  Context = #{client_address => maps:get(address, Options),
              client_port => maps:get(port, Options),
              start_time => Now,
              request_id => RequestId},
  {Response0, Context2} =
    try
      find_and_call_route(State, Request, Context)
    catch
      error:Reason:Trace ->
        ?LOG_ERROR("request processing error: ~p~n~p", [Reason, Trace]),
        ErrHandler = maps:get(error_handler, Options),
        call_error_handler(ErrHandler, Request, Context, Reason, Trace)
    end,
  Response = finalize_response(State, Response0),
  log_request(Request, Response, Context2, State),
  send_response(Response, State),
  State.

-spec find_and_call_route(state(), mhttp:request(), mhttp:handler_context()) ->
        {mhttp:response(), mhttp:handler_context()}.
find_and_call_route(#{options := Options}, Request, Context) ->
  ServerPid = maps:get(server_pid, Options),
  case mhttp_server:find_route(ServerPid, Request, Context) of
    {ok, {_Router, Route, Context2}} ->
      try
        call_route(Request, Context2, Route)
      catch
        error:Reason:Trace ->
          ?LOG_ERROR("request handling error: ~p~n~p", [Reason, Trace]),
          ErrHandler = maps:get(error_handler, Options),
          call_error_handler(ErrHandler, Request, Context, Reason, Trace)
      end;
    {error, Reason} ->
      throw({error, Reason})
  end.

-spec call_route(mhttp:request(), mhttp:handler_context(), mhttp:route()) ->
        {mhttp:response(), mhttp:handler_context()}.
call_route(Request, Context, {_, Handler}) ->
  call_handler(Handler, Request, Context).

-spec call_handler(mhttp:handler(), mhttp:request(), mhttp:handler_context()) ->
        {mhttp:response(), mhttp:handler_context()}.
call_handler(Handler, Request, Context) ->
  try
    case Handler(Request, Context) of
      {Response2, Context2} ->
        {Response2, Context2};
      Response2 ->
        {Response2, Context}
    end
  catch
    throw:{response, ThrownResponse, ThrownContext} ->
      {ThrownResponse, ThrownContext};
    throw:{response, ThrownResponse} ->
      {ThrownResponse, Context}
  end.

-spec call_error_handler(mhttp:error_handler(), mhttp:request(),
                         mhttp:handler_context(), Reason :: term(),
                         [et_erlang:stack_item()]) ->
        {mhttp:response(), mhttp:handler_context()}.
call_error_handler(Handler, Request, Context, Reason, Trace) ->
  case Handler(Request, Context, Reason, Trace) of
    {Response2, Context2} ->
      {Response2, Context2};
    Response2 ->
      {Response2, Context}
  end.

-spec finalize_response(state(), mhttp:response()) -> mhttp:response().
finalize_response(_State, Response) ->
  Funs = [fun mhttp_response:ensure_reason/1,
          fun mhttp_response:ensure_date/1,
          fun mhttp_response:maybe_add_content_length/1],
  lists:foldl(fun (Fun, R) -> Fun(R) end, Response, Funs).

-spec send_response(mhttp:response(), state()) -> ok.
send_response(Response, #{socket := Socket}) ->
  Data = mhttp_proto:encode_response(Response),
  ok = gen_tcp:send(Socket, Data),
  ok.

-spec set_socket_active(state(), boolean() | pos_integer()) -> ok.
set_socket_active(#{socket := Socket}, Active) ->
  ok = inet:setopts(Socket, [{active, Active}]),
  ok.

-spec schedule_idle_timeout(state()) -> state().
schedule_idle_timeout(State = #{options := Options}) ->
  case maps:find(idle_timer, State) of
    {ok, T} ->
      erlang:cancel_timer(T);
    error ->
      ok
  end,
  Timeout = maps:get(idle_timeout, Options),
  Timer = erlang:send_after(Timeout, self(), idle_timeout),
  State#{idle_timer => Timer}.

-spec log_request(mhttp:request(), mhttp:response(), mhttp:handler_context(),
                  state()) -> ok.
log_request(Request, Response, Context, #{options := Options}) ->
  case maps:get(log_requests, Options, true) of
    true ->
      Server = maps:get(server, Options, undefined),
      mhttp_log:log_incoming_request(Request, Response, Context, Server,
                                     log_domain()),
      ok;
    false ->
      ok
  end.

-spec log_domain() -> [atom()].
log_domain() ->
  [mhttp, connection].
