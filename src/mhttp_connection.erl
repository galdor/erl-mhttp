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
  logger:update_process_metadata(#{domain => [mhttp, connection]}),
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
      State3 = process_request(Request, State#{parser => Parser2}),
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

-spec process_request(mhttp:request(), state()) -> state().
process_request(Request, State = #{options := Options}) ->
  Now = erlang:system_time(microsecond),
  Context = #{client_address => maps:get(address, Options),
              client_port => maps:get(port, Options),
              start_time => Now},
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
    {ok, {Router, {_, Handler}, Context2}} ->
      Middlewares = maps:get(middlewares, Router, []),
      try
        call_route(Request, Context2, Handler, Middlewares)
      catch
        error:Reason:Trace ->
          ?LOG_ERROR("request handling error: ~p~n~p", [Reason, Trace]),
          ErrHandler = maps:get(error_handler, Options),
          call_error_handler(ErrHandler, Request, Context, Reason, Trace);
        throw:{response, ThrownResponse, ThrownContext} ->
          {ThrownResponse, ThrownContext};
        throw:{response, ThrownResponse} ->
          {ThrownResponse, Context2}
      end;
    {error, Reason} ->
      throw({error, Reason})
  end.

-spec call_route(mhttp:request(), mhttp:handler_context(), mhttp:handler(),
                 [mhttp:middleware()]) ->
        {mhttp:response(), mhttp:handler_context()}.
call_route(Request, Context, Handler, Middlewares) ->
  %% Preprocessing middlewares
  PreMiddlewares = mhttp_middleware:preprocessing_middlewares(Middlewares),
  {PreprocessedRequest, PreprocessedContext} =
    lists:foldl(fun (M, {Req, Ctx}) ->
                    call_preprocessing_middleware(M, Req, Ctx)
                end, {Request, Context}, PreMiddlewares),
  %% Handler
  {Response, HandledContext} =
    call_handler(Handler, PreprocessedRequest, PreprocessedContext),
  %% Postprocessing middlewares
  PostMiddlewares = mhttp_middleware:postprocessing_middlewares(Middlewares),
  {PostprocessedResponse, PostprocessedContext} =
    lists:foldl(fun (M, {Res, Ctx}) ->
                    call_postprocessing_middleware(M, PreprocessedRequest,
                                                   Res, Ctx)
                end, {Response, HandledContext},
                PostMiddlewares),
  {validate_route_response(PostprocessedResponse), PostprocessedContext}.

-spec call_handler(mhttp:handler(), mhttp:request(), mhttp:handler_context()) ->
        {mhttp:response(), mhttp:handler_context()}.
call_handler(Handler, Request, Context) ->
  case Handler(Request, Context) of
    {Response2, Context2} ->
      {Response2, Context2};
    Response2 ->
      {Response2, Context}
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

-spec call_preprocessing_middleware(mhttp:middleware(), mhttp:request(),
                                    mhttp:handler_context()) ->
        {mhttp:request(), mhttp:handler_context()}.
call_preprocessing_middleware({preprocess, Module, Args}, Request, Context) ->
  call_preprocessing_middleware({Module, Args}, Request, Context);
call_preprocessing_middleware({Module, Args}, Request, Context) ->
  case Module:preprocess(Request, Context, Args) of
    {Request2, Context2} ->
      {Request2, Context2};
    Request2 ->
      {Request2, Context}
  end.

-spec call_postprocessing_middleware(mhttp:middleware(),
                                     mhttp:request(), mhttp:response(),
                                     mhttp:handler_context()) ->
        {mhttp:response(), mhttp:handler_context()}.
call_postprocessing_middleware({postprocess, Module, Args},
                               Request, Response, Context) ->
  call_postprocessing_middleware({Module, Args}, Request, Response, Context);
call_postprocessing_middleware({Module, Args}, Request, Response, Context) ->
  case Module:postprocess(Request, Response, Context, Args) of
    {Response2, Context2} ->
      {Response2, Context2};
    Response2 ->
      {Response2, Context}
  end.

-spec validate_route_response(mhttp:response()) -> mhttp:response().
validate_route_response(#{body := Body}) when is_tuple(Body) ->
  %% Payloads have to be serialized by one of the postprocessing middlewares.
  error({invalid_response, unserialized_payload});
validate_route_response(Response) ->
  Response.

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
      StartTime = maps:get(start_time, Context),
      Server = maps:get(server, Options, undefined),
      Address = maps:get(client_address, Context),
      mhttp_log:log_incoming_request(Request, Response, StartTime,
                                     Server, Address),
      ok;
    false ->
      ok
  end.
