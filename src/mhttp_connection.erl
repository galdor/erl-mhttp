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

-type options() :: #{server_pid := pid(),
                     error_handler := mhttp:error_handler(),
                     idle_timeout := pos_integer(),
                     address => inet:address(),
                     port => inet:port_number()}.

-type state() :: #{options := options(),
                   socket => inet:socket(),
                   parser := mhttp_parser:parser(),
                   idle_timer => reference()}.

-spec start_link(options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).

init([Options]) ->
  logger:update_process_metadata(#{domain => [mhttp, connection]}),
  State = #{options => Options,
            parser => mhttp_parser:new(request)},
  {ok, State}.

terminate(Reason, State = #{socket := Socket}) ->
  gen_tcp:close(Socket),
  terminate(Reason, maps:remove(socket, State));
terminate(_Reason, _State) ->
  ok.

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {noreply, State}.

handle_cast({socket, Socket}, State) ->
  State2 = State#{socket => Socket},
  set_socket_active(State2, 1),
  {noreply, schedule_idle_timeout(State2)};

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

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
      {noreply, State#{parser => Parser2}}
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
process_request(Request, State) ->
  ?LOG_DEBUG("received request ~p", [Request]),
  Response0 = call_route(State, Request),
  Response = finalize_response(State, Response0),
  ?LOG_DEBUG("sending response ~p", [Response]),
  send_response(Response, State),
  State.

-spec call_route(state(), mhttp:request()) -> mhttp:response().
call_route(#{options := Options}, Request) ->
  ServerPid = maps:get(server_pid, Options),
  try
    Context = #{client_address => maps:get(address, Options),
                client_port => maps:get(port, Options)},
    {{_, Handler}, Context2} = mhttp_server:find_route(ServerPid, Request,
                                                       Context),
    Handler(Request, Context2)
  catch
    error:Reason:Trace ->
      ?LOG_ERROR("handler error ~p ~p", [Reason, Trace]),
      ErrHandler = maps:get(error_handler, Options),
      ErrHandler(Request, #{}, Reason, Trace);
    throw:{response, Response} ->
      Response
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
