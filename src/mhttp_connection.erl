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

-export([start_link/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-type state() :: #{socket => inet:socket(),
                   parser := mhttp_parser:parser()}.

-spec start_link() -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  logger:update_process_metadata(#{domain => [mhttp, connection]}),
  {ok, #{parser => mhttp_parser:new(request)}}.

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
  {noreply, State2};

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

handle_info({tcp, _Socket, Data}, State = #{parser := Parser}) ->
  case mhttp_parser:parse(Parser, Data) of
    {ok, Request, Parser2} ->
      State2 = process_request(Request, State#{parser => Parser2}),
      set_socket_active(State, 1),
      {noreply, State2};
    {more, Parser2} ->
      set_socket_active(State, 1),
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
  Response0 = #{status => 200}, % TODO request handling
  Response = finalize_response(State, Response0),
  ?LOG_DEBUG("sending response ~p", [Response]),
  send_response(Response, State),
  State.

-spec finalize_response(state(), mhttp:response()) -> mhttp:response().
finalize_response(_State, Response) ->
  Funs = [fun mhttp_response:ensure_reason/1,
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
