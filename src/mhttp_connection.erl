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

%% -type state() :: #{socket => inet:socket()}.

-spec start_link() -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  logger:update_process_metadata(#{domain => [mhttp, connection]}),
  {ok, #{}}.

terminate(Reason, State = #{socket := Socket}) ->
  gen_tcp:close(Socket),
  terminate(Reason, maps:remove(socket, State));
terminate(_Reason, _State) ->
  ok.

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {noreply, State}.

handle_cast({socket, Socket}, State) ->
  %% TODO
  {noreply, State#{socket => Socket}};

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.
