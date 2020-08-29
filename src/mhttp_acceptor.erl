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

-module(mhttp_acceptor).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([options/0]).

-type options() :: #{socket => inet:socket(),
                     connection_options := mhttp_connection:options()}.

-type state() :: #{options := options(),
                   socket => inet:socket()}.

-spec start_link(options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).

init([Options = #{socket := Socket}]) ->
  logger:update_process_metadata(#{domain => [mhttp, acceptor]}),
  process_flag(trap_exit, true),
  State = #{options => Options, socket => Socket},
  gen_server:cast(self(), accept),
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {noreply, State}.

handle_cast(accept, State = #{socket := Socket}) ->
  case gen_tcp:accept(Socket, 5000) of
    {ok, ConnSocket} ->
      spawn_connection(State, ConnSocket),
      gen_server:cast(self(), accept),
      {noreply, State};
    {error, timeout} ->
      gen_server:cast(self(), accept),
      {noreply, State};
    {error, closed} ->
      {noreply, State};
    {error, Reason} ->
      ?LOG_ERROR("cannot accept connection: ~p", [Reason]),
      exit({accept_failure, Reason})
  end;

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

handle_info({'EXIT', Pid, normal}, State) ->
  {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
  ?LOG_WARNING("connection ~p exited (~p)", [Pid, Reason]),
  {noreply, State};

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec spawn_connection(state(), inet:socket()) -> ok.
spawn_connection(#{options := Options}, Socket) ->
  ConnOptions = maps:get(connection_options, Options),
  {ok, ConnPid} = mhttp_connection:start_link(ConnOptions),
  gen_tcp:controlling_process(Socket, ConnPid),
  gen_server:cast(ConnPid, {socket, Socket}),
  ok.
