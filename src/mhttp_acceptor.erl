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

-spec init(list()) -> et_gen_server:init_ret(state()).
init([Options = #{socket := Socket}]) ->
  logger:update_process_metadata(#{domain => [mhttp, acceptor]}),
  process_flag(trap_exit, true),
  State = #{options => Options, socket => Socket},
  gen_server:cast(self(), accept),
  {ok, State}.

-spec terminate(et_gen_server:terminate_reason(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec handle_call(term(), {pid(), et_gen_server:request_id()}, state()) ->
        et_gen_server:handle_call_ret(state()).

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

-spec handle_cast(term(), state()) -> et_gen_server:handle_cast_ret(state()).

handle_cast(accept, State = #{socket := Socket}) ->
  case gen_tcp:accept(Socket, 5000) of
    {ok, ConnSocket} ->
      case inet:peername(ConnSocket) of
        {ok, {ConnAddress, ConnPort}} ->
          ?LOG_DEBUG("connection accepted from ~s:~b",
                     [inet:ntoa(ConnAddress), ConnPort]),
          spawn_connection(State, ConnSocket, ConnAddress, ConnPort),
          gen_server:cast(self(), accept),
          {noreply, State};
        {error, Reason} ->
          ?LOG_ERROR("cannot obtain peer address and port: ~p", Reason),
          gen_server:cast(self(), accept),
          {noreply, State}
      end;
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

-spec handle_info(term(), state()) -> et_gen_server:handle_info_ret(state()).

handle_info({'EXIT', _Pid, normal}, State) ->
  {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
  ?LOG_WARNING("connection ~p exited (~p)", [Pid, Reason]),
  {noreply, State};

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec spawn_connection(state(), inet:socket(),
                       inet:ip_address(), inet:port_number()) -> ok.
spawn_connection(#{options := Options}, Socket, Address, Port) ->
  ConnOptions0 = maps:get(connection_options, Options),
  ConnOptions = ConnOptions0#{address => Address, port => Port},
  {ok, ConnPid} = mhttp_connection:start_link(ConnOptions),
  gen_tcp:controlling_process(Socket, ConnPid),
  gen_server:cast(ConnPid, {socket, Socket}),
  ok.
