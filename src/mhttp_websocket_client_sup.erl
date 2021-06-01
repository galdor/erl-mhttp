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

-module(mhttp_websocket_client_sup).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/1, start_client/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([ref/0, options/0]).

-type ref() :: et_gen_server:ref().

-type options() :: #{}.

-type state() :: #{options := options()}.

-spec start_link(options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Options) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).

-spec start_client(mhttp_websocket_client:options()) -> Result when
    Result :: {ok, pid()} | {error, term()}.
start_client(Options) ->
  gen_server:call(?MODULE, {start_client, Options}, infinity).

-spec init(list()) -> et_gen_server:init_ret(state()).
init([Options]) ->
  logger:update_process_metadata(#{domain => [mhttp, websocket_client_sup]}),
  process_flag(trap_exit, true),
  State = #{options => Options},
  {ok, State}.

-spec terminate(et_gen_server:terminate_reason(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec handle_call(term(), {pid(), et_gen_server:request_id()}, state()) ->
        et_gen_server:handle_call_ret(state()).
handle_call({start_client, Options}, _From, State) ->
  case mhttp_websocket_client:start_link(Options) of
    {ok, Pid} ->
      ?LOG_DEBUG("client ~p started", [Pid]),
      {reply, {ok, Pid}, State};
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
handle_info({'EXIT', _Pid, normal}, State) ->
  {noreply, State};
handle_info({'EXIT', Pid, Reason}, State) ->
  ?LOG_ERROR("client ~p exited:~n~tp", [Pid, Reason]),
  {noreply, State};
handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.
