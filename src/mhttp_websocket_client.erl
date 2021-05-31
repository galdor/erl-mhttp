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

-module(mhttp_websocket_client).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([name/0, ref/0, options/0]).

-type name() :: et_gen_server:name().
-type ref() :: et_gen_server:ref().

-type options() :: #{}.

-type state() :: #{options := options(),
                   transport => mhttp:transport(),
                   socket => mhttp:socket(),
                   peer_address => inet:ip_address(),
                   peer_port => inet:port_number(),
                   parser => mhttp_websocket_parser:parser()}.

-spec start(options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start(Options) ->
  gen_server:start(?MODULE, [Options], []).

-spec init(list()) -> et_gen_server:init_ret(state()).
init([Options]) ->
  logger:update_process_metadata(#{domain => [mhttp, websocket_client]}),
  State = #{options => Options},
  {ok, State}.

-spec terminate(et_gen_server:terminate_reason(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec handle_call(term(), {pid(), et_gen_server:request_id()}, state()) ->
        et_gen_server:handle_call_ret(state()).
handle_call({activate, Socket, Transport, SocketData}, _From, State) ->
  try
    {Address, Port} = peername(Socket, Transport),
    ?LOG_DEBUG("connected to ~s:~b", [inet:ntoa(Address), Port]),
    Parser0 = mhttp_websocket_parser:new(),
    Parser = mhttp_websocket_parser:append_data(Parser0, SocketData),
    State2 = State#{transport => Transport,
                    socket => Socket,
                    peer_address => Address,
                    peer_port => Port,
                    parser => Parser},
    State3 = process_data(State2),
    set_socket_active(State3, true),
    send_message({ping, <<"">>}, State3), % XXX test
    {reply, ok, State3}
  catch
    throw:{error, Reason} ->
      {stop, {error, Reason}, {error, Reason}, State}
  end;
handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

-spec handle_cast(term(), state()) -> et_gen_server:handle_cast_ret(state()).
handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

-spec handle_info(term(), state()) -> et_gen_server:handle_info_ret(state()).
handle_info({tcp, _Port, Data}, State = #{parser := Parser}) ->
  State2 = State#{parser => mhttp_websocket_parser:append_data(Parser, Data)},
  try
    {noreply, process_data(State2)}
  catch
    throw:{error, Reason} ->
      ?LOG_ERROR("invalid data: ~tp", [Reason]),
      {stop, {error, Reason}, State}
  end;
handle_info({tcp_closed, _Port}, State) ->
  ?LOG_DEBUG("connection closed"),
  {stop, normal, State};
handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec send_message(mhttp_websocket:message(), state()) -> ok.
send_message(Message, State) ->
  send(mhttp_websocket:serialize(Message), State).

-spec process_data(state()) -> state().
process_data(State = #{parser := Parser}) ->
  case mhttp_websocket_parser:parse(Parser) of
    {ok, Message, Parser2} ->
      ?LOG_DEBUG("message: ~tp", [Message]),
      process_data(State#{parser => Parser2});
    {more, Parser2} ->
      State#{parser => Parser2};
    {error, Reason} ->
      throw({error, {invalid_data, Reason}})
  end.

-spec peername(mhttp:socket(), mhttp:transport()) ->
        {inet:ip_address(), inet:port_number()}.
peername(Socket, Transport) ->
  Peername = case Transport of
               tcp -> fun inet:peername/1;
               tls -> fun ssl:peername/1
             end,
  case Peername(Socket) of
    {ok, {Address, Port}} ->
      {Address, Port};
    {error, Reason} ->
      throw({error, {peername, Reason}})
  end.

-spec set_socket_active(state(), boolean() | pos_integer()) -> ok.
set_socket_active(#{transport := Transport, socket := Socket}, Active) ->
  Setopts = case Transport of
              tcp -> fun inet:setopts/2;
              tls -> fun ssl:setopts/2
            end,
  case Setopts(Socket, [{active, Active}]) of
    ok ->
      ok;
    {error, closed} ->
      throw({error, connection_closed});
    {error, Reason} ->
      throw({error, {setopts, Reason}})
  end.

-spec send(iodata(), state()) -> ok.
send(Data, #{transport := Transport, socket := Socket}) ->
  Send = case Transport of
          tcp -> fun gen_tcp:send/2;
          tls -> fun ssl:send/2
        end,
  case Send(Socket, Data) of
    ok ->
      ok;
    {error, closed} ->
      throw({error, connection_closed});
    {error, timeout} ->
      throw({error, write_timeout});
    {error, Reason} ->
      throw({error, {send, Reason}})
  end.
