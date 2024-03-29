%% Copyright (c) 2020-2021 Exograd SAS.
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

-export([start_link/1, send_message/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([ref/0, options/0, event_message/0, event/0]).

-type ref() :: c_gen_server:ref().

-type options() :: #{event_target => pid() | atom(),
                     ping_interval => pos_integer(),
                     ping_timeout => pos_integer()}.

-type state() :: #{options := options(),
                   transport => mhttp:transport(),
                   socket => mhttp:socket(),
                   peer_address => inet:ip_address(),
                   peer_port => inet:port_number(),
                   parser => mhttp_websocket_parser:parser(),
                   ping_timer => reference(),
                   ping_data => binary()}.

-type event_message() :: {websocket, event()}.

-type event() ::
        connected
      | {message, mhttp_websocket:message()}
      | terminating.

-spec start_link(options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).

-spec send_message(ref(), mhttp_websocket:message()) -> ok | {error, term()}.
send_message(Ref, Message) ->
  gen_server:call(Ref, {send_message, Message}, infinity).

-spec init(list()) -> c_gen_server:init_ret(state()).
init([Options]) ->
  logger:update_process_metadata(#{domain => [mhttp, websocket_client]}),
  State = #{options => Options},
  {ok, State}.

-spec terminate(c_gen_server:terminate_reason(), state()) -> ok.
terminate(_Reason, State) ->
  send_event(terminating, State).

-spec handle_call(term(), {pid(), c_gen_server:request_id()}, state()) ->
        c_gen_server:handle_call_ret(state()).
handle_call({activate, Socket, Transport, Data}, _From, State) ->
  {Address, Port} = peername(Socket, Transport),
  ?LOG_DEBUG("connected to ~s:~b", [inet:ntoa(Address), Port]),
  State2 = State#{transport => Transport,
                  socket => Socket,
                  peer_address => Address,
                  peer_port => Port,
                  parser => mhttp_websocket_parser:new()},
  set_socket_active(State2, true),
  schedule_send_ping(State2),
  %% Data coming from the parser buffer of the http connection must be
  %% reinjected. It is one hell of a hack.
  case Transport of
    tcp -> self() ! {tcp, Socket, Data};
    tls -> self() ! {ssl, Socket, Data}
  end,
  send_event(connected, State2),
  {reply, ok, State2};
handle_call({send_message, Message}, _From, State) ->
  do_send_message(Message, State),
  {reply, ok, State};
handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

-spec handle_cast(term(), state()) -> c_gen_server:handle_cast_ret(state()).
handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

-spec handle_info(term(), state()) -> c_gen_server:handle_info_ret(state()).
handle_info(send_ping, State = #{options := Options}) ->
  Data = integer_to_binary(os:system_time(millisecond)),
  do_send_message({ping, Data}, State),
  Timeout = maps:get(ping_timeout, Options, 10000),
  Timer = erlang:send_after(Timeout, self(), ping_timeout),
  {noreply, State#{ping_data => Data, ping_timer => Timer}};
handle_info(ping_timeout, State) ->
  ?LOG_ERROR("ping timeout"),
  {stop, normal, State};
handle_info({Event, _Port, Data}, State = #{parser := Parser}) when
    Event =:= tcp; Event =:= ssl ->
  State2 = State#{parser => mhttp_websocket_parser:append_data(Parser, Data)},
  case process_data(State2) of
    {ok, State3} ->
      {noreply, State3};
    {error, close, State3} ->
      ?LOG_DEBUG("closing connection"),
      {stop, normal, State3};
    {error, Reason, State3} ->
      ?LOG_ERROR("~tp", [Reason]),
      {stop, Reason, State3}
  end;
handle_info({Event, _Port}, State) when
    Event =:= tcp_closed; Event =:= ssl_closed ->
  ?LOG_DEBUG("connection closed"),
  {stop, normal, State};
handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec do_send_message(mhttp_websocket:message(), state()) -> ok.
do_send_message(Message, State) ->
  send(mhttp_websocket:serialize(Message), State).

-spec process_data(state()) -> {ok, state()} | {error, term(), state()}.
process_data(State = #{parser := Parser}) ->
  case mhttp_websocket_parser:parse_all(Parser) of
    {ok, Messages, Parser2} ->
      process_messages(Messages, State#{parser => Parser2});
    {error, Reason} ->
      {error, Reason, State}
  end.

-spec process_messages([mhttp_websocket:message()], state()) ->
        {ok, state()} | {error, term(), state()}.
process_messages([], State) ->
  {ok, State};
process_messages([Message | Messages], State) ->
  case process_message(Message, State) of
    {ok, State2} ->
      process_messages(Messages, State2);
    {error, Reason, State2} ->
      {error, Reason, State2}
  end.

-spec process_message(mhttp_websocket:message(), state()) ->
        {ok, state()} | {error, term(), state()}.
process_message(close, State) ->
  ?LOG_INFO("server closing connection"),
  {error, close, State};
process_message({close, 1000, _}, State) ->
  %% Normal shutdown
  ?LOG_DEBUG("server closing connection"),
  {error, close, State};
process_message({close, Status, <<"">>}, State) ->
  ?LOG_INFO("server closing connection (status ~b)", [Status]),
  {error, close, State};
process_message({close, Status, Message}, State) ->
  ?LOG_INFO("server closing connection (status ~b): ~ts", [Status, Message]),
  {error, close, State};
process_message({ping, Data}, State) ->
  do_send_message({pong, Data}, State),
  {ok, State};
process_message({pong, Data}, State = #{ping_data := ExpectedData,
                                        ping_timer := Timer}) ->
  case Data =:= ExpectedData of
    true ->
      erlang:cancel_timer(Timer),
      schedule_send_ping(State),
      {ok, maps:without([ping_data, ping_timer], State)};
    false ->
      ?LOG_WARNING("pong data mismatch"),
      {ok, State}
  end;
process_message({pong, _}, State) ->
  ?LOG_WARNING("unexpected pong"),
  {ok, State};
process_message(Message = {data, _, _}, State) ->
  send_event({message, Message}, State),
  {ok, State}.

-spec send_event(event(), state()) -> ok.
send_event(Event, #{options := #{event_target := Target}}) ->
  Target ! {websocket, Event},
  ok;
send_event(_, _) ->
  ok.

-spec schedule_send_ping(state()) -> ok.
schedule_send_ping(#{options := Options}) ->
  Interval = maps:get(ping_interval, Options, 10000),
  erlang:send_after(Interval, self(), send_ping),
  ok.

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
      error({peername, Reason})
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
    {error, connection_closed} ->
      error(connection_closed);
    {error, Reason} ->
      error({setopts, Reason})
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
      error(connection_closed);
    {error, timeout} ->
      error(send_timeout);
    {error, Reason} ->
      error({send, Reason})
  end.
