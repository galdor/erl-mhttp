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

-module(mhttp_client).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/1, send_request/2, send_request/3]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([client_name/0, client_ref/0, options/0]).

-type client_name() :: mhttp:gen_server_name().
-type client_ref() :: mhttp:gen_server_ref().

-type options() :: #{host => mhttp:host(),
                     port => inets:port_number(),
                     transport => mhttp:transport(),
                     tcp_options => [gen_tcp:connect_option()],
                     tls_options => [ssl:tls_client_option()]}.

-type state() :: #{options := options(),
                   transport := mhttp:transport(),
                   socket := inet:socket() | ssl:sslsocket(),
                   read_buffer := binary()}.

-spec start_link(options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).

-spec send_request(mhttp:client_ref(), mhttp:request()) ->
        {mhttp:response()} | {error, term()}.
send_request(Ref, Request) ->
  send_request(Ref, Request, #{}).

-spec send_request(mhttp:client_ref(), mhttp:request(),
                   mhttp:request_options()) ->
        {ok, mhttp:response()} | {error, term()}.
send_request(Ref, Request, Options) ->
  gen_server:call(Ref, {send_request, Request, Options}, infinity).

init([Options]) ->
  logger:update_process_metadata(#{domain => [mhttp, client]}),
  {ok, connect(Options)}.

terminate(_Reason, #{transport := tcp, socket := Socket}) ->
  ?LOG_DEBUG("closing connection"),
  gen_tcp:close(Socket),
  ok;
terminate(_Reason, #{transport := tls, socket := Socket}) ->
  ?LOG_DEBUG("closing connection"),
  ssl:close(Socket),
  ok.

handle_call({send_request, Request, Options}, _From, State) ->
  {State2, Response} = do_send_request(State, Request, Options),
  case connection_needs_closing(Response) of
    true ->
      {stop, normal, {ok, Response}, State2};
    false ->
      {reply, {ok, Response}, State2}
  end;

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {noreply, State}.

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

handle_info({tcp, _Socket, Data}, _State) ->
  error({unexpected_data, Data});

handle_info({tcp_closed, _}, _State) ->
  on_connection_closed();

handle_info({ssl, _Socket, Data}, _State) ->
  error({unexpected_data, Data});

handle_info({ssl_closed, _}, _State) ->
  on_connection_closed();

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec connect(options()) -> state().
connect(Options) ->
  case maps:get(transport, Options, tcp) of
    tcp ->
      connect_tcp(Options);
    tls ->
      connect_tls(Options)
  end.

-spec connect_tcp(options()) -> state().
connect_tcp(Options) ->
  Host = maps:get(host, Options, <<"localhost">>),
  Port = maps:get(port, Options, 80),
  RequiredTCPOptions = [{mode, binary}],
  TCPOptions = RequiredTCPOptions ++ maps:get(tcp_options, Options, []),
  ?LOG_INFO("connecting to ~s:~b", [Host, Port]),
  HostString = unicode:characters_to_list(Host),
  case gen_tcp:connect(HostString, Port, TCPOptions, 5000) of
    {ok, Socket} ->
      ?LOG_INFO("connection established"),
      #{options => Options#{host => Host, port => Port},
        transport => tcp,
        socket => Socket,
        read_buffer => <<>>};
    {error, Reason} ->
      ?LOG_ERROR("connection failed: ~p", [Reason]),
      error({connection_failure, {Host, Port, tcp}, Reason})
  end.

-spec connect_tls(options()) -> state().
connect_tls(Options) ->
  Host = maps:get(host, Options, <<"localhost">>),
  Port = maps:get(port, Options, 443),
  RequiredTLSOptions = [{mode, binary}],
  TLSOptions = RequiredTLSOptions ++
    maps:get(tcp_options, Options, []) ++
    maps:get(tls_options, Options, []),
  ?LOG_INFO("connecting to ~s:~b", [Host, Port]),
  HostString = unicode:characters_to_list(Host),
  case ssl:connect(HostString, Port, TLSOptions, 5000) of
    {ok, Socket} ->
      ?LOG_INFO("connection established"),
      #{options => Options#{host => Host, port => Port},
        transport => tls,
        socket => Socket,
        read_buffer => <<>>};
    {error, Reason} ->
      ?LOG_ERROR("connection failed: ~p", [Reason]),
      error({connection_failure, {Host, Port, tls}, Reason})
  end.

-spec do_send_request(state(), mhttp:request(), mhttp:request_options()) ->
        {state(), mhttp:response()}.
do_send_request(State, Request0, _RequestOptions) ->
  Request = finalize_request(State, Request0),
  send(State, mhttp_proto:encode_request(Request)),
  set_socket_active(State, false),
  {Response, Rest} = read_response(State, Request),
  set_socket_active(State, true),
  {State#{read_buffer => Rest}, Response}.

-spec finalize_request(state(), mhttp:request()) -> mhttp:request().
finalize_request(#{options := Options}, Request) ->
  #{host := Host, port := Port} = Options,
  Funs = [fun (R) -> mhttp_request:ensure_host(R, Host, Port) end,
          fun mhttp_request:maybe_add_content_length/1],
  lists:foldl(fun (Fun, R) -> Fun(R) end, Request, Funs).

-spec read_response(state(), mhttp:request()) ->
        {mhttp:response(), Rest :: binary()}.
read_response(State = #{read_buffer := Data}, Request) ->
  Parser = mhttp_response_parser:new(Data, Request),
  read_response(State, Parser, <<>>).

-spec read_response(state(), mhttp_response_parser:parser(), binary()) ->
        {mhttp:response(), Rest :: binary()}.
read_response(State, Parser, Data) ->
  case mhttp_response_parser:parse(Parser, Data) of
    {ok, Response, #{data := Rest}} ->
      {Response, Rest};
    {more, Parser2} ->
      read_response(State, Parser2, recv(State, 0))
  end.

-spec set_socket_active(state(), boolean()) -> ok.
set_socket_active(#{transport := Transport, socket := Socket}, Active) ->
  Fun = case Transport of
          tcp -> fun inet:setopts/2;
          tls -> fun ssl:setopts/2
        end,
  Fun(Socket, [{active, Active}]).

-spec send(state(), iodata()) -> ok.
send(#{transport := Transport, socket := Socket}, Data) ->
  Fun = case Transport of
          tcp -> fun gen_tcp:send/2;
          tls -> fun ssl:send/2
        end,
  case Fun(Socket, Data) of
    ok ->
      ok;
    {error, closed} ->
      on_connection_closed()
  end.

-spec recv(state(), non_neg_integer()) -> binary().
recv(#{transport := Transport, socket := Socket}, N) ->
  Fun = case Transport of
          tcp -> fun gen_tcp:recv/2;
          tls -> fun ssl:recv/2
        end,
  case Fun(Socket, N) of
    {ok, Data} ->
      Data;
    {error, closed} ->
      on_connection_closed()
  end.

-spec on_connection_closed() -> no_return().
on_connection_closed() ->
  ?LOG_INFO("connection closed"),
  exit(normal).

-spec connection_needs_closing(mhttp:response()) -> boolean().
connection_needs_closing(Response) ->
  mhttp_header:has_connection_close(mhttp_response:header(Response)).
