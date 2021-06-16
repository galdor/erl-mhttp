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

-module(mhttp_client).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([start_link/2, send_request/2, send_request/3]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([name/0, ref/0, options/0, tcp_option/0, tls_option/0]).

-type name() :: et_gen_server:name().
-type ref() :: et_gen_server:ref().

-type options() ::
        #{host => uri:host(),
          port => uri:port_number(),
          transport => mhttp:transport(),
          tcp_options => [tcp_option()],
          tls_options => [tls_option()],
          connection_timeout => timeout(),
          read_timeout => timeout(),
          header => mhttp:header(),
          compression => boolean(),
          log_requests => boolean(),
          ca_certificate_bundle_path => file:name_all() | undefined}.

-type tcp_option() :: gen_tcp:connect_option().
-type tls_option() :: ssl:tls_client_option().

-type state() ::
        #{pool := mhttp:pool_id(),
          options := options(),
          transport := mhttp:transport(),
          socket := mhttp:socket(),
          parser := mhttp_parser:parser(),
          upgraded => boolean()}.

-spec start_link(mhttp:pool_id(), options()) -> et_gen_server:start_ret().
start_link(PoolId, Options) ->
  gen_server:start_link(?MODULE, [PoolId, Options], []).

-spec send_request(ref(), mhttp:request()) ->
        mhttp:result(mhttp:response_result()).
send_request(Ref, Request) ->
  send_request(Ref, Request, #{}).

-spec send_request(ref(), mhttp:request(), mhttp:request_options()) ->
        mhttp:result(mhttp:response_result()).
send_request(Ref, Request, Options) ->
  gen_server:call(Ref, {send_request, Request, Options}, infinity).

-spec init(list()) -> et_gen_server:init_ret(state()).
init([PoolId, Options]) ->
  logger:update_process_metadata(#{domain => log_domain()}),
  case connect(PoolId, Options) of
    {ok, State} ->
      {ok, State};
    {error, Reason} ->
      {stop, Reason}
  end.

-spec terminate(et_gen_server:terminate_reason(), state()) -> ok.
terminate(_Reason, State = #{socket := Socket}) ->
  case maps:get(upgraded, State, false) of
    false ->
      case maps:get(transport, State) of
        tcp -> gen_tcp:close(Socket);
        tls -> ssl:close(Socket)
      end,
      ok;
    true ->
      ok
  end.

-spec handle_call(term(), {pid(), et_gen_server:request_id()}, state()) ->
        et_gen_server:handle_call_ret(state()).
handle_call({send_request, Request, Options}, _From, State) ->
  try send_request_1(Request, Options, State) of
    {Response, State2} when is_map(Response) ->
      case connection_needs_closing(Response) of
        true ->
          {stop, normal, {ok, Response}, State2};
        false ->
          {reply, {ok, Response}, State2}
      end;
    {Result = {upgraded, _, _}, State2} ->
      {stop, normal, {ok, Result}, State2}
  catch
    throw:{error, Reason} ->
      {stop, Reason, {error, Reason}, State}
  end;
handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

-spec handle_cast(term(), state()) -> et_gen_server:handle_cast_ret(state()).
handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

-spec handle_info(term(), state()) -> et_gen_server:handle_info_ret(state()).
handle_info({Event, _}, _State) when Event =:= tcp_closed;
                                     Event =:= ssl_closed ->
  ?LOG_DEBUG("connection closed"),
  exit(normal);
handle_info({Event, _Socket, Data}, _State) when Event =:= tcp;
                                                 Event =:= ssl ->
  throw({error, {unexpected_data, Data}});
handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec options_transport(options()) -> mhttp:transport().
options_transport(Options) ->
  maps:get(transport, Options, tcp).

-spec options_connect_options(options()) -> [Options] when
    Options :: tcp_option() | tls_option().
options_connect_options(Options = #{transport := tcp}) ->
  maps:get(tcp_options, Options, []);
options_connect_options(Options = #{transport := tls}) ->
  TCPOptions = maps:get(tcp_options, Options, []),
  TLSOptions = maps:get(tls_options, Options, []),
  DefaultTLSOptions = default_tls_options(Options),
  TCPOptions ++ TLSOptions ++ DefaultTLSOptions.

-spec options_host(options()) -> binary().
options_host(Options) ->
  maps:get(host, Options, <<"localhost">>).

-spec options_port(options()) -> inet:port_number().
options_port(Options) ->
  maps:get(port, Options, 80).

-spec connect(mhttp:pool_id(), options()) -> {ok, state()} | {error, term()}.
connect(PoolId, Options) ->
  Transport = options_transport(Options),
  Host = options_host(Options),
  Port = options_port(Options),
  Timeout = maps:get(connection_timeout, Options, 5000),
  RequiredConnectOptions = [{mode, binary}],
  ConnectOptions = RequiredConnectOptions ++ options_connect_options(Options),
  ?LOG_DEBUG("connecting to ~s:~b (transport ~p)", [Host, Port, Transport]),
  HostAddress = host_address(Host),
  Connect = case Transport of
              tcp -> fun gen_tcp:connect/4;
              tls -> fun ssl:connect/4
            end,
  case Connect(HostAddress, Port, ConnectOptions, Timeout) of
    {ok, Socket} ->
      State = #{pool => PoolId,
                options => Options,
                transport => Transport,
                socket => Socket,
                parser => mhttp_parser:new(response)},
      {ok, State};
    {error, Reason} ->
      ?LOG_ERROR("connection failed: ~p", [Reason]),
      {error, {connect, Reason}}
  end.

-spec host_address(uri:host()) -> inet:hostname() | inet:socket_address().
host_address(Host) ->
  %% While low level connection functions are perfectly able to connect to an
  %% IP address passed as a string, some features such as ssl peer hostname
  %% verification treat host strings as hostnames even if they represent an IP
  %% address. In the ssl case, they will check for SAN DNS names entries
  %% instead of SAN IP address entries.
  %%
  %% Therefore we check the host string to see if it is an IP address; when
  %% this is the case, we use the inet socket address format (a tuple).
  HostString = binary_to_list(Host),
  case inet:parse_address(HostString) of
    {ok, Address} ->
      Address;
    {error, _} ->
      HostString
  end.

-spec send_request_1(mhttp:request(), mhttp:request_options(), state()) ->
        {mhttp:response_result(), state()}.
send_request_1(Request0, RequestOptions, State) ->
  StartTime = erlang:system_time(microsecond),
  Request = finalize_request(State, Request0, RequestOptions),
  send(State, mhttp_proto:encode_request(Request)),
  set_socket_active(State, false),
  {State2, Response} = read_response(State),
  log_request(Request, Response, StartTime, State2),
  case maybe_upgrade(Request, RequestOptions, Response, State2) of
    {upgraded, Pid, State3} ->
      {{upgraded, Response, Pid}, State3};
    not_upgraded ->
      set_socket_active(State2, true),
      {Response, State2}
  end.

-spec finalize_request(state(), mhttp:request(), mhttp:request_options()) ->
        mhttp:request().
finalize_request(#{options := Options}, Request, RequestOptions) ->
  Funs = [protocol_finalization_fun(RequestOptions),
          compression_finalization_fun(Options),
          header_finalization_fun(Options),
          host_finalization_fun(Options),
          fun mhttp_request:maybe_add_content_length/1],
  lists:foldl(fun (Fun, R) -> Fun(R) end, Request, Funs).

-spec protocol_finalization_fun(mhttp:request_options()) ->
        fun((mhttp:request()) -> mhttp:request()).
protocol_finalization_fun(RequestOptions) ->
  fun (Request) ->
      case maps:find(protocol, RequestOptions) of
        {ok, Protocol} ->
          ProtocolOptions = maps:get(protocol_options, RequestOptions, #{}),
          Protocol:request(Request, ProtocolOptions);
        error ->
          Request
      end
  end.

-spec compression_finalization_fun(options()) ->
        fun((mhttp:request()) -> mhttp:request()).
compression_finalization_fun(Options) ->
  fun (Request) ->
      Header = mhttp_request:header(Request),
      Header2 = case maps:get(compression, Options, false) of
                 true ->
                   mhttp_header:add(Header, <<"Accept-Encoding">>, <<"gzip">>);
                  false ->
                    Header
                end,
      Request#{header => Header2}
  end.

-spec header_finalization_fun(options()) ->
        fun((mhttp:request()) -> mhttp:request()).
header_finalization_fun(Options) ->
  fun (Request) ->
      Header = maps:get(header, Options, []),
      mhttp_request:prepend_header(Request, Header)
  end.

-spec host_finalization_fun(options()) ->
        fun((mhttp:request()) -> mhttp:request()).
host_finalization_fun(Options) ->
  Transport = options_transport(Options),
  Host = options_host(Options),
  Port = options_port(Options),
  fun (Request) ->
      mhttp_request:ensure_host(Request, Host, Port, Transport)
  end.

-spec log_request(mhttp:request(), mhttp:response(), StartTime :: integer(),
                  state()) -> ok.
log_request(Request, Response, StartTime,
            #{pool := PoolId, options := Options}) ->
  case maps:get(log_requests, Options, true) of
    true ->
      mhttp_log:log_outgoing_request(Request, Response, StartTime, PoolId,
                                     log_domain()),
      ok;
    false ->
      ok
  end.

-spec read_response(state()) -> {state(), mhttp:response()}.
read_response(State = #{parser := Parser}) ->
  Data = recv(State, 0),
  case mhttp_parser:parse(Parser, Data) of
    {ok, Response, Parser2} ->
      {State#{parser => Parser2}, Response};
    {more, Parser2} ->
      read_response(State#{parser => Parser2});
    {error, Reason} ->
      throw({error, {invalid_data, Reason}})
  end.

-spec maybe_upgrade(mhttp:request(), mhttp:request_options(),
                    mhttp:response(), state()) ->
        {upgraded, pid(), state()} | not_upgraded.
maybe_upgrade(Request, RequestOptions = #{protocol := Protocol},
              Response = #{status := 101},
              State = #{socket := Socket, transport := Transport,
                        parser := #{data := SocketData}}) ->
  Header = mhttp_response:header(Response),
  case mhttp_header:has_connection_upgrade(Header) of
    true ->
      ?LOG_DEBUG("upgrading connection using protocol ~p", [Protocol]),
      ProtocolOptions = maps:get(protocol_options, RequestOptions, #{}),
      case Protocol:upgrade(Request, Response, ProtocolOptions) of
        {ok, Pid} ->
          set_controlling_process(State, Pid),
          case Protocol:activate(Pid, Socket, Transport, SocketData) of
            ok ->
              {upgraded, Pid, State#{upgraded => true}};
            {error, Reason} ->
              throw({error, Reason})
          end;
        {error, Reason} ->
          throw({error, Reason})
      end;
    false ->
      not_upgraded
  end;
maybe_upgrade(_Request, _RequestOptions, _Response, _State) ->
  not_upgraded.

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

-spec set_controlling_process(state(), pid()) -> ok.
set_controlling_process(#{transport := Transport, socket := Socket}, Pid) ->
  ControllingProcess = case Transport of
                         tcp -> fun gen_tcp:controlling_process/2;
                         tls -> fun ssl:controlling_process/2
                       end,
  case ControllingProcess(Socket, Pid) of
    ok ->
      ok;
    {error, Reason} ->
      throw({error, {controlling_process, Reason}})
  end.

-spec send(state(), iodata()) -> ok.
send(#{transport := Transport, socket := Socket}, Data) ->
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
      throw({error, send_timeout});
    {error, Reason} ->
      throw({error, {send, Reason}})
  end.

-spec recv(state(), non_neg_integer()) -> binary().
recv(#{options := Options, transport := Transport, socket := Socket}, N) ->
  Recv = case Transport of
          tcp -> fun gen_tcp:recv/3;
          tls -> fun ssl:recv/3
        end,
  Timeout = maps:get(read_timeout, Options, 30_000),
  case Recv(Socket, N, Timeout) of
    {ok, Data} ->
      Data;
    {error, closed} ->
      throw({error, connection_closed});
    {error, timeout} ->
      throw({error, recv_timeout});
    {error, Reason} ->
      throw({error, {recv, Reason}})
  end.

-spec connection_needs_closing(mhttp:response()) -> boolean().
connection_needs_closing(Response) ->
  mhttp_header:has_connection_close(mhttp_response:header(Response)).

-spec log_domain() -> [atom()].
log_domain() ->
  [mhttp, client].

-spec default_tls_options(options()) -> [tls_option()].
default_tls_options(Options) ->
  %% See https://github.com/benoitc/hackney/issues/624.
  %%
  %% It is infortunately impossible to trust Erlang/OTP regarding any default
  %% setting.
  HostnameCheck = [{match_fun,
                    public_key:pkix_verify_hostname_match_fun(https)}],
  TLSOptions0 = [{log_level, error},
                 {verify, verify_peer},
                 {customize_hostname_check, HostnameCheck}],
  case maps:get(ca_certificate_bundle_path, Options, undefined) of
    undefined ->
      TLSOptions0;
    Path ->
      [{cacertfile, Path} | TLSOptions0]
  end.
