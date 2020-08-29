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

-module(mhttp_server).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([process_name/1, start_link/1, start_link/2,
         set_router/2, find_route/2]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([server_name/0, server_ref/0, options/0]).

-type server_name() :: mhttp:gen_server_name().
-type server_ref() :: mhttp:gen_server_ref().

-type options() :: #{address => inet:socket_address(),
                     port => inets:port_number(),
                     tcp_options => [gen_tcp:listen_option()],
                     nb_acceptors => pos_integer(),
                     unavailable_service_handler => mhttp:handler(),
                     error_handler => mhttp:error_handler()}.

-type state() :: #{options := options(),
                   socket := inet:socket(),
                   router => mhttp_router:router()}.

-spec process_name(mhttp:server_id()) -> atom().
process_name(Id) ->
  Name = <<"mhttp_server_", (atom_to_binary(Id))/binary>>,
  binary_to_atom(Name).

-spec start_link(options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Options) ->
  gen_server:start_link(?MODULE, [Options], []).

-spec start_link(server_name(), options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Name, Options) ->
  gen_server:start_link(Name, ?MODULE, [Options], []).

-spec set_router(server_ref(), mhttp_router:router()) -> ok.
set_router(Ref, Router) ->
  gen_server:call(Ref, {set_router, Router}, infinity).

-spec find_route(server_ref(), mhttp:request()) ->
        {mhttp_router:route(), mhttp_router:request_context()}.
find_route(Ref, Request) ->
  gen_server:call(Ref, {find_route, Request}, infinity).

init([Options]) ->
  logger:update_process_metadata(#{domain => [mhttp, server]}),
  State = listen(Options),
  spawn_acceptors(State),
  {ok, State}.

terminate(Reason, State = #{socket := Socket}) ->
  gen_tcp:close(Socket),
  terminate(Reason, maps:remove(socket, State));
terminate(_Reason, _State) ->
  ok.

handle_call({set_router, Router}, _From, State) ->
  {reply, ok, State#{router => Router}};

handle_call({find_route, Request}, _From, State = #{router := Router}) ->
  {reply, mhttp_router:find_route(Router, Request), State};
handle_call({find_route, _Request}, _From, State = #{options := Options}) ->
  Handler = maps:get(unavailable_service_handler, Options,
                     fun mhttp_handlers:unavailable_service_handler/2),
  Route = {unavailable_service, Handler},
  {reply, {Route, #{}}, State};

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {noreply, State}.

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec listen(options()) -> state().
listen(Options) ->
  Address = maps:get(address, Options, loopback),
  Port = maps:get(port, Options, 80),
  DefaultTCPOptions = [{ip, Address},
                       {reuseaddr, true},
                       {active, false},
                       {send_timeout, 5000},
                       {send_timeout_close, true},
                       binary],
  TCPOptions = DefaultTCPOptions ++ maps:get(tcp_options, Options, []),
  case gen_tcp:listen(Port, TCPOptions) of
    {ok, Socket} ->
      {ok, {LocalAddress, LocalPort}} = inet:sockname(Socket),
      ?LOG_INFO("listening on ~s:~b", [inet:ntoa(LocalAddress), LocalPort]),
      #{options => Options#{address => Address, port => Port},
        socket => Socket};
    {error, Reason} ->
      ?LOG_ERROR("cannot listen for connections: ~p", [Reason]),
      error({listen_failure, {Address, Port}, Reason})
  end.

-spec spawn_acceptors(state()) -> ok.
spawn_acceptors(State = #{options := Options, socket := Socket}) ->
  ConnOptions = connection_options(State),
  AcceptorOptions = #{socket => Socket, connection_options => ConnOptions},
  {ok, AcceptorSup} = mhttp_acceptor_sup:start_link(AcceptorOptions),
  NbAcceptors = maps:get(nb_acceptors, Options, 5),
  mhttp_acceptor_sup:start_children(AcceptorSup, NbAcceptors).

-spec connection_options(state()) -> mhttp_connection:options().
connection_options(#{options := Options}) ->
  ErrorHandler = maps:get(error_handler, Options,
                          fun mhttp_handlers:error_handler/4),
  #{server_pid => self(),
    error_handler => ErrorHandler}.
