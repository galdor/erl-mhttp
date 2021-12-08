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

-module(mhttp_server).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

-export([process_name/1, start_link/2, stop/1,
         set_router/2, find_route/3]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2]).

-export_type([name/0, ref/0, options/0]).

-type name() :: c_gen_server:name().
-type ref() :: c_gen_server:ref().

-type options() :: #{address => inet:socket_address(),
                     port => inet:port_number(),
                     listen_options => [gen_tcp:listen_option()],
                     nb_acceptors => pos_integer(),
                     route_not_found_handler => mhttp:handler(),
                     service_unavailable_handler => mhttp:handler(),
                     error_handler => mhttp:error_handler(),
                     idle_timeout => pos_integer()}.

-type state() :: #{id := mhttp:server_id(),
                   options := options(),
                   socket := inet:socket(),
                   router => mhttp_router:router()}.

-spec process_name(mhttp:server_id()) -> atom().
process_name(Id) ->
 Name = <<"mhttp_server_", (atom_to_binary(Id))/binary>>,
  binary_to_atom(Name).

-spec start_link(mhttp:server_id(), options()) -> Result when
    Result :: {ok, pid()} | ignore | {error, term()}.
start_link(Id, Options) ->
  Name = mhttp_server:process_name(Id),
  gen_server:start_link({local, Name}, ?MODULE, [Id, Options], []).

-spec stop(mhttp:server_id()) -> ok.
stop(Id) ->
  Name = mhttp_server:process_name(Id),
  gen_server:stop(Name).

-spec set_router(ref(), mhttp_router:router()) -> ok.
set_router(Ref, Router) ->
  gen_server:call(Ref, {set_router, Router}, infinity).

-spec find_route(ref(), mhttp:request(), mhttp:handler_context()) ->
        {ok, {mhttp_router:router(), mhttp:route(), mhttp:handler_context()}} |
        {error, term()}.
find_route(Ref, Request, Context) ->
  gen_server:call(Ref, {find_route, Request, Context}, infinity).

-spec init(list()) -> c_gen_server:init_ret(state()).
init([Id, Options]) ->
  logger:update_process_metadata(#{domain => [mhttp, server, Id]}),
  case listen(Id, Options) of
    {ok, State} ->
      spawn_acceptors(State),
      {ok, State};
    {error, Reason} ->
      {stop, Reason}
  end.

-spec terminate(c_gen_server:terminate_reason(), state()) -> ok.
terminate(Reason, State = #{socket := Socket}) ->
  gen_tcp:close(Socket),
  terminate(Reason, maps:remove(socket, State));
terminate(_Reason, _State) ->
  ok.

-spec handle_call(term(), {pid(), c_gen_server:request_id()}, state()) ->
        c_gen_server:handle_call_ret(state()).

handle_call({set_router, Router}, _From, State) ->
  {reply, ok, State#{router => Router}};

handle_call({find_route, Request, Context}, _From,
            State = #{options := Options, router := Router}) ->
  case mhttp_router:find_route(Router, Request, Context) of
    {ok, {Route, Context2}} ->
      {reply, {ok, {Router, Route, Context2}}, State};
    {error, not_found} ->
      Handler = maps:get(route_not_found_handler, Options,
                         fun mhttp_handlers:route_not_found_handler/2),
      Route = {route_not_found, Handler},
      {reply, {ok, {Router, Route, Context}}, State};
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;
handle_call({find_route, _Request, Context}, _From,
            State = #{options := Options}) ->
  Router = #{routes => []},
  Handler = maps:get(service_unavailable_handler, Options,
                     fun mhttp_handlers:service_unavailable_handler/2),
  Route = {service_unavailable, Handler},
  {reply, {ok, {Router, Route, Context}}, State};

handle_call(Msg, From, State) ->
  ?LOG_WARNING("unhandled call ~p from ~p", [Msg, From]),
  {reply, unhandled, State}.

-spec handle_cast(term(), state()) -> c_gen_server:handle_cast_ret(state()).

handle_cast(Msg, State) ->
  ?LOG_WARNING("unhandled cast ~p", [Msg]),
  {noreply, State}.

-spec handle_info(term(), state()) -> c_gen_server:handle_info_ret(state()).

handle_info(Msg, State) ->
  ?LOG_WARNING("unhandled info ~p", [Msg]),
  {noreply, State}.

-spec listen(mhttp:server_id(), options()) -> {ok, state()} | {error, term()}.
listen(Id, Options) ->
  Address = maps:get(address, Options, loopback),
  Port = maps:get(port, Options, 80),
  DefaultListenOptions = [{ip, Address},
                          {reuseaddr, true},
                          {active, false},
                          {send_timeout, 5000},
                          {send_timeout_close, true},
                          binary],
  ListenOptions = DefaultListenOptions ++ maps:get(listen_options, Options, []),
  case gen_tcp:listen(Port, ListenOptions) of
    {ok, Socket} ->
      {ok, {LocalAddress, LocalPort}} = inet:sockname(Socket),
      ?LOG_INFO("listening on ~s:~b", [inet:ntoa(LocalAddress), LocalPort]),
      State = #{id => Id,
                options => Options,
                socket => Socket},
      {ok, State};
    {error, Reason} ->
      ?LOG_ERROR("cannot listen for connections: ~p", [Reason]),
      {error, Reason}
  end.

-spec spawn_acceptors(state()) -> ok.
spawn_acceptors(State = #{options := Options, socket := Socket}) ->
  ConnOptions = connection_options(State),
  AcceptorOptions = #{socket => Socket, connection_options => ConnOptions},
  {ok, AcceptorSup} = mhttp_acceptor_sup:start_link(AcceptorOptions),
  NbAcceptors = maps:get(nb_acceptors, Options, 5),
  mhttp_acceptor_sup:start_children(AcceptorSup, NbAcceptors).

-spec connection_options(state()) -> mhttp_connection:options().
connection_options(#{id := Id, options := Options}) ->
  ErrorHandler = maps:get(error_handler, Options,
                          fun mhttp_handlers:error_handler/4),
  IdleTimeout = maps:get(idle_timeout, Options, 10_000),
  #{server_pid => self(),
    error_handler => ErrorHandler,
    idle_timeout => IdleTimeout,
    server => Id}.
