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

-module(mhttp).

-export([start_pool/2, send_request/1, send_request/2,
         start_server/2, set_server_router/2,
         path_variable/2, request_id/1, route_id/1,
         header_name_equal/2,
         status/1]).

-export_type([result/0, result/1, error_reason/0,
              pool_id/0, server_id/0,
              transport/0, socket/0,
              client_key/0,
              request/0, request_options/0, request_result/0,
              response/0,
              msg_internal/0,
              method/0, target/0, version/0, status/0, status_name/0,
              header_name/0, header_value/0, header_field/0,
              header/0, body/0,
              protocol/0, protocol_options/0,
              route_pattern/0, route/0,
              handler_fun/0, handler/0,
              handler_router_options/0, handler_context/0,
              error_handler/0]).

-type result() :: ok | {error, error_reason()}.
-type result(Type) :: {ok, Type} | {error, error_reason()}.

-type error_reason() ::
        {invalid_target, uri:error_reason()}
      | {invalid_target, missing_scheme}
      | {invalid_target, missing_host}
      | too_many_redirections
      | {invalid_location_header_field, uri:error_reason()}
      | {websocket, mhttp_websocket:error_reason()}
      | {connect, term()}
      | connection_closed
      | send_timeout
      | {send, term()}
      | recv_timeout
      | {recv, term()}
      | {setopts, term()}
      | {peername, term()}
      | {controlling_process, term()}
      | {unexpected_data, binary()}
      | {invalid_data, term()} % TODO mhttp_parser:error_reason()
      | connection_failure % client died before we get the EXIT signal
      | {too_many_connections, client_key()}
      | request_timeout
      | term().

-type pool_id() :: atom().
-type server_id() :: atom().

-type transport() :: tcp | tls.

-type socket() :: inet:socket() | ssl:sslsocket().

-type client_key() :: {uri:host(), uri:port_number(), transport()}.

-type request() :: #{method := method(),
                     target := target(),
                     version => version(),
                     header => header(),
                     body => body(),
                     trailer => header(),
                     internal => msg_internal()}.

-type request_options() :: #{pool => pool_id(),
                             follow_redirections => boolean(),
                             max_nb_redirections => pos_integer(),
                             protocol => protocol(),
                             protocol_options => protocol_options(),
                             timeout => pos_integer()}.

-type response() :: #{version => version(),
                      status := status(),
                      reason => binary(),
                      header => header(),
                      body => body(),
                      trailer => header(),
                      internal => msg_internal()}.

-type request_result() ::
        {ok, response()}
      | {ok, {upgraded, response(), pid()}}
      | {error, error_reason()}.

-type msg_internal() :: #{original_body_size => non_neg_integer()}.

-type method() :: get | head | post | put | delete | connect | options | trace
                | binary().

-type target() :: binary() | uri:uri().

-type version() :: http_1_0 | http_1_1 | binary().

-type status() :: 100..999.
-type status_name() :: mhttp_statuses:status_name().

-type header_name() :: binary().
-type header_value() :: binary().
-type header_field() :: {header_name(), header_value()}.
-type header() :: [header_field()].

-type body() :: iodata().

-type protocol() :: module(). % behaviour: mhttp_protocol
-type protocol_options() :: map().

-type route_pattern() :: route_not_found
                       | service_unavailable
                       | mhttp_patterns:pattern().
-type route() :: {route_pattern(), handler()}.

-type handler_fun() :: fun((request(), handler_context()) -> response()).
-type handler() :: handler_fun()
                 | {router, mhttp_router:router()}
                 | {router, mhttp_router:router(), handler_router_options()}.
-type handler_router_options() :: #{strip_path_prefix := binary()}.
-type handler_context() :: #{client_address := inet:ip_address(),
                             client_port := inet:port_number(),
                             route => route(),
                             path_variables => mhttp_patterns:path_variables(),
                             start_time := integer(),
                             request_id := binary()}.

-type error_handler() :: fun((request(), handler_context(),
                              Reason :: term(), [et_erlang:stack_item()]) ->
                                response()).

-spec start_pool(pool_id(), mhttp_pool:options()) ->
        supervisor:startchild_ret().
start_pool(Id, Options) ->
  mhttp_pool_sup:start_pool(Id, Options).

-spec send_request(request()) -> request_result().
send_request(Request) ->
  send_request(Request, #{}).

-spec send_request(request(), request_options()) -> request_result().
send_request(Request, Options) ->
  PoolId = maps:get(pool, Options, default),
  PoolRef = mhttp_pool:process_name(PoolId),
  mhttp_pool:send_request(PoolRef, Request, Options).

-spec start_server(server_id(), mhttp_server:options()) ->
        supervisor:startchild_ret().
start_server(Id, Options) ->
  mhttp_server_sup:start_server(Id, Options).

-spec set_server_router(server_id(), mhttp_router:router()) -> ok.
set_server_router(ServerId, Router) ->
  ServerRef = mhttp_server:process_name(ServerId),
  mhttp_server:set_router(ServerRef, Router).

-spec path_variable(mhttp_patterns:path_variable_name(), handler_context()) ->
        mhttp_patterns:path_variable_value().
path_variable(Name, #{path_variables := Variables}) ->
  case maps:find(Name, Variables) of
    {ok, Value} ->
      Value;
    error ->
      error({unknown_path_variable, Name})
  end.

-spec request_id(handler_context()) -> binary().
request_id(#{request_id := RequestId}) ->
  RequestId.

-spec route_id(handler_context()) -> binary().
route_id(#{route := {route_not_found, _}}) ->
  <<"route_not_found">>;
route_id(#{route := {service_unavailable, _}}) ->
  <<"service_unavailable">>;
route_id(#{route := {Pattern, _}}) ->
  mhttp_patterns:path_pattern(Pattern).

-spec header_name_equal(header_name(), header_name()) -> boolean().
header_name_equal(N1, N2) ->
  string:lowercase(N1) =:= string:lowercase(N2).

-spec status(status_name()) -> mhttp:status().
status(Name) ->
  mhttp_statuses:status(Name).
