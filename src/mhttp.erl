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

-module(mhttp).

-export([send_request/1, send_request/2,
         header_name_equal/2]).

-export_type([gen_server_name/0, gen_server_ref/0,
              host/0, transport/0,
              client_key/0,
              request/0, request_options/0,
              response/0,
              method/0, target/0, version/0, status/0,
              header_name/0, header_value/0, header_field/0,
              header/0, body/0]).

-type gen_server_name() :: {local, term()}
                         | {global, term()}
                         | {via, atom(), term()}.

-type gen_server_ref() :: term()
                        | {term(), atom()}
                        | {global, term()}
                        | {via, atom(), term()}
                        | pid().

-type pool_id() :: atom().

-type host() :: inet:hostname() | inet:ip_address().

-type transport() :: tcp | tls.

-type client_key() :: {host(), inets:port_number(), transport()}.

-type request() :: #{method := method(),
                     target := target(),
                     version => version(),
                     header => header(),
                     body => body(),
                     trailer => header()}.

-type request_options() :: #{pool => pool_id(),
                             follow_redirections => boolean(),
                             max_nb_redirections => pos_integer()}.

-type response() :: #{version := version(),
                      status := status(),
                      reason := binary(),
                      header => header(),
                      body => body(),
                      trailer => header()}.

-type method() :: get | head | post | put | delete | connect | options | trace
                | binary().

-type target() :: binary() | uri:uri().

-type version() :: http_1_0 | http_1_1 | binary().

-type status() :: 100..999.

-type header_name() :: binary().
-type header_value() :: binary().
-type header_field() :: {header_name(), header_value()}.
-type header() :: [header_field()].

-type body() :: iodata().

-spec send_request(mhttp:request()) -> {ok, mhttp:response()} | {error, term()}.
send_request(Request) ->
  send_request(Request, #{}).

-spec send_request(mhttp:request(), mhttp:request_options()) ->
        {ok, mhttp:response()} | {error, term()}.
send_request(Request, Options) ->
  PoolId = maps:get(pool, Options, default),
  PoolRef = mhttp_pool:process_name(PoolId),
  mhttp_pool:send_request(PoolRef, Request, Options).

-spec header_name_equal(header_name(), header_name()) -> boolean().
header_name_equal(N1, N2) ->
  string:lowercase(N1) =:= string:lowercase(N2).
