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

-module(mhttp_payload_tests).

-include_lib("eunit/include/eunit.hrl").

server_test_() ->
  {setup,
   fun () ->
       {ok, _} = application:ensure_all_started(mhttp),
       Options = #{port => 8080,
                   middlewares => [{mhttp_test_middleware_base64, []}]},
       {ok, _} = mhttp_server:start_link(test, Options)
   end,
   fun (_) ->
       mhttp_server:stop(test)
   end,
   [fun input_payload/0,
    fun output_payload/0]}.

input_payload() ->
  Handler = fun (Request, _Context) ->
                ?assertEqual({base64, <<"Hello">>},
                             mhttp_request:body(Request)),
                #{status => 200}
            end,
  Router = #{routes => [{<<"/">>, Handler}]},
  mhttp:set_server_router(test, Router),
  Request = #{method => post,
              target => test_uri(<<"/">>),
              body => <<"SGVsbG8=">>},
  {ok, Response} = mhttp:send_request(Request),
  ?assertEqual(200, mhttp_response:status(Response)).

output_payload() ->
  Handler = fun (_Request, _Context) ->
                #{status => 200,
                  body => {base64, <<"Hello">>}}
            end,
  Router = #{routes => [{<<"/">>, Handler}]},
  mhttp:set_server_router(test, Router),
  Request = #{method => get, target => test_uri(<<"/">>)},
  {ok, Response} = mhttp:send_request(Request),
  ?assertEqual(200, mhttp_response:status(Response)),
  ?assertEqual(<<"SGVsbG8=">>, mhttp_response:body(Response)).

-spec test_uri(uri:path()) -> uri:uri().
test_uri(Path) ->
  #{scheme => <<"http">>,
    host => <<"localhost">>,
    port => 8080,
    path => Path}.
