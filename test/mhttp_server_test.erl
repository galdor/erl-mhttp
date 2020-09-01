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

-module(mhttp_server_test).

-include_lib("eunit/include/eunit.hrl").

server_test_() ->
  {setup,
   fun () ->
       Name = mhttp_server:process_name(test),
       Options = #{port => 8080},
       {ok, _} = mhttp_server:start_link({local, Name}, Options)
   end,
   fun (_) ->
       mhttp_server:stop(mhttp_server:process_name(test))
   end,
   [fun start_stop/0,
    fun no_router/0,
    fun not_found/0,
    fun found/0]}.

start_stop() ->
  ok.

no_router() ->
  {ok, {{_, Status, _}, _, _}} = httpc:request(test_uri(<<"/">>)),
  ?assertEqual(503, Status).

not_found() ->
  Router = #{routes => []},
  mhttp:set_server_router(test, Router),
  {ok, {{_, Status, _}, _, _}} = httpc:request(test_uri(<<"/">>)),
  ?assertEqual(404, Status).

found() ->
  Handler = fun (_Request, _Context) -> #{status => 200} end,
  Router = #{routes => [{<<"/">>, Handler}]},
  mhttp:set_server_router(test, Router),
  {ok, {{_, Status, _}, _, _}} = httpc:request(test_uri(<<"/">>)),
  ?assertEqual(200, Status).

-spec test_uri(uri:path()) -> httpc:url().
test_uri(Path) ->
  URI = #{scheme => <<"http">>,
          host => <<"localhost">>,
          port => 8080,
          path => Path},
  binary_to_list(uri:serialize(URI)).
