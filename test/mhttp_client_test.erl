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

-module(mhttp_client_test).

-include_lib("eunit/include/eunit.hrl").

client_test_() ->
  {setup,
   fun () ->
       %% Client pool
       PoolName = mhttp_pool:process_name(test),
       mhttp_pool:start_link({local, PoolName}, #{}),
       %% Server
       ServerRoot = "/tmp/mhttp-tests/httpd/server",
       ensure_directory(ServerRoot),
       DocumentRoot = "/tmp/mhttp-tests/httpd/documents",
       ensure_directory(DocumentRoot),
       Options = [{server_name, "localhost"},
                  {bind_address, "localhost"},
                  {port, 8081},
                  {server_root, ServerRoot},
                  {document_root, DocumentRoot},
                  {modules, [mhttp_httpd_mod_test]}],
       {ok, Pid} = inets:start(httpd, Options),
       Pid
   end,
   fun (Pid) ->
       inets:stop(httpd, Pid),
       mhttp_pool:stop(mhttp_pool:process_name(test))
   end,
   [fun simple_request/0,
    fun redirections/0]}.

simple_request() ->
  URI = test_uri(#{path => <<"/">>}),
  {ok, Response} = send_request(#{method => get, target => URI}),
  ?assertEqual(200, mhttp_response:status(Response)),
  ?assertEqual(<<"hello">>, mhttp_response:body(Response)).

redirections() ->
  URI = test_uri(#{path => <<"/redirect-1">>}),
  {ok, Response1} = send_request(#{method => get, target => URI}),
  ?assertEqual(302, mhttp_response:status(Response1)),
  {ok, Response2} = send_request(#{method => get, target => URI},
                                 #{follow_redirections => true,
                                   max_nb_redirections => 5}),
  ?assertEqual(200, mhttp_response:status(Response2)),
  Result3 = send_request(#{method => get, target => URI},
                         #{follow_redirections => true,
                           max_nb_redirections => 1}),
  ?assertMatch({error, {too_many_redirections, _}}, Result3).

-spec send_request(mhttp:request()) -> {ok, mhttp:response()} | {error, term()}.
send_request(Request) ->
  send_request(Request, #{}).

-spec send_request(mhttp:request(), mhttp:request_options()) ->
        {ok, mhttp:response()} | {error, term()}.
send_request(Request, Options) ->
  mhttp:send_request(Request, Options#{pool => test}).

-spec test_uri(uri:uri()) -> uri:uri().
test_uri(URI) ->
  URI#{scheme => <<"http">>,
       host => <<"localhost">>,
       port => 8081}.

-spec ensure_directory(file:name_all()) -> ok | {error, term()}.
ensure_directory(Path) ->
  ok = case file:del_dir_r(Path) of
         ok -> ok;
         {error, enoent} -> ok;
         Result -> Result
       end,
  case filelib:ensure_dir(Path) of
    ok ->
      case file:make_dir(Path) of
        ok ->
          ok;
        {error, eexist} ->
          ok;
        {error, Reason} ->
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.
