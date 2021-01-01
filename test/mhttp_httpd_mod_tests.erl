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

-module(mhttp_httpd_mod_tests).

-export([do/1]).

-include_lib("inets/include/httpd.hrl").

do(Data) ->
  Path = Data#mod.request_uri,
  Method = Data#mod.method,
  Header = Data#mod.parsed_header,
  Body = Data#mod.entity_body,
  handle(Path, Method, Header, Body).

-spec handle(Path :: string(), Method :: string(),
             Header :: [{atom() | string(), string()}],
             Body :: iolist()) -> list().
handle("/", "GET", _Header, _Body) ->
  response(200, [], "hello");

handle("/redirect-1", "GET", _Header, _Body) ->
  response(302, [{location, "/redirect-2"}], "");

handle("/redirect-2", "GET", _Header, _Body) ->
  response(302, [{location, "/redirect-3"}], "");

handle("/redirect-3", "GET", _Header, _Body) ->
  response(200, [], "hello");

handle(_Path, _Method, _Header, _Body) ->
  response(404, [], "Not found.").

-spec response(Status :: pos_integer(),
               Header :: [{atom() | string(), string()}],
               Body :: iolist()) -> tuple().
response(Status, Header, Body) ->
  BodyLength = iolist_size(Body),
  Head = [{code, Status},
          {content_length, integer_to_list(BodyLength)}] ++ Header,
  {proceed, [{response, {response, Head, Body}}]}.
