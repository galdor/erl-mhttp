%% Copyright (c) 2020-2021 Exograd SAS.
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

-module(mhttp_header_tests).

-include_lib("eunit/include/eunit.hrl").

contains_test_() ->
  Contains = fun mhttp_header:contains/2,
  [?_assertNot(Contains([], <<"a">>)),
   ?_assertNot(Contains([{<<"b">>, <<"1">>}], <<"a">>)),
   ?_assert(Contains([{<<"a">>, <<"1">>}], <<"a">>)),
   ?_assert(Contains([{<<"a">>, <<"1">>}], <<"A">>)),
   ?_assert(Contains([{<<"b">>, <<"1">>}, {<<"a">>, <<"2">>}], <<"a">>)),
   ?_assert(Contains([{<<"b">>, <<"1">>}, {<<"a">>, <<"2">>},
                      {<<"c">>, <<"3">>}], <<"a">>))].

find_test_() ->
  Find = fun mhttp_header:find/2,
  [?_assertEqual(error,
                 Find([], <<"a">>)),
   ?_assertEqual(error,
                 Find([{<<"b">>, <<"1">>}], <<"a">>)),
   ?_assertEqual({ok, <<"1">>},
                 Find([{<<"a">>, <<"1">>}], <<"a">>)),
   ?_assertEqual({ok, <<"1">>},
                 Find([{<<"a">>, <<"1">>}], <<"A">>)),
   ?_assertEqual({ok, <<"1">>},
                 Find([{<<"a">>, <<"1">>}, {<<"a">>, <<"2">>}], <<"a">>)),
   ?_assertEqual({ok, <<"2">>},
                 Find([{<<"b">>, <<"1">>}, {<<"a">>, <<"2">>}], <<"a">>)),
   ?_assertEqual({ok, <<"2">>},
                 Find([{<<"b">>, <<"1">>}, {<<"a">>, <<"2">>},
                       {<<"c">>, <<"3">>}], <<"a">>))].

find_all_test_() ->
  FindAll = fun mhttp_header:find_all/2,
  [?_assertEqual([],
                 FindAll([], <<"a">>)),
   ?_assertEqual([<<"1">>],
                 FindAll([{<<"a">>, <<"1">>}], <<"a">>)),
   ?_assertEqual([<<"1">>, <<"3">>, <<"4">>],
                 FindAll([{<<"a">>, <<"1">>},
                          {<<"b">>, <<"2">>},
                          {<<"a">>, <<"3">>},
                          {<<"A">>, <<"4">>}], <<"a">>))].

find_all_concat_test_() ->
  FindAllConcat = fun mhttp_header:find_all_concat/2,
  [?_assertEqual(<<>>,
                 FindAllConcat([], <<"a">>)),
   ?_assertEqual(<<"1">>,
                 FindAllConcat([{<<"a">>, <<"1">>}], <<"a">>)),
   ?_assertEqual(<<"1, 3, 4">>,
                 FindAllConcat([{<<"a">>, <<"1">>},
                                {<<"b">>, <<"2">>},
                                {<<"a">>, <<"3">>},
                                {<<"A">>, <<"4">>}], <<"a">>))].

find_all_split_test_() ->
  FindAllSplit = fun mhttp_header:find_all_split/2,
  [?_assertEqual([],
                 FindAllSplit([], <<"a">>)),
   ?_assertEqual([<<"1">>],
                 FindAllSplit([{<<"a">>, <<"1">>}], <<"a">>)),
   ?_assertEqual([<<"1">>, <<"2">>, <<"4">>, <<"5">>, <<"6">>,
                  <<"7">>, <<"foo">>, <<"bar">>, <<"baz">>],
                 FindAllSplit([{<<"a">>, <<"  1 \t,\t  2\t">>},
                               {<<"b">>, <<"3">>},
                               {<<"a">>, <<"4 ,\t5,6">>},
                               {<<"A">>, <<" \t7 \t">>},
                               {<<"A">>, <<"foo, bar ,baz">>}],
                              <<"a">>))].

add_test_() ->
  Add = fun mhttp_header:add/3,
  [?_assertEqual([{<<"a">>, <<"1">>}],
                 Add([], <<"a">>, <<"1">>)),
   ?_assertEqual([{<<"a">>, <<"2">>}, {<<"a">>, 1}],
                 Add([{<<"a">>, 1}], <<"a">>, <<"2">>)),
   ?_assertEqual([{<<"b">>, <<"2">>}, {<<"a">>, 1}],
                 Add([{<<"a">>, 1}], <<"b">>, <<"2">>))].

add_if_missing_test_() ->
  AddIfMissing = fun mhttp_header:add_if_missing/3,
  [?_assertEqual([{<<"a">>, 1}],
                 AddIfMissing([], <<"a">>, 1)),
   ?_assertEqual([{<<"a">>, 1}],
                 AddIfMissing([{<<"a">>, 1}], <<"a">>, 2)),
   ?_assertEqual([{<<"a">>, 1}],
                 AddIfMissing([{<<"a">>, 1}], <<"A">>, 2)),
   ?_assertEqual([{<<"b">>, 1}, {<<"a">>, 2}],
                 AddIfMissing([{<<"b">>, 1}, {<<"a">>, 2}], <<"a">>, 1)),
   ?_assertEqual([{<<"b">>, 1}, {<<"a">>, 2}],
                 AddIfMissing([{<<"b">>, 1}, {<<"a">>, 2}], <<"a">>, 3)),
   ?_assertEqual([{<<"a">>, 3}, {<<"b">>, 1}, {<<"c">>, 2}],
                 AddIfMissing([{<<"b">>, 1}, {<<"c">>, 2}], <<"a">>, 3))].

content_length_test_() ->
  ContentLength = fun mhttp_header:content_length/1,
  [?_assertEqual({ok, 42},
                 ContentLength([{<<"Content-Length">>, <<"42">>}])),
   ?_assertEqual({error, not_found},
                 ContentLength([])),
   ?_assertEqual({error, invalid_content_length},
                 ContentLength([{<<"Content-Length">>, <<"foo">>}])),
   ?_assertEqual({error, multiple_content_length_fields},
                 ContentLength([{<<"Content-Length">>, <<"42">>},
                                {<<"Content-Length">>, <<"43">>}]))].

transfer_encoding_test_() ->
  TransferEncoding = fun mhttp_header:transfer_encoding/1,
  [?_assertEqual([],
                 TransferEncoding([])),
   ?_assertEqual([<<"foo">>, <<"bar">>, <<"baz">>],
                 TransferEncoding([{<<"Transfer-Encoding">>,
                                    <<"foo, Bar, BAZ">>}])),
   ?_assertEqual([<<"foo">>, <<"bar">>, <<"baz">>, <<"hello">>],
                 TransferEncoding([{<<"Transfer-Encoding">>, <<"Foo">>},
                                   {<<"Transfer-Encoding">>, <<"bAR, baz">>},
                                   {<<"Transfer-Encoding">>, <<"HELLO">>}]))].

set_cookies_test_() ->
  SetCookies = fun mhttp_header:set_cookies/1,
  [?_assertEqual({ok, []},
                 SetCookies([])),
   ?_assertEqual({ok, [#{name => <<"Foo">>, value => <<"bar">>,
                         secure => true},
                       #{name => <<"Foo">>, value => <<"baz">>}]},
                 SetCookies([{<<"Set-Cookie">>, <<"Foo=bar; Secure">>},
                             {<<"Set-Cookie">>, <<"Foo=baz">>}]))].

cookies_test_() ->
  Cookies = fun mhttp_header:cookies/1,
  [?_assertEqual({ok, []},
                 Cookies([])),
   ?_assertEqual({ok, [{<<"Foo">>, <<"bar">>},
                       {<<"A">>, <<"1">>},
                       {<<"B">>, <<>>},
                       {<<"C">>, <<"3">>}]},
                 Cookies([{<<"Cookie">>, <<"Foo=bar">>},
                          {<<"Cookie">>, <<"A=1; B=; C=3">>}]))].

authorization_test_() ->
  Auth = fun mhttp_header:authorization/1,
  [?_assertEqual(error,
                 Auth([])),
   ?_assertEqual({error, {invalid_authorization, invalid_format, <<"foo">>}},
                 Auth([{<<"Authorization">>, <<"foo">>}])),
   ?_assertEqual({ok, <<"Basic">>, <<"Zm9vOmJhcg==">>},
                 Auth([{<<"Authorization">>, <<"Basic Zm9vOmJhcg==">>}])),
   ?_assertEqual({ok, <<"Bearer">>, <<"foo bar baz">>},
                 Auth([{<<"Authorization">>, <<"Bearer foo bar baz">>}]))].
