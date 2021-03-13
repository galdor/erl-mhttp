-module(mhttp_cookies_tests).

-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
  Parse = fun mhttp_cookies:parse/1,
  [
   %% Name
   ?_assertEqual({error, empty_name},
                 Parse(<<"">>)),
   ?_assertEqual({error, empty_name},
                 Parse(<<"=bar">>)),
   ?_assertEqual({error, {invalid_name_character, $\b}},
                 Parse(<<"Foo\bbar=42">>)),
   ?_assertEqual({error, {invalid_name_character, 16#c3}},
                 Parse(<<"Été=42"/utf8>>)),
   ?_assertEqual({error, {invalid_name_character, $?}},
                 Parse(<<"Foo?=42">>)),
   %% Value
   ?_assertEqual({ok, #{name => <<"Foo">>, value => <<"bar">>}},
                 Parse(<<"Foo=bar">>)),
   ?_assertEqual({ok, #{name => <<"Foo">>, value => <<"">>}},
                 Parse(<<"Foo=\"\"">>)),
   ?_assertEqual({ok, #{name => <<"Foo">>, value => <<"bar">>}},
                 Parse(<<"Foo=\"bar\"">>)),
   ?_assertEqual({error, truncated_quoted_value},
                 Parse(<<"Foo=\"">>)),
   ?_assertEqual({error, truncated_quoted_value},
                 Parse(<<"Foo=\"bar">>)),
   ?_assertEqual({error, {invalid_value_character, $,}},
                 Parse(<<"Foo=a,b,c">>)),
   ?_assertEqual({error, {invalid_value_character, 16#c3}},
                 Parse(<<"Foo=été"/utf8>>)),
   ?_assertEqual({error, {invalid_value_character, $\\}},
                 Parse(<<"Foo=a\\b">>)),
   ?_assertEqual({error, {invalid_value_character, $"}},
                 Parse(<<"Foo=a\"b\"c">>)),
   %% Attributes
   ?_assertEqual({error, empty_attribute},
                 Parse(<<"Foo=bar; ; Secure">>)),
   %% Expires
   ?_assertEqual({error, {duplicate_attribute, <<"Expires">>}},
                 Parse(<<"Foo=bar"
                         "; Expires=Sun, 06 Nov 1994 08:49:37 GMT",
                         "; Expires=Sun, 06 Nov 1994 08:49:37 GMT">>)),
   ?_assertEqual({error, {invalid_attribute, <<"Expires">>}},
                 Parse(<<"Foo=bar"
                         "; Expires=">>)),
   ?_assertEqual({error, {invalid_attribute, <<"Expires">>}},
                 Parse(<<"Foo=bar"
                         "; Expires=test">>)),
   ?_assertEqual({ok, #{name => <<"Foo">>, value => <<"bar">>,
                        expires => {{1994,11,6},{8,49,37}}}},
                 Parse(<<"Foo=bar"
                         "; Expires=Sun, 06 Nov 1994 08:49:37 GMT">>)),
   ?_assertEqual({ok, #{name => <<"Foo">>, value => <<"bar">>,
                        expires => {{1994,11,6},{8,49,37}}}},
                 Parse(<<"Foo=bar"
                         "; Expires=Sunday, 06-Nov-94 08:49:37 GMT">>)),
   ?_assertEqual({ok, #{name => <<"Foo">>, value => <<"bar">>,
                        expires => {{1994,11,6},{8,49,37}}}},
                 Parse(<<"Foo=bar"
                         "; Expires=Sun Nov  6 08:49:37 1994">>)),
   %% Max-Age
   ?_assertEqual({error, {duplicate_attribute, <<"Max-Age">>}},
                 Parse(<<"Foo=bar; Max-Age=3600; Max-Age=7200">>)),
   ?_assertEqual({error, {invalid_attribute, <<"Max-Age">>}},
                 Parse(<<"Foo=bar; Max-Age=">>)),
   ?_assertEqual({error, {invalid_attribute, <<"Max-Age">>}},
                 Parse(<<"Foo=bar; Max-Age=foo">>)),
   ?_assertEqual({error, {invalid_attribute, <<"Max-Age">>}},
                 Parse(<<"Foo=bar; Max-Age=42foo">>)),
   ?_assertEqual({error, {invalid_attribute, <<"Max-Age">>}},
                 Parse(<<"Foo=bar; Max-Age=0">>)),
   ?_assertEqual({error, {invalid_attribute, <<"Max-Age">>}},
                 Parse(<<"Foo=bar; Max-Age=03600">>)),
   ?_assertEqual({error, {invalid_attribute, <<"Max-Age">>}},
                 Parse(<<"Foo=bar; Max-Age=-10">>)),
   ?_assertEqual({ok, #{name => <<"Foo">>, value => <<"bar">>,
                        max_age => 3600}},
                 Parse(<<"Foo=bar; Max-Age=3600">>)),
   %% Domain
   ?_assertEqual({error, {duplicate_attribute, <<"Domain">>}},
                 Parse(<<"Foo=bar; Domain=example.com; Domain=example.com">>)),
   ?_assertEqual({error, {invalid_attribute, <<"Domain">>}},
                 Parse(<<"Foo=bar; Domain=">>)),
   ?_assertEqual({ok, #{name => <<"Foo">>, value => <<"bar">>,
                        domain => <<"example.com">>}},
                 Parse(<<"Foo=bar; Domain=example.com">>)),
   %% Path
   ?_assertEqual({error, {duplicate_attribute, <<"Path">>}},
                 Parse(<<"Foo=bar; Path=/; Path=/">>)),
   ?_assertEqual({error, {invalid_attribute, <<"Path">>}},
                 Parse(<<"Foo=bar; Path=">>)),
   ?_assertEqual({ok, #{name => <<"Foo">>, value => <<"bar">>,
                        path => <<"/a/b">>}},
                 Parse(<<"Foo=bar; Path=/a/b">>)),
   %% Secure
   ?_assertEqual({error, {duplicate_attribute, <<"Secure">>}},
                 Parse(<<"Foo=bar; Secure; Secure">>)),
   ?_assertEqual({ok, #{name => <<"Foo">>, value => <<"bar">>,
                        secure => true}},
                 Parse(<<"Foo=bar; Secure">>)),
   %% HttpOnly
   ?_assertEqual({error, {duplicate_attribute, <<"HttpOnly">>}},
                 Parse(<<"Foo=bar; HttpOnly; HttpOnly">>)),
   ?_assertEqual({ok, #{name => <<"Foo">>, value => <<"bar">>,
                        http_only => true}},
                 Parse(<<"Foo=bar; HttpOnly">>)),
   %% Extension
   ?_assertEqual({error, duplicate_extension},
                 Parse(<<"Foo=bar; hello world; bye">>)),
   ?_assertEqual({ok, #{name => <<"Foo">>, value => <<"bar">>,
                        extension => <<"hello world">>}},
                 Parse(<<"Foo=bar; hello world">>)),
   %% Misc
   ?_assertEqual({ok, #{name => <<"Foo">>, value => <<"bar%20baz">>,
                        expires => {{1994,11,6},{8,49,37}},
                        max_age => 3600,
                        domain => <<"example.com">>,
                        path => <<"/">>,
                        secure => true,
                        http_only => true,
                        extension => <<"Hello world!">>}},
                 Parse(<<"Foo=\"bar%20baz\""
                         "; Expires=Sun, 06 Nov 1994 08:49:37 GMT"
                         "; Max-Age=3600"
                         "; Domain=example.com"
                         "; Path=/"
                         "; Secure"
                         "; HttpOnly"
                         "; Hello world!">>))].
