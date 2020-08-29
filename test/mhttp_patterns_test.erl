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

-module(mhttp_patterns_test).

-include_lib("eunit/include/eunit.hrl").

match_pattern_test_() ->
  Match = fun mhttp_patterns:match/2,
  [
   %% Simple path patterns
   ?_assertEqual({true, #{}},
                 Match(<<"">>,
                       #{method => get, target => <<"">>})),
   ?_assertEqual({true, #{}},
                 Match(<<"/">>,
                       #{method => get, target => <<"/">>})),
   ?_assertEqual({true, #{}},
                 Match(<<"/a">>,
                       #{method => get, target => <<"/a">>})),
   ?_assertEqual({true, #{}},
                 Match(<<"/a">>,
                       #{method => get, target => <<"/a/">>})),
   ?_assertEqual({true, #{}},
                 Match(<<"/a/">>,
                       #{method => get, target => <<"/a">>})),
   ?_assertEqual({true, #{}},
                 Match(<<"/a">>,
                       #{method => get, target => <<"/a/b">>})),
   ?_assertEqual({true, #{}},
                 Match(<<"/a">>,
                       #{method => get, target => <<"/a/b/c">>})),
   ?_assertEqual({true, #{}},
                 Match(<<"/a/b">>,
                       #{method => get, target => <<"/a/b/c">>})),
   ?_assertEqual(false,
                 Match(<<"/a/b">>,
                       #{method => get, target => <<"/a/c">>})),
   %% Path pattern wildcards
   ?_assertEqual({true, #{}},
                 Match(<<"/*">>,
                       #{method => get, target => <<"/a">>})),
   ?_assertEqual({true, #{}},
                 Match(<<"/*">>,
                       #{method => get, target => <<"/a/b/c">>})),
   ?_assertEqual({true, #{}},
                 Match(<<"/*/*">>,
                       #{method => get, target => <<"/a/b">>})),
   ?_assertEqual({true, #{}},
                 Match(<<"/*/*">>,
                       #{method => get, target => <<"/a/b/c">>})),
   ?_assertEqual(false,
                 Match(<<"/*/*">>,
                       #{method => get, target => <<"/a">>})),
   ?_assertEqual({true, #{}},
                 Match(<<"/*/b/*">>,
                       #{method => get, target => <<"/a/b/c">>})),
   ?_assertEqual(false,
                 Match(<<"/*/b/*">>,
                       #{method => get, target => <<"/a/c/d">>})),
   %% Path pattern captures
   ?_assertEqual({true, #{x => <<"a">>}},
                 Match(<<"/:x">>,
                       #{method => get, target => <<"/a">>})),
   ?_assertEqual({true, #{x => <<"b">>}},
                 Match(<<"/a/:x/c">>,
                       #{method => get, target => <<"/a/b/c">>})),
   ?_assertEqual({true, #{x => <<"c">>}},
                 Match(<<"/a/b/:x">>,
                       #{method => get, target => <<"/a/b/c/d/e">>})),
   ?_assertEqual({true, #{x => <<"a">>, y => <<"c">>}},
                 Match(<<"/:x/b/:y">>,
                       #{method => get, target => <<"/a/b/c">>})),
   ?_assertEqual({true, #{x => <<"a">>, y => <<"c">>}},
                 Match(<<"/:x/b/:y">>,
                       #{method => get, target => <<"/a/b/c/d">>})),
   %% Path and method
   ?_assertEqual({true, #{}},
                 Match({<<"/a">>, get},
                       #{method => get, target => <<"/a">>})),
   ?_assertEqual(false,
                 Match({<<"/a">>, get},
                       #{method => post, target => <<"/a">>})),
   ?_assertEqual(false,
                 Match({<<"/a">>, get},
                       #{method => get, target => <<"/b">>})),
   %% Path and filters
   ?_assertEqual({true, #{}},
                 Match({<<"/a">>, []},
                       #{method => get, target => <<"/a">>})),
   ?_assertEqual({true, #{}},
                 Match({<<"/a">>, []},
                       #{method => post, target => <<"/a">>})),
   ?_assertEqual(false,
                 Match({<<"/a">>, []},
                       #{method => get, target => <<"/b">>})),
   ?_assertEqual({true, #{}},
                 Match({<<"/a">>, [mhttp_filters:header_contains(<<"Foo">>)]},
                       #{method => get, target => <<"/a">>,
                         header => [{<<"Foo">>, <<"1">>}]})),
   ?_assertEqual(false,
                 Match({<<"/a">>, [mhttp_filters:header_contains(<<"Foo">>)]},
                       #{method => get, target => <<"/a">>,
                         header => []})),
   ?_assertEqual(false,
                 Match({<<"/a">>, [mhttp_filters:header_contains(<<"Foo">>)]},
                       #{method => get, target => <<"/b">>,
                         header => [{<<"Foo">>, <<"1">>}]})),
   %% Path, method and filters
   ?_assertEqual({true, #{}},
                 Match({<<"/a">>, get,
                        [mhttp_filters:header_contains(<<"Foo">>)]},
                       #{method => get, target => <<"/a">>,
                         header => [{<<"Foo">>, <<"1">>}]})),
   ?_assertEqual(false,
                 Match({<<"/a">>, post,
                        [mhttp_filters:header_contains(<<"Foo">>)]},
                       #{method => get, target => <<"/a">>,
                         header => [{<<"Foo">>, <<"1">>}]})),
   ?_assertEqual(false,
                 Match({<<"/a">>, get,
                        [mhttp_filters:header_contains(<<"Foo">>)]},
                       #{method => get, target => <<"/b">>,
                         header => [{<<"Foo">>, <<"1">>}]})),
   ?_assertEqual(false,
                 Match({<<"/a">>, get,
                        [mhttp_filters:header_contains(<<"Foo">>)]},
                       #{method => get, target => <<"/a">>,
                         header => []}))].