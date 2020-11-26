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

-module(mhttp_uri_tests).

-include_lib("eunit/include/eunit.hrl").

strip_path_prefix_test_() ->
  Strip = fun mhttp_uri:strip_path_prefix/2,
  [?_assertEqual(#{path => <<"/bar">>},
                 Strip(#{path => <<"/foo/bar">>}, <<"/foo">>)),
   ?_assertEqual(#{path => <<"/oo/bar">>},
                 Strip(#{path => <<"/foo/bar">>}, <<"/f">>)),
   ?_assertEqual(#{path => <<"/foo/bar">>},
                 Strip(#{path => <<"/foo/bar">>}, <<"/">>)),
   ?_assertEqual(#{path => <<"/">>},
                 Strip(#{path => <<"/foo/bar">>}, <<"/foo/bar">>)),
   ?_assertEqual(#{path => <<"/foo/bar">>},
                 Strip(#{path => <<"/foo/bar">>}, <<"/bar">>)),
   ?_assertEqual(#{path => <<"/">>},
                 Strip(#{path => <<"/">>}, <<"/foo">>)),
   ?_assertEqual(#{path => <<"/">>},
                 Strip(#{path => <<"/">>}, <<"/">>))].
