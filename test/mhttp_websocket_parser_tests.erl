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

-module(mhttp_websocket_parser_tests).

-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
  P = fun (Data) ->
          P = mhttp_websocket_parser:new(Data),
          mhttp_websocket_parser:parse_all(P)
      end,
  [%% Truncated header
   ?_assertMatch({ok, [], _},
                 P(<<>>)),
   ?_assertMatch({ok, [], _},
                 P(<<1:1, 0:3, 1:4>>)),
   %% Truncated payload length
   ?_assertMatch({ok, [], _},
                 P(<<1:1, 0:3, 1:4, 0:1, 127:7, 0:48>>)),
   ?_assertMatch({ok, [], _},
                 P(<<1:1, 0:3, 1:4, 0:1, 126:7, 0:8>>)),
   ?_assertMatch({ok, [], _},
                 P(<<1:1, 0:3, 1:4, 0:1, 1:7>>)),
   %% Truncated masking key
   ?_assertMatch({ok, [], _},
                 P(<<1:1, 0:3, 1:4, 1:1, 0:7>>)),
   ?_assertMatch({ok, [], _},
                 P(<<1:1, 0:3, 1:4, 1:1, 0:7, 0:16>>)),
   %% Truncated payload
   ?_assertMatch({ok, [], _},
                 P(<<1:1, 0:3, 1:4, 0:1, 2:7, 0:8>>)),
   %% Valid data frames
   ?_assertMatch({ok, [{data, text, <<>>}], _},
                 P(<<1:1, 0:3, 1:4, 0:1, 0:7>>)),
   ?_assertMatch({ok, [{data, text, <<"a">>}], _},
                 P(<<1:1, 0:3, 1:4, 0:1, 1:7, $a>>)),
   ?_assertMatch({ok, [{data, binary, <<>>}], _},
                 P(<<1:1, 0:3, 2:4, 0:1, 0:7>>)),
   ?_assertMatch({ok, [{data, binary, <<"abc">>}], _},
                 P(<<1:1, 0:3, 2:4, 0:1, 3:7, "abc">>)),
   ?_assertMatch({ok, [{data, binary, <<"abc">>},
                       {data, text, <<"">>},
                       {data, text, <<"de">>}],
                  _},
                 P(<<1:1, 0:3, 2:4, 0:1, 3:7, "abc",
                     1:1, 0:3, 1:4, 0:1, 0:7,
                     1:1, 0:3, 1:4, 0:1, 2:7, "de">>)),
   %% Missing next fragment
   ?_assertMatch({ok, [], _},
                 P(<<0:1, 0:3, 1:4, 0:1, 2:7, $a>>)),
   %% Truncated next fragment
   ?_assertMatch({ok, [], _},
                 P(<<0:1, 0:3, 1:4, 0:1, 2:7, $a,
                     1:1, 0:3, 1:4, 0:1, 1:7>>)),
   %% Invalid interleaved data frames
   ?_assertEqual({error, interleaved_data_frames},
                 P(<<0:1, 0:3, 1:4, 0:1, 1:7, $a,
                     1:1, 0:3, 2:4, 0:1, 2:7, "bc">>)),
   %% Invalid fragmented control frames
   ?_assertMatch({error, fragmented_control_frame},
                 P(<<0:1, 0:3, 8:4, 0:1, 0:7>>)),
   ?_assertMatch({error, fragmented_control_frame},
                 P(<<0:1, 0:3, 9:4, 0:1, 0:7>>)),
   ?_assertMatch({error, fragmented_control_frame},
                 P(<<0:1, 0:3, 10:4, 0:1, 3:7, "abc">>)),
   %% Valid fragmented data frames
   ?_assertMatch({ok, [{data, text, <<"abc">>}], _},
                 P(<<0:1, 0:3, 1:4, 0:1, 1:7, $a,
                     1:1, 0:3, 0:4, 0:1, 2:7, "bc">>)),
   ?_assertMatch({ok, [{data, binary, <<"abcdef">>}], _},
                 P(<<0:1, 0:3, 2:4, 0:1, 3:7, "abc",
                     0:1, 0:3, 0:4, 0:1, 2:7, "de",
                     1:1, 0:3, 0:4, 0:1, 1:7, "f">>)),
   %% Control frame between data fragments
   ?_assertMatch({ok, [{ping, <<>>},
                       {data, binary, <<"abcdef">>}],
                  _},
                 P(<<0:1, 0:3, 2:4, 0:1, 3:7, "abc",
                     1:1, 0:3, 9:4, 0:1, 0:7,
                     1:1, 0:3, 0:4, 0:1, 3:7, "def">>)),
   ?_assertMatch({ok, [{pong, <<"a">>},
                       {close, <<"c">>},
                       {data, text, <<"abcdef">>}],
                  _},
                 P(<<0:1, 0:3, 1:4, 0:1, 3:7, "abc",
                     1:1, 0:3, 10:4, 0:1, 1:7, $a,
                     0:1, 0:3, 0:4, 0:1, 1:7, "d",
                     1:1, 0:3, 8:4, 0:1, 1:7, $c,
                     1:1, 0:3, 0:4, 0:1, 2:7, "ef">>))
   %% TODO masking key
].
