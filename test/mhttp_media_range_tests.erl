-module(mhttp_media_range_tests).

-include_lib("eunit/include/eunit.hrl").

match_test_() ->
  Match = fun
            (RangeStrings, TypeString) when is_list(RangeStrings) ->
              Ranges = [Range ||
                         {ok, Range} <- [mhttp_media_range:parse(S) ||
                                          S <- RangeStrings]],
              {ok, Type} = mhttp_media_type:parse(TypeString),
              mhttp_media_range:match(Ranges, Type);
            (RangeString, TypeString) ->
              {ok, Range} = mhttp_media_range:parse(RangeString),
              {ok, Type} = mhttp_media_type:parse(TypeString),
              mhttp_media_range:match(Range, Type)
          end,
  [?_assert(Match(<<"*/*">>, <<"text/plain">>)),
   ?_assert(Match(<<"text/*">>, <<"text/plain">>)),
   ?_assert(Match(<<"text/plain">>, <<"Text/plaiN">>)),
   ?_assert(Match(<<"text/plain;q=0.8">>, <<"Text/plaiN">>)),
   ?_assert(Match(<<"text/plain;q=0.8;a=1">>, <<"Text/plaiN;a=1;b=2">>)),
   ?_assertNot(Match(<<"text/*">>, <<"application/json">>)),
   ?_assertNot(Match(<<"text/plain">>, <<"application/json">>)),
   ?_assert(Match([<<"text/*">>, <<"video/*">>], <<"text/plain">>)),
   ?_assert(Match([<<"text/*">>, <<"video/*">>], <<"video/vc1">>)),
   ?_assertNot(Match([], <<"image/png">>)),
   ?_assertNot(Match([<<"text/*">>, <<"video/*">>], <<"image/png">>))].

parse_test_() ->
  Parse = fun mhttp_media_range:parse/1,
  [?_assertEqual({ok, {<<"text">>, <<"plain">>}},
                 Parse(<<"text/plain">>)),
   ?_assertEqual({ok, {<<"text">>, any}},
                 Parse(<<"text/*">>)),
   ?_assertEqual({ok, {any, any}},
                 Parse(<<"*/*">>)),
   ?_assertEqual({ok, {<<"text">>, any, 0.8, []}},
                 Parse(<<"text/*;q=0.8">>)),
   ?_assertEqual({ok, {<<"text">>, <<"plain">>, 1.0, []}},
                 Parse(<<"text/plain ;\tq =\t 1.0">>)),
   ?_assertEqual({ok, {<<"text">>, <<"plain">>, 1.0,
                       [{<<"a">>, <<"1">>}, {<<"b">>, <<"2">>}]}},
                 Parse(<<"text/plain;q=1.0;a=1;b=2">>)),
   ?_assertEqual({error, invalid_wildcard},
                 Parse(<<"*/foo">>)),
   ?_assertEqual({error, {invalid_weight, <<"1foo">>}},
                 Parse(<<"text/*;q=1foo">>)),
   ?_assertEqual({error, missing_weight},
                 Parse(<<"*/*;a=1">>))].
