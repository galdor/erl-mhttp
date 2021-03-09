-module(mhttp_media_range_tests).

-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
  [?_assertEqual({ok, {<<"text">>, <<"plain">>}},
                 mhttp_media_range:parse(<<"text/plain">>)),
   ?_assertEqual({ok, {<<"text">>, any}},
                 mhttp_media_range:parse(<<"text/*">>)),
   ?_assertEqual({ok, {any, any}},
                 mhttp_media_range:parse(<<"*/*">>)),
   ?_assertEqual({ok, {<<"text">>, any, 0.8, []}},
                 mhttp_media_range:parse(<<"text/*;q=0.8">>)),
   ?_assertEqual({ok, {<<"text">>, <<"plain">>, 1.0, []}},
                 mhttp_media_range:parse(<<"text/plain ;\tq =\t 1.0">>)),
   ?_assertEqual({ok, {<<"text">>, <<"plain">>, 1.0,
                       [{<<"a">>, <<"1">>}, {<<"b">>, <<"2">>}]}},
                 mhttp_media_range:parse(<<"text/plain;q=1.0;a=1;b=2">>)),
   ?_assertEqual({error, invalid_wildcard},
                 mhttp_media_range:parse(<<"*/foo">>)),
   ?_assertEqual({error, {invalid_weight, <<"1foo">>}},
                 mhttp_media_range:parse(<<"text/*;q=1foo">>)),
   ?_assertEqual({error, missing_weight},
                 mhttp_media_range:parse(<<"*/*;a=1">>))].
