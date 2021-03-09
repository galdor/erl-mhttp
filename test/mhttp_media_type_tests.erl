-module(mhttp_media_type_tests).

-include_lib("eunit/include/eunit.hrl").

format_test_() ->
  [?_assertEqual(<<"text/plain">>,
                 mhttp_media_type:format({<<"text">>, <<"plain">>})),
   ?_assertEqual(<<"text/plain;encoding=UTF-8">>,
                 mhttp_media_type:format({<<"text">>, <<"plain">>,
                                          [{<<"encoding">>, <<"UTF-8">>}]})),
   ?_assertEqual(<<"x-mhttp/test;a=1;b=2">>,
                 mhttp_media_type:format({<<"x-mhttp">>, <<"test">>,
                                          [{<<"a">>, <<"1">>},
                                           {<<"b">>, <<"2">>}]}))].

parse_test_() ->
  [?_assertEqual({ok, {<<"text">>, <<"plain">>}},
                 mhttp_media_type:parse(<<"text/plain">>)),
   ?_assertEqual({ok, {<<"Text">>, <<"Plain">>}},
                 mhttp_media_type:parse(<<"Text/Plain">>)),
   ?_assertEqual({ok, {<<"text">>, <<"plain">>,
                       [{<<"encoding">>, <<"UTF-8">>}]}},
                 mhttp_media_type:parse(<<"text/plain;encoding=UTF-8">>)),
   ?_assertEqual({ok, {<<"x-mhttp">>, <<"test">>,
                       [{<<"a">>, <<"1">>}, {<<"b">>, <<"2">>}]}},
                 mhttp_media_type:parse(<<"x-mhttp/test;a=1;b=2">>)),
   ?_assertEqual({ok, {<<"x-mhttp">>, <<"test">>,
                       [{<<"a">>, <<"1">>}, {<<"b">>, <<"2">>}]}},
                 mhttp_media_type:parse(<<"x-mhttp/test ;  a =1\t;\nb=  2">>)),
   ?_assertEqual({error, invalid_format},
                 mhttp_media_type:parse(<<"foo">>)),
   ?_assertEqual({error, invalid_format},
                 mhttp_media_type:parse(<<"foo/">>)),
   ?_assertEqual({error, invalid_format},
                 mhttp_media_type:parse(<<"/bar">>)),
   ?_assertEqual({error, invalid_format},
                 mhttp_media_type:parse(<<"/">>)),
   ?_assertEqual({error, {invalid_parameter, <<"">>}},
                 mhttp_media_type:parse(<<"foo/bar;">>)),
   ?_assertEqual({error, {invalid_parameter, <<"a=">>}},
                 mhttp_media_type:parse(<<"foo/bar;a=">>)),
   ?_assertEqual({error, {invalid_parameter, <<"=1">>}},
                 mhttp_media_type:parse(<<"foo/bar;=1">>)),
   ?_assertEqual({error, {invalid_parameter, <<"=">>}},
                 mhttp_media_type:parse(<<"foo/bar;=">>))].

normalize_test_() ->
  [?_assertEqual({<<"text">>, <<"plain">>},
                 mhttp_media_type:normalize({<<"Text">>, <<"plaiN">>})),
   ?_assertEqual({<<"text">>, <<"plain">>},
                 mhttp_media_type:normalize({<<"Text">>, <<"plaiN">>})),
   ?_assertEqual({<<"text">>, <<"plain">>, [{<<"encoding">>, <<"UTF-8">>}]},
                 mhttp_media_type:normalize({<<"Text">>, <<"plaiN">>,
                                             [{<<"EncodinG">>,
                                               <<"UTF-8">>}]}))].
