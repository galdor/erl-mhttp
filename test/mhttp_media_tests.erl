-module(mhttp_media_tests).

-include_lib("eunit/include/eunit.hrl").

format_type_test_() ->
  [?_assertEqual(<<"text/plain">>,
                mhttp_media:format_type({<<"text">>, <<"plain">>})),
   ?_assertEqual(<<"text/plain;encoding=UTF-8">>,
                 mhttp_media:format_type({<<"text">>, <<"plain">>,
                                          [{<<"encoding">>, <<"UTF-8">>}]})),
   ?_assertEqual(<<"x-mhttp/test;a=1;b=2">>,
                 mhttp_media:format_type({<<"x-mhttp">>, <<"test">>,
                                          [{<<"a">>, <<"1">>},
                                           {<<"b">>, <<"2">>}]}))].

parse_type_test_() ->
  [?_assertEqual({ok, {<<"text">>, <<"plain">>}},
                 mhttp_media:parse_type(<<"text/plain">>)),
   ?_assertEqual({ok, {<<"Text">>, <<"Plain">>}},
                 mhttp_media:parse_type(<<"Text/Plain">>)),
   ?_assertEqual({ok, {<<"text">>, <<"plain">>,
                       [{<<"encoding">>, <<"UTF-8">>}]}},
                 mhttp_media:parse_type(<<"text/plain;encoding=UTF-8">>)),
   ?_assertEqual({ok, {<<"x-mhttp">>, <<"test">>,
                       [{<<"a">>, <<"1">>}, {<<"b">>, <<"2">>}]}},
                 mhttp_media:parse_type(<<"x-mhttp/test;a=1;b=2">>)),
   ?_assertEqual({ok, {<<"x-mhttp">>, <<"test">>,
                       [{<<"a">>, <<"1">>}, {<<"b">>, <<"2">>}]}},
                 mhttp_media:parse_type(<<"x-mhttp/test ;  a =1\t;\nb=  2">>)),
   ?_assertEqual({error, invalid_format},
                 mhttp_media:parse_type(<<"foo">>)),
   ?_assertEqual({error, invalid_format},
                 mhttp_media:parse_type(<<"foo/">>)),
   ?_assertEqual({error, invalid_format},
                 mhttp_media:parse_type(<<"/bar">>)),
   ?_assertEqual({error, invalid_format},
                 mhttp_media:parse_type(<<"/">>)),
   ?_assertEqual({error, {invalid_parameter, <<"">>}},
                 mhttp_media:parse_type(<<"foo/bar;">>)),
   ?_assertEqual({error, {invalid_parameter, <<"a=">>}},
                 mhttp_media:parse_type(<<"foo/bar;a=">>)),
   ?_assertEqual({error, {invalid_parameter, <<"=1">>}},
                 mhttp_media:parse_type(<<"foo/bar;=1">>)),
   ?_assertEqual({error, {invalid_parameter, <<"=">>}},
                 mhttp_media:parse_type(<<"foo/bar;=">>))].
