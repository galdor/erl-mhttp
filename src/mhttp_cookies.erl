-module(mhttp_cookies).

-export([format/1, parse/1,
         format_pairs/1, parse_pairs/1]).

-export_type([cookie/0, error_reason/0]).

%% Reference: RFC 6265.

-type cookie() ::
        #{name := binary(),
          value := binary(),
          expires => calendar:datetime(),
          max_age => integer(),
          domain => binary(),
          path => binary(),
          secure => boolean(),
          http_only => boolean(),
          extension => binary()}.

-type cookie_pair() ::
        {binary(), binary()}.

-type error_reason() ::
        empty_name
      | {invalid_name_character, integer()}
      | truncated_quoted_value
      | {invalid_value_character, integer()}
      | {duplicate_attribute, binary()}
      | {invalid_attribute, binary()}
      | empty_attribute
      | duplicate_extension.

-spec format(cookie()) -> binary().
format(Cookie = #{name := Name, value := Value}) ->
  Attributes = maps:fold(fun format_attribute/3, [], Cookie),
  Data = [Name, $=, Value,
          [[<<"; ">>, Attribute] || Attribute <- Attributes]],
  iolist_to_binary(Data).

-spec format_attribute(atom(), term(), [iodata()]) -> [iodata()].
format_attribute(name, _, Data) ->
  Data;
format_attribute(value, _, Data) ->
  Data;
format_attribute(expires, Datetime, Data) ->
  [[<<"Expires=">>, mhttp_time:format_rfc1123_date(Datetime)] | Data];
format_attribute(max_age, Age, Data) ->
  [[<<"Max-Age=">>, integer_to_binary(Age)] | Data];
format_attribute(domain, Domain, Data) ->
  [[<<"Domain=">>, Domain] | Data];
format_attribute(path, Path, Data) ->
  [[<<"Path=">>, Path] | Data];
format_attribute(secure, true, Data) ->
  [<<"Secure">> | Data];
format_attribute(secure, false, Data) ->
  Data;
format_attribute(http_only, true, Data) ->
  [<<"HttpOnly">> | Data];
format_attribute(http_only, false, Data) ->
  Data;
format_attribute(extension, Value, Data) ->
  [Value | Data].

-spec parse(binary()) -> {ok, cookie()}| {error, error_reason()}.
parse(Data) ->
  try
    {ok, parse_1(Data)}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec parse_1(binary()) -> cookie().
parse_1(Data) ->
  {Name, Rest1} = split2(Data, <<"=">>),
  validate_name(Name),
  {ValueData, Rest2} = split2(Rest1, <<"; ">>),
  Value = validate_value(ValueData),
  Cookie0 = #{name => Name, value => Value},
  case Rest2 of
    <<>> ->
      Cookie0;
    _ ->
      lists:foldl(fun (Extension, Cookie) ->
                      parse_extension(Extension, Cookie)
                  end, Cookie0, binary:split(Rest2, <<"; ">>, [global]))
  end.

-spec parse_extension(binary(), cookie()) -> cookie().
parse_extension(<<"Expires=", _/binary>>, #{expires := _}) ->
  throw({error, {duplicate_attribute, <<"Expires">>}});
parse_extension(<<"Expires=", Data/binary>>, Cookie) ->
  ParseDate = fun
                F([]) ->
                  throw({error, {invalid_attribute, <<"Expires">>}});
                F([Fun | Funs]) ->
                  case Fun(Data) of
                    {ok, Datetime} ->
                      Cookie#{expires => Datetime};
                    {error, _} ->
                      F(Funs)
                  end
              end,
  ParseDate([fun mhttp_time:parse_rfc1123_date/1,
             fun mhttp_time:parse_rfc1036_date/1,
             fun mhttp_time:parse_asctime_date/1]);
parse_extension(<<"Max-Age=", _/binary>>, #{max_age := _}) ->
  throw({error, {duplicate_attribute, <<"Max-Age">>}});
parse_extension(<<"Max-Age=", Data/binary>>, Cookie) ->
  re:run(Data, "^[1-9][0-9]*$") =:= nomatch andalso
    throw({error, {invalid_attribute, <<"Max-Age">>}}),
  MaxAge = binary_to_integer(Data),
  Cookie#{max_age => MaxAge};
parse_extension(<<"Domain=", _/binary>>, #{domain := _}) ->
  throw({error, {duplicate_attribute, <<"Domain">>}});
parse_extension(<<"Domain=">>, _) ->
  throw({error, {invalid_attribute, <<"Domain">>}});
parse_extension(<<"Domain=", Data/binary>>, Cookie) ->
  Cookie#{domain => Data};
parse_extension(<<"Path=", _/binary>>, #{path := _}) ->
  throw({error, {duplicate_attribute, <<"Path">>}});
parse_extension(<<"Path=">>, _) ->
  throw({error, {invalid_attribute, <<"Path">>}});
parse_extension(<<"Path=", Data/binary>>, Cookie) ->
  Cookie#{path => Data};
parse_extension(<<"Secure">>, #{secure := _}) ->
  throw({error, {duplicate_attribute, <<"Secure">>}});
parse_extension(<<"Secure">>, Cookie) ->
  Cookie#{secure => true};
parse_extension(<<"HttpOnly">>, #{http_only := _}) ->
  throw({error, {duplicate_attribute, <<"HttpOnly">>}});
parse_extension(<<"HttpOnly">>, Cookie) ->
  Cookie#{http_only => true};
parse_extension(<<>>, _) ->
  throw({error, empty_attribute});
parse_extension(_, #{extension := _}) ->
  throw({error, duplicate_extension});
parse_extension(Data, Cookie) ->
  Cookie#{extension => Data}.

-spec validate_name(binary()) -> ok.
validate_name(<<>>) ->
  throw({error, empty_name});
validate_name(Name) ->
  validate_name_characters(Name).

-spec validate_name_characters(binary()) -> ok.
validate_name_characters(<<>>) ->
  ok;
validate_name_characters(<<C, _/binary>>) when
    C > 127;
    C < 32; C =:= 127;
    C =:= $(; C =:= $); C =:= $<; C =:= $>; C =:= $@;
    C =:= $,; C =:= $;; C =:= $:; C =:= $\\; C =:= $";
    C =:= $/; C =:= $[; C =:= $]; C =:= $?; C =:= $=;
    C =:= ${; C =:= $}; C =:= $\s; C =:= $\t ->
  throw({error, {invalid_name_character, C}});
validate_name_characters(<<_, Rest/binary>>) ->
  validate_name_characters(Rest).

-spec validate_value(binary()) -> binary().
validate_value(<<>>) ->
  <<>>;
validate_value(<<$">>) ->
  throw({error, truncated_quoted_value});
validate_value(<<$", Data0/binary>>) ->
  case binary:last(Data0) of
    $" ->
      Data = binary:part(Data0, 0, byte_size(Data0)-1),
      validate_value_characters(Data),
      Data;
    _ ->
      throw({error, truncated_quoted_value})
  end;
validate_value(Data) ->
  validate_value_characters(Data),
  Data.

-spec validate_value_characters(binary()) -> ok.
validate_value_characters(<<>>) ->
  ok;
validate_value_characters(<<C, Rest/binary>>) when
    C =:= 16#21;
    C >= 16#23, C =< 16#2b; C >= 16#2d, C =< 16#3a;
    C >= 16#3c, C =< 16#5b; C >= 16#5d, C =< 16#7e ->
  validate_value_characters(Rest);
validate_value_characters(<<C, _/binary>>) ->
  throw({error, {invalid_value_character, C}}).

-spec format_pairs([cookie_pair()]) -> binary().
format_pairs(Pairs) ->
  Data = [[N, $=, V] || {N, V} <- Pairs],
  iolist_to_binary(lists:join(<<"; ">>, Data)).

-spec parse_pairs(binary()) -> {ok, [cookie_pair()]} | {error, error_reason()}.
parse_pairs(Data) ->
  try
    {ok, parse_pairs_1(Data)}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec parse_pairs_1(binary()) -> [cookie_pair()].
parse_pairs_1(<<>>) ->
  [];
parse_pairs_1(Data) ->
  Parts = binary:split(Data, <<"; ">>, [global]),
  lists:map(fun parse_pair/1, Parts).

-spec parse_pair(binary()) -> cookie_pair().
parse_pair(Data) ->
  {Name, ValueData} = split2(Data, <<"=">>),
  validate_name(Name),
  Value = validate_value(ValueData),
  {Name, Value}.

-spec split2(Subject :: binary(), Separator) -> {binary(), binary()} when
    Separator :: binary() | [binary()] | binary:cp().
split2(Subject, Separator) ->
  case binary:split(Subject, Separator) of
    [Value] ->
      {Value, <<>>};
    [Value, Rest] ->
      {Value, Rest}
  end.
