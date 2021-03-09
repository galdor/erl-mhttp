-module(mhttp_media).

-export([format_type/1, parse_type/1]).

-export_type([parameter_name/0, parameter_value/0, parameters/0, type/0]).

-type error_reason() ::
        invalid_format
      | {invalid_parameter, binary()}.

-type parameter_name() :: binary().
-type parameter_value() :: binary().
-type parameter() :: {parameter_name(), parameter_value()}.
-type parameters() :: [parameter()].

-type type() :: {binary(), binary()}
              | {binary(), binary(), parameters()}.

-spec format_type(type()) -> binary().
format_type({Type, Subtype}) ->
  format_type({Type, Subtype, []});
format_type({Type, Subtype, Parameters}) ->
  iolist_to_binary([Type, $/, Subtype, format_parameters(Parameters)]).

-spec format_parameters(parameters()) -> iodata().
format_parameters(Parameters) ->
  [[$;, N, $=, V] || {N, V} <- Parameters].

-spec parse_type(binary()) -> {ok, type()} | {error, error_reason()}.
parse_type(Data) ->
  try
    {ok, parse_type_1(Data)}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec parse_type_1(binary()) -> type().
parse_type_1(Data) ->
  case binary:split(Data, <<"/">>) of
    [<<>>, _] ->
      throw({error, invalid_format});
    [Type, Rest] ->
      case binary:split(Rest, <<";">>) of
        [<<>>, _] ->
          throw({error, invalid_format});
        [Subtype0, ParametersData] ->
          Subtype = string:trim(Subtype0, trailing),
          {Type, Subtype, parse_parameters(ParametersData)};
        [<<>>] ->
          throw({error, invalid_format});
        [Subtype] ->
          {Type, Subtype}
      end;
    [_] ->
      throw({error, invalid_format})
  end.

-spec parse_parameters(binary()) -> parameters().
parse_parameters(Data) ->
  lists:map(fun parse_parameter/1, binary:split(Data, <<";">>, [global])).

-spec parse_parameter(binary()) -> parameter().
parse_parameter(Data) ->
  case binary:split(Data, <<"=">>) of
    [Name, Value] when Name =:= <<>>; Value =:= <<>> ->
      throw({error, {invalid_parameter, Data}});
    [Name, Value] ->
      {string:trim(Name, both), string:trim(Value, both)};
    [_] ->
      throw({error, {invalid_parameter, Data}})
  end.
