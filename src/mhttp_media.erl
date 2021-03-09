-module(mhttp_media).

-export([type_parameters/1, type_parameter/2, type_parameter/3,
         find_type_parameter/2, has_type_parameter/2,
         format_type/1, parse_type/1]).

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

-spec type_parameters(type()) -> parameters().
type_parameters({_, _}) ->
  [];
type_parameters({_, _, Parameters}) ->
  Parameters.

-spec type_parameter(type(), parameter_name()) -> parameter_value().
type_parameter(Type, Name) ->
  case lists:keyfind(Name, 1, type_parameters(Type)) of
    {_, Value} ->
      Value;
    false ->
      error({unknown_media_type_parameter, Name, Type})
  end.

-spec type_parameter(type(), parameter_name(), parameter_value()) ->
        parameter_value().
type_parameter(Type, Name, DefaultValue) ->
  case lists:keyfind(Name, 1, type_parameters(Type)) of
    {_, Value} ->
      Value;
    false ->
      DefaultValue
  end.

-spec find_type_parameter(type(), parameter_name()) ->
        {ok, parameter_value()} | error.
find_type_parameter(Type, Name) ->
  case lists:keyfind(Name, 1, type_parameters(Type)) of
    {_, Value} ->
      {ok, Value};
    false ->
      error
  end.

-spec has_type_parameter(type(), parameter_name()) -> boolean().
has_type_parameter(Type, Name) ->
  lists:keymember(Name, 1, type_parameters(Type)).

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
