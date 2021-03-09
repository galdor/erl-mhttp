-module(mhttp_media_type).

-export([type/1, subtype/1,
         parameters/1, parameter/2, parameter/3,
         find_parameter/2, has_parameter/2,
         format/1, parse/1,
         normalize/1]).

-export_type([parameter_name/0, parameter_value/0, parameters/0, type/0,
              error_reason/0]).

-type parameter_name() :: binary().
-type parameter_value() :: binary().
-type parameter() :: {parameter_name(), parameter_value()}.
-type parameters() :: [parameter()].

-type type() :: {binary(), binary()}
              | {binary(), binary(), parameters()}.

-type error_reason() ::
        invalid_format
      | {invalid_parameter, binary()}.

-spec type(type()) -> binary().
type({Type, _}) ->
  Type;
type({Type, _, _}) ->
  Type.

-spec subtype(type()) -> binary().
subtype({_, Subtype}) ->
  Subtype;
subtype({_, Subtype, _}) ->
  Subtype.

-spec parameters(type()) -> parameters().
parameters({_, _}) ->
  [];
parameters({_, _, Parameters}) ->
  Parameters.

-spec parameter(type(), parameter_name()) -> parameter_value().
parameter(Type, Name) ->
  case find_parameter(Type, Name) of
    {ok, Value} ->
      Value;
    error ->
      error({unknown_media_type_parameter, Name, Type})
  end.

-spec parameter(type(), parameter_name(), parameter_value()) ->
        parameter_value().
parameter(Type, Name, DefaultValue) ->
  case find_parameter(Type, Name) of
    {ok, Value} ->
      Value;
    error ->
      DefaultValue
  end.

-spec find_parameter(type(), parameter_name()) ->
        {ok, parameter_value()} | error.
find_parameter(Type, Name0) ->
  Name = string:lowercase(Name0),
  Pred = fun ({N, _}) ->
             string:lowercase(N) =:= Name
         end,
  case lists:search(Pred, parameters(Type)) of
    {value, {_, Value}} ->
      {ok, Value};
    false ->
      error
  end.

-spec has_parameter(type(), parameter_name()) -> boolean().
has_parameter(Type, Name) ->
  case find_parameter(Type, Name) of
    {ok, _} ->
      true;
    error ->
      false
  end.

-spec format(type()) -> binary().
format({Type, Subtype}) ->
  format({Type, Subtype, []});
format({Type, Subtype, Parameters}) ->
  iolist_to_binary([Type, $/, Subtype, format_parameters(Parameters)]).

-spec format_parameters(parameters()) -> iodata().
format_parameters(Parameters) ->
  [[$;, N, $=, V] || {N, V} <- Parameters].

-spec parse(binary()) -> {ok, type()} | {error, error_reason()}.
parse(Data) ->
  try
    {ok, parse_1(Data)}
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec parse_1(binary()) -> type().
parse_1(Data) ->
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

-spec normalize(type()) -> type().
normalize({Type, Subtype}) ->
  {string:lowercase(Type), string:lowercase(Subtype)};
normalize({Type, Subtype, Parameters}) ->
  {string:lowercase(Type), string:lowercase(Subtype),
   [{string:lowercase(N), V} || {N, V} <- Parameters]}.
