-module(mhttp_media_range).

-export([parse/1]).

-export_type([type/0, weight/0, media_range/0, parameters/0,
              error_reason/0]).

-type type() :: mhttp_media_type:type() | any.
-type weight() :: float().

-type media_range() ::
        {type(), type()}
      | {type(), type(), weight(), parameters()}.

-type parameters() :: mhttp_media_type:parameters().

-type error_reason() ::
        mhttp_media_type:error_reason()
      | invalid_wildcard
      | {invalid_weight, binary()}
      | missing_weight.

-spec parse(binary()) -> {ok, media_range} | {error, error_reason()}.
parse(Data) ->
  case mhttp_media_type:parse(Data) of
    {ok, MediaType} ->
      parse_media_type(MediaType);
    {error, Reason} ->
      {error, Reason}
  end.

-spec parse_media_type(mhttp_media_type:media_type()) ->
        {ok, media_range()} | {error, error_reason()}.
parse_media_type(MediaType) ->
  Type = parse_type(mhttp_media_type:type(MediaType)),
  Subtype = parse_type(mhttp_media_type:subtype(MediaType)),
  Parameters = mhttp_media_type:parameters(MediaType),
  case mhttp_media_type:parameters(MediaType) of
    [] ->
      validate({Type, Subtype});
    _ ->
      case mhttp_media_type:find_parameter(MediaType, <<"q">>) of
        {ok, WeightString} ->
          try
            erlang:binary_to_float(WeightString)
          of
            Weight ->
              Parameters2 = [P || (P = {N, _}) <- Parameters,
                                  string:lowercase(N) =/= <<"q">>],
              validate({Type, Subtype, Weight, Parameters2})
          catch
            error:_ ->
              {error, {invalid_weight, WeightString}}
          end;
        error ->
          {error, missing_weight}
      end
  end.

-spec parse_type(binary()) -> type().
parse_type(<<"*">>) ->
  any;
parse_type(Type) ->
  Type.

-spec validate(media_range()) ->
        {ok, media_range()} | {error, error_reason()}.
validate({any, Subtype}) when Subtype =/= any ->
  {error, invalid_wildcard};
validate({any, Subtype, _, _}) when Subtype =/= any ->
  {error, invalid_wildcard};
validate(MediaRange) ->
  {ok, MediaRange}.
