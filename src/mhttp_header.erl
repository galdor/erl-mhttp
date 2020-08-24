%% Copyright (c) 2020 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
%% REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
%% AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT,
%% INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
%% LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
%% OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
%% PERFORMANCE OF THIS SOFTWARE.

-module(mhttp_header).

-export([new/0,
         append/2,
         contains/2, find/2, find_all/2, find_all_concat/2, find_all_split/2,
         add/3, add_field/2, add_if_missing/3,
         content_length/1,
         transfer_encoding/1, has_transfer_encoding/2,
         has_connection_close/1,
         body/1]).

-spec new() -> mhttp:header().
new() ->
  [].

-spec append(mhttp:header(), mhttp:header()) -> mhttp:header().
append(Header1, Header2) ->
  Header1 ++ Header2.

-spec contains(mhttp:header(), mhttp:header_name()) -> boolean().
contains(Header, Name) ->
  case find(Header, Name) of
    {ok, _} ->
      true;
    error ->
      false
  end.

-spec find(mhttp:header(), mhttp:header_name()) ->
        {ok, mhttp:header_value()} | error.
find(Header, Name) ->
  Pred = fun ({FieldName, _}) ->
             mhttp:header_name_equal(FieldName, Name)
         end,
  case lists:search(Pred, Header) of
    {value, {_, Value}} ->
      {ok, Value};
    false ->
      error
  end.

-spec find_all(mhttp:header(), mhttp:header_name()) -> [mhttp:header_value()].
find_all(Header, Name) ->
  lists:filtermap(fun ({FieldName, Value}) ->
                      case mhttp:header_name_equal(FieldName, Name) of
                        true ->
                          {true, Value};
                        false ->
                          false
                      end
                  end, Header).

-spec find_all_concat(mhttp:header(), mhttp:header_name()) ->
        mhttp:header_value().
find_all_concat(Header, Name) ->
  Values = find_all(Header, Name),
  Data = lists:join(<<", ">>, Values),
  iolist_to_binary(Data).

-spec find_all_split(mhttp:header(), mhttp:header_name()) ->
        [mhttp:header_value()].
find_all_split(Header, Name) ->
  case find_all_concat(Header, Name) of
    <<>> ->
      [];
    Value ->
      Values = binary:split(Value, <<",">>, [global]),
      lists:map(fun (V) -> string:trim(V, both, " \t") end, Values)
  end.

-spec add(mhttp:header(), mhttp:header_name(), mhttp:header_value()) ->
        mhttp:header().
add(Header, Name, Value) ->
  [{Name, Value} | Header].

-spec add_field(mhttp:header(), mhttp:header_field()) -> mhttp:header().
add_field(Header, Field) ->
  [Field | Header].

-spec add_if_missing(mhttp:header(),
                     mhttp:header_name(), mhttp:header_value()) ->
        mhttp:header().
add_if_missing(Header, Name, Value) ->
  case contains(Header, Name) of
    true ->
      Header;
    false ->
      add(Header, Name, Value)
  end.

-spec content_length(mhttp:header()) -> {ok, pos_integer()} | error.
content_length(Header) ->
  case mhttp_header:find_all(Header, <<"Content-Length">>) of
    [] ->
      error;
    [Value] ->
      try
        {ok, binary_to_integer(Value)}
      catch
        error:badarg ->
          error({invalid_content_length, Value})
      end;
    _Values ->
      error(multiple_content_length_fields)
  end.

-spec transfer_encoding(mhttp:header()) -> Codings :: [binary()].
transfer_encoding(Header) ->
  Values = mhttp_header:find_all_split(Header, <<"Transfer-Encoding">>),
  lists:map(fun string:lowercase/1, Values).

-spec has_transfer_encoding(mhttp:header(), Coding :: binary()) -> boolean().
has_transfer_encoding(Header, Coding) ->
  lists:member(string:lowercase(Coding), transfer_encoding(Header)).

-spec has_connection_close(mhttp:header()) -> boolean().
has_connection_close(Header) ->
  Values0 = mhttp_header:find_all_split(Header, <<"Connection">>),
  Values = lists:map(fun string:lowercase/1, Values0),
  lists:member(<<"close">>, Values).

-spec body(mhttp:header()) -> {fixed, pos_integer()} | chunked | none.
body(Header) ->
  case mhttp_header:content_length(Header) of
    {ok, Length} ->
      {fixed, Length};
    error ->
      case mhttp_header:has_transfer_encoding(Header, <<"chunked">>) of
        true ->
          chunked;
        false ->
          none
      end
    end.
