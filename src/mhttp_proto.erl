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

-module(mhttp_proto).

-export([encode_request/1,
         encode_request_line/3, encode_status_line/3, encode_header/1,
         parse_request_line/1, parse_status_line/1,
         parse_header_field/1,
         parse_chunk_header/1]).

-spec encode_request(mhttp:request()) -> iodata().
encode_request(Request = #{method := Method, target := Target}) ->
  Version = maps:get(version, Request, http_1_1),
  Header = mhttp_request:header(Request),
  Body = maps:get(body, Request, <<>>),
  Trailer = maps:get(trailer, Request, []),
  [mhttp_proto:encode_request_line(Method, Target, Version),
   mhttp_proto:encode_header(Header),
   <<"\r\n">>,
   Body,
   mhttp_proto:encode_header(Trailer)].

-spec encode_request_line(mhttp:method(), mhttp:target(), mhttp:version()) ->
        iodata().
encode_request_line(Method, Target, Version) ->
  [encode_method(Method), $\s, encode_target(Target), $\s,
   encode_version(Version), <<"\r\n">>].

-spec encode_status_line(mhttp:version(), mhttp:status(), Reason) ->
        iodata() when
    Reason :: binary().
encode_status_line(Version, Status, Reason) ->
  [encode_version(Version), $\s, encode_status(Status), $\s,
   Reason, <<"\r\n">>].

-spec encode_header(mhttp:header()) -> iodata().
encode_header(Header) ->
  lists:map(fun (Field) ->
                [encode_header_field(Field), <<"\r\n">>]
            end, Header).

-spec encode_header_field(mhttp:header_field()) -> iodata().
encode_header_field({Name, Value}) ->
  [Name, <<": ">>, Value].

-spec encode_method(mhttp:method()) -> iodata().
encode_method(Method) when is_atom(Method) ->
  string:uppercase(atom_to_binary(Method));
encode_method(Method) when is_binary(Method) ->
  Method.

-spec encode_target(mhttp:target()) -> iodata().
encode_target(Target) when is_map(Target) ->
  uri:serialize(Target);
encode_target(Target) ->
  Target.

-spec encode_version(mhttp:version()) -> iodata().
encode_version(http_1_0) ->
  <<"HTTP/1.0">>;
encode_version(http_1_1) ->
  <<"HTTP/1.1">>;
encode_version(Version) when is_binary(Version) ->
  Version.

-spec encode_status(mhttp:status()) -> iodata().
encode_status(Status) ->
  integer_to_binary(Status).

-spec parse_request_line(binary()) -> {mhttp:method(), mhttp:target(),
                                       mhttp:version()}.
parse_request_line(Line) ->
  case binary:split(Line, <<" ">>, [global]) of
    [Method, Target, Version] ->
      {parse_method(Method), parse_target(Target), parse_version(Version)};
    _ ->
      error({invalid_request_line, Line})
  end.

-spec parse_status_line(binary()) -> {mhttp:version(), mhttp:status(),
                                      Reason :: binary()}.
parse_status_line(Line) ->
  case binary:split(Line, <<" ">>) of
    [Version, Rest] ->
      case binary:split(Rest, <<" ">>) of
        [Status, Reason] ->
          {parse_version(Version), parse_status(Status), Reason};
        _ ->
          error({truncated_status, Rest})
      end;
    _ ->
      error({truncated_version, Line})
  end.

-spec parse_method(binary()) -> mhttp:method().
parse_method(Data) ->
  case string:lowercase(Data) of
    <<"get">> ->
      get;
    <<"head">> ->
      head;
    <<"post">> ->
      post;
    <<"put">> ->
      put;
    <<"delete">> ->
      delete;
    <<"connect">> ->
      connect;
    <<"options">> ->
      options;
    <<"trace">> ->
      trace;
    _ ->
      Data
  end.

-spec parse_target(binary()) -> mhttp:target().
parse_target(Data) ->
  uri:parse(Data).

-spec parse_version(binary()) -> mhttp:version().
parse_version(<<"HTTP/1.0">>) ->
  http_1_0;
parse_version(<<"HTTP/1.1">>) ->
  http_1_1;
parse_version(Data) ->
  case re:run(Data, <<"HTTP/[0-9].[0-9]">>, [anchored]) of
    nomatch ->
      error({invalid_version, Data});
    _ ->
      Data
  end.

-spec parse_status(binary()) -> mhttp:status().
parse_status(Data) ->
  try
    binary_to_integer(Data)
  catch
    error:badarg ->
      error({invalid_status, Data})
  end.

-spec parse_header_field(binary()) -> mhttp:header_field().
parse_header_field(Data) ->
  case binary:split(Data, <<":">>) of
    [Name, Value] ->
      {Name, string:trim(Value, both, " \t")};
    _ ->
      error({invalid_header_field, Data})
  end.

-spec parse_chunk_header(binary()) -> Length :: non_neg_integer().
parse_chunk_header(Data) ->
  LengthData = case binary:split(Data, <<";">>) of
                 [Len, _ExtData] ->
                   Len;
                 [Len] ->
                   Len
               end,
  try
    erlang:binary_to_integer(LengthData, 16)
  catch
    error:badarg ->
      error({invalid_chunk_length, LengthData})
  end.
