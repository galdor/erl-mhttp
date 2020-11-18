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

-module(mhttp_parser).

-export([new/1, parse/2]).

-export_type([msg_type/0, msg/0, parser/0,
              parse_result/0, parse_error_reason/0]).

-type msg_type() :: request | response.
-type msg() :: mhttp:request() | mhttp:response().

-type parser() :: #{data := binary(),
                    state := state(),
                    msg_type := msg_type(),
                    msg => msg()}.

-type state() :: initial
               | request_line | status_line
               | header | body | chunked_body | trailer
               | final.

-type parse_result() :: {ok, msg(), parser()}
                      | {more, parser()}
                      | {error, parse_error_reason()}.

-type parse_error_reason() :: term(). % TODO

-spec new(msg_type()) -> parser().
new(MsgType) ->
  #{data => <<>>,
    state => initial,
    msg_type => MsgType}.

-spec parse(parser(), binary()) -> parse_result().
parse(P = #{data := Data}, NewData) ->
  try
    parse(P#{data => <<Data/binary, NewData/binary>>})
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec parse(parser()) -> parse_result().

parse(P = #{msg_type := request, state := initial}) ->
  parse(P#{state => request_line});

parse(P = #{msg_type := response, state := initial}) ->
  parse(P#{state => status_line});

parse(P = #{data := Data, state := request_line}) ->
  case binary:split(Data, <<"\r\n">>) of
    [Line, Rest] ->
      {Method, Target, Version} = mhttp_proto:parse_request_line(Line),
      Request = #{method => Method,
                  target => Target,
                  version => Version,
                  header => mhttp_header:new(),
                  body => <<>>,
                  trailer => mhttp_header:new()},
      parse(P#{data => Rest, state => header, msg => Request});
    _ ->
      {more, P}
  end;

parse(P = #{data := Data, state := status_line}) ->
  case binary:split(Data, <<"\r\n">>) of
    [Line, Rest] ->
      {Version, Status, Reason} = mhttp_proto:parse_status_line(Line),
      Response = #{status => Status,
                   reason => Reason,
                   version => Version,
                   header => mhttp_header:new(),
                   body => <<>>,
                   trailer => mhttp_header:new()},
      parse(P#{data => Rest, state => header, msg => Response});
    _ ->
      {more, P}
  end;

parse(P = #{data := <<"\r\n", Rest/binary>>, state := header, msg := Msg}) ->
  Header = maps:get(header, Msg, mhttp_header:new()),
  Msg2 = Msg#{header => lists:reverse(Header)},
  parse(P#{data => Rest, state => body, msg => Msg2});

parse(P = #{data := Data, state := header, msg := Msg}) ->
  case split_header_field(Data) of
    [FieldData, Rest] ->
      Field = mhttp_proto:parse_header_field(FieldData),
      Header = maps:get(header, Msg, mhttp_header:new()),
      Msg2 = Msg#{header => mhttp_header:add_field(Header, Field)},
      parse(P#{data => Rest, state => header, msg => Msg2});
    _ ->
      {more, P}
  end;

parse(P = #{data := Data, state := body, msg := Msg}) ->
  Header = maps:get(header, Msg, mhttp_header:new()),
  case mhttp_header:body(Header) of
    {ok, {fixed, Length}} ->
      case Data of
        <<Body:Length/binary, Rest/binary>> ->
          Msg2 = Msg#{body => Body},
          parse(P#{data => Rest, state => final, msg => Msg2});
        _ ->
          {more, P}
      end;
    {ok, chunked} ->
      parse(P#{state => chunked_body});
    {ok, none} ->
      parse(P#{state => final});
    {error, Reason} ->
      throw({error, Reason})
  end;

parse(P = #{data := Data, state := chunked_body, msg := Msg}) ->
  case binary:split(Data, <<"\r\n">>) of
    [Line, Rest] ->
      case mhttp_proto:parse_chunk_header(Line) of
        0 ->
          parse(P#{data => Rest, state => trailer});
        Length ->
          case Rest of
            <<Chunk:Length/binary, "\r\n", Rest2/binary>> ->
              Body = maps:get(body, Msg, <<>>),
              Msg2 = Msg#{body => <<Body/binary, Chunk/binary>>},
              parse(P#{data => Rest2, state => chunked_body, msg => Msg2});
            <<_:Length/binary, _/binary>> ->
              throw({error, invalid_chunk});
            _ ->
              {more, P}
          end
      end;
    _ ->
      {more, P}
  end;

parse(P = #{data := <<"\r\n", Rest/binary>>, state := trailer, msg := Msg}) ->
  Trailer = maps:get(trailer, Msg, []),
  Msg2 = Msg#{trailer => lists:reverse(Trailer)},
  parse(P#{data => Rest, state => final, msg => Msg2});

parse(P = #{data := Data, state := trailer, msg := Msg}) ->
  case split_header_field(Data) of
    [FieldData, Rest] ->
      Field = mhttp_proto:parse_header_field(FieldData),
      Trailer = maps:get(trailer, Msg, []),
      Msg2 = Msg#{trailer => mhttp_header:add_field(Trailer, Field)},
      parse(P#{data => Rest, state => trailer, msg => Msg2});
    _ ->
      {more, P}
  end;

parse(P = #{state := final, msg := Msg}) ->
  P2 = maps:remove(response, P),
  {ok, decode_message_body(Msg), P2#{state => initial}}.

-spec split_header_field(binary()) -> [binary()].
split_header_field(Data) ->
  split_header_field(Data, <<>>).

-spec split_header_field(binary(), Acc :: binary()) -> [binary()].
split_header_field(Data, Acc) ->
  case binary:split(Data, <<"\r\n">>) of
    [Field, <<C, Rest/binary>>] when C =:= $\s; C =:= $\t ->
      split_header_field(Rest, <<Acc/binary, Field/binary, $\s>>);
    [Field, Rest] ->
      [<<Acc/binary, Field/binary>>, Rest];
    _ ->
      Data
  end.

-spec decode_message_body(msg()) -> msg().
decode_message_body(Msg = #{header := Header, body := Body}) ->
  OriginalSize = iolist_size(Body),
  TransferCodings0 = mhttp_header:transfer_encoding(Header),
  TransferCodings = case lists:reverse(TransferCodings0) of
                      [<<"chunked">> | Rest] -> Rest; % already decoded in the parser
                      Cs -> Cs
                    end,
  Body2 = decode_body(Body, transfer, TransferCodings),
  ContentCodings0 = mhttp_header:content_encoding(Header),
  ContentCodings = lists:reverse(ContentCodings0),
  Body3 = decode_body(Body2, content, ContentCodings),
  Header2 = mhttp_header:remove(Header, [<<"Transfer-Encoding">>,
                                         <<"Content-Encoding">>]),
  Internal = maps:get(internal, Msg, #{}),
  Msg#{header => Header2,
       body => Body3,
       internal => Internal#{original_body_size => OriginalSize}}.

-spec decode_body(iodata(), Type, Codings) -> binary() when
    Type :: content | transfer,
    Codings :: [binary()].
decode_body(Body, _Type, []) ->
  iolist_to_binary(Body);
decode_body(_Body, transfer, [<<"chunked">> | _Codings]) ->
  %% Any final chunked transfer encoding was removed from the list in
  %% decode_message_body/1. Any additional chunked encoding is invalid (RFC
  %% 7230 3.3.1.)
  throw({error, invalid_chunked_transfer_encoding});
decode_body(Body, transfer, [<<"identity">> | Codings]) ->
  decode_body(Body, transfer, Codings);
decode_body(Body, transfer, [<<"trailers">> | Codings]) ->
  decode_body(Body, transfer, Codings);
decode_body(Body, Type, [<<"gzip">> | Codings]) ->
  Body2 = mhttp_compression:decompress(gzip, Body),
  decode_body(Body2, Type, Codings);
decode_body(Body, Type, [<<"x-gzip">> | Codings]) ->
  Body2 = mhttp_compression:decompress(gzip, Body),
  decode_body(Body2, Type, Codings);
decode_body(_Body, content, [Coding | _Codings]) ->
  throw({error, {unsupported_content_encoding, Coding}});
decode_body(_Body, transfer, [Coding | _Codings]) ->
  throw({error, {unsupported_transfer_encoding, Coding}}).
