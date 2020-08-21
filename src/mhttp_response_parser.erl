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

-module(mhttp_response_parser).

-export([new/2, parse/2]).

-type parser() :: #{data := binary(),
                    state := state(),
                    request := mhttp:request(),
                    response => mhttp:response()}.

-type state() :: initial
               | status_line | header | body | chunked_body | trailer
               | final.

-type parse_result() :: {ok, mhttp:response(), parser()} |
                        {more, parser()}.

-spec new(Data :: binary(), mhttp:request()) -> parser().
new(Data, Request) ->
  #{data => Data,
    state => initial,
    request => Request}.

-spec parse(parser(), binary()) -> parse_result().
parse(P = #{data := Data}, NewData) ->
  parse(P#{data => <<Data/binary, NewData/binary>>}).

-spec parse(parser()) -> parse_result().

parse(P = #{state := initial}) ->
  parse(P#{state => status_line});

parse(P = #{data := Data, state := status_line}) ->
  case binary:split(Data, <<"\r\n">>) of
    [Line, Rest] ->
      {Version, Status, Reason} = mhttp_proto:parse_status_line(Line),
      Response = #{status => Status, reason => Reason, version => Version},
      parse(P#{data => Rest, state => header, response => Response});
    _ ->
      {more, P}
  end;

parse(P = #{data := <<"\r\n", Rest/binary>>,
            state := header,
            response := Response}) ->
  Header = mhttp_response:header(Response),
  Response2 = Response#{header => lists:reverse(Header)},
  parse(P#{data => Rest, state => body, response => Response2});

parse(P = #{data := Data, state := header, response := Response}) ->
  case header_field_split(Data) of
    [FieldData, Rest] ->
      Field = mhttp_proto:parse_header_field(FieldData),
      Header = mhttp_response:header(Response),
      Response2 = Response#{header => mhttp_header:add_field(Header, Field)},
      parse(P#{data => Rest, state => header, response => Response2});
    _ ->
      {more, P}
  end;

parse(P = #{data := Data, state := body, request := Request,
            response := Response}) ->
  case mhttp_response:expected_body(Response, Request) of
    {fixed, Length} ->
      case Data of
        <<Body:Length/binary, Rest/binary>> ->
          Response2 = Response#{body => Body},
          parse(P#{data => Rest, state => final, response => Response2});
        _ ->
          {more, P}
      end;
    chunked ->
      parse(P#{state => chunked_body});
    none ->
      parse(P#{state => final})
  end;

parse(P = #{data := Data, state := chunked_body, response := Response}) ->
  case binary:split(Data, <<"\r\n">>) of
    [Line, Rest] ->
      case mhttp_proto:parse_chunk_header(Line) of
        0 ->
          parse(P#{data => Rest, state => trailer});
        Length ->
          case Rest of
            <<Chunk:Length/binary, "\r\n", Rest2/binary>> ->
              Body = maps:get(body, Response, <<>>),
              Response2 = Response#{body => <<Body/binary, Chunk/binary>>},
              parse(P#{data => Rest2, state => chunked_body,
                       response => Response2});
            <<_:Length/binary, _/binary>> ->
              error(invalid_chunk);
            _ ->
              {more, P}
          end
      end;
    _ ->
      {more, P}
  end;

parse(P = #{data := <<"\r\n", Rest/binary>>,
            state := trailer,
            response := Response}) ->
  Trailer = maps:get(trailer, Response, []),
  Response2 = Response#{trailer => lists:reverse(Trailer)},
  parse(P#{data => Rest, state => final, response => Response2});

parse(P = #{data := Data, state := trailer, response := Response}) ->
  case header_field_split(Data) of
    [FieldData, Rest] ->
      Field = mhttp_proto:parse_header_field(FieldData),
      Trailer = maps:get(trailer, Response, []),
      Response2 = Response#{trailer => mhttp_trailer:add_field(Trailer, Field)},
      parse(P#{data => Rest, state => trailer, response => Response2});
    _ ->
      {more, P}
  end;

parse(P = #{state := final, response := Response}) ->
  P2 = maps:remove(response, P),
  {ok, Response, P2#{state => initial}}.

-spec header_field_split(binary()) -> [binary()].
header_field_split(Data) ->
  header_field_split(Data, <<>>).

-spec header_field_split(binary(), Acc :: binary()) -> [binary()].
header_field_split(Data, Acc) ->
  case binary:split(Data, <<"\r\n">>) of
    [Field, <<C, Rest/binary>>] when C =:= $\s; C =:= $\t ->
      header_field_split(Rest, <<Acc/binary, Field/binary, $\s>>);
    [Field, Rest] ->
      [<<Acc/binary, Field/binary>>, Rest];
    _ ->
      Data
  end.
