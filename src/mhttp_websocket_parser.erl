%% Copyright (c) 2020-2021 Nicolas Martyanoff <khaelin@gmail.com>.
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
%% IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(mhttp_websocket_parser).

-export([new/0, new/1, append_data/2, parse/1, parse_all/1]).

-export_type([parser/0,
              parse_result/0, parse_all_result/0,
              parse_error_reason/0]).

-type frame() ::
        #{fin := 0..1,
          rsv1 := 0..1,
          rsv2 := 0..1,
          rsv3 := 0..1,
          opcode := opcode(),
          mask => 0..1,
          payload_length => non_neg_integer(),
          masking_key => <<_:32>>,
          payload_data => binary()}.

-type opcode() :: 0..15.

-type parser() :: #{data := binary(),
                    state := state(),
                    message => mhttp_websocket:message(),
                    incomplete_message => boolean(),
                    frame => frame()}.

-type state() :: initial
               | frame_start
               | frame_payload_length
               | frame_masking_key
               | frame_payload_data
               | frame_end.

-type parse_result() ::
        {ok, mhttp_websocket:message(), parser()}
      | {more, parser()}
      | {error, parse_error_reason()}.

-type parse_all_result() ::
        {ok, [mhttp_websocket:message()], parser()}
      | {error, parse_error_reason()}.

-type parse_error_reason() ::
        {invalid_opcode, opcode()}
      | fragmented_control_frame
      | invalid_continuation_frame
      | interleaved_data_frames.

-spec new() -> parser().
new() ->
  new(<<"">>).

-spec new(binary()) -> parser().
new(Data) ->
  #{data => Data,
    state => initial}.

-spec append_data(parser(), binary()) -> parser().
append_data(P = #{data := Data}, NewData) ->
  P#{data => <<Data/binary, NewData/binary>>}.

-spec parse(parser()) -> parse_result().
parse(P) ->
  try
    parse1(P)
  catch
    throw:{error, Reason} ->
      {error, Reason}
  end.

-spec parse_all(parser()) -> parse_all_result().
parse_all(P) ->
  parse_all(P, []).

-spec parse_all(parser(), [mhttp_websocket:message()]) -> parse_all_result().
parse_all(P, Messages) ->
  case parse(P) of
    {ok, Message, P2} ->
      parse_all(P2, [Message | Messages]);
    {more, P2} ->
      {ok, lists:reverse(Messages), P2};
    {error, Reason} ->
      {error, Reason}
  end.

-spec parse1(parser()) -> parse_result().
parse1(P = #{state := initial}) ->
  parse1(P#{state => frame_start});
parse1(P = #{state := frame_start, data := Data}) ->
  case Data of
    <<FIN:1, RSV1:1, RSV2:1, RSV3:1, Op:4, Rest/binary>> ->
      Frame = #{fin => FIN,
                rsv1 => RSV1,
                rsv2 => RSV2,
                rsv3 => RSV3,
                opcode => Op},
      parse1(P#{state => frame_payload_length, data => Rest, frame => Frame});
    _ ->
      {more, P}
  end;
parse1(P = #{state := frame_payload_length, frame := Frame, data := Data}) ->
  case Data of
    <<Mask:1, 127:7, Length:64, Rest/binary>> ->
      parse1(P#{state => frame_masking_key, data => Rest,
                frame => Frame#{mask => Mask, payload_length => Length}});
    <<Mask:1, 126:7, Length:16, Rest/binary>> ->
      parse1(P#{state => frame_masking_key, data => Rest,
                frame => Frame#{mask => Mask, payload_length => Length}});
    <<Mask:1, Length:7, Rest/binary>> ->
      parse1(P#{state => frame_masking_key, data => Rest,
                frame => Frame#{mask => Mask, payload_length => Length}});
    _ ->
      {more, P}
  end;
parse1(P = #{state := frame_masking_key, frame := #{mask := 0}}) ->
  parse1(P#{state => frame_payload_data});
parse1(P = #{state := frame_masking_key, frame := (Frame = #{mask := 1}),
             data := Data}) ->
  case Data of
    <<Key:4/binary, Rest/binary>> ->
      parse1(P#{state => frame_payload_data,
                frame => Frame#{masking_key => Key},
                data => Rest});
    _ ->
      {more, P}
  end;
parse1(P = #{state := frame_payload_data,
             frame := (Frame = #{payload_length := Length}),
             data := Data}) ->
  case Data of
    <<PayloadData:Length/binary, Rest/binary>> ->
      parse1(P#{state => frame_end,
                frame => Frame#{payload_data => PayloadData},
                data => Rest});
    _ ->
      {more, P}
  end;
parse1(P = #{state := frame_end, frame := (_Frame = #{opcode := Op})}) ->
  case Op of
    16#0 -> process_continuation_frame(P);
    16#1 -> process_data_frame(P, text);
    16#2 -> process_data_frame(P, binary);
    16#8 -> process_close_frame(P);
    16#9 -> process_ping_frame(P);
    16#a -> process_pong_frame(P);
    _ -> throw({error, {invalid_opcode, Op}})
  end.

-spec process_continuation_frame(parser()) -> parse_result().
process_continuation_frame(P = #{message := {data, DataType, Data},
                                 incomplete_message := true,
                                 frame := #{fin := FIN,
                                            payload_data := NewData}}) ->
  Message2 = {data, DataType, <<Data/binary, NewData/binary>>},
  case FIN of
    1 ->
      P2 = maps:without([frame, message, incomplete_message], P),
      {ok, Message2, P2#{state => initial}};
    0 ->
      parse1(P#{state => initial, message => Message2})
  end;
process_continuation_frame(_P) ->
  throw({error, invalid_continuation_frame}).

-spec process_data_frame(parser(), mhttp_websocket:data_type()) ->
        parse_result().
process_data_frame(#{message := _}, _) ->
  throw({error, interleaved_data_frames});
process_data_frame(P = #{frame := #{fin := FIN, payload_data := Data}},
                   DataType) ->
  Message = {data, DataType, Data},
  case FIN of
    1 ->
      P2 = maps:without([frame, message, incomplete_message], P),
      {ok, Message, P2#{state => initial}};
    0 ->
      parse1(P#{state => initial,
                message => Message, incomplete_message => true})
  end.

-spec process_close_frame(parser()) -> parse_result().
process_close_frame(#{frame := #{fin := 0}}) ->
  throw({error, fragmented_control_frame});
process_close_frame(P = #{frame := #{payload_data := Data}}) ->
  {ok, {close, Data}, P#{state => initial}}.

-spec process_ping_frame(parser()) -> parse_result().
process_ping_frame(#{frame := #{fin := 0}}) ->
  throw({error, fragmented_control_frame});
process_ping_frame(P = #{frame := #{payload_data := Data}}) ->
  {ok, {ping, Data}, P#{state => initial}}.

-spec process_pong_frame(parser()) -> parse_result().
process_pong_frame(#{frame := #{fin := 0}}) ->
  throw({error, fragmented_control_frame});
process_pong_frame(P = #{frame := #{payload_data := Data}}) ->
  {ok, {pong, Data}, P#{state => initial}}.
