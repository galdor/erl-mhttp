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

-module(mhttp_websocket).

-behaviour(mhttp_protocol).

-export([request/2, upgrade/3, activate/4]).
-export([connect/1, connect/2, serialize/1, mask/2]).

-export_type([protocol_options/0,
              message/0, data_type/0,
              close_status/0, masking_key/0]).

-type protocol_options() ::
        #{nonce := binary(),
          subprotocols => [binary()],
          client_options => mhttp_websocket_client:options()}.

-type message() ::
        {data, data_type(), binary()}
      | close
      | {close, close_status(), binary()}
      | {ping, binary()}
      | {pong, binary()}.

-type data_type() :: text | binary.

-type close_status() :: 0..65535.

-type masking_key() :: <<_:32>>.

-spec request(mhttp:request(), protocol_options()) -> mhttp:request().
request(Request, Options = #{nonce := Nonce}) ->
  %% Note that while RFC 6455 indicates that the "upgrade" connection value is
  %% case-insensitive, some implementations (I am looking at you, Ruby's
  %% em-websocket), decided it must start with an upper case 'U'.
  Header = mhttp_request:header(Request),
  Fields0 = [{<<"Connection">>, <<"Upgrade">>},
             {<<"Upgrade">>, <<"websocket">>},
             {<<"Sec-WebSocket-Version">>, <<"13">>},
             {<<"Sec-WebSocket-Key">>, base64:encode(Nonce)}],
  Fields = case maps:find(subprotocols, Options) of
             {ok, Subprotocols} ->
               %% TODO proper formatting with quoting if needed
               Value = iolist_to_binary(lists:join($\s, Subprotocols)),
               [{<<"Sec-WebSocket-Protocol">>, Value} | Fields0];
             error ->
               Fields0
           end,
  Header2 = mhttp_header:add_fields(Header, Fields),
  Request#{header => Header2}.

-spec upgrade(mhttp:request(), mhttp:response(), protocol_options()) ->
        {ok, pid()} | {error, term()}.
upgrade(_Request, Response, ProtocolOptions = #{nonce := Nonce}) ->
  ClientOptions = maps:get(client_options, ProtocolOptions, #{}),
  case validate_response(Response, Nonce) of
    ok ->
      case mhttp_websocket_client_sup:start_client(ClientOptions) of
        {ok, Pid} ->
          {ok, Pid};
        {error, Reason} ->
          {error, {websocket, {start_client, Reason}}}
      end;
    {error, Reason} ->
      {error, {websocket, Reason}}
  end.

-spec activate(pid(), mhttp:socket(), mhttp:transport(), binary()) ->
        ok | {error, term()}.
activate(Pid, Socket, Transport, SocketData) ->
  %% TODO gen_server exit error ?
  gen_server:call(Pid, {activate, Socket, Transport, SocketData}, infinity).

-spec validate_response(mhttp:response(), binary()) -> ok | {error, term()}.
validate_response(Response, Nonce) ->
  Header = mhttp_response:header(Response),
  case mhttp_header:find(Header, <<"Sec-WebSocket-Accept">>) of
    {ok, Value} ->
      Key = base64:encode(Nonce),
      Suffix = <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>,
      Expected = base64:encode(crypto:hash(sha, [Key, Suffix])),
      if
        Value =:= Expected ->
          ok;
        true ->
          {error, accept_header_field_mismatch}
      end;
    error ->
      {error, missing_accept_header_field}
  end.

-spec connect(mhttp:request()) ->
        {ok, pid()} | {error, term()}.
connect(Request) ->
  connect(Request, #{}).

-spec connect(mhttp:request(), mhttp:request_options()) ->
        {ok, pid()} | {error, term()}.
connect(Request, Options) ->
  Target = mhttp_request:target_uri(Request),
  Scheme = case maps:find(scheme, Target) of
             {ok, S} -> string:lowercase(S);
             error -> <<"ws">>
           end,
  case Scheme of
    <<"ws">> ->
      Target2 = Target#{scheme => <<"http">>},
      connect_1(Request#{method => <<"GET">>, target => Target2}, Options);
    <<"wss">> ->
      Target2 = Target#{scheme => <<"https">>},
      connect_1(Request#{method => <<"GET">>, target => Target2}, Options);
    InvalidScheme ->
      {error, {websocket, {invalid_scheme, InvalidScheme}}}
  end.

-spec connect_1(mhttp:request(), mhttp:request_options()) ->
        {ok, pid()} | {error, term()}.
connect_1(Request, Options0) ->
  ProtocolOptions = maps:merge(#{nonce => crypto:strong_rand_bytes(16)},
                               maps:get(protocol_options, Options0, #{})),
  Options = Options0#{protocol => mhttp_websocket,
                      protocol_options => ProtocolOptions},
  case mhttp:send_request(Request, Options) of
    {ok, Response} when is_map(Response) ->
      {error, {no_upgrade, Response}};
    {ok, {upgraded, _Response, Pid}} ->
      {ok, Pid};
    {error, Reason} ->
      {error, Reason}
  end.

-spec serialize(message()) -> iodata().
serialize({data, text, Data}) ->
  serialize_frame(1, Data);
serialize({data, binary, Data}) ->
  serialize_frame(2, Data);
serialize(close) ->
  serialize_frame(8, <<"">>);
serialize({close, Status, Data}) ->
  serialize_frame(8, <<Status:16, Data/binary>>);
serialize({ping, Data}) ->
  serialize_frame(9, Data);
serialize({pong, Data}) ->
  serialize_frame(10, Data).

-spec serialize_frame(Opcode :: 0..16, binary()) -> iodata().
serialize_frame(Opcode, Data0) ->
  MaskingKey = generate_masking_key(),
  Data = mask(Data0, MaskingKey),
  serialize_frame(Opcode, MaskingKey, Data, erlang:iolist_size(Data)).

-spec serialize_frame(Opcode :: 0..16, masking_key(), iodata(),
                      non_neg_integer()) ->
        iodata().
serialize_frame(Opcode, MaskingKey, Data, Length) when Length < 125 ->
  [<<1:1, 0:3, Opcode:4, 1:1, Length:7>>, MaskingKey, Data];
serialize_frame(Opcode, MaskingKey, Data, Length) when Length < 16#ffff ->
  [<<1:1, 0:3, Opcode:4, 1:1, 126:7, Length:16>>, MaskingKey, Data];
serialize_frame(Opcode, MaskingKey, Data, Length) ->
  [<<1:1, 0:3, Opcode:4, 1:1, 127:7, Length:64>>, MaskingKey, Data].

-spec generate_masking_key() -> masking_key().
generate_masking_key() ->
  rand:bytes(4).

-spec mask(binary(), masking_key()) -> binary().
mask(Data, Key) ->
  mask(Data, Key, <<>>).

-spec mask(binary(), masking_key(), binary()) -> binary().
mask(<<>>, _, Acc) ->
  Acc;
mask(<<V:32, Data/binary>>, Key = <<K:32>>, Acc) ->
  mask(Data, Key, <<Acc/binary, (V bxor K):32>>);
mask(<<V:24>>, <<Key:24, _:8>>, Acc) ->
  <<Acc/binary, (V bxor Key):24>>;
mask(<<V:16>>, <<Key:16, _:16>>, Acc) ->
  <<Acc/binary, (V bxor Key):16>>;
mask(<<V:8>>, <<Key:8, _:24>>, Acc) ->
  <<Acc/binary, (V bxor Key):8>>.
