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

-module(mhttp_log).

-export([log_request/3]).

-spec log_request(mhttp:request(), mhttp:response(),
                  StartTime :: integer()) -> ok.
log_request(Request, Response, StartTime) ->
  Now = erlang:system_time(microsecond),
  MethodString = mhttp_proto:encode_method(mhttp_request:method(Request)),
  Target = mhttp_request:target_string(Request),
  Status = mhttp_response:status(Response),
  Internal = maps:get(internal, Response, #{}),
  BodySize = case maps:find(original_body_size, Internal) of
               {ok, Size} ->
                 Size;
               error ->
                 iolist_size(mhttp_response:body(Response))
             end,
  ProcessingTime = Now - StartTime,
  Data = #{domain => [mhttp, request],
           status => Status,
           processing_time => ProcessingTime},
  logger:info("~s ~s ~b ~ts ~ts",
              [MethodString, Target, Status,
               format_data_size(BodySize),
               format_processing_time(ProcessingTime)],
              Data),
  ok.

-spec format_processing_time(Microseconds :: non_neg_integer()) -> binary().
format_processing_time(Microseconds) when Microseconds < 1_000 ->
  <<(integer_to_binary(Microseconds))/binary, "μs"/utf8>>;
format_processing_time(Microseconds) when Microseconds > 1_000_000 ->
  <<(float_to_binary(Microseconds / 1.0e6, [{decimals, 1}]))/binary, "s">>;
format_processing_time(Microseconds) ->
  <<(float_to_binary(Microseconds / 1.0e3, [{decimals, 1}]))/binary, "ms">>.

-spec format_data_size(Bytes :: non_neg_integer()) -> binary().
format_data_size(Bytes) when Bytes < 1_000 ->
  <<(integer_to_binary(Bytes))/binary, "B"/utf8>>;
format_data_size(Bytes) when Bytes < 1_000_000 ->
  <<(float_to_binary(Bytes / 1.0e3, [{decimals, 1}]))/binary, "kB">>;
format_data_size(Bytes) when Bytes < 1_000_000_000 ->
  <<(float_to_binary(Bytes / 1.0e6, [{decimals, 1}]))/binary, "MB">>;
format_data_size(Bytes) ->
  <<(float_to_binary(Bytes / 1.0e9, [{decimals, 1}]))/binary, "GB">>.
