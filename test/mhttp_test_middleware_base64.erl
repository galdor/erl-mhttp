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

-module(mhttp_test_middleware_base64).

-export([preprocess/3, postprocess/4]).

-spec preprocess(mhttp:request(), mhttp:handler_context(), Args :: list()) ->
        mhttp_middleware:preprocess_result().
preprocess(Request = #{body := Data}, _Context, _Args) ->
  Request#{body => {base64, base64:decode(Data)}};
preprocess(Request, _Context, _Args) ->
  Request.

-spec postprocess(mhttp:request(), mhttp:response(),
                  mhttp:handler_context(), Args :: list()) ->
        mhttp_middleware:postprocess_result().
postprocess(_Request, Response = #{body := {base64, Data}}, _Context, _Args) ->
  Response#{body => base64:encode(Data)};
postprocess(_Request, Response, _Context, _Args) ->
  Response.
