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

-module(mhttp_handlers).

-export([not_found_handler/2, unavailable_service_handler/2,
         error_handler/4,
         debug_handler/2]).

-spec not_found_handler(mhttp:request(), mhttp:handler_context()) ->
        mhttp:handler_ret().
not_found_handler(_Request, _Context) ->
  #{status => 404,
    header => [{<<"Content-Type">>, <<"text/plain">>}],
    body => <<"Not found.\n">>}.

-spec unavailable_service_handler(mhttp:request(), mhttp:handler_context()) ->
        mhttp:handler_ret().
unavailable_service_handler(_Request, _Context) ->
  #{status => 503,
    header => [{<<"Content-Type">>, <<"text/plain">>}],
    body => <<"Service unavailable.\n">>}.

-spec error_handler(mhttp:request(), mhttp:handler_context(),
                    Reason :: term(), Trace :: [et_erlang:stack_item()]) ->
        mhttp:handler_ret().
error_handler(Request, Context, Reason, Trace) ->
  Body = [io_lib:format(<<"~s\n~p\n\n">>, [Title, Datum]) ||
           {Title, Datum} <- [{<<"REQUEST">>, Request},
                              {<<"CONTEXT">>, Context},
                              {<<"REASON">>, Reason},
                              {<<"TRACE">>, Trace}]],
  #{status => 500,
    header => [{<<"Content-Type">>, <<"text/plain">>}],
    body => Body}.

-spec debug_handler(mhttp:request(), mhttp:handler_context()) ->
        mhttp:handler_ret().
debug_handler(Request, Context) ->
  Body = [io_lib:format(<<"~s\n~p\n\n">>, [Title, Datum]) ||
           {Title, Datum} <- [{<<"REQUEST">>, Request},
                              {<<"CONTEXT">>, Context}]],
  #{status => 200,
    header => [{<<"Content-Type">>, <<"text/plain">>}],
    body => Body}.
