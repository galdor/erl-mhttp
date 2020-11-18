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

-module(mhttp_response).

-export([version/1, status/1, reason/1, header/1, body/1, trailer/1,
         internal/1, with_internal/2,
         ensure_reason/1, ensure_date/1, maybe_add_content_length/1,
         is_redirection/1]).

-spec version(mhttp:response()) -> mhttp:version().
version(Response) ->
  maps:get(version, Response, http_1_1).

-spec status(mhttp:response()) -> mhttp:status().
status(#{status := Status}) ->
  Status.

-spec reason(mhttp:response()) -> binary().
reason(Response) ->
  maps:get(reason, Response, <<>>).

-spec header(mhttp:response()) -> mhttp:header().
header(Response) ->
  maps:get(header, Response, mhttp_header:new()).

-spec body(mhttp:response()) -> mhttp:body().
body(Response) ->
  maps:get(body, Response, <<>>).

-spec trailer(mhttp:response()) -> mhttp:header().
trailer(Response) ->
  maps:get(trailer, Response, mhttp_header:new()).

-spec internal(mhttp:response()) -> mhttp:msg_internal().
internal(Response) ->
  maps:get(internal, Response, #{}).

-spec with_internal(mhttp:response(), mhttp:msg_internal()) -> mhttp:response().
with_internal(Response, NewInternal) ->
  Response#{internal => maps:merge(internal(Response), NewInternal)}.

-spec ensure_reason(mhttp:response()) -> mhttp:response().
ensure_reason(Response = #{reason := _}) ->
  Response;
ensure_reason(Response = #{status := Status}) ->
  Response#{reason => mhttp_statuses:reason(Status)}.

-spec ensure_date(mhttp:response()) -> mhttp:response().
ensure_date(Response) ->
  Header = mhttp_response:header(Response),
  case mhttp_header:contains(Header, <<"Date">>) of
    true ->
      Response;
    false ->
      Now = erlang:universaltime(),
      Value = mhttp_calendar:format_rfc7231_datetime(Now),
      Response#{header => mhttp_header:add(Header, <<"Date">>, Value)}
  end.

-spec maybe_add_content_length(mhttp:response()) -> mhttp:response().
maybe_add_content_length(Response) ->
  Body = maps:get(body, Response, <<>>),
  Length = iolist_size(Body),
  Header = mhttp_response:header(Response),
  Header2 = mhttp_header:add_if_missing(Header, <<"Content-Length">>,
                                        integer_to_binary(Length)),
  Response#{header => Header2}.

-spec is_redirection(mhttp:response()) ->
        {true, uri:uri()} | false | {error, term()}.
is_redirection(Response = #{status := S}) when S =:= 301; S =:= 302; S =:= 303;
                                               S =:= 307; S =:= 308 ->
  Header = header(Response),
  case mhttp_header:find(Header, <<"Location">>) of
    {ok, Value} ->
      case uri:parse(Value) of
        {ok, URI} ->
          {true, URI};
        {error, Reason} ->
          {error, {invalid_location_header_field, Reason}}
      end;
    error ->
      false
  end;
is_redirection(_Response) ->
  false.
