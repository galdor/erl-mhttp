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

-export([header/1,
         is_redirection/1]).

-spec header(mhttp:response()) -> mhttp:header().
header(Response) ->
  maps:get(header, Response, mhttp_header:new()).

-spec is_redirection(mhttp:response()) -> {true, uri:uri()} | false.
is_redirection(Response = #{status := S}) when S =:= 301; S =:= 302; S =:= 303;
                                               S =:= 307; S =:= 308 ->
  Header = header(Response),
  case mhttp_header:find(Header, <<"Location">>) of
    {ok, Value} ->
      try
        {true, uri:parse(Value)}
      catch
        error:Reason ->
          error({invalid_location_header_field, Value, Reason})
      end;
    error ->
      false
  end;
is_redirection(_Response) ->
  false.
