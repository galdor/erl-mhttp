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

-module(mhttp_request).

-export([target_uri/1,
         header/1, prepend_header/2,
         ensure_host/3, maybe_add_content_length/1,
         redirect/3, redirection_uri/2]).

-spec target_uri(mhttp:request()) -> uri:uri().
target_uri(#{target := Target}) when is_map(Target) ->
  Target;
target_uri(#{target := Target}) ->
  try
    uri:parse(Target)
  catch
    error:Reason ->
      error({invalid_target, Target, Reason})
  end.

-spec header(mhttp:request()) -> mhttp:header().
header(Request) ->
  maps:get(header, Request, mhttp_header:new()).

-spec prepend_header(mhttp:request(), mhttp:header()) -> mhttp:request().
prepend_header(Request, Header) ->
  Request#{header => mhttp_header:append(Header, header(Request))}.

-spec ensure_host(mhttp:request(), mhttp:host(), inets:port_number()) ->
        mhttp:request().
ensure_host(Request, Host, Port) ->
  Value = iolist_to_binary([Host, $:, integer_to_binary(Port)]),
  Header = mhttp_request:header(Request),
  Header2 = mhttp_header:add_if_missing(Header, <<"Host">>, Value),
  Request#{header => Header2}.

-spec maybe_add_content_length(mhttp:request()) -> mhttp:request().
maybe_add_content_length(Request = #{body := Body}) ->
  case iolist_size(Body) of
    0 ->
      Request;
    Length ->
      Header = mhttp_request:header(Request),
      Header2 = mhttp_header:add_if_missing(Header, <<"Content-Length">>,
                                            integer_to_binary(Length)),
      Request#{header => Header2}
  end;
maybe_add_content_length(Request) ->
  Request.

-spec redirect(mhttp:request(), mhttp:status(), uri:uri()) ->
        mhttp:request().
redirect(Request = #{method := Method, target := Target}, Status, URI) ->
  {Method, Body} = case Status of
                     303 ->
                       {get, <<>>};
                     _ ->
                       {Method, maps:get(body, Request, <<>>)}
                   end,
  Request#{method => Method,
           target => redirection_uri(Target, URI),
           body => Body}.

-spec redirection_uri(Target :: mhttp:target(), URIReference :: uri:uri()) ->
        uri:uri().
redirection_uri(Target, URIReference) ->
  try
    uri:resolve_reference(URIReference, Target)
  catch
    error:Reason ->
      error({invalid_uri_reference, URIReference, Reason})
  end.
