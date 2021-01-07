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

-module(mhttp_request).

-export([method/1, target_uri/1, target_string/1, version/1, header/1, body/1,
         trailer/1, internal/1, with_internal/2,
         request_id/1,
         canonicalize_target/1,
         prepend_header/2,
         ensure_host/4, maybe_add_content_length/1,
         redirect/3, redirection_uri/2]).

-spec method(mhttp:request()) -> mhttp:method().
method(#{method := Method}) ->
  Method.

-spec target_uri(mhttp:request()) -> uri:uri().
target_uri(#{target := Target}) when is_map(Target) ->
  Target;
target_uri(#{target := Target}) ->
  case uri:parse(Target) of
    {ok, URI} ->
      URI;
    {error, Reason} ->
      error({invalid_target_uri, Reason})
  end.

-spec target_string(mhttp:request()) -> binary().
target_string(#{target := Target}) when is_map(Target) ->
  uri:serialize(Target);
target_string(#{target := Target}) ->
  Target.

-spec version(mhttp:request()) -> mhttp:version().
version(Request) ->
  maps:get(version, Request, http_1_1).

-spec header(mhttp:request()) -> mhttp:header().
header(Request) ->
  maps:get(header, Request, mhttp_header:new()).

-spec body(mhttp:request()) -> mhttp:body().
body(Request) ->
  maps:get(body, Request, <<>>).

-spec trailer(mhttp:request()) -> mhttp:header().
trailer(Request) ->
  maps:get(trailer, Request, mhttp_header:new()).

-spec internal(mhttp:request()) -> mhttp:msg_internal().
internal(Request) ->
  maps:get(internal, Request, #{}).

-spec with_internal(mhttp:request(), mhttp:msg_internal()) -> mhttp:request().
with_internal(Request, NewInternal) ->
  Request#{internal => maps:merge(internal(Request), NewInternal)}.

-spec request_id(mhttp:request()) -> {ok, binary()} | error.
request_id(Request) ->
  mhttp_header:find(header(Request), <<"X-Request-Id">>).

-spec canonicalize_target(mhttp:request()) ->
        {ok, mhttp:request()} | {error, term()}.
canonicalize_target(Request = #{target := Target}) when is_map(Target) ->
  {ok, Request};
canonicalize_target(Request = #{target := Target}) when is_binary(Target) ->
  case uri:parse(Target) of
    {ok, URI = #{scheme := _, host := _}} ->
      {ok, Request#{target => URI}};
    {ok, #{scheme := _}} ->
      {error, {invalid_target, missing_scheme}};
    {ok, #{host := _}} ->
      {error, {invalid_target, missing_host}};
    {error, Reason} ->
      {error, {invalid_target, Reason}}
  end.

-spec prepend_header(mhttp:request(), mhttp:header()) -> mhttp:request().
prepend_header(Request, Header) ->
  Request#{header => mhttp_header:append(Header, header(Request))}.

-spec ensure_host(mhttp:request(), uri:host(), uri:port_number(),
                  mhttp:transport()) ->
        mhttp:request().
ensure_host(Request, Host, Port, Transport) ->
  %% We take care not to include the port in the value if this is the default
  %% port for the transport used; infortunately, some servers will respond
  %% with a Location header field based on the value of the Host header field
  %% without any host/port analysis. For example, GitHub will redirect a
  %% request for http://github.com with a Host header field set to
  %% "github.com:80" to "https://github.com:80/".
  Value = case {Transport, Port} of
            {tcp, 80} -> Host;
            {tls, 443} -> Host;
            _ -> <<Host/binary, $:, (integer_to_binary(Port))/binary>>
          end,
  Header = mhttp_request:header(Request),
  Header2 = mhttp_header:add_if_missing(Header, <<"Host">>, Value),
  Request#{header => Header2}.

-spec maybe_add_content_length(mhttp:request()) -> mhttp:request().
maybe_add_content_length(Request) ->
  Body = maps:get(body, Request, <<>>),
  Length = iolist_size(Body),
  Header = mhttp_request:header(Request),
  Header2 = mhttp_header:add_if_missing(Header, <<"Content-Length">>,
                                        integer_to_binary(Length)),
  Request#{header => Header2}.

-spec redirect(mhttp:request(), mhttp:status(), uri:uri()) ->
        mhttp:request().
redirect(Request = #{method := Method, target := Target}, Status, URI) ->
  {Method2, Body} = case Status of
                      303 ->
                        {get, <<>>};
                      _ ->
                        {Method, maps:get(body, Request, <<>>)}
                    end,
  Request#{method => Method2,
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
