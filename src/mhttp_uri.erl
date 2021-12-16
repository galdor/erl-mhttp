%% Copyright (c) 2020-2021 Exograd SAS.
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

-module(mhttp_uri).

-export([scheme/1, host/1, port/1, path/1, transport/1,
         strip_path_prefix/2]).

-spec scheme(uri:uri()) -> uri:scheme().
scheme(#{scheme := Scheme}) ->
  string:lowercase(Scheme);
scheme(_URI) ->
  error(missing_scheme).

-spec host(uri:uri()) -> uri:host().
host(#{host := Host}) ->
  Host;
host(_URI) ->
  error(missing_host).

-spec port(uri:uri()) -> uri:port_number().
port(#{port := Port}) ->
  Port;
port(URI) ->
  case scheme(URI) of
    <<"http">> ->
      80;
    <<"https">> ->
      443
  end.

-spec path(uri:uri()) -> unicode:chardata().
path(#{path := Path}) when Path /= <<>> ->
  Path;
path(_URI) ->
  <<"/">>.

-spec transport(uri:uri()) -> mhttp:transport().
transport(URI) ->
  case scheme(URI) of
    <<"http">> ->
      tcp;
    <<"https">> ->
      tls
  end.

-spec strip_path_prefix(uri:uri(), binary()) -> uri:uri().
strip_path_prefix(URI, Prefix) ->
  Path = path(URI),
  Path2 = case string:prefix(Path, Prefix) of
            nomatch -> Path;
            Rest = <<$/, _/binary>> -> Rest;
            Rest -> <<$/, Rest/binary>>
          end,
  URI#{path => Path2}.
