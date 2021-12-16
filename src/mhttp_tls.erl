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

-module(mhttp_tls).

-export([init/0]).

-spec init() -> ok.
init() ->
  case locate_ca_certificate_bundle() of
    {ok, Path} ->
      logger:info("using ca certificate bundle from ~s", [Path],
                  #{domain => [mhttp, tls]}),
      persistent_term:put(mhttp_ca_certificate_bundle_path, Path);
    error ->
      logger:warning("cannot locate ca certificate bundle, "
                     "tls connections will fail if peer verification is "
                     "enabled", [],
                     #{domain => [mhttp, tls]}),
      persistent_term:put(mhttp_ca_certificate_bundle_path, undefined)
  end.

-spec locate_ca_certificate_bundle() -> {ok, file:name_all()} | error.
locate_ca_certificate_bundle() ->
  %% The list comes from the cmake script of curl (https://curl.se). This kind
  %% of detection should of course be done by Erlang/OTP itself...
  Candidates = ["/etc/ssl/certs/ca-certificates.crt",
                "/etc/pki/tls/certs/ca-bundle.crt",
                "/usr/share/ssl/certs/ca-bundle.crt",
                "/usr/local/share/certs/ca-root-nss.crt",
                "/etc/ssl/cert.pem"],
  locate_ca_certificate_bundle(Candidates).

-spec locate_ca_certificate_bundle([file:name_all()]) ->
  {ok, file:name_all()} | error.
locate_ca_certificate_bundle([]) ->
  error;
locate_ca_certificate_bundle([Path | Paths]) ->
  case file:open(Path, [read]) of
    {ok, File} ->
      file:close(File),
      {ok, Path};
    {error, enoent} ->
      logger:debug("~s not found", [Path], #{domain => [mhttp, tls]}),
      locate_ca_certificate_bundle(Paths);
    {error, Reason} ->
      logger:error("cannot read ~s: ~tp", [Path, Reason],
                   #{domain => [mhttp, tls]}),
      locate_ca_certificate_bundle(Paths)
  end.
