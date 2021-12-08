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

-module(mhttp_sup).

-behaviour(c_sup).

-export([start_link/0]).
-export([children/0]).

-spec start_link() -> c_sup:start_ret().
start_link() ->
  c_sup:start_link({local, ?MODULE}, ?MODULE, #{}).

-spec children() -> c_sup:child_specs().
children() ->
  [{pools,
    #{start => fun mhttp_pool_sup:start_link/0}},
   {servers,
    #{start => fun mhttp_server_sup:start_link/0}},
   {websocket_clients,
    #{start => fun mhttp_websocket_client_sup:start_link/1,
      start_args => [#{}]}}].
