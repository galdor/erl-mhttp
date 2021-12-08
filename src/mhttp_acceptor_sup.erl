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

-module(mhttp_acceptor_sup).

-behaviour(c_sup).

-export([start_link/0, start_children/3]).
-export([children/0]).

-spec start_link() -> c_sup:start_ret().
start_link() ->
  c_sup:start_link(?MODULE, #{}).

-spec children() -> c_sup:child_specs().
children() ->
  [].

-spec start_children(c_gen_server:ref(), non_neg_integer(),
                     mhttp_acceptor:options()) ->
        ok.
start_children(_SupRef, 0, _Options) ->
  ok;
start_children(SupRef, N, Options) ->
  Spec = #{start => fun mhttp_acceptor:start_link/1,
           start_args => [Options]},
  {ok, _} = c_sup:start_child(SupRef, {acceptor, N}, Spec),
  start_children(SupRef, N-1, Options).
