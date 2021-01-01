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

-behaviour(supervisor).

-export([start_link/1, start_children/2]).
-export([init/1]).

-spec start_link(mhttp_acceptor:options()) -> supervisor:startlink_ret().
start_link(Options) ->
  supervisor:start_link(?MODULE, [Options]).

init([Options]) ->
  ChildSpec = #{id => mhttp_acceptor,
                start => {mhttp_acceptor, start_link, [Options]}},
  {ok, {{simple_one_for_one, 1, 5}, [ChildSpec]}}.

-spec start_children(SupRef :: pid(),
                     NbChildren :: non_neg_integer()) -> ok.
start_children(_SupRef, 0) ->
  ok;
start_children(SupRef, N) ->
  {ok, _} = supervisor:start_child(SupRef, []),
  start_children(SupRef, N-1).
