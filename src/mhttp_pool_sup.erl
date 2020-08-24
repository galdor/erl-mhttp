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

-module(mhttp_pool_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Children = pool_child_specs(),
  {ok, {{one_for_one, 1, 5}, Children}}.

-spec pool_child_specs() -> [supervisor:child_spec()].
pool_child_specs() ->
  EnvPoolSpecs = application:get_env(mhttp, pools, #{}),
  PoolSpecs = case maps:is_key(default, EnvPoolSpecs) of
                true ->
                  EnvPoolSpecs;
                false ->
                  EnvPoolSpecs#{default => #{}}
              end,
  maps:fold(fun (Id, Options, Acc) ->
                [pool_child_spec(Id, Options) | Acc]
            end,
            [], PoolSpecs).

-spec pool_child_spec(atom(), mhttp_server:options()) ->
        supervisor:child_spec().
pool_child_spec(ChildId, Options) ->
  Name = mhttp_pool:process_name(ChildId),
  #{id => ChildId,
    start => {mhttp_pool, start_link, [{local, Name}, Options]}}.
