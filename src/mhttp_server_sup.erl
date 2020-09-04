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

-module(mhttp_server_sup).

-behaviour(supervisor).

-export([start_link/0, start_server/2]).
-export([init/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_server(mhttp:server_id(), mhttp_server:options()) ->
        supervisor:startchild_ret().
start_server(Id, Options) ->
  supervisor:start_child(?MODULE, server_child_spec(Id, Options)).

init([]) ->
  Children = server_child_specs(),
  {ok, {{one_for_one, 1, 5}, Children}}.

-spec server_child_specs() -> [supervisor:child_spec()].
server_child_specs() ->
  ServerSpecs = application:get_env(mhttp, servers, #{}),
  maps:fold(fun (Id, Options, Acc) ->
                [server_child_spec(Id, Options) | Acc]
            end,
            [], ServerSpecs).

-spec server_child_spec(mhttp:server_id(), mhttp_server:options()) ->
        supervisor:child_spec().
server_child_spec(ChildId, Options) ->
  Name = mhttp_server:process_name(ChildId),
  #{id => ChildId,
    start => {mhttp_server, start_link, [{local, Name}, Options]}}.
