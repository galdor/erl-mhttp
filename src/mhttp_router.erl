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

-module(mhttp_router).

-export([default_route/0, find_route/3]).

-export_type([router/0]).

-type router() :: #{routes := [mhttp:route()],
                    default_route => mhttp:route(),
                    middlewares => [mhttp:middleware()]}.

-spec default_route() -> mhttp:route().
default_route() ->
  {default, fun mhttp_handlers:not_found_handler/2}.

-spec find_route(router(), mhttp:request(), mhttp:request_context()) ->
        {mhttp:route(), mhttp:request_context()}.
find_route(Router = #{routes := Routes}, Request, Context) ->
  case find_route_(Routes, Request) of
    {ok, Route, PathVariables} ->
      {Route, Context#{path_variables => PathVariables}};
    error ->
      Route = maps:get(default_route, Router, default_route()),
      {Route, Context}
  end.

-spec find_route_([mhttp:route()], mhttp:request()) ->
        {ok, mhttp:route(), mhttp_patterns:path_variables()} | error.
find_route_([], _Request) ->
  error;
find_route_([Route = {Pattern, _} | Routes], Request) ->
  case mhttp_patterns:match(Pattern, Request) of
    {true, PathVariables} ->
      {ok, Route, PathVariables};
    false ->
      find_route_(Routes, Request)
  end.
