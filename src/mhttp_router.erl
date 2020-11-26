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

-export([find_route/3]).

-export_type([router/0]).

-type router() :: #{routes := [mhttp:route()]}.

-spec find_route(router(), mhttp:request(), mhttp:handler_context()) ->
        {ok, {mhttp:route(), mhttp:handler_context()}} |
        {error, not_found | term()}.
find_route(#{routes := Routes}, Request, Context) ->
  case do_find_route(Routes, Request) of
    {ok, {Route, PathVariables}} ->
      case Route of
        {_, Handler} when is_function(Handler) ->
          {ok, {Route, Context#{path_variables => PathVariables}}};
        {_, {router, Router2}} ->
          find_route(Router2, Request, Context);
        {_, {router, Router2, Options}} ->
          {Request2, Context2} =
            apply_handler_router_options(Options, Request, Context),
          find_route(Router2, Request2, Context2)
      end;
    {error, not_found} ->
      {error, not_found};
    {error, Reason} ->
      {error, Reason}
  end.

-spec do_find_route([mhttp:route()], mhttp:request()) ->
        {ok, {mhttp:route(), mhttp_patterns:path_variables()}} |
        {error, not_found | term()}.
do_find_route([], _Request) ->
  {error, not_found};
do_find_route([Route = {Pattern, _} | Routes], Request) ->
  case mhttp_patterns:match(Pattern, Request) of
    {true, PathVariables} ->
      {ok, {Route, PathVariables}};
    false ->
      do_find_route(Routes, Request);
    {error, Reason} ->
      {error, Reason}
  end.

-spec apply_handler_router_options(mhttp:handler_router_options(),
                                   mhttp:request(), mhttp:handler_context()) ->
        {mhttp:request(), mhttp:handler_context()}.
apply_handler_router_options(Options, Request, Context) ->
  maps:fold(fun apply_handler_router_option/3, {Request, Context}, Options).

-spec apply_handler_router_option(Name :: atom(), Value :: term(),
                                  {mhttp:request(), mhttp:handler_context()}) ->
        {mhttp:request(), mhttp:handler_context()}.
apply_handler_router_option(strip_path_prefix, Prefix, {Request, Context}) ->
  Target = mhttp_request:target_uri(Request),
  Target2 = mhttp_uri:strip_path_prefix(Target, Prefix),
  {Request#{target => Target2}, Context}.
