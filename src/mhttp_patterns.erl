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

-module(mhttp_patterns).

-export([match/2]).

-export_type([path_pattern/0,
              path_variable_name/0, path_variable_value/0, path_variables/0,
              pattern/0]).

-type pattern() :: path_pattern()
                 | {path_pattern(), mhttp:method()}
                 | {path_pattern(), [mhttp_filters:filter()]}
                 | {path_pattern(), mhttp:method(), [mhttp_filters:filter()]}.

-type path_pattern() :: binary().
-type segment_pattern() :: [binary() | '*' | {named, atom()}].
-type segments() :: [binary()].

-type path_variable_name() :: atom().
-type path_variable_value() :: binary().
-type path_variables() :: #{path_variable_name() => path_variable_value()}.

-type match_result() :: {true, path_variables()} | false | {error, term()}.

-spec match(pattern(), mhttp:request()) -> match_result().
match(PathPattern, Request = #{method := Method}) when
    is_binary(PathPattern) ->
  match({PathPattern, Method, []}, Request);
match({PathPattern, Filters}, Request = #{method := Method}) when
    is_list(Filters) ->
  match({PathPattern, Method, Filters}, Request);
match({PathPattern, Method}, Request) ->
  match({PathPattern, Method, []}, Request);
match({_PathPattern, Method, _Filters}, #{method := RequestMethod}) when
    Method =/= RequestMethod ->
  false;
match(Pattern, Request = #{target := Target}) when is_binary(Target) ->
  case uri:parse(Target) of
    {ok, URI} ->
      match(Pattern, Request#{target => URI});
    {error, Reason} ->
      {error, {invalid_target, Reason}}
  end;
match({PathPattern, _Method, Filters}, Request = #{target := Target}) ->
  case match_path_pattern(PathPattern, mhttp_uri:path(Target)) of
    {true, Variables} ->
      case lists:all(fun (Filter) -> Filter(Request) end, Filters) of
        true ->
          {true, Variables};
        false ->
          false
      end;
    false ->
      false
  end.

-spec match_path_pattern(path_pattern(), uri:path()) -> match_result().
match_path_pattern(Pattern, Path) ->
  SegmentPattern = split_path_pattern(Pattern),
  Segments = binary:split(Path, <<"/">>, [global, trim_all]),
  match_segment_pattern(SegmentPattern, Segments).

-spec split_path_pattern(path_pattern()) -> segment_pattern().
split_path_pattern(Pattern) ->
  Segments = binary:split(Pattern, <<"/">>, [global, trim_all]),
  lists:map(fun (S) ->
                case S of
                  <<"*">> ->
                    '*';
                  <<":", Name/binary>> ->
                    {named, binary_to_atom(Name)};
                  _ ->
                    S
                end
            end, Segments).

-spec match_segment_pattern(segment_pattern(), segments()) -> match_result().
match_segment_pattern(Pattern, Segments) ->
  match_segment_pattern(Pattern, Segments, #{}).

-spec match_segment_pattern(segment_pattern(), segments(), path_variables()) ->
        match_result().
match_segment_pattern([], _Segments, Variables) ->
  {true, Variables};
match_segment_pattern(_Pattern, [], _Variables) ->
  false;
match_segment_pattern([P | Pattern], [S | Segments], Variables) when
    P =:= S; P =:= '*' ->
  match_segment_pattern(Pattern, Segments, Variables);
match_segment_pattern([{named, Name} | Pattern], [S | Segments], Variables) ->
  match_segment_pattern(Pattern, Segments, Variables#{Name => S});
match_segment_pattern([_ | _Pattern], [_ | _Segments], _Variables) ->
  false.
