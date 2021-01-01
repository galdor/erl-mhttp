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

-module(mhttp_filters).

-export_type([filter/0]).

-export([header_contains/1, header_equal/2]).

-type filter() :: fun ((mhttp:request()) -> boolean()).

-spec header_contains(mhttp:header_name()) -> filter().
header_contains(Name) ->
  fun (Request) ->
      mhttp_header:contains(mhttp_request:header(Request), Name)
  end.

-spec header_equal(mhttp:header_name(), mhttp:header_value()) -> filter().
header_equal(Name, Value) ->
  fun (Request) ->
      case mhttp_header:find(mhttp_request:header(Request), Name) of
        {ok, Value2} ->
          Value =:= Value2;
        error ->
          false
      end
  end.
