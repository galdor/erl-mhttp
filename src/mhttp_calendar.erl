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

-module(mhttp_calendar).

-export([format_rfc7231_datetime/1]).

-spec format_rfc7231_datetime(calendar:datetime()) -> binary().
format_rfc7231_datetime({{Year, Month, Day}, {Hour, Minute, Second}}) ->
  WeekDay = calendar:day_of_the_week(Year, Month, Day),
  WeekDayName = rfc7231_week_day_name(WeekDay),
  MonthName = rfc7231_month_name(Month),
  Data = io_lib:format(<<"~s, ~2..0b ~s ~4..0b ~2..0b:~2..0b:~2..0b GMT">>,
                       [WeekDayName, Day, MonthName, Year,
                        Hour, Minute, Second]),
  iolist_to_binary(Data).

-spec rfc7231_week_day_name(1..7) -> binary().
rfc7231_week_day_name(1) ->
  <<"Mon">>;
rfc7231_week_day_name(2) ->
  <<"Tue">>;
rfc7231_week_day_name(3) ->
  <<"Wed">>;
rfc7231_week_day_name(4) ->
  <<"Thu">>;
rfc7231_week_day_name(5) ->
  <<"Fri">>;
rfc7231_week_day_name(6) ->
  <<"Sat">>;
rfc7231_week_day_name(7) ->
  <<"Sun">>.

-spec rfc7231_month_name(1..12) -> binary().
rfc7231_month_name(1) ->
  <<"Jan">>;
rfc7231_month_name(2) ->
  <<"Feb">>;
rfc7231_month_name(3) ->
  <<"Mar">>;
rfc7231_month_name(4) ->
  <<"Apr">>;
rfc7231_month_name(5) ->
  <<"May">>;
rfc7231_month_name(6) ->
  <<"Jun">>;
rfc7231_month_name(7) ->
  <<"Jul">>;
rfc7231_month_name(8) ->
  <<"Aug">>;
rfc7231_month_name(9) ->
  <<"Sep">>;
rfc7231_month_name(10) ->
  <<"Oct">>;
rfc7231_month_name(11) ->
  <<"Nov">>;
rfc7231_month_name(12) ->
  <<"Dec">>.
