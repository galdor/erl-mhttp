-module(mhttp_time).

-export([format_rfc1123_date/1,
         parse_rfc1123_date/1,
         parse_rfc1036_date/1,
         parse_asctime_date/1]).

-spec format_rfc1123_date(calendar:datetime()) -> binary().
format_rfc1123_date({Date = {Year, Month, Day}, {Hour, Minute, Second}}) ->
  Weekday = calendar:day_of_the_week(Date),
  WeekdayName = format_short_weekday(Weekday),
  MonthName = format_short_month(Month),
  Data = io_lib:format(<<"~s, ~2..0b ~s ~4..0b ~2..0b:~2..0b:~2..0b GMT">>,
                       [WeekdayName, Day, MonthName, Year,
                        Hour, Minute, Second]),
  iolist_to_binary(Data).

-spec parse_rfc1123_date(binary()) ->
        {ok, calendar:datetime()} | {error, invalid_format}.
parse_rfc1123_date(Data) ->
  WeekdayRe = "Mon|Tue|Wed|Thu|Fri|Sat|Sun",
  MonthRe = "Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec",
  DateRe = ["([0-9]{2}) (", MonthRe, ") ([0-9]{4})"],
  TimeRe = ["([0-9]{2}):([0-9]{2}):([0-9]{2}) GMT"],
  Re = ["^(?:", WeekdayRe, "), ", DateRe, " ", TimeRe, "$"],
  case re:run(Data, Re, [{capture, all_but_first, binary}]) of
    {match, [Day, Month, Year, Hour, Minute, Second]} ->
      try
        {ok, {{binary_to_integer(Year),
               parse_short_month(Month),
               binary_to_integer(Day)},
              {binary_to_integer(Hour),
               binary_to_integer(Minute),
               binary_to_integer(Second)}}}
      catch
        error:_ ->
          {error, invalid_format}
      end;
    nomatch ->
      {error, invalid_format}
  end.

-spec parse_rfc1036_date(binary()) ->
        {ok, calendar:datetime()} | {error, invalid_format}.
parse_rfc1036_date(Data) ->
  WeekdayRe = "Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday",
  MonthRe = "Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec",
  DateRe = ["([0-9]{2})-(", MonthRe, ")-([0-9]{2})"],
  TimeRe = ["([0-9]{2}):([0-9]{2}):([0-9]{2}) GMT"],
  Re = ["^(?:", WeekdayRe, "), ", DateRe, " ", TimeRe, "$"],
  case re:run(Data, Re, [{capture, all_but_first, binary}]) of
    {match, [Day, Month, Year, Hour, Minute, Second]} ->
      try
        {ok, {{1900+binary_to_integer(Year),
               parse_short_month(Month),
               binary_to_integer(Day)},
              {binary_to_integer(Hour),
               binary_to_integer(Minute),
               binary_to_integer(Second)}}}
      catch
        error:_ ->
          {error, invalid_format}
      end;
    nomatch ->
      {error, invalid_format}
  end.

-spec parse_asctime_date(binary()) ->
        {ok, calendar:datetime()} | {error, invalid_format}.
parse_asctime_date(Data) ->
  WeekdayRe = "Mon|Tue|Wed|Thu|Fri|Sat|Sun",
  MonthRe = "Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec",
  DayRe = "[0-9 ][0-9]",
  YearRe = "[0-9]{4}",
  TimeRe = ["([0-9]{2}):([0-9]{2}):([0-9]{2})"],
  Re = ["^(?:", WeekdayRe, ") (", MonthRe, ") (", DayRe, ") ",
        TimeRe, " (", YearRe, ")$"],
  case re:run(Data, Re, [{capture, all_but_first, binary}]) of
    {match, [Month, Day, Hour, Minute, Second, Year]} ->
      try
        {ok, {{binary_to_integer(Year),
               parse_short_month(Month),
               binary_to_integer(string:trim(Day, leading, " "))},
              {binary_to_integer(Hour),
               binary_to_integer(Minute),
               binary_to_integer(Second)}}}
      catch
        error:_ ->
          {error, invalid_format}
      end;
    nomatch ->
      {error, invalid_format}
  end.

-spec format_short_month(1..12) -> binary().
format_short_month(1) -> <<"Jan">>;
format_short_month(2) -> <<"Feb">>;
format_short_month(3) -> <<"Mar">>;
format_short_month(4) -> <<"Apr">>;
format_short_month(5) -> <<"May">>;
format_short_month(6) -> <<"Jun">>;
format_short_month(7) -> <<"Jul">>;
format_short_month(8) -> <<"Aug">>;
format_short_month(9) -> <<"Sep">>;
format_short_month(10) -> <<"Oct">>;
format_short_month(11) -> <<"Nov">>;
format_short_month(12) -> <<"Dec">>.

-spec parse_short_month(binary()) -> 1..12.
parse_short_month(<<"Jan">>) -> 1;
parse_short_month(<<"Feb">>) -> 2;
parse_short_month(<<"Mar">>) -> 3;
parse_short_month(<<"Apr">>) -> 4;
parse_short_month(<<"May">>) -> 5;
parse_short_month(<<"Jun">>) -> 6;
parse_short_month(<<"Jul">>) -> 7;
parse_short_month(<<"Aug">>) -> 8;
parse_short_month(<<"Sep">>) -> 9;
parse_short_month(<<"Oct">>) -> 10;
parse_short_month(<<"Nov">>) -> 11;
parse_short_month(<<"Dec">>) -> 12.

-spec format_short_weekday(1..7) -> binary().
format_short_weekday(1) -> <<"Mon">>;
format_short_weekday(2) -> <<"Tue">>;
format_short_weekday(3) -> <<"Wed">>;
format_short_weekday(4) -> <<"Thu">>;
format_short_weekday(5) -> <<"Fri">>;
format_short_weekday(6) -> <<"Sat">>;
format_short_weekday(7) -> <<"Sun">>.
