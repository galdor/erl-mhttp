-module(mhttp_time).

-export([parse_rfc1123_date/1,
         parse_rfc1036_date/1,
         parse_asctime_date/1]).

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
