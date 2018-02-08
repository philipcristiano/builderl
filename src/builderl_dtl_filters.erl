-module(builderl_dtl_filters).

-behaviour(erlydtl_library).

-export([version/0,
         inventory/1]).

-export([s_to_datetime/1]).

version() ->
  1.

inventory(filters) ->
  [s_to_datetime];
inventory(tags) ->
  [].


s_to_datetime(Seconds) when is_integer(Seconds)->
    {Date, Time} = seconds_to_date_and_time(Seconds),
    format_date(Date, Time).

format_date({Y, Mo, D}, {H, Mi, S}) ->
    [integer_to_list(Y), "-",
     integer_to_list(Mo), "-",
     integer_to_list(D), " ",
     integer_to_list(H), ":",
     integer_to_list(Mi), ":",
     integer_to_list(S)].


seconds_to_date_and_time(TotalSeconds) ->
    BaseDate      = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds       = BaseDate + TotalSeconds,
    {Date, Time} = calendar:gregorian_seconds_to_datetime(Seconds),
    {Date, Time}.
