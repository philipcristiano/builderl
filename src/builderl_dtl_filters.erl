-module(builderl_dtl_filters).

-behaviour(erlydtl_library).

-export([version/0,
         inventory/1]).

-export([sToDatetime/1]).

version() ->
  1.

inventory(filters) ->
  [sToDatetime];
inventory(tags) ->
  [].


sToDatetime(Seconds) when is_integer(Seconds)->
    {Date, Time} = secondsToDateAndTime(Seconds),
    formatDate(Date, Time);
sToDatetime(Seconds) ->
    io:format("huh? ~p~n", [Seconds]),
    ["foob"].

formatDate({Y, Mo, D}, {H, Mi, S}) ->
    [integer_to_list(Y), "-",
     integer_to_list(Mo), "-",
     integer_to_list(D), " ",
     integer_to_list(H), ":",
     integer_to_list(Mi), ":",
     integer_to_list(S)].


secondsToDateAndTime(TotalSeconds) ->
    BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Seconds       = BaseDate + TotalSeconds,
    {Date, Time} = calendar:gregorian_seconds_to_datetime(Seconds),
    {Date, Time}.
