-module(builderl_dtl_filters_test).

-compile({parse_transform, lager_transform}).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


-define(MUT, builderl_dtl_filters).


version_test() ->
    V = ?MUT:version(),

    ?assertEqual(1, V).

inventory_test() ->
    FI = ?MUT:inventory(filters),
    TI = ?MUT:inventory(tags),

    ?assertEqual([s_to_datetime], FI),
    ?assertEqual([], TI).


s_to_datetime_test() ->
    Seconds = 100,
    Date = ?MUT:s_to_datetime(Seconds),
    ExpectedDate = ["1970","-","1","-","1"," ","0",":","1",":","40"],
    ?assertEqual(ExpectedDate, Date).
