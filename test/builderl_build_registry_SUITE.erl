-module(builderl_build_registry_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile({parse_transform, lager_transform}).
-compile(export_all).



-define(MUT, builderl_build_registry).

all() -> [{group, test_init}].

groups() -> [{test_init,
             [{create_priv_dir, auto_per_tc}],
             [aa_create_and_get_build,
              ab_create_and_list_builds,
              ac_create_and_change_state]}].



init_per_testcase(_, Config) ->
    ok = lager_common_test_backend:bounce(debug),
    Config.

end_per_testcase(_, Config) ->
    Config.

aa_create_and_get_build(Config) ->
    PrivDir = ?config(priv_dir, Config),
    {ok, _Pid} = ?MUT:start_link(PrivDir),
    Project = "project",
    Ref = "ref",
    Commitish = "committish",

    {ok, ID} = ?MUT:create(Project, Ref, Commitish),
    {ok, Build} = ?MUT:get_build(Project, ID),
    ?assertEqual(Project  , proplists:get_value(project, Build)),
    ?assertEqual(Ref      , proplists:get_value(ref, Build)),
    ?assertEqual(Commitish, proplists:get_value(commitish, Build)),
    ok.

ab_create_and_list_builds(Config) ->
    PrivDir = ?config(priv_dir, Config),
    {ok, _Pid} = ?MUT:start_link(PrivDir),
    Project = "project",
    Ref = "ref",
    Commitish = "committish",

    {ok, _ID} = ?MUT:create(Project, Ref, Commitish),
    {ok, Builds} = ?MUT:get_builds(Project),

    lager:debug("Builds ~p", [Builds]),
    lager:debug("Build l ~p", [length(Builds)]),
    ?assertEqual(1        , length(Builds)),

    [Build] = Builds,
    ?assertEqual(Project  , proplists:get_value(project, Build)),
    ?assertEqual(Ref      , proplists:get_value(ref, Build)),
    ?assertEqual(Commitish, proplists:get_value(commitish, Build)),

    ok.

ac_create_and_change_state(Config) ->
    PrivDir = ?config(priv_dir, Config),
    {ok, _Pid} = ?MUT:start_link(PrivDir),
    Project = "project",
    Ref = "ref",
    Commitish = "committish",

    {ok, ID} = ?MUT:create(Project, Ref, Commitish),
    {ok, Build1} = ?MUT:get_build(Project, ID),
    ?assertEqual(created  , proplists:get_value(state, Build1)),
    ?assertEqual(Project  , proplists:get_value(project, Build1)),
    ?assertEqual(Ref      , proplists:get_value(ref, Build1)),
    ?assertEqual(Commitish, proplists:get_value(commitish, Build1)),

    ok = ?MUT:set_build_state(ID, running),

    {ok, Build2} = ?MUT:get_build(Project, ID),
    ?assertEqual(running  , proplists:get_value(state, Build2)),
    ?assertEqual(Project  , proplists:get_value(project, Build2)),
    ?assertEqual(Ref      , proplists:get_value(ref, Build2)),
    ?assertEqual(Commitish, proplists:get_value(commitish, Build2)),

    ok.
