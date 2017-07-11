-module(builderl_github_webhook_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(MUT, builderl_github_webhook).

init_test_() ->
    {setup,
     fun setup_handler/0,
     fun teardown_handler/1,
     fun init_projects/1}.


setup_handler() ->
    application:ensure_all_started(lager),
    ok = lager_common_test_backend:bounce(debug),

    ok = meck:new(builderl, []),
    ok = meck:expect(builderl, get_projects, fun() -> [project_1] end),

    [].

teardown_handler(Config) ->
    ok = meck:unload(builderl),
    Config.

init_projects(_Config) ->
    State0 = ?MUT:state_init(),
    Projects = proplists:get_value(projects, State0, undefined),
    [?_assertEqual([project_1], Projects)].
