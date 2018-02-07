-module(builderl_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile({parse_transform, lager_transform}).
-compile(export_all).



-define(MUT, builderl).

all() -> [{group, test_init}].

groups() -> [{test_init,
             [{create_priv_dir, auto_per_tc}],
             [aa_build_echo_hello_world]}].



init_per_testcase(_, Config) ->
    ok = lager_common_test_backend:bounce(debug),
    application:set_env(builderl, builderl, global_env, [{"PATH", "/bin"}]),
    application:set_env(builderl, domain, "test-domain"),
    application:ensure_all_started(yamerl),
    PrivDir = ?config(priv_dir, Config),
    {ok, _Pid} = builderl_build_registry:start_link(PrivDir),
    ok = meck:new(builderl_github, []),
    % lager:debug("CT Config ~p", [Config]),

    % ok = meck:new(erlcloud_aws, []),
    % ok = meck:new(sssftp_user_session, []),
    % Contents = [
    %     [{key, "uploads/USER/file.txt"},
    %      {content_length, 1024}]],

    ok = meck:expect(builderl_github, create_status, fun(_,_,_,_,_,_) -> ok end),
    % ok = meck:expect(erlcloud_s3, list_objects, fun(_, _, _) -> [{contents, Contents}] end),
    % ok = meck:expect(erlcloud_aws, auto_config, fun() -> {ok, autoconfig} end),
    % ok = meck:expect(sssftp_user_session, get, fun(user_auth_server, _) -> {ok, "USER"} end),

    % State0 = [{aws_bucket, "TESTBUCKET"},
    %           {user_auth_server, user_auth_server}],
    % {_, State1} = ?MUT:get_cwd(State0),

    % InitState = {initstate, State1},
    % [InitState | Config].
    Config.

end_per_testcase(_, Config) ->
    ok = meck:unload(builderl_github),
    % ok = meck:unload(sssftp_user_session),
    Config.

aa_build_echo_hello_world(Config) ->
    PrivDir = ?config(priv_dir, Config),
    lager:debug("Config ~p", [Config]),
    Project = "echo/hello-world",


    {ok, ID} = ?MUT:build(Project,
                          "https://github.com/philipcristiano/builderl_test_project.git",
                          [{spawn_fun, fun erlang:spawn_link/3},
                           {builds_directory, PrivDir}]),

    {ok, successful} = wait_for_build_state(Project, ID, [successful, failed]),
    ok.

wait_for_build_state(Project, ID, States) ->
    {ok, Build} = builderl_build_registry:get_build(Project, ID),
    BuildState = proplists:get_value(state, Build),

    case lists:member(BuildState, States) of
        true -> {ok, BuildState};
        false -> ok = lager:debug("State of ~p is not in ~p", [BuildState, States]),
                 timer:sleep(1000),
                 wait_for_build_state(Project, ID, States)
    end.
