-module(builderl_test).
-compile({parse_transform, lager_transform}).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


-define(MUT, builderl).

validate_builderl_build_config_test_() ->
    [non_string_environment_config(),
     valid_config()].

non_string_environment_config() ->
    {error, ErrMsg} = ?MUT:validate_builderl_config([{"environment", [{"foo", bar}]}]),
    DoesMatch = re_match(ErrMsg, "Environment variables must be strings"),

    [?_assert(DoesMatch)].

valid_config() ->
    Config = [{"environment", [{"foo", "bar"}]}],
    Resp = ?MUT:validate_builderl_config(Config),
    [?_assertEqual(ok, Resp)].

re_match(Subject, RE) ->
    case re:run(Subject, RE) of
        nomatch -> ok = lager:info("~p does not match ~p", [RE, Subject]),
                   false;
        {match, _} -> true
    end.

get_projects_test_() ->
    ProjectConfig = [{"project_1", config}],
    meck:new(application, [unstick]),
    meck:expect(application, get_env, fun(builderl, projects, []) -> ProjectConfig end),

    Projects = builderl:get_projects(),

    meck:unload(application),

    [?_assertEqual([<<"project_1">>], Projects)].


get_project_config_test_() ->
    {setup,
     fun setup_project_config/0,
     fun teard_project_config/1,
     fun (Config) ->
         [get_project_config_value(Config),
          get_project_config_no_value(Config),
          get_project_config_no_project(Config)]
     end}.

setup_project_config() ->
    ProjectConfig = [{"project_1", [{key, value}]}],
    meck:new(application, [unstick]),
    meck:expect(application, get_env, fun(builderl, projects, []) -> ProjectConfig end),
    [].

teard_project_config(_Config) ->
    meck:unload(application).

get_project_config_value(_Config) ->
    Val = builderl:get_project_config_value(<<"project_1">>, key),
    [?_assertEqual(value, Val)].
get_project_config_no_value(_Config) ->
    Val = builderl:get_project_config_value(<<"project_1">>, invalid_key),
    [?_assertEqual(undefined, Val)].
get_project_config_no_project(_Config) ->
    Val = builderl:get_project_config_value(<<"invalid_project">>, key),
    [?_assertEqual(undefined, Val)].
