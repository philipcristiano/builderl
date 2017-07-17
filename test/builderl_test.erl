-module(builderl_test).
-compile({parse_transform, lager_transform}).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


-define(MUT, builderl).

validate_builderl_build_config_test_() ->
    [non_string_environment_config()].

non_string_environment_config() ->
    {error, ErrMsg} = ?MUT:validate_builderl_config([{"environment", [{"foo", bar}]}]),
    DoesMatch = re_match(ErrMsg, "Environment variables must be strings"),

    [?_assert(DoesMatch)].

re_match(Subject, RE) ->
    case re:run(Subject, RE) of
        nomatch -> ok = lager:info("~p does not match ~p", [RE, Subject]),
                   false;
        {match, _} -> true
    end.
