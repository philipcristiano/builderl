-module(builderl_github_webhook_test).
-compile({parse_transform, lager_transform}).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").


-define(MUT, builderl_github_webhook).

init_test_() ->
    {setup,
     fun setup_handler/0,
     fun teardown_handler/1,
     fun init_projects/1}.

non_whitelist_test_() ->
    {setup,
     fun setup_handler/0,
     fun teardown_handler/1,
     fun (Config) ->
         [invalid_signature(Config)]
     end}.

no_configured_signature_test_() ->
    {setup,
     fun setup_no_sig_handler/0,
     fun teardown_handler/1,
     fun (Config) ->
          [no_signature_configured_not_whitelisted(Config)]
     end}.

push_to_delete_branch_test_() ->
    {setup,
     fun setup_no_sig_handler/0,
     fun teardown_handler/1,
     fun (Config) ->
          [delete_branch(Config)]
     end}.

setup_handler() ->
    application:ensure_all_started(lager),
    ok = lager_common_test_backend:bounce(debug),

    ok = meck:new(builderl, []),
    ok = meck:new(cowboy_req, []),
    ok = meck:expect(builderl, get_projects, fun() -> [<<"project_1">>] end),
    ok = meck:expect(builderl, get_project_config_value, fun(_, _) -> "VALUE" end),

    [].

setup_no_sig_handler() ->
    application:ensure_all_started(lager),
    ok = lager_common_test_backend:bounce(debug),

    ok = meck:new(builderl, []),
    ok = meck:new(cowboy_req, []),
    ok = meck:expect(builderl, get_projects, fun() -> [<<"project_1">>] end),
    ok = meck:expect(builderl, get_project_config_value, fun(_, _) -> undefined end),

    [].

teardown_handler(Config) ->
    ok = meck:unload(builderl),
    ok = meck:unload(cowboy_req),
    Config.

init_projects(_Config) ->
    State0 = ?MUT:state_init(),
    Projects = proplists:get_value(projects, State0, undefined),
    [?_assertEqual([<<"project_1">>], Projects)].

invalid_signature(_Config) ->
    State0 = ?MUT:state_init(),
    PostReq = #{method => <<"POST">>} ,

    Data = #{<<"repository">> =>
                #{<<"url">> => <<"repo_url">>,
                  <<"full_name">> => <<"test_full_name">>
                },
             <<"ref">> => <<"test_ref">>,
             <<"after">> => <<"after_sha">>
    },

    ok = meck:expect(cowboy_req, read_body, fun(Req) -> make_json_response(Data, Req) end),
    ok = meck:expect(cowboy_req, header, fun(_, _) -> <<"signature">> end),

    Ref = make_ref(),
    ok = meck:expect(cowboy_req, reply, fun(400, _, Body, _Req) -> {Ref, Body} end),
    {ok, {RRef, Body}, _} = ?MUT:init(PostReq, State0),
    [?_assertEqual(Ref, RRef),
     ?_assertEqual(<<"Invalid signature">>, Body)].

no_signature_configured_not_whitelisted(_Config) ->
    State0 = ?MUT:state_init(),
    PostReq = #{method => <<"POST">>} ,

    Data = #{<<"repository">> =>
                #{<<"url">> => <<"repo_url">>,
                  <<"full_name">> => <<"test_full_name">>
                },
             <<"ref">> => <<"test_ref">>,
             <<"after">> => <<"after_sha">>
    },

    ok = meck:expect(cowboy_req, read_body, fun(Req) -> make_json_response(Data, Req) end),
    ok = meck:expect(cowboy_req, header, fun(_, _) -> undefined end),

    Ref = make_ref(),
    ok = meck:expect(cowboy_req, reply, fun(400, _, Body, _Req) -> {Ref, Body} end),
    {ok, {RRef, Body}, _} = ?MUT:init(PostReq, State0),
    DoesMatch = re_match(Body, ".*not in the builderl whitelist.*"),
    [?_assertEqual(Ref, RRef),
     ?_assert(DoesMatch)].

delete_branch(_Config) ->
    State0 = ?MUT:state_init(),
    PostReq = #{method => <<"POST">>} ,

    Data = #{<<"repository">> =>
                #{<<"url">> => <<"repo_url">>,
                  <<"full_name">> => <<"project_1">>
                },
             <<"ref">> => <<"test_ref">>,
             <<"after">> => <<"0000000000000000000000000000000000000000">>
    },

    ok = meck:expect(cowboy_req, read_body, fun(Req) -> make_json_response(Data, Req) end),
    ok = meck:expect(cowboy_req, header, fun(_, _) -> undefined end),

    Ref = make_ref(),
    ok = meck:expect(cowboy_req, reply, fun(200, _, Body, _Req) -> {Ref, Body} end),
    {ok, {RRef, Body}, _} = ?MUT:init(PostReq, State0),
    DoesMatch = re_match(Body, ".*deleted branch, not triggering a build.*"),
    [?_assertEqual(Ref, RRef),
     ?_assert(DoesMatch)].


make_json_response(Data, Req) ->
    Json = jsx:encode(Data),
    {ok, Json, Req}.

re_match(Subject, RE) ->
    case re:run(Subject, RE) of
        nomatch -> ok = lager:info("~p does not match ~p", [RE, Subject]),
                   false;
        {match, _} -> true
    end.
