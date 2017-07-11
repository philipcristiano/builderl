-module(builderl_github_webhook_test).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

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
     fun invalid_signature/1}.

setup_handler() ->
    application:ensure_all_started(lager),
    ok = lager_common_test_backend:bounce(debug),

    ok = meck:new(builderl, []),
    ok = meck:new(cowboy_req, []),
    ok = meck:expect(builderl, get_projects, fun() -> [project_1] end),
    ok = meck:expect(builderl, get_project_config_value, fun(_, _) -> "VALUE" end),

    [].

teardown_handler(Config) ->
    ok = meck:unload(builderl),
    ok = meck:unload(cowboy_req),
    Config.

init_projects(_Config) ->
    State0 = ?MUT:state_init(),
    Projects = proplists:get_value(projects, State0, undefined),
    [?_assertEqual([project_1], Projects)].

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



make_json_response(Data, Req) ->
    Json = jsx:encode(Data),
    {ok, Json, Req}.
