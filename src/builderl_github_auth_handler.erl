-module(builderl_github_auth_handler).
-behaviour(cowboy_handler).
-compile({parse_transform, lager_transform}).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
    {Cookies, Req1} = builderl_sessions:request_start(Req0),
    GHState = builderl_sessions:get_value("gh_state", Cookies),
    {CID, CS} = application:get_env(builderl, github_oauth_app_credentials, error),
    QS = cowboy_req:match_qs([code, state], Req1),
    lager:info("GHState ~p", [GHState]),
    lager:info("QS ~p", [QS]),


    case binary:bin_to_list(maps:get(state, QS)) of
        GHState ->
            lager:info("GH State matches"),
            PostData = [
                {client_id, CID},
                {client_secret, CS},
                {code, maps:get(code, QS)},
                {state, GHState}
            ],
            {ok, 200, _Headers, ClientRef} = hackney:request(get,
                                   "https://github.com/login/oauth/access_token",
                                   [{"accept", "application/json"}],
                                   {form, PostData}),
            {ok, Body} = hackney:body(ClientRef),
            BodyData = jsx:decode(Body, [return_maps]),
            save_access_token(BodyData, Cookies, Req1, State);
        _ ->
            lager:info("GH State does not match, redirecting"),
            cowboy_req:reply(302, #{<<"Location">> => <<"/github_login">>}, <<>>, Req1)
    end.

save_access_token(#{<<"access_token">> := AT} = _Resp, Cookies, Req0, State) ->
    ok = builderl_sessions:set_value(github_access_token, AT, Cookies),
    GHCreds = egithub:oauth(AT),
    {ok, UserData} = egithub:user(GHCreds),
    ok = builderl_sessions:set_value(github_login, maps:get(<<"login">>, UserData), Cookies),

    Messages = builderl_http:add_message(success, "Successfully logged into Github!"),
    RenderData = [{messages, Messages}],
    {ok, Data} = builderl_http:render(tmpl_github_login_dtl, RenderData, Cookies),
    Reply = cowboy_req:reply(200,
                             #{<<"content-type">> => <<"text/html">>},
                             Data,
                             Req0),
    {ok, Reply, State};
save_access_token(#{<<"error">> := _Error} = _Resp, Cookies, Req0, State) ->
    lager:info("Error getting GH access token"),
    %Reply = cowboy_req:reply(302, #{<<"Location">> => <<"/github_login">>}, <<>>, Req0),

    Messages = builderl_http:add_message(warning, "Error while logging into Github!"),
    RenderData = [{messages, Messages}],
    {ok, Data} = builderl_http:render(tmpl_github_login_dtl, RenderData, Cookies),
    Reply = cowboy_req:reply(200,
                             #{<<"content-type">> => <<"text/html">>},
                             Data,
                             Req0),
    {ok, Reply, State}.


