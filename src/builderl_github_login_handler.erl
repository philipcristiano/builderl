-module(builderl_github_login_handler).
-behaviour(cowboy_handler).
-compile({parse_transform, lager_transform}).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
    {Cookies, Req1} = builderl_sessions:request_start(Req0),
    {CID, _} = application:get_env(builderl, github_oauth_app_credentials, error),
    GHState = get_or_set_state(Cookies),
    QS_args = [{"client_id", CID}, {"scope", "user"}, {"state", GHState}],
    lager:info("Args ~p", [QS_args]),
    URI = urilib:build({https, undefined, "github.com", 443, "login/oauth/authorize", QS_args, undefined}),

    {ok, Data} = builderl_http:render(tmpl_github_login_dtl,
                                      [{gh_login, URI}],
                                      Cookies),
    Reply = cowboy_req:reply(200,
                             #{<<"content-type">> => <<"text/html">>},
                             Data,
                             Req1),
    {ok, Reply, State}.

get_or_set_state(Cookies) ->
    Val = builderl_sessions:get_value("gh_state", Cookies),
    get_or_set_state(Val, Cookies).

get_or_set_state(undefined, Cookies) ->
    GHState = uuid:to_string(simple, uuid:uuid4()),
    builderl_sessions:set_value("gh_state", GHState, Cookies),
    GHState;
get_or_set_state(Val, _Cookies) ->
    Val.
