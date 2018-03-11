-module(builderl_projects_handler).
-behaviour(cowboy_handler).
-compile({parse_transform, lager_transform}).

-export([init/2,
         state_init/0]).

state_init() ->
    Projects = builderl:get_projects(),
    [{projects, Projects}].

init(Req0=#{method := <<"GET">>}, State) ->
    {Cookies, Req1} = builderl_sessions:request_start(Req0),
    builderl_sessions:set_value("foo", "bar", Cookies),
    Projects = proplists:get_value(projects, State),
    ok = lager:info("projects ~p", [Projects]),

    {ok, Data} = builderl_http:render(tmpl_projects_dtl, [{projects, Projects}], Cookies),

    Reply = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
        Data,
        Req1),
    {ok, Reply, State}.
