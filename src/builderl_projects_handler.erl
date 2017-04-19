-module(builderl_projects_handler).
-behaviour(cowboy_handler).
-compile({parse_transform, lager_transform}).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
    {ok, Projects} = builderl_build_registry:get_projects(),
    lager:info("projects ~p", [Projects]),

    {ok, Data} = tmpl_projects_dtl:render([{projects, Projects}]),

    Reply = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
        Data,
        Req0),
    {ok, Reply, State}.
