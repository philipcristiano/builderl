-module(builderl_projects_handler).
-behaviour(cowboy_handler).
-compile({parse_transform, lager_transform}).

-export([init/2,
         state_init/0]).

state_init() ->
    Projects = builderl:get_projects(),
    [{projects, Projects}].

init(Req0=#{method := <<"GET">>}, State) ->
    Projects = proplists:get_value(projects, State),
    lager:info("projects ~p", [Projects]),

    {ok, Data} = tmpl_projects_dtl:render([{projects, Projects}]),

    Reply = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
        Data,
        Req0),
    {ok, Reply, State}.
