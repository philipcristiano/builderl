-module(builderl_build_handler).
-behaviour(cowboy_handler).
-compile({parse_transform, lager_transform}).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
    Org = cowboy_req:binding(org, Req0),
    Repo = cowboy_req:binding(repo, Req0),
    BuildID = binary:bin_to_list(cowboy_req:binding(build, Req0)),
    ok = lager:info("Org/Repo ~p/~p", [Org, Repo]),
    Project = string:join([binary:bin_to_list(Org), binary:bin_to_list(Repo)], "/"),
    ok = lager:info("build ~p", [BuildID]),
    {ok, Objects} = builderl_build_registry:get_build(Project, BuildID),
    {ok, Logs} = builderl:get_build_logs(Project, BuildID),
    ok = lager:info("Objects ~p", [Objects]),
    _Data = jsx:encode(#{logs => Logs}),
    Reply = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        Logs,
        Req0),
    {ok, Reply, State}.
