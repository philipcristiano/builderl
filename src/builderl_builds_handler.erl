-module(builderl_builds_handler).
-behaviour(cowboy_handler).
-compile({parse_transform, lager_transform}).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
    Org = cowboy_req:binding(org, Req0),
    Repo = cowboy_req:binding(repo, Req0),
    lager:info("Org/Repo ~p/~p", [Org, Repo]),
    Project = string:join([binary:bin_to_list(Org), binary:bin_to_list(Repo)], "/"),
    {ok, BuildIDs} = builderl_build_registry:get_builds(Project),
    lager:info("builds ~p", [BuildIDs]),
    Data = jsx:encode(#{builds => build_ids(BuildIDs)}),
    Reply = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        Data,
        Req0),
    {ok, Reply, State}.

build_ids([H|T]) ->
    [binary:list_to_bin(H) | build_ids(T)];
build_ids([]) ->
    [].
