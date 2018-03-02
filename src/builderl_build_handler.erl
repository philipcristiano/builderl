-module(builderl_build_handler).
-behaviour(cowboy_handler).
-compile({parse_transform, lager_transform}).

-export([init/2]).

init(Req0=#{method := <<"GET">>}, State) ->
    {Cookies, Req1} = builderl_sessions:request_start(Req0),
    Org = cowboy_req:binding(org, Req1),
    Repo = cowboy_req:binding(repo, Req1),
    BuildID = binary:bin_to_list(cowboy_req:binding(build, Req1)),
    ok = lager:info("Org/Repo ~p/~p", [Org, Repo]),
    Project = string:join([binary:bin_to_list(Org), binary:bin_to_list(Repo)], "/"),
    ok = lager:info("build ~p", [BuildID]),

    case builderl_build_registry:get_build(Project, BuildID) of
      {ok, Objects} -> {ok, Logs} = builderl:get_build_logs(Project, BuildID),
                       SplitLogs = binary:split(Logs, <<"\n">>, [global]),
                       ok = lager:info("Objects ~p", [Objects]),
                       _Data = jsx:encode(#{logs => Logs}),
                       {ok, Data} = builderl_http:render(
                                      tmpl_build_dtl,
                                      [{logs, SplitLogs}, {project, Project}],
                                      Cookies),
                       Reply = cowboy_req:reply(200,
                                                #{<<"content-type">> => <<"text/html">>},
                                                Data,
                                                Req1),
                       {ok, Reply, State};
      {error, invalid_uuid} -> Reply = cowboy_req:reply(404, #{}, <<"">>, Req1),
                               {ok, Reply, State}
    end.
