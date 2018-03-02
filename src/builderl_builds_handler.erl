-module(builderl_builds_handler).
-behaviour(cowboy_handler).
-compile({parse_transform, lager_transform}).

-export([init/2]).

-export([build_ids/1]).

init(Req0=#{method := <<"GET">>}, State) ->
    {Cookies, Req1} = builderl_sessions:request_start(Req0),
    Org = cowboy_req:binding(org, Req1),
    Repo = cowboy_req:binding(repo, Req1),
    ok = lager:info("Org/Repo ~p/~p", [Org, Repo]),
    Project = string:join([binary:bin_to_list(Org), binary:bin_to_list(Repo)], "/"),
    {ok, Builds} = builderl_build_registry:get_builds(Project),
    % ok = lager:debug("builds ~p", [Builds]),
    SortedBuilds = proplist_sort(time, Builds),

    Value = builderl_sessions:get_value("foo", Cookies),
    lager:info("cookie value ~p", [Value]),
    NewValue = builderl_sessions:get_value("baz", Cookies),
    lager:info("cookie value ~p", [NewValue]),
    % Data = jsx:encode(#{builds => build_ids(BuildIDs)}),
    {ok, Data} = builderl_http:render(tmpl_builds_dtl,
                                      [{builds, SortedBuilds}, {project, Project}],
                                      Cookies),
    Reply = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/html">>},
        Data,
        Req1),
    {ok, Reply, State}.

build_ids([H|T]) ->
    [binary:list_to_bin(H) | build_ids(T)];
build_ids([]) ->
    [].

proplist_sort(Key, Lists) ->
  Sortf = fun(A, B) ->
    Av = proplists:get_value(Key, A),
    Bv = proplists:get_value(Key, B),
    Av >= Bv
  end,
  lists:sort(Sortf, Lists).

% sToDate(Seconds) ->
%    BaseDate      = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
%    Seconds       = BaseDate + Seconds,
%    { Date,_Time} = calendar:gregorian_seconds_to_datetime(Seconds),
%    Date.
