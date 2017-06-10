-module(builderl_github_webhook).
-behavior(cowboy_handler).
-compile({parse_transform, lager_transform}).

-export([init/2,
         state_init/0]).

state_init() ->
    Projects = builderl:get_projects(),
    lager:info("Whitelisted projects ~p", [Projects]),
    [{projects, Projects}].

init(Req0=#{method := <<"POST">>}, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Projects = proplists:get_value(projects, State),
    Data = jsx:decode(Body, [return_maps]),

    Repo = maps:get(<<"repository">>, Data, #{}),
    FullName = maps:get(<<"full_name">>, Repo),
    Url = maps:get(<<"url">>, Repo),
    Ref = maps:get(<<"ref">>, Data),
    CommitIsh = binary:bin_to_list(maps:get(<<"after">>, Data)),

    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        << <<"Hello Erlang! ">>/binary, FullName/binary, Url/binary, Ref/binary >>,
        Req1),

    io:format("Member ~p~n", [FullName]),
    NameL = binary:bin_to_list(FullName),
    LRef = binary:bin_to_list(Ref),
    IsMember = lists:member(FullName, Projects),
    trigger_build(IsMember, NameL, Url, CommitIsh, LRef),

    {ok, Req2, State}.

trigger_build(true, FullName, Url, CommitIsh, Ref) ->
    io:format("Building~n"),
    builderl:build(FullName, Url, [{commit_ish, CommitIsh},
                                   {ref, Ref}]);
trigger_build(false, _FullName, _Url, _CommitIsh, _Ref) ->
    io:format("Not a whitelisted project~n"),
    ok.
