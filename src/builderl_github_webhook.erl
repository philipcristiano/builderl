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

    % Validate project is in whitelist
    Repo = maps:get(<<"repository">>, Data, #{}),
    FullName = maps:get(<<"full_name">>, Repo),
    IsMember = lists:member(FullName, Projects),

    Resp = handle_webhook(IsMember, FullName, Data, Repo, Req1),
    {ok, Resp, State}.

handle_webhook(true, FullName, Data, Repo, Req) ->
    Url = maps:get(<<"url">>, Repo),
    Ref = maps:get(<<"ref">>, Data),
    CommitIsh = binary:bin_to_list(maps:get(<<"after">>, Data)),

    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        << <<"Hello Erlang! ">>/binary, FullName/binary, Url/binary, Ref/binary >>,
        Req),

    NameL = binary:bin_to_list(FullName),
    LRef = binary:bin_to_list(Ref),
    trigger_build(NameL, Url, CommitIsh, LRef),
    Req2;

handle_webhook(false, FullName, _Data, _Repo, Req) ->
    Req2 = cowboy_req:reply(400,
        #{<<"content-type">> => <<"text/plain">>},
        << FullName/binary, <<" is not in the builderl whitelist">>/binary >>,
        Req),
    Req2.

trigger_build(FullName, Url, CommitIsh, Ref) ->
    io:format("Building~n"),
    builderl:build(FullName, Url, [{commit_ish, CommitIsh},
                                   {ref, Ref}]).
