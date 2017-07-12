-module(builderl_github_webhook).
-behavior(cowboy_handler).
-compile({parse_transform, lager_transform}).

-export([init/2,
         state_init/0]).

-record(push_event, {full_name,
                     url,
                     ref,
                     committish}).


state_init() ->
    Projects = builderl:get_projects(),
    ok = lager:info("Whitelisted projects ~p", [Projects]),
    [{projects, Projects}].

init(Req0=#{method := <<"POST">>}, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Projects = proplists:get_value(projects, State),
    ok = lager:debug("Body ~p", [Req0]),
    {ok, PE} = post_body_to_record(Body),

    GHSignature = cowboy_req:header(<<"x-hub-signature">>, Req1),
    Secret = builderl:get_project_config_value(PE#push_event.full_name, github_webhook_secret),

    Resp = case valid_signature(GHSignature, Secret, Body) of
        false -> ok = lager:warning("Invalid signature for project ~p with sig ~p",
                               [PE#push_event.full_name, GHSignature]),
                 cowboy_req:reply(400,
                                  #{<<"content-type">> => <<"text/plain">>},
                                    <<"Invalid signature">>, Req1);
        true  -> IsMember = lists:member(PE#push_event.full_name, Projects),
                 handle_whitelist_member(IsMember, PE, Req1)
    end,
    {ok, Resp, State}.

valid_signature(_Signature, undefined, _Body) ->
    ok = lager:debug("No signature for us :/"),
    true;
valid_signature(Signature, Secret, Body) when is_list(Secret)->
    <<Mac:160/integer>> = crypto:hmac(sha, binary:list_to_bin(Secret), Body),
    OurHash = binary:list_to_bin(lists:flatten(io_lib:format("~40.16.0b", [Mac]))),
    OurSig = << <<"sha1=">>/binary, OurHash/binary>>,
    ok = lager:debug("Our signature ~p, their signature ~p", [OurSig, Signature]),
    OurSig == Signature.

post_body_to_record(Body) ->
    Data = jsx:decode(Body, [return_maps]),
    Repo = maps:get(<<"repository">>, Data, #{}),
    Url = maps:get(<<"url">>, Repo),
    Ref = maps:get(<<"ref">>, Data),
    CommitIsh = binary:bin_to_list(maps:get(<<"after">>, Data)),
    FullName = maps:get(<<"full_name">>, Repo),

    {ok, #push_event{full_name=FullName,
                     url=Url,
                     ref=Ref,
                     committish=CommitIsh}}.


handle_whitelist_member(true, #push_event{full_name=FullName, url=Url, ref=Ref, committish=CommitIsh}, Req) ->

    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        << <<"Hello Erlang! ">>/binary, FullName/binary, Url/binary, Ref/binary >>,
        Req),

    NameL = binary:bin_to_list(FullName),
    LRef = binary:bin_to_list(Ref),
    {ok, _} = trigger_build(NameL, Url, CommitIsh, LRef),
    Req2;

handle_whitelist_member(false, #push_event{full_name=FullName}, Req) ->
    Req2 = cowboy_req:reply(400,
        #{<<"content-type">> => <<"text/plain">>},
        << FullName/binary, <<" is not in the builderl whitelist">>/binary >>,
        Req),
    Req2.

trigger_build(FullName, Url, CommitIsh, Ref) ->
    ok = lager:debug("Trigger build from webhook for ~p", [FullName]),
    builderl:build(FullName, Url, [{commit_ish, CommitIsh},
                                   {ref, Ref}]).
