-module(builderl_sessions).
-compile({parse_transform, lager_transform}).

-export([request_start/1,
         get_value/2,
         set_value/3]).

-define(STORAGE, builderl_ets_session_store).
-define(SESSION_KEY, <<"session_id">>).

request_start(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    lager:debug("Cookies: ~p", [Cookies]),
    Req1 = init_cookies(Cookies, Req),
    {Cookies, Req1}.

set_value(Key, Value, Cookies) ->
    SID = proplists:get_value(?SESSION_KEY, Cookies),
    ?STORAGE:set_value(Key, Value, SID).

get_value(Key, Cookies) ->
    SID = proplists:get_value(?SESSION_KEY, Cookies),
    ?STORAGE:get_value(Key, SID).

init_cookies([], Req) ->
    ID = uuid:uuid4(),
    SID = uuid:to_string(simple, ID),
    cowboy_req:set_resp_cookie(<<"session_id">>, SID, Req, #{http_only => true,
                                                             path => "/"});
init_cookies(_, Req) ->
    Req.
