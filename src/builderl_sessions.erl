-module(builderl_sessions).
-compile({parse_transform, lager_transform}).

-export([request_start/1,
         get_value/2,
         set_value/3]).

request_start(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    lager:debug("Cookies: ~p", [Cookies]),
    Req1 = init_cookies(Cookies, Req),
    {Cookies, Req1}.

set_value(_Key, _Value, _Req) ->
    ok.

get_value(_Key, _Req) ->
    ok.

init_cookies([], Req) ->
    ID = uuid:uuid4(),
    SID = uuid:to_string(simple, ID),
    cowboy_req:set_resp_cookie(<<"session_id">>, SID, Req, #{http_only => true});
init_cookies(_, Req) ->
    Req.
