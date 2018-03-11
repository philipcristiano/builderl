-module(builderl_https_redirect_handler).
-behaviour(cowboy_handler).
-compile({parse_transform, lager_transform}).

-export([init/2]).

init(Req0, State) ->
    {_Cookies, Req1} = builderl_sessions:request_start(Req0),
    Domain = proplists:get_value(domain, State),
    Path = binary:bin_to_list(cowboy_req:path(Req1)),
    NewLocation = "https://" ++ Domain ++ Path,
    BinLocation = binary:list_to_bin(NewLocation),
    Req1 = cowboy_req:reply(302,
                            #{<<"Location">> => BinLocation},
                            <<>>,
                            Req1),
    {halt, Req1, State}.
