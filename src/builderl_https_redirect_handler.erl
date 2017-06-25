-module(builderl_https_redirect_handler).
-behaviour(cowboy_handler).
-compile({parse_transform, lager_transform}).

-export([init/2]).

init(Req0, State) ->
    Domain = proplists:get_value(domain, State),
    Path = binary:bin_to_list(cowboy_req:path(Req0)),
    NewLocation = "https://" ++ Domain ++ Path,
    BinLocation = binary:list_to_bin(NewLocation),
    Req1 = cowboy_req:reply(302,
                            #{<<"Location">> => BinLocation},
                            <<>>,
                            Req0),
    {halt, Req1, State}.
