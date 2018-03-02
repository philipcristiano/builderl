-module(builderl_http).
-compile({parse_transform, lager_transform}).

-export([ render/3]).


render(Mod, Data, Cookies) ->
    GHUser = builderl_sessions:get_value(github_login, Cookies),
    UserData  = {user, [{name, GHUser}]},
    Data1 = [UserData|Data],
    lager:info("HTTP Render Data ~p", [Data1]),

    Mod:render(Data1).
