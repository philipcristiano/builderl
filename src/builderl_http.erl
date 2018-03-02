-module(builderl_http).
-compile({parse_transform, lager_transform}).

-export([add_message/2,
         add_message/3,
         render/3]).

-type messagelevel() :: success | danger | warning | info.
-export_type([messagelevel/0]).

-spec add_message(messagelevel, list()) -> list().
add_message(Level, Message) ->
    add_message(Level, Message, []).
add_message(Level, Message, Messages) ->
    [ [{level, Level}, {message, Message}] | Messages].


render(Mod, Data, Cookies) ->
    GHUser = builderl_sessions:get_value(github_login, Cookies, ""),
    UserData  = {user, [{name, GHUser}]},
    Data1 = [UserData|Data],
    lager:info("HTTP Render Data ~p", [Data1]),

    Mod:render(Data1).
