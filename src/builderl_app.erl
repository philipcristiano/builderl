-module(builderl_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/webhooks/github", builderl_github_webhook, []},
               {"/v0/builds/:org/:repo", builderl_builds_handler, []},
               {"/v0/builds/:org/:repo/:build", builderl_build_handler, []}]
    }]),
    {ok, _} = cowboy:start_clear(my_http_listener, 10,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

	builderl_sup:start_link().

stop(_State) ->
	ok.
