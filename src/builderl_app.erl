-module(builderl_app).
-behaviour(application).
-compile({parse_transform, lager_transform}).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/webhooks/github", builderl_github_webhook, []},
               {"/projects", builderl_projects_handler, []},
               {"/builds/:org/:repo", builderl_builds_handler, []},
               {"/builds/:org/:repo/:build", builderl_build_handler, []}]
    }]),
    Port = application:get_env(builderl, http_port, 8080),
    {ok, _} = cowboy:start_clear(my_http_listener, 10,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    lager:info("HTTP listening on port ~p", [Port]),

	  builderl_sup:start_link().

stop(_State) ->
	  ok.
