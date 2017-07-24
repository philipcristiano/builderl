-module(builderl_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  FileBase = application:get_env(builderl, root, "tmp"),

	Procs = [
        #{id    => builderl_build_registry,
          start => {builderl_build_registry, start_link, [FileBase]}
        }
    ],
	{ok, {{one_for_one, 1, 5}, Procs}}.
