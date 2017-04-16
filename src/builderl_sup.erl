-module(builderl_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
        #{id    => builderl_build_registry,
          start => {builderl_build_registry, start_link, []}
        }
    ],
	{ok, {{one_for_one, 1, 5}, Procs}}.
