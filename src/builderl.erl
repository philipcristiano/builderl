-module(builderl).

-export([build/2,
         get_empty_env/0,
         get_global_env/0,
         run/0]).


build(GitRepo, Opts) ->
    Time = erlang:monotonic_time(seconds),
    Path = "/tmp/" ++ lists:flatten(io_lib:format("build~p",[Time])),
    io:format("Build ~p~n", [{GitRepo, Path}]),
    BuilderlFile = Path ++ "/builderl.yml",
    IsDir = filelib:is_dir(Path),
    clone_if_needed(IsDir, GitRepo, Path),

    CommitIsh = proplists:get_value(commit_ish, Opts, "master"),
    io:format("Commit-ish ~p~n", [CommitIsh]),
    checkout_ref(Path, CommitIsh),
    build_project(Path, BuilderlFile),

    ok.

clone_if_needed(false, GitRepo, Path) ->
    io:format("Clone ~n"),
    git:clone(GitRepo, Path),
    ok;
clone_if_needed(true, _GitRepo, Path) ->
    io:format("Fetch ~n"),
    git:fetch(Path),
    ok.

build_project(CWD, BuilderlFile) ->
    io:format("Build project ~p~n", [BuilderlFile]),
    [BuildConfig] = yamerl_constr:file(BuilderlFile),
    io:format("File ~p~n", [BuildConfig]),
    Stages = proplists:get_value("stages", BuildConfig),
    execute_stages(Stages, CWD),
    ok.

execute_stages([Stage|Stages], Dir) ->
    Steps = proplists:get_value("steps", Stage),
    execute_steps(Steps, Dir),
    execute_stages(Stages, Dir),
    ok;
execute_stages([], _Dir) ->
    ok.


execute_steps([Step|Steps], Dir) ->
    io:format("Step: ~p in ~p~n", [Step, Dir]),
    % EnvOpt = {env, [{"foo", "bar"},
    %                 {"APP_DIR", ""}]},
    Env = get_empty_env(),
    GlobalEnv = get_global_env(),
    NewEnv = merge_env(Env, GlobalEnv),
    CombinedEnv = merge_env(NewEnv, [{"HOME", Dir}]),
    0 = builderl_process:run(Step, Dir, CombinedEnv),
    % {ok, _} = exec:run(Step, [{stdout, print}, {stderr, print}, {cd, Dir}, sync, EnvOpt]),
    execute_steps(Steps, Dir),
    ok;
execute_steps([], _Dir) ->
    ok.


checkout_ref(Path, Ref) ->
    git:checkout(Path, Ref).

run() ->
    build("https://github.com/philipcristiano/AWSMF-Data.git", [{ref, "package"}, {commit_ish, "package"}]).

get_empty_env() ->
    Env = os:getenv(),
    empty_env(Env).

empty_env([Envvar|T]) ->
    [Key, _Value] = re:split(Envvar, "=", [{parts, 2}]),
    [{binary:bin_to_list(Key), false} | empty_env(T)];
empty_env([]) ->
    [].

merge_env(Env1, Env2) ->
    orddict:merge(fun(_,_X,Y) -> Y end, orddict:from_list(Env1), orddict:from_list(Env2)).

get_global_env() ->
    Config = application:get_env(builderl, global_env, []),
    process_config(Config).

process_config([{Var, keep} | Config]) ->
    [{Var, os:getenv(Var)} | process_config(Config)];
process_config([{Var, Val} | Config]) ->
    [{Var, Val} | process_config(Config)];
process_config([]) ->
    [].
