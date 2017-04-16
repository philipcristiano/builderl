-module(builderl).

-export([build/3,
         get_empty_env/0,
         get_global_env/0,
         run/0]).

-record(buildrecord, {id=undefined,
                      project=undefined,
                      stage_count=0,
                      committish=undefined,
                      ref=undefined,
                      step_count=0}).

build(Name, GitRepo, Opts) ->
    Time = erlang:monotonic_time(seconds),
    Path = "/tmp/" ++ lists:flatten(io_lib:format("build~p",[Time])),
    io:format("Build ~p~n", [{GitRepo, Path}]),
    BuilderlFile = Path ++ "/builderl.yml",
    IsDir = filelib:is_dir(Path),
    clone_if_needed(IsDir, GitRepo, Path),

    CommitIsh = proplists:get_value(commit_ish, Opts, "master"),
    Ref = proplists:get_value(ref, Opts, "Unknown"),
    {ok, BuildID} = builderl_build_registry:create(Name, Ref, CommitIsh),
    BR=#buildrecord{id=BuildID,
                    project=Name,
                    committish=CommitIsh,
                    ref=Ref},

    io:format("Commit-ish ~p~n", [CommitIsh]),
    checkout_ref(Path, CommitIsh),
    build_project(Path, BuilderlFile, BR),

    ok.

clone_if_needed(false, GitRepo, Path) ->
    io:format("Clone ~n"),
    {ok, _Text} = git:clone(GitRepo, Path),
    ok;
clone_if_needed(true, _GitRepo, Path) ->
    io:format("Fetch ~n"),
    {ok, _Test} = git:fetch(Path),
    ok.

build_project(CWD, BuilderlFile, BR=#buildrecord{}) ->
    io:format("Build project ~p~n", [BuilderlFile]),
    [BuildConfig] = yamerl_constr:file(BuilderlFile),
    io:format("File ~p~n", [BuildConfig]),
    Stages = proplists:get_value("stages", BuildConfig),
    execute_stages(Stages, CWD, BR),
    ok.

execute_stages([Stage|Stages], Dir, BR=#buildrecord{}) ->
    Steps = proplists:get_value("steps", Stage),
    execute_steps(Steps, Dir, BR#buildrecord{step_count=0}),
    execute_stages(Stages, Dir, BR#buildrecord{stage_count=BR#buildrecord.stage_count + 1}),
    ok;
execute_stages([], _Dir, _Count) ->
    ok.


execute_steps([Step|Steps], Dir, BR=#buildrecord{}) ->
    io:format("Step: ~p in ~p~n", [Step, Dir]),
    Env = get_empty_env(),
    GlobalEnv = get_global_env(),
    NewEnv = merge_env(Env, GlobalEnv),
    CombinedEnv = merge_env(NewEnv, [{"HOME", Dir}]),
    Filename = filename_from_br(BR),
    0 = builderl_process:run(Step, Dir, CombinedEnv, {file, Filename}),
    % {ok, _} = exec:run(Step, [{stdout, print}, {stderr, print}, {cd, Dir}, sync, EnvOpt]),

    execute_steps(Steps, Dir, BR#buildrecord{step_count=BR#buildrecord.step_count + 1}),
    ok;
execute_steps([], _Dir, _BR) ->
    ok.

filename_from_br(#buildrecord{id=BuildID, project=Name, step_count=Step, stage_count=Stage}) ->
    lists:flatten(["/tmp/build-project-",
                   Name,
                   "/",
                   BuildID,
                   "-",
                   erlang:integer_to_list(Stage),
                   "-",
                   erlang:integer_to_list(Step)]).

checkout_ref(Path, Ref) ->
    git:checkout(Path, Ref).

run() ->
    build("philipcristiano/AWSMF-Data",
          "https://github.com/philipcristiano/AWSMF-Data.git",
          [{ref, "package"}, {commit_ish, "package"}]).

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
