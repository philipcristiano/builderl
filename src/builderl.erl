-module(builderl).
-compile({parse_transform, lager_transform}).

-export([build/3,
         build_proc/1,
         get_build_logs/2,
         get_empty_env/0,
         get_global_env/0,
         get_projects/0,
         run/0]).

-record(buildrecord, {id=undefined,
                      project=undefined,
                      repo=undefined,
                      stage_count=0,
                      committish=undefined,
                      ref=undefined,
                      step_count=0}).

get_projects() ->
    Projects = application:get_env(builderl, projects, []),
    BinProjects = [ binary:list_to_bin(P) || P <- Projects ],
    BinProjects.

build(Name, GitRepo, Opts) ->
    Ref = proplists:get_value(ref, Opts, "Unknown"),
    CommitIsh = proplists:get_value(commit_ish, Opts, "master"),

    {ok, BuildID} = builderl_build_registry:create(Name, Ref, CommitIsh),
    BR=#buildrecord{id=BuildID,
                    project=Name,
                    repo=GitRepo,
                    committish=CommitIsh,
                    ref=Ref},
    erlang:spawn(builderl, build_proc, [BR]),
    {ok, BuildID}.

build_proc(BR=#buildrecord{committish=CommitIsh, repo=GitRepo}) ->
    lager:debug("Commit-ish ~p", [CommitIsh]),

    Time = erlang:monotonic_time(seconds),
    BuildDir = application:get_env(builderl, builds_directory, "/tmp"),
    Path = BuildDir ++ "/" ++ lists:flatten(io_lib:format("build~p",[Time])),
    lager:debug("Build ~p", [{GitRepo, Path}]),
    BuilderlFile = Path ++ "/builderl.yml",
    IsDir = filelib:is_dir(Path),
    clone_if_needed(IsDir, GitRepo, Path),

    checkout_ref(Path, CommitIsh),
    build_project(Path, BuilderlFile, BR),

    ok.

clone_if_needed(false, GitRepo, Path) ->
    lager:debug("Clone "),
    {ok, _Text} = git:clone(GitRepo, Path),
    ok;
clone_if_needed(true, _GitRepo, Path) ->
    lager:debug("Fetch "),
    {ok, _Test} = git:fetch(Path),
    ok.

build_project(CWD, BuilderlFile, BR=#buildrecord{}) ->
    lager:debug("Build project ~p", [BuilderlFile]),
    [BuildConfig] = yamerl_constr:file(BuilderlFile),
    lager:debug("File ~p", [BuildConfig]),
    Stages = proplists:get_value("stages", BuildConfig),
    BuildFileEnv = proplists:get_value("environment", BuildConfig, []),
    lager:debug("FileEnv ~p", [BuildFileEnv]),
    execute_stages(Stages, CWD, BR, BuildFileEnv),
    ok.

execute_stages([Stage|Stages], Dir, BR=#buildrecord{ref=Ref}, BuildFileEnv) ->
    {_RefType, ShortRef} = short_ref(Ref),

    Steps = proplists:get_value("steps", Stage),
    RefMatcher = proplists:get_value("match", Stage, ".*"),
    case re:run(ShortRef, RefMatcher) of
        % No match or error, don't need to run anything
        nomatch -> ok;
        {error, _ErrorType} -> ok;
        % match or {match, captured} run these steps
        _ -> execute_steps(Steps, Dir, BR#buildrecord{step_count=0}, BuildFileEnv)
    end,
    execute_stages(Stages,
                   Dir,
                   BR#buildrecord{stage_count=BR#buildrecord.stage_count + 1},
                   BuildFileEnv),
    ok;
execute_stages([], _Dir, #buildrecord{id=ID}, _BuildFileEnv) ->
    builderl_build_registry:set_build_state(ID, successful),
    ok.


short_ref(Ref) ->
    [<<"refs">>, B, Rest] = re:split(Ref, "/", [{parts , 3}]),
    short_ref(B, Rest).

short_ref(<<"tags">>, ShortRef) ->
    {tag, ShortRef};
short_ref(<<"heads">>, ShortRef) ->
    {branch, ShortRef}.

execute_steps([Step|Steps], Dir, BR=#buildrecord{}, BuildFileEnv) ->
    lager:debug("Step: ~p in ~p", [Step, Dir]),

    Env = get_empty_env(),
    GlobalEnv = get_global_env(),
    BREnv = get_env_from_br(BR),
    NeededEnv = [{"HOME", Dir}],

    Env1 = merge_env(Env, GlobalEnv),
    Env2 = merge_env(Env1, BREnv),
    Env3 = merge_env(Env2, NeededEnv),
    Env4 = merge_env(Env3, BuildFileEnv),

    Filename = filename_from_br(BR),
    Status = builderl_process:run(Step, Dir, Env4, {file, Filename}),
    % {ok, _} = exec:run(Step, [{stdout, print}, {stderr, print}, {cd, Dir}, sync, EnvOpt]),
    case Status of
        0 -> execute_steps(Steps,
                           Dir,
                           BR#buildrecord{step_count=BR#buildrecord.step_count + 1},
                           BuildFileEnv);
        _ -> lager:debug("Step exited non-zero")
    end,
    ok;
execute_steps([], _Dir, _BR, _BuildFileEnv) ->
    ok.

get_build_logs(Project, BuildID) ->
    BR = #buildrecord{id=BuildID, project=Project},
    {ok, Data} = read_build(BR),
    {ok, Data}.

read_build(BR) ->
    Filename = filename_from_br(BR),
    file:read_file(Filename).

filename_from_br(#buildrecord{id=BuildID, project=Name}) ->
    BuildDir = application:get_env(builderl, build_logs_directory, "/tmp"),
    lists:flatten([BuildDir,
                   "/build-project-",
                   Name,
                   "/",
                   BuildID]).

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

get_env_from_br(#buildrecord{id=BuildId, project=Name, committish=C, ref=Ref}) ->
    [{"BUILDERL_BUILD_ID", BuildId},
     {"BUILDERL_PROJECT", Name},
     {"BUILDERL_COMMITTISH", C},
     {"BUILDERL_REF", Ref}].

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
