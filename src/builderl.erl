-module(builderl).
-compile({parse_transform, lager_transform}).

-export([build/3,
         build_proc/2,
         get_build_logs/2,
         get_empty_env/0,
         get_global_env/0,
         get_projects/0,
         get_project_config_value/2,
         validate_builderl_config/1,
         run/0]).

-record(buildrecord, {id=undefined,
                      project=undefined,
                      repo=undefined,
                      stage_count=0,
                      committish=undefined,
                      ref=undefined,
                      step_count=0}).

-spec get_projects() -> list().
get_projects() ->
    Projects = application:get_env(builderl, projects, []),
    BinProjects = [ binary:list_to_bin(P) || {P, _Config} <- Projects ],
    BinProjects.

-spec get_project_config_value(binary(), atom()) -> any().
get_project_config_value(BProject, Key) when is_binary(BProject) ->
    Project = binary:bin_to_list(BProject),
    Projects = application:get_env(builderl, projects, []),
    Config = proplists:get_value(Project, Projects, []),
    proplists:get_value(Key, Config, undefined).

build(Name, GitRepo, Opts) ->
    Ref = proplists:get_value(ref, Opts, "refs/heads/master"),
    CommitIsh = proplists:get_value(commit_ish, Opts, "master"),

    {ok, BuildID} = builderl_build_registry:create(Name, Ref, CommitIsh),
    lager:info("Aboud to send info to Github"),
    Resp = builderl_github:create_status(GitRepo, CommitIsh, pending, "Build starting", "builderl", "http://example.com"),
    lager:info("Create_status ~p", [Resp]),
    BR=#buildrecord{id=BuildID,
                    project=Name,
                    repo=GitRepo,
                    committish=CommitIsh,
                    ref=Ref},
    SpawnFun = proplists:get_value(spawn_fun, Opts, fun erlang:spawn/3),
    SpawnFun(builderl, build_proc, [BR, Opts]),
    {ok, BuildID}.

build_proc(BR=#buildrecord{id=ID, committish=CommitIsh, repo=GitRepo}, Opts) ->
    ok = lager:debug("Commit-ish ~p", [CommitIsh]),
    Time = erlang:monotonic_time(seconds),

    BuildDirArg = proplists:get_value(builds_directory, Opts, "/tmp"),
    BuildDir = application:get_env(builderl, builds_directory, BuildDirArg),

    Path = BuildDir ++ "/" ++ lists:flatten(io_lib:format("build~p",[Time])),
    ok = lager:debug("Build ~p", [{GitRepo, Path}]),
    BuilderlFile = Path ++ "/builderl.yml",
    IsDir = filelib:is_dir(Path),
    ok = case clone_if_needed(IsDir, GitRepo, Path) of
        ok -> ok;
        error -> builderl_build_registry:set_build_state(ID, clone_failed)
    end,
    ok = case checkout_ref(Path, CommitIsh, BR) of
        ok -> ok;
        error -> builderl_build_registry:set_build_state(ID, checkout_failed),
                 error
    end,
    build_project(Path, BuilderlFile, BR),
    ok.

clone_if_needed(false, GitRepo, Path) ->
    ok = lager:debug("Clone "),
    case git:clone(GitRepo, Path) of
      {ok, _Text} -> ok;
      Error -> ok = lager:debug("Clone failed ~p", [Error]),
             error
    end,
    ok;
clone_if_needed(true, _GitRepo, Path) ->
    ok = lager:debug("Fetch "),
    case git:fetch(Path) of
      {ok, _Text} -> ok;
      Error -> ok = lager:debug("Fetch failed ~p", [Error]),
             error
    end,
    ok.

build_project(CWD, BuilderlFile, BR=#buildrecord{id=ID}) ->
    ok = lager:debug("Build project ~p", [BuilderlFile]),
    builderl_build_registry:set_build_state(ID, running),
    [BuildConfig] = yamerl_constr:file(BuilderlFile),
    ok = lager:debug("File ~p", [BuildConfig]),
    Stages = proplists:get_value("stages", BuildConfig),
    BuildFileEnv = proplists:get_value("environment", BuildConfig, []),
    case validate_builderl_config(BuildConfig) of
        ok -> execute_stages(Stages, CWD, BR, BuildFileEnv);
        {error, ErrorMsg} -> build_record_message(BR, ErrorMsg),
                             builderl_build_registry:set_build_state(ID, config_error)
    end,
    ok.

execute_stages([Stage|Stages], Dir, BR=#buildrecord{ref=Ref}, BuildFileEnv) ->
    {_RefType, ShortRef} = short_ref(Ref),

    Steps = proplists:get_value("steps", Stage),
    RefMatcher = proplists:get_value("match", Stage, ".*"),
    StepState = case re:run(ShortRef, RefMatcher) of
        % No match or error, don't need to run anything
        nomatch -> ok;
        % match or {match, captured} run these steps
        {match, _} -> execute_steps(Steps, Dir, BR#buildrecord{step_count=0}, BuildFileEnv)
    end,
    case StepState of
        ok -> execute_stages(Stages,
                             Dir,
                             BR#buildrecord{stage_count=BR#buildrecord.stage_count + 1},
                             BuildFileEnv);
        _ -> ok
    end;
execute_stages([], _Dir, #buildrecord{id=ID}, _BuildFileEnv) ->
    builderl_build_registry:set_build_state(ID, successful).

short_ref(Ref) ->
    [<<"refs">>, B, Rest] = re:split(Ref, "/", [{parts , 3}]),
    short_ref(B, Rest).

short_ref(<<"tags">>, ShortRef) ->
    {tag, ShortRef};
short_ref(<<"heads">>, ShortRef) ->
    {branch, ShortRef}.

execute_steps([Step|Steps], Dir, BR=#buildrecord{id=ID}, BuildFileEnv) ->
    ok = lager:debug("Step: ~p in ~p", [Step, Dir]),

    Env = get_empty_env(),
    GlobalEnv = get_global_env(),
    BREnv = get_env_from_br(BR),
    NeededEnv = [{"HOME", Dir}],

    Env1 = merge_env(Env, GlobalEnv),
    Env2 = merge_env(Env1, BREnv),
    Env3 = merge_env(Env2, NeededEnv),
    Env4 = merge_env(Env3, BuildFileEnv),
    ok = lager:debug("Build env ~p", [Env4]),

    Filename = filename_from_br(BR),
    Status = builderl_process:run(Step, Dir, Env4, {file, Filename}),
    % {ok, _} = exec:run(Step, [{stdout, print}, {stderr, print}, {cd, Dir}, sync, EnvOpt]),
    case Status of
        0 -> execute_steps(Steps,
                           Dir,
                           BR#buildrecord{step_count=BR#buildrecord.step_count + 1},
                           BuildFileEnv);
        _ -> ok = builderl_build_registry:set_build_state(ID, failed),
             ok = lager:debug("Step exited non-zero"),
             error
    end;
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

checkout_ref(Path, Ref, BR) ->
    build_record_message(BR, ["Checking out project ", Ref, " to ", Path]),
    Resp = git:checkout(Path, Ref),
    handle_checkout(Resp, BR).

handle_checkout({ok, _Msg}, BR) ->
    build_record_message(BR, ["Checkout succeeded!"]),
    ok;
handle_checkout({_, {_Code, Msg}}, BR) ->
    build_record_message(BR, ["Checkout failed! ", Msg]),
    error.

build_record_message(BR, Msg) ->
    Filename = filename_from_br(BR),
    ok = builderl_process:save_msg({file, Filename}, Msg).

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


-spec validate_builderl_config(list()) -> ok | {error, list()}.
validate_builderl_config(Config) ->
    Env = proplists:get_value("environment", Config, []),
    case proplist_is_strings(Env) of
        true -> ok;
        false -> {error, "Environment variables must be strings"}
    end.

-spec proplist_is_strings(list()) -> true | false.
proplist_is_strings([]) ->
    true;
proplist_is_strings([{_Key, Value}| L]) when is_list(Value) ->
    proplist_is_strings(L);
proplist_is_strings(_)  ->
    false.
