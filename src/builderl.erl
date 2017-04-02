-module(builderl).

-export([build/2,
         run/0]).


build(GitRepo, Opts) ->
    Time = erlang:monotonic_time(second),
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
    {ok, [BuildConfig]} = yaml:load_file(BuilderlFile),
    io:format("File ~p~n", [BuildConfig]),
    Stages = maps:get(<<"stages">>, BuildConfig),
    execute_stages(Stages, CWD),
    ok.

execute_stages([Stage|Stages], Dir) ->
    Steps = maps:get(<<"steps">>, Stage),
    execute_steps(Steps, Dir),
    execute_stages(Stages, Dir),
    ok;
execute_stages([], _Dir) ->
    ok.


execute_steps([Step|Steps], Dir) ->
    io:format("Step: ~p~n", [Step]),
    SStep = binary:bin_to_list(Step),
    exec:run(SStep, [{stdout, print}, {cd, Dir}]),
    execute_steps(Steps, Dir),
    ok;
execute_steps([], _Dir) ->
    ok.


checkout_ref(Path, Ref) ->
    git:checkout(Path, Ref).

run() ->
    build("git@github.com:philipcristiano/AWSMF-Data.git", [{ref, "package"}, {commit_ish, "package"}]).
