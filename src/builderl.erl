-module(builderl).

-export([build/2,
         run/0]).


build(GitRepo, Opts) ->
    Path = "tmp",
    BuilderlFile = "tmp" ++ "/builderl.yml",
    IsDir = filelib:is_dir(Path),
    clone_if_needed(IsDir, GitRepo, Path),

    Ref = proplists:get_value(ref, Opts, "master"),
    io:format("Ref ~p~n", [Ref]),
    checkout_ref(Path, Ref),
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

build_project(_CWD, BuilderlFile) ->
    BuildConfig = yaml:load_file(BuilderlFile),
    io:format("File ~p~n", [BuildConfig]),
    ok.

checkout_ref(Path, Ref) ->
    git:checkout(Path, Ref).

run() ->
    build("git@github.com:philipcristiano/AWSMF-Data.git", [{ref, "package"}]).
