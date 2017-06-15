-module(builderl_process).
-compile({parse_transform, lager_transform}).

-export([run/4]).

run(Command, Dir, Env, console) ->
    run(Command, Dir, Env, fun output_to_console/1);
run(Command, Dir, Env, {file, Path}) ->
    Output = create_output_to_newfile(Path),
    run(Command, Dir, Env, Output);
run(Command, Dir, Env, Func) ->
    [Cmd|Args] = string:tokens(Command, " "),
    % Use the Env PATH to find the executable!
    PATH = proplists:get_value("PATH", Env),
    io:format("Finding command with PATH of ~p~n", [PATH]),
    Exec = os:find_executable(Cmd, PATH),
    io:format("Command ~p~n", [Exec]),
    Port = erlang:open_port({spawn_executable, Exec},
        [stream, stderr_to_stdout, binary, exit_status,
         {args, Args}, {cd, Dir}, {env, Env}]),
    Status = loop(Port, Func),
    io:format("Status ~p~n",[Status]),
    Status.

loop(Port, Fun) ->
    receive
        {Port, {exit_status, Status}} -> Status;
        {Port, Msg} -> Fun(Msg),
                       loop(Port, Fun)
    end.


output_to_console({data, {eol, Data}}) ->
    io:format("eol ~p~n", [Data]);
output_to_console({data, {noeol, Data}}) ->
    io:format("noeol ~p", [Data]);
output_to_console({data, Data}) ->
    io:format("boop data ~p~n", [Data]).

create_output_to_newfile(Path) ->
    ok = filelib:ensure_dir(Path),
    lager:debug("Creating output to newfile ~p", [Path]),
    {ok, IoD} = file:open(Path, [append]),

    Output = fun({data, {eol, Data}}) ->
                file:write(IoD, ["eol", Data, "\n"]);
             ({data, {noeol, Data}}) ->
                file:write(IoD, ["noeol", Data]);
             ({data, Data}) ->
                file:write(IoD, Data)
    end,
    Output.
