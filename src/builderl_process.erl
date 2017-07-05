-module(builderl_process).
-compile({parse_transform, lager_transform}).

-export([run/4, save_msg/2]).

run(Command, Dir, Env, console) ->
    run(Command, Dir, Env, fun output_to_console/1);
run(Command, Dir, Env, {file, Path}) ->
    Output = write_output_to_file(Path),
    run(Command, Dir, Env, Output);
run(Command, Dir, Env, Func) ->
    [Cmd|Args] = string:tokens(Command, " "),
    % Use the Env PATH to find the executable!
    PATH = proplists:get_value("PATH", Env),
    lager:debug("Finding command with PATH of ~p", [PATH]),
    Exec = os:find_executable(Cmd, PATH),
    lager:debug("Command ~p", [Exec]),
    Port = erlang:open_port({spawn_executable, Exec},
        [stream, stderr_to_stdout, binary, exit_status,
         {args, Args}, {cd, Dir}, {env, Env}]),
    lager:debug("Port started, entering IO loop"),
    Status = loop(Port, Func),
    lager:debug("Status ~p",[Status]),
    Status.

loop(Port, Fun) ->
    receive
        {Port, {exit_status, Status}} -> Status;
        {Port, Msg} -> Fun(Msg),
                       loop(Port, Fun);
        Msg -> lager:info("Unhandled loop message ~p", [Msg])
    end.

save_msg({file, Path}, Msg) ->
    ok = filelib:ensure_dir(Path),
    lager:debug("Writing output to file ~p", [Path]),
    {ok, IoD} = file:open(Path, [append]),
    file:write(IoD, ["Builderl message: ", Msg, "\n"]).

output_to_console({data, {eol, Data}}) ->
    lager:debug("eol ~p", [Data]);
output_to_console({data, {noeol, Data}}) ->
    lager:debug("noeol ~p", [Data]);
output_to_console({data, Data}) ->
    lager:debug("boop data ~p", [Data]).

write_output_to_file(Path) ->
    ok = filelib:ensure_dir(Path),
    lager:debug("Writing output to file ~p", [Path]),
    {ok, IoD} = file:open(Path, [append]),

    Output = fun({data, {eol, Data}}) ->
                file:write(IoD, ["eol", Data, "\n"]);
             ({data, {noeol, Data}}) ->
                file:write(IoD, ["noeol", Data]);
             ({data, Data}) ->
                file:write(IoD, Data)
    end,
    Output.
