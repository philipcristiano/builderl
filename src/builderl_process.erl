-module(builderl_process).

-export([run/3]).

run(Command, Dir, Env) ->
    [Cmd|Args] = string:tokens(Command, " "),
    Exec = os:find_executable(Cmd),
    io:format("Command ~p~n", [Exec]),
    Port = erlang:open_port({spawn_executable, Exec},
        [stream, stderr_to_stdout, binary, exit_status,
         {args, Args}, {cd, Dir}, {env, Env}]),
    Status = loop(Port),
    io:format("Status ~p~n",[Status]),
    Status.


loop(Port) ->
    receive
        {Port, {data, {eol, Line}}} -> io:format("eol ~p~n", [Line]),
                                       loop(Port);
        {Port, {data, {noeol, Line}}} -> io:format("noeol ~p~n", [Line]),
                                         loop(Port);
        {Port, {data, Data}} -> io:format("data ~p~n", [Data]),
                                loop(Port);
        {Port, {exit_status, Status}} -> Status
    end.
