-module(builderl_build_registry).
-behaviour(gen_server).
-compile({parse_transform, lager_transform}).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([create/3,
         get_build/2,
         get_builds/1,
         get_projects/0]).

-record(state, {
}).

-define(TABLE, build_registry).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create(Project, Ref, Commitish) ->
    gen_server:call(?MODULE, {create, {Project, Ref, Commitish}}).

get_builds(Project) when is_list(Project) ->
    gen_server:call(?MODULE, {get_builds, Project}).

get_build(Project, ID) when is_list(ID)->
    gen_server:call(?MODULE, {get_build, Project, ID}).

get_projects() ->
    gen_server:call(?MODULE, get_projects).

%% gen_server.

init([]) ->
    FileBase = application:get_env(builderl, root, "tmp"),
    Filename = filename:join([FileBase, "dets", "build_registry"]),
    ok = lager:info("Build registry file ~p", [Filename]),
    ok = filelib:ensure_dir(Filename),
    {ok, _Name} = dets:open_file(?TABLE, [{type, bag},
                                          {file, Filename}]),
	{ok, #state{}}.

handle_call({create, {Project, Ref, Commitish}}, _From, State) ->
    ID = uuid:uuid4(),
    SID = uuid:to_string(simple, ID),
    lager:info("Generated ID ~p", [SID]),
    Time = erlang:monotonic_time(seconds),
    ok = dets:insert(?TABLE, {Project, ID, Ref, Commitish, Time}),
    dets:sync(?TABLE),
	{reply, {ok, SID}, State};

handle_call({get_builds, Project}, _From, State) ->
    Objects = dets:lookup(?TABLE, Project),
    IDs = get_ids(Objects),
	{reply, {ok, IDs}, State};

handle_call({get_build, Project, ID}, _From, State) ->
    BID = uuid:to_binary(ID),
    io:format("ID ~p", [BID]),
    Objects = dets:match(?TABLE, {Project, '_', '$1', '$2', '_'}),
	{reply, {ok, Objects}, State};

handle_call(get_projects, _From, State) ->
    Projects = keys(?TABLE),
    {reply, {ok, Projects}, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

get_ids([{_Project, ID, _Ref, _Commitish, _Time}|T]) ->
    [uuid:to_string(simple, ID) | get_ids(T)];
get_ids([]) ->
    [].

% Get keys of a table
keys(TableName) ->
    FirstKey = dets:first(TableName),
        keys(TableName, FirstKey, [FirstKey]).

keys(_TableName, '$end_of_table', ['$end_of_table'|Acc]) ->
    Acc;
keys(TableName, CurrentKey, Acc) ->
    NextKey = dets:next(TableName, CurrentKey),
    keys(TableName, NextKey, [NextKey|Acc]).

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
