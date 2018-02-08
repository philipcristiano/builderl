-module(builderl_build_registry).
-behaviour(gen_server).
-compile({parse_transform, lager_transform}).
-include_lib("stdlib/include/ms_transform.hrl").

%% API.
-export([start_link/1]).

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
         set_build_state/2]).

-record(state, {}).

-define(TABLE, build_registry).
-record(builderl_build_record, {id, project, ref, committish, state, time}).

%% API.

-spec start_link(atom()) -> {ok, pid()}.
start_link(FileBase) ->
	  gen_server:start_link({local, ?MODULE}, ?MODULE, [FileBase], []).

-spec create(nonempty_list(), nonempty_list(), nonempty_list()) -> {ok, nonempty_list()}.
create(Project, Ref, Commitish) ->
    gen_server:call(?MODULE, {create, {Project, Ref, Commitish}}).

-spec get_builds(nonempty_list()) -> {ok, nonempty_list()}.
get_builds(Project) when is_list(Project) ->
    gen_server:call(?MODULE, {get_builds, Project}).

-spec get_build(nonempty_list(), nonempty_list()) -> {ok, nonempty_list()}.
get_build(Project, ID) when is_list(ID) ->
    try uuid:is_valid(ID) of
      true -> gen_server:call(?MODULE, {get_build, Project, ID});
      _ -> {error, invalid_uuid}
    catch
      error:badarg -> {error, invalid_uuid}
    end.

-spec set_build_state(nonempty_list(), atom()) -> ok.
set_build_state(ID, State) ->
    gen_server:call(?MODULE, {set_build_state, ID, State}).

%% gen_server.

init([FileBase]) ->
    Filename = filename:join([FileBase, "dets", "build_registry_set"]),
    ok = lager:info("Build registry file ~p", [Filename]),
    ok = filelib:ensure_dir(Filename),
    {ok, _Name} = dets:open_file(?TABLE, [{type, set},
                                          {keypos, #builderl_build_record.id},
                                          {file, Filename}]),
	{ok, #state{}}.

handle_call({create, {Project, Ref, Commitish}}, _From, State) ->
    ID = uuid:uuid4(),
    SID = uuid:to_string(simple, ID),
    ok = lager:info("Generated ID ~p", [SID]),
    Time = os:system_time(seconds),
    Record = #builderl_build_record{id=ID,
                                    project=Project,
                                    ref=Ref,
                                    committish=Commitish,
                                    state=created,
                                    time=Time},
    ok = dets:insert(?TABLE, Record),
    ok = dets:sync(?TABLE),
	  {reply, {ok, SID}, State};

handle_call({get_builds, Project}, _From, State) ->
    Match = ets:fun2ms(
            fun(BBR=#builderl_build_record{project=BBRProject}) when BBRProject =:= Project ->
                BBR
            end),

    Objects = dets:select(?TABLE, Match),
    Builds = builds_to_proplist(Objects),
	  {reply, {ok, Builds}, State};

handle_call({get_build, _Project, ID}, _From, State) ->
    BID = uuid:to_binary(ID),
    ok = lager:debug("ID ~p~n", [{BID, ID}]),
    Objects = dets:lookup(?TABLE, BID),
    [Builds] = builds_to_proplist(Objects),
	  {reply, {ok, Builds}, State};

handle_call({set_build_state, ID, BRState}, _From, State) ->
    BID = uuid:to_binary(ID),
    ok = lager:debug("ID ~p~n", [BID]),
    [BR0] = dets:lookup(?TABLE, BID),
    ok = lager:debug("Item ~p~n", [BR0]),
    BR1 = BR0#builderl_build_record{state=BRState},
    ok = lager:debug("Save item ~p", [BR1]),
    ok = dets:insert(?TABLE, BR1),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
	  {reply, ignored, State}.

builds_to_proplist([{Project, ID, Ref, Commitish, Time}|Rest]) ->
    [[{id, uuid:to_string(simple, ID)},
     {project, Project},
     {ref, Ref},
     {commitish, Commitish},
     {time, Time}]| builds_to_proplist(Rest)];
builds_to_proplist([#builderl_build_record{project=P, id=ID, ref=R, committish=C, time=T, state=S}|Rest]) ->
    [[{id, uuid:to_string(simple, ID)},
     {project, P},
     {ref, R},
     {commitish, C},
     {state, S},
     {time, T}]| builds_to_proplist(Rest)];

builds_to_proplist([]) ->
    [].

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
