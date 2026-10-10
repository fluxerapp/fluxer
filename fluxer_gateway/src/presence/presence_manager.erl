%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_manager).
-typing([eqwalizer]).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([
    start_link/0,
    start_or_lookup/1,
    dispatch_to_user/3,
    handoff_for_drain/0,
    call_via_manager_local/2,
    track_shard_user/2,
    untrack_shard_user/2,
    ensure_shard_user_table/0,
    delete_shard_user_table/0,
    clear_shard_user_ids/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SHARD_USER_TABLE, presence_shard_user_ids).

-type user_id() :: integer().
-type event_type() :: atom() | binary().
-type shard() :: #{pid := pid(), ref := reference()}.
-type state() :: #{
    shards := #{non_neg_integer() => shard()}, shard_count := pos_integer(), _ => _
}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    normalize_start_link(gen_server:start_link({local, ?MODULE}, ?MODULE, [], [])).

-spec start_or_lookup(map()) -> {ok, pid()} | {error, term()}.
start_or_lookup(Request) when is_map(Request) ->
    case request_user_id(Request) of
        undefined ->
            {error, invalid_user_id};
        UserId ->
            start_or_lookup_for_user(UserId, Request)
    end.

-spec request_user_id(map()) -> user_id() | undefined.
request_user_id(#{user_id := UserId}) when is_integer(UserId) ->
    UserId;
request_user_id(_) ->
    undefined.

-spec start_or_lookup_for_user(user_id(), map()) -> {ok, pid()} | {error, term()}.
start_or_lookup_for_user(UserId, Request) ->
    case
        presence_manager_routing:call_owner_manager(
            UserId, {start_or_lookup, Request}, ?DEFAULT_GEN_SERVER_TIMEOUT
        )
    of
        {ok, Pid} when is_pid(Pid) ->
            presence_manager_cache:put_if_local(UserId, Pid),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, unavailable}
    end.

-spec handoff_for_drain() -> ok.
handoff_for_drain() ->
    gen_server:call(?MODULE, handoff_for_drain, 30000).

-spec dispatch_to_user(user_id(), event_type(), term()) -> ok | {error, not_found}.
dispatch_to_user(UserId, Event, Data) ->
    case presence_manager_cache:lookup(UserId) of
        {hit, Pid} ->
            dispatch_to_pid(Pid, Event, Data);
        miss ->
            dispatch_via_owner(UserId, Event, Data)
    end.

-spec dispatch_to_pid(pid(), event_type(), term()) -> ok.
dispatch_to_pid(Pid, Event, Data) ->
    _ = gen_server:cast(Pid, {dispatch, Event, Data}),
    ok.

-spec dispatch_via_owner(user_id(), event_type(), term()) -> ok | {error, not_found}.
dispatch_via_owner(UserId, Event, Data) ->
    case
        presence_manager_routing:call_owner_manager(
            UserId, {dispatch, UserId, Event, Data}, ?DEFAULT_GEN_SERVER_TIMEOUT
        )
    of
        ok ->
            ok;
        {error, not_found} ->
            {error, not_found};
        _ ->
            {error, not_found}
    end.

-spec init(list()) -> {ok, state(), hibernate}.
init([]) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 10),
    ensure_shard_user_table(),
    presence_manager_shards:ensure_table(),
    presence_manager_cache:ensure_table(),
    {ShardCount, _Source} = presence_manager_shards:determine_count(),
    ShardMap = start_all_shards(ShardCount),
    State = #{shards => ShardMap, shard_count => ShardCount},
    presence_manager_shards:sync_table(State),
    {ok, State, hibernate}.

-spec start_all_shards(pos_integer()) -> #{non_neg_integer() => shard()}.
start_all_shards(ShardCount) ->
    lists:foldl(fun start_shard_acc/2, #{}, lists:seq(0, ShardCount - 1)).

-spec start_shard_acc(non_neg_integer(), #{non_neg_integer() => shard()}) ->
    #{non_neg_integer() => shard()}.
start_shard_acc(Index, Acc) ->
    case presence_manager_shards:start(Index) of
        {ok, Shard} -> Acc#{Index => Shard};
        {error, _Reason} -> Acc
    end.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({lookup, UserId}, _From, State) when is_integer(UserId) ->
    {Reply, NewState} = presence_manager_shards:forward_call(UserId, {lookup, UserId}, State),
    {reply, Reply, NewState};
handle_call({dispatch, UserId, Event, Data}, _From, State) when is_integer(UserId) ->
    {Reply, NewState} = presence_manager_shards:forward_call(
        UserId, {dispatch, UserId, Event, Data}, State
    ),
    {reply, Reply, NewState};
handle_call({start_or_lookup, _} = Request, _From, State) ->
    handle_start_or_lookup_call(Request, State);
handle_call(get_local_count, _From, State) ->
    {Count, NewState} = presence_manager_shards:aggregate_counts(get_local_count, State),
    {reply, {ok, Count}, NewState};
handle_call(get_global_count, _From, State) ->
    {Count, NewState} = presence_manager_shards:aggregate_counts(get_global_count, State),
    {reply, {ok, Count}, NewState};
handle_call(handoff_for_drain, _From, State) ->
    presence_manager_handoff:do(State),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, process, Pid, _Reason}, State) when is_reference(Ref), is_pid(Pid) ->
    presence_manager_cache:clean_by_pid(Pid),
    Shards = maps:get(shards, State),
    {noreply,
        maybe_restart_manager_shard(
            presence_manager_shards:find_by_ref(Ref, Shards), State
        )};
handle_info({'EXIT', Pid, _Reason}, State) when is_pid(Pid) ->
    presence_manager_cache:clean_by_pid(Pid),
    Shards = maps:get(shards, State),
    {noreply,
        maybe_restart_manager_shard(
            presence_manager_shards:find_by_pid(Pid, Shards), State
        )};
handle_info(_Info, State) ->
    {noreply, State}.

-spec maybe_restart_manager_shard({ok, non_neg_integer()} | not_found, state()) -> state().
maybe_restart_manager_shard({ok, Index}, State) ->
    AffectedUserIds = get_shard_user_ids(Index),
    clear_shard_user_ids(Index),
    {_ShardEntry, UpdatedState} = presence_manager_shards:restart(Index, State),
    spawn_presence_rejoin_broadcast(AffectedUserIds),
    UpdatedState;
maybe_restart_manager_shard(not_found, State) ->
    State.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    presence_manager_shards:delete_table(),
    presence_manager_cache:delete_table(),
    delete_shard_user_table(),
    Shards = maps:get(shards, State),
    lists:foreach(
        fun(#{pid := Pid}) -> safe_stop_shard(Pid) end,
        maps:values(Shards)
    ),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec safe_stop_shard(pid()) -> ok.
safe_stop_shard(Pid) when is_pid(Pid) ->
    try gen_server:stop(Pid, shutdown, 5000) of
        _ -> ok
    catch
        error:_ -> ok;
        exit:_ -> ok
    end.

-spec call_via_manager_local(term(), pos_integer()) -> term().
call_via_manager_local(Request, Timeout) ->
    presence_manager_routing:call_via_manager_local(Request, Timeout).

-spec handle_start_or_lookup_call(term(), state()) -> {reply, term(), state()}.
handle_start_or_lookup_call(Request, State) ->
    case extract_user_id(Request) of
        undefined ->
            {reply, {error, invalid_user_id}, State};
        Key ->
            {Reply, NewState} = presence_manager_shards:forward_call(Key, Request, State),
            {reply, Reply, NewState}
    end.

-spec extract_user_id(term()) -> user_id() | undefined.
extract_user_id({start_or_lookup, #{user_id := UserId}}) when is_integer(UserId) -> UserId;
extract_user_id(_) -> undefined.

-spec normalize_start_link(gen_server:start_ret()) -> {ok, pid()} | {error, term()}.
normalize_start_link({ok, Pid}) ->
    {ok, Pid};
normalize_start_link({error, Reason}) ->
    {error, Reason};
normalize_start_link(ignore) ->
    {error, ignore}.

-spec track_shard_user(non_neg_integer(), user_id()) -> ok.
track_shard_user(ShardIndex, UserId) ->
    try
        ets:insert(?SHARD_USER_TABLE, {{ShardIndex, UserId}}),
        ok
    catch
        error:badarg -> ok
    end.

-spec untrack_shard_user(non_neg_integer(), user_id()) -> ok.
untrack_shard_user(ShardIndex, UserId) ->
    try
        ets:delete(?SHARD_USER_TABLE, {ShardIndex, UserId}),
        ok
    catch
        error:badarg -> ok
    end.

-spec get_shard_user_ids(non_neg_integer()) -> [user_id()].
get_shard_user_ids(ShardIndex) ->
    try
        ets:select(?SHARD_USER_TABLE, [{{{ShardIndex, '$1'}}, [], ['$1']}])
    catch
        error:badarg -> []
    end.

-spec clear_shard_user_ids(non_neg_integer()) -> ok.
clear_shard_user_ids(ShardIndex) ->
    try
        ets:select_delete(?SHARD_USER_TABLE, [{{{ShardIndex, '_'}}, [], [true]}]),
        ok
    catch
        error:badarg -> ok
    end.

-spec ensure_shard_user_table() -> ok.
ensure_shard_user_table() ->
    case ets:whereis(?SHARD_USER_TABLE) of
        undefined ->
            _ = ets:new(?SHARD_USER_TABLE, [
                named_table, public, set, {write_concurrency, true}
            ]),
            ok;
        _ ->
            ok
    end.

-spec delete_shard_user_table() -> ok.
delete_shard_user_table() ->
    try ets:delete(?SHARD_USER_TABLE) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec spawn_presence_rejoin_broadcast([user_id()]) -> ok.
spawn_presence_rejoin_broadcast([]) ->
    ok;
spawn_presence_rejoin_broadcast(UserIds) ->
    spawn(fun() -> do_presence_rejoin_broadcast(UserIds) end),
    ok.

-spec do_presence_rejoin_broadcast([user_id()]) -> ok.
do_presence_rejoin_broadcast(UserIds) ->
    invalidate_cache_for_users(UserIds),
    trigger_presence_rejoin(UserIds),
    ok.

-spec invalidate_cache_for_users([user_id()]) -> ok.
invalidate_cache_for_users(UserIds) ->
    lists:foreach(fun presence_manager_cache:invalidate/1, UserIds),
    ok.

-spec trigger_presence_rejoin([user_id()]) -> ok.
trigger_presence_rejoin(UserIds) ->
    lists:foreach(fun request_presence_rejoin/1, UserIds),
    ok.

-spec request_presence_rejoin(user_id()) -> ok.
request_presence_rejoin(UserId) ->
    PresenceKey = process_registry:build_process_key(presence, UserId),
    case process_registry:registry_whereis(PresenceKey) of
        Pid when is_pid(Pid) ->
            safe_cast_presence_rejoin(Pid);
        _ ->
            ok
    end.

-spec safe_cast_presence_rejoin(pid()) -> ok.
safe_cast_presence_rejoin(Pid) ->
    try gen_server:cast(Pid, presence_rejoin) of
        _ -> ok
    catch
        _:_ -> ok
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

safe_cast_presence_rejoin_delivers_cast_test() ->
    Parent = self(),
    Pid = spawn(fun() -> rejoin_cast_receiver(Parent) end),
    ?assertEqual(ok, safe_cast_presence_rejoin(Pid)),
    receive
        got_rejoin -> ok;
        rejoin_timeout -> ?assert(false, rejoin_cast_not_delivered)
    after 1500 -> ?assert(false, no_response)
    end.

rejoin_cast_receiver(Parent) ->
    receive
        {'$gen_cast', presence_rejoin} -> Parent ! got_rejoin
    after 1000 -> Parent ! rejoin_timeout
    end.

safe_cast_presence_rejoin_tolerates_dead_pid_test() ->
    Pid = spawn(fun() -> ok end),
    ok = gateway_retry_timer:wait(50),
    ?assertEqual(ok, safe_cast_presence_rejoin(Pid)).

-endif.
