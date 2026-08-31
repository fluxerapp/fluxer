%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_manager_shard).
-typing([eqwalizer]).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PID_CACHE_TABLE, presence_pid_cache).
-define(CACHE_TTL_MS, 300000).

-type user_id() :: integer().
-type presence_ref() :: {pid(), reference()}.
-type presences() :: #{user_id() => presence_ref()}.
-type event_type() :: atom() | binary().

-type start_or_lookup_request() :: map().

-type state() :: #{presences := presences(), _ => _}.

-spec start_link(non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(ShardIndex) ->
    normalize_start_link(gen_server:start_link(?MODULE, #{shard_index => ShardIndex}, [])).

-spec init(map()) -> {ok, state(), hibernate}.
init(Args) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 10),
    ShardIndex = maps:get(shard_index, Args, 0),
    {ok, #{presences => #{}, shard_index => ShardIndex}, hibernate}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({lookup, UserId}, _From, State) when is_integer(UserId) ->
    do_lookup(UserId, State);
handle_call({dispatch, UserId, Event, Data}, _From, State) when
    is_integer(UserId), is_atom(Event);
    is_integer(UserId), is_binary(Event)
->
    do_dispatch(UserId, Event, Data, State);
handle_call({start_or_lookup, Request}, _From, State) when is_map(Request) ->
    do_start_or_lookup(Request, State);
handle_call({terminate_all_sessions, UserId}, _From, State) when is_integer(UserId) ->
    {Result, NewState} = terminate_sessions_for_user(UserId, State),
    {reply, Result, NewState};
handle_call(Request, From, State) ->
    handle_call_info(Request, From, State).

-spec handle_call_info(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call_info(get_local_count, _From, State) ->
    {reply, {ok, get_presence_count(State)}, State};
handle_call_info(get_global_count, _From, State) ->
    {reply, {ok, get_presence_count(State)}, State};
handle_call_info(get_all_presences, _From, State) ->
    {reply, {ok, maps:get(presences, State)}, State};
handle_call_info(_Unknown, _From, State) ->
    {reply, ok, State}.

-spec get_presence_count(state()) -> non_neg_integer().
get_presence_count(State) ->
    process_registry:get_count(maps:get(presences, State)).

-spec shard_index(state()) -> non_neg_integer().
shard_index(State) ->
    case maps:get(shard_index, State, 0) of
        V when is_integer(V), V >= 0 -> V;
        _ -> 0
    end.

-spec do_dispatch(user_id(), event_type(), term(), state()) ->
    {reply, ok | {error, not_found}, state()}.
do_dispatch(UserId, Event, Data, State) ->
    case lookup_presence(UserId, State) of
        {ok, PresencePid, NewState} ->
            gen_server:cast(PresencePid, {dispatch, Event, Data}),
            {reply, ok, NewState};
        {error, not_found, NewState} ->
            {reply, {error, not_found}, NewState}
    end.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Unknown, State) ->
    {noreply, State}.

-spec handle_info(Info, State) -> {noreply, state()} when
    Info :: {'DOWN', reference(), process, pid(), term()} | term(),
    State :: state().
handle_info({'DOWN', Ref, process, Pid, _Reason}, State) when is_reference(Ref), is_pid(Pid) ->
    demonitor(Ref, [flush]),
    presence_manager_cache:clean_by_pid(Pid),
    Presences = maps:get(presences, State),
    ShardIndex = shard_index(State),
    RemovedUserIds = find_user_ids_by_pid(Pid, Presences),
    demonitor_refs_for_pid(Pid, Presences),
    NewPresences = sanitize_presences(
        process_registry:cleanup_on_down(presence, Pid, Presences)
    ),
    lists:foreach(
        fun(UserId) -> presence_manager:untrack_shard_user(ShardIndex, UserId) end,
        RemovedUserIds
    ),
    {noreply, State#{presences := NewPresences}};
handle_info(_Unknown, State) ->
    {noreply, State}.

-spec terminate(Reason, State) -> ok when
    Reason :: term(),
    State :: state().
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec do_lookup(user_id(), state()) -> {reply, {ok, pid()} | {error, not_found}, state()}.
do_lookup(UserId, State) ->
    case lookup_presence(UserId, State) of
        {ok, Pid, NewState} ->
            {reply, {ok, Pid}, NewState};
        {error, not_found, NewState} ->
            {reply, {error, not_found}, NewState}
    end.

-spec do_start_or_lookup(start_or_lookup_request(), state()) ->
    {reply, {ok, pid()} | {error, term()}, state()}.
do_start_or_lookup(Request, State) ->
    Presences = maps:get(presences, State),
    #{user_id := UserId} = Request,
    case maps:get(UserId, Presences, undefined) of
        {Pid, Ref} ->
            handle_existing_presence(Pid, Ref, UserId, Request, Presences, State);
        undefined ->
            handle_missing_presence(UserId, Request, Presences, State)
    end.

-spec handle_existing_presence(
    pid(), reference(), user_id(), start_or_lookup_request(), presences(), state()
) ->
    {reply, {ok, pid()} | {error, term()}, state()}.
handle_existing_presence(Pid, Ref, UserId, Request, Presences, State) ->
    case process_liveness:is_alive(Pid) of
        true ->
            {reply, {ok, Pid}, State};
        false ->
            demonitor(Ref, [flush]),
            PresenceName = process_registry:build_process_key(presence, UserId),
            process_registry:safe_unregister(PresenceName),
            do_start_or_lookup(Request, State#{presences := maps:remove(UserId, Presences)})
    end.

-spec handle_missing_presence(user_id(), start_or_lookup_request(), presences(), state()) ->
    {reply, {ok, pid()} | {error, term()}, state()}.
handle_missing_presence(UserId, Request, Presences, State) ->
    PresenceName = process_registry:build_process_key(presence, UserId),
    case process_registry:registry_whereis(PresenceName) of
        undefined ->
            start_new_presence(UserId, PresenceName, Request, Presences, State);
        _ExistingPid ->
            adopt_existing_presence(UserId, PresenceName, Presences, State)
    end.

-spec start_new_presence(
    user_id(), process_registry:process_key(), start_or_lookup_request(), presences(), state()
) ->
    {reply, {ok, pid()} | {error, term()}, state()}.
start_new_presence(UserId, PresenceName, Request, Presences, State) ->
    PresenceData = build_presence_data(Request),
    case presence:start_link(PresenceData) of
        {ok, Pid} ->
            register_new_presence(UserId, PresenceName, Pid, Presences, State);
        Error ->
            {reply, Error, State}
    end.

-spec build_presence_data(start_or_lookup_request()) -> map().
build_presence_data(Request) ->
    #{user_id := UserId, user_data := UserData, guild_ids := GuildIds, status := Status} =
        Request,
    #{
        user_id => UserId,
        user_data => UserData,
        guild_ids => GuildIds,
        status => Status,
        friend_ids => maps:get(friend_ids, Request, []),
        group_dm_recipients => maps:get(group_dm_recipients, Request, #{}),
        custom_status => maps:get(custom_status, Request, null)
    }.

-spec register_new_presence(
    user_id(), process_registry:process_key(), pid(), presences(), state()
) ->
    {reply, {ok, pid()} | {error, term()}, state()}.
register_new_presence(UserId, PresenceName, Pid, Presences, State) ->
    case process_registry:register_and_monitor(PresenceName, Pid, Presences) of
        {ok, RegisteredPid, Ref, NewPresences0} ->
            CleanPresences = sanitize_presences(maps:remove(PresenceName, NewPresences0)),
            NewPresences = CleanPresences#{UserId => {RegisteredPid, Ref}},
            update_cache(UserId, RegisteredPid),
            presence_manager:track_shard_user(shard_index(State), UserId),
            {reply, {ok, RegisteredPid}, State#{presences := NewPresences}};
        {error, registration_race_condition} ->
            {reply, {error, registration_failed}, State};
        {error, _Reason} = Error ->
            {reply, Error, State}
    end.

-spec adopt_existing_presence(user_id(), process_registry:process_key(), presences(), state()) ->
    {reply, {ok, pid()} | {error, process_disappeared}, state()}.
adopt_existing_presence(UserId, PresenceName, Presences, State) ->
    case process_registry:lookup_or_monitor(PresenceName, UserId, Presences) of
        {ok, Pid, _Ref, NewPresences} ->
            FinalPresences = sanitize_presences(NewPresences),
            update_cache(UserId, Pid),
            {reply, {ok, Pid}, State#{presences := FinalPresences}};
        {error, not_found} ->
            {reply, {error, process_disappeared}, State}
    end.

-spec lookup_presence(user_id(), state()) -> {ok, pid(), state()} | {error, not_found, state()}.
lookup_presence(UserId, State) ->
    Presences = maps:get(presences, State),
    case maps:get(UserId, Presences, undefined) of
        {Pid, Ref} ->
            lookup_known_presence(UserId, Pid, Ref, State);
        undefined ->
            lookup_via_registry(UserId, State)
    end.

-spec lookup_known_presence(user_id(), pid(), reference(), state()) ->
    {ok, pid(), state()} | {error, not_found, state()}.
lookup_known_presence(UserId, Pid, Ref, State) ->
    case process_liveness:is_alive(Pid) of
        true ->
            {ok, Pid, State};
        false ->
            demonitor(Ref, [flush]),
            PresenceName = process_registry:build_process_key(presence, UserId),
            process_registry:safe_unregister(PresenceName),
            Presences = maps:get(presences, State),
            lookup_presence(UserId, State#{presences := maps:remove(UserId, Presences)})
    end.

-spec lookup_via_registry(user_id(), state()) ->
    {ok, pid(), state()} | {error, not_found, state()}.
lookup_via_registry(UserId, State) ->
    Presences = maps:get(presences, State),
    PresenceName = process_registry:build_process_key(presence, UserId),
    case process_registry:lookup_or_monitor(PresenceName, UserId, Presences) of
        {ok, Pid, Ref, NewPresences0} ->
            CleanPresences = sanitize_presences(maps:remove(PresenceName, NewPresences0)),
            FinalPresences = CleanPresences#{UserId => {Pid, Ref}},
            update_cache(UserId, Pid),
            {ok, Pid, State#{presences := FinalPresences}};
        {error, not_found} ->
            {error, not_found, State}
    end.

-spec terminate_sessions_for_user(user_id(), state()) -> {ok, state()}.
terminate_sessions_for_user(UserId, State) ->
    Presences = maps:get(presences, State),
    case maps:get(UserId, Presences, undefined) of
        {Pid, Ref} ->
            terminate_known_presence(UserId, Pid, Ref, State);
        undefined ->
            terminate_via_registry(UserId, State)
    end.

-spec terminate_known_presence(user_id(), pid(), reference(), state()) -> {ok, state()}.
terminate_known_presence(UserId, Pid, Ref, State) ->
    case process_liveness:is_alive(Pid) of
        true ->
            gen_server:cast(Pid, {terminate_all_sessions}),
            {ok, State};
        false ->
            demonitor(Ref, [flush]),
            PresenceName = process_registry:build_process_key(presence, UserId),
            process_registry:safe_unregister(PresenceName),
            Presences = maps:get(presences, State),
            terminate_sessions_for_user(
                UserId, State#{presences := maps:remove(UserId, Presences)}
            )
    end.

-spec terminate_via_registry(user_id(), state()) -> {ok, state()}.
terminate_via_registry(UserId, State) ->
    Presences = maps:get(presences, State),
    PresenceName = process_registry:build_process_key(presence, UserId),
    case process_registry:lookup_or_monitor(PresenceName, UserId, Presences) of
        {ok, Pid, Ref, NewPresences0} ->
            CleanPresences = sanitize_presences(maps:remove(PresenceName, NewPresences0)),
            FinalPresences = CleanPresences#{UserId => {Pid, Ref}},
            update_cache(UserId, Pid),
            gen_server:cast(Pid, {terminate_all_sessions}),
            {ok, State#{presences := FinalPresences}};
        {error, not_found} ->
            {ok, State}
    end.

-spec update_cache(user_id(), pid()) -> ok.
update_cache(UserId, Pid) ->
    Timestamp = erlang:monotonic_time(millisecond),
    try
        ets:insert(?PID_CACHE_TABLE, {UserId, Pid, Timestamp}),
        ok
    catch
        _:_ -> ok
    end.

-spec demonitor_refs_for_pid(pid(), presences()) -> ok.
demonitor_refs_for_pid(Pid, Presences) ->
    maps:foreach(
        fun
            (_UserId, {P, Ref}) when P =:= Pid, is_reference(Ref) -> demonitor(Ref, [flush]);
            (_Key, _Value) -> ok
        end,
        Presences
    ).

-spec find_user_ids_by_pid(pid(), presences()) -> [user_id()].
find_user_ids_by_pid(Pid, Presences) ->
    maps:fold(
        fun
            (UserId, {P, _Ref}, Acc) when is_integer(UserId), P =:= Pid -> [UserId | Acc];
            (_Key, _Value, Acc) -> Acc
        end,
        [],
        Presences
    ).

-spec sanitize_presences(process_registry:process_map() | map()) -> presences().
sanitize_presences(Presences) ->
    maps:fold(
        fun
            (UserId, {Pid, Ref}, Acc) when is_integer(UserId), is_pid(Pid), is_reference(Ref) ->
                Acc#{UserId => {Pid, Ref}};
            (_Key, _Value, Acc) ->
                Acc
        end,
        #{},
        Presences
    ).

-spec normalize_start_link(gen_server:start_ret()) -> {ok, pid()} | {error, term()}.
normalize_start_link({ok, Pid}) ->
    {ok, Pid};
normalize_start_link({error, Reason}) ->
    {error, Reason};
normalize_start_link(ignore) ->
    {error, ignore}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

init_returns_empty_presences_test() ->
    {ok, State, hibernate} = init(#{shard_index => 0}),
    ?assertEqual(#{}, maps:get(presences, State)),
    ?assertEqual(0, maps:get(shard_index, State)).

init_stores_shard_index_test() ->
    {ok, State, hibernate} = init(#{shard_index => 7}),
    ?assertEqual(7, maps:get(shard_index, State)).

update_cache_handles_missing_table_test() ->
    ?assertEqual(ok, update_cache(999, self())).

find_user_ids_by_pid_test() ->
    Pid1 = self(),
    Pid2 = spawn(fun test_wait_for_stop/0),
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Ref3 = make_ref(),
    Presences = #{
        100 => {Pid1, Ref1},
        200 => {Pid2, Ref2},
        300 => {Pid1, Ref3}
    },
    Found = lists:sort(find_user_ids_by_pid(Pid1, Presences)),
    ?assertEqual([100, 300], Found),
    ?assertEqual([200], find_user_ids_by_pid(Pid2, Presences)),
    Pid2 ! stop.

process_down_clears_the_pid_cache_test() ->
    Pid = spawn(fun test_wait_for_stop/0),
    UserId = 987654321,
    Ref = make_ref(),
    ok = presence_manager_cache:put_if_local(UserId, Pid),
    ?assertEqual({hit, Pid}, presence_manager_cache:lookup(UserId)),
    {noreply, _NewState} = handle_info(
        {'DOWN', Ref, process, Pid, normal},
        #{presences => #{UserId => {Pid, Ref}}, shard_index => 0}
    ),
    ?assertEqual([], ets:lookup(presence_pid_cache, UserId)),
    Pid ! stop.

find_user_ids_by_pid_empty_test() ->
    ?assertEqual([], find_user_ids_by_pid(self(), #{})).

-spec test_wait_for_stop() -> ok.
test_wait_for_stop() ->
    receive
        stop -> ok
    after 5000 -> ok
    end.
-endif.
