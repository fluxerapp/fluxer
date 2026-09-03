%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild).
-feature(maybe_expr, enable).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/1, update_counts/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(HIBERNATE_TIMEOUT, 60000).
-define(VOICE_MEMBERS_TABLE_WARNED, {?MODULE, voice_members_table_unavailable}).

-type guild_state() :: map().
-type call_reply() ::
    {reply, term(), guild_state()}
    | {noreply, guild_state()}
    | {stop, term(), term(), guild_state()}.
-type cast_reply() :: {noreply, guild_state()}.
-type info_reply() ::
    {noreply, guild_state()}
    | {noreply, guild_state(), timeout() | hibernate}
    | {stop, term(), guild_state()}.

-spec start_link(map()) -> gen_server:start_ret().
start_link(GuildState) -> gen_server:start_link(?MODULE, GuildState, []).

-spec update_counts(guild_state()) -> guild_state().
update_counts(State) -> guild_maintenance:update_counts(State).

-spec init(map()) -> {ok, guild_state(), timeout()}.
init(GuildState) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 10),
    State0 = guild_init:init_base_state(GuildState),
    State1 = guild_init:init_member_list(State0),
    State2 = guild_init:init_counts(State1),
    State3 = guild_init:init_caches_and_timers(State2),
    State4 = guild_init:init_voice_server(State3),
    erlang:garbage_collect(),
    {ok, State4, ?HIBERNATE_TIMEOUT}.

-spec handle_call(term(), gen_server:from(), guild_state()) -> call_reply().
handle_call({session_connect, Request}, {CallerPid, _}, State) ->
    handle_session_connect_call(Request, CallerPid, State);
handle_call(export_handoff_state, _From, State) ->
    {reply, {ok, guild_handoff:export_handoff_state(State)}, State};
handle_call({get_cached_voice_state_by_connection, ConnectionId}, _From, State) ->
    handle_cached_voice_state_call(ConnectionId, State);
handle_call({get_guild_id}, _From, State) ->
    {reply, maps:get(id, State, undefined), State};
handle_call({get_voice_guild_state}, _From, State) ->
    {reply, voice_guild_state(State), State};
handle_call({dispatch, Request}, _From, State) ->
    handle_dispatch_call(Request, State);
handle_call({reload, NewData}, _From, State) ->
    handle_reload_call(NewData, State);
handle_call(get_voice_server_pid, _From, State) ->
    guild_voice_lifecycle:reply_voice_server_pid(State);
handle_call({terminate}, _From, State) ->
    {stop, normal, ok, State};
handle_call(Msg, From, State) when is_tuple(Msg) ->
    route_call(element(1, Msg), Msg, From, State);
handle_call(_, _From, State) ->
    {reply, ok, State}.

-spec route_call(atom(), term(), gen_server:from(), guild_state()) -> call_reply().
route_call(Tag, Msg, From, State) ->
    case call_handler(Tag) of
        query -> guild_query_handler:handle_call(Msg, From, State);
        voice -> guild_voice_handler:handle_call(Msg, From, State);
        subscription -> guild_subscription_handler:handle_call(Msg, From, State);
        undefined -> {reply, ok, State}
    end.

-spec call_handler(atom()) -> query | voice | subscription | undefined.
call_handler(Tag) -> query_call_handler(Tag).

-spec query_call_handler(atom()) -> query | voice | subscription | undefined.
query_call_handler(get_counts) -> query;
query_call_handler(get_user_counts) -> query;
query_call_handler(get_channel_member_counts) -> query;
query_call_handler(get_large_guild_metadata) -> query;
query_call_handler(get_users_to_mention_by_roles) -> query;
query_call_handler(get_users_to_mention_by_user_ids) -> query;
query_call_handler(get_all_users_to_mention) -> query;
query_call_handler(resolve_all_mentions) -> query;
query_call_handler(resolve_mention_sources) -> query;
query_call_handler(resolve_mention_sources_page) -> query;
query_call_handler(resolve_channel_mentions) -> query;
query_call_handler(get_members_with_role) -> query;
query_call_handler(check_permission) -> query;
query_call_handler(get_user_permissions) -> query;
query_call_handler(can_manage_roles) -> query;
query_call_handler(can_manage_role) -> query;
query_call_handler(get_guild_data) -> query;
query_call_handler(get_guild_auth_context) -> query;
query_call_handler(get_assignable_roles) -> query;
query_call_handler(get_user_max_role_position) -> query;
query_call_handler(check_target_member) -> query;
query_call_handler(get_viewable_channels) -> query;
query_call_handler(get_guild_member) -> query;
query_call_handler(get_guild_members_batch) -> query;
query_call_handler(Tag) -> query_call_handler_more(Tag).

-spec query_call_handler_more(atom()) -> query | voice | subscription | undefined.
query_call_handler_more(has_member) -> query;
query_call_handler_more(list_guild_members) -> query;
query_call_handler_more(search_guild_members) -> query;
query_call_handler_more(list_guild_members_cursor) -> query;
query_call_handler_more(get_vanity_url_channel) -> query;
query_call_handler_more(get_first_viewable_text_channel) -> query;
query_call_handler_more(get_category_channel_count) -> query;
query_call_handler_more(get_channel_count) -> query;
query_call_handler_more(get_sessions) -> query;
query_call_handler_more(get_push_base_state) -> query;
query_call_handler_more(get_cluster_merge_state) -> query;
query_call_handler_more(Tag) -> voice_call_handler(Tag).

-spec voice_call_handler(atom()) -> voice | subscription | undefined.
voice_call_handler(voice_state_update) -> voice;
voice_call_handler(get_voice_state) -> voice;
voice_call_handler(update_member_voice) -> voice;
voice_call_handler(disconnect_voice_user) -> voice;
voice_call_handler(disconnect_voice_user_if_in_channel) -> voice;
voice_call_handler(disconnect_all_voice_users_in_channel) -> voice;
voice_call_handler(confirm_voice_connection_from_livekit) -> voice;
voice_call_handler(move_member) -> voice;
voice_call_handler(switch_voice_region) -> voice;
voice_call_handler(add_virtual_channel_access) -> voice;
voice_call_handler(store_pending_connection) -> voice;
voice_call_handler(get_voice_states_for_channel) -> voice;
voice_call_handler(get_pending_joins_for_channel) -> voice;
voice_call_handler(Tag) -> subscription_call_handler(Tag).

-spec subscription_call_handler(atom()) -> subscription | undefined.
subscription_call_handler(lazy_subscribe) -> subscription;
subscription_call_handler(_) -> undefined.

-spec handle_cast(term(), guild_state()) -> cast_reply().
handle_cast({dispatch, Request}, State) ->
    handle_dispatch_cast(Request, State);
handle_cast(
    {session_connect_async,
        #{guild_id := GuildId, attempt := Attempt, request := Request} = Msg},
    State
) ->
    handle_session_connect_async_cast(GuildId, Attempt, Request, Msg, State);
handle_cast({session_connect_worker_done, SessionId, Attempt, Result0, Computed}, State) ->
    handle_session_connect_worker_done_cast(SessionId, Attempt, Result0, Computed, State);
handle_cast({set_session_active, SessionId}, State) ->
    handle_set_session_active_cast(SessionId, State);
handle_cast({set_session_passive, SessionId}, State) ->
    handle_set_session_passive_cast(SessionId, State);
handle_cast({drop_session_member_lists, SessionId}, State) when is_binary(SessionId) ->
    {noreply, guild_member_list:unsubscribe_session(SessionId, State)};
handle_cast({set_session_typing_override, SessionId, TypingFlag}, State) ->
    handle_set_session_typing_override_cast(SessionId, TypingFlag, State);
handle_cast({send_guild_sync, SessionId}, State) ->
    handle_send_guild_sync_cast(SessionId, State);
handle_cast({send_members_chunk, SessionId, ChunkData}, State) ->
    handle_send_members_chunk_cast(SessionId, ChunkData, State);
handle_cast({patch_everyone_perms, Bit}, State) when is_integer(Bit), Bit > 0 ->
    {noreply, guild_maintenance:apply_everyone_perm_bit(Bit, State)};
handle_cast(Msg, State) when is_tuple(Msg) ->
    route_cast(element(1, Msg), Msg, State);
handle_cast(_, State) ->
    {noreply, State}.

-spec route_cast(atom(), term(), guild_state()) -> cast_reply().
route_cast(Tag, Msg, State) ->
    case cast_handler(Tag) of
        voice -> guild_voice_handler:handle_cast(Msg, State);
        subscription -> guild_subscription_handler:handle_cast(Msg, State);
        undefined -> {noreply, State}
    end.

-spec cast_handler(atom()) -> voice | subscription | undefined.
cast_handler(relay_voice_state_update) -> voice;
cast_handler(relay_voice_server_update) -> voice;
cast_handler(store_pending_connection) -> voice;
cast_handler(add_virtual_channel_access) -> voice;
cast_handler(remove_virtual_channel_access) -> voice;
cast_handler(cleanup_virtual_access_for_user) -> voice;
cast_handler(update_member_subscriptions) -> subscription;
cast_handler(_) -> undefined.

-spec handle_info(term(), guild_state()) -> info_reply().
handle_info({presence, UserId, Payload}, State) ->
    handle_presence_info(UserId, Payload, State);
handle_info({'EXIT', Pid, Reason}, State) ->
    handle_exit_info(Pid, Reason, State);
handle_info({'DOWN', Ref, process, _Pid, Reason}, State) ->
    handle_down_info(Ref, Reason, State);
handle_info(count_cache_refresh, State) ->
    State1 = update_counts(State),
    _ = guild_maintenance:schedule_count_cache_refresh(State1),
    {noreply, State1};
handle_info(availability_recheck, State) ->
    {noreply, guild_availability:handle_availability_recheck(State)};
handle_info(passive_sync, State) ->
    guild_passive_sync:handle_passive_sync(State);
handle_info(presence_reconcile, State) ->
    guild_presence_reconcile:start_async(State),
    _ = guild_presence_reconcile:schedule(),
    {noreply, State};
handle_info({presence_reconcile_apply, PresenceById}, State) when is_map(PresenceById) ->
    {noreply, guild_presence_reconcile:apply_reconcile_result(PresenceById, State)};
handle_info({reconcile_user_presence, UserId}, State) ->
    {noreply, guild_presence_reconcile:reconcile_user(UserId, State)};
handle_info({clear_stale_cached_voice_states, ConnectionIds}, State) ->
    handle_clear_stale_cached_voice_states_info(ConnectionIds, State);
handle_info(flush_lazy_subscribe_buffer, State) ->
    guild_subscription_handler:handle_info(flush_lazy_subscribe_buffer, State);
handle_info(flush_member_list_sync_batch, State) ->
    {noreply, guild_member_list:flush_pending_member_list_syncs(State)};
handle_info({check_auto_stop_empty, Token}, State) ->
    handle_auto_stop_info(Token, State);
handle_info(check_auto_stop_empty, State) ->
    {noreply, State};
handle_info(timeout, State) ->
    {noreply, State, hibernate};
handle_info(_, State) ->
    {noreply, State}.

-spec handle_session_connect_call(term(), pid(), guild_state()) -> call_reply().
handle_session_connect_call(Request, CallerPid, State) when is_map(Request) ->
    guild_sessions:handle_session_connect(
        Request, session_connect_pid(Request, CallerPid), State
    ).

-spec session_connect_pid(map(), pid()) -> pid().
session_connect_pid(#{session_pid := Pid}, _CallerPid) when is_pid(Pid) ->
    Pid;
session_connect_pid(#{session_pid := Pid}, _CallerPid) ->
    erlang:error({bad_session_pid, Pid});
session_connect_pid(_Request, CallerPid) ->
    CallerPid.

-spec handle_cached_voice_state_call(term(), guild_state()) -> call_reply().
handle_cached_voice_state_call(ConnectionId, State) when is_binary(ConnectionId) ->
    guild_voice_lifecycle:reply_cached_voice_state(ConnectionId, State).

-spec handle_reload_call(term(), guild_state()) -> call_reply().
handle_reload_call(NewData, State) when is_map(NewData) ->
    guild_init:handle_reload(NewData, State).

-spec handle_dispatch_cast(term(), guild_state()) -> cast_reply().
handle_dispatch_cast(#{event := Event, data := EventData}, State) ->
    {noreply, dispatch_event(Event, EventData, State)}.

-spec handle_session_connect_async_cast(term(), term(), term(), map(), guild_state()) ->
    cast_reply().
handle_session_connect_async_cast(GuildId, Attempt, Request, Msg, State) when
    is_integer(GuildId), is_integer(Attempt), Attempt >= 0, is_map(Request)
->
    NewState = guild_connect_async:enqueue_session_connect_async(
        GuildId, Attempt, Request, Msg, State
    ),
    {noreply, NewState}.

-spec handle_session_connect_worker_done_cast(term(), term(), term(), term(), guild_state()) ->
    cast_reply().
handle_session_connect_worker_done_cast(SessionId, Attempt, Result0, Computed, State) when
    is_binary(SessionId), is_integer(Attempt), Attempt >= 0, is_map(Computed)
->
    finalize_session_connect_worker_done(SessionId, Attempt, Result0, Computed, State);
handle_session_connect_worker_done_cast(undefined, Attempt, Result0, Computed, State) when
    is_integer(Attempt), Attempt >= 0, is_map(Computed)
->
    finalize_session_connect_worker_done(undefined, Attempt, Result0, Computed, State).

-spec finalize_session_connect_worker_done(
    binary() | undefined, non_neg_integer(), term(), map(), guild_state()
) -> cast_reply().
finalize_session_connect_worker_done(SessionId, Attempt, Result0, Computed, State) ->
    NewState = guild_connect_async:finalize_session_connect_async(
        SessionId, Attempt, session_connect_result(Result0), Computed, State
    ),
    {noreply, NewState}.

-spec session_connect_result(term()) ->
    {ok, map()} | {ok_unavailable, map()} | {error, term()}.
session_connect_result({ok, Result}) when is_map(Result) ->
    {ok, Result};
session_connect_result({ok_unavailable, Result}) when is_map(Result) ->
    {ok_unavailable, Result};
session_connect_result({error, _Reason} = Error) ->
    Error.

-spec handle_set_session_active_cast(term(), guild_state()) -> cast_reply().
handle_set_session_active_cast(SessionId, State) when is_binary(SessionId) ->
    {noreply, guild_sessions:set_session_active_guild(SessionId, state_guild_id(State), State)}.

-spec handle_set_session_passive_cast(term(), guild_state()) -> cast_reply().
handle_set_session_passive_cast(SessionId, State) when is_binary(SessionId) ->
    {noreply,
        guild_sessions:set_session_passive_guild(SessionId, state_guild_id(State), State)}.

-spec handle_set_session_typing_override_cast(term(), term(), guild_state()) -> cast_reply().
handle_set_session_typing_override_cast(SessionId, TypingFlag, State) when
    is_binary(SessionId), is_boolean(TypingFlag)
->
    {noreply, guild_sessions:handle_set_typing_override(SessionId, TypingFlag, State)}.

-spec handle_send_guild_sync_cast(term(), guild_state()) -> cast_reply().
handle_send_guild_sync_cast(SessionId, State) when is_binary(SessionId) ->
    {noreply, guild_sessions:handle_send_guild_sync(SessionId, State)}.

-spec handle_send_members_chunk_cast(term(), term(), guild_state()) -> cast_reply().
handle_send_members_chunk_cast(SessionId, ChunkData, State) when
    is_binary(SessionId), is_map(ChunkData)
->
    guild_sessions:handle_send_members_chunk(SessionId, ChunkData, State),
    {noreply, State}.

-spec handle_presence_info(term(), term(), guild_state()) -> info_reply().
handle_presence_info(UserId, Payload, State) when is_integer(UserId), is_map(Payload) ->
    guild_presence:handle_bus_presence(UserId, Payload, State).

-spec handle_exit_info(term(), term(), guild_state()) -> info_reply().
handle_exit_info(Pid, Reason, State) when is_pid(Pid) ->
    handle_exit(Pid, Reason, State).

-spec handle_down_info(term(), term(), guild_state()) -> info_reply().
handle_down_info(Ref, Reason, State) when is_reference(Ref) ->
    handle_down(Ref, Reason, State).

-spec handle_clear_stale_cached_voice_states_info(term(), guild_state()) -> info_reply().
handle_clear_stale_cached_voice_states_info(ConnectionIds, State) when is_list(ConnectionIds) ->
    {noreply,
        guild_voice_lifecycle:clear_stale_cached_voice_states(binary_ids(ConnectionIds), State)}.

-spec binary_ids([term()]) -> [binary()].
binary_ids(Ids) ->
    [Id || Id <- Ids, is_binary(Id)].

-spec handle_auto_stop_info(term(), guild_state()) -> info_reply().
handle_auto_stop_info(Token, State) when is_reference(Token) ->
    handle_auto_stop(Token, State).

-spec handle_exit(pid(), term(), guild_state()) -> info_reply().
handle_exit(Pid, Reason, State) ->
    case maps:get(voice_server_pid, State, undefined) of
        Pid -> {noreply, guild_voice_lifecycle:handle_voice_server_exit(Pid, Reason, State)};
        _ -> handle_non_voice_exit(Pid, Reason, State)
    end.

-spec handle_non_voice_exit(pid(), term(), guild_state()) -> info_reply().
handle_non_voice_exit(Pid, Reason, State) ->
    case maps:get(broadcaster_pid, State, undefined) of
        Pid ->
            {noreply, maps:remove(broadcaster_pid, State)};
        _ ->
            {stop, linked_process_exit_reason(Pid, Reason), State}
    end.

-spec handle_down(reference(), term(), guild_state()) -> info_reply().
handle_down(Ref, Reason, State) ->
    WorkerRefs = session_connect_worker_refs(State),
    case maps:is_key(Ref, WorkerRefs) of
        true ->
            handle_session_connect_worker_down(Ref, Reason, WorkerRefs, State);
        false ->
            guild_sessions:handle_session_down(Ref, State)
    end.

-spec handle_session_connect_worker_down(reference(), term(), map(), guild_state()) ->
    info_reply().
handle_session_connect_worker_down(Ref, Reason, WorkerRefs, State) ->
    State1 = State#{session_connect_worker_refs => maps:remove(Ref, WorkerRefs)},
    handle_session_connect_worker_down_reason(Reason, State1).

-spec handle_session_connect_worker_down_reason(term(), guild_state()) -> info_reply().
handle_session_connect_worker_down_reason(normal, State) ->
    {noreply, State};
handle_session_connect_worker_down_reason(_Reason, State) ->
    State1 = guild_connect_async:decrement_session_connect_inflight(State),
    {noreply, guild_connect_async:maybe_start_session_connect_workers(State1)}.

-spec handle_auto_stop(reference(), guild_state()) ->
    {noreply, guild_state()} | {stop, normal, guild_state()}.
handle_auto_stop(Token, State) ->
    case maps:get(auto_stop_pending, State, undefined) of
        #{token := Token} ->
            auto_stop_pending_reply(State);
        _ ->
            {noreply, State}
    end.

-spec terminate(term(), guild_state() | term()) -> ok.
terminate(Reason, State) when is_map(State) ->
    safe_cleanup(
        fun() ->
            PresenceSubs = presence_subscriptions(State),
            lists:foreach(fun safe_unsubscribe_presence/1, maps:keys(PresenceSubs))
        end,
        "presence_unsubscribe"
    ),
    safe_cleanup(
        fun() ->
            guild_maintenance:maybe_delete_permission_cache(
                maps:get(id, State, undefined), State
            )
        end,
        "permission_cache_delete"
    ),
    safe_cleanup(
        fun() ->
            cleanup_per_guild_ets(State)
        end,
        "ets_cleanup"
    ),
    safe_cleanup(
        fun() ->
            cleanup_voice_server(State)
        end,
        "voice_cleanup"
    ),
    safe_cleanup(
        fun() ->
            cleanup_member_list_subs(State)
        end,
        "member_list_subs_cleanup"
    ),
    safe_cleanup(
        fun() ->
            cleanup_member_list_engine(State)
        end,
        "member_list_engine_cleanup"
    ),
    maybe_report_crash(Reason, State),
    ok;
terminate(Reason, State) ->
    maybe_report_crash(Reason, State),
    ok.

-spec code_change(term(), guild_state(), term()) -> {ok, guild_state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:process_flag(fullsweep_after, 10),
    erlang:garbage_collect(),
    {ok, State}.

-spec safe_unsubscribe_presence(integer()) -> ok.
safe_unsubscribe_presence(UserId) ->
    try presence_bus:unsubscribe(UserId) of
        _ -> ok
    catch
        throw:_Reason -> ok;
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec safe_cleanup(fun(() -> term()), string()) -> ok.
safe_cleanup(Fun, Label) ->
    try Fun() of
        _ -> ok
    catch
        Class:Reason ->
            logger:warning(
                "guild_terminate_cleanup_failed: step=~s class=~p reason=~p",
                [Label, Class, Reason]
            ),
            ok
    end.

-spec cleanup_per_guild_ets(guild_state()) -> ok.
cleanup_per_guild_ets(State) ->
    Data = maps:get(data, State, #{}),
    safe_delete_ets(maps:get(members_ets, Data, undefined)),
    safe_delete_ets(maps:get(member_presence, State, undefined)),
    safe_delete_ets(maps:get(viewable_channels_cache, State, undefined)),
    ok.

-spec cleanup_voice_server(guild_state()) -> ok.
cleanup_voice_server(State) ->
    case maps:get(voice_server_pid, State, undefined) of
        Pid when is_pid(Pid) ->
            stop_voice_server_if_alive(Pid);
        _ ->
            ok
    end.

-spec stop_voice_server_if_alive(pid()) -> ok.
stop_voice_server_if_alive(Pid) ->
    case process_liveness:is_alive(Pid) of
        true -> safe_stop_voice_server(Pid);
        false -> ok
    end.

-spec safe_stop_voice_server(pid()) -> ok.
safe_stop_voice_server(Pid) ->
    try gen_server:stop(Pid, shutdown, 5000) of
        _ -> ok
    catch
        exit:_Reason -> ok
    end.

-spec cleanup_member_list_subs(guild_state()) -> ok.
cleanup_member_list_subs(State) ->
    case maps:get(member_list_subscriptions, State, undefined) of
        Tab when Tab =/= undefined ->
            guild_member_list_subs:destroy(Tab);
        _ ->
            ok
    end.

-spec cleanup_member_list_engine(guild_state()) -> ok.
cleanup_member_list_engine(State) ->
    _ = guild_member_list_channel_engine:destroy_all(State),
    case maps:get(member_list_engine, State, undefined) of
        Ref when Ref =/= undefined ->
            guild_member_list_engine:destroy(Ref);
        _ ->
            ok
    end.

-spec safe_delete_ets(term()) -> ok.
safe_delete_ets(undefined) ->
    ok;
safe_delete_ets(Tab) ->
    try ets:delete(eqwalizer:dynamic_cast(Tab)) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec linked_process_exit_reason(pid(), term()) -> term().
linked_process_exit_reason(_Pid, normal) -> normal;
linked_process_exit_reason(_Pid, shutdown) -> shutdown;
linked_process_exit_reason(_Pid, {shutdown, _} = Reason) -> Reason;
linked_process_exit_reason(Pid, Reason) -> {linked_process_exit, Pid, Reason}.

-spec handle_dispatch_call(term(), guild_state()) -> {reply, ok, guild_state()}.
handle_dispatch_call(#{event := Event, data := EventData}, State) ->
    {reply, ok, dispatch_event(Event, EventData, State)}.

-spec dispatch_event(term(), term(), guild_state()) -> guild_state().
dispatch_event(Event, EventData, State) ->
    ParsedEventData = parse_event_data(EventData),
    {noreply, NewState} = guild_dispatch:handle_dispatch(
        Event, ParsedEventData, State
    ),
    StateAfterPrune = guild_maintenance:maybe_prune_invalid_member_subscriptions(
        Event, NewState
    ),
    ok = maybe_refresh_permission_cache(Event, ParsedEventData, State, StateAfterPrune),
    StateAfterPrune.

-spec parse_event_data(term()) -> map().
parse_event_data(D) when is_binary(D) -> require_map(json:decode(D));
parse_event_data(D) when is_map(D) -> D.

-spec maybe_refresh_permission_cache(term(), map(), guild_state(), guild_state()) -> ok.
maybe_refresh_permission_cache(Event, EventData, OldState, NewState) ->
    case event_mutates_guild_data(Event) of
        true ->
            guild_maintenance:maybe_put_permission_cache(Event, EventData, OldState, NewState);
        false ->
            ok
    end.

-spec event_mutates_guild_data(term()) -> boolean().
event_mutates_guild_data(E) ->
    lists:member(E, [
        guild_member_add,
        guild_member_update,
        guild_member_remove,
        guild_role_create,
        guild_role_update,
        guild_role_update_bulk,
        guild_role_delete,
        channel_create,
        channel_update,
        channel_update_bulk,
        channel_delete,
        guild_update
    ]).

-spec state_guild_id(guild_state()) -> integer().
state_guild_id(#{id := GuildId}) when is_integer(GuildId) ->
    GuildId.

-spec session_connect_worker_refs(guild_state()) -> map().
session_connect_worker_refs(State) ->
    require_map(maps:get(session_connect_worker_refs, State, #{})).

-spec auto_stop_pending_reply(guild_state()) ->
    {noreply, guild_state()} | {stop, normal, guild_state()}.
auto_stop_pending_reply(State) ->
    CleanState = maps:remove(auto_stop_pending, State),
    case map_size(sessions_map(State)) of
        0 -> {stop, normal, CleanState};
        _ -> {noreply, CleanState}
    end.

-spec sessions_map(guild_state()) -> map().
sessions_map(State) ->
    require_map(maps:get(sessions, State, #{})).

-spec presence_subscriptions(guild_state()) -> map().
presence_subscriptions(State) ->
    require_map(maps:get(presence_subscriptions, State, #{})).

-spec require_map(term()) -> map().
require_map(Value) when is_map(Value) ->
    Value;
require_map(Value) ->
    erlang:error({badmap, Value}).

-spec voice_guild_state(guild_state()) -> map().
voice_guild_state(State) ->
    case maps:get(data, State, #{}) of
        #{members_ets := Tab} = Data ->
            case voice_members_table_healthy(Tab) of
                true ->
                    ok = clear_voice_members_table_warning(),
                    project_voice_guild_state(Data, State);
                false ->
                    voice_members_table_unavailable(State)
            end;
        _ ->
            voice_members_table_unavailable(State)
    end.

-spec voice_members_table_healthy(term()) -> boolean().
voice_members_table_healthy(Tab) when is_reference(Tab) ->
    try ets:info(eqwalizer:dynamic_cast(Tab), owner) of
        Owner when is_pid(Owner) -> Owner =:= self();
        _ -> false
    catch
        error:badarg -> false
    end;
voice_members_table_healthy(_Tab) ->
    false.

-spec project_voice_guild_state(map(), guild_state()) -> map().
project_voice_guild_state(Data, State) ->
    Projected = maps:with(voice_guild_state_keys(), State),
    project_voice_sessions(Projected#{data => maps:with(voice_guild_data_keys(), Data)}).

%% The sessions map keeps every key it had, so fold order over it is unchanged and the
%% dispatch order of every voice broadcast is unchanged. Only session ENTRIES are narrowed.
-spec project_voice_sessions(map()) -> map().
project_voice_sessions(#{sessions := Sessions} = Projected) when is_map(Sessions) ->
    Projected#{sessions => maps:map(fun project_voice_session/2, Sessions)};
project_voice_sessions(Projected) ->
    Projected.

-spec project_voice_session(term(), term()) -> term().
project_voice_session(_SessionId, Session) when is_map(Session) ->
    maps:without(voice_session_drop_keys(), Session);
project_voice_session(_SessionId, Session) ->
    Session.

%% Every reader reachable from the voice guild state takes only pending_connect, user_id,
%% viewable_channels and pid out of a session entry: guild_sessions:filter_sessions_for_channel,
%% guild_voice_broadcast and guild_virtual_channel_access. These five are read nowhere on that path.
-spec voice_session_drop_keys() -> [atom()].
voice_session_drop_keys() ->
    [active_guilds, user_roles, mref, bot, is_staff].

-spec voice_guild_state_keys() -> [atom() | binary()].
voice_guild_state_keys() ->
    [
        id,
        <<"id">>,
        sessions,
        guild_pid,
        voice_states,
        pending_voice_connections,
        recently_disconnected_voice_states,
        e2ee_room_keys,
        virtual_channel_access,
        virtual_channel_access_pending,
        virtual_channel_access_preserve,
        virtual_channel_access_move_pending,
        test_perm_fun,
        test_force_disconnect_fun,
        test_livekit_fun,
        test_permission_sync_fun
    ].

-spec voice_guild_data_keys() -> [atom() | binary()].
voice_guild_data_keys() ->
    [
        <<"id">>,
        <<"guild">>,
        <<"roles">>,
        <<"role_index">>,
        role_perms_cache,
        <<"channels">>,
        <<"channel_index">>,
        overwrite_perms_cache,
        members_ets
    ].

-spec voice_members_table_unavailable(guild_state()) -> map().
voice_members_table_unavailable(State) ->
    ok = log_voice_members_table_unavailable(maps:get(id, State, undefined)),
    State.

-spec log_voice_members_table_unavailable(term()) -> ok.
log_voice_members_table_unavailable(GuildId) ->
    case erlang:put(?VOICE_MEMBERS_TABLE_WARNED, true) of
        true ->
            ok;
        _ ->
            logger:warning("guild_voice_members_table_unavailable: guild_id=~p", [GuildId])
    end.

-spec clear_voice_members_table_warning() -> ok.
clear_voice_members_table_warning() ->
    _ = erlang:erase(?VOICE_MEMBERS_TABLE_WARNED),
    ok.

-spec maybe_report_crash(term(), term()) -> ok.
maybe_report_crash(normal, _) ->
    ok;
maybe_report_crash(shutdown, _) ->
    ok;
maybe_report_crash({shutdown, _}, _) ->
    ok;
maybe_report_crash(_Reason, _State) ->
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

handle_non_voice_exit_broadcaster_keeps_guild_alive_test() ->
    BPid = list_to_pid("<0.250.0>"),
    State = #{id => 42, broadcaster_pid => BPid},
    ?assertEqual({noreply, #{id => 42}}, handle_non_voice_exit(BPid, killed, State)).

handle_non_voice_exit_other_linked_stops_guild_test() ->
    BPid = list_to_pid("<0.250.0>"),
    OtherPid = list_to_pid("<0.251.0>"),
    State = #{id => 42, broadcaster_pid => BPid},
    ?assertMatch(
        {stop, {linked_process_exit, OtherPid, boom}, State},
        handle_non_voice_exit(OtherPid, boom, State)
    ).

voice_guild_state_pins_projected_key_set_test() ->
    with_voice_projection_state(fun(Full) ->
        Projected = voice_guild_state(Full),
        ?assertEqual(
            lists:sort([
                id,
                <<"id">>,
                data,
                sessions,
                guild_pid,
                voice_states,
                pending_voice_connections,
                recently_disconnected_voice_states,
                e2ee_room_keys,
                virtual_channel_access,
                virtual_channel_access_pending,
                virtual_channel_access_preserve,
                virtual_channel_access_move_pending
            ]),
            lists:sort(maps:keys(Projected))
        ),
        ?assertEqual(
            lists:sort([
                <<"id">>,
                <<"guild">>,
                <<"roles">>,
                <<"role_index">>,
                role_perms_cache,
                <<"channels">>,
                <<"channel_index">>,
                overwrite_perms_cache,
                members_ets
            ]),
            lists:sort(maps:keys(maps:get(data, Projected)))
        )
    end).

voice_guild_state_drops_non_voice_payload_test() ->
    with_voice_projection_state(fun(Full) ->
        Projected = voice_guild_state(Full),
        ?assertNot(maps:is_key(member_list_subscriptions, Projected)),
        ?assertNot(maps:is_key(presence_subscriptions, Projected)),
        ?assertNot(maps:is_key(connected_user_ids, Projected)),
        Data = maps:get(data, Projected),
        ?assertNot(maps:is_key(<<"members">>, Data)),
        ?assertNot(maps:is_key(members_normalized, Data)),
        ?assertNot(maps:is_key(<<"member_role_index">>, Data)),
        ?assertNot(maps:is_key(<<"emojis">>, Data))
    end).

voice_guild_state_preserves_member_permissions_test() ->
    with_voice_projection_state(fun(Full) ->
        Projected = voice_guild_state(Full),
        Expected = guild_permissions:get_member_permissions(10, 500, Full),
        ?assert(Expected > 0),
        ?assert(permission_bits:has(Expected, constants:speak_permission())),
        ?assertEqual(Expected, guild_permissions:get_member_permissions(10, 500, Projected)),
        ?assertEqual(
            voice_utils:compute_voice_permissions(10, 500, Full),
            voice_utils:compute_voice_permissions(10, 500, Projected)
        ),
        ?assertEqual(
            guild_permissions:find_member_by_user_id(10, Full),
            guild_permissions:find_member_by_user_id(10, Projected)
        ),
        ?assertEqual(
            guild_permissions:can_view_channel(10, 500, undefined, Full),
            guild_permissions:can_view_channel(10, 500, undefined, Projected)
        )
    end).

voice_guild_state_absent_member_still_resolves_to_zero_test() ->
    with_voice_projection_state(fun(Full) ->
        Projected = voice_guild_state(Full),
        ?assertEqual(undefined, guild_permissions:find_member_by_user_id(11, Projected)),
        ?assertEqual(0, guild_permissions:get_member_permissions(11, 500, Projected)),
        ?assertEqual(
            guild_permissions:get_member_permissions(11, 500, Full),
            guild_permissions:get_member_permissions(11, 500, Projected)
        )
    end).

voice_guild_state_put_member_keeps_permissions_resolvable_test() ->
    with_voice_projection_state(fun(Full) ->
        Projected = voice_guild_state(Full),
        Data = maps:get(data, Projected),
        Member = guild_permissions:find_member_by_user_id(10, Projected),
        ?assert(is_map(Member)),
        Rebuilt = guild_data_index:put_member(Member#{<<"mute">> => true}, Data),
        RebuiltState = Projected#{data => Rebuilt},
        ?assertEqual(1, map_size(maps:get(<<"members">>, Rebuilt))),
        ?assertEqual(1, map_size(maps:get(members_normalized, Rebuilt))),
        ?assertEqual(maps:get(members_ets, Data), maps:get(members_ets, Rebuilt)),
        ?assertEqual(
            guild_permissions:get_member_permissions(10, 500, Full),
            guild_permissions:get_member_permissions(10, 500, RebuiltState)
        ),
        ?assertMatch(
            #{<<"mute">> := true},
            guild_permissions:find_member_by_user_id(10, RebuiltState)
        ),
        ?assertEqual(
            guild_permissions:can_view_channel(10, 500, undefined, Full),
            guild_permissions:can_view_channel(10, 500, undefined, RebuiltState)
        )
    end).

voice_guild_state_without_members_table_returns_full_state_test() ->
    with_voice_projection_state(fun(Full) ->
        Data = maps:get(data, Full),
        Broken = Full#{data => maps:remove(members_ets, Data)},
        _ = erlang:erase(?VOICE_MEMBERS_TABLE_WARNED),
        ?assertEqual(Broken, voice_guild_state(Broken)),
        ?assertEqual(true, erlang:get(?VOICE_MEMBERS_TABLE_WARNED)),
        _ = erlang:erase(?VOICE_MEMBERS_TABLE_WARNED)
    end).

voice_members_table_warning_rearms_after_recovery_test() ->
    with_voice_projection_state(fun(Full) ->
        Data = maps:get(data, Full),
        Broken = Full#{data => maps:remove(members_ets, Data)},
        _ = erlang:erase(?VOICE_MEMBERS_TABLE_WARNED),
        _ = voice_guild_state(Broken),
        ?assertEqual(true, erlang:get(?VOICE_MEMBERS_TABLE_WARNED)),
        _ = voice_guild_state(Broken),
        ?assertEqual(true, erlang:get(?VOICE_MEMBERS_TABLE_WARNED)),
        _ = voice_guild_state(Full),
        ?assertEqual(undefined, erlang:get(?VOICE_MEMBERS_TABLE_WARNED)),
        _ = voice_guild_state(Broken),
        ?assertEqual(true, erlang:get(?VOICE_MEMBERS_TABLE_WARNED)),
        _ = erlang:erase(?VOICE_MEMBERS_TABLE_WARNED)
    end).

voice_guild_state_session_entries_keep_only_reader_keys_test() ->
    with_voice_projection_state(fun(Base) ->
        Full = Base#{sessions => voice_projection_sessions()},
        Projected = voice_guild_state(Full),
        Sessions = maps:get(sessions, Projected),
        Session = maps:get(<<"s-view">>, Sessions),
        ?assertEqual(
            lists:sort([session_id, user_id, pid, pending_connect, viewable_channels]),
            lists:sort(maps:keys(Session))
        ),
        ?assertNot(maps:is_key(active_guilds, Session)),
        ?assertNot(maps:is_key(user_roles, Session)),
        ?assertNot(maps:is_key(mref, Session)),
        ?assertNot(maps:is_key(bot, Session)),
        ?assertNot(maps:is_key(is_staff, Session))
    end).

voice_guild_state_session_projection_matches_reference_test() ->
    with_voice_projection_state(fun(Base) ->
        Full = Base#{sessions => voice_projection_sessions()},
        Reference = reference_voice_guild_state(Full),
        Projected = voice_guild_state(Full),
        ?assertEqual(maps:remove(sessions, Reference), maps:remove(sessions, Projected)),
        RefSessions = maps:get(sessions, Reference),
        NewSessions = maps:get(sessions, Projected),
        ?assertEqual(maps:keys(RefSessions), maps:keys(NewSessions)),
        ?assertEqual(
            maps:map(fun project_voice_session/2, RefSessions),
            NewSessions
        )
    end).

voice_guild_state_session_readers_see_identical_results_test() ->
    with_voice_projection_state(fun(Base) ->
        Full = Base#{sessions => voice_projection_sessions()},
        Reference = reference_voice_guild_state(Full),
        Projected = voice_guild_state(Full),
        Expected = voice_session_reader_result(500, Reference),
        ?assertEqual(Expected, voice_session_reader_result(500, Projected)),
        {SessionIds, Pids} = Expected,
        ?assertEqual([<<"s-cached">>, <<"s-perm">>, <<"s-view">>], lists:sort(SessionIds)),
        ?assertEqual([self(), self(), self()], Pids),
        Hidden = voice_session_reader_result(501, Reference),
        ?assertEqual(Hidden, voice_session_reader_result(501, Projected)),
        ?assertEqual({[<<"s-cached">>], [self()]}, Hidden)
    end).

voice_guild_state_session_projection_boundaries_test() ->
    with_voice_projection_state(fun(Base) ->
        Bare = #{user_id => 10, pid => self()},
        Full = Base#{sessions => #{<<"s-bare">> => Bare, <<"s-odd">> => not_a_map}},
        Sessions = maps:get(sessions, voice_guild_state(Full)),
        ?assertEqual(Bare, maps:get(<<"s-bare">>, Sessions)),
        ?assertEqual(not_a_map, maps:get(<<"s-odd">>, Sessions)),
        NoSessions = maps:remove(sessions, Base),
        ?assertNot(maps:is_key(sessions, voice_guild_state(NoSessions))),
        BadSessions = Base#{sessions => not_a_map},
        ?assertEqual(not_a_map, maps:get(sessions, voice_guild_state(BadSessions)))
    end).

voice_guild_state_preserves_session_fold_order_at_scale_test() ->
    with_voice_projection_state(fun voice_session_fold_order_scenario/1).

%% 200 entries forces the hashmap representation, where iteration order is driven by key
%% hashes. maps:map/2 rewrites values only, so the key set and therefore the order is identical.
voice_session_fold_order_scenario(Base) ->
    Sessions = maps:from_list(lists:map(fun voice_scale_session/1, lists:seq(1, 200))),
    Full = Base#{sessions => Sessions},
    Projected = voice_guild_state(Full),
    ?assertEqual(maps:keys(Sessions), maps:keys(maps:get(sessions, Projected))),
    ?assertEqual(
        voice_session_reader_result(500, reference_voice_guild_state(Full)),
        voice_session_reader_result(500, Projected)
    ).

voice_scale_session(N) ->
    SessionId = integer_to_binary(N),
    {SessionId, voice_projection_session(SessionId, 10, #{500 => true}, false)}.

voice_session_drop_keys_never_drops_a_reader_key_test() ->
    Dropped = voice_session_drop_keys(),
    Readers = [pending_connect, user_id, viewable_channels, pid],
    ?assertEqual([], [K || K <- Readers, lists:member(K, Dropped)]).

%% Reproduces the projection exactly as it was before session entries were narrowed, so the
%% tests above compare the new path against a live oracle rather than a hand-written literal.
reference_voice_guild_state(State) ->
    Data = maps:get(data, State, #{}),
    Projected = maps:with(voice_guild_state_keys(), State),
    Projected#{data => maps:with(voice_guild_data_keys(), Data)}.

%% The reader tuple is everything the voice path can observe about the sessions map: which
%% sessions match, in which fold order, and which pids the broadcast dispatches to.
voice_session_reader_result(ChannelId, State) ->
    Sessions = maps:get(sessions, State, #{}),
    Pairs = guild_sessions:filter_sessions_for_channel(Sessions, ChannelId, undefined, State),
    {[Sid || {Sid, _S} <- Pairs], [maps:get(pid, S) || {_Sid, S} <- Pairs]}.

voice_projection_sessions() ->
    #{
        <<"s-view">> => voice_projection_session(<<"s-view">>, 10, #{500 => true}, false),
        <<"s-perm">> => voice_projection_session(<<"s-perm">>, 10, #{}, false),
        <<"s-pending">> => voice_projection_session(<<"s-pending">>, 10, #{500 => true}, true),
        <<"s-stranger">> => voice_projection_session(<<"s-stranger">>, 11, #{}, false),
        <<"s-cached">> => voice_projection_session(<<"s-cached">>, 10, #{501 => true}, false)
    }.

voice_projection_session(SessionId, UserId, ViewableChannels, Pending) ->
    #{
        session_id => SessionId,
        user_id => UserId,
        pid => self(),
        mref => make_ref(),
        active_guilds => sets:from_list([42, 43, 44]),
        user_roles => [77],
        bot => false,
        is_staff => false,
        pending_connect => Pending,
        viewable_channels => ViewableChannels
    }.

voice_members_table_warning_key_is_module_scoped_test() ->
    ?assertEqual({?MODULE, voice_members_table_unavailable}, ?VOICE_MEMBERS_TABLE_WARNED),
    ?assertNot(is_atom(?VOICE_MEMBERS_TABLE_WARNED)).

with_voice_projection_state(Fun) ->
    Tab = ets:new(guild_members_data, [set, public, {read_concurrency, true}]),
    try
        Fun(voice_projection_state(Tab))
    after
        ets:delete(Tab)
    end.

voice_projection_state(Tab) ->
    Data = voice_projection_data(),
    maps:foreach(
        fun(UserId, Member) -> ets:insert(Tab, {UserId, Member}) end,
        maps:get(members_normalized, Data)
    ),
    #{
        id => 42,
        <<"id">> => <<"42">>,
        data => Data#{members_ets => Tab, <<"emojis">> => [], channels_stale => true},
        sessions => #{},
        guild_pid => self(),
        voice_states => #{},
        pending_voice_connections => #{},
        recently_disconnected_voice_states => #{},
        e2ee_room_keys => #{},
        virtual_channel_access => #{},
        virtual_channel_access_pending => #{},
        virtual_channel_access_preserve => #{},
        virtual_channel_access_move_pending => #{},
        presence_subscriptions => #{},
        member_list_subscriptions => #{},
        connected_user_ids => sets:new(),
        counts => #{member_count => 1}
    }.

voice_projection_data() ->
    Everyone = #{
        <<"id">> => <<"42">>,
        <<"name">> => <<"@everyone">>,
        <<"permissions">> => integer_to_binary(constants:view_channel_permission())
    },
    Speaker = #{
        <<"id">> => <<"77">>,
        <<"name">> => <<"speaker">>,
        <<"permissions">> => integer_to_binary(
            constants:connect_permission() bor constants:speak_permission()
        )
    },
    Channel = #{
        <<"id">> => <<"500">>,
        <<"type">> => 2,
        <<"name">> => <<"General">>,
        <<"permission_overwrites">> => [
            #{
                <<"id">> => <<"77">>,
                <<"type">> => 0,
                <<"allow">> => integer_to_binary(constants:stream_permission()),
                <<"deny">> => <<"0">>
            }
        ]
    },
    HiddenChannel = #{
        <<"id">> => <<"501">>,
        <<"type">> => 2,
        <<"name">> => <<"Hidden">>,
        <<"permission_overwrites">> => [
            #{
                <<"id">> => <<"42">>,
                <<"type">> => 0,
                <<"allow">> => <<"0">>,
                <<"deny">> => integer_to_binary(constants:view_channel_permission())
            }
        ]
    },
    Member = #{
        <<"user">> => #{<<"id">> => <<"10">>, <<"username">> => <<"speaker">>},
        <<"roles">> => [<<"77">>],
        <<"mute">> => false,
        <<"deaf">> => false
    },
    guild_data_index:normalize_data(#{
        <<"id">> => <<"42">>,
        <<"guild">> => #{<<"id">> => <<"42">>, <<"owner_id">> => <<"9999">>},
        <<"roles">> => [Everyone, Speaker],
        <<"channels">> => [Channel, HiddenChannel],
        <<"members">> => [Member]
    }).

-endif.
