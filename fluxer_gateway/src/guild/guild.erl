%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild).
-feature(maybe_expr, enable).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/1, update_counts/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(HIBERNATE_AFTER_MS, 15000).
-define(FULLSWEEP_AFTER, 100).

-type guild_state() :: map().
-type call_reply() ::
    {reply, term(), guild_state()}
    | {noreply, guild_state()}
    | {stop, term(), term(), guild_state()}.
-type cast_reply() :: {noreply, guild_state()}.
-type info_reply() ::
    {noreply, guild_state()}
    | {stop, term(), guild_state()}.

-spec start_link(map()) -> gen_server:start_ret().
start_link(GuildState) ->
    gen_server:start_link(?MODULE, GuildState, [{hibernate_after, ?HIBERNATE_AFTER_MS}]).

-spec update_counts(guild_state()) -> guild_state().
update_counts(State) -> guild_maintenance:update_counts(State).

-spec init(map()) -> {ok, guild_state()}.
init(GuildState) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, ?FULLSWEEP_AFTER),
    State0 = guild_init:init_base_state(GuildState),
    State1 = guild_init:init_member_list(State0),
    State2 = guild_init:init_counts(State1),
    State3 = guild_init:init_caches_and_timers(State2),
    State4 = guild_init:init_voice_server(State3),
    erlang:garbage_collect(),
    State5 = guild_health:register_guild(State4),
    ok = guild_read_model:put_state(State5),
    {ok, State5}.

-spec handle_call(term(), gen_server:from(), guild_state()) -> call_reply().
handle_call(Msg, From, State) ->
    ok = guild_mailbox_age:note(),
    Result = handle_call_internal(Msg, From, State),
    ok = publish_read_model(Result, State),
    Result.

-spec handle_cast(term(), guild_state()) -> cast_reply().
handle_cast(Msg, State) ->
    ok = guild_mailbox_age:note(),
    Result = handle_cast_internal(Msg, State),
    ok = publish_read_model(Result, State),
    Result.

-spec handle_info(term(), guild_state()) -> info_reply().
handle_info({guild_mailbox_age, Seq}, State) when is_integer(Seq), Seq >= 0 ->
    ok = guild_mailbox_age:handle_mark(Seq),
    {noreply, State};
handle_info(Msg, State) ->
    ok = guild_mailbox_age:note(),
    Result = handle_info_internal(Msg, State),
    ok = publish_read_model(Result, State),
    Result.

-spec publish_read_model(call_reply() | cast_reply() | info_reply(), guild_state()) -> ok.
publish_read_model({reply, _Reply, NewState}, OldState) ->
    guild_read_model:update(OldState, NewState);
publish_read_model({noreply, NewState}, OldState) ->
    guild_read_model:update(OldState, NewState);
publish_read_model(_Result, _OldState) ->
    ok.

-spec handle_call_internal(term(), gen_server:from(), guild_state()) -> call_reply().
handle_call_internal({session_connect, Request}, {CallerPid, _}, State) ->
    handle_session_connect_call(Request, CallerPid, State);
handle_call_internal(export_handoff_state, _From, State) ->
    {reply, {ok, guild_handoff:export_handoff_state(State)}, State};
handle_call_internal({get_guild_id}, _From, State) ->
    {reply, maps:get(id, State, undefined), State};
handle_call_internal({get_voice_guild_state}, _From, State) ->
    {reply, guild_voice_projection:from_state(State), State};
handle_call_internal({dispatch, Request}, _From, State) ->
    handle_dispatch_call(Request, State);
handle_call_internal({reload, NewData}, _From, State) ->
    handle_reload_call(NewData, State);
handle_call_internal(get_voice_server_pid, _From, State) ->
    guild_voice_lifecycle:reply_voice_server_pid(State);
handle_call_internal({released_push_holds, SessionIds}, _From, State) when
    is_list(SessionIds)
->
    {reply, guild_sessions:released_push_holds(SessionIds, State), State};
handle_call_internal({evict_visitors, IpKeys}, _From, State) when is_list(IpKeys) ->
    {UserIds, NewState} = guild_visitors:evict_ips([K || K <- IpKeys, is_binary(K)], State),
    {reply, {ok, UserIds}, NewState};
handle_call_internal({terminate}, _From, State) ->
    {stop, normal, ok, State};
handle_call_internal(Msg, From, State) when is_tuple(Msg) ->
    guild_request_router:route_call(element(1, Msg), Msg, From, State);
handle_call_internal(_, _From, State) ->
    {reply, ok, State}.

-spec handle_cast_internal(term(), guild_state()) -> cast_reply().
handle_cast_internal({dispatch, Request}, State) ->
    handle_dispatch_cast(Request, State);
handle_cast_internal({dispatch_many, Requests}, State) when is_list(Requests) ->
    {noreply,
        lists:foldl(
            fun(#{event := Event, data := EventData}, Acc) ->
                dispatch_event(Event, EventData, Acc)
            end,
            State,
            Requests
        )};
handle_cast_internal(
    {session_connect_async,
        #{guild_id := GuildId, attempt := Attempt, request := Request} = Msg},
    State
) ->
    handle_session_connect_async_cast(GuildId, Attempt, Request, Msg, State);
handle_cast_internal(
    {session_connect_worker_done, SessionId, Attempt, Result0, Computed}, State
) ->
    handle_session_connect_worker_done_cast(SessionId, Attempt, Result0, Computed, State);
handle_cast_internal({session_connect_worker_batch_done, Results}, State) when
    is_list(Results)
->
    {noreply, guild_connect_async:finalize_session_connect_batch(Results, State)};
handle_cast_internal({visitor_leave, SessionId}, State) when is_binary(SessionId) ->
    {noreply, guild_visitors:remove_session(SessionId, State)};
handle_cast_internal({visitor_focus, SessionId, ChannelId}, State) when is_binary(SessionId) ->
    {noreply, guild_visitors:set_focus(SessionId, ChannelId, State)};
handle_cast_internal({set_session_active, SessionId}, State) ->
    handle_set_session_active_cast(SessionId, State);
handle_cast_internal({set_session_passive, SessionId}, State) ->
    handle_set_session_passive_cast(SessionId, State);
handle_cast_internal({drop_session_member_lists, SessionId}, State) when is_binary(SessionId) ->
    {noreply, guild_member_list_subscribe:unsubscribe_session(SessionId, State)};
handle_cast_internal({set_session_typing_override, SessionId, TypingFlag}, State) ->
    handle_set_session_typing_override_cast(SessionId, TypingFlag, State);
handle_cast_internal({set_session_push_hold, SessionId, Hold}, State) when
    is_binary(SessionId), is_boolean(Hold)
->
    {noreply, guild_sessions:set_session_push_hold(SessionId, Hold, State)};
handle_cast_internal({send_guild_sync, SessionId}, State) ->
    handle_send_guild_sync_cast(SessionId, State);
handle_cast_internal({thread_user_flip, SessionId}, State) when is_binary(SessionId) ->
    {noreply, guild_thread_flip:user_flip(SessionId, State)};
handle_cast_internal({thread_subscriptions, SessionId, Subscriptions}, State) when
    is_binary(SessionId), is_map(Subscriptions)
->
    {noreply, guild_thread_subscriptions:update(SessionId, Subscriptions, State)};
handle_cast_internal({patch_everyone_perms, Bit}, State) when is_integer(Bit), Bit > 0 ->
    {noreply, guild_maintenance:apply_everyone_perm_bit(Bit, State)};
handle_cast_internal(Msg, State) when is_tuple(Msg) ->
    guild_request_router:route_cast(element(1, Msg), Msg, State);
handle_cast_internal(_, State) ->
    {noreply, State}.

-spec handle_info_internal(term(), guild_state()) -> info_reply().
handle_info_internal({guild_health_probe, Ref}, State) when is_reference(Ref) ->
    gen_server:cast(guild_health, {pong, self(), Ref, erlang:monotonic_time(millisecond)}),
    {noreply, State};
handle_info_internal(guild_health_register, State) ->
    {noreply, guild_health:register_guild(State)};
handle_info_internal({presence, UserId, Payload}, State) ->
    handle_presence_info(UserId, Payload, State);
handle_info_internal({'EXIT', Pid, Reason}, State) ->
    handle_exit_info(Pid, Reason, State);
handle_info_internal({'DOWN', Ref, process, _Pid, Reason}, State) ->
    handle_down_info(Ref, Reason, State);
handle_info_internal(count_cache_refresh, State) ->
    State1 = update_counts(State),
    _ = guild_maintenance:schedule_count_cache_refresh(State1),
    {noreply, State1};
handle_info_internal(availability_recheck, State) ->
    {noreply, guild_availability:handle_availability_recheck(State)};
handle_info_internal(passive_sync, State) ->
    guild_passive_sync:handle_passive_sync(State);
handle_info_internal(presence_reconcile, State) ->
    guild_presence_reconcile:start_async(State),
    _ = guild_presence_reconcile:schedule(),
    {noreply, State};
handle_info_internal({presence_reconcile_apply, Mismatches}, State) when is_list(Mismatches) ->
    {noreply,
        guild_presence_reconcile:apply_mismatches(eqwalizer:dynamic_cast(Mismatches), State)};
handle_info_internal({reconcile_user_presence, UserId}, State) ->
    {noreply, guild_presence_reconcile:reconcile_user(UserId, State)};
handle_info_internal({clear_stale_cached_voice_states, ConnectionIds}, State) ->
    handle_clear_stale_cached_voice_states_info(ConnectionIds, State);
handle_info_internal(flush_lazy_subscribe_buffer, State) ->
    guild_subscription_handler:handle_info(flush_lazy_subscribe_buffer, State);
handle_info_internal(flush_member_list_sync_batch, State) ->
    {noreply, guild_member_list_write:flush_pending_member_list_syncs(State)};
handle_info_internal({check_auto_stop_empty, Token}, State) ->
    handle_auto_stop_info(Token, State);
handle_info_internal(check_auto_stop_empty, State) ->
    {noreply, State};
handle_info_internal(Msg, State) ->
    handle_background_info(Msg, State).

-spec handle_background_info(term(), guild_state()) -> info_reply().
handle_background_info({thread_list_sync_tick, GuildIdToken}, State) ->
    {noreply, guild_thread_subscriptions:handle_tick(GuildIdToken, State)};
handle_background_info(thread_list_sync_drain, State) ->
    {noreply, guild_thread_subscriptions:handle_drain(State)};
handle_background_info(thread_member_list_flush, State) ->
    {noreply, guild_thread_subscriptions:handle_list_flush(State)};
handle_background_info(thread_load_retry, State) ->
    {noreply, guild_thread_load:handle_retry(State)};
handle_background_info({thread_load_result, Ref, Result}, State) when is_reference(Ref) ->
    {noreply, guild_thread_load:handle_load_result(Ref, Result, State)};
handle_background_info({thread_gate_flip, Version}, State) when is_integer(Version) ->
    {noreply, guild_thread_flip:start(Version, State)};
handle_background_info({thread_flip_result, Ref, Result}, State) when is_reference(Ref) ->
    {noreply, guild_thread_flip:handle_result(Ref, Result, State)};
handle_background_info({thread_flip_resend, SessionId}, State) when is_binary(SessionId) ->
    {noreply, guild_thread_flip:resend(SessionId, State)};
handle_background_info({thread_store_retire, Tab}, State) ->
    ok = retire_thread_store(Tab, State),
    {noreply, State};
handle_background_info({timeout, TimerRef, member_list_sync_item_cache_rotate}, State) when
    is_reference(TimerRef)
->
    ok = guild_member_list_subscribe:handle_sync_item_cache_timeout(TimerRef),
    {noreply, State};
handle_background_info(_, State) ->
    {noreply, State}.

-spec handle_session_connect_call(term(), pid(), guild_state()) -> call_reply().
handle_session_connect_call(Request, CallerPid, State) when is_map(Request) ->
    guild_sessions_connect:handle_session_connect(
        Request, session_connect_pid(Request, CallerPid), State
    ).

-spec session_connect_pid(map(), pid()) -> pid().
session_connect_pid(#{session_pid := Pid}, _CallerPid) when is_pid(Pid) ->
    Pid;
session_connect_pid(#{session_pid := Pid}, _CallerPid) ->
    erlang:error({bad_session_pid, Pid});
session_connect_pid(_Request, CallerPid) ->
    CallerPid.

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
    case guild_visitors:is_visitor_request(Request) of
        true -> handle_visitor_connect(GuildId, Attempt, Request, Msg, State);
        false -> enqueue_session_connect(GuildId, Attempt, Request, Msg, State)
    end.

-spec handle_visitor_connect(integer(), non_neg_integer(), map(), map(), guild_state()) ->
    cast_reply().
handle_visitor_connect(GuildId, Attempt, Request, Msg, State) ->
    UserId = maps:get(user_id, Request, undefined),
    Pid = maps:get(session_pid, Request, undefined),
    case {guild_permissions_check:find_member_by_user_id(UserId, State), Pid} of
        {_, SessionPid} when not is_pid(SessionPid) ->
            {noreply, State};
        {undefined, SessionPid} when is_pid(SessionPid) ->
            {Reply, NewState} = admit_visitor(Request, SessionPid, State),
            SessionPid ! {guild_connect_result, GuildId, Attempt, Reply},
            {noreply, NewState};
        {_Member, _SessionPid} ->
            enqueue_session_connect(
                GuildId, Attempt, guild_visitors:member_request(Request), Msg, State
            )
    end.

-spec admit_visitor(map(), pid(), guild_state()) -> {term(), guild_state()}.
admit_visitor(Request, SessionPid, State) ->
    case guild_visitors:admit(Request, SessionPid, State) of
        {ok, GuildState, S1} -> {{ok, self(), GuildState}, S1};
        {rejected, Reason, S1} -> {{error, {visit_rejected, Reason}}, S1}
    end.

-spec enqueue_session_connect(integer(), non_neg_integer(), map(), map(), guild_state()) ->
    cast_reply().
enqueue_session_connect(GuildId, Attempt, Request, Msg, State) ->
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
    {noreply,
        guild_sessions_passive:set_session_active_guild(
            SessionId, state_guild_id(State), State
        )}.

-spec handle_set_session_passive_cast(term(), guild_state()) -> cast_reply().
handle_set_session_passive_cast(SessionId, State) when is_binary(SessionId) ->
    {noreply,
        guild_sessions_passive:set_session_passive_guild(
            SessionId, state_guild_id(State), State
        )}.

-spec handle_set_session_typing_override_cast(term(), term(), guild_state()) -> cast_reply().
handle_set_session_typing_override_cast(SessionId, TypingFlag, State) when
    is_binary(SessionId), is_boolean(TypingFlag)
->
    {noreply, guild_sessions_passive:handle_set_typing_override(SessionId, TypingFlag, State)}.

-spec handle_send_guild_sync_cast(term(), guild_state()) -> cast_reply().
handle_send_guild_sync_cast(SessionId, State) when is_binary(SessionId) ->
    {noreply, guild_sessions_passive:handle_send_guild_sync(SessionId, State)}.

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
    maybe
        false ?= maps:is_key(Ref, WorkerRefs),
        not_visitor ?= guild_visitors:handle_down(Ref, State),
        guild_sessions:handle_session_down(Ref, State)
    else
        true -> handle_session_connect_worker_down(Ref, Reason, WorkerRefs, State);
        {handled, NewState} -> {noreply, NewState}
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
terminate(_Reason, State) when is_map(State) ->
    guild_cleanup:safe_cleanup(
        fun() -> guild_read_model:delete(maps:get(id, State, undefined)) end,
        "read_model_delete"
    ),
    guild_cleanup:safe_cleanup(
        fun() ->
            PresenceSubs = presence_subscriptions(State),
            lists:foreach(fun guild_cleanup:unsubscribe_presence/1, maps:keys(PresenceSubs))
        end,
        "presence_unsubscribe"
    ),
    guild_cleanup:safe_cleanup(
        fun() ->
            guild_maintenance:maybe_delete_permission_cache(
                maps:get(id, State, undefined), State
            )
        end,
        "permission_cache_delete"
    ),
    guild_cleanup:safe_cleanup(
        fun() ->
            guild_cleanup:cleanup_per_guild_ets(State)
        end,
        "ets_cleanup"
    ),
    guild_cleanup:safe_cleanup(
        fun() ->
            guild_cleanup:cleanup_passive_sync_registry(State)
        end,
        "passive_sync_cleanup"
    ),
    guild_cleanup:safe_cleanup(
        fun() ->
            guild_cleanup:cleanup_voice_server(State)
        end,
        "voice_cleanup"
    ),
    guild_cleanup:safe_cleanup(
        fun() ->
            guild_cleanup:cleanup_member_list_subs(State)
        end,
        "member_list_subs_cleanup"
    ),
    guild_cleanup:safe_cleanup(
        fun() ->
            guild_cleanup:cleanup_member_list_engine(State)
        end,
        "member_list_engine_cleanup"
    ),
    guild_cleanup:safe_cleanup(
        fun() ->
            guild_visitor_metrics:session_removed(guild_visitors:count(State))
        end,
        "visitor_metrics_cleanup"
    ),
    ok;
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), guild_state(), term()) -> {ok, guild_state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:process_flag(fullsweep_after, ?FULLSWEEP_AFTER),
    erlang:garbage_collect(),
    {ok, State}.

-spec retire_thread_store(term(), guild_state()) -> ok.
retire_thread_store(Tab, State) ->
    case maps:get(thread_store, maps:get(data, State, #{}), undefined) of
        Tab -> ok;
        _ -> guild_thread_store:destroy(eqwalizer:dynamic_cast(Tab))
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
    {noreply, DispatchedState} = guild_dispatch:handle_dispatch(
        eqwalizer:dynamic_cast(Event), ParsedEventData, State
    ),
    NewState = visitor_dispatch(Event, ParsedEventData, State, DispatchedState),
    StateAfterPrune = guild_maintenance:maybe_prune_invalid_member_subscriptions(
        Event, NewState
    ),
    ok = maybe_refresh_permission_cache(Event, ParsedEventData, State, StateAfterPrune),
    guild_dm_partners:maybe_reevaluate(Event, ParsedEventData, State, StateAfterPrune).

-spec visitor_dispatch(term(), map(), guild_state(), guild_state()) -> guild_state().
visitor_dispatch(Event, Data, OldState, NewState) when is_atom(Event); is_binary(Event) ->
    case guild_dispatch:normalize_event(Event) of
        Atom when is_atom(Atom) ->
            safe_visitor_dispatch(Atom, Data, OldState, NewState);
        _ ->
            NewState
    end;
visitor_dispatch(_Event, _Data, _OldState, NewState) ->
    NewState.

-spec safe_visitor_dispatch(atom(), map(), guild_state(), guild_state()) -> guild_state().
safe_visitor_dispatch(Event, Data, OldState, NewState) ->
    try
        guild_visitor_dispatch:after_dispatch(Event, Data, OldState, NewState)
    catch
        Class:Reason:Stack ->
            logger:error(
                "guild_visitor_dispatch_failed: guild_id=~p event=~p class=~p reason=~p "
                "stack=~p",
                [maps:get(id, NewState, undefined), Event, Class, Reason, Stack]
            ),
            evict_visitors_after_failure(NewState)
    end.

-spec evict_visitors_after_failure(guild_state()) -> guild_state().
evict_visitors_after_failure(State) ->
    try
        guild_visitors:evict_all(error, State)
    catch
        Class:Reason:Stack ->
            logger:error(
                "guild_visitor_eviction_failed: guild_id=~p class=~p reason=~p stack=~p",
                [maps:get(id, State, undefined), Class, Reason, Stack]
            ),
            State
    end.

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
    case map_size(sessions_map(State)) + guild_visitors:count(State) of
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
