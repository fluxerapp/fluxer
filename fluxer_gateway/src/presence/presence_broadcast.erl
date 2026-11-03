%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_broadcast).
-typing([eqwalizer]).

-export([
    publish_global_presence/2,
    force_publish_global_presence/1,
    dispatch_global_presence/3,
    dispatch_to_all_sessions/3,
    publish_user_update_to_bus/3,
    is_last_published_visible/1,
    cache_if_visible/2,
    current_visible_presence/1,
    normalize_group_dm_recipients/3
]).

-export_type([user_id/0, state/0, sessions/0]).

-type user_id() :: integer().
-type state() :: map().
-type sessions() :: #{binary() => map()}.

-define(CUSTOM_STATUS_EXPIRY_TIMER, custom_status_expiry_timer).

-spec publish_global_presence(sessions(), state()) -> state().
publish_global_presence(_Sessions, State0) ->
    State = refresh_custom_status_expiry_timer(State0),
    {Payload, CurrentExternal, ExternalStatus} = build_presence_external(State),
    LastPublished = maps:get(last_published_presence, State, undefined),
    case presence_changed(LastPublished, CurrentExternal) of
        true ->
            NewState = publish_presence_payload(
                State, Payload, CurrentExternal, ExternalStatus
            ),
            presence_update:maybe_update_push_eligibility(NewState);
        false ->
            update_cache_for_status(maps:get(user_id, State), ExternalStatus, Payload),
            presence_update:maybe_update_push_eligibility(State)
    end.

-spec force_publish_global_presence(state()) -> state().
force_publish_global_presence(State0) ->
    State = refresh_custom_status_expiry_timer(State0),
    {Payload, CurrentExternal, ExternalStatus} = build_presence_external(State),
    NewState = publish_presence_payload(State, Payload, CurrentExternal, ExternalStatus),
    presence_update:maybe_update_push_eligibility(NewState).

-spec refresh_custom_status_expiry_timer(state()) -> state().
refresh_custom_status_expiry_timer(State) ->
    ok = cancel_expiry_timer(maps:get(?CUSTOM_STATUS_EXPIRY_TIMER, State, undefined)),
    arm_expiry_timer(
        custom_status_expiry:next_wakeup_ms(maps:get(custom_status, State, null)), State
    ).

-spec arm_expiry_timer({ok, pos_integer()} | none, state()) -> state().
arm_expiry_timer(none, State) ->
    maps:remove(?CUSTOM_STATUS_EXPIRY_TIMER, State);
arm_expiry_timer({ok, DelayMs}, State) ->
    Ref = erlang:send_after(DelayMs, self(), custom_status_expiry:reconcile_message()),
    State#{?CUSTOM_STATUS_EXPIRY_TIMER => Ref}.

-spec cancel_expiry_timer(term()) -> ok.
cancel_expiry_timer(Ref) when is_reference(Ref) ->
    _ = erlang:cancel_timer(Ref, [{async, true}, {info, false}]),
    ok;
cancel_expiry_timer(_Ref) ->
    ok.

-spec dispatch_global_presence(user_id(), map(), state()) -> {noreply, state()}.
dispatch_global_presence(TargetId, Payload, State) ->
    UserId = maps:get(user_id, State),
    case TargetId =:= UserId of
        true -> {noreply, State};
        false -> dispatch_foreign_presence(TargetId, Payload, State)
    end.

-spec dispatch_to_all_sessions(atom(), map() | list(), state()) -> ok.
dispatch_to_all_sessions(EventAtom, Data, State) ->
    SessionPids = presence_connect:collect_session_pids(State),
    dispatch_with_backpressure(SessionPids, EventAtom, Data).

-spec dispatch_with_backpressure([pid()], atom(), map() | list()) -> ok.
dispatch_with_backpressure(SessionPids, EventAtom, Data) ->
    Msg = {dispatch, EventAtom, Data},
    lists:foreach(
        fun(Pid) -> safe_cast_or_shed(Pid, Msg) end,
        SessionPids
    ).

-spec safe_cast_or_shed(pid(), term()) -> ok.
safe_cast_or_shed(Pid, Msg) ->
    case shard_utils:safe_cast(Pid, Msg) of
        ok ->
            ok;
        {error, overloaded} ->
            ok
    end.

-spec publish_user_update_to_bus(user_id(), map(), state()) -> ok.
publish_user_update_to_bus(UserId, UserData, State) ->
    LastPublished = maps:get(last_published_presence, State, undefined),
    case is_last_published_visible(LastPublished) of
        true ->
            NormalizedUserData = user_utils:normalize_user(UserData),
            Payload = #{<<"user">> => NormalizedUserData, <<"user_update">> => true},
            presence_bus:publish(UserId, Payload);
        false ->
            ok
    end.

-spec is_last_published_visible(map() | undefined) -> boolean().
is_last_published_visible(undefined) -> false;
is_last_published_visible(#{status := <<"online">>}) -> true;
is_last_published_visible(#{status := <<"idle">>}) -> true;
is_last_published_visible(#{status := <<"dnd">>}) -> true;
is_last_published_visible(_) -> false.

-spec cache_if_visible(term(), map()) -> ok.
cache_if_visible(UserId, Payload) when is_integer(UserId), is_map(Payload) ->
    case maps:get(<<"status">>, Payload, <<"offline">>) of
        <<"offline">> -> ok;
        <<"invisible">> -> ok;
        _ -> presence_cache:put(UserId, Payload)
    end;
cache_if_visible(_, _) ->
    ok.

-spec current_visible_presence(state()) -> {ok, map()} | not_found.
current_visible_presence(State) ->
    {Payload, _CurrentExternal, ExternalStatus} = build_presence_external(State),
    case ExternalStatus of
        <<"offline">> -> not_found;
        <<"invisible">> -> not_found;
        _ -> {ok, Payload}
    end.

-spec normalize_group_dm_recipients(map(), user_id(), boolean()) ->
    #{integer() => #{user_id() => true}}.
normalize_group_dm_recipients(_RecipientsByChannel, _UserId, true) ->
    #{};
normalize_group_dm_recipients(RecipientsByChannel, UserId, IncludeBot) ->
    presence_broadcast_subscriptions:normalize_group_dm_recipients(
        RecipientsByChannel, UserId, IncludeBot
    ).

-spec build_presence_external(state()) -> {map(), map(), binary()}.
build_presence_external(State) ->
    Payload = build_presence_payload(State),
    ExternalStatus = maps:get(<<"status">>, Payload, <<"offline">>),
    CurrentExternal = #{
        status => ExternalStatus,
        mobile => maps:get(<<"mobile">>, Payload, false),
        afk => maps:get(<<"afk">>, Payload, false),
        custom_status => maps:get(<<"custom_status">>, Payload, null)
    },
    {Payload, CurrentExternal, ExternalStatus}.

-spec publish_presence_payload(state(), map(), map(), binary()) -> state().
publish_presence_payload(State, Payload, CurrentExternal, ExternalStatus) ->
    UserId = maps:get(user_id, State),
    update_cache_for_status(UserId, ExternalStatus, Payload),
    presence_bus:publish(UserId, Payload),
    State#{last_published_presence := CurrentExternal}.

-spec update_cache_for_status(user_id(), binary(), map()) -> ok.
update_cache_for_status(UserId, <<"offline">>, _Payload) ->
    presence_cache:delete(UserId);
update_cache_for_status(UserId, _Status, Payload) ->
    presence_cache:put(UserId, Payload).

-spec presence_changed(map() | undefined, map()) -> boolean().
presence_changed(undefined, _Current) -> true;
presence_changed(Last, Current) -> Last =/= Current.

-spec build_presence_payload(state()) -> map().
build_presence_payload(State) ->
    Sessions = presence_status:effective_sessions(State),
    Status = presence_status:get_current_status(Sessions),
    Mobile = presence_status:get_flattened_mobile(Sessions),
    Afk = presence_status:get_flattened_afk(Sessions),
    UserData = maps:get(user_data, State, #{}),
    CustomStatus = maps:get(custom_status, State, null),
    presence_payload:build(UserData, Status, Mobile, Afk, CustomStatus).

-spec dispatch_foreign_presence(user_id(), map(), state()) -> {noreply, state()}.
dispatch_foreign_presence(TargetId, Payload, State) ->
    case maps:get(<<"user_update">>, Payload, false) of
        true ->
            dispatch_global_user_update(TargetId, Payload, State);
        false ->
            cache_if_visible(TargetId, Payload),
            dispatch_to_sessions(Payload, State),
            {noreply, State}
    end.

-spec dispatch_global_user_update(user_id(), map(), state()) -> {noreply, state()}.
dispatch_global_user_update(TargetId, Payload, State) ->
    NewUserData = maps:get(<<"user">>, Payload, #{}),
    case presence_cache_safe:get(TargetId) of
        {ok, CachedPresence} ->
            MergedPresence = CachedPresence#{<<"user">> => NewUserData},
            presence_cache:put(TargetId, MergedPresence),
            dispatch_to_sessions(MergedPresence, State),
            {noreply, State};
        _ ->
            {noreply, State}
    end.

-spec dispatch_to_sessions(map(), state()) -> ok.
dispatch_to_sessions(Payload, State) ->
    SessionPids = presence_connect:collect_session_pids(State),
    gateway_dispatch_relay:dispatch_many(SessionPids, presence_update, Payload).
