%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_presence_reconcile).
-typing([eqwalizer]).

-export([
    schedule/0,
    start_async/1,
    reconcile_user/2,
    maybe_schedule_user_repair/2,
    apply_mismatches/2
]).

-export_type([guild_state/0, user_id/0, mismatch/0]).

-type guild_state() :: map().
-type user_id() :: integer().
-type presence() :: map().
-type presence_by_id() :: #{user_id() => presence()}.
-type display() :: {binary(), boolean(), boolean(), term()}.
-type mismatch() :: {user_id(), presence(), display()}.

-define(DEFAULT_INTERVAL_MS, 30000).
-define(MIN_INTERVAL_MS, 5000).
-define(REPAIR_DELAY_MS, 1500).

-spec schedule() -> reference().
schedule() ->
    erlang:send_after(interval_ms(), self(), presence_reconcile).

-spec interval_ms() -> pos_integer().
interval_ms() ->
    case application:get_env(fluxer_gateway, guild_presence_reconcile_interval_ms) of
        {ok, N} when is_integer(N), N >= ?MIN_INTERVAL_MS -> N;
        _ -> ?DEFAULT_INTERVAL_MS
    end.

-spec start_async(guild_state()) -> ok.
start_async(State) ->
    case connected_user_ids_list(State) of
        [] -> ok;
        UserIds -> spawn_reconcile_fetch(UserIds, member_presence_tab(State), self())
    end.

-spec spawn_reconcile_fetch([user_id(), ...], ets:tid() | undefined, pid()) -> ok.
spawn_reconcile_fetch(UserIds, Tab, Parent) ->
    _ = spawn(fun() -> fetch_and_reply(UserIds, Tab, Parent) end),
    ok.

-spec fetch_and_reply([user_id(), ...], ets:tid() | undefined, pid()) -> ok.
fetch_and_reply(UserIds, Tab, Parent) ->
    Parent ! {presence_reconcile_apply, persistent_mismatches(UserIds, Tab, ?REPAIR_DELAY_MS)},
    ok.

-spec persistent_mismatches([user_id()], ets:tid() | undefined, non_neg_integer()) ->
    [mismatch()].
persistent_mismatches(UserIds, Tab, ConfirmDelayMs) ->
    case observe_mismatches(UserIds, Tab) of
        [] ->
            [];
        First ->
            timer:sleep(ConfirmDelayMs),
            Second = observe_mismatches([UserId || {UserId, _, _} <- First], Tab),
            confirmed_mismatches(First, Second)
    end.

-spec observe_mismatches([user_id()], ets:tid() | undefined) -> [mismatch()].
observe_mismatches(UserIds, Tab) ->
    Authoritative = authoritative_presence_map(UserIds),
    try
        find_mismatches(UserIds, Authoritative, Tab)
    catch
        error:badarg -> []
    end.

-spec find_mismatches([user_id()], presence_by_id(), ets:tid() | undefined) -> [mismatch()].
find_mismatches(UserIds, Authoritative, Tab) ->
    lists:filtermap(
        fun(UserId) ->
            Seen = tab_display(UserId, Tab),
            Presence = lookup_presence_value(UserId, Authoritative),
            desired_display(Presence) =/= Seen andalso
                {true, {UserId, replay_payload(Presence), Seen}}
        end,
        UserIds
    ).

-spec confirmed_mismatches([mismatch()], [mismatch()]) -> [mismatch()].
confirmed_mismatches(First, Second) ->
    Keys = sets:from_list([mismatch_key(M) || M <- First], [{version, 2}]),
    [M || M <- Second, sets:is_element(mismatch_key(M), Keys)].

-spec mismatch_key(mismatch()) -> {user_id(), display(), display()}.
mismatch_key({UserId, Payload, Seen}) ->
    {UserId, display_fields(Payload), Seen}.

-spec apply_mismatches([mismatch()], guild_state()) -> guild_state().
apply_mismatches(Mismatches, State) ->
    Connected = guild_member_list_connected:connected_session_user_ids(State),
    lists:foldl(
        fun(Mismatch, Acc) -> replay_if_unchanged(Mismatch, Connected, Acc) end,
        State,
        Mismatches
    ).

-spec replay_if_unchanged(mismatch(), sets:set(user_id()), guild_state()) -> guild_state().
replay_if_unchanged({UserId, Payload, Seen}, Connected, State) ->
    case sets:is_element(UserId, Connected) andalso current_display(UserId, State) =:= Seen of
        true -> replay_presence(UserId, Payload, State);
        false -> State
    end.

-spec member_presence_tab(guild_state()) -> ets:tid() | undefined.
member_presence_tab(State) ->
    case maps:get(member_presence, State, undefined) of
        Tab when is_reference(Tab) -> Tab;
        _ -> undefined
    end.

-spec tab_display(user_id(), ets:tid() | undefined) -> display().
tab_display(_UserId, undefined) ->
    offline_display();
tab_display(UserId, Tab) ->
    display_fields(guild_state_member:lookup_presence(Tab, UserId)).

-spec reconcile_user(term(), guild_state()) -> guild_state().
reconcile_user(UserId, State) when is_integer(UserId), UserId > 0 ->
    case is_connected(UserId, State) of
        false -> State;
        true -> apply_user_reconcile(UserId, authoritative_presence(UserId), State)
    end;
reconcile_user(_UserId, State) ->
    State.

-spec maybe_schedule_user_repair(user_id(), guild_state()) -> ok.
maybe_schedule_user_repair(UserId, State) ->
    case has_member_presence_row(UserId, State) of
        true -> ok;
        false -> schedule_user_repair(UserId)
    end.

-spec schedule_user_repair(user_id()) -> ok.
schedule_user_repair(UserId) ->
    _ = erlang:send_after(?REPAIR_DELAY_MS, self(), {reconcile_user_presence, UserId}),
    ok.

-spec has_member_presence_row(user_id(), guild_state()) -> boolean().
has_member_presence_row(UserId, State) ->
    case maps:get(member_presence, State, undefined) of
        Tab when is_reference(Tab); is_atom(Tab) -> presence_row_exists(Tab, UserId);
        _ -> false
    end.

-spec presence_row_exists(ets:table(), user_id()) -> boolean().
presence_row_exists(Tab, UserId) ->
    try ets:lookup(Tab, UserId) of
        [] -> false;
        [_ | _] -> true
    catch
        error:badarg -> false
    end.

-spec lookup_presence_value(user_id(), map()) -> presence() | undefined.
lookup_presence_value(UserId, PresenceById) ->
    case maps:get(UserId, PresenceById, undefined) of
        Presence when is_map(Presence) -> Presence;
        _ -> undefined
    end.

-spec apply_user_reconcile(user_id(), presence() | undefined, guild_state()) -> guild_state().
apply_user_reconcile(UserId, Authoritative, State) ->
    case reconcile_action(UserId, Authoritative, State) of
        noop -> State;
        {replay, Payload} -> replay_presence(UserId, Payload, State)
    end.

-spec reconcile_action(user_id(), presence() | undefined, guild_state()) ->
    noop | {replay, presence()}.
reconcile_action(UserId, Authoritative, State) ->
    Desired = desired_display(Authoritative),
    Current = current_display(UserId, State),
    case Desired =:= Current of
        true -> noop;
        false -> {replay, replay_payload(Authoritative)}
    end.

-spec replay_presence(user_id(), presence(), guild_state()) -> guild_state().
replay_presence(UserId, Payload, State) ->
    {noreply, NewState} = guild_presence:handle_bus_presence(UserId, Payload, State),
    NewState.

-spec replay_payload(presence() | undefined) -> presence().
replay_payload(undefined) -> #{<<"status">> => <<"offline">>};
replay_payload(Payload) -> Payload.

-spec desired_display(presence() | undefined) -> display().
desired_display(undefined) -> offline_display();
desired_display(Payload) -> display_fields(Payload).

-spec current_display(user_id(), guild_state()) -> display().
current_display(UserId, State) ->
    tab_display(UserId, member_presence_tab(State)).

-spec display_fields(presence()) -> display().
display_fields(Presence) ->
    {
        normalize_status(maps:get(<<"status">>, Presence, <<"offline">>)),
        maps:get(<<"mobile">>, Presence, false),
        maps:get(<<"afk">>, Presence, false),
        maps:get(<<"custom_status">>, Presence, null)
    }.

-spec offline_display() -> display().
offline_display() -> {<<"offline">>, false, false, null}.

-spec normalize_status(term()) -> binary().
normalize_status(<<"invisible">>) -> <<"offline">>;
normalize_status(Status) when is_binary(Status) -> Status;
normalize_status(_) -> <<"offline">>.

-spec is_connected(user_id(), guild_state()) -> boolean().
is_connected(UserId, State) ->
    sets:is_element(UserId, guild_member_list_connected:connected_session_user_ids(State)).

-spec connected_user_ids_list(guild_state()) -> [user_id()].
connected_user_ids_list(State) ->
    Set = guild_member_list_connected:connected_session_user_ids(State),
    [UserId || UserId <- sets:to_list(Set), is_integer(UserId), UserId > 0].

-spec authoritative_presence(user_id()) -> presence() | undefined.
authoritative_presence(UserId) ->
    try presence_cache:get(UserId) of
        {ok, Payload} when is_map(Payload) -> Payload;
        _ -> undefined
    catch
        error:_ -> undefined;
        exit:_ -> undefined
    end.

-spec authoritative_presence_map([user_id(), ...]) -> presence_by_id().
authoritative_presence_map(UserIds) ->
    lists:foldl(fun add_presence_to_map/2, #{}, safe_bulk_get(UserIds)).

-spec add_presence_to_map(presence(), presence_by_id()) -> presence_by_id().
add_presence_to_map(Presence, Acc) ->
    case presence_user_id(Presence) of
        Uid when is_integer(Uid), Uid > 0 -> Acc#{Uid => Presence};
        _ -> Acc
    end.

-spec safe_bulk_get([user_id()]) -> [presence()].
safe_bulk_get(UserIds) ->
    try presence_cache:bulk_get(UserIds) of
        Presences -> Presences
    catch
        error:_ -> [];
        exit:_ -> []
    end.

-spec presence_user_id(presence()) -> user_id() | undefined.
presence_user_id(Presence) ->
    case maps:get(<<"user">>, Presence, undefined) of
        UserMap when is_map(UserMap) ->
            snowflake_id:parse_optional(maps:get(<<"id">>, UserMap, undefined));
        _ ->
            undefined
    end.
