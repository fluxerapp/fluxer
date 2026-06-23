%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_presence_reconcile).
-typing([eqwalizer]).

-export([
    schedule/0,
    interval_ms/0,
    start_async/1,
    reconcile_user/2,
    maybe_schedule_user_repair/2,
    apply_reconcile_result/2,
    connected_user_ids_list/1,
    reconcile_action/3
]).

-export_type([guild_state/0, user_id/0, presence/0, presence_by_id/0]).

-type guild_state() :: map().
-type user_id() :: integer().
-type presence() :: map().
-type presence_by_id() :: #{user_id() => presence()}.
-type display() :: {binary(), boolean(), boolean(), term()}.

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
        UserIds -> spawn_reconcile_fetch(UserIds, self())
    end.

-spec spawn_reconcile_fetch([user_id(), ...], pid()) -> ok.
spawn_reconcile_fetch(UserIds, Parent) ->
    _ = spawn(fun() -> fetch_and_reply(UserIds, Parent) end),
    ok.

-spec fetch_and_reply([user_id(), ...], pid()) -> ok.
fetch_and_reply(UserIds, Parent) ->
    Parent ! {presence_reconcile_apply, authoritative_presence_map(UserIds)},
    ok.

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

-spec apply_reconcile_result(map(), guild_state()) -> guild_state().
apply_reconcile_result(PresenceById, State) when is_map(PresenceById) ->
    lists:foldl(
        fun(UserId, Acc) ->
            apply_user_reconcile(UserId, lookup_presence_value(UserId, PresenceById), Acc)
        end,
        State,
        connected_user_ids_list(State)
    );
apply_reconcile_result(_PresenceById, State) ->
    State.

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
    case maps:get(member_presence, State, undefined) of
        undefined -> offline_display();
        Tab -> display_fields(guild_state_member:lookup_presence(Tab, UserId))
    end.

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

interval_clamps_below_minimum_test() ->
    application:set_env(fluxer_gateway, guild_presence_reconcile_interval_ms, 10),
    try
        ?assertEqual(?DEFAULT_INTERVAL_MS, interval_ms())
    after
        application:unset_env(fluxer_gateway, guild_presence_reconcile_interval_ms)
    end.

interval_uses_configured_value_test() ->
    application:set_env(fluxer_gateway, guild_presence_reconcile_interval_ms, 12000),
    try
        ?assertEqual(12000, interval_ms())
    after
        application:unset_env(fluxer_gateway, guild_presence_reconcile_interval_ms)
    end.

reconcile_action_noop_when_already_online_test() ->
    State = state_with_presence(#{1 => dnd_presence(1)}),
    ?assertEqual(noop, reconcile_action(1, dnd_presence(1), State)).

reconcile_action_replays_when_member_presence_missing_test() ->
    State = state_with_presence(#{}),
    ?assertMatch({replay, _}, reconcile_action(1, dnd_presence(1), State)).

reconcile_action_replays_offline_when_stale_online_test() ->
    State = state_with_presence(#{1 => dnd_presence(1)}),
    ?assertEqual(
        {replay, #{<<"status">> => <<"offline">>}}, reconcile_action(1, undefined, State)
    ).

reconcile_action_noop_when_already_offline_test() ->
    State = state_with_presence(#{}),
    ?assertEqual(noop, reconcile_action(1, undefined, State)).

connected_user_ids_list_test() ->
    State = #{connected_user_ids => sets:from_list([1, 2, 3])},
    ?assertEqual([1, 2, 3], lists:sort(connected_user_ids_list(State))).

presence_user_id_test() ->
    ?assertEqual(7, presence_user_id(#{<<"user">> => #{<<"id">> => 7}})),
    ?assertEqual(7, presence_user_id(#{<<"user">> => #{<<"id">> => <<"7">>}})),
    ?assertEqual(undefined, presence_user_id(#{})).

apply_reconcile_result_repairs_connected_offline_member_test() ->
    GuildId = 4242,
    UserId = 99,
    Engine = guild_member_list_engine:new(),
    try
        State = guild_test_state(GuildId, UserId, Engine),
        ok = guild_member_list_engine:bulk_load(
            Engine, [{UserId, <<"hampus">>, [], false}], []
        ),
        ?assertEqual({1, 0}, guild_member_list_engine:get_counts(Engine)),
        NewState = apply_reconcile_result(#{UserId => dnd_presence(UserId)}, State),
        ?assertEqual({1, 1}, guild_member_list_engine:get_counts(Engine)),
        Row = guild_state_member:lookup_presence(maps:get(member_presence, NewState), UserId),
        ?assertEqual(<<"dnd">>, maps:get(<<"status">>, Row))
    after
        guild_member_list_engine:destroy(Engine)
    end.

apply_reconcile_result_is_noop_when_consistent_test() ->
    GuildId = 4242,
    UserId = 99,
    Engine = guild_member_list_engine:new(),
    try
        State = guild_test_state(GuildId, UserId, Engine),
        ets:insert(maps:get(member_presence, State), {UserId, dnd_presence(UserId)}),
        ok = guild_member_list_engine:bulk_load(
            Engine, [{UserId, <<"hampus">>, [], true}], []
        ),
        ?assertEqual({1, 1}, guild_member_list_engine:get_counts(Engine)),
        _ = apply_reconcile_result(#{UserId => dnd_presence(UserId)}, State),
        ?assertEqual({1, 1}, guild_member_list_engine:get_counts(Engine))
    after
        guild_member_list_engine:destroy(Engine)
    end.

apply_reconcile_result_marks_stale_online_offline_test() ->
    GuildId = 4242,
    UserId = 99,
    Engine = guild_member_list_engine:new(),
    try
        State = guild_test_state(GuildId, UserId, Engine),
        ets:insert(maps:get(member_presence, State), {UserId, dnd_presence(UserId)}),
        ok = guild_member_list_engine:bulk_load(
            Engine, [{UserId, <<"hampus">>, [], true}], []
        ),
        ?assertEqual({1, 1}, guild_member_list_engine:get_counts(Engine)),
        _ = apply_reconcile_result(#{}, State),
        ?assertEqual({1, 0}, guild_member_list_engine:get_counts(Engine))
    after
        guild_member_list_engine:destroy(Engine)
    end.

guild_test_state(GuildId, UserId, Engine) ->
    Data = guild_data_index:normalize_data(#{
        <<"guild">> => #{<<"owner_id">> => integer_to_binary(UserId)},
        <<"roles">> => [
            #{
                <<"id">> => integer_to_binary(GuildId),
                <<"name">> => <<"everyone">>,
                <<"hoist">> => false,
                <<"position">> => 0,
                <<"permissions">> => <<"0">>
            }
        ],
        <<"members">> => [
            #{
                <<"user">> => #{
                    <<"id">> => integer_to_binary(UserId),
                    <<"username">> => <<"hampus">>
                },
                <<"roles">> => []
            }
        ],
        <<"channels">> => []
    }),
    #{
        id => GuildId,
        data => Data,
        sessions => #{},
        member_presence => ets:new(test_member_presence, [set, public]),
        connected_user_ids => sets:from_list([UserId]),
        member_list_engine => Engine,
        channel_member_list_engines => #{},
        member_list_subscriptions => guild_member_list_subs:new(),
        member_subscriptions => guild_subscriptions:init_state(),
        presence_subscriptions => #{UserId => 1}
    }.

state_with_presence(PresenceMap) ->
    Tab = ets:new(test_member_presence, [set, public]),
    maps:foreach(fun(UserId, Presence) -> ets:insert(Tab, {UserId, Presence}) end, PresenceMap),
    #{
        member_presence => Tab,
        connected_user_ids => sets:from_list(maps:keys(PresenceMap))
    }.

dnd_presence(UserId) ->
    #{
        <<"status">> => <<"dnd">>,
        <<"mobile">> => false,
        <<"afk">> => false,
        <<"custom_status">> => null,
        <<"user">> => #{<<"id">> => UserId}
    }.

-endif.
