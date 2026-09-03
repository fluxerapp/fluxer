%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_init).
-typing([eqwalizer]).

-export([
    init_base_state/1,
    init_member_list/1,
    init_counts/1,
    init_caches_and_timers/1,
    init_voice_server/1,
    extract_voice_states_from_data/2,
    handle_reload/2
]).

-define(MAX_RELOAD_CARRY_WARN_MEMBERS, 100000).

-type guild_state() :: map().
-type user_id() :: integer().
-type member() :: map().

-export_type([guild_state/0]).

-spec init_base_state(map()) -> guild_state().
init_base_state(GuildState) ->
    TransferSafe = guild_handoff:remonitor_transferred_sessions(GuildState),
    Data0 = maps:get(data, TransferSafe, #{}),
    ExistingVoice = maps:get(voice_states, TransferSafe, #{}),
    {VoiceStates, Data1} = extract_voice_states_from_data(Data0, ExistingVoice),
    NormalizedData = guild_data_index:normalize_map(Data1),
    MemberTab = ets:new(guild_members_data, [set, public, {read_concurrency, true}]),
    populate_member_ets(MemberTab, NormalizedData),
    BaseState = TransferSafe#{
        data => NormalizedData#{members_ets => MemberTab},
        voice_states => VoiceStates,
        presence_subscriptions => #{},
        member_list_subscriptions => guild_member_list_subs:new(),
        member_subscriptions => guild_subscriptions:init_state(),
        member_presence => ets:new(member_presence, [set, public]),
        connected_user_ids => sets:new(),
        user_session_counts => #{},
        viewable_channels_cache => ets:new(viewable_channels_cache, [set, public])
    },
    guild_handoff:restore_transferred_session_state(BaseState).

-spec populate_member_ets(ets:table(), map()) -> ok.
populate_member_ets(Tab, Data) ->
    MemberMap =
        case maps:get(members_normalized, Data, undefined) of
            M when is_map(M) -> M;
            _ -> guild_data_index_members:member_map(Data)
        end,
    maps:foreach(
        fun(UserId, Member) ->
            ets:insert(Tab, {UserId, Member})
        end,
        MemberMap
    ).

-spec init_member_list(guild_state()) -> guild_state().
init_member_list(State) ->
    Data = maps:get(data, State, #{}),
    case guild_id(State) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            NifRef = guild_member_list_store:new(GuildId),
            Roles = map_utils:ensure_list(maps:get(<<"roles">>, Data, [])),
            MemberMap = guild_data_index:member_map(Data),
            MemberTuples = guild_member_list_store:prepare_member_tuples(MemberMap, State),
            HoistedRoleIds = guild_member_list_store:prepare_hoisted_role_ids(Roles, GuildId),
            ok = guild_member_list_store:bulk_load(NifRef, MemberTuples, HoistedRoleIds),
            State#{member_list_engine => NifRef};
        _ ->
            State
    end.

-spec init_counts(guild_state()) -> guild_state().
init_counts(State) ->
    Data = maps:get(data, State, #{}),
    MemberCount =
        case maps:get(member_count, State, undefined) of
            N when is_integer(N), N >= 0 -> N;
            _ -> guild_data_index:member_count(Data)
        end,
    State1 = State#{member_count => MemberCount},
    OnlineCount = guild_member_list:get_online_count(State1),
    State2 = State1#{online_count => OnlineCount},
    PublicOnlineCount = guild_public_online:compute_count(State2),
    State2#{public_online_count => PublicOnlineCount}.

-spec init_caches_and_timers(guild_state()) -> guild_state().
init_caches_and_timers(State) ->
    MemberCount = maps:get(member_count, State, 0),
    PublicOnlineCount = maps:get(public_online_count, State, 0),
    ok = guild_maintenance:maybe_put_permission_cache(State),
    _ = guild_availability:update_unavailability_cache_for_state(State),
    ok = guild_maintenance:maybe_put_guild_count_cache(State, MemberCount, PublicOnlineCount),
    _ = guild_passive_sync:schedule_passive_sync(State),
    _ = guild_maintenance:schedule_count_cache_refresh(State),
    _ = guild_availability:schedule_availability_recheck(State),
    _ = guild_presence_reconcile:schedule(),
    State.

-spec init_voice_server(guild_state()) -> guild_state().
init_voice_server(State) ->
    GuildId = maps:get(id, State),
    InitialVoice = maps:get(voice_states, State, #{}),
    {ok, VoicePid} = guild_voice_server:start_link(GuildId, self(), InitialVoice),
    State#{voice_server_pid => VoicePid}.

-spec extract_voice_states_from_data(map(), map()) -> {map(), map()}.
extract_voice_states_from_data(Data, Fallback) ->
    case maps:find(<<"voice_states">>, Data) of
        {ok, VoiceStatesCollection} ->
            {
                normalize_voice_states_collection(VoiceStatesCollection, Fallback),
                maps:remove(<<"voice_states">>, Data)
            };
        error ->
            {voice_state_utils:ensure_voice_states(Fallback), Data}
    end.

-spec handle_reload(map(), guild_state()) -> {reply, ok, guild_state()}.
handle_reload(NewData, State) ->
    OldData = maps:get(data, State),
    ExistingVoiceStates = maps:get(voice_states, State, #{}),
    {ReloadVoiceStates, ReloadData} = extract_voice_states_from_data(
        NewData, ExistingVoiceStates
    ),
    NormalizedNewData0 = guild_data_index:normalize_map(ReloadData),
    NormalizedNewData = carry_members_table(OldData, NormalizedNewData0),
    NewState0 = State#{voice_states => ReloadVoiceStates, data => NormalizedNewData},
    NewState1 = guild_availability:handle_unavailability_transition(State, NewState0),
    NewState2 = guild_sessions:refresh_all_viewable_channels(NewState1),
    GuildId = maps:get(id, State),
    NewGuild = maps:get(<<"guild">>, NormalizedNewData, #{}),
    Sessions = maps:get(sessions, NewState2, #{}),
    Pids = maps:fold(fun collect_active_pid/3, [], Sessions),
    GuildIdBin = integer_to_binary(GuildId),
    EventData = NewGuild#{<<"guild_id">> => GuildIdBin},
    gateway_dispatch_relay:dispatch_many(Pids, guild_update, EventData, GuildId),
    NewState = guild_maintenance:cleanup_removed_member_subscriptions(
        OldData, NormalizedNewData, NewState2
    ),
    ok = guild_maintenance:maybe_put_permission_cache(NewState),
    {reply, ok, NewState}.

-spec carry_members_table(term(), term()) -> term().
carry_members_table(OldData, NewData) when is_map(OldData), is_map(NewData) ->
    carry_healthy_members_table(OldData, NewData);
carry_members_table(_OldData, NewData) ->
    NewData.

-spec carry_healthy_members_table(map(), map()) -> map().
carry_healthy_members_table(OldData, NewData) ->
    case members_ets_table(OldData) of
        Tab when is_reference(Tab) -> carry_live_members_table(Tab, OldData, NewData);
        undefined -> maps:remove(members_ets, NewData)
    end.

-spec members_ets_table(map()) -> ets:tid() | undefined.
members_ets_table(#{members_ets := Tab}) ->
    Tab;
members_ets_table(_Data) ->
    undefined.

-spec carry_live_members_table(ets:tid(), map(), map()) -> map().
carry_live_members_table(Tab, OldData, NewData) ->
    case guild_members_table_repair:members_table_healthy(Tab) of
        true -> apply_members_table_delta(Tab, OldData, NewData);
        false -> maps:remove(members_ets, NewData)
    end.

-spec apply_members_table_delta(ets:tid(), map(), map()) -> map().
apply_members_table_delta(Tab, OldData, NewData) ->
    OldMap = guild_data_index_members:member_map(OldData),
    NewMap = guild_data_index_members:member_map(NewData),
    maybe_warn_large_carry(map_size(NewMap)),
    try
        ok = insert_changed_members(Tab, OldMap, NewMap),
        ok = delete_removed_members(Tab, OldMap, NewMap),
        NewData#{members_ets => Tab}
    catch
        error:badarg ->
            logger:warning(
                "guild_reload_members_table_carry_failed: members=~p", [map_size(NewMap)]
            ),
            maps:remove(members_ets, NewData)
    end.

-spec insert_changed_members(ets:tid(), map(), map()) -> ok.
insert_changed_members(Tab, OldMap, NewMap) ->
    maps:foreach(
        fun(UserId, Member) -> insert_changed_member(Tab, OldMap, UserId, Member) end,
        NewMap
    ).

-spec insert_changed_member(ets:tid(), map(), user_id(), member()) -> ok.
insert_changed_member(Tab, OldMap, UserId, Member) ->
    case maps:get(UserId, OldMap, undefined) of
        Member -> ok;
        _ -> insert_member_row(Tab, UserId, Member)
    end.

-spec insert_member_row(ets:tid(), user_id(), member()) -> ok.
insert_member_row(Tab, UserId, Member) ->
    true = ets:insert(Tab, {UserId, Member}),
    ok.

-spec delete_removed_members(ets:tid(), map(), map()) -> ok.
delete_removed_members(Tab, OldMap, NewMap) ->
    OldIds = sets:from_list(maps:keys(OldMap)),
    NewIds = sets:from_list(maps:keys(NewMap)),
    RemovedIds = sets:to_list(sets:subtract(OldIds, NewIds)),
    lists:foreach(fun(UserId) -> delete_member_row(Tab, UserId) end, RemovedIds).

-spec delete_member_row(ets:tid(), user_id()) -> ok.
delete_member_row(Tab, UserId) ->
    true = ets:delete(Tab, UserId),
    ok.

-spec maybe_warn_large_carry(non_neg_integer()) -> ok.
maybe_warn_large_carry(Size) when Size > ?MAX_RELOAD_CARRY_WARN_MEMBERS ->
    logger:warning(
        "guild_reload_members_table_carry_large: members=~p threshold=~p",
        [Size, ?MAX_RELOAD_CARRY_WARN_MEMBERS]
    );
maybe_warn_large_carry(_Size) ->
    ok.

-spec collect_active_pid(term(), map(), [pid()]) -> [pid()].
collect_active_pid(_Sid, S, Acc) ->
    case maps:get(pending_connect, S, false) of
        true -> Acc;
        _ -> [maps:get(pid, S) | Acc]
    end.

-spec normalize_voice_states_collection(term(), map()) -> map().
normalize_voice_states_collection(Collection, _Fallback) when is_list(Collection) ->
    lists:foldl(fun maybe_index_voice_state/2, #{}, Collection);
normalize_voice_states_collection(Collection, _Fallback) when is_map(Collection) ->
    Collection;
normalize_voice_states_collection(_Collection, Fallback) ->
    voice_state_utils:ensure_voice_states(Fallback).

-spec maybe_index_voice_state(term(), map()) -> map().
maybe_index_voice_state(VoiceState, Acc) when is_map(VoiceState) ->
    case maps:get(<<"connection_id">>, VoiceState, undefined) of
        ConnectionId when is_binary(ConnectionId) ->
            Acc#{ConnectionId => VoiceState};
        _ ->
            Acc
    end;
maybe_index_voice_state(_, Acc) ->
    Acc.

-spec guild_id(guild_state()) -> integer() | undefined.
guild_id(State) ->
    snowflake_id:parse_optional(maps:get(id, State, undefined)).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

carry_members_table_carries_healthy_tid_test() ->
    Tab = ets:new(carry_members, [set, public]),
    try
        OldMap = #{1 => carry_member(1), 2 => carry_member(2)},
        NewMap = #{1 => carry_member(1), 3 => carry_member(3)},
        seed_carry_table(Tab, OldMap),
        OldData = carry_data(OldMap, Tab),
        Carried = carry_members_table(OldData, carry_data(NewMap, undefined)),
        ?assertEqual(Tab, maps:get(members_ets, Carried)),
        ?assertEqual([1, 3], carry_table_ids(Tab))
    after
        ets:delete(Tab)
    end.

carry_members_table_skips_dead_tid_test() ->
    Tab = ets:new(dead_carry_members, [set, public]),
    true = ets:delete(Tab),
    OldData = carry_data(#{1 => carry_member(1)}, Tab),
    NewData = carry_data(#{1 => carry_member(1)}, undefined),
    ?assertEqual(NewData, carry_members_table(OldData, NewData)).

carry_members_table_without_old_tid_leaves_new_data_test() ->
    OldData = carry_data(#{1 => carry_member(1)}, undefined),
    NewData = carry_data(#{2 => carry_member(2)}, undefined),
    ?assertEqual(NewData, carry_members_table(OldData, NewData)).

carry_members_table_delta_failure_falls_back_to_new_data_test() ->
    Tab = ets:new(failing_members, [set, public]),
    true = ets:delete(Tab),
    OldData = carry_data(#{}, undefined),
    NewData = carry_data(#{1 => carry_member(1)}, undefined),
    ?assertEqual(NewData, apply_members_table_delta(Tab, OldData, NewData)).

carry_delta_never_empties_table_test() ->
    Tab = ets:new(delta_members, [set, public]),
    try
        OldMap = #{1 => carry_member(1), 2 => carry_member(2), 3 => carry_member(3)},
        NewMap = #{
            1 => carry_member(1),
            2 => carry_member(2, <<"changed">>),
            4 => carry_member(4)
        },
        seed_carry_table(Tab, OldMap),
        ?assertEqual(3, ets:info(Tab, size)),
        ok = insert_changed_members(Tab, OldMap, NewMap),
        ?assertEqual(4, ets:info(Tab, size)),
        ?assertEqual([{1, carry_member(1)}], ets:lookup(Tab, 1)),
        ok = delete_removed_members(Tab, OldMap, NewMap),
        ?assertEqual(3, ets:info(Tab, size)),
        ?assertEqual([{1, carry_member(1)}], ets:lookup(Tab, 1)),
        ?assertEqual([1, 2, 4], carry_table_ids(Tab)),
        ?assertEqual([{2, carry_member(2, <<"changed">>)}], ets:lookup(Tab, 2))
    after
        ets:delete(Tab)
    end.

carry_delta_skips_unchanged_members_test() ->
    Tab = ets:new(unchanged_members, [set, public]),
    try
        MemberMap = #{1 => carry_member(1)},
        true = ets:insert(Tab, {1, sentinel}),
        ok = insert_changed_members(Tab, MemberMap, MemberMap),
        ?assertEqual([{1, sentinel}], ets:tab2list(Tab))
    after
        ets:delete(Tab)
    end.

carry_data(MemberMap, undefined) ->
    #{<<"members">> => MemberMap, members_normalized => MemberMap};
carry_data(MemberMap, Tab) ->
    #{<<"members">> => MemberMap, members_normalized => MemberMap, members_ets => Tab}.

seed_carry_table(Tab, MemberMap) ->
    maps:foreach(fun(UserId, Member) -> ets:insert(Tab, {UserId, Member}) end, MemberMap).

carry_member(UserId) ->
    #{<<"user">> => #{<<"id">> => UserId}}.

carry_table_ids(Tab) ->
    lists:sort([Id || {Id, _} <- ets:tab2list(Tab)]).

carry_member(UserId, Nick) ->
    #{<<"user">> => #{<<"id">> => UserId}, <<"nick">> => Nick}.

-endif.
