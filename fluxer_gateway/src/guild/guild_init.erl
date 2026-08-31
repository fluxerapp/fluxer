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

-type guild_state() :: map().

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
    NormalizedNewData = guild_data_index:normalize_map(ReloadData),
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
