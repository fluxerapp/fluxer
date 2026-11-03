%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_passive_sync).
-typing([eqwalizer]).

-export([schedule_passive_sync/1, handle_passive_sync/1]).

-define(PASSIVE_SYNC_INTERVAL, 30000).
-define(LARGE_GUILD_MEMBER_COUNT, 250).
-define(HEAVY_MEMBER_DATA_KEYS, [
    <<"members">>, members_normalized, <<"member_role_index">>, members_sorted_ids
]).

-type guild_state() :: map().
-type channel_id() :: binary().
-type last_message_id() :: binary().

-spec schedule_passive_sync(map()) -> map().
schedule_passive_sync(State) ->
    erlang:send_after(?PASSIVE_SYNC_INTERVAL, self(), passive_sync),
    State.

-spec handle_passive_sync(map()) -> {noreply, map()}.
handle_passive_sync(State) ->
    GuildId = maps:get(id, State),
    PassiveSessions = large_guild_passive_sessions(GuildId, State),
    ok = maybe_spawn_passive_updates(PassiveSessions, State),
    _ = schedule_passive_sync(State),
    {noreply, State}.

-spec maybe_spawn_passive_updates(map(), guild_state()) -> ok.
maybe_spawn_passive_updates(PassiveSessions, _State) when map_size(PassiveSessions) =:= 0 ->
    ok;
maybe_spawn_passive_updates(PassiveSessions, State) ->
    SyncState = passive_sync_state(State, PassiveSessions),
    _ = spawn(fun() -> send_passive_updates(PassiveSessions, SyncState) end),
    ok.

-spec large_guild_passive_sessions(integer(), guild_state()) -> map().
large_guild_passive_sessions(GuildId, State) ->
    case is_large_guild(maps:get(member_count, State, undefined)) of
        false -> #{};
        true -> passive_sessions(GuildId, maps:get(sessions, State))
    end.

-spec passive_sessions(integer(), map()) -> map().
passive_sessions(GuildId, Sessions) ->
    maps:from_list([
        {SessionId, maps:with([pid, user_id], SessionData)}
     || {SessionId, SessionData} <- maps:to_list(Sessions),
        session_passive:is_passive(GuildId, SessionData)
    ]).

-spec passive_sync_state(guild_state(), map()) -> guild_state().
passive_sync_state(State, PassiveSessions) ->
    Base = maps:with([id, voice_server_pid, virtual_channel_access], State),
    Base#{
        data => passive_sync_data(maps:get(data, State)),
        voice_states => maps:get(voice_states, State),
        sessions => first_viewable_sessions(PassiveSessions, maps:get(sessions, State))
    }.

-spec passive_sync_data(map()) -> map().
passive_sync_data(#{members_ets := Tab} = Data) when is_reference(Tab) ->
    maps:without(?HEAVY_MEMBER_DATA_KEYS, Data);
passive_sync_data(Data) ->
    Data.

-spec first_viewable_sessions(map(), map()) -> map().
first_viewable_sessions(PassiveSessions, Sessions) ->
    UserIds = maps:fold(
        fun(_SessionId, SessionData, Acc) ->
            Acc#{maps:get(user_id, SessionData) => true}
        end,
        #{},
        PassiveSessions
    ),
    first_viewable_sessions_iter(maps:next(maps:iterator(Sessions)), UserIds, #{}).

-spec first_viewable_sessions_iter(none | {term(), term(), maps:iterator()}, map(), map()) ->
    map().
first_viewable_sessions_iter(none, _UserIds, Acc) ->
    Acc;
first_viewable_sessions_iter(
    {SessionId, #{user_id := UserId, viewable_channels := Viewable}, Next}, UserIds, Acc
) when is_map(Viewable), is_map_key(UserId, UserIds) ->
    first_viewable_sessions_iter(
        maps:next(Next),
        maps:remove(UserId, UserIds),
        Acc#{SessionId => #{user_id => UserId, viewable_channels => Viewable}}
    );
first_viewable_sessions_iter({_SessionId, _SessionData, Next}, UserIds, Acc) ->
    first_viewable_sessions_iter(maps:next(Next), UserIds, Acc).

-spec is_large_guild(term()) -> boolean().
is_large_guild(MemberCount) ->
    is_integer(MemberCount) andalso MemberCount > ?LARGE_GUILD_MEMBER_COUNT.

-spec send_passive_updates(map(), guild_state()) -> ok.
send_passive_updates(PassiveSessions, SyncState) ->
    Channels = guild_data_index_channels:channel_list(maps:get(data, SyncState)),
    maps:foreach(
        fun(SessionId, SessionData) ->
            process_single_passive_session(SessionId, SessionData, Channels, SyncState)
        end,
        PassiveSessions
    ).

-spec process_single_passive_session(binary(), map(), [map()], guild_state()) ->
    ok.
process_single_passive_session(SessionId, SessionData, Channels, State) ->
    GuildId = maps:get(id, State),
    Pid = maps:get(pid, SessionData),
    UserId = maps:get(user_id, SessionData),
    Member = guild_permissions_check:find_member_by_user_id(UserId, State),
    RegState = passive_sync_registry:lookup(SessionId, GuildId),
    Visible =
        session_channels(SessionData, Channels, State) ++
            guild_thread_push:passive_threads(SessionData, UserId, State),
    MsgDiffs = compute_passive_msg_diffs(Visible, UserId, Member, State, RegState),
    VoiceDiffs = compute_passive_voice_diffs(UserId, GuildId, State, RegState),
    Diffs = maps:merge(MsgDiffs, VoiceDiffs),
    dispatch_passive_diffs(SessionId, GuildId, Pid, Diffs).

-spec session_channels(map(), [map()], guild_state()) -> [map()].
session_channels(SessionData, Channels, State) ->
    case
        guild_thread_gate:needs_variant(State) andalso
            not guild_thread_gate:session_viewer(SessionData)
    of
        true ->
            [
                C
             || C <- Channels,
                not channel_types:is_thread_only(maps:get(<<"type">>, C, undefined))
            ];
        false ->
            Channels
    end.

-spec compute_passive_msg_diffs([map()], integer(), map() | undefined, guild_state(), map()) ->
    map().
compute_passive_msg_diffs(Channels, UserId, Member, State, RegState) ->
    CurrentLastMessageIds = build_last_message_ids(Channels, UserId, Member, State),
    PreviousLastMessageIds = maps:get(previous_passive_updates, RegState, #{}),
    #{
        delta => compute_delta(CurrentLastMessageIds, PreviousLastMessageIds),
        previous_last_message_ids => PreviousLastMessageIds
    }.

-spec compute_passive_voice_diffs(integer(), integer(), guild_state(), map()) -> map().
compute_passive_voice_diffs(UserId, GuildId, State, RegState) ->
    ViewableChannels = guild_visibility_channels:viewable_channel_set(UserId, State),
    StateWithLatestVoice = guild_data:fetch_latest_voice_states(State),
    LatestVoiceStates = maps:get(voice_states, StateWithLatestVoice),
    Current = guild_passive_sync_voice:build_current_voice_state_map(
        ViewableChannels, LatestVoiceStates
    ),
    Previous = maps:get(previous_passive_voice_states, RegState, #{}),
    Updates = guild_passive_sync_voice:compute_voice_state_updates(Current, Previous, GuildId),
    #{current_voice_states => Current, voice_state_updates => Updates}.

-spec dispatch_passive_diffs(binary(), integer(), pid(), map()) -> ok.
dispatch_passive_diffs(SessionId, GuildId, Pid, Diffs) ->
    #{
        delta := Delta,
        voice_state_updates := VoiceUpdates
    } = Diffs,
    ShouldSend = (map_size(Delta) > 0 orelse VoiceUpdates =/= []) andalso is_pid(Pid),
    case ShouldSend of
        true ->
            send_and_store_passive(SessionId, GuildId, Pid, Diffs);
        false ->
            store_passive_baseline(SessionId, GuildId, Diffs)
    end.

-spec send_and_store_passive(binary(), integer(), pid(), map()) -> ok.
send_and_store_passive(SessionId, GuildId, Pid, Diffs) ->
    #{
        delta := Delta,
        previous_last_message_ids := PrevMsgIds,
        voice_state_updates := VoiceUpdates,
        current_voice_states := CurVoice
    } = Diffs,
    EventData = build_passive_event_data(
        GuildId, Delta, VoiceUpdates
    ),
    gateway_dispatch_relay:dispatch(Pid, passive_updates, EventData, GuildId),
    MergedMsgIds = maps:merge(PrevMsgIds, Delta),
    passive_sync_registry:store(SessionId, GuildId, #{
        previous_passive_updates => MergedMsgIds,
        previous_passive_channel_versions => #{},
        previous_passive_voice_states => CurVoice
    }),
    ok.

-spec store_passive_baseline(binary(), integer(), map()) -> ok.
store_passive_baseline(SessionId, GuildId, Diffs) ->
    #{
        previous_last_message_ids := PrevMsgIds,
        current_voice_states := CurVoice
    } = Diffs,
    passive_sync_registry:store(SessionId, GuildId, #{
        previous_passive_updates => PrevMsgIds,
        previous_passive_channel_versions => #{},
        previous_passive_voice_states => CurVoice
    }),
    ok.

-spec build_passive_event_data(integer(), map(), [map()]) ->
    map().
build_passive_event_data(GuildId, Delta, VoiceUpdates) ->
    Base = #{<<"guild_id">> => integer_to_binary(GuildId), <<"channels">> => Delta},
    case VoiceUpdates of
        [] -> Base;
        _ -> Base#{<<"voice_states">> => VoiceUpdates}
    end.

-spec compute_delta(#{channel_id() => last_message_id()}, #{channel_id() => last_message_id()}) ->
    #{channel_id() => last_message_id()}.
compute_delta(CurrentLastMessageIds, PreviousLastMessageIds) ->
    maps:filter(
        fun(ChannelId, CurrentValue) ->
            maps:get(ChannelId, PreviousLastMessageIds, undefined) =/= CurrentValue
        end,
        CurrentLastMessageIds
    ).

-spec build_last_message_ids([map()], integer(), map() | undefined, guild_state()) ->
    #{channel_id() => last_message_id()}.
build_last_message_ids(_Channels, _UserId, undefined, _State) ->
    #{};
build_last_message_ids(Channels, UserId, Member, State) ->
    lists:foldl(
        fun(Channel, Acc) ->
            maybe_add_last_message(Channel, UserId, Member, State, Acc)
        end,
        #{},
        Channels
    ).

-spec maybe_add_last_message(map(), integer(), map(), guild_state(), map()) -> map().
maybe_add_last_message(Channel, UserId, Member, State, Acc) ->
    RawId = maps:get(<<"id">>, Channel, undefined),
    RawMsgId = maps:get(<<"last_message_id">>, Channel, null),
    Parsed =
        case {RawId, RawMsgId} of
            {undefined, _} ->
                undefined;
            {_, undefined} ->
                undefined;
            {_, null} ->
                undefined;
            _ ->
                {
                    snowflake_binary(<<"id">>, RawId),
                    snowflake_binary(<<"last_message_id">>, RawMsgId)
                }
        end,
    ChannelId =
        case Parsed of
            {ParsedId, ParsedMsgId} when is_binary(ParsedId), is_binary(ParsedMsgId) ->
                guild_dispatch_decorate:parse_snowflake(<<"id">>, RawId);
            _ ->
                undefined
        end,
    Allowed =
        case ChannelId of
            ChId when is_integer(ChId) ->
                guild_permissions_check:can_view_channel(UserId, ChId, Member, State);
            _ ->
                false
        end,
    case {Allowed, Parsed} of
        {true, {IdBin, MsgIdBin}} -> Acc#{IdBin => MsgIdBin};
        _ -> Acc
    end.

-spec snowflake_binary(binary(), term()) -> binary() | undefined.
snowflake_binary(FieldName, Value) ->
    case validation:validate_snowflake(FieldName, Value) of
        {ok, Id} -> integer_to_binary(Id);
        {error, _, _} -> undefined
    end.
