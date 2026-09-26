%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_passive_sync).
-typing([eqwalizer]).

-export([
    schedule_passive_sync/1,
    handle_passive_sync/1,
    send_passive_updates_to_sessions/1,
    compute_delta/2,
    compute_channel_diffs/2,
    compute_voice_state_updates/3
]).

-export_type([guild_state/0, channel_id/0, last_message_id/0, version/0, voice_state/0]).

-define(PASSIVE_SYNC_INTERVAL, 30000).
-define(LARGE_GUILD_MEMBER_COUNT, 250).

-type guild_state() :: map().
-type channel_id() :: binary().
-type last_message_id() :: binary().
-type version() :: integer().
-type voice_state() :: map().

-spec schedule_passive_sync(guild_state()) -> guild_state().
schedule_passive_sync(State) ->
    erlang:send_after(?PASSIVE_SYNC_INTERVAL, self(), passive_sync),
    State.

-spec handle_passive_sync(guild_state()) -> {noreply, guild_state()}.
handle_passive_sync(State) ->
    GuildId = maps:get(id, State),
    ok = maybe_spawn_passive_updates(GuildId, State),
    _ = schedule_passive_sync(State),
    {noreply, State}.

%% send_passive_updates/4 keeps no session unless is_large_guild/1 holds, so for any other
%% guild the spawned child returns without touching a session, a payload or the registry.
-spec maybe_spawn_passive_updates(integer(), guild_state()) -> ok.
maybe_spawn_passive_updates(GuildId, State) ->
    case is_large_guild(maps:get(member_count, State, undefined)) of
        false -> ok;
        true -> spawn_passive_updates(GuildId, State)
    end.

-spec spawn_passive_updates(integer(), guild_state()) -> ok.
spawn_passive_updates(GuildId, State) ->
    _ = spawn(fun() -> send_passive_updates_for_state(GuildId, State) end),
    ok.

-spec send_passive_updates_to_sessions(guild_state()) -> guild_state().
send_passive_updates_to_sessions(State) ->
    ok = send_passive_updates_for_state(maps:get(id, State), State),
    State.

-spec send_passive_updates_for_state(integer(), guild_state()) -> ok.
send_passive_updates_for_state(GuildId, State) ->
    Sessions = maps:get(sessions, State, #{}),
    Data = maps:get(data, State, #{}),
    MemberCount = maps:get(member_count, State, undefined),
    VoiceStates = maps:get(voice_states, State, #{}),
    send_passive_updates(
        GuildId, Sessions, passive_sync_state(State, Data, VoiceStates), MemberCount
    ).

-spec passive_sync_state(guild_state(), map(), map()) -> guild_state().
passive_sync_state(State, Data, VoiceStates) ->
    State#{data => Data, voice_states => VoiceStates}.

-spec is_large_guild(term()) -> boolean().
is_large_guild(MemberCount) ->
    is_integer(MemberCount) andalso MemberCount > ?LARGE_GUILD_MEMBER_COUNT.

-spec send_passive_updates(integer(), map(), guild_state(), non_neg_integer() | undefined) ->
    ok.
send_passive_updates(GuildId, Sessions, State, MemberCount) ->
    Data = maps:get(data, State, #{}),
    Channels = guild_data_index:channel_list(Data),
    IsLargeGuild = is_large_guild(MemberCount),
    PassiveSessions = maps:filter(
        fun(_SessionId, SessionData) ->
            IsLargeGuild andalso session_passive:is_passive(GuildId, SessionData)
        end,
        Sessions
    ),
    case map_size(PassiveSessions) of
        0 ->
            ok;
        _ ->
            send_passive_session_updates(PassiveSessions, GuildId, Channels, State)
    end.

-spec send_passive_session_updates(map(), integer(), [map()], guild_state()) -> ok.
send_passive_session_updates(PassiveSessions, GuildId, Channels, SyncState) ->
    maps:foreach(
        fun(SessionId, SessionData) ->
            process_single_passive_session(SessionId, SessionData, GuildId, Channels, SyncState)
        end,
        PassiveSessions
    ).

-spec process_single_passive_session(binary(), map(), integer(), [map()], guild_state()) ->
    ok.
process_single_passive_session(SessionId, SessionData, GuildId, Channels, State) ->
    Pid = maps:get(pid, SessionData),
    UserId = maps:get(user_id, SessionData),
    Member = guild_permissions:find_member_by_user_id(UserId, State),
    RegState = passive_sync_registry:lookup(SessionId, GuildId),
    Diffs = compute_passive_diffs(Channels, UserId, Member, GuildId, State, RegState),
    dispatch_passive_diffs(SessionId, GuildId, Pid, Diffs).

-spec compute_passive_diffs(
    [map()], integer(), map() | undefined, integer(), guild_state(), map()
) -> map().
compute_passive_diffs(Channels, UserId, Member, GuildId, State, RegState) ->
    MsgDiffs = compute_passive_msg_diffs(Channels, UserId, Member, State, RegState),
    VoiceDiffs = compute_passive_voice_diffs(UserId, GuildId, State, RegState),
    maps:merge(MsgDiffs, VoiceDiffs).

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
    ViewableChannels = guild_visibility:viewable_channel_set(UserId, State),
    StateWithLatestVoice = guild_data:fetch_latest_voice_states(State),
    LatestVoiceStates = maps:get(voice_states, StateWithLatestVoice, #{}),
    Current = guild_passive_sync_voice:build_current_voice_state_map(
        ViewableChannels, LatestVoiceStates
    ),
    Previous = maps:get(previous_passive_voice_states, RegState, #{}),
    Updates = compute_voice_state_updates(Current, Previous, GuildId),
    #{current_voice_states => Current, voice_state_updates => Updates}.

-spec dispatch_passive_diffs(binary(), integer(), pid(), map()) -> ok.
dispatch_passive_diffs(SessionId, GuildId, Pid, Diffs) ->
    #{
        delta := Delta,
        voice_state_updates := VoiceUpdates
    } = Diffs,
    ShouldSend = has_passive_changes(Delta, VoiceUpdates),
    case {ShouldSend, is_pid(Pid)} of
        {true, true} ->
            send_and_store_passive(SessionId, GuildId, Pid, Diffs);
        _ ->
            store_passive_baseline(SessionId, GuildId, Diffs)
    end.

-spec has_passive_changes(map(), [map()]) -> boolean().
has_passive_changes(Delta, VoiceUpdates) ->
    map_size(Delta) > 0 orelse VoiceUpdates =/= [].

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
    lists:foldl(fun maybe_put_field/2, Base, [
        {<<"voice_states">>, VoiceUpdates}
    ]).

-spec maybe_put_field({binary(), list()}, map()) -> map().
maybe_put_field({_Key, []}, Map) -> Map;
maybe_put_field({Key, Value}, Map) -> Map#{Key => Value}.

-spec compute_delta(#{channel_id() => last_message_id()}, #{channel_id() => last_message_id()}) ->
    #{channel_id() => last_message_id()}.
compute_delta(CurrentLastMessageIds, PreviousLastMessageIds) ->
    maps:filter(
        fun(ChannelId, CurrentValue) ->
            last_message_changed(ChannelId, CurrentValue, PreviousLastMessageIds)
        end,
        CurrentLastMessageIds
    ).

-spec last_message_changed(channel_id(), last_message_id(), #{channel_id() => last_message_id()}) ->
    boolean().
last_message_changed(ChannelId, CurrentValue, PreviousLastMessageIds) ->
    case maps:get(ChannelId, PreviousLastMessageIds, undefined) of
        undefined -> true;
        PreviousValue -> CurrentValue =/= PreviousValue
    end.

-spec compute_channel_diffs(#{channel_id() => version()}, #{channel_id() => version()}) ->
    {[channel_id()], [channel_id()], [channel_id()]}.
compute_channel_diffs(Current, Previous) ->
    {Created, Updated} = maps:fold(
        fun(Id, V, {CreatedAcc, UpdatedAcc}) ->
            collect_channel_version_diff(Id, V, Previous, {CreatedAcc, UpdatedAcc})
        end,
        {[], []},
        Current
    ),
    Deleted = maps:fold(
        fun(Id, _, Acc) ->
            collect_deleted_channel_id(Id, Current, Acc)
        end,
        [],
        Previous
    ),
    {Created, Updated, Deleted}.

-spec collect_channel_version_diff(
    channel_id(), version(), #{channel_id() => version()}, {[channel_id()], [channel_id()]}
) -> {[channel_id()], [channel_id()]}.
collect_channel_version_diff(Id, V, Previous, {CreatedAcc, UpdatedAcc}) ->
    case maps:find(Id, Previous) of
        error -> {[Id | CreatedAcc], UpdatedAcc};
        {ok, PrevV} when PrevV =/= V -> {CreatedAcc, [Id | UpdatedAcc]};
        _ -> {CreatedAcc, UpdatedAcc}
    end.

-spec collect_deleted_channel_id(channel_id(), #{channel_id() => version()}, [channel_id()]) ->
    [channel_id()].
collect_deleted_channel_id(Id, Current, Acc) ->
    case maps:is_key(Id, Current) of
        false -> [Id | Acc];
        true -> Acc
    end.

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
    case
        {maps:get(<<"id">>, Channel, undefined), maps:get(<<"last_message_id">>, Channel, null)}
    of
        {undefined, _} ->
            Acc;
        {_, undefined} ->
            Acc;
        {_, null} ->
            Acc;
        {RawId, RawMsgId} ->
            maybe_add_last_message_id(RawId, RawMsgId, UserId, Member, State, Acc)
    end.

-spec maybe_add_last_message_id(term(), term(), integer(), map(), guild_state(), map()) ->
    map().
maybe_add_last_message_id(RawId, RawMsgId, UserId, Member, State, Acc) ->
    case
        {snowflake_binary(<<"id">>, RawId), snowflake_binary(<<"last_message_id">>, RawMsgId)}
    of
        {undefined, _} ->
            Acc;
        {_, undefined} ->
            Acc;
        {IdBin, MsgIdBin} ->
            maybe_add_last_message_for_channel(
                IdBin, MsgIdBin, RawId, UserId, Member, State, Acc
            )
    end.

-spec maybe_add_last_message_for_channel(
    binary(), binary(), term(), integer(), map(), guild_state(), map()
) -> map().
maybe_add_last_message_for_channel(IdBin, MsgIdBin, RawId, UserId, Member, State, Acc) ->
    case parse_snowflake(<<"id">>, RawId) of
        ChId when is_integer(ChId) ->
            maybe_add_visible_last_message(IdBin, MsgIdBin, UserId, ChId, Member, State, Acc);
        undefined ->
            Acc
    end.

-spec maybe_add_visible_last_message(
    binary(), term(), integer(), integer(), map(), guild_state(), map()
) -> map().
maybe_add_visible_last_message(IdBin, MsgId, UserId, ChId, Member, State, Acc) ->
    case guild_permissions:can_view_channel(UserId, ChId, Member, State) of
        true -> Acc#{IdBin => MsgId};
        false -> Acc
    end.

-spec compute_voice_state_updates(
    #{binary() => voice_state()}, #{binary() => voice_state()}, integer()
) -> [voice_state()].
compute_voice_state_updates(Current, Previous, GuildId) ->
    guild_passive_sync_voice:compute_voice_state_updates(Current, Previous, GuildId).

-spec parse_snowflake(binary(), term()) -> integer() | undefined.
parse_snowflake(FieldName, Value) ->
    case validation:validate_snowflake(FieldName, Value) of
        {ok, Id} -> Id;
        {error, _, _} -> undefined
    end.

-spec snowflake_binary(binary(), term()) -> binary() | undefined.
snowflake_binary(FieldName, Value) ->
    case validation:validate_snowflake(FieldName, Value) of
        {ok, Id} -> integer_to_binary(Id);
        {error, _, _} -> undefined
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_large_guild_matches_previous_inline_threshold_test() ->
    lists:foreach(
        fun(MemberCount) ->
            Expected = is_integer(MemberCount) andalso MemberCount > 250,
            ?assertEqual(Expected, is_large_guild(MemberCount))
        end,
        [undefined, 0, 1, 249, 250, 251, 1000000]
    ).

skipped_spawn_drops_no_session_the_old_filter_kept_test() ->
    GuildId = 4242,
    lists:foreach(
        fun({MemberCount, Sessions}) ->
            Kept = reference_passive_sessions(GuildId, Sessions, MemberCount),
            ?assert(is_large_guild(MemberCount) orelse map_size(Kept) =:= 0)
        end,
        passive_gate_cases()
    ),
    Large = reference_passive_sessions(GuildId, passive_test_sessions(), 251),
    ?assertEqual(1, map_size(Large)).

reference_passive_sessions(GuildId, Sessions, MemberCount) ->
    IsLargeGuild = is_integer(MemberCount) andalso MemberCount > 250,
    maps:filter(
        fun(_SessionId, SessionData) ->
            IsLargeGuild andalso session_passive:is_passive(GuildId, SessionData)
        end,
        Sessions
    ).

passive_gate_cases() ->
    Sessions = passive_test_sessions(),
    [
        {undefined, #{}},
        {undefined, Sessions},
        {0, Sessions},
        {250, Sessions},
        {251, Sessions},
        {251, #{}},
        {not_a_count, Sessions}
    ].

passive_test_sessions() ->
    passive_test_sessions(<<"passive-session">>).

passive_test_sessions(SessionId) ->
    #{
        SessionId => #{
            session_id => SessionId,
            user_id => 1130650140672000000,
            pid => self(),
            active_guilds => sets:new(),
            bot => false
        }
    }.

handle_passive_sync_stays_silent_below_threshold_test() ->
    flush_passive_dispatches(),
    ok = passive_sync_registry:init(),
    GuildId = 1427764661718740995,
    SessionId = <<"small-guild-session">>,
    State = passive_sync_test_state(GuildId, SessionId, 250),
    ?assertMatch({noreply, State}, handle_passive_sync(State)),
    ?assertEqual(no_dispatch, receive_passive_dispatch(200)),
    RegState = passive_sync_registry:lookup(SessionId, GuildId),
    ?assertEqual(#{}, maps:get(previous_passive_updates, RegState)).

handle_passive_sync_still_dispatches_above_threshold_test() ->
    flush_passive_dispatches(),
    ok = passive_sync_registry:init(),
    GuildId = 1427764661718740996,
    SessionId = <<"large-guild-session">>,
    ChannelId = passive_test_channel_id(),
    MessageId = passive_test_message_id(),
    State = passive_sync_test_state(GuildId, SessionId, 251),
    ?assertMatch({noreply, State}, handle_passive_sync(State)),
    Payload = receive_passive_dispatch(5000),
    ?assertEqual(
        #{integer_to_binary(ChannelId) => integer_to_binary(MessageId)},
        maps:get(<<"channels">>, Payload)
    ).

passive_sync_test_state(GuildId, SessionId, MemberCount) ->
    UserId = 1130650140672000000,
    Channel = #{
        <<"id">> => passive_test_channel_id(),
        <<"last_message_id">> => passive_test_message_id()
    },
    #{
        id => GuildId,
        member_count => MemberCount,
        sessions => passive_test_sessions(SessionId),
        data => #{
            <<"guild">> => #{<<"id">> => GuildId, <<"owner_id">> => UserId},
            <<"channels">> => [Channel],
            <<"members">> => [#{<<"user">> => #{<<"id">> => UserId}, <<"roles">> => []}],
            <<"roles">> => []
        },
        voice_states => #{}
    }.

passive_test_channel_id() ->
    1497639278555484216.

passive_test_message_id() ->
    1500000000000000000.

receive_passive_dispatch(Timeout) ->
    receive
        {'$gen_cast', {dispatch, passive_updates, Payload}} when is_map(Payload) ->
            Payload
    after Timeout ->
        no_dispatch
    end.

flush_passive_dispatches() ->
    case receive_passive_dispatch(0) of
        no_dispatch -> ok;
        _ -> flush_passive_dispatches()
    end.

-endif.
