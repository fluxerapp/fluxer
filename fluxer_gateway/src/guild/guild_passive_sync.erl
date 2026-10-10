%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_passive_sync).
-typing([eqwalizer]).

-export([
    schedule_passive_sync/1,
    handle_passive_sync/1,
    send_passive_updates_to_sessions/1,
    compute_delta/2,
    compute_voice_state_updates/3
]).

-export_type([guild_state/0, channel_id/0, last_message_id/0, version/0, voice_state/0]).

-define(PASSIVE_SYNC_INTERVAL, 30000).
-define(LARGE_GUILD_MEMBER_COUNT, 250).
-define(HEAVY_MEMBER_DATA_KEYS, [
    <<"members">>, members_normalized, <<"member_role_index">>, members_sorted_ids
]).

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

-spec maybe_spawn_passive_updates(integer(), guild_state()) -> ok.
maybe_spawn_passive_updates(GuildId, State) ->
    case large_guild_passive_sessions(GuildId, State) of
        PassiveSessions when map_size(PassiveSessions) =:= 0 ->
            ok;
        PassiveSessions ->
            SyncState = passive_sync_state(State, PassiveSessions),
            _ = spawn(fun() -> send_passive_updates(GuildId, PassiveSessions, SyncState) end),
            ok
    end.

-spec send_passive_updates_to_sessions(guild_state()) -> guild_state().
send_passive_updates_to_sessions(State) ->
    GuildId = maps:get(id, State),
    PassiveSessions = large_guild_passive_sessions(GuildId, State),
    ok = send_passive_updates(
        GuildId, PassiveSessions, passive_sync_state(State, PassiveSessions)
    ),
    State.

-spec large_guild_passive_sessions(integer(), guild_state()) -> map().
large_guild_passive_sessions(GuildId, State) ->
    case is_large_guild(maps:get(member_count, State, undefined)) of
        false -> #{};
        true -> passive_sessions(GuildId, maps:get(sessions, State, #{}))
    end.

-spec passive_sessions(integer(), map()) -> map().
passive_sessions(GuildId, Sessions) ->
    maps:filtermap(
        fun(_SessionId, SessionData) ->
            case session_passive:is_passive(GuildId, SessionData) of
                true -> {true, maps:with([pid, user_id], SessionData)};
                false -> false
            end
        end,
        Sessions
    ).

-spec passive_sync_state(guild_state(), map()) -> guild_state().
passive_sync_state(State, PassiveSessions) ->
    Base = maps:with([id, voice_server_pid, virtual_channel_access], State),
    Base#{
        data => passive_sync_data(maps:get(data, State, #{})),
        voice_states => maps:get(voice_states, State, #{}),
        sessions => first_viewable_sessions(PassiveSessions, maps:get(sessions, State, #{}))
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
            Acc#{maps:get(user_id, SessionData, undefined) => true}
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

-spec send_passive_updates(integer(), map(), guild_state()) -> ok.
send_passive_updates(GuildId, PassiveSessions, SyncState) ->
    Channels = guild_data_index:channel_list(maps:get(data, SyncState)),
    send_passive_session_updates(PassiveSessions, GuildId, Channels, SyncState).

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
    Visible =
        session_channels(SessionData, Channels, State) ++
            guild_thread_push:passive_threads(SessionData, UserId, State),
    Diffs = compute_passive_diffs(Visible, UserId, Member, GuildId, State, RegState),
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
                not guild_thread_gate:is_thread_only_type(maps:get(<<"type">>, C, undefined))
            ];
        false ->
            Channels
    end.

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

passive_sync_state_matches_full_state_dispatches_test() ->
    with_differential_guild(fun(VoiceServer, Rounds, SessionIds, GuildId) ->
        Reference = run_passive_rounds(
            fun reference_send_passive_updates/1, VoiceServer, Rounds, SessionIds, GuildId
        ),
        Projected = run_passive_rounds(
            fun send_passive_updates_to_sessions/1, VoiceServer, Rounds, SessionIds, GuildId
        ),
        [{Round1Dispatches, _}, {Round2Dispatches, _}] = Reference,
        ?assertEqual(4, length(Round1Dispatches)),
        ?assertEqual(4, length(Round2Dispatches)),
        ?assertEqual(Reference, Projected)
    end).

passive_sync_state_drops_members_and_unrelated_sessions_test() ->
    with_differential_guild(fun(_VoiceServer, [{_, State} | _], _SessionIds, GuildId) ->
        PassiveSessions = large_guild_passive_sessions(GuildId, State),
        SyncState = passive_sync_state(State, PassiveSessions),
        ?assertEqual(
            [data, id, sessions, virtual_channel_access, voice_server_pid, voice_states],
            lists:sort(maps:keys(SyncState))
        ),
        SyncData = maps:get(data, SyncState),
        ?assertEqual([], [K || K <- ?HEAVY_MEMBER_DATA_KEYS, is_map_key(K, SyncData)]),
        ?assert(is_map_key(members_ets, SyncData)),
        ?assertEqual(
            #{
                <<"u2-passive">> => #{
                    user_id => diff_user(2), viewable_channels => #{diff_channel(1) => true}
                },
                <<"u3-first">> => #{
                    user_id => diff_user(3), viewable_channels => #{diff_channel(3) => true}
                }
            },
            maps:get(sessions, SyncState)
        ),
        ?assertEqual(
            lists:sort([
                <<"u1-passive">>,
                <<"u2-passive">>,
                <<"u3-passive">>,
                <<"u4-passive">>,
                <<"u5-passive">>
            ]),
            lists:sort(maps:keys(PassiveSessions))
        )
    end).

passive_sync_data_keeps_members_without_members_ets_test() ->
    Data = #{<<"members">> => #{1 => #{}}, members_sorted_ids => [1], <<"channels">> => []},
    ?assertEqual(Data, passive_sync_data(Data)).

first_viewable_sessions_picks_the_session_the_full_state_lookup_finds_test() ->
    Sessions = maps:from_list([
        {integer_to_binary(N), #{user_id => N rem 3, viewable_channels => #{N => true}}}
     || N <- lists:seq(1, 64)
    ]),
    Passive = #{<<"p">> => #{user_id => 1}, <<"q">> => #{user_id => 2}},
    Projected = first_viewable_sessions(Passive, Sessions),
    ?assertEqual(2, map_size(Projected)),
    lists:foreach(
        fun(UserId) ->
            ?assertEqual(
                guild_visibility_channels:get_cached_viewable_channel_map(
                    UserId, #{sessions => Sessions}
                ),
                guild_visibility_channels:get_cached_viewable_channel_map(
                    UserId, #{sessions => Projected}
                )
            )
        end,
        [1, 2]
    ).

reference_send_passive_updates(State) ->
    GuildId = maps:get(id, State),
    PassiveSessions = reference_passive_sessions(
        GuildId, maps:get(sessions, State), maps:get(member_count, State)
    ),
    Data = maps:get(data, State),
    Channels = guild_data_index:channel_list(Data),
    SyncState = State#{data => Data, voice_states => maps:get(voice_states, State, #{})},
    ok = send_passive_session_updates(PassiveSessions, GuildId, Channels, SyncState),
    State.

run_passive_rounds(Send, VoiceServer, Rounds, SessionIds, GuildId) ->
    flush_passive_dispatches(),
    ok = passive_sync_registry:init(),
    lists:foreach(
        fun(SessionId) -> passive_sync_registry:delete(SessionId, GuildId) end, SessionIds
    ),
    [
        begin
            ok = gen_server:call(VoiceServer, {set, VoiceStates}),
            _ = Send(State),
            {collect_passive_dispatches([]), [
                {SessionId, passive_sync_registry:lookup(SessionId, GuildId)}
             || SessionId <- SessionIds
            ]}
        end
     || {VoiceStates, State} <- Rounds
    ].

collect_passive_dispatches(Acc) ->
    case receive_passive_dispatch(0) of
        no_dispatch -> lists:reverse(Acc);
        Payload -> collect_passive_dispatches([Payload | Acc])
    end.

with_differential_guild(Fun) ->
    GuildId = 1427764882469228556,
    Tab = ets:new(passive_diff_members, [set, public]),
    VoiceServer = spawn(fun() -> fake_voice_server(#{}) end),
    try
        Rounds = [
            {
                diff_voice_states(GuildId, Round),
                differential_state(GuildId, Tab, VoiceServer, Round)
            }
         || Round <- [0, 1]
        ],
        [{_, #{sessions := Sessions}} | _] = Rounds,
        Fun(VoiceServer, Rounds, lists:sort(maps:keys(Sessions)), GuildId)
    after
        exit(VoiceServer, kill),
        ets:delete(Tab)
    end.

fake_voice_server(VoiceStates) ->
    receive
        {'$gen_call', From, {set, NewVoiceStates}} ->
            gen_server:reply(From, ok),
            fake_voice_server(NewVoiceStates);
        {'$gen_call', From, {get_voice_states_map}} ->
            gen_server:reply(From, VoiceStates),
            fake_voice_server(VoiceStates)
    end.

differential_state(GuildId, Tab, VoiceServer, Round) ->
    Members = [
        diff_member(diff_user(0), []),
        diff_member(diff_user(1), [diff_role(1)]),
        diff_member(diff_user(2), [diff_role(2)]),
        diff_member(diff_user(3), [diff_role(1), diff_role(2)]),
        diff_member(diff_user(4), []),
        diff_member(diff_user(6), [diff_role(1)]),
        diff_member(diff_user(7), [])
    ],
    true = ets:insert(Tab, [
        {diff_user(N), M}
     || {N, M} <- lists:zip([0, 1, 2, 3, 4, 6, 7], Members)
    ]),
    Data = guild_data_index:normalize_map(#{
        <<"guild">> => #{<<"id">> => GuildId, <<"owner_id">> => diff_user(0)},
        <<"roles">> => [
            #{<<"id">> => GuildId, <<"permissions">> => <<"1024">>, <<"position">> => 0},
            #{<<"id">> => diff_role(1), <<"permissions">> => <<"0">>, <<"position">> => 1},
            #{<<"id">> => diff_role(2), <<"permissions">> => <<"0">>, <<"position">> => 2}
        ],
        <<"channels">> => diff_channels(GuildId, Round),
        <<"members">> => Members
    }),
    #{
        id => GuildId,
        member_count => 55278,
        voice_server_pid => VoiceServer,
        virtual_channel_access => #{diff_user(4) => sets:from_list([diff_channel(5)])},
        voice_states => #{},
        member_presence => make_ref(),
        presence_subscriptions => #{diff_user(1) => true},
        data => Data#{members_ets => Tab},
        sessions => diff_sessions(GuildId)
    }.

diff_channels(GuildId, Round) ->
    Deny = fun(Id) ->
        #{<<"id">> => Id, <<"type">> => 0, <<"allow">> => <<"0">>, <<"deny">> => <<"1024">>}
    end,
    Allow = fun(Id, Type) ->
        #{<<"id">> => Id, <<"type">> => Type, <<"allow">> => <<"1024">>, <<"deny">> => <<"0">>}
    end,
    [
        #{
            <<"id">> => diff_channel(1),
            <<"type">> => 0,
            <<"last_message_id">> => diff_message(1, Round)
        },
        #{
            <<"id">> => diff_channel(2),
            <<"type">> => 0,
            <<"last_message_id">> => diff_message(2, Round),
            <<"permission_overwrites">> => [Deny(GuildId), Allow(diff_role(1), 0)]
        },
        #{
            <<"id">> => diff_channel(3),
            <<"type">> => 4,
            <<"permission_overwrites">> => [Deny(GuildId)]
        },
        #{
            <<"id">> => diff_channel(4),
            <<"type">> => 0,
            <<"parent_id">> => diff_channel(3),
            <<"last_message_id">> => diff_message(4, 0),
            <<"permission_overwrites">> => [Deny(GuildId), Allow(diff_role(2), 0)]
        },
        #{
            <<"id">> => diff_channel(5),
            <<"type">> => 2,
            <<"last_message_id">> => diff_message(5, Round),
            <<"permission_overwrites">> => [Deny(GuildId), Allow(diff_user(7), 1)]
        },
        #{<<"id">> => diff_channel(6), <<"type">> => 0, <<"last_message_id">> => null}
    ].

diff_sessions(GuildId) ->
    Active = sets:from_list([GuildId]),
    Passive = sets:new(),
    Session = fun(User, ActiveGuilds, Extra) ->
        maps:merge(
            #{
                user_id => diff_user(User),
                pid => self(),
                active_guilds => ActiveGuilds,
                bot => false,
                user_roles => [],
                pending_connect => false
            },
            Extra
        )
    end,
    #{
        <<"u0-active">> => Session(0, Active, #{viewable_channels => #{diff_channel(2) => true}}),
        <<"u1-passive">> => Session(1, Passive, #{}),
        <<"u2-passive">> => Session(2, Passive, #{
            viewable_channels => #{diff_channel(1) => true}
        }),
        <<"u3-first">> => Session(3, Active, #{viewable_channels => #{diff_channel(3) => true}}),
        <<"u3-passive">> => Session(3, Passive, #{
            viewable_channels => #{diff_channel(5) => true}
        }),
        <<"u4-passive">> => Session(4, Passive, #{viewable_channels => not_a_map}),
        <<"u5-passive">> => Session(5, Passive, #{}),
        <<"u6-bot">> => Session(6, Passive, #{bot => true}),
        <<"u7-active">> => Session(7, Active, #{})
    }.

diff_voice_states(GuildId, Round) ->
    VoiceState = fun(Conn, User, Channel, Version) ->
        {Conn, #{
            <<"connection_id">> => Conn,
            <<"guild_id">> => integer_to_binary(GuildId),
            <<"channel_id">> => integer_to_binary(diff_channel(Channel)),
            <<"user_id">> => integer_to_binary(diff_user(User)),
            <<"version">> => Version
        }}
    end,
    maps:from_list(
        [
            VoiceState(<<"c1">>, 7, 5, 1),
            VoiceState(<<"c2">>, 1, 1, 1 + Round),
            VoiceState(<<"c4">>, 3, 3, 1)
        ] ++
            [VoiceState(<<"c3">>, 2, 2, 1) || Round =:= 0]
    ).

diff_member(UserId, Roles) ->
    #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => Roles}.

diff_user(N) -> 1130650140672000000 + N.

diff_role(N) -> 1428000118785000000 + N.

diff_channel(N) -> 1428100000000000000 + N.

diff_message(N, Round) -> 1500000000000000000 + N * 10 + Round.

-endif.
