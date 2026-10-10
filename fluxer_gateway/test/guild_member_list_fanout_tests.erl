%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_fanout_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

batched_member_list_fanout_coalesces_test_() ->
    {timeout, 180, fun() ->
        with_batched_fanout_relay_mock(fun() ->
            batched_fanout_case(identical_ranges, 16, 128, 5000),
            batched_fanout_case(unique_ranges, 16, 128, 5000)
        end)
    end}.

batched_fanout_case(RangeMode, ChannelCount, SessionsPerChannel, MemberCount) ->
    {State, SubsTab} = batched_fanout_state(
        RangeMode, ChannelCount, SessionsPerChannel, MemberCount
    ),
    ExpectedDispatches = expected_batched_fanout_dispatches(
        RangeMode, ChannelCount, SessionsPerChannel
    ),
    try
        {ok, QueuedState} = guild_member_list:broadcast_all_member_list_updates(State),
        FlushedState = guild_member_list:flush_pending_member_list_syncs(QueuedState),
        {Dispatches, Recipients, Bytes} = collect_batched_fanout_dispatches(
            ExpectedDispatches
        ),
        assert_no_batched_fanout_dispatch(),
        ?assertEqual(ExpectedDispatches, Dispatches),
        ?assertEqual(ChannelCount * SessionsPerChannel, Recipients),
        ?assert(Bytes > 0),
        FlushedState
    after
        guild_member_list_subs:destroy(SubsTab),
        guild_member_list_channel_engine:destroy_all(State)
    end.

expected_batched_fanout_dispatches(identical_ranges, ChannelCount, _SessionsPerChannel) ->
    ChannelCount;
expected_batched_fanout_dispatches(unique_ranges, ChannelCount, SessionsPerChannel) ->
    ChannelCount * SessionsPerChannel.

batched_fanout_state(RangeMode, ChannelCount, SessionsPerChannel, MemberCount) ->
    GuildId = 424242,
    ChannelIds = lists:seq(700000, 700000 + ChannelCount - 1),
    MemberIds = lists:seq(1, MemberCount),
    Members = [fanout_member(UserId) || UserId <- MemberIds],
    SubsTab = guild_member_list_subs:new(),
    Sessions = batched_fanout_sessions(
        RangeMode, ChannelIds, SessionsPerChannel, SubsTab, MemberIds
    ),
    State = #{
        id => GuildId,
        member_count => MemberCount,
        sessions => Sessions,
        connected_user_ids => sets:from_list(MemberIds),
        member_presence => maps:from_list([
            {UserId, #{<<"status">> => <<"online">>, <<"mobile">> => false}}
         || UserId <- MemberIds
        ]),
        member_list_subscriptions => SubsTab,
        data => #{
            <<"guild">> => #{
                <<"id">> => integer_to_binary(GuildId),
                <<"owner_id">> => <<"1">>,
                <<"features">> => [],
                <<"member_count">> => MemberCount
            },
            <<"roles">> => [fanout_everyone_role(GuildId)],
            <<"channels">> => [fanout_channel(ChannelId) || ChannelId <- ChannelIds],
            <<"members">> => Members
        },
        channel_member_list_engines => #{}
    },
    {State, SubsTab}.

batched_fanout_sessions(RangeMode, ChannelIds, SessionsPerChannel, SubsTab, MemberIds) ->
    ViewableChannels = maps:from_list([{ChannelId, true} || ChannelId <- ChannelIds]),
    maps:from_list(
        lists:append([
            batched_fanout_channel_sessions(
                RangeMode, ChannelId, SessionsPerChannel, SubsTab, MemberIds, ViewableChannels
            )
         || ChannelId <- ChannelIds
        ])
    ).

batched_fanout_channel_sessions(
    RangeMode, ChannelId, SessionsPerChannel, SubsTab, MemberIds, ViewableChannels
) ->
    [
        batched_fanout_session_entry(
            RangeMode, ChannelId, SessionIdx, SubsTab, MemberIds, ViewableChannels
        )
     || SessionIdx <- lists:seq(1, SessionsPerChannel)
    ].

batched_fanout_session_entry(
    RangeMode, ChannelId, SessionIdx, SubsTab, MemberIds, ViewableChannels
) ->
    SessionId = batched_fanout_session_id(ChannelId, SessionIdx),
    Ranges = batched_fanout_ranges(RangeMode, SessionIdx),
    {_OldRanges, _ShouldSync} = guild_member_list_subs:subscribe(
        SessionId, integer_to_binary(ChannelId), Ranges, SubsTab
    ),
    {SessionId, #{
        pid => self(),
        user_id => lists:nth(((SessionIdx - 1) rem length(MemberIds)) + 1, MemberIds),
        viewable_channels => ViewableChannels
    }}.

batched_fanout_session_id(ChannelId, SessionIdx) ->
    <<"fanout_", (integer_to_binary(ChannelId))/binary, "_",
        (integer_to_binary(SessionIdx))/binary>>.

batched_fanout_ranges(identical_ranges, _SessionIdx) ->
    [{0, 99}];
batched_fanout_ranges(unique_ranges, SessionIdx) ->
    Start = SessionIdx * 2,
    [{Start, Start}].

fanout_everyone_role(GuildId) ->
    #{
        <<"id">> => GuildId,
        <<"name">> => <<"everyone">>,
        <<"permissions">> =>
            constants:view_channel_permission() bor constants:view_channel_members_permission(),
        <<"hoist">> => false,
        <<"position">> => 0
    }.

fanout_channel(ChannelId) ->
    #{
        <<"id">> => ChannelId,
        <<"name">> => <<"fanout">>,
        <<"type">> => 0,
        <<"permission_overwrites">> => []
    }.

fanout_member(UserId) ->
    UserIdBin = integer_to_binary(UserId),
    #{
        <<"user">> => #{
            <<"id">> => UserIdBin,
            <<"username">> => <<"fanout_user_", UserIdBin/binary>>
        },
        <<"roles">> => []
    }.

with_batched_fanout_relay_mock(Fun) ->
    meck:new(gateway_dispatch_relay, [passthrough, no_link]),
    Parent = self(),
    Ref = make_ref(),
    put(batched_fanout_ref, Ref),
    meck:expect(
        gateway_dispatch_relay,
        dispatch_many,
        fun(Pids, guild_member_list_update, Payload, _GuildId) ->
            Parent ! {batched_fanout_dispatch, Ref, length(Pids), payload_bytes(Payload)},
            ok
        end
    ),
    try
        Fun()
    after
        erase(batched_fanout_ref),
        meck:unload(gateway_dispatch_relay)
    end.

payload_bytes({pre_encoded, Bin}) when is_binary(Bin) ->
    byte_size(Bin);
payload_bytes(Bin) when is_binary(Bin) ->
    byte_size(Bin);
payload_bytes(_) ->
    0.

collect_batched_fanout_dispatches(ExpectedCount) ->
    Ref = get(batched_fanout_ref),
    collect_batched_fanout_dispatches(ExpectedCount, Ref, 0, 0, 0).

collect_batched_fanout_dispatches(0, _Ref, Dispatches, Recipients, Bytes) ->
    {Dispatches, Recipients, Bytes};
collect_batched_fanout_dispatches(ExpectedCount, Ref, Dispatches, Recipients, Bytes) ->
    receive
        {batched_fanout_dispatch, Ref, PidCount, PayloadBytes} ->
            collect_batched_fanout_dispatches(
                ExpectedCount - 1,
                Ref,
                Dispatches + 1,
                Recipients + PidCount,
                Bytes + PayloadBytes
            )
    after 30000 ->
        error({batched_fanout_dispatch_timeout, ExpectedCount})
    end.

assert_no_batched_fanout_dispatch() ->
    Ref = get(batched_fanout_ref),
    receive
        {batched_fanout_dispatch, Ref, PidCount, PayloadBytes} ->
            ?assert(false, {unexpected_batched_fanout_dispatch, PidCount, PayloadBytes})
    after 100 ->
        ok
    end.
