%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_state_channel_resync_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(GUILD_ID, 9100).
-define(CHANNEL_ID, 9500).
-define(ROLE_ID, 9200).
-define(USER_A, 9010).
-define(USER_B, 9020).
-define(USER_C, 9030).

channel_update_syncs_every_subscriber_after_permission_change_test() ->
    with_relay_mock(fun run_channel_update_syncs_every_subscriber/0).

channel_update_bulk_syncs_every_subscriber_after_permission_change_test() ->
    with_relay_mock(fun run_channel_update_bulk_syncs_every_subscriber/0).

run_channel_update_syncs_every_subscriber() ->
    run_resync_case(channel_update, denied_channel()).

run_channel_update_bulk_syncs_every_subscriber() ->
    run_resync_case(channel_update_bulk, #{<<"channels">> => [denied_channel()]}).

run_resync_case(Event, EventData) ->
    PidA = spawn(fun idle/0),
    PidB = spawn(fun idle/0),
    SubsTab = guild_member_list_subs:new(),
    State0 = base_state(SubsTab, PidA, PidB),
    {State1, _SyncA, _RangesA} = guild_member_list:subscribe_ranges(
        <<"s_a">>, list_id(), [{0, 99}], State0
    ),
    {State2, _SyncB, _RangesB} = guild_member_list:subscribe_ranges(
        <<"s_b">>, list_id(), [{0, 99}], State1
    ),
    try
        ?assertEqual({3, 0}, guild_member_list:get_counts(list_id(), State2)),
        {noreply, UpdatedState} = guild_dispatch:handle_dispatch(Event, EventData, State2),
        ?assertEqual({2, 0}, guild_member_list:get_counts(list_id(), UpdatedState)),
        {Pids, Raw, Payload} = receive_member_list_dispatch(),
        ?assertEqual(lists:sort([PidA, PidB]), lists:sort(Pids)),
        ?assertEqual(list_id(), maps:get(<<"channel_id">>, Payload)),
        ?assertEqual(list_id(), maps:get(<<"id">>, Payload)),
        ?assertEqual(2, maps:get(<<"member_count">>, Payload)),
        ?assertEqual(nomatch, binary:match(Raw, integer_to_binary(?USER_C))),
        assert_no_further_dispatch(),
        guild_member_list_channel_engine:destroy_all(UpdatedState)
    after
        PidA ! stop,
        PidB ! stop,
        ets:delete(SubsTab)
    end.

base_state(SubsTab, PidA, PidB) ->
    #{
        id => ?GUILD_ID,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(?GUILD_ID),
                    <<"permissions">> => integer_to_binary(viewer_permissions())
                },
                #{<<"id">> => integer_to_binary(?ROLE_ID), <<"permissions">> => <<"0">>}
            ],
            <<"members">> => [
                member(?USER_A, []),
                member(?USER_B, []),
                member(?USER_C, [integer_to_binary(?ROLE_ID)])
            ],
            <<"channels">> => [visible_channel()]
        },
        sessions => #{
            <<"s_a">> => session(<<"s_a">>, ?USER_A, PidA),
            <<"s_b">> => session(<<"s_b">>, ?USER_B, PidB)
        },
        member_presence => #{},
        member_list_subscriptions => SubsTab
    }.

session(SessionId, UserId, Pid) ->
    #{
        session_id => SessionId,
        user_id => UserId,
        pid => Pid,
        viewable_channels => #{?CHANNEL_ID => true}
    }.

member(UserId, Roles) ->
    #{
        <<"user">> => #{
            <<"id">> => integer_to_binary(UserId),
            <<"username">> => <<"u", (integer_to_binary(UserId))/binary>>
        },
        <<"roles">> => Roles
    }.

visible_channel() ->
    #{<<"id">> => list_id(), <<"type">> => 0, <<"permission_overwrites">> => []}.

denied_channel() ->
    #{
        <<"id">> => list_id(),
        <<"type">> => 0,
        <<"permission_overwrites">> => [
            #{
                <<"id">> => integer_to_binary(?ROLE_ID),
                <<"type">> => 0,
                <<"allow">> => <<"0">>,
                <<"deny">> => integer_to_binary(constants:view_channel_permission())
            }
        ]
    }.

viewer_permissions() ->
    constants:view_channel_permission() bor constants:view_channel_members_permission().

list_id() ->
    integer_to_binary(?CHANNEL_ID).

with_relay_mock(Fun) ->
    meck:new(gateway_dispatch_relay, [passthrough, no_link]),
    Parent = self(),
    meck:expect(
        gateway_dispatch_relay,
        dispatch_many,
        fun
            (Pids, guild_member_list_update, {pre_encoded, Bin}, GuildId) when is_binary(Bin) ->
                Parent ! {member_list_dispatch, Pids, guild_member_list_update, GuildId, Bin},
                ok;
            (_Pids, _Event, _Payload, _GuildId) ->
                ok
        end
    ),
    try
        Fun()
    after
        meck:unload(gateway_dispatch_relay)
    end.

receive_member_list_dispatch() ->
    receive
        {member_list_dispatch, Pids, guild_member_list_update, ?GUILD_ID, Bin} ->
            {Pids, Bin, json:decode(Bin)}
    after 1000 ->
        ?assert(false, no_member_list_sync_dispatched),
        {[], <<>>, #{}}
    end.

assert_no_further_dispatch() ->
    receive
        {member_list_dispatch, _Pids, _Event, _GuildId, _Bin} = Msg ->
            ?assert(false, {unexpected_member_list_dispatch, Msg})
    after 0 ->
        ok
    end.

idle() ->
    receive
        stop -> ok
    after 30000 -> ok
    end.
