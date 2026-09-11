%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_dm_partners_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(GUILD_ID, 42).
-define(VIEWER_ROLE, 1000).
-define(OTHER_ROLE, 2000).
-define(SESSION, <<"session-10">>).
-define(FLAG, dm_presence_mutual_context).

registration_notifies_mutual_partners_test() ->
    with_flag(true, fun() ->
        State = register_partners(state(), [20, 30, 777]),
        ?assertEqual({dm_partner_mutual, ?GUILD_ID, [20]}, receive_mutual()),
        ?assert(maps:is_key(?SESSION, maps:get(dm_partners, State)))
    end).

unknown_session_is_not_registered_test() ->
    with_flag(true, fun() ->
        {noreply, State} = guild_dm_partners:handle_cast(
            {update_dm_partners, <<"missing">>, [20]}, state()
        ),
        ?assertEqual(#{}, maps:get(dm_partners, State, #{})),
        ?assertEqual(none, receive_mutual())
    end).

disabled_flag_does_not_register_test() ->
    with_flag(false, fun() ->
        State = register_partners(state(), [20]),
        ?assertEqual(#{}, maps:get(dm_partners, State, #{})),
        ?assertEqual(none, receive_mutual())
    end).

unchanged_eligibility_is_not_sent_again_test() ->
    with_flag(true, fun() ->
        State = register_partners(state(), [20]),
        {dm_partner_mutual, _, [20]} = receive_mutual(),
        _ = register_partners(State, [20, 777]),
        ?assertEqual(none, receive_mutual())
    end).

partner_leaving_the_guild_withdraws_the_partner_test() ->
    with_flag(true, fun() ->
        Before = register_partners(state(), [20]),
        {dm_partner_mutual, _, [20]} = receive_mutual(),
        After = without_member(20, Before),
        _ = guild_dm_partners:maybe_reevaluate(
            guild_member_remove, #{<<"user">> => #{<<"id">> => <<"20">>}}, Before, After
        ),
        ?assertEqual({dm_partner_mutual, ?GUILD_ID, []}, receive_mutual())
    end).

guild_wide_event_without_visibility_change_does_nothing_test() ->
    with_flag(true, fun() ->
        Before = register_partners(state(), [20]),
        {dm_partner_mutual, _, [20]} = receive_mutual(),
        Renamed = with_channels(
            [
                (channel(500, viewer_overwrites()))#{<<"name">> => <<"renamed">>},
                channel(600, other_overwrites())
            ],
            Before
        ),
        _ = guild_dm_partners:maybe_reevaluate(channel_update, #{}, Before, Renamed),
        ?assertEqual(none, receive_mutual())
    end).

guild_wide_visibility_change_reevaluates_test() ->
    with_flag(true, fun() ->
        Before = register_partners(state(), [30]),
        ?assertEqual(none, receive_mutual()),
        Opened = with_channels(
            [
                channel(500, viewer_overwrites() ++ [overwrite(?OTHER_ROLE, 0)]),
                channel(600, other_overwrites())
            ],
            Before
        ),
        _ = guild_dm_partners:maybe_reevaluate(channel_update, #{}, Before, Opened),
        ?assertEqual({dm_partner_mutual, ?GUILD_ID, [30]}, receive_mutual())
    end).

disconnected_sessions_are_dropped_on_reevaluation_test() ->
    with_flag(true, fun() ->
        Before = register_partners(state(), [20]),
        {dm_partner_mutual, _, [20]} = receive_mutual(),
        Gone = Before#{sessions => #{}},
        After = guild_dm_partners:maybe_reevaluate(guild_role_delete, #{}, Before, Gone#{
            virtual_channel_access => #{1 => sets:new()}
        }),
        ?assertEqual(#{}, maps:get(dm_partners, After))
    end).

register_partners(State, PartnerIds) ->
    {noreply, NewState} = guild_dm_partners:handle_cast(
        {update_dm_partners, ?SESSION, PartnerIds}, State
    ),
    NewState.

state() ->
    Base = #{
        id => ?GUILD_ID,
        virtual_channel_access => #{},
        data => data(
            [channel(500, viewer_overwrites()), channel(600, other_overwrites())], members()
        )
    },
    Viewable = guild_sessions:build_viewable_channel_map(
        guild_visibility:get_user_viewable_channels(10, Base)
    ),
    Base#{
        sessions => #{
            ?SESSION => #{user_id => 10, pid => self(), viewable_channels => Viewable}
        }
    }.

data(Channels, Members) ->
    #{
        <<"guild">> => #{<<"owner_id">> => <<"7">>},
        <<"roles">> => [role(?GUILD_ID), role(?VIEWER_ROLE), role(?OTHER_ROLE)],
        <<"members">> => Members,
        <<"channels">> => Channels,
        <<"channel_index">> => guild_data_index:build_id_index(Channels)
    }.

members() ->
    maps:from_list(
        [{7, member(7, [])}] ++
            [{Id, member(Id, [?VIEWER_ROLE])} || Id <- [10, 20]] ++
            [{30, member(30, [?OTHER_ROLE])}]
    ).

without_member(UserId, #{data := Data} = State) ->
    State#{data => Data#{<<"members">> => maps:remove(UserId, maps:get(<<"members">>, Data))}}.

with_channels(Channels, #{data := Data} = State) ->
    State#{
        data => Data#{
            <<"channels">> => Channels,
            <<"channel_index">> => guild_data_index:build_id_index(Channels)
        }
    }.

role(RoleId) ->
    #{<<"id">> => integer_to_binary(RoleId), <<"permissions">> => <<"0">>}.

member(UserId, RoleIds) ->
    #{
        <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
        <<"roles">> => [integer_to_binary(RoleId) || RoleId <- RoleIds]
    }.

viewer_overwrites() ->
    [overwrite(?VIEWER_ROLE, 0)].

other_overwrites() ->
    [overwrite(?OTHER_ROLE, 0)].

overwrite(TargetId, Type) ->
    #{
        <<"id">> => integer_to_binary(TargetId),
        <<"type">> => Type,
        <<"allow">> => integer_to_binary(constants:view_channel_permission()),
        <<"deny">> => <<"0">>
    }.

channel(ChannelId, Overwrites) ->
    #{
        <<"id">> => integer_to_binary(ChannelId),
        <<"type">> => 0,
        <<"permission_overwrites">> => Overwrites
    }.

receive_mutual() ->
    receive
        {dm_partner_mutual, _, _} = Msg -> Msg
    after 100 ->
        none
    end.

with_flag(Value, Fun) ->
    Previous = application:get_env(fluxer_gateway, ?FLAG),
    application:set_env(fluxer_gateway, ?FLAG, Value),
    try
        Fun()
    after
        restore_flag(Previous)
    end.

restore_flag(undefined) ->
    application:unset_env(fluxer_gateway, ?FLAG);
restore_flag({ok, Value}) ->
    application:set_env(fluxer_gateway, ?FLAG, Value).
