%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_targets_dm_partner_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(SELF, 1).
-define(GUILD, 900).
-define(FLAG, dm_presence_mutual_context).

eligible_direct_dm_is_a_presence_target_test() ->
    with_flag(true, fun() ->
        State = state(#{?GUILD => #{2 => true}}, connected()),
        ?assertEqual(
            #{100 => #{2 => true}, 300 => #{5 => true, 6 => true}},
            presence_targets:group_dm_recipients_from_state(State)
        )
    end).

direct_dm_without_mutual_context_is_not_a_target_test() ->
    with_flag(true, fun() ->
        State = state(#{}, connected()),
        ?assertEqual(
            #{300 => #{5 => true, 6 => true}},
            presence_targets:group_dm_recipients_from_state(State)
        )
    end).

mutual_context_from_a_disconnected_guild_is_ignored_test() ->
    with_flag(true, fun() ->
        State = state(#{?GUILD => #{2 => true}}, #{?GUILD => undefined}),
        ?assertEqual(
            #{300 => #{5 => true, 6 => true}},
            presence_targets:group_dm_recipients_from_state(State)
        )
    end).

disabled_flag_keeps_direct_dms_out_test() ->
    with_flag(false, fun() ->
        State = state(#{?GUILD => #{2 => true, 3 => true}}, connected()),
        ?assertEqual(
            #{300 => #{5 => true, 6 => true}},
            presence_targets:group_dm_recipients_from_state(State)
        )
    end).

user_allowlist_gates_direct_dms_test() ->
    State = state(#{?GUILD => #{2 => true}}, connected()),
    with_flag({users, [?SELF]}, fun() ->
        ?assert(maps:is_key(100, presence_targets:group_dm_recipients_from_state(State)))
    end),
    with_flag({users, [42]}, fun() ->
        ?assertNot(maps:is_key(100, presence_targets:group_dm_recipients_from_state(State)))
    end).

direct_dm_partner_ids_lists_one_to_one_partners_only_test() ->
    ?assertEqual([2, 3], presence_targets:direct_dm_partner_ids(state(#{}, connected()))).

state(MutualByGuild, Guilds) ->
    #{
        user_id => ?SELF,
        guilds => Guilds,
        dm_mutual_by_guild => MutualByGuild,
        channels => #{
            100 => channel(1, [?SELF, 2]),
            200 => channel(1, [?SELF, 3]),
            300 => channel(3, [?SELF, 5, 6])
        }
    }.

channel(Type, RecipientIds) ->
    #{
        <<"type">> => Type,
        <<"recipients">> => [#{<<"id">> => integer_to_binary(Id)} || Id <- RecipientIds]
    }.

connected() ->
    #{?GUILD => {self(), make_ref()}}.

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
