%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_dm_partners_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(SELF, 1).
-define(GUILD, 900).
-define(SESSION_ID, <<"session-1">>).
-define(FLAG, dm_presence_mutual_context).

handle_mutual_stores_partners_for_a_connected_guild_test() ->
    {noreply, State} = session_dm_partners:handle_mutual(?GUILD, [2], state(connected())),
    ?assertEqual(#{?GUILD => #{2 => true}}, maps:get(dm_mutual_by_guild, State)).

handle_mutual_ignores_a_guild_that_is_not_connected_test() ->
    Initial = state(#{}),
    ?assertEqual({noreply, Initial}, session_dm_partners:handle_mutual(?GUILD, [2], Initial)).

handle_mutual_drops_the_guild_when_nothing_is_mutual_test() ->
    {noreply, With} = session_dm_partners:handle_mutual(?GUILD, [2], state(connected())),
    {noreply, Without} = session_dm_partners:handle_mutual(?GUILD, [], With),
    ?assertEqual(#{}, maps:get(dm_mutual_by_guild, Without)).

forget_guild_drops_its_partners_test() ->
    {noreply, With} = session_dm_partners:handle_mutual(?GUILD, [2], state(connected())),
    ?assertEqual(
        #{}, maps:get(dm_mutual_by_guild, session_dm_partners:forget_guild(?GUILD, With))
    ).

register_all_sends_direct_partners_to_every_connected_guild_test() ->
    with_flag(true, fun() ->
        _ = session_dm_partners:register_all(state(connected())),
        ?assertEqual({update_dm_partners, ?SESSION_ID, [2, 3]}, receive_cast())
    end).

register_guild_sends_direct_partners_to_that_guild_test() ->
    with_flag(true, fun() ->
        _ = session_dm_partners:register_guild(?GUILD, self(), state(#{})),
        ?assertEqual({update_dm_partners, ?SESSION_ID, [2, 3]}, receive_cast())
    end).

register_guild_without_direct_partners_stays_silent_test() ->
    with_flag(true, fun() ->
        State = (state(#{}))#{channels => #{300 => channel(3, [?SELF, 5, 6])}},
        _ = session_dm_partners:register_guild(?GUILD, self(), State),
        ?assertEqual(none, receive_cast())
    end).

register_all_is_inert_when_disabled_test() ->
    with_flag(false, fun() ->
        _ = session_dm_partners:register_all(state(connected())),
        ?assertEqual(none, receive_cast())
    end).

bots_never_register_test() ->
    with_flag(true, fun() ->
        _ = session_dm_partners:register_all((state(connected()))#{bot => true}),
        ?assertEqual(none, receive_cast())
    end).

state(Guilds) ->
    #{
        id => ?SESSION_ID,
        user_id => ?SELF,
        presence_pid => undefined,
        guilds => Guilds,
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

receive_cast() ->
    receive
        {'$gen_cast', Msg} -> Msg
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
