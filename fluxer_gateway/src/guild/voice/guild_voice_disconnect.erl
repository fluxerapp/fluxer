%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_disconnect).
-typing([eqwalizer]).

-export([handle_voice_disconnect/5]).
-export([force_disconnect_participant/4]).
-export([disconnect_voice_user/2]).
-export([disconnect_voice_user_if_in_channel/2]).
-export([disconnect_all_voice_users_in_channel/2]).
-export([cleanup_virtual_channel_access_for_user/2]).
-export([recently_disconnected_voice_states/1]).
-export([clear_recently_disconnected/2]).
-export([clear_recently_disconnected_for_channel/2]).

-export_type([
    guild_state/0,
    voice_state_map/0,
    voice_reply/0
]).

-type guild_state() :: map().
-type voice_state_map() :: #{binary() => map()}.
-type voice_reply() :: {reply, map() | {error, atom(), atom()}, guild_state()}.

-spec handle_voice_disconnect(
    binary() | undefined, term(), integer(), voice_state_map() | term(), guild_state()
) -> voice_reply().
handle_voice_disconnect(ConnId, SessionId, UserId, VoiceStates, State) ->
    guild_voice_disconnect_user:handle_voice_disconnect(
        ConnId, SessionId, UserId, VoiceStates, State
    ).

-spec force_disconnect_participant(integer(), integer(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
force_disconnect_participant(GuildId, ChannelId, UserId, ConnectionId) ->
    guild_voice_disconnect_user:force_disconnect_participant(
        GuildId, ChannelId, UserId, ConnectionId
    ).

-spec disconnect_voice_user(map(), guild_state()) -> voice_reply().
disconnect_voice_user(Request, State) ->
    guild_voice_disconnect_user:disconnect_voice_user(Request, State).

-spec disconnect_voice_user_if_in_channel(map(), guild_state()) -> voice_reply().
disconnect_voice_user_if_in_channel(Request, State) ->
    guild_voice_disconnect_user:disconnect_voice_user_if_in_channel(Request, State).

-spec disconnect_all_voice_users_in_channel(map(), guild_state()) ->
    voice_reply().
disconnect_all_voice_users_in_channel(Request, State) ->
    guild_voice_disconnect_channel:disconnect_all_voice_users_in_channel(Request, State).

-spec cleanup_virtual_channel_access_for_user(integer(), guild_state()) -> guild_state().
cleanup_virtual_channel_access_for_user(UserId, State) ->
    guild_voice_disconnect_user:cleanup_virtual_channel_access_for_user(UserId, State).

-spec recently_disconnected_voice_states(guild_state()) -> map().
recently_disconnected_voice_states(State) ->
    guild_voice_disconnect_broadcast:recently_disconnected_voice_states(State).

-spec clear_recently_disconnected(binary(), guild_state()) -> guild_state().
clear_recently_disconnected(ConnectionId, State) ->
    guild_voice_disconnect_broadcast:clear_recently_disconnected(ConnectionId, State).

-spec clear_recently_disconnected_for_channel(integer(), guild_state()) -> guild_state().
clear_recently_disconnected_for_channel(ChannelId, State) ->
    guild_voice_disconnect_broadcast:clear_recently_disconnected_for_channel(ChannelId, State).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

voice_state_fixture(UserId, GuildId, ChannelId) ->
    #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"channel_id">> => integer_to_binary(ChannelId)
    }.

collect_force_disconnect_messages(0, Acc) ->
    lists:reverse(Acc);
collect_force_disconnect_messages(Count, Acc) when Count > 0 ->
    receive
        {force_disconnect, _, _, _, _} = Msg ->
            collect_force_disconnect_messages(Count - 1, [Msg | Acc])
    after 200 -> lists:reverse(Acc)
    end.
collect_force_disconnect_messages(Count) -> collect_force_disconnect_messages(Count, []).

test_force_disconnect_fun() ->
    Self = self(),
    fun(GId, CId, UId, ConnId) ->
        Self ! {force_disconnect, GId, CId, UId, ConnId},
        {ok, #{success => true}}
    end.

assert_disconnect_clears_voice_states(UserId, State) ->
    {reply, #{success := true}, #{voice_states := #{}}} =
        disconnect_voice_user(#{user_id => UserId, connection_id => null}, State).

disconnect_voice_user_removes_all_connections_test() ->
    VoiceStates = #{
        <<"a">> => voice_state_fixture(5, 10, 20),
        <<"b">> => voice_state_fixture(5, 10, 21)
    },
    State = #{
        voice_states => VoiceStates,
        test_force_disconnect_fun => fun(_, _, _, _) -> {ok, #{success => true}} end
    },
    assert_disconnect_clears_voice_states(5, State).

disconnect_voice_user_with_session_id_test() ->
    TestFun = test_force_disconnect_fun(),
    VoiceStates = #{
        <<"a">> => (voice_state_fixture(5, 10, 20))#{<<"session_id">> => <<"sess-a">>},
        <<"b">> => (voice_state_fixture(5, 10, 21))#{<<"session_id">> => <<"sess-b">>},
        <<"c">> => (voice_state_fixture(6, 10, 22))#{<<"session_id">> => <<"sess-a">>}
    },
    State = #{id => 10, voice_states => VoiceStates, test_force_disconnect_fun => TestFun},
    {reply, #{success := true}, NewState} =
        disconnect_voice_user(
            #{user_id => 5, connection_id => null, session_id => <<"sess-a">>}, State
        ),
    ?assertEqual(
        [{force_disconnect, 10, 20, 5, <<"a">>}], collect_force_disconnect_messages(1)
    ),
    Remaining = maps:get(voice_states, NewState),
    ?assertNot(maps:is_key(<<"a">>, Remaining)),
    ?assert(maps:is_key(<<"b">>, Remaining)),
    ?assert(maps:is_key(<<"c">>, Remaining)).

handle_voice_disconnect_invalid_state_test() ->
    VoiceState = #{<<"user_id">> => <<"5">>},
    State = #{voice_states => #{<<"conn">> => VoiceState}},
    {reply, {error, validation_error, _}, _} =
        handle_voice_disconnect(<<"conn">>, undefined, 5, #{<<"conn">> => VoiceState}, State).

disconnect_voice_user_if_in_channel_ignored_test() ->
    State = #{voice_states => #{}},
    {reply, #{ignored := true}, _} =
        disconnect_voice_user_if_in_channel(#{user_id => 5, expected_channel_id => 99}, State).

recently_disconnected_test() ->
    ?assertEqual(#{}, recently_disconnected_voice_states(#{})),
    Cache = #{<<"conn">> => #{voice_state => #{}, disconnected_at => 1000}},
    ?assertEqual(
        Cache,
        recently_disconnected_voice_states(
            #{recently_disconnected_voice_states => Cache}
        )
    ).

cache_and_clear_recently_disconnected_test() ->
    VS = voice_state_fixture(5, 10, 20),
    S1 = guild_voice_disconnect_broadcast:cache_recently_disconnected(#{<<"conn">> => VS}, #{}),
    ?assert(maps:is_key(<<"conn">>, recently_disconnected_voice_states(S1))),
    S2 = clear_recently_disconnected(<<"conn">>, S1),
    ?assertEqual(#{}, recently_disconnected_voice_states(S2)).

clear_recently_disconnected_for_channel_test() ->
    VS1 = voice_state_fixture(5, 10, 20),
    VS2 = voice_state_fixture(6, 10, 30),
    S0 = guild_voice_disconnect_broadcast:cache_recently_disconnected(
        #{<<"a">> => VS1, <<"b">> => VS2}, #{}
    ),
    S1 = clear_recently_disconnected_for_channel(20, S0),
    Cache = recently_disconnected_voice_states(S1),
    ?assertNot(maps:is_key(<<"a">>, Cache)),
    ?assert(maps:is_key(<<"b">>, Cache)).

disconnect_voice_user_if_in_channel_caches_test() ->
    VS = voice_state_fixture(5, 10, 20),
    State = #{voice_states => #{<<"conn">> => VS}},
    {reply, #{success := true}, NewState} =
        disconnect_voice_user_if_in_channel(
            #{user_id => 5, expected_channel_id => 20, connection_id => <<"conn">>}, State
        ),
    ?assert(maps:is_key(<<"conn">>, recently_disconnected_voice_states(NewState))).

disconnect_voice_user_force_disconnect_all_test() ->
    TestFun = test_force_disconnect_fun(),
    VoiceStates = #{
        <<"a">> => voice_state_fixture(5, 10, 20),
        <<"b">> => voice_state_fixture(5, 10, 21)
    },
    State = #{id => 10, voice_states => VoiceStates, test_force_disconnect_fun => TestFun},
    assert_disconnect_clears_voice_states(5, State),
    Msgs = collect_force_disconnect_messages(2),
    ?assertEqual(2, length(Msgs)).

disconnect_all_voice_users_in_channel_test() ->
    TestFun = test_force_disconnect_fun(),
    VoiceStates = #{
        <<"a">> => voice_state_fixture(5, 10, 20),
        <<"b">> => voice_state_fixture(6, 10, 20),
        <<"c">> => voice_state_fixture(7, 10, 30)
    },
    State = #{id => 10, voice_states => VoiceStates, test_force_disconnect_fun => TestFun},
    {reply, #{success := true, disconnected_count := 2}, NewState} =
        disconnect_all_voice_users_in_channel(#{channel_id => 20}, State),
    _ = collect_force_disconnect_messages(2),
    Remaining = maps:get(voice_states, NewState),
    ?assert(maps:is_key(<<"c">>, Remaining)),
    ?assertNot(maps:is_key(<<"a">>, Remaining)).

pending_connection_tests_test() ->
    Pending = #{
        <<"conn1">> => #{user_id => 5, channel_id => 100},
        <<"conn2">> => #{user_id => 6, channel_id => 100}
    },
    State = #{pending_voice_connections => Pending, voice_states => #{}},
    S1 = guild_voice_disconnect_broadcast:clear_pending_voice_connection(<<"conn1">>, State),
    ?assertNot(maps:is_key(<<"conn1">>, maps:get(pending_voice_connections, S1))),
    S2 = guild_voice_disconnect_broadcast:clear_pending_voice_connection(<<"missing">>, State),
    ?assertEqual(State, S2).

disconnect_voice_user_cleans_pending_test() ->
    Pending = #{
        <<"conn1">> => #{user_id => 5, channel_id => 100},
        <<"conn2">> => #{user_id => 6, channel_id => 100}
    },
    State = #{id => 10, voice_states => #{}, pending_voice_connections => Pending},
    {reply, #{success := true}, NewState} = disconnect_voice_user(#{user_id => 5}, State),
    NewPending = maps:get(pending_voice_connections, NewState),
    ?assertNot(maps:is_key(<<"conn1">>, NewPending)),
    ?assert(maps:is_key(<<"conn2">>, NewPending)).

-endif.
