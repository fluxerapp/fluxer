%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_connection_confirm_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

required_voice_perms() ->
    constants:view_channel_permission() bor constants:connect_permission().

base_test_member(UserId) ->
    #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}}.

base_test_channel(ChannelId) ->
    #{<<"id">> => integer_to_binary(ChannelId), <<"type">> => 2, <<"user_limit">> => 0}.

base_test_state() ->
    #{
        id => 999,
        data => #{
            <<"channels">> => [base_test_channel(100)],
            <<"members">> => [base_test_member(10)]
        },
        voice_states => #{},
        test_perm_fun => fun(_) -> required_voice_perms() end
    }.

confirm_voice_connection_missing_id_test() ->
    State = base_test_state(),
    {reply, {error, validation_error, voice_missing_connection_id}, _} =
        guild_voice_connection_confirm:confirm_voice_connection_from_livekit(#{}, State).

confirm_voice_connection_moves_pending_to_voice_states_test() ->
    VoiceState = #{
        <<"user_id">> => <<"5">>, <<"guild_id">> => <<"999">>, <<"channel_id">> => <<"100">>
    },
    PendingData = #{
        user_id => 5, guild_id => 999, channel_id => 100, voice_state => VoiceState
    },
    State = maps:merge(base_test_state(), #{
        pending_voice_connections => #{<<"conn1">> => PendingData}, voice_states => #{}
    }),
    NewState = confirm_success(#{connection_id => <<"conn1">>}, State),
    NewVoiceStates = maps:get(voice_states, NewState),
    NewPending = maps:get(pending_voice_connections, NewState, #{}),
    ?assert(maps:is_key(<<"conn1">>, NewVoiceStates)),
    ?assertNot(maps:is_key(<<"conn1">>, NewPending)).

confirm_voice_connection_clears_pending_even_without_voice_state_test() ->
    PendingData = #{user_id => 5, channel_id => 100},
    State = maps:merge(base_test_state(), #{
        pending_voice_connections => #{<<"conn1">> => PendingData}, voice_states => #{}
    }),
    NewState = confirm_success(#{connection_id => <<"conn1">>}, State),
    NewPending = maps:get(pending_voice_connections, NewState, #{}),
    ?assertNot(maps:is_key(<<"conn1">>, NewPending)).

confirm_voice_connection_not_found_in_pending_test() ->
    State = maps:merge(base_test_state(), #{pending_voice_connections => #{}}),
    assert_not_found(#{connection_id => <<"missing">>}, State).

confirm_voice_connection_found_in_voice_states_test() ->
    VoiceState = #{
        <<"user_id">> => <<"5">>, <<"guild_id">> => <<"999">>, <<"channel_id">> => <<"200">>
    },
    State = maps:merge(base_test_state(), #{
        pending_voice_connections => #{}, voice_states => #{<<"conn1">> => VoiceState}
    }),
    NewState = confirm_success(#{connection_id => <<"conn1">>}, State),
    NewVoiceStates = maps:get(voice_states, NewState),
    ?assert(maps:is_key(<<"conn1">>, NewVoiceStates)),
    ?assertEqual(VoiceState, maps:get(<<"conn1">>, NewVoiceStates)).

try_restore_from_recently_disconnected_restores_test() ->
    VS = #{
        <<"user_id">> => <<"5">>,
        <<"guild_id">> => <<"10">>,
        <<"channel_id">> => <<"20">>,
        <<"connection_id">> => <<"conn">>
    },
    Now = erlang:system_time(millisecond),
    Cache = #{<<"conn">> => #{voice_state => VS, disconnected_at => Now - 5000}},
    State = #{
        voice_states => #{},
        recently_disconnected_voice_states => Cache,
        sessions => #{},
        data => #{},
        id => 10
    },
    NewState = confirm_success(#{connection_id => <<"conn">>}, State),
    NewVoiceStates = maps:get(voice_states, NewState),
    ?assert(maps:is_key(<<"conn">>, NewVoiceStates)),
    NewCache = maps:get(recently_disconnected_voice_states, NewState),
    ?assertNot(maps:is_key(<<"conn">>, NewCache)).

try_restore_from_recently_disconnected_expired_test() ->
    VS = #{
        <<"user_id">> => <<"5">>,
        <<"guild_id">> => <<"10">>,
        <<"channel_id">> => <<"20">>,
        <<"connection_id">> => <<"conn">>
    },
    Now = erlang:system_time(millisecond),
    Cache = #{<<"conn">> => #{voice_state => VS, disconnected_at => Now - 70000}},
    State = #{voice_states => #{}, recently_disconnected_voice_states => Cache, data => #{}},
    assert_not_found(#{connection_id => <<"conn">>}, State).

try_restore_from_recently_disconnected_not_found_test() ->
    State = #{voice_states => #{}, data => #{}},
    assert_not_found(#{connection_id => <<"conn">>}, State).

confirm_voice_connection_validates_nonce_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = pending_data_with_nonce(Now),
    State = maps:merge(base_test_state(), #{
        pending_voice_connections => #{<<"conn1">> => PendingData}, voice_states => #{}
    }),
    {reply, {error, validation_error, voice_nonce_mismatch}, _} =
        guild_voice_connection_confirm:confirm_voice_connection_from_livekit(
            #{connection_id => <<"conn1">>, token_nonce => <<"wrong-nonce">>}, State
        ).

confirm_voice_connection_validates_expiry_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = (pending_data_with_nonce(Now))#{
        created_at => Now - 35000, expires_at => Now - 5000
    },
    State = maps:merge(base_test_state(), #{
        pending_voice_connections => #{<<"conn1">> => PendingData}, voice_states => #{}
    }),
    {reply, {error, validation_error, voice_pending_expired}, _} =
        guild_voice_connection_confirm:confirm_voice_connection_from_livekit(
            #{connection_id => <<"conn1">>, token_nonce => <<"valid-nonce">>}, State
        ).

confirm_voice_connection_accepts_valid_nonce_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = pending_data_with_nonce(Now),
    State = maps:merge(base_test_state(), #{
        pending_voice_connections => #{<<"conn1">> => PendingData}, voice_states => #{}
    }),
    NewState = confirm_success(
        #{connection_id => <<"conn1">>, token_nonce => <<"valid-nonce">>}, State
    ),
    NewVoiceStates = maps:get(voice_states, NewState),
    ?assert(maps:is_key(<<"conn1">>, NewVoiceStates)).

confirm_voice_connection_rejects_missing_nonce_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = pending_data_with_nonce(Now),
    State = maps:merge(base_test_state(), #{
        pending_voice_connections => #{<<"conn1">> => PendingData}, voice_states => #{}
    }),
    {reply, {error, validation_error, voice_nonce_mismatch}, _} =
        guild_voice_connection_confirm:confirm_voice_connection_from_livekit(
            #{connection_id => <<"conn1">>}, State
        ).

pending_data_with_nonce(Now) ->
    #{
        user_id => 5,
        guild_id => 999,
        channel_id => 100,
        token_nonce => <<"valid-nonce">>,
        created_at => Now - 5000,
        expires_at => Now + 25000,
        voice_state => #{
            <<"user_id">> => <<"5">>, <<"guild_id">> => <<"999">>, <<"channel_id">> => <<"100">>
        }
    }.

confirm_success(Data, State) ->
    {reply, #{success := true}, NewState} =
        guild_voice_connection_confirm:confirm_voice_connection_from_livekit(Data, State),
    NewState.

assert_not_found(Data, State) ->
    {reply, {error, not_found, voice_connection_not_found}, _} =
        guild_voice_connection_confirm:confirm_voice_connection_from_livekit(Data, State).
