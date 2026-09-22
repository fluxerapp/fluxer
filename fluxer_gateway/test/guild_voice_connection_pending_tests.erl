%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_connection_pending_tests).
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

sweep_expired_pending_joins_removes_expired_test() ->
    Now = erlang:system_time(millisecond),
    Expired = #{user_id => 10, guild_id => 999, channel_id => 100, expires_at => Now - 1000},
    Valid = #{user_id => 11, guild_id => 999, channel_id => 101, expires_at => Now + 25000},
    PendingConnections = #{<<"expired-conn">> => Expired, <<"valid-conn">> => Valid},
    State = maps:put(pending_voice_connections, PendingConnections, base_test_state()),
    NewState = guild_voice_connection:sweep_expired_pending_joins(State),
    NewPending = maps:get(pending_voice_connections, NewState, #{}),
    ?assertNot(maps:is_key(<<"expired-conn">>, NewPending)),
    ?assert(maps:is_key(<<"valid-conn">>, NewPending)).

sweep_expired_pending_joins_keeps_valid_test() ->
    Now = erlang:system_time(millisecond),
    V1 = #{user_id => 10, guild_id => 999, channel_id => 100, expires_at => Now + 25000},
    V2 = #{user_id => 11, guild_id => 999, channel_id => 101, expires_at => Now + 30000},
    PendingConnections = #{<<"conn-1">> => V1, <<"conn-2">> => V2},
    State = maps:put(pending_voice_connections, PendingConnections, base_test_state()),
    NewState = guild_voice_connection:sweep_expired_pending_joins(State),
    NewPending = maps:get(pending_voice_connections, NewState, #{}),
    ?assertEqual(2, maps:size(NewPending)),
    ?assert(maps:is_key(<<"conn-1">>, NewPending)),
    ?assert(maps:is_key(<<"conn-2">>, NewPending)).

sweep_expired_pending_joins_clears_virtual_access_test() ->
    Now = erlang:system_time(millisecond),
    ExpiredMetadata = #{
        user_id => 10, guild_id => 999, channel_id => 100, expires_at => Now - 1000
    },
    PendingConnections = #{<<"expired-conn">> => ExpiredMetadata},
    State = maps:put(pending_voice_connections, PendingConnections, base_test_state()),
    State1 = guild_virtual_channel_access:mark_pending_join(10, 100, State),
    State2 = guild_virtual_channel_access:mark_preserve(10, 100, State1),
    State3 = guild_virtual_channel_access:mark_move_pending(10, 100, State2),
    ?assert(guild_virtual_channel_access:is_pending_join(10, 100, State3)),
    ?assert(guild_virtual_channel_access:has_preserve(10, 100, State3)),
    ?assert(guild_virtual_channel_access:is_move_pending(10, 100, State3)),
    NewState = guild_voice_connection:sweep_expired_pending_joins(State3),
    ?assertNot(guild_virtual_channel_access:is_pending_join(10, 100, NewState)),
    ?assertNot(guild_virtual_channel_access:has_preserve(10, 100, NewState)),
    ?assertNot(guild_virtual_channel_access:is_move_pending(10, 100, NewState)).

sweep_expired_pending_joins_empty_map_test() ->
    State = maps:put(pending_voice_connections, #{}, base_test_state()),
    NewState = guild_voice_connection:sweep_expired_pending_joins(State),
    NewPending = maps:get(pending_voice_connections, NewState, #{}),
    ?assertEqual(0, maps:size(NewPending)).

sweep_expired_pending_joins_missing_user_id_test() ->
    Now = erlang:system_time(millisecond),
    InvalidMetadata = #{guild_id => 999, channel_id => 100, expires_at => Now - 1000},
    PendingConnections = #{<<"invalid-conn">> => InvalidMetadata},
    State = maps:put(pending_voice_connections, PendingConnections, base_test_state()),
    NewState = guild_voice_connection:sweep_expired_pending_joins(State),
    NewPending = maps:get(pending_voice_connections, NewState, #{}),
    ?assertNot(maps:is_key(<<"invalid-conn">>, NewPending)).

build_context_normalizes_fields_test() ->
    Request = #{
        user_id => <<"42">>,
        channel_id => <<"99">>,
        connection_id => <<"conn">>,
        self_mute => true,
        self_deaf => <<"nope">>,
        self_video => true,
        self_stream => false,
        is_mobile => <<"yes">>
    },
    Context = guild_voice_connection_util:build_context(Request),
    ?assertEqual(42, maps:get(user_id, Context)),
    ?assertEqual(99, maps:get(channel_id, Context)),
    ?assertEqual(<<"conn">>, maps:get(connection_id, Context)),
    ?assertEqual(true, maps:get(self_mute, Context)),
    ?assertEqual(false, maps:get(self_deaf, Context)),
    ?assertEqual(true, maps:get(self_video, Context)),
    ?assertEqual(false, maps:get(self_stream, Context)),
    ?assertEqual(false, maps:get(is_mobile, Context)).

build_context_normalizes_null_connection_id_test() ->
    Context = guild_voice_connection_util:build_context(#{
        user_id => 42,
        channel_id => null,
        connection_id => null
    }),
    ?assertEqual(undefined, maps:get(connection_id, Context)),
    ?assertNot(maps:is_key(raw_connection_id, Context)).

build_context_does_not_accept_zero_ids_test() ->
    Context = guild_voice_connection_util:build_context(#{
        user_id => 0,
        channel_id => <<"0">>
    }),
    ?assertEqual(undefined, maps:get(user_id, Context)),
    ?assertEqual(undefined, maps:get(channel_id, Context)).

build_context_does_not_accept_leading_zero_ids_test() ->
    Context = guild_voice_connection_util:build_context(#{
        user_id => <<"042">>,
        channel_id => <<"099">>
    }),
    ?assertEqual(undefined, maps:get(user_id, Context)),
    ?assertEqual(undefined, maps:get(channel_id, Context)).

resolve_guild_identity_prefers_data_test() ->
    State = #{
        id => 7, data => #{<<"id">> => <<"555">>, <<"guild">> => #{<<"id">> => <<"111">>}}
    },
    ?assertMatch(
        {ok, 555, <<"555">>}, guild_voice_connection_util:resolve_guild_identity(State)
    ).

resolve_guild_identity_does_not_use_zero_state_id_test() ->
    ?assertEqual(
        {error, voice_guild_id_missing},
        guild_voice_connection_util:resolve_guild_identity(#{id => 0, data => #{}})
    ),
    ?assertEqual(
        {error, voice_guild_id_missing},
        guild_voice_connection_util:resolve_guild_identity(#{id => <<"007">>, data => #{}})
    ).

normalize_session_id_test() ->
    ?assertEqual(undefined, guild_voice_connection_util:normalize_session_id(undefined)),
    ?assertEqual(undefined, guild_voice_connection_util:normalize_session_id(null)),
    ?assertEqual(<<"abc">>, guild_voice_connection_util:normalize_session_id(<<"abc">>)),
    ?assertEqual(<<"123">>, guild_voice_connection_util:normalize_session_id(123)),
    ?assertEqual(<<"test">>, guild_voice_connection_util:normalize_session_id("test")).

normalize_boolean_test() ->
    ?assertEqual(true, guild_voice_connection_normalize:normalize_boolean(true)),
    ?assertEqual(true, guild_voice_connection_normalize:normalize_boolean(<<"true">>)),
    ?assertEqual(false, guild_voice_connection_normalize:normalize_boolean(false)),
    ?assertEqual(false, guild_voice_connection_normalize:normalize_boolean(<<"false">>)),
    ?assertEqual(false, guild_voice_connection_normalize:normalize_boolean(<<"other">>)),
    ?assertEqual(false, guild_voice_connection_normalize:normalize_boolean(123)).

resolve_voice_state_from_pending_uses_stored_voice_state_test() ->
    VoiceState = #{
        <<"user_id">> => <<"5">>, <<"guild_id">> => <<"999">>, <<"channel_id">> => <<"100">>
    },
    PendingData = #{
        user_id => 5, guild_id => 999, channel_id => 100, voice_state => VoiceState
    },
    State = base_test_state(),
    Result = guild_voice_connection_pending:resolve_voice_state_from_pending(
        <<"conn1">>, PendingData, State, #{}
    ),
    ?assertEqual(VoiceState, Result).

resolve_voice_state_from_pending_prefers_existing_voice_state_test() ->
    ExistingVoiceState = #{
        <<"user_id">> => <<"5">>,
        <<"guild_id">> => <<"999">>,
        <<"channel_id">> => <<"100">>,
        <<"existing">> => true
    },
    PendingVoiceState = #{
        <<"user_id">> => <<"5">>,
        <<"guild_id">> => <<"999">>,
        <<"channel_id">> => <<"100">>,
        <<"existing">> => false
    },
    PendingData = #{user_id => 5, voice_state => PendingVoiceState},
    VoiceStates = #{<<"conn1">> => ExistingVoiceState},
    State = base_test_state(),
    Result = guild_voice_connection_pending:resolve_voice_state_from_pending(
        <<"conn1">>, PendingData, State, VoiceStates
    ),
    ?assertEqual(ExistingVoiceState, Result).

resolve_voice_state_from_pending_rejects_zero_pending_ids_test() ->
    Result = guild_voice_connection_pending:resolve_voice_state_from_pending(
        <<"conn1">>, #{user_id => 0, guild_id => 999, channel_id => 100}, base_test_state(), #{}
    ),
    ?assertEqual(undefined, Result).

resolve_voice_state_from_pending_rejects_leading_zero_pending_ids_test() ->
    Result = guild_voice_connection_pending:resolve_voice_state_from_pending(
        <<"conn1">>,
        #{user_id => <<"05">>, guild_id => 999, channel_id => 100},
        base_test_state(),
        #{}
    ),
    ?assertEqual(undefined, Result).

get_pending_joins_for_channel_ignores_missing_user_id_test() ->
    State = #{
        pending_voice_connections => #{
            <<"missing-user">> => #{channel_id => 100},
            <<"valid">> => #{channel_id => 100, user_id => 42, token_nonce => <<"nonce">>}
        }
    },
    {reply, #{pending_joins := PendingJoins}, _} = guild_voice_handler:handle_call(
        {get_pending_joins_for_channel, <<"100">>}, {self(), make_ref()}, State
    ),
    ?assertEqual(
        [
            #{
                connection_id => <<"valid">>,
                user_id => <<"42">>,
                token_nonce => <<"nonce">>,
                expires_at => 0
            }
        ],
        PendingJoins
    ).

validate_pending_nonce_valid_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = #{
        token_nonce => <<"abc123">>, created_at => Now - 5000, expires_at => Now + 25000
    },
    ?assertEqual(
        ok,
        guild_voice_connection_pending:validate_pending_nonce_and_expiry(
            <<"abc123">>, PendingData
        )
    ).

validate_pending_nonce_mismatch_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = #{
        token_nonce => <<"abc123">>, created_at => Now - 5000, expires_at => Now + 25000
    },
    ?assertEqual(
        {error, voice_nonce_mismatch},
        guild_voice_connection_pending:validate_pending_nonce_and_expiry(
            <<"wrong-nonce">>, PendingData
        )
    ).

validate_pending_nonce_missing_request_nonce_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = #{
        token_nonce => <<"abc123">>, created_at => Now - 5000, expires_at => Now + 25000
    },
    ?assertEqual(
        {error, voice_nonce_mismatch},
        guild_voice_connection_pending:validate_pending_nonce_and_expiry(undefined, PendingData)
    ).

validate_pending_nonce_expired_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = #{
        token_nonce => <<"abc123">>, created_at => Now - 35000, expires_at => Now - 5000
    },
    ?assertEqual(
        {error, voice_pending_expired},
        guild_voice_connection_pending:validate_pending_nonce_and_expiry(
            <<"abc123">>, PendingData
        )
    ).

validate_pending_nonce_missing_expires_at_test() ->
    PendingData = #{token_nonce => <<"abc123">>},
    ?assertEqual(
        ok,
        guild_voice_connection_pending:validate_pending_nonce_and_expiry(
            <<"abc123">>, PendingData
        )
    ).

pending_get_value_test() ->
    Data = #{key1 => value1, <<"key2">> => value2},
    ?assertEqual(value1, maps:get(key1, Data)),
    ?assertEqual(value2, maps:get(<<"key2">>, Data)).

check_pending_expiry_rejects_entry_without_any_timing_test() ->
    PendingData = #{
        user_id => 10,
        guild_id => 999,
        channel_id => 100,
        token_nonce => <<"abc123">>
    },
    PendingConnections = #{<<"conn-no-times">> => PendingData},
    State = (base_test_state())#{pending_voice_connections => PendingConnections},
    Result = guild_voice_connection_pending:maybe_restore_pending_connection(
        <<"conn-no-times">>, 100, 10, #{}, State
    ),
    ?assertEqual({error, voice_pending_expired}, Result).

check_pending_expiry_accepts_entry_with_recent_joined_at_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = #{
        user_id => 10,
        guild_id => 999,
        channel_id => 100,
        joined_at => Now - 1000
    },
    PendingConnections = #{<<"conn-joined">> => PendingData},
    State = (base_test_state())#{
        pending_voice_connections => PendingConnections,
        voice_states => #{}
    },
    Result = guild_voice_connection_pending:maybe_restore_pending_connection(
        <<"conn-joined">>, 100, 10, #{}, State
    ),
    ?assertNotEqual({error, voice_pending_expired}, Result).

check_pending_expiry_rejects_ancient_joined_at_test() ->
    Now = erlang:system_time(millisecond),
    PendingData = #{
        user_id => 10,
        guild_id => 999,
        channel_id => 100,
        joined_at => Now - 400000
    },
    PendingConnections = #{<<"conn-old">> => PendingData},
    State = (base_test_state())#{
        pending_voice_connections => PendingConnections,
        voice_states => #{}
    },
    Result = guild_voice_connection_pending:maybe_restore_pending_connection(
        <<"conn-old">>, 100, 10, #{}, State
    ),
    ?assertEqual({error, voice_pending_expired}, Result).
