%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

clear_channel_notifications_disabled_by_default_test() ->
    erase_persistent_term(push_noop),
    erase_persistent_term(push_clear_notifications_enabled),
    ?assertEqual(ok, push:clear_channel_notifications(1, 2, 3)).

push_owner_key_prefers_first_recipient_test() ->
    ?assertEqual(
        42,
        push:push_owner_key(#{
            user_ids => [42, 99],
            author_id => 10,
            guild_id => 20
        })
    ).

push_owner_key_falls_back_to_author_then_guild_test() ->
    ?assertEqual(10, push:push_owner_key(#{user_ids => [], author_id => 10, guild_id => 20})),
    ?assertEqual(20, push:push_owner_key(#{guild_id => 20})).

push_owner_key_rejects_malformed_ids_test() ->
    ?assertEqual(42, push:push_owner_key(#{user_ids => [<<"42">>, 99]})),
    ?assertEqual(
        undefined,
        push:push_owner_key(#{
            user_ids => [<<"bad">>],
            author_id => <<"not-an-id">>,
            guild_id => <<"001">>
        })
    ).

message_params_context_normalizes_string_ids_test() ->
    Params = #{
        message_data => #{<<"channel_id">> => <<"123">>, <<"id">> => <<"456">>},
        user_ids => [<<"42">>],
        guild_id => <<"789">>,
        author_id => <<"7">>,
        guild_default_notifications => <<"1">>,
        role_names => #{10 => <<"Admins">>}
    },
    {ok, Context} = push_message_params:context(Params),
    ?assertEqual([42], maps:get(user_ids, Context)),
    ?assertEqual(789, maps:get(guild_id, Context)),
    ?assertEqual(7, maps:get(author_id, Context)),
    ?assertEqual(123, maps:get(channel_id, Context)),
    ?assertEqual(456, maps:get(message_id, Context)),
    ?assertEqual(1, maps:get(guild_default_notifications, Context)),
    ?assertEqual(#{10 => <<"Admins">>}, maps:get(role_names, Context)).

message_params_context_defaults_invalid_role_names_test() ->
    Params = #{
        message_data => #{<<"channel_id">> => <<"123">>, <<"id">> => <<"456">>},
        user_ids => [42],
        guild_id => <<"789">>,
        author_id => <<"7">>,
        guild_default_notifications => <<"1">>,
        role_names => invalid
    },
    {ok, Context} = push_message_params:context(Params),
    ?assertEqual(#{}, maps:get(role_names, Context)).

message_params_context_builds_group_dm_markdown_context_with_nicks_test() ->
    Params = #{
        message_data => #{
            <<"channel_id">> => <<"123">>,
            <<"id">> => <<"456">>,
            <<"content">> => <<"Hi <@42>">>,
            <<"channel_type">> => 3,
            <<"nicks">> => #{<<"42">> => <<"Group Nick">>},
            <<"mentions">> => [
                #{
                    <<"id">> => <<"42">>,
                    <<"global_name">> => <<"Global Name">>,
                    <<"username">> => <<"user42">>
                }
            ]
        },
        user_ids => [42],
        guild_id => 0,
        author_id => <<"7">>
    },
    {ok, Context} = push_message_params:context(Params),
    MarkdownContext = maps:get(markdown_context, Context),
    ?assertEqual(
        <<"Group Nick">>, maps:get(<<"42">>, maps:get(<<"users">>, MarkdownContext))
    ),
    ?assertEqual(
        <<"Group Nick">>,
        maps:get(<<"42">>, maps:get(<<"user_nicknames">>, MarkdownContext))
    ).

message_params_context_rejects_malformed_ids_test() ->
    Params = #{
        message_data => #{<<"channel_id">> => <<"bad">>, <<"id">> => <<"456">>},
        user_ids => [42],
        guild_id => <<"789">>,
        author_id => <<"7">>
    },
    ?assertEqual({error, invalid_channel_id}, push_message_params:context(Params)).

message_params_context_requires_explicit_guild_id_test() ->
    Params = #{
        message_data => #{<<"channel_id">> => <<"123">>, <<"id">> => <<"456">>},
        user_ids => [42],
        author_id => 7
    },
    DmParams = Params#{guild_id => 0},
    ?assertEqual({error, invalid_guild_id}, push_message_params:context(Params)),
    {ok, Context} = push_message_params:context(DmParams),
    ?assertEqual(0, maps:get(guild_id, Context)).

prefetch_user_guild_settings_batches_missing_users_into_one_rpc_test() ->
    push_ets_cache:init(),
    ok = push_ets_cache:put_user_guild_settings(9004, 4242, #{<<"muted">> => true}),
    Response = #{<<"user_guild_settings">> => [#{<<"mobile_push">> => false}, null]},
    with_rpc_client_stub({ok, Response}, fun() ->
        ok = push_eligibility:prefetch_user_guild_settings(
            [9002, 9001, 9004, 9999, 9001], 9999, 4242
        )
    end),
    ?assertEqual([{<<"4242">>, [<<"9001">>, <<"9002">>]}], settings_requests()),
    ?assertEqual(
        #{<<"mobile_push">> => false}, push_ets_cache:get_user_guild_settings(9001, 4242)
    ),
    ?assertEqual(#{}, push_ets_cache:get_user_guild_settings(9002, 4242)),
    ?assertEqual(
        #{<<"muted">> => true}, push_ets_cache:get_user_guild_settings(9004, 4242)
    ),
    lists:foreach(
        fun(UserId) -> push_ets_cache:delete_user_guild_settings(UserId, 4242) end,
        [9001, 9002, 9004]
    ).

prefetch_user_guild_settings_chunks_large_batches_test() ->
    push_ets_cache:init(),
    UserIds = lists:seq(90000, 90200),
    with_rpc_client_stub({ok, #{<<"user_guild_settings">> => []}}, fun() ->
        ok = push_eligibility:prefetch_user_guild_settings(UserIds, 9999, 4243)
    end),
    ?assertEqual([200, 1], [length(Ids) || {_GuildId, Ids} <- settings_requests()]).

prefetch_user_guild_settings_leaves_cache_cold_on_rpc_failure_test() ->
    push_ets_cache:init(),
    with_rpc_client_stub({error, timeout}, fun() ->
        ok = push_eligibility:prefetch_user_guild_settings([9007, 9008], 9999, 4244)
    end),
    ?assertEqual([{<<"4244">>, [<<"9007">>, <<"9008">>]}], settings_requests()),
    ?assertEqual(undefined, push_ets_cache:get_user_guild_settings(9007, 4244)),
    ?assertEqual(undefined, push_ets_cache:get_user_guild_settings(9008, 4244)).

prefetch_user_guild_settings_skips_direct_messages_test() ->
    push_ets_cache:init(),
    with_rpc_client_stub({error, timeout}, fun() ->
        ok = push_eligibility:prefetch_user_guild_settings([9007, 9008], 9999, 0)
    end),
    ?assertEqual([], settings_requests()).

init_logs_a_mismatched_vapid_pair_test() ->
    with_push_env(
        fun() ->
            {Pub, _} = generate_vapid_pair(),
            {_, OtherPriv} = generate_vapid_pair(),
            patch_vapid(true, Pub, OtherPriv),
            {ok, Pid} = with_captured_logs(fun() -> push:start_link() end),
            ?assert(is_process_alive(Pid)),
            ?assert(any_error_log_mentions("FLUXER_VAPID_PUBLIC_KEY")),
            gen_server:stop(Pid)
        end
    ).

init_accepts_a_matching_vapid_pair_test() ->
    with_push_env(
        fun() ->
            {Pub, Priv} = generate_vapid_pair(),
            patch_vapid(true, Pub, Priv),
            {ok, Pid} = push:start_link(),
            ?assert(is_process_alive(Pid)),
            gen_server:stop(Pid)
        end
    ).

with_rpc_client_stub(Result, Fun) ->
    Self = self(),
    ok = meck:new(rpc_client, [passthrough, no_link]),
    try
        ok = meck:expect(rpc_client, call, fun(Request) ->
            Self ! {rpc_request, Request},
            Result
        end),
        Fun()
    after
        meck:unload(rpc_client)
    end.

settings_requests() ->
    receive
        {rpc_request, #{<<"type">> := <<"get_user_guild_settings">>} = Request} ->
            [
                {maps:get(<<"guild_id">>, Request), maps:get(<<"user_ids">>, Request)}
                | settings_requests()
            ]
    after 0 ->
        []
    end.

with_push_env(Fun) ->
    push_ets_cache:init(),
    push_worker_pool:init_counter(),
    OldConfig = fluxer_gateway_env:get_map(),
    OldTrap = erlang:process_flag(trap_exit, true),
    try
        Fun()
    after
        _ = erlang:process_flag(trap_exit, OldTrap),
        flush_exit_signals(),
        _ = fluxer_gateway_env:update(fun(_) -> OldConfig end)
    end.

with_captured_logs(Fun) ->
    Self = self(),
    ok = logger:add_primary_filter(
        capture_logs, {
            fun(Event, Pid) ->
                Pid ! {captured_log, Event},
                stop
            end,
            Self
        }
    ),
    try
        Fun()
    after
        _ = logger:remove_primary_filter(capture_logs)
    end.

any_error_log_mentions(Needle) ->
    receive
        {captured_log, #{level := error, msg := {string, Message}}} ->
            case string:find(Message, Needle) of
                nomatch -> any_error_log_mentions(Needle);
                _ -> true
            end;
        {captured_log, _} ->
            any_error_log_mentions(Needle)
    after 0 ->
        false
    end.

patch_vapid(Enabled, Pub, Priv) ->
    _ = fluxer_gateway_env:patch(#{
        push_enabled => Enabled,
        vapid_public_key => push_utils:base64url_encode(Pub),
        vapid_private_key => push_utils:base64url_encode(Priv)
    }),
    ok.

generate_vapid_pair() ->
    case crypto:generate_key(ecdh, prime256v1) of
        {<<4, _:64/binary>> = Pub, <<_:32/binary>> = Priv} -> {Pub, Priv};
        _ -> generate_vapid_pair()
    end.

flush_exit_signals() ->
    receive
        {'EXIT', _, _} -> flush_exit_signals()
    after 0 ->
        ok
    end.

erase_persistent_term(Key) ->
    try persistent_term:erase(Key) of
        _ -> ok
    catch
        error:badarg -> ok
    end.
