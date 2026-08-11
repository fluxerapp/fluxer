%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_ready_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

process_guild_state_unavailable_dispatches_guild_delete_test() ->
    State0 = base_state_for_guild_dispatch_test(),
    GuildState = #{<<"id">> => <<"123">>, <<"unavailable">> => true},
    {noreply, State1} = session_ready:process_guild_state(GuildState, State0),
    Buffer = limited_deque:to_list(maps:get(buffer, State1)),
    ?assertEqual(1, length(Buffer)),
    [First | _] = Buffer,
    FirstEvent = maps:get(event, eqwalizer:dynamic_cast(First)),
    ?assertEqual(guild_delete, FirstEvent),
    ok.

process_guild_state_available_dispatches_guild_create_test() ->
    State0 = base_state_for_guild_dispatch_test(),
    GuildState = #{
        <<"id">> => <<"123">>,
        <<"unavailable">> => false,
        <<"channels">> => [],
        <<"members">> => []
    },
    {noreply, State1} = session_ready:process_guild_state(GuildState, State0),
    Buffer = limited_deque:to_list(maps:get(buffer, State1)),
    ?assertEqual(1, length(Buffer)),
    [First | _] = Buffer,
    FirstEvent = maps:get(event, eqwalizer:dynamic_cast(First)),
    ?assertEqual(guild_create, FirstEvent),
    ok.

mark_guild_unavailable_hidden_includes_flag_test() ->
    State0 = #{
        collected_guild_states => [],
        ready => #{<<"guilds">> => []}
    },
    {noreply, State1} = session_ready:mark_guild_unavailable(777, true, State0),
    Collected = maps:get(collected_guild_states, State1, []),
    ?assertEqual(1, length(Collected)),
    [UnavailableGuild] = Collected,
    ?assertEqual(<<"777">>, maps:get(<<"id">>, UnavailableGuild)),
    ?assertEqual(true, maps:get(<<"unavailable">>, UnavailableGuild)),
    ?assertEqual(true, maps:get(<<"unavailable_hidden">>, UnavailableGuild)).

mark_guild_unavailable_after_ready_dispatches_guild_delete_test() ->
    drain_mailbox(),
    State0 = (base_state_for_guild_dispatch_test())#{
        guilds => #{777 => undefined},
        socket_pid => self()
    },
    {noreply, State1} = session_ready:mark_guild_unavailable(777, true, State0),
    Collected = maps:get(collected_guild_states, State1, []),
    ?assertEqual(0, length(Collected)),
    ?assertEqual(unavailable, maps:get(777, maps:get(guilds, State1))),
    receive
        {dispatch, guild_delete, GuildDeleteData, _Seq} ->
            ?assertEqual(<<"777">>, maps:get(<<"id">>, GuildDeleteData)),
            ?assertEqual(true, maps:get(<<"unavailable">>, GuildDeleteData)),
            ?assertEqual(true, maps:get(<<"unavailable_hidden">>, GuildDeleteData));
        Other ->
            ?assert(false, {unexpected_message, Other})
    after 1000 ->
        ?assert(false, guild_delete_not_dispatched)
    end.

update_ready_guilds_does_not_duplicate_collected_guild_state_test() ->
    GuildState = #{
        <<"id">> => <<"777">>,
        <<"members">> => [#{<<"user">> => #{<<"id">> => <<"1">>}}]
    },
    Ready = #{<<"guilds">> => [], <<"v">> => 9},
    State = #{ready => Ready, bot => false},
    Updated = session_ready:update_ready_guilds(GuildState, State),
    ?assertEqual(Ready, maps:get(ready, Updated)).

dispatch_ready_data_bot_unavailable_dispatches_guild_delete_test() ->
    drain_mailbox(),
    UnavailableGuild = #{<<"id">> => <<"987">>, <<"unavailable">> => true},
    State0 = base_ready_state(
        <<"session-ready-test">>,
        42,
        true,
        #{987 => cached_unavailable},
        [UnavailableGuild]
    ),
    {noreply, _State1} = session_ready:dispatch_ready_data(State0),
    receive
        {dispatch, ready, ReadyData, _ReadySeq} ->
            ?assertEqual([], maps:get(<<"guilds">>, ReadyData, []));
        OtherReady ->
            ?assert(false, {unexpected_ready_message, OtherReady})
    after 1000 ->
        ?assert(false, ready_not_dispatched)
    end,
    receive
        {dispatch, guild_delete, GuildDeleteData, _GuildDeleteSeq} ->
            ?assertEqual(<<"987">>, maps:get(<<"id">>, GuildDeleteData)),
            ?assertEqual(true, maps:get(<<"unavailable">>, GuildDeleteData));
        OtherDelete ->
            ?assert(false, {unexpected_guild_event, OtherDelete})
    after 1000 ->
        ?assert(false, guild_delete_not_dispatched)
    end,
    receive
        {dispatch, guild_create, _GuildCreateData, _GuildCreateSeq} ->
            ?assert(false, unexpected_guild_create_for_unavailable_guild)
    after 100 ->
        ok
    end.

dispatch_ready_data_nonbot_includes_unavailable_guild_test() ->
    drain_mailbox(),
    UnavailableGuild = #{<<"id">> => <<"654">>, <<"unavailable">> => true},
    State0 = base_ready_state(
        <<"session-ready-nonbot-test">>,
        43,
        false,
        #{654 => cached_unavailable},
        [UnavailableGuild]
    ),
    {noreply, _State1} = session_ready:dispatch_ready_data(State0),
    receive
        {dispatch, ready, ReadyData, _ReadySeq} ->
            ReadyGuilds = maps:get(<<"guilds">>, ReadyData, []),
            ?assertEqual(1, length(ReadyGuilds)),
            ReadyGuild = hd(ReadyGuilds),
            ?assertEqual(<<"654">>, maps:get(<<"id">>, ReadyGuild)),
            ?assertEqual(true, maps:get(<<"unavailable">>, ReadyGuild));
        OtherReady ->
            ?assert(false, {unexpected_ready_message, OtherReady})
    after 1000 ->
        ?assert(false, ready_not_dispatched)
    end,
    receive
        {dispatch, guild_create, _GuildCreateData, _GuildCreateSeq} ->
            ?assert(false, unexpected_guild_create_for_nonbot_ready)
    after 100 ->
        ok
    end,
    receive
        {dispatch, guild_delete, _GuildDeleteData, _GuildDeleteSeq} ->
            ?assert(false, unexpected_guild_delete_for_nonbot_ready)
    after 100 ->
        ok
    end.

dispatch_ready_data_nonbot_includes_api_guild_ids_as_unavailable_test() ->
    drain_mailbox(),
    State0 = base_ready_state(
        <<"session-ready-pending-test">>,
        44,
        false,
        #{111 => undefined, 222 => cached_unavailable},
        []
    ),
    {noreply, _State1} = session_ready:dispatch_ready_data(State0),
    receive
        {dispatch, ready, ReadyData, _ReadySeq} ->
            ReadyGuilds = maps:get(<<"guilds">>, ReadyData, []),
            ?assertEqual(2, length(ReadyGuilds)),
            ReadyGuildIds = lists:sort([maps:get(<<"id">>, G) || G <- ReadyGuilds]),
            ?assertEqual([<<"111">>, <<"222">>], ReadyGuildIds),
            lists:foreach(
                fun(ReadyGuild) ->
                    ?assertEqual(true, maps:get(<<"unavailable">>, ReadyGuild))
                end,
                ReadyGuilds
            );
        OtherReady ->
            ?assert(false, {unexpected_ready_message, OtherReady})
    after 1000 ->
        ?assert(false, ready_not_dispatched)
    end.

dispatch_ready_data_nonbot_replaces_unavailable_placeholder_with_collected_guild_test() ->
    drain_mailbox(),
    CollectedGuild = #{
        <<"id">> => <<"111">>,
        <<"name">> => <<"guild-111">>,
        <<"members">> => [#{<<"user">> => #{<<"id">> => <<"1">>}}],
        <<"channels">> => []
    },
    State0 = base_ready_state(
        <<"session-ready-replace-test">>,
        46,
        false,
        #{111 => undefined, 222 => undefined},
        [CollectedGuild]
    ),
    {noreply, _State1} = session_ready:dispatch_ready_data(State0),
    receive
        {dispatch, ready, ReadyData, _ReadySeq} ->
            ReadyGuilds = maps:get(<<"guilds">>, ReadyData, []),
            ?assertEqual(2, length(ReadyGuilds)),
            FullGuild = find_ready_guild(<<"111">>, ReadyGuilds),
            PlaceholderGuild = find_ready_guild(<<"222">>, ReadyGuilds),
            ?assertEqual(<<"guild-111">>, maps:get(<<"name">>, FullGuild)),
            ?assertEqual(false, maps:is_key(<<"unavailable">>, FullGuild)),
            ?assertEqual(true, maps:get(<<"unavailable">>, PlaceholderGuild));
        OtherReady ->
            ?assert(false, {unexpected_ready_message, OtherReady})
    after 1000 ->
        ?assert(false, ready_not_dispatched)
    end.

dispatch_ready_data_bot_does_not_synthesize_unavailable_placeholders_test() ->
    drain_mailbox(),
    State0 = base_ready_state(
        <<"session-ready-bot-placeholder-test">>,
        47,
        true,
        #{111 => undefined, 222 => cached_unavailable},
        []
    ),
    {noreply, _State1} = session_ready:dispatch_ready_data(State0),
    receive
        {dispatch, ready, ReadyData, _ReadySeq} ->
            ?assertEqual([], maps:get(<<"guilds">>, ReadyData, []));
        OtherReady ->
            ?assert(false, {unexpected_ready_message, OtherReady})
    after 1000 ->
        ?assert(false, ready_not_dispatched)
    end,
    receive
        {dispatch, guild_delete, _GuildDeleteData, _GuildDeleteSeq} ->
            ?assert(false, unexpected_synthesized_unavailable_guild)
    after 100 ->
        ok
    end.

dispatch_ready_data_includes_shard_metadata_test() ->
    drain_mailbox(),
    State0 = (base_ready_state(
        <<"session-ready-shard-test">>,
        45,
        true,
        #{},
        []
    ))#{
        shard => {1, 4}
    },
    {noreply, _State1} = session_ready:dispatch_ready_data(State0),
    receive
        {dispatch, ready, ReadyData, _ReadySeq} ->
            ?assertEqual([1, 4], maps:get(<<"shard">>, ReadyData));
        OtherReady ->
            ?assert(false, {unexpected_ready_message, OtherReady})
    after 1000 ->
        ?assert(false, ready_not_dispatched)
    end.

dispatch_ready_data_includes_gateway_timings_test() ->
    drain_mailbox(),
    GwTimings = gateway_timings:record(
        test_gateway_step, gateway_timings:start() - 10, gateway_timings:new()
    ),
    State0 = (base_ready_state(
        <<"session-ready-gateway-timings-test">>,
        48,
        false,
        #{},
        []
    ))#{
        gw_timings => GwTimings
    },
    {noreply, _State1} = session_ready:dispatch_ready_data(State0),
    receive
        {dispatch, ready, ReadyData, _ReadySeq} ->
            Timings = maps:get(<<"_timings_gw">>, ReadyData),
            ?assertEqual(<<"microseconds">>, maps:get(<<"unit">>, Timings)),
            ?assert(is_binary(maps:get(<<"pod_name">>, Timings))),
            ?assertNot(maps:is_key(<<"node_name">>, Timings)),
            ?assertNot(maps:is_key(<<"erlang_node_name">>, Timings)),
            Trace = maps:get(<<"trace">>, Timings),
            TraceNames = [maps:get(<<"name">>, Span) || Span <- Trace],
            ?assert(lists:member(<<"test_gateway_step">>, TraceNames)),
            ReadySpan = find_trace_span(
                <<"session_ready_dispatch:dispatch_ready_to_socket/1">>, Trace
            ),
            ReadyChildren = maps:get(<<"children">>, ReadySpan),
            ReadyChildNames = [maps:get(<<"name">>, Span) || Span <- ReadyChildren],
            ?assert(
                lists:member(
                    <<"session_ready_collect:collect_ready_presences/2">>, ReadyChildNames
                )
            ),
            ?assert(
                lists:member(
                    <<"session_ready_collect:collect_ready_users/2">>, ReadyChildNames
                )
            ),
            ?assert(
                lists:member(
                    <<"session_ready_dispatch:build_final_ready_data/9">>, ReadyChildNames
                )
            ),
            ?assertNot(maps:is_key(<<"role">>, Timings)),
            ?assertNot(maps:is_key(<<"steps">>, Timings)),
            ?assertNot(maps:is_key(<<"nodes">>, Timings));
        OtherReady ->
            ?assert(false, {unexpected_ready_message, OtherReady})
    after 1000 ->
        ?assert(false, ready_not_dispatched)
    end.

-spec find_trace_span(binary(), [map()]) -> map().
find_trace_span(Name, Trace) ->
    case [Span || Span <- Trace, maps:get(<<"name">>, Span, undefined) =:= Name] of
        [Span | _] -> Span;
        [] -> error({trace_span_not_found, Name})
    end.

-spec drain_mailbox() -> ok.
drain_mailbox() ->
    receive
        _Message ->
            drain_mailbox()
    after 0 ->
        ok
    end.

-spec base_ready_state(binary(), integer(), boolean(), map(), [map()]) -> map().
base_ready_state(SessionId, UserId, Bot, Guilds, CollectedGuildStates) ->
    #{
        id => SessionId,
        user_id => UserId,
        version => 1,
        ready => #{<<"v">> => 9, <<"guilds">> => []},
        bot => Bot,
        guilds => Guilds,
        channels => #{},
        relationships => #{},
        collected_guild_states => CollectedGuildStates,
        collected_sessions => [],
        seq => 0,
        buffer => limited_deque:new(4096, 16777216),
        socket_pid => self(),
        ignored_events => #{},
        suppress_presence_updates => false,
        pending_presences => [],
        debounce_reactions => false,
        reaction_buffer => [],
        reaction_buffer_timer => undefined,
        presence_pid => undefined
    }.

-spec base_state_for_guild_dispatch_test() -> map().
base_state_for_guild_dispatch_test() ->
    #{
        guilds => #{},
        ready => undefined,
        seq => 0,
        buffer => limited_deque:new(4096, 16777216),
        socket_pid => undefined,
        ignored_events => #{},
        channels => #{},
        relationships => #{},
        suppress_presence_updates => false,
        pending_presences => [],
        presence_pid => undefined,
        debounce_reactions => false,
        reaction_buffer => [],
        reaction_buffer_timer => undefined,
        collected_guild_states => []
    }.

collect_ready_presences_does_not_add_one_to_one_dm_as_global_target_test() ->
    {ok, CachePid} = maybe_start_presence_cache(),
    OnlineDmUser = #{
        <<"status">> => <<"online">>,
        <<"user">> => #{<<"id">> => <<"2">>}
    },
    OnlineStranger = #{
        <<"status">> => <<"online">>,
        <<"user">> => #{<<"id">> => <<"99">>}
    },
    ok = presence_cache:put(2, OnlineDmUser),
    ok = presence_cache:put(99, OnlineStranger),
    _ = sys:get_state(CachePid),
    State = #{
        bot => false,
        user_id => 1,
        ready => #{},
        relationships => #{},
        channels => #{
            100 => #{
                <<"id">> => <<"100">>,
                <<"type">> => 1,
                <<"recipients">> => [#{<<"id">> => <<"2">>, <<"username">> => <<"dm-user">>}]
            }
        }
    },
    Presences = session_ready_collect:collect_ready_presences(State, []),
    PresenceIds = [
        maps:get(<<"id">>, maps:get(<<"user">>, P, #{}), undefined)
     || P <- Presences
    ],
    ?assertEqual([], PresenceIds),
    ?assertEqual(ok, gen_server:stop(CachePid)).

collect_ready_presences_includes_friend_without_dm_recipients_test() ->
    {ok, CachePid} = maybe_start_presence_cache(),
    OnlineFriend = #{
        <<"status">> => <<"online">>,
        <<"user">> => #{<<"id">> => <<"2">>}
    },
    ok = presence_cache:put(2, OnlineFriend),
    _ = sys:get_state(CachePid),
    Ready = #{
        <<"relationships">> => [
            #{<<"id">> => <<"2">>, <<"type">> => 1, <<"user">> => #{<<"id">> => <<"2">>}}
        ],
        <<"private_channels">> => [
            #{<<"id">> => <<"100">>, <<"type">> => 1}
        ]
    },
    State = #{
        bot => false,
        user_id => 1,
        ready => Ready,
        relationships => session_init:load_relationships(Ready),
        channels => session_init:load_private_channels(Ready)
    },
    Presences = session_ready_collect:collect_ready_presences(State, []),
    PresenceIds = [
        maps:get(<<"id">>, maps:get(<<"user">>, P, #{}), undefined)
     || P <- Presences
    ],
    ?assertEqual([<<"2">>], PresenceIds),
    ?assertEqual(ok, gen_server:stop(CachePid)).

collect_ready_presences_uses_live_friend_presence_when_cache_missing_test() ->
    {ok, CachePid} = maybe_start_presence_cache(),
    {ManagerPid, ManagerStarted} = maybe_start_presence_manager(),
    TargetId = 32,
    {ok, TargetPid} = presence_manager:start_or_lookup(presence_request(TargetId)),
    {ok, _TargetSessions} = gen_server:call(
        TargetPid, {session_connect, presence_connect_req(<<"target-ready-session">>)}, 5000
    ),
    _ = sys:get_state(CachePid),
    ok = presence_cache:delete(TargetId),
    _ = sys:get_state(CachePid),
    ?assertEqual(not_found, presence_cache:get(TargetId)),
    Ready = #{
        <<"relationships">> => [
            #{
                <<"id">> => integer_to_binary(TargetId),
                <<"type">> => 1,
                <<"user">> => #{<<"id">> => integer_to_binary(TargetId)}
            }
        ]
    },
    State = #{
        bot => false,
        user_id => 1,
        ready => Ready,
        relationships => session_init:load_relationships(Ready),
        channels => #{}
    },
    Presences = session_ready_collect:collect_ready_presences(State, []),
    ?assertEqual([TargetId], presence_ids(Presences)),
    _ = sys:get_state(CachePid),
    ?assertMatch({ok, _}, presence_cache:get(TargetId)),
    ok = gen_server:stop(TargetPid),
    stop_presence_manager(ManagerPid, ManagerStarted),
    ?assertEqual(ok, gen_server:stop(CachePid)).

collect_ready_presences_skips_offline_dm_recipients_test() ->
    {ok, CachePid} = maybe_start_presence_cache(),
    State = #{
        bot => false,
        user_id => 1,
        ready => #{},
        relationships => #{},
        channels => #{
            100 => #{
                <<"id">> => <<"100">>,
                <<"type">> => 1,
                <<"recipients">> => [#{<<"id">> => <<"5">>, <<"username">> => <<"dm-user">>}]
            }
        }
    },
    ?assertEqual([], session_ready_collect:collect_ready_presences(State, [])),
    ?assertEqual(ok, gen_server:stop(CachePid)).

maybe_start_presence_cache() ->
    case whereis(presence_cache) of
        undefined -> presence_cache:start_link();
        Existing when is_pid(Existing) -> {ok, Existing}
    end.

maybe_start_presence_manager() ->
    process_registry:init(),
    case whereis(presence_manager) of
        undefined ->
            case presence_manager:start_link() of
                {ok, Pid} -> {Pid, true};
                {error, {already_started, Pid}} -> {Pid, false}
            end;
        Existing when is_pid(Existing) ->
            {Existing, false}
    end.

stop_presence_manager(_Pid, false) ->
    ok;
stop_presence_manager(Pid, true) ->
    try gen_server:stop(Pid) of
        ok -> ok
    catch
        error:_ -> ok;
        exit:_ -> ok
    end.

presence_request(UserId) ->
    #{
        user_id => UserId,
        user_data => #{
            <<"id">> => integer_to_binary(UserId),
            <<"username">> => <<"ready-target">>,
            <<"discriminator">> => <<"0001">>,
            <<"avatar">> => null,
            <<"flags">> => 0
        },
        guild_ids => [],
        friend_ids => [],
        group_dm_recipients => #{},
        status => online,
        custom_status => null
    }.

presence_connect_req(SessionId) ->
    #{
        session_id => SessionId,
        status => online,
        afk => false,
        mobile => false,
        socket_pid => undefined
    }.

presence_ids(Presences) ->
    [
        snowflake_id:parse_maybe(maps:get(<<"id">>, maps:get(<<"user">>, P, #{}), undefined))
     || P <- Presences
    ].

-spec find_ready_guild(binary(), [map()]) -> map().
find_ready_guild(GuildId, ReadyGuilds) ->
    case [G || G <- ReadyGuilds, maps:get(<<"id">>, G, undefined) =:= GuildId] of
        [Guild] -> Guild;
        [] -> error({missing_ready_guild, GuildId})
    end.
