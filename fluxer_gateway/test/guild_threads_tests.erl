%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_threads_tests).

-include_lib("eunit/include/eunit.hrl").

-define(G, 1).
-define(A, 10).
-define(C, 11).
-define(X, 12).
-define(P, 13).
-define(V, 14).
-define(M, 15).
-define(OWNER, 999).
-define(TEXT, 100).
-define(FORUM, 200).
-define(CATEGORY, 300).
-define(HIDDEN, 400).
-define(T1, 500).
-define(T2, 600).
-define(MOD_ROLE, 50).
-define(VIEW, 1024).
-define(SEND, 2048).
-define(HISTORY, 65536).
-define(IN_THREADS, (1 bsl 38)).
-define(MANAGE_THREADS, (1 bsl 34)).
-define(CONFIG_KEY, channel_threads_config).

with_config(Fun) ->
    Previous = persistent_term:get(?CONFIG_KEY, undefined),
    Config = (channel_threads_config:default_config())#{
        enabled => true,
        included_users =>
            #{
                integer_to_binary(Id) => true
             || Id <- [?A, ?P, ?V, ?M, ?OWNER]
            }
    },
    persistent_term:put(?CONFIG_KEY, Config),
    try
        Fun()
    after
        case Previous of
            undefined -> persistent_term:erase(?CONFIG_KEY);
            _ -> persistent_term:put(?CONFIG_KEY, Previous)
        end
    end.

raw_data() ->
    Gid = integer_to_binary(?G),
    Everyone = integer_to_binary(?VIEW bor ?SEND bor ?HISTORY bor ?IN_THREADS),
    Member = fun(Id, Roles, Bot) ->
        #{
            <<"user">> => #{<<"id">> => integer_to_binary(Id), <<"bot">> => Bot},
            <<"roles">> => Roles,
            <<"joined_at">> => <<"2026-01-01T00:00:00Z">>
        }
    end,
    #{
        <<"guild">> => #{
            <<"id">> => Gid, <<"owner_id">> => integer_to_binary(?OWNER), <<"features">> => []
        },
        <<"roles">> => [
            #{<<"id">> => Gid, <<"permissions">> => Everyone, <<"position">> => 0},
            #{
                <<"id">> => integer_to_binary(?MOD_ROLE),
                <<"permissions">> => integer_to_binary(?MANAGE_THREADS),
                <<"position">> => 1
            }
        ],
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"type">> => 0, <<"permission_overwrites">> => []},
            #{
                <<"id">> => <<"300">>,
                <<"type">> => 4,
                <<"permission_overwrites">> => [
                    #{
                        <<"id">> => Gid,
                        <<"type">> => 0,
                        <<"allow">> => <<"0">>,
                        <<"deny">> => <<"1024">>
                    }
                ]
            },
            #{
                <<"id">> => <<"200">>,
                <<"type">> => 15,
                <<"parent_id">> => <<"300">>,
                <<"default_thread_rate_limit_per_user">> => 5,
                <<"permission_overwrites">> => [
                    #{
                        <<"id">> => Gid,
                        <<"type">> => 0,
                        <<"allow">> => <<"1024">>,
                        <<"deny">> => <<"0">>
                    }
                ]
            },
            #{
                <<"id">> => <<"400">>,
                <<"type">> => 0,
                <<"permission_overwrites">> => [
                    #{
                        <<"id">> => integer_to_binary(?P),
                        <<"type">> => 1,
                        <<"allow">> => <<"0">>,
                        <<"deny">> => <<"1024">>
                    }
                ]
            }
        ],
        <<"members">> => [
            Member(?A, [], false),
            Member(?C, [], false),
            Member(?X, [integer_to_binary(?MOD_ROLE)], true),
            Member(?P, [], false),
            Member(?V, [], false),
            Member(?M, [integer_to_binary(?MOD_ROLE)], false),
            Member(?OWNER, [], false)
        ],
        <<"emojis">> => [],
        <<"stickers">> => []
    }.

thread(Id, Parent, Type) ->
    #{
        <<"id">> => integer_to_binary(Id),
        <<"guild_id">> => integer_to_binary(?G),
        <<"parent_id">> => integer_to_binary(Parent),
        <<"owner_id">> => integer_to_binary(?A),
        <<"type">> => Type,
        <<"name">> => <<"thread">>,
        <<"last_message_id">> => null,
        <<"rate_limit_per_user">> => 0,
        <<"flags">> => 0,
        <<"message_count">> => 0,
        <<"total_message_sent">> => 0,
        <<"member_count">> => 1,
        <<"thread_metadata">> => #{
            <<"archived">> => false,
            <<"locked">> => false,
            <<"auto_archive_duration">> => 4320,
            <<"archive_timestamp">> => <<"2026-01-01T00:00:00.000Z">>,
            <<"create_timestamp">> => <<"2026-01-01T00:00:00.000Z">>
        }
    }.

thread_member(ThreadId, UserId) ->
    #{
        <<"id">> => integer_to_binary(ThreadId),
        <<"user_id">> => integer_to_binary(UserId),
        <<"join_timestamp">> => <<"2026-01-01T00:00:00.000Z">>,
        <<"flags">> => 1,
        <<"muted">> => false,
        <<"mute_config">> => null
    }.

active_state(Pids) ->
    Payload = guild_thread_load:load_payload(#{
        <<"threads">> => [thread(?T1, ?TEXT, 11), thread(?T2, ?TEXT, 12)],
        <<"thread_members">> => [thread_member(?T1, ?A), thread_member(?T2, ?A)]
    }),
    Base = #{
        id => ?G,
        member_count => 10,
        data => guild_data_index:normalize_map(raw_data()),
        sessions => sessions(Pids),
        member_list_subscriptions => guild_member_list_subs:new()
    },
    guild_thread_load:install(
        #{
            thread_gate => #{active => true, version => 1},
            thread_tainted => true,
            thread_load => {ok, Payload}
        },
        Base
    ).

inactive_state(Pids) ->
    guild_thread_load:install(#{}, #{
        id => ?G,
        member_count => 10,
        data => guild_data_index:normalize_map(raw_data()),
        sessions => sessions(Pids),
        member_list_subscriptions => guild_member_list_subs:new()
    }).

sessions(Pids) ->
    maps:from_list([
        {atom_to_binary(Tag), session(Tag, UserId, Bot, Capable, Pid)}
     || {Tag, UserId, Bot, Capable, Pid} <- [
            {a, ?A, false, true, maps:get(a, Pids)},
            {c, ?C, false, true, maps:get(c, Pids)},
            {x, ?X, true, false, maps:get(x, Pids)},
            {p, ?P, false, true, maps:get(p, Pids)},
            {v, ?V, false, true, maps:get(v, Pids)},
            {i, ?A, false, false, maps:get(i, Pids)}
        ]
    ]).

session(Tag, UserId, Bot, Capable, Pid) ->
    #{
        session_id => atom_to_binary(Tag),
        user_id => UserId,
        pid => Pid,
        bot => Bot,
        is_staff => false,
        pending_connect => false,
        active_guilds => sets:from_list([?G]),
        thread_capable => Capable
    }.

start_captures() ->
    Parent = self(),
    maps:from_list([
        {Tag, spawn(fun() -> capture_loop(Tag, Parent) end)}
     || Tag <- [a, c, x, p, v, i]
    ]).

stop_captures(Pids) ->
    maps:foreach(fun(_Tag, Pid) -> Pid ! stop end, Pids).

capture_loop(Tag, Parent) ->
    receive
        stop ->
            ok;
        {'$gen_cast', {dispatch, Event, Payload}} ->
            Parent ! {captured, Tag, Event, decode(Payload)},
            capture_loop(Tag, Parent);
        _Other ->
            capture_loop(Tag, Parent)
    end.

decode({pre_encoded, Bin}) ->
    json:decode(Bin);
decode(Map) when is_map(Map) ->
    json:decode(iolist_to_binary(json:encode(guild_data_wire:payload(Map)))).

drain() ->
    drain([]).

drain(Acc) ->
    receive
        {captured, Tag, Event, Payload} -> drain([{Tag, Event, Payload} | Acc])
    after 150 ->
        lists:reverse(Acc)
    end.

received(Tag, Event, Events) ->
    [Payload || {T, E, Payload} <- Events, T =:= Tag, E =:= Event].

with_guild(Fun) ->
    with_config(fun() ->
        Pids = start_captures(),
        State = active_state(Pids),
        try
            Fun(State)
        after
            guild_thread_store:destroy(guild_thread_gate:store(State)),
            stop_captures(Pids),
            settle_mailbox_age()
        end
    end).

settle_mailbox_age() ->
    receive
        {guild_mailbox_age, Seq} ->
            ok = guild_mailbox_age:handle_mark(Seq),
            settle_mailbox_age()
    after 0 ->
        ok
    end.

dispatch(Event, Data, State) ->
    {noreply, NewState} = guild_dispatch:handle_dispatch(Event, Data, State),
    NewState.

viewer_flags_follow_capability_bot_and_enrolment_test() ->
    with_guild(fun(State) ->
        Sessions = maps:get(sessions, State),
        Viewer = fun(Sid) -> guild_thread_gate:session_viewer(maps:get(Sid, Sessions)) end,
        ?assert(Viewer(<<"a">>)),
        ?assertNot(Viewer(<<"c">>)),
        ?assert(Viewer(<<"x">>)),
        ?assertNot(Viewer(<<"i">>))
    end).

untainted_inactive_guild_create_is_unchanged_test() ->
    with_config(fun() ->
        Pids = start_captures(),
        try
            State = inactive_state(Pids),
            meck:new(guild_thread_view, [passthrough]),
            try
                Plain = guild_data:get_guild_state(?A, State),
                Opts = guild_data:get_guild_state(?A, State, #{thread_viewer => false}),
                ?assertEqual(Plain, Opts),
                ?assertNot(maps:is_key(<<"threads">>, Plain)),
                ?assertEqual(0, meck:num_calls(guild_thread_view, '_', '_'))
            after
                meck:unload(guild_thread_view)
            end,
            ?assertEqual(
                #{},
                maps:with([thread_gate, thread_store, thread_tainted], maps:get(data, State))
            )
        after
            stop_captures(Pids)
        end
    end).

tainted_inactive_reload_masks_role_bits_test() ->
    Pids = start_captures(),
    try
        {_Channels, Extras} = guild_thread_load:channels_collection(#{
            <<"channels">> => [], <<"thread_tainted">> => true
        }),
        State = guild_thread_load:install(Extras, #{
            id => ?G,
            member_count => 10,
            data => guild_data_index:normalize_map(raw_data()),
            sessions => sessions(Pids),
            member_list_subscriptions => guild_member_list_subs:new()
        }),
        ?assertNot(guild_thread_gate:active(State)),
        ?assert(guild_thread_gate:needs_variant(State)),
        GuildState = guild_data:get_guild_state(?C, State, #{thread_viewer => false}),
        [
            ?assertEqual(0, maps:get(<<"permissions">>, R) band ?IN_THREADS)
         || R <- maps:get(<<"roles">>, GuildState)
        ],
        Untainted = guild_data:get_guild_state(
            ?C, inactive_state(Pids), #{thread_viewer => false}
        ),
        ?assert(
            lists:any(
                fun(R) -> maps:get(<<"permissions">>, R) band ?IN_THREADS =/= 0 end,
                maps:get(<<"roles">>, Untainted)
            )
        )
    after
        stop_captures(Pids)
    end.

viewer_guild_create_carries_joined_threads_test() ->
    with_guild(fun(State) ->
        GuildState = guild_data:get_guild_state(?A, State, #{
            thread_viewer => true, bot => false
        }),
        Threads = maps:get(<<"threads">>, GuildState),
        ?assertEqual([?T1, ?T2], lists:sort([maps:get(<<"id">>, T) || T <- Threads])),
        [T1] = [T || T <- Threads, maps:get(<<"id">>, T) =:= ?T1],
        Member = maps:get(<<"member">>, T1),
        ?assertNot(maps:is_key(<<"id">>, Member)),
        ?assertNot(maps:is_key(<<"user_id">>, Member)),
        ?assertNot(maps:is_key(<<"member_ids_preview">>, T1)),
        VState = guild_data:get_guild_state(?V, State, #{thread_viewer => true, bot => false}),
        ?assertEqual([], maps:get(<<"threads">>, VState)),
        XState = guild_data:get_guild_state(?X, State, #{thread_viewer => true, bot => true}),
        ?assertEqual(
            [?T1, ?T2],
            lists:sort([maps:get(<<"id">>, T) || T <- maps:get(<<"threads">>, XState)])
        )
    end).

control_session_guild_create_hides_forums_and_bits_test() ->
    with_guild(fun(State) ->
        lists:foreach(
            fun(UserId) ->
                GuildState = guild_data:get_guild_state(UserId, State, #{thread_viewer => false}),
                ?assertNot(maps:is_key(<<"threads">>, GuildState)),
                Ids = [maps:get(<<"id">>, C) || C <- maps:get(<<"channels">>, GuildState)],
                ?assertNot(lists:member(?FORUM, Ids)),
                ?assertEqual(UserId =:= ?OWNER, lists:member(?CATEGORY, Ids)),
                [
                    ?assertEqual(0, maps:get(<<"permissions">>, R) band ?IN_THREADS)
                 || R <- maps:get(<<"roles">>, GuildState)
                ]
            end,
            [?C, ?OWNER]
        ),
        Viewer = guild_data:get_guild_state(?A, State, #{thread_viewer => true}),
        ViewerIds = [maps:get(<<"id">>, C) || C <- maps:get(<<"channels">>, Viewer)],
        ?assert(lists:member(?FORUM, ViewerIds)),
        ?assert(lists:member(?CATEGORY, ViewerIds))
    end).

permissions_fail_closed_for_unknown_ids_test() ->
    with_guild(fun(State) ->
        ?assertEqual(0, guild_permissions:get_member_permissions(?A, 424242, State)),
        Inactive = State#{data => maps:remove(thread_gate, maps:get(data, State))},
        ?assertNotEqual(0, guild_permissions:get_member_permissions(?A, 424242, Inactive))
    end).

thread_permissions_derive_from_parent_test() ->
    with_guild(fun(State) ->
        Perms = guild_permissions:get_member_permissions(?V, ?T1, State),
        ?assert(permission_bits:has(Perms, ?VIEW)),
        ?assert(permission_bits:has(Perms, ?SEND)),
        ?assertNot(
            permission_bits:has(guild_permissions:get_member_permissions(?V, ?T2, State), ?VIEW)
        ),
        ?assert(
            permission_bits:has(guild_permissions:get_member_permissions(?A, ?T2, State), ?VIEW)
        ),
        ?assert(
            permission_bits:has(guild_permissions:get_member_permissions(?M, ?T2, State), ?VIEW)
        ),
        {reply, #{auth_context := Context}, _} = guild_data:get_auth_context(
            #{user_id => ?V, channel_id => ?T1}, State
        ),
        ?assertEqual(?TEXT, maps:get(<<"id">>, maps:get(<<"parent_channel">>, Context)))
    end).

thread_create_recipients_and_shaping_test() ->
    with_guild(fun(State) ->
        Data = (thread(700, ?TEXT, 11))#{
            <<"newly_created">> => true,
            <<"_fluxer_member_ids_preview">> => [integer_to_binary(?A)],
            <<"_fluxer_members">> => [thread_member(700, ?A)]
        },
        _ = dispatch(thread_create, Data, State),
        Events = drain(),
        [ForA] = received(a, thread_create, Events),
        ?assertEqual(true, maps:get(<<"newly_created">>, ForA)),
        ?assertEqual(
            integer_to_binary(?A), maps:get(<<"user_id">>, maps:get(<<"member">>, ForA))
        ),
        ?assertNot(maps:is_key(<<"_fluxer_members">>, ForA)),
        ?assertNot(maps:is_key(<<"member_ids_preview">>, ForA)),
        [ForX] = received(x, thread_create, Events),
        ?assertNot(maps:is_key(<<"member">>, ForX)),
        ?assertEqual([], received(v, thread_create, Events)),
        ?assertEqual([], received(c, thread_create, Events)),
        ?assertEqual([], received(i, thread_create, Events))
    end).

private_thread_create_never_reaches_subscribed_non_members_test() ->
    with_guild(fun(State0) ->
        State = guild_thread_subscriptions:update(<<"v">>, #{threads => true}, State0),
        _ = dispatch(thread_create, thread(701, ?TEXT, 12), State),
        Events = drain(),
        ?assertEqual([], received(v, thread_create, Events)),
        ?assertMatch([_], received(x, thread_create, Events)),
        _ = dispatch(thread_create, thread(702, ?TEXT, 11), State),
        ?assertMatch([_], received(v, thread_create, drain()))
    end).

members_update_synthesizes_create_and_suppresses_creator_duplicate_test() ->
    with_guild(fun(State0) ->
        Created = (thread(703, ?TEXT, 12))#{<<"_fluxer_members">> => [thread_member(703, ?A)]},
        State1 = dispatch(thread_create, Created, State0),
        _ = drain(),
        CreatorAdded = #{
            <<"id">> => <<"703">>,
            <<"member_count">> => 1,
            <<"added_members">> => [thread_member(703, ?A)],
            <<"_fluxer_thread">> => thread(703, ?TEXT, 12)
        },
        State2 = dispatch(thread_members_update, CreatorAdded, State1),
        Events1 = drain(),
        ?assertEqual([], received(a, thread_create, Events1)),
        ?assertMatch([_], received(a, thread_members_update, Events1)),
        VAdded = CreatorAdded#{
            <<"member_count">> => 2, <<"added_members">> => [thread_member(703, ?V)]
        },
        _ = dispatch(thread_members_update, VAdded, State2),
        Events2 = drain(),
        [Create] = received(v, thread_create, Events2),
        ?assertNot(maps:is_key(<<"newly_created">>, Create)),
        ?assertEqual(
            integer_to_binary(?V), maps:get(<<"user_id">>, maps:get(<<"member">>, Create))
        ),
        [Update] = received(v, thread_members_update, Events2),
        [Added] = maps:get(<<"added_members">>, Update),
        ?assertEqual(
            integer_to_binary(?V),
            maps:get(<<"id">>, maps:get(<<"user">>, maps:get(<<"member">>, Added)))
        ),
        ?assert(maps:is_key(<<"presence">>, Added)),
        ?assertNot(maps:is_key(<<"muted">>, Added)),
        ?assertNot(maps:is_key(<<"mute_config">>, Added)),
        ?assertNot(maps:is_key(<<"_fluxer_thread">>, Update)),
        ?assertMatch([_], received(a, thread_members_update, Events2)),
        ?assertEqual([], received(i, thread_members_update, Events2)),
        ?assertMatch([_], received(x, thread_members_update, Events2)),
        ?assertEqual([], received(p, thread_members_update, Events2)),
        ?assertEqual(
            thread_create,
            element(2, hd([E || {v, _, _} = E <- Events2]))
        )
    end).

removed_user_without_view_still_gets_members_update_test() ->
    with_guild(fun(State) ->
        Removal = #{
            <<"id">> => integer_to_binary(?T2),
            <<"member_count">> => 1,
            <<"removed_member_ids">> => [integer_to_binary(?P)]
        },
        _ = dispatch(thread_members_update, Removal, State),
        [Update] = received(p, thread_members_update, drain()),
        ?assertEqual([integer_to_binary(?P)], maps:get(<<"removed_member_ids">>, Update))
    end).

thread_delete_payload_is_exact_test() ->
    with_guild(fun(State) ->
        Data = #{
            <<"id">> => integer_to_binary(?T1),
            <<"parent_id">> => integer_to_binary(?TEXT),
            <<"type">> => 11
        },
        State1 = dispatch(thread_delete, Data, State),
        Events = drain(),
        [ForA] = received(a, thread_delete, Events),
        ?assertEqual(
            lists:sort([<<"guild_id">>, <<"id">>, <<"parent_id">>, <<"type">>]),
            lists:sort(maps:keys(ForA))
        ),
        ?assertMatch([_], received(x, thread_delete, Events)),
        ?assertEqual([], received(c, thread_delete, Events)),
        ?assertEqual(undefined, guild_thread_gate:thread(?T1, State1))
    end).

archived_private_thread_delete_reaches_listed_members_test() ->
    with_guild(fun(State0) ->
        Archived = (thread(?T2, ?TEXT, 12))#{
            <<"thread_metadata">> => #{<<"archived">> => true, <<"locked">> => false}
        },
        State1 = dispatch(thread_update, Archived, State0),
        drain(),
        ?assertEqual(undefined, guild_thread_gate:thread(?T2, State1)),
        Data = #{
            <<"id">> => integer_to_binary(?T2),
            <<"parent_id">> => integer_to_binary(?TEXT),
            <<"type">> => 12
        },
        _ = dispatch(thread_delete, Data, State1),
        ?assertEqual([], received(a, thread_delete, drain())),
        _ = dispatch(
            thread_delete, Data#{<<"_fluxer_member_ids">> => [integer_to_binary(?A)]}, State1
        ),
        Events = drain(),
        [ForA] = received(a, thread_delete, Events),
        ?assertNot(maps:is_key(<<"_fluxer_member_ids">>, ForA)),
        ?assertEqual([], received(v, thread_delete, Events))
    end).

unknown_thread_uses_internal_context_test() ->
    with_guild(fun(State) ->
        Data = #{
            <<"id">> => <<"800">>,
            <<"member_count">> => 1,
            <<"added_members">> => [thread_member(800, ?V)],
            <<"_fluxer_thread">> => thread(800, ?TEXT, 11)
        },
        State1 = dispatch(thread_members_update, Data, State),
        ?assertNotEqual(undefined, guild_thread_gate:thread(800, State1)),
        Events = drain(),
        ?assertMatch([_], received(v, thread_create, Events)),
        ?assertMatch([_], received(v, thread_members_update, Events))
    end).

archive_removes_thread_and_unarchive_orders_update_before_member_update_test() ->
    with_guild(fun(State0) ->
        Archived = (thread(?T1, ?TEXT, 11))#{
            <<"thread_metadata">> => #{<<"archived">> => true, <<"locked">> => false}
        },
        State1 = dispatch(thread_update, Archived, State0),
        ?assertMatch([_], received(a, thread_update, drain())),
        ?assertEqual(undefined, guild_thread_gate:thread(?T1, State1)),
        Unarchived = (thread(?T1, ?TEXT, 11))#{
            <<"_fluxer_members">> => [thread_member(?T1, ?A)]
        },
        {noreply, State2} = guild:handle_cast(
            {dispatch_many, [#{event => thread_update, data => Unarchived}]}, State1
        ),
        Drained = drain(),
        ?assertEqual([thread_update, thread_member_update], [E || {a, E, _} <- Drained]),
        [Member] = received(a, thread_member_update, Drained),
        ?assertEqual(integer_to_binary(?G), maps:get(<<"guild_id">>, Member)),
        ?assertEqual(integer_to_binary(?A), maps:get(<<"user_id">>, Member)),
        ?assertEqual([], received(v, thread_member_update, Drained)),
        ?assertNotEqual(undefined, guild_thread_gate:thread(?T1, State2))
    end).

message_create_in_thread_reaches_members_and_bots_only_test() ->
    with_guild(fun(State) ->
        Message = #{
            <<"id">> => <<"900">>,
            <<"channel_id">> => integer_to_binary(?T1),
            <<"type">> => 0,
            <<"content">> => <<"hi">>,
            <<"author">> => #{<<"id">> => integer_to_binary(?X)},
            <<"mentions">> => [],
            <<"mention_roles">> => []
        },
        State1 = dispatch(message_create, Message, State),
        Events = drain(),
        ?assertMatch([_], received(a, message_create, Events)),
        ?assertMatch([_], received(x, message_create, Events)),
        ?assertEqual([], received(v, message_create, Events)),
        ?assertEqual([], received(c, message_create, Events)),
        ?assertEqual([], received(i, message_create, Events)),
        Thread = guild_thread_gate:thread(?T1, State1),
        ?assertEqual(1, maps:get(<<"message_count">>, Thread))
    end).

type_18_and_thread_only_updates_are_hidden_from_control_test() ->
    with_guild(fun(State) ->
        Created = #{
            <<"id">> => <<"901">>,
            <<"channel_id">> => integer_to_binary(?TEXT),
            <<"type">> => 18,
            <<"author">> => #{<<"id">> => integer_to_binary(?A)},
            <<"mentions">> => [],
            <<"mention_roles">> => []
        },
        _ = dispatch(message_create, Created, State),
        Events1 = drain(),
        ?assertMatch([_], received(a, message_create, Events1)),
        ?assertEqual([], received(c, message_create, Events1)),
        Update = #{
            <<"id">> => <<"902">>,
            <<"channel_id">> => integer_to_binary(?TEXT),
            <<"flags">> => 16#20,
            <<"thread">> => #{<<"id">> => <<"903">>},
            <<"__thread_only_update">> => true
        },
        _ = dispatch(message_update, Update, State),
        Events2 = drain(),
        [ForA] = received(a, message_update, Events2),
        ?assertNot(maps:is_key(<<"__thread_only_update">>, ForA)),
        ?assertEqual([], received(c, message_update, Events2)),
        Reply = #{
            <<"id">> => <<"904">>,
            <<"channel_id">> => integer_to_binary(?TEXT),
            <<"flags">> => 16#20 bor 4,
            <<"thread">> => #{<<"id">> => <<"905">>},
            <<"referenced_message">> => #{<<"type">> => 18, <<"id">> => <<"906">>}
        },
        _ = dispatch(message_update, Reply, State),
        Events3 = drain(),
        [Masked] = received(c, message_update, Events3),
        ?assertEqual(4, maps:get(<<"flags">>, Masked)),
        ?assertNot(maps:is_key(<<"thread">>, Masked)),
        ?assertEqual(null, maps:get(<<"referenced_message">>, Masked)),
        [Full] = received(a, message_update, Events3),
        ?assertEqual(16#24, maps:get(<<"flags">>, Full))
    end).

forum_channel_events_reach_viewers_only_test() ->
    with_guild(fun(State) ->
        Forum = #{
            <<"id">> => <<"210">>,
            <<"type">> => 15,
            <<"permission_overwrites">> => []
        },
        _ = dispatch(channel_create, Forum, State),
        Events = drain(),
        ?assertMatch([_], received(a, channel_create, Events)),
        ?assertEqual([], received(c, channel_create, Events)),
        ?assertEqual([], received(i, channel_create, Events))
    end).

inactive_guild_drops_gated_events_test() ->
    with_config(fun() ->
        Pids = start_captures(),
        try
            State = inactive_state(Pids),
            _ = dispatch(thread_create, thread(700, ?TEXT, 11), State),
            _ = dispatch(channel_create, #{<<"id">> => <<"210">>, <<"type">> => 15}, State),
            ?assertEqual([], drain())
        after
            stop_captures(Pids)
        end
    end).

live_audit_entries_on_thread_targets_are_viewer_only_test() ->
    with_guild(fun(State0) ->
        Sessions = maps:get(sessions, State0),
        Passive = maps:map(fun(_Sid, S) -> S#{active_guilds => sets:new()} end, Sessions),
        Owner = maps:get(<<"a">>, Passive),
        State = State0#{
            member_count => 300,
            sessions => Passive#{
                <<"c">> => (maps:get(<<"c">>, Passive))#{user_id => ?OWNER},
                <<"a">> => Owner#{user_id => ?OWNER, thread_viewer => true}
            }
        },
        Entry = #{
            <<"id">> => <<"1000">>,
            <<"action_type">> => 110,
            <<"target_id">> => integer_to_binary(?T1),
            <<"__thread_scoped">> => integer_to_binary(?G)
        },
        _ = dispatch(guild_audit_log_entry_create, Entry, State),
        Events = drain(),
        [ForA] = received(a, guild_audit_log_entry_create, Events),
        ?assertNot(maps:is_key(<<"__thread_scoped">>, ForA)),
        ?assertEqual([], received(c, guild_audit_log_entry_create, Events))
    end).

passive_sessions_get_self_directed_thread_events_test() ->
    with_guild(fun(State0) ->
        Sessions = maps:get(sessions, State0),
        Passive = maps:map(fun(_Sid, S) -> S#{active_guilds => sets:new()} end, Sessions),
        State1 = guild_thread_subscriptions:update(
            <<"v">>, #{threads => true}, State0#{member_count => 300, sessions => Passive}
        ),
        _ = dispatch(thread_member_update, (thread_member(?T1, ?A))#{<<"flags">> => 3}, State1),
        ?assertMatch([_], received(a, thread_member_update, drain())),
        Created = (thread(704, ?TEXT, 11))#{<<"_fluxer_members">> => [thread_member(704, ?A)]},
        State2 = dispatch(thread_create, Created, State1),
        Events = drain(),
        ?assertMatch([_], received(a, thread_create, Events)),
        ?assertEqual([], received(v, thread_create, Events)),
        _ = dispatch(
            thread_members_update,
            #{
                <<"id">> => <<"704">>,
                <<"member_count">> => 2,
                <<"added_members">> => [thread_member(704, ?V)],
                <<"_fluxer_thread">> => thread(704, ?TEXT, 11)
            },
            State2
        ),
        Events2 = drain(),
        ?assertMatch([_], received(v, thread_members_update, Events2)),
        ?assertEqual([], received(a, thread_members_update, Events2)),
        Unarchive = (thread(?T1, ?TEXT, 11))#{<<"name">> => <<"renamed">>},
        _ = dispatch(thread_update, Unarchive, State2),
        Events3 = drain(),
        ?assertMatch([_], received(a, thread_update, Events3)),
        ?assertEqual([], received(v, thread_update, Events3))
    end).

permission_loss_keeps_membership_and_sends_no_delete_test() ->
    with_guild(fun(State) ->
        Gid = integer_to_binary(?G),
        Update = #{
            <<"id">> => integer_to_binary(?TEXT),
            <<"type">> => 0,
            <<"permission_overwrites">> => [
                #{
                    <<"id">> => integer_to_binary(?A),
                    <<"type">> => 1,
                    <<"allow">> => <<"0">>,
                    <<"deny">> => <<"1024">>
                },
                #{
                    <<"id">> => Gid,
                    <<"type">> => 0,
                    <<"allow">> => <<"0">>,
                    <<"deny">> => <<"0">>
                }
            ]
        },
        State1 = dispatch(channel_update, Update, State),
        Events = drain(),
        ?assertEqual([], received(a, thread_delete, Events)),
        Tab = guild_thread_gate:store(State1),
        ?assert(guild_thread_store:is_member(Tab, ?T1, ?A)),
        Message = #{
            <<"id">> => <<"910">>,
            <<"channel_id">> => integer_to_binary(?T1),
            <<"type">> => 0,
            <<"mentions">> => [],
            <<"mention_roles">> => []
        },
        _ = dispatch(message_create, Message, State1),
        ?assertEqual([], received(a, message_create, drain()))
    end).

op14_threads_subscription_is_viewer_only_and_syncs_test() ->
    with_guild(fun(State0) ->
        State1 = guild_thread_subscriptions:update(<<"c">>, #{threads => true}, State0),
        ?assertNot(
            maps:is_key(thread_subscribed, maps:get(<<"c">>, maps:get(sessions, State1)))
        ),
        State2 = guild_thread_subscriptions:update(<<"v">>, #{threads => true}, State1),
        _ = guild_thread_subscriptions:handle_tick(<<"v">>, State2),
        [Sync] = received(v, thread_list_sync, drain()),
        ?assertNot(maps:is_key(<<"channel_ids">>, Sync)),
        ?assertEqual([integer_to_binary(?T1)], [
            maps:get(<<"id">>, T)
         || T <- maps:get(<<"threads">>, Sync)
        ]),
        ?assertEqual([], maps:get(<<"members">>, Sync)),
        State3 = guild_thread_subscriptions:update(
            <<"a">>, #{member_lists => [?T1, ?T2, 424242]}, State2
        ),
        Lists = received(a, thread_member_list_update, drain()),
        ?assertEqual(2, length(Lists)),
        [First | _] = Lists,
        [Entry] = maps:get(<<"members">>, First),
        ?assertEqual(integer_to_binary(?A), maps:get(<<"user_id">>, Entry)),
        ?assert(maps:is_key(<<"presence">>, Entry)),
        Passive = guild_sessions_passive:set_session_passive_guild(<<"v">>, ?G, State3),
        ?assertNot(
            maps:is_key(thread_subscribed, maps:get(<<"v">>, maps:get(sessions, Passive)))
        )
    end).

access_gain_sends_list_sync_with_channel_ids_test() ->
    with_guild(fun(State0) ->
        State1 = dispatch(thread_create, thread(705, ?HIDDEN, 11), State0),
        Events0 = drain(),
        ?assertEqual([], received(p, thread_create, Events0)),
        Lifted = #{
            <<"id">> => integer_to_binary(?HIDDEN),
            <<"type">> => 0,
            <<"permission_overwrites">> => []
        },
        _ = dispatch(channel_update, Lifted, State1),
        [Sync] = received(p, thread_list_sync, drain()),
        ?assertEqual([integer_to_binary(?HIDDEN)], maps:get(<<"channel_ids">>, Sync)),
        ?assertEqual([<<"705">>], [maps:get(<<"id">>, T) || T <- maps:get(<<"threads">>, Sync)])
    end).

op16_from_control_session_ignores_forums_and_threads_test() ->
    with_guild(fun(State) ->
        Request = #{session_id => <<"c">>, user_id => ?C, channel_ids => [?FORUM, ?T1]},
        {reply, #{counts := Counts}, _} = guild_query_handler:handle_call(
            {get_channel_member_counts, Request}, {self(), make_ref()}, State
        ),
        ?assertEqual([], Counts)
    end).

public_online_count_ignores_forums_test() ->
    with_guild(fun(State) ->
        Data = maps:get(data, State),
        WithoutForums = guild_data_index:put_channels(
            [
                C
             || C <- guild_data_index:channel_list(Data),
                maps:get(<<"type">>, C) =/= 15
            ],
            Data
        ),
        ?assertEqual(
            guild_public_online:compute_count(State#{data => WithoutForums}),
            guild_public_online:compute_count(State)
        )
    end).

flip_deactivation_resends_only_changed_sessions_test_() ->
    {timeout, 30, fun flip_deactivation_resends_only_changed_sessions/0}.

flip_deactivation_resends_only_changed_sessions() ->
    with_guild(fun(State) ->
        Flip = #{
            <<"thread_gate">> => #{<<"active">> => false, <<"config_version">> => 2},
            <<"thread_tainted">> => true,
            <<"channels">> => [
                C
             || C <- maps:get(<<"channels">>, raw_data()), maps:get(<<"type">>, C) =/= 15
            ],
            <<"roles">> => maps:get(<<"roles">>, raw_data())
        },
        Off = guild_thread_flip:apply_flip(Flip, State),
        ?assertNot(guild_thread_gate:active(Off)),
        ?assertEqual(undefined, guild_thread_gate:store(Off)),
        Sessions = maps:get(sessions, Off),
        ?assertNot(lists:any(fun guild_thread_gate:session_viewer/1, maps:values(Sessions))),
        Resends = collect_resends([]),
        ?assertEqual([<<"a">>, <<"p">>, <<"v">>, <<"x">>], lists:sort(Resends)),
        _ = guild_thread_flip:resend(<<"a">>, Off),
        Events = drain(),
        [GuildCreate] = received(a, guild_create, Events),
        ?assertNot(maps:is_key(<<"threads">>, GuildCreate)),
        ?assertEqual([], received(a, guild_update, Events))
    end).

collect_resends(Acc) ->
    receive
        {thread_flip_resend, SessionId} -> collect_resends([SessionId | Acc])
    after 11000 ->
        Acc
    end.

flip_activation_marks_viewers_and_loads_threads_test() ->
    with_config(fun() ->
        Pids = start_captures(),
        try
            State = inactive_state(Pids),
            Flip = #{
                <<"thread_gate">> => #{<<"active">> => true, <<"config_version">> => 3},
                <<"thread_tainted">> => true,
                <<"channels">> => maps:get(<<"channels">>, raw_data()),
                <<"roles">> => maps:get(<<"roles">>, raw_data()),
                <<"threads">> => [thread(?T1, ?TEXT, 11)],
                <<"thread_members">> => [thread_member(?T1, ?A)]
            },
            On = guild_thread_flip:apply_flip(Flip, State),
            ?assert(guild_thread_gate:active(On)),
            ?assertNotEqual(undefined, guild_thread_gate:thread(?T1, On)),
            ?assert(
                guild_thread_gate:session_viewer(maps:get(<<"a">>, maps:get(sessions, On)))
            ),
            ?assertNot(
                guild_thread_gate:session_viewer(maps:get(<<"c">>, maps:get(sessions, On)))
            ),
            guild_thread_store:destroy(guild_thread_gate:store(On))
        after
            stop_captures(Pids)
        end
    end).

flip_result_keeps_live_channels_and_replays_held_events_test() ->
    with_config(fun() ->
        Pids = start_captures(),
        try
            On = run_flip_with_held_events(3, Pids),
            ?assert(guild_thread_gate:active(On)),
            ?assertNot(maps:is_key(thread_flip_held, On)),
            ?assertNot(maps:is_key(thread_flip_retry, On)),
            Index = guild_data_index:channel_index(maps:get(data, On)),
            ?assertMatch(#{<<"type">> := 15}, maps:get(?FORUM, Index)),
            ?assertEqual([], maps:get(<<"permission_overwrites">>, maps:get(?HIDDEN, Index))),
            ?assertEqual(2, length(guild_data_index:role_list(maps:get(data, On)))),
            ?assertNotEqual(undefined, guild_thread_gate:thread(?T1, On)),
            guild_thread_store:destroy(guild_thread_gate:store(On))
        after
            stop_captures(Pids)
        end
    end).

flip_result_from_a_stale_api_rearms_the_flip_test() ->
    with_config(fun() ->
        Pids = start_captures(),
        try
            On = run_flip_with_held_events(2, Pids),
            ?assert(guild_thread_gate:active(On)),
            #{thread_flip_retry := {Timer, 1}} = On,
            ?assert(erlang:read_timer(Timer) > 0),
            _ = erlang:cancel_timer(Timer),
            guild_thread_store:destroy(guild_thread_gate:store(On))
        after
            stop_captures(Pids)
        end
    end).

held_event_without_an_id_is_skipped_on_replay_test() ->
    with_config(fun() ->
        Pids = start_captures(),
        try
            Raw = raw_data(),
            Snapshot = [
                C
             || C <- maps:get(<<"channels">>, Raw), maps:get(<<"type">>, C) =/= 15
            ],
            Ref = make_ref(),
            Held = [
                {thread_create, thread(?T1, ?FORUM, 11)},
                {thread_update, #{<<"guild_id">> => integer_to_binary(?G)}}
            ],
            State = (inactive_state(Pids))#{
                sessions => #{},
                data => guild_data_index:normalize_map(Raw),
                thread_flip => {Ref, 3},
                thread_flip_held => {2, Held}
            },
            Flip = #{
                <<"config_version">> => 3,
                <<"thread_gate">> => #{<<"active">> => true, <<"config_version">> => 3},
                <<"thread_tainted">> => true,
                <<"channels">> => Snapshot,
                <<"roles">> => [],
                <<"threads">> => [],
                <<"thread_members">> => []
            },
            On = guild_thread_flip:handle_result(Ref, {ok, Flip}, State),
            ?assert(guild_thread_gate:active(On)),
            ?assertNotEqual(undefined, guild_thread_gate:thread(?T1, On)),
            guild_thread_store:destroy(guild_thread_gate:store(On))
        after
            stop_captures(Pids)
        end
    end).

run_flip_with_held_events(ApiVersion, Pids) ->
    Raw = raw_data(),
    [Forum] = [C || C <- maps:get(<<"channels">>, Raw), maps:get(<<"type">>, C) =:= 15],
    Snapshot = [C || C <- maps:get(<<"channels">>, Raw), maps:get(<<"type">>, C) =/= 15],
    Inactive = inactive_state(Pids),
    Ref = make_ref(),
    State0 = Inactive#{
        sessions => #{},
        data => guild_data_index:normalize_map(Raw#{<<"channels">> => Snapshot}),
        thread_flip => {Ref, 3},
        thread_flip_held => {0, []}
    },
    Lifted = #{
        <<"id">> => integer_to_binary(?HIDDEN),
        <<"type">> => 0,
        <<"permission_overwrites">> => []
    },
    State1 = dispatch(channel_create, Forum, State0),
    State2 = dispatch(thread_create, thread(?T1, ?FORUM, 11), State1),
    State3 = dispatch(channel_update, Lifted, State2),
    ?assertMatch(#{thread_flip_held := {2, _}}, State3),
    Flip = #{
        <<"config_version">> => ApiVersion,
        <<"thread_gate">> => #{<<"active">> => true, <<"config_version">> => ApiVersion},
        <<"thread_tainted">> => true,
        <<"channels">> => Snapshot,
        <<"roles">> => [],
        <<"threads">> => [],
        <<"thread_members">> => []
    },
    guild_thread_flip:handle_result(Ref, {ok, Flip}, State3).

thread_store_retire_destroys_only_non_current_stores_test() ->
    Current = guild_thread_store:new(),
    Retired = guild_thread_store:new(),
    State = #{data => #{thread_store => Current}},
    ?assertEqual({noreply, State}, guild:handle_info({thread_store_retire, Current}, State)),
    ?assertEqual({noreply, State}, guild:handle_info({thread_store_retire, Retired}, State)),
    ?assertNotEqual(undefined, ets:info(Current)),
    ?assertEqual(undefined, ets:info(Retired)),
    guild_thread_store:destroy(Current).

flip_activation_without_sessions_rebuilds_the_permission_cache_test() ->
    with_config(fun() ->
        Pids = start_captures(),
        try
            Raw = raw_data(),
            NoForum = Raw#{
                <<"channels">> => [
                    C
                 || C <- maps:get(<<"channels">>, Raw), maps:get(<<"type">>, C) =/= 15
                ]
            },
            Inactive = inactive_state(Pids),
            State = Inactive#{sessions => #{}, data => guild_data_index:normalize_map(NoForum)},
            ok = guild_permission_cache:put_state(State),
            Flip = #{
                <<"thread_gate">> => #{<<"active">> => true, <<"config_version">> => 3},
                <<"thread_tainted">> => true,
                <<"channels">> => maps:get(<<"channels">>, raw_data()),
                <<"roles">> => maps:get(<<"roles">>, raw_data()),
                <<"threads">> => [],
                <<"thread_members">> => []
            },
            On = guild_thread_flip:apply_flip(Flip, State),
            {ok, #{data := Cached}} = guild_permission_cache:get_snapshot(?G),
            ?assertMatch(#{active := true}, maps:get(thread_gate, Cached, undefined)),
            ?assert(
                lists:any(
                    fun(C) -> maps:get(<<"type">>, C) =:= 15 end,
                    guild_data_index:channel_list(Cached)
                )
            ),
            guild_permission_cache:delete(?G),
            guild_thread_store:destroy(guild_thread_gate:store(On))
        after
            stop_captures(Pids)
        end
    end).

non_viewer_visibility_of_indexed_channels_uses_type_only_test() ->
    with_guild(fun(State) ->
        [C] = [S || S <- maps:values(maps:get(sessions, State)), maps:get(user_id, S) =:= ?C],
        ?assertNot(guild_thread_gate:session_viewer(C)),
        Tab = guild_thread_gate:store(State),
        Fake = guild_data_normalize:thread(thread(?TEXT, ?TEXT, 11)),
        guild_thread_store:put_thread(Tab, Fake),
        ?assertNotEqual(undefined, guild_thread_gate:thread(?TEXT, State)),
        ?assert(guild_thread_gate:channel_visible(C, ?TEXT, State)),
        ?assertNot(guild_thread_gate:channel_visible(C, ?FORUM, State)),
        ?assertNot(guild_thread_gate:channel_visible(C, ?T1, State)),
        Data = maps:get(data, State),
        Index = guild_data_index:channel_index(Data),
        Indexed = State#{
            data => Data#{
                <<"channel_index">> => Index#{?T2 => #{<<"id">> => ?T2, <<"type">> => 12}}
            }
        },
        ?assertNot(guild_thread_gate:channel_visible(C, ?T2, Indexed))
    end).

forum_only_category_update_skips_non_viewers_test() ->
    with_guild(fun(State) ->
        [Category] = [
            C
         || C <- guild_data_index:channel_list(maps:get(data, State)),
            maps:get(<<"type">>, C) =:= 4
        ],
        Update = guild_data_wire:payload(Category#{<<"name">> => <<"renamed">>}),
        _ = dispatch(channel_update, Update, State),
        Events = drain(),
        ?assertMatch([_], received(a, channel_update, Events)),
        ?assertMatch([_], received(x, channel_update, Events)),
        ?assertEqual([], received(c, channel_update, Events)),
        ?assertEqual([], received(i, channel_update, Events))
    end).

visibility_gain_channel_create_is_masked_for_non_viewers_test() ->
    with_guild(fun(State) ->
        Gid = integer_to_binary(?G),
        Everyone = #{
            <<"id">> => Gid,
            <<"type">> => 0,
            <<"allow">> => integer_to_binary(?IN_THREADS),
            <<"deny">> => <<"0">>
        },
        DenyC = #{
            <<"id">> => integer_to_binary(?C),
            <<"type">> => 1,
            <<"allow">> => <<"0">>,
            <<"deny">> => integer_to_binary(?VIEW)
        },
        WithText = fun(Overwrites) ->
            Data = maps:get(data, State),
            Channels = [
                case maps:get(<<"id">>, C) of
                    ?TEXT ->
                        C#{
                            <<"permission_overwrites">> => Overwrites,
                            <<"default_thread_rate_limit_per_user">> => 7
                        };
                    _ ->
                        C
                end
             || C <- guild_data_index:channel_list(Data)
            ],
            State#{data => guild_data_index:put_channels(Channels, Data)}
        end,
        Old = WithText([Everyone, DenyC]),
        New = WithText([Everyone]),
        _ = guild_visibility_overwrites:compute_and_dispatch_visibility_changes_for_users(
            [?C], Old, New
        ),
        [Create] = received(c, channel_create, drain()),
        ?assertEqual(integer_to_binary(?TEXT), maps:get(<<"id">>, Create)),
        ?assertNot(maps:is_key(<<"default_thread_rate_limit_per_user">>, Create)),
        [Overwrite] = maps:get(<<"permission_overwrites">>, Create),
        ?assertEqual(<<"0">>, maps:get(<<"allow">>, Overwrite))
    end).

handoff_export_carries_rows_within_budget_test() ->
    with_guild(fun(State) ->
        Exported = guild_handoff:export_handoff_state(State),
        Data = maps:get(data, Exported),
        ?assertNot(maps:is_key(thread_store, Data)),
        ?assert(is_list(maps:get(thread_rows, Data))),
        Restored = guild_init:init_base_state(Exported),
        ?assertNotEqual(undefined, guild_thread_gate:thread(?T1, Restored)),
        ?assert(
            guild_thread_gate:session_viewer(maps:get(<<"a">>, maps:get(sessions, Restored)))
        ),
        guild_thread_store:destroy(guild_thread_gate:store(Restored))
    end).

handoff_strips_rows_over_budget_test() ->
    Tab = guild_thread_store:new(),
    try
        true = ets:insert(Tab, [{{p, 1, N}, true} || N <- lists:seq(1, 50001)]),
        Exported = guild_thread_load:export_handoff(#{thread_store => Tab}),
        ?assertEqual(#{}, Exported)
    after
        guild_thread_store:destroy(Tab)
    end.

memory_fixture_of_a_thousand_threads_test() ->
    Tab = guild_thread_store:new(),
    try
        Threads = [thread(10000 + N, ?TEXT, 11) || N <- lists:seq(1, 1000)],
        Members = [
            thread_member(10000 + N, 20000 + U)
         || N <- lists:seq(1, 1000), U <- lists:seq(1, 50)
        ],
        ok = guild_thread_store:load(
            Tab,
            [guild_data_normalize:thread(T) || T <- Threads],
            [guild_data_normalize:thread_member(M) || M <- Members]
        ),
        ?assertEqual(1000, guild_thread_store:thread_count(Tab)),
        ?assertEqual(1000 * 2 + 1000 * 50 * 2, guild_thread_store:row_count(Tab)),
        Words = ets:info(Tab, memory),
        ?assert(Words * erlang:system_info(wordsize) < 64 * 1024 * 1024)
    after
        guild_thread_store:destroy(Tab)
    end.

can_view_channel_call_sites_are_enumerated_test() ->
    Known = lists:sort([
        guild,
        guild_data_channels,
        guild_dispatch,
        guild_dispatch_push,
        guild_dispatch_send,
        guild_member_list_channel_engine,
        guild_member_list_connected,
        guild_members_common,
        guild_members_query,
        guild_passive_sync,
        guild_permissions,
        guild_permissions_check,
        guild_query_handler,
        guild_request_forum_unreads,
        guild_member_list_subscribe,
        guild_sessions,
        guild_subscription_handler,
        guild_thread_gate,
        guild_visibility_channels,
        guild_visibility_roles,
        guild_members_search,
        guild_members_mutation
    ]),
    Dir = filename:join([code:lib_dir(fluxer_gateway), "..", "..", "..", "..", "src", "guild"]),
    Files = filelib:wildcard(filename:join(source_dir(Dir), "*.erl")),
    ?assert(length(Files) > 50),
    Modules = lists:usort([
        list_to_atom(filename:basename(F, ".erl"))
     || F <- Files,
        calls_view_primitive(F)
    ]),
    ?assertEqual([], Modules -- Known).

source_dir(Fallback) ->
    Candidates = [
        "src/guild",
        filename:join([code:lib_dir(fluxer_gateway), "src", "guild"]),
        Fallback
    ],
    hd([D || D <- Candidates, filelib:is_dir(D)] ++ [Fallback]).

calls_view_primitive(File) ->
    {ok, Bin} = file:read_file(File),
    binary:match(Bin, [
        <<"can_view_channel(">>,
        <<"session_can_view_channel(">>,
        <<"session_can_view_channel_members(">>
    ]) =/= nomatch.

load_race_replays_events_queued_while_loading_test_() ->
    {spawn, {timeout, 20, fun load_race_replays_events_queued_while_loading/0}}.

load_race_replays_events_queued_while_loading() ->
    with_config(fun() ->
        Pids = start_captures(),
        Base = #{
            id => ?G,
            member_count => 10,
            data => guild_data_index:normalize_map(raw_data()),
            sessions => sessions(Pids)
        },
        Loading = guild_thread_load:install(
            #{thread_gate => #{active => true, version => 1}}, Base
        ),
        try
            ?assert(guild_thread_load:loading(Loading)),
            #{thread_load := #{timer := Timer}} = Loading,
            erlang:cancel_timer(Timer),
            flush_load_retries(),
            Ref = make_ref(),
            Fetching = Loading#{thread_load => (maps:get(thread_load, Loading))#{ref => Ref}},
            Created = (thread(706, ?TEXT, 11))#{
                <<"_fluxer_members">> => [thread_member(706, ?A)]
            },
            Retrying = dispatch(thread_create, Created, Fetching),
            ?assertMatch([_], received(a, thread_create, drain())),
            Payload = guild_thread_load:load_payload(#{
                <<"threads">> => [thread(?T1, ?TEXT, 11)],
                <<"thread_members">> => [thread_member(?T1, ?A)]
            }),
            Loaded = guild_thread_load:handle_load_result(Ref, {ok, Payload}, Retrying),
            ?assertNot(guild_thread_load:loading(Loaded)),
            ?assertNotEqual(undefined, guild_thread_gate:thread(?T1, Loaded)),
            ?assertNotEqual(undefined, guild_thread_gate:thread(706, Loaded)),
            Ticks = lists:sort(sync_ticks(6000)),
            ?assertEqual([<<"a">>, <<"x">>], Ticks),
            Synced = lists:foldl(
                fun(Sid, Acc) -> guild_thread_subscriptions:handle_tick(Sid, Acc) end,
                Loaded,
                Ticks
            ),
            Events = drain(),
            [Sync] = received(a, thread_list_sync, Events),
            ?assertNot(maps:is_key(<<"channel_ids">>, Sync)),
            ?assertEqual(
                lists:sort([integer_to_binary(?T1), <<"706">>]),
                lists:sort([maps:get(<<"id">>, T) || T <- maps:get(<<"threads">>, Sync)])
            ),
            ?assertMatch([_], received(x, thread_list_sync, Events)),
            ?assertEqual([], received(c, thread_list_sync, Events)),
            guild_thread_store:destroy(guild_thread_gate:store(Synced))
        after
            stop_captures(Pids)
        end
    end).

sync_ticks(Timeout) ->
    Deadline = erlang:monotonic_time(millisecond) + Timeout,
    sync_ticks(Deadline, []).

sync_ticks(Deadline, Acc) ->
    Wait = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {thread_list_sync_tick, Sid} -> sync_ticks(Deadline, [Sid | Acc])
    after Wait -> Acc
    end.

flush_load_retries() ->
    receive
        thread_load_retry -> flush_load_retries()
    after 0 -> ok
    end.

first_fetch_after_a_stripped_handoff_is_immediate_test() ->
    flush_load_retries(),
    Installed = guild_thread_load:install(
        #{thread_gate => #{active => true, version => 1}},
        #{id => ?G, data => #{}, sessions => #{}}
    ),
    try
        ?assert(guild_thread_load:loading(Installed)),
        receive
            thread_load_retry -> ok
        after 100 -> ?assert(false)
        end,
        Ref = make_ref(),
        Loading = Installed#{thread_load => (maps:get(thread_load, Installed))#{ref => Ref}},
        Failed = guild_thread_load:handle_load_result(Ref, {error, down}, Loading),
        #{thread_load := #{timer := Timer, attempt := 1}} = Failed,
        Remaining = erlang:cancel_timer(Timer),
        ?assert(is_integer(Remaining) andalso Remaining > 29000)
    after
        guild_thread_load:deactivate(Installed)
    end.

active_channels_collection_loads_threads_asynchronously_test() ->
    flush_load_retries(),
    {Channels, Extras} = guild_thread_load:channels_collection(#{
        <<"channels">> => [#{<<"id">> => <<"1">>}],
        <<"thread_gate">> => #{<<"active">> => true, <<"config_version">> => 1},
        <<"threads">> => [thread(?T1, ?TEXT, 11)]
    }),
    ?assertEqual([#{<<"id">> => <<"1">>}], Channels),
    ?assertNot(maps:is_key(thread_load, Extras)),
    Installed = guild_thread_load:install(Extras, #{id => ?G, data => #{}, sessions => #{}}),
    ?assert(guild_thread_load:loading(Installed)),
    ?assertEqual(undefined, guild_thread_gate:thread(?T1, Installed)),
    receive
        thread_load_retry -> ok
    after 100 -> ?assert(false)
    end,
    guild_thread_load:deactivate(Installed).

flip_resend_skips_sessions_of_an_unavailable_guild_test() ->
    with_guild(fun(State) ->
        Data = maps:get(data, State),
        Guild = maps:get(<<"guild">>, Data),
        Unavailable = State#{
            data => Data#{
                <<"guild">> => Guild#{<<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]}
            }
        },
        _ = guild_thread_flip:resend(<<"a">>, Unavailable),
        ?assertEqual([], received(a, guild_create, drain())),
        _ = guild_thread_flip:resend(<<"a">>, State),
        ?assertMatch([_], received(a, guild_create, drain()))
    end).

flip_resend_keeps_a_degraded_guild_degraded_test() ->
    with_guild(fun(State) ->
        ok = guild_ets_owner:ensure_table(guild_health_status, [named_table, public, set]),
        _ = guild_thread_flip:resend(<<"a">>, State),
        [Healthy] = received(a, guild_create, drain()),
        ?assertNot(maps:is_key(<<"degraded">>, Healthy)),
        true = ets:insert(guild_health_status, {self(), ?G, true, undefined, undefined}),
        try
            _ = guild_thread_flip:resend(<<"a">>, State),
            ?assertMatch([#{<<"degraded">> := true}], received(a, guild_create, drain()))
        after
            ets:delete(guild_health_status, self())
        end
    end).

gate_only_reload_keeps_the_ready_store_while_it_refetches_test() ->
    with_guild(fun(State) ->
        Store = guild_thread_gate:store(State),
        Reloaded = guild_thread_flip:reinstall(
            #{thread_gate => #{active => true, version => 1}}, State, State
        ),
        flush_load_retries(),
        ?assertEqual(Store, guild_thread_gate:store(Reloaded)),
        ?assertMatch(#{<<"id">> := _}, guild_thread_gate:thread(?T1, Reloaded)),
        ?assert(guild_thread_load:loading(Reloaded)),
        Ref = make_ref(),
        Fresh = guild_thread_load:handle_load_result(
            Ref,
            {ok, guild_thread_load:load_payload(#{<<"threads">> => [thread(?T2, ?TEXT, 12)]})},
            thread_load_ref(Ref, Reloaded)
        ),
        ?assertNot(guild_thread_load:loading(Fresh)),
        ?assertEqual(undefined, guild_thread_gate:thread(?T1, Fresh)),
        ?assertMatch(#{<<"id">> := _}, guild_thread_gate:thread(?T2, Fresh)),
        guild_thread_store:destroy(guild_thread_gate:store(Fresh))
    end).

changed_gate_reload_starts_an_empty_store_test() ->
    with_guild(fun(State) ->
        Reloaded = guild_thread_flip:reinstall(
            #{thread_gate => #{active => true, version => 2}}, State, State
        ),
        flush_load_retries(),
        try
            ?assertNotEqual(guild_thread_gate:store(State), guild_thread_gate:store(Reloaded)),
            ?assertEqual(undefined, guild_thread_gate:thread(?T1, Reloaded))
        after
            guild_thread_store:destroy(guild_thread_gate:store(Reloaded))
        end
    end).

thread_load_ref(Ref, #{thread_load := Load} = State) ->
    State#{thread_load => Load#{ref => Ref}}.

reactivation_clears_thread_subscriptions_so_op14_resyncs_test_() ->
    {spawn, {timeout, 20, fun reactivation_clears_thread_subscriptions_so_op14_resyncs/0}}.

reactivation_clears_thread_subscriptions_so_op14_resyncs() ->
    with_guild(fun(State) ->
        Sessions = maps:get(sessions, State),
        A = maps:get(<<"a">>, Sessions),
        Subscribed = State#{
            sessions => Sessions#{
                <<"a">> => A#{thread_subscribed => true, thread_member_lists => [?T1]}
            }
        },
        Off = guild_thread_flip:reinstall(
            #{thread_gate => #{active => false, version => 2}}, Subscribed, Subscribed
        ),
        OffA = maps:get(<<"a">>, maps:get(sessions, Off)),
        ?assertNot(maps:is_key(thread_subscribed, OffA)),
        ?assertNot(maps:is_key(thread_member_lists, OffA)),
        On = guild_thread_flip:reinstall(
            #{
                thread_gate => #{active => true, version => 3},
                thread_load => {ok, #{threads => [], members => [], parent_settings => []}}
            },
            Off,
            Off
        ),
        try
            OnA = maps:get(<<"a">>, maps:get(sessions, On)),
            ?assert(guild_thread_gate:session_viewer(OnA)),
            Updated = guild_thread_subscriptions:update(<<"a">>, #{threads => true}, On),
            ?assertEqual(
                true,
                maps:get(thread_subscribed, maps:get(<<"a">>, maps:get(sessions, Updated)))
            ),
            receive
                {thread_list_sync_tick, <<"a">>} -> ok
            after 6000 -> ?assert(false)
            end
        after
            guild_thread_store:destroy(guild_thread_gate:store(On)),
            flush_flip_resends()
        end
    end).

flush_flip_resends() ->
    receive
        {thread_flip_resend, _} -> flush_flip_resends();
        {thread_list_sync_tick, _} -> flush_flip_resends()
    after 0 -> ok
    end.

thread_member_list_follows_presence_changes_test() ->
    with_guild(fun(State) ->
        Subscribed = guild_thread_subscriptions:update(
            <<"a">>, #{member_lists => [?T1]}, State
        ),
        ?assertMatch(#{thread_presence_users := #{?A := [?T1]}}, Subscribed),
        [_] = received(a, thread_member_list_update, drain()),
        Changed = lists:foldl(
            fun guild_thread_subscriptions:presence_changed/2, Subscribed, [?A, ?A, ?A]
        ),
        ?assertEqual([], received(a, thread_member_list_update, drain())),
        _ = guild_thread_subscriptions:handle_list_flush(Changed),
        ?assertMatch([_], received(a, thread_member_list_update, drain())),
        ?assertEqual(Subscribed, guild_thread_subscriptions:presence_changed(?C, Subscribed)),
        ?assertEqual(State, guild_thread_subscriptions:presence_changed(?A, State))
    end).

thread_member_list_follows_member_updates_test() ->
    with_guild(fun(State) ->
        Subscribed = guild_thread_subscriptions:update(
            <<"a">>, #{member_lists => [?T1]}, State
        ),
        [_] = received(a, thread_member_list_update, drain()),
        Update = fun(UserId, Nick) ->
            #{
                <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                <<"roles">> => [],
                <<"nick">> => Nick
            }
        end,
        Changed = dispatch(guild_member_update, Update(?A, <<"renamed">>), Subscribed),
        ?assertEqual([], received(a, thread_member_list_update, drain())),
        _ = guild_thread_subscriptions:handle_list_flush(Changed),
        [#{<<"members">> := Members}] = received(a, thread_member_list_update, drain()),
        ?assertEqual(
            [<<"renamed">>],
            [
                maps:get(<<"nick">>, Member)
             || #{<<"user_id">> := Id, <<"member">> := Member} <- Members,
                Id =:= integer_to_binary(?A)
            ]
        ),
        Unlisted = dispatch(guild_member_update, Update(?C, <<"other">>), Subscribed),
        ?assertNot(maps:is_key(thread_list_dirty, Unlisted))
    end).

thread_member_list_resubscribe_resends_snapshot_test() ->
    with_guild(fun(State) ->
        Subscribed = guild_thread_subscriptions:update(
            <<"a">>, #{member_lists => [?T1]}, State
        ),
        [_] = received(a, thread_member_list_update, drain()),
        Again = guild_thread_subscriptions:update(
            <<"a">>, #{member_lists => [?T1]}, Subscribed
        ),
        [_] = received(a, thread_member_list_update, drain()),
        _ = guild_thread_subscriptions:update(<<"a">>, #{threads => true}, Again),
        ?assertEqual([], received(a, thread_member_list_update, drain()))
    end).

thread_member_list_drops_removed_guild_members_test() ->
    with_guild(fun(State0) ->
        Added = #{
            <<"id">> => integer_to_binary(?T1),
            <<"member_count">> => 2,
            <<"added_members">> => [thread_member(?T1, ?V)]
        },
        State1 = dispatch(thread_members_update, Added, State0),
        Subscribed = guild_thread_subscriptions:update(
            <<"a">>, #{member_lists => [?T1]}, State1
        ),
        _ = drain(),
        Removed = dispatch(
            guild_member_remove,
            #{<<"user">> => #{<<"id">> => integer_to_binary(?V)}},
            Subscribed
        ),
        ?assertEqual(#{?T1 => true}, maps:get(thread_list_dirty, Removed)),
        _ = guild_thread_subscriptions:handle_list_flush(Removed),
        [#{<<"members">> := Members}] = received(a, thread_member_list_update, drain()),
        ?assertEqual(
            [integer_to_binary(?A)], [maps:get(<<"user_id">>, M) || M <- Members]
        )
    end).

handoff_through_an_old_node_round_trips_test() ->
    with_guild(fun(State) ->
        Exported = guild_handoff:export_handoff_state(State),
        OldShape = Exported#{
            data => maps:without(
                [thread_gate, thread_tainted, thread_rows, thread_rows_at],
                maps:get(data, Exported)
            )
        },
        FromOld = guild_init:init_base_state(OldShape),
        ?assertEqual(undefined, guild_thread_gate:store(FromOld)),
        ?assertNot(guild_thread_load:loading(FromOld)),
        ?assertNot(
            lists:any(
                fun guild_thread_gate:session_viewer/1,
                maps:values(maps:get(sessions, FromOld))
            )
        ),
        ReExported = guild_handoff:export_handoff_state(FromOld),
        ?assertNot(maps:is_key(thread_rows, maps:get(data, ReExported))),
        Stale = Exported#{
            data => (maps:get(data, Exported))#{thread_rows_at => 0}
        },
        flush_load_retries(),
        FromStale = guild_init:init_base_state(Stale),
        try
            ?assert(guild_thread_load:loading(FromStale)),
            ?assertEqual(undefined, guild_thread_gate:thread(?T1, FromStale)),
            receive
                thread_load_retry -> ok
            after 100 -> ?assert(false)
            end
        after
            guild_thread_load:deactivate(FromStale)
        end
    end).

excluded_bot_is_a_control_session_test() ->
    with_config(fun() ->
        Config = persistent_term:get(?CONFIG_KEY),
        persistent_term:put(?CONFIG_KEY, Config#{
            excluded_users => #{integer_to_binary(?X) => true}
        }),
        Pids = start_captures(),
        State = active_state(Pids),
        try
            ?assertNot(
                guild_thread_gate:session_viewer(maps:get(<<"x">>, maps:get(sessions, State)))
            ),
            _ = dispatch(thread_create, thread(707, ?TEXT, 11), State),
            ?assertEqual([], received(x, thread_create, drain()))
        after
            guild_thread_store:destroy(guild_thread_gate:store(State)),
            stop_captures(Pids)
        end
    end).

thread_message_permission_work_ignores_non_member_sessions_test() ->
    with_guild(fun(State) ->
        Message = #{
            <<"id">> => <<"920">>,
            <<"channel_id">> => integer_to_binary(?T1),
            <<"type">> => 0,
            <<"content">> => <<"hi">>,
            <<"author">> => #{<<"id">> => integer_to_binary(?X)},
            <<"mentions">> => [],
            <<"mention_roles">> => []
        },
        Crowd = maps:from_list([
            {
                <<"crowd", (integer_to_binary(N))/binary>>,
                (session(crowd, UserId, false, true, self()))#{
                    session_id => <<"crowd", (integer_to_binary(N))/binary>>
                }
            }
         || N <- lists:seq(1, 1000), UserId <- [lists:nth(1 + N rem 2, [?C, ?V])]
        ]),
        Crowded = guild_thread_gate:recompute_session_viewers(State#{
            sessions => maps:merge(maps:get(sessions, State), Crowd)
        }),
        meck:new(guild_thread_permissions, [passthrough, no_link]),
        try
            _ = dispatch(message_create, Message, State),
            Base = meck:num_calls(guild_thread_permissions, resolve, '_'),
            ?assertMatch([_], received(a, message_create, drain())),
            meck:reset(guild_thread_permissions),
            _ = dispatch(message_create, Message#{<<"id">> => <<"921">>}, Crowded),
            ?assertEqual(Base, meck:num_calls(guild_thread_permissions, resolve, '_')),
            ?assertMatch([_], received(a, message_create, drain()))
        after
            meck:unload(guild_thread_permissions)
        end,
        flush_crowd()
    end).

flush_crowd() ->
    receive
        {'$gen_cast', _} -> flush_crowd()
    after 0 -> ok
    end.

tainted_inactive_guild_drops_thread_message_deletes_test() ->
    with_config(fun() ->
        Pids = start_captures(),
        try
            {_Channels, Extras} = guild_thread_load:channels_collection(#{
                <<"channels">> => [], <<"thread_tainted">> => true
            }),
            State = guild_thread_load:install(Extras, #{
                id => ?G,
                member_count => 10,
                data => guild_data_index:normalize_map(raw_data()),
                sessions => sessions(Pids),
                member_list_subscriptions => guild_member_list_subs:new()
            }),
            Bulk = #{<<"channel_id">> => integer_to_binary(?T1), <<"ids">> => [<<"930">>]},
            _ = dispatch(message_delete_bulk, Bulk, State),
            _ = dispatch(
                message_delete_bulk,
                Bulk#{<<"_fluxer_thread">> => thread(?T1, ?TEXT, 11)},
                State
            ),
            ?assertEqual([], drain()),
            Text = #{<<"channel_id">> => integer_to_binary(?TEXT), <<"ids">> => [<<"931">>]},
            _ = dispatch(message_delete_bulk, Text, State),
            ?assertMatch([_], received(c, message_delete_bulk, drain()))
        after
            stop_captures(Pids)
        end
    end).

archived_thread_message_delete_uses_the_internal_context_test() ->
    with_guild(fun(State0) ->
        Archived = (thread(?T1, ?TEXT, 11))#{
            <<"thread_metadata">> => #{<<"archived">> => true, <<"locked">> => false}
        },
        State1 = guild_thread_subscriptions:update(
            <<"v">>, #{threads => true}, dispatch(thread_update, Archived, State0)
        ),
        drain(),
        ?assertEqual(undefined, guild_thread_gate:thread(?T1, State1)),
        Delete = #{<<"channel_id">> => integer_to_binary(?T1), <<"id">> => <<"940">>},
        _ = dispatch(message_delete, Delete, State1),
        ?assertEqual([], drain()),
        Context = #{
            <<"id">> => integer_to_binary(?T1),
            <<"parent_id">> => integer_to_binary(?TEXT),
            <<"type">> => 11
        },
        _ = dispatch(message_delete, Delete#{<<"_fluxer_thread">> => Context}, State1),
        Events = drain(),
        [ForV] = received(v, message_delete, Events),
        ?assertNot(maps:is_key(<<"_fluxer_thread">>, ForV)),
        ?assertMatch([_], received(x, message_delete, Events)),
        ?assertEqual([], received(c, message_delete, Events)),
        ?assertEqual([], received(i, message_delete, Events))
    end).

archived_thread_member_update_checks_parent_access_test() ->
    with_guild(fun(State0) ->
        State1 = dispatch(thread_create, thread(708, ?HIDDEN, 11), State0),
        Archived = (thread(708, ?HIDDEN, 11))#{
            <<"thread_metadata">> => #{<<"archived">> => true, <<"locked">> => false}
        },
        State2 = dispatch(thread_update, Archived, State1),
        drain(),
        ?assertEqual(undefined, guild_thread_gate:thread(708, State2)),
        Update = fun(UserId) ->
            (thread_member(708, UserId))#{<<"_fluxer_parent_id">> => integer_to_binary(?HIDDEN)}
        end,
        _ = dispatch(thread_member_update, Update(?V), State2),
        [ForV] = received(v, thread_member_update, drain()),
        ?assertNot(maps:is_key(<<"_fluxer_parent_id">>, ForV)),
        _ = dispatch(thread_member_update, Update(?P), State2),
        ?assertEqual([], received(p, thread_member_update, drain())),
        _ = dispatch(thread_member_update, thread_member(708, ?V), State2),
        ?assertEqual([], received(v, thread_member_update, drain()))
    end).

handoff_restores_thread_member_list_presence_tracking_test() ->
    with_guild(fun(State) ->
        Subscribed = guild_thread_subscriptions:update(
            <<"a">>, #{member_lists => [?T1]}, State
        ),
        drain(),
        Exported = guild_manager_shard_lifecycle:normalize_transferred_guild_state(
            maps:get(id, Subscribed), guild_handoff:export_handoff_state(Subscribed)
        ),
        Restored = guild_init:init_base_state(Exported),
        try
            ?assertMatch(#{thread_presence_users := #{?A := [?T1]}}, Restored),
            _ = guild_thread_subscriptions:handle_list_flush(
                guild_thread_subscriptions:presence_changed(?A, Restored)
            ),
            ?assertMatch([_], received(a, thread_member_list_update, drain()))
        after
            guild_thread_store:destroy(guild_thread_gate:store(Restored))
        end
    end).

capable_and_incapable_sessions_of_one_user_split_test() ->
    with_guild(fun(State) ->
        Sessions = maps:get(sessions, State),
        ?assert(guild_thread_gate:session_viewer(maps:get(<<"a">>, Sessions))),
        ?assertNot(guild_thread_gate:session_viewer(maps:get(<<"i">>, Sessions))),
        ?assert(
            maps:is_key(
                <<"threads">>,
                guild_data:get_guild_state(
                    ?A, State, guild_thread_gate:state_opts(maps:get(<<"a">>, Sessions))
                )
            )
        ),
        ?assertNot(
            maps:is_key(
                <<"threads">>,
                guild_data:get_guild_state(
                    ?A, State, guild_thread_gate:state_opts(maps:get(<<"i">>, Sessions))
                )
            )
        ),
        _ = dispatch(
            thread_create,
            (thread(709, ?TEXT, 11))#{<<"_fluxer_members">> => [thread_member(709, ?A)]},
            State
        ),
        Created = drain(),
        ?assertMatch([_], received(a, thread_create, Created)),
        ?assertEqual([], received(i, thread_create, Created)),
        Update = #{
            <<"id">> => <<"950">>,
            <<"channel_id">> => integer_to_binary(?TEXT),
            <<"flags">> => 16#20 bor 4,
            <<"thread">> => #{<<"id">> => <<"709">>}
        },
        _ = dispatch(message_update, Update, State),
        Updated = drain(),
        [Full] = received(a, message_update, Updated),
        ?assertEqual(16#24, maps:get(<<"flags">>, Full)),
        ?assert(maps:is_key(<<"thread">>, Full)),
        [Masked] = received(i, message_update, Updated),
        ?assertEqual(4, maps:get(<<"flags">>, Masked)),
        ?assertNot(maps:is_key(<<"thread">>, Masked))
    end).

forum_only_shared_channel_grants_no_presence_to_control_sessions_test() ->
    with_guild(fun(State0) ->
        Data0 = maps:get(data, State0),
        Forum = [
            maps:remove(<<"parent_id">>, C)
         || C <- guild_data_index:channel_list(Data0), maps:get(<<"type">>, C) =:= 15
        ],
        State = State0#{data => guild_data_index:put_channels(Forum, Data0)},
        Sessions = maps:get(sessions, State),
        ?assertEqual(
            [?V],
            guild_subscription_mutual_channels:filter_member_ids(
                maps:get(<<"a">>, Sessions), ?A, [?V], State
            )
        ),
        ?assertEqual(
            [],
            guild_subscription_mutual_channels:filter_member_ids(
                maps:get(<<"c">>, Sessions), ?C, [?V], State
            )
        ),
        ?assertEqual(
            #{<<"a">> => [?V], <<"c">> => []},
            guild_subscription_mutual_channels:filter_session_member_ids(
                [{<<"a">>, ?A, [?V]}, {<<"c">>, ?C, [?V]}], State
            )
        )
    end).

-define(NEWS, 110).
-define(NEWS_THREAD, 800).

news_raw_data() ->
    Raw = raw_data(),
    News = #{<<"id">> => <<"110">>, <<"type">> => 5, <<"permission_overwrites">> => []},
    Raw#{<<"channels">> => maps:get(<<"channels">>, Raw) ++ [News]}.

news_state(Pids) ->
    Payload = guild_thread_load:load_payload(#{
        <<"threads">> => [thread(?T1, ?TEXT, 11), thread(?NEWS_THREAD, ?NEWS, 10)],
        <<"thread_members">> => [thread_member(?T1, ?A), thread_member(?NEWS_THREAD, ?A)]
    }),
    guild_thread_load:install(
        #{
            thread_gate => #{active => true, version => 1},
            thread_tainted => true,
            thread_load => {ok, Payload}
        },
        #{
            id => ?G,
            member_count => 10,
            data => guild_data_index:normalize_map(news_raw_data()),
            sessions => sessions(Pids),
            member_list_subscriptions => guild_member_list_subs:new()
        }
    ).

with_news_guild(Fun) ->
    with_config(fun() ->
        Pids = start_captures(),
        State = news_state(Pids),
        try
            Fun(State)
        after
            guild_thread_store:destroy(guild_thread_gate:store(State)),
            stop_captures(Pids),
            settle_mailbox_age()
        end
    end).

news_thread_create_reaches_viewers_only_test() ->
    with_news_guild(fun(State0) ->
        Data = (thread(801, ?NEWS, 10))#{
            <<"newly_created">> => true,
            <<"_fluxer_members">> => [thread_member(801, ?A)]
        },
        State1 = dispatch(thread_create, Data, State0),
        Events1 = drain(),
        [ForA] = received(a, thread_create, Events1),
        ?assertEqual(10, maps:get(<<"type">>, ForA)),
        ?assertEqual(integer_to_binary(?NEWS), maps:get(<<"parent_id">>, ForA)),
        ?assertMatch([_], received(x, thread_create, Events1)),
        ?assertEqual([], received(v, thread_create, Events1)),
        ?assertEqual([], received(c, thread_create, Events1)),
        ?assertEqual([], received(i, thread_create, Events1)),
        State2 = guild_thread_subscriptions:update(<<"v">>, #{threads => true}, State1),
        _ = dispatch(thread_create, thread(802, ?NEWS, 10), State2),
        Events2 = drain(),
        ?assertMatch([_], received(v, thread_create, Events2)),
        ?assertEqual([], received(c, thread_create, Events2)),
        ?assertEqual([], received(i, thread_create, Events2))
    end).

news_thread_permissions_derive_from_announcement_parent_test() ->
    with_news_guild(fun(State) ->
        Perms = guild_permissions:get_member_permissions(?V, ?NEWS_THREAD, State),
        ?assert(permission_bits:has(Perms, ?VIEW)),
        ?assert(permission_bits:has(Perms, ?SEND)),
        {reply, #{auth_context := Context}, _} = guild_data:get_auth_context(
            #{user_id => ?V, channel_id => ?NEWS_THREAD}, State
        ),
        ?assertEqual(?NEWS, maps:get(<<"id">>, maps:get(<<"parent_channel">>, Context))),
        [C] = [S || S <- maps:values(maps:get(sessions, State)), maps:get(user_id, S) =:= ?C],
        ?assert(guild_thread_gate:channel_visible(C, ?NEWS, State)),
        ?assertNot(guild_thread_gate:channel_visible(C, ?NEWS_THREAD, State))
    end).

news_thread_permission_loss_on_parent_hides_thread_test() ->
    with_news_guild(fun(State0) ->
        Gid = integer_to_binary(?G),
        Hidden = #{
            <<"id">> => integer_to_binary(?NEWS),
            <<"type">> => 5,
            <<"permission_overwrites">> => [
                #{
                    <<"id">> => Gid,
                    <<"type">> => 0,
                    <<"allow">> => <<"0">>,
                    <<"deny">> => <<"1024">>
                }
            ]
        },
        State1 = dispatch(channel_update, Hidden, State0),
        _ = drain(),
        ?assertNot(
            permission_bits:has(
                guild_permissions:get_member_permissions(?A, ?NEWS_THREAD, State1), ?VIEW
            )
        ),
        ?assertNotEqual(undefined, guild_thread_gate:thread(?NEWS_THREAD, State1)),
        _ = dispatch(thread_update, thread(?NEWS_THREAD, ?NEWS, 10), State1),
        ?assertEqual([], received(a, thread_update, drain()))
    end).

news_guild_create_carries_type_10_for_viewers_only_test() ->
    with_news_guild(fun(State) ->
        Viewer = guild_data:get_guild_state(?A, State, #{thread_viewer => true, bot => false}),
        [News] = [
            T
         || T <- maps:get(<<"threads">>, Viewer), maps:get(<<"id">>, T) =:= ?NEWS_THREAD
        ],
        ?assertEqual(10, maps:get(<<"type">>, News)),
        ?assert(maps:is_key(<<"member">>, News)),
        Bot = guild_data:get_guild_state(?X, State, #{thread_viewer => true, bot => true}),
        ?assert(
            lists:member(?NEWS_THREAD, [
                maps:get(<<"id">>, T)
             || T <- maps:get(<<"threads">>, Bot)
            ])
        ),
        Control = guild_data:get_guild_state(?C, State, #{thread_viewer => false}),
        ?assertNot(maps:is_key(<<"threads">>, Control)),
        Ids = [maps:get(<<"id">>, C) || C <- maps:get(<<"channels">>, Control)],
        ?assert(lists:member(?NEWS, Ids)),
        ?assertNot(lists:member(?NEWS_THREAD, Ids))
    end).

news_thread_messages_and_crosspost_updates_hide_threads_from_control_test() ->
    with_news_guild(fun(State) ->
        Message = #{
            <<"id">> => <<"910">>,
            <<"channel_id">> => integer_to_binary(?NEWS_THREAD),
            <<"type">> => 0,
            <<"content">> => <<"hi">>,
            <<"author">> => #{<<"id">> => integer_to_binary(?X)},
            <<"mentions">> => [],
            <<"mention_roles">> => []
        },
        _ = dispatch(message_create, Message, State),
        Events1 = drain(),
        ?assertMatch([_], received(a, message_create, Events1)),
        ?assertEqual([], received(c, message_create, Events1)),
        ?assertEqual([], received(i, message_create, Events1)),
        Created = #{
            <<"id">> => <<"911">>,
            <<"channel_id">> => integer_to_binary(?NEWS),
            <<"type">> => 18,
            <<"author">> => #{<<"id">> => integer_to_binary(?A)},
            <<"mentions">> => [],
            <<"mention_roles">> => []
        },
        _ = dispatch(message_create, Created, State),
        Events2 = drain(),
        ?assertMatch([_], received(a, message_create, Events2)),
        ?assertEqual([], received(c, message_create, Events2)),
        Crossposted = #{
            <<"id">> => <<"912">>,
            <<"channel_id">> => integer_to_binary(?NEWS),
            <<"flags">> => 16#20 bor 1,
            <<"thread">> => thread(912, ?NEWS, 10)
        },
        _ = dispatch(message_update, Crossposted, State),
        Events3 = drain(),
        [Full] = received(a, message_update, Events3),
        ?assertEqual(16#21, maps:get(<<"flags">>, Full)),
        ?assertEqual(10, maps:get(<<"type">>, maps:get(<<"thread">>, Full))),
        lists:foreach(
            fun(Tag) ->
                [Masked] = received(Tag, message_update, Events3),
                ?assertEqual(1, maps:get(<<"flags">>, Masked)),
                ?assertNot(maps:is_key(<<"thread">>, Masked))
            end,
            [c, i]
        )
    end).

news_thread_list_sync_includes_type_10_test() ->
    with_news_guild(fun(State0) ->
        State1 = guild_thread_subscriptions:update(<<"v">>, #{threads => true}, State0),
        _ = guild_thread_subscriptions:handle_tick(<<"v">>, State1),
        [Sync] = received(v, thread_list_sync, drain()),
        ?assertEqual(
            [integer_to_binary(?T1), integer_to_binary(?NEWS_THREAD)],
            lists:sort([maps:get(<<"id">>, T) || T <- maps:get(<<"threads">>, Sync)])
        )
    end).

inactive_guild_drops_news_threads_and_keeps_announcement_channels_test() ->
    with_config(fun() ->
        Pids = start_captures(),
        try
            State = inactive_state(Pids),
            _ = dispatch(thread_create, thread(803, ?TEXT, 10), State),
            _ = dispatch(channel_create, #{<<"id">> => <<"804">>, <<"type">> => 10}, State),
            ?assertEqual([], drain()),
            News = #{<<"id">> => <<"805">>, <<"type">> => 5, <<"permission_overwrites">> => []},
            _ = dispatch(channel_create, News, State),
            Events = drain(),
            [ForI] = received(i, channel_create, Events),
            ?assertEqual(5, maps:get(<<"type">>, ForI)),
            ?assertMatch([_], received(c, channel_create, Events))
        after
            stop_captures(Pids)
        end
    end).

news_thread_handoff_round_trips_test() ->
    with_news_guild(fun(State) ->
        Exported = guild_handoff:export_handoff_state(State),
        Restored = guild_init:init_base_state(Exported),
        try
            ?assertMatch(
                #{<<"type">> := 10}, guild_thread_gate:thread(?NEWS_THREAD, Restored)
            ),
            ?assert(
                permission_bits:has(
                    guild_permissions:get_member_permissions(?V, ?NEWS_THREAD, Restored), ?VIEW
                )
            )
        after
            guild_thread_store:destroy(guild_thread_gate:store(Restored))
        end
    end).
