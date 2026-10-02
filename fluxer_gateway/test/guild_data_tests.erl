%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

read_model_preserves_query_results_test() ->
    State = read_model_state(),
    try
        ok = guild_read_model:put_state(State),
        lists:foreach(
            fun({Tag, Request, Handler}) ->
                {reply, Expected, _} = Handler(Request, State),
                ?assertEqual({ok, Expected}, guild_read_model:query(100, {Tag, Request}))
            end,
            [
                {get_guild_data, #{user_id => 200}, fun guild_data:get_guild_data/2},
                {get_guild_data, #{user_id => 999}, fun guild_data:get_guild_data/2},
                {get_guild_data, #{user_id => null}, fun guild_data:get_guild_data/2},
                {get_guild_auth_context, #{user_id => 200, channel_id => 500},
                    fun guild_data:get_auth_context/2},
                {get_guild_auth_context, #{user_id => 999, channel_id => null},
                    fun guild_data:get_auth_context/2},
                {get_guild_member, #{user_id => 200}, fun guild_data:get_guild_member/2},
                {get_guild_member, #{user_id => 999}, fun guild_data:get_guild_member/2},
                {has_member, #{user_id => 200}, fun guild_data:has_member/2}
            ]
        )
    after
        cleanup_read_model(State)
    end.

read_model_uses_live_member_rows_without_copying_members_test() ->
    State = read_model_state(),
    #{data := #{members_ets := Tab}} = State,
    try
        ok = guild_read_model:put_state(State),
        [{100, _, #{data := SnapshotData}}] = ets:lookup(guild_read_model, 100),
        ?assertEqual(#{}, maps:get(<<"members">>, SnapshotData)),
        [{200, Member}] = ets:lookup(Tab, 200),
        Updated = Member#{
            <<"nick">> => <<"new nickname">>, <<"communication_disabled_until">> => null
        },
        ets:insert(Tab, {200, Updated}),
        ?assertEqual(
            {ok, #{success => true, member_data => Updated}},
            guild_read_model:query(100, {get_guild_member, #{user_id => 200}})
        ),
        ets:delete(Tab, 200),
        ?assertEqual(
            {ok, #{has_member => false}},
            guild_read_model:query(100, {has_member, #{user_id => 200}})
        ),
        ?assertMatch(
            {ok, #{guild_data := null}},
            guild_read_model:query(100, {get_guild_data, #{user_id => 200}})
        )
    after
        cleanup_read_model(State)
    end.

read_model_observes_role_and_collection_changes_test() ->
    State = read_model_state(),
    try
        ok = guild_read_model:put_state(State),
        Data = maps:get(data, State),
        UpdatedData = guild_data_index:normalize_map(Data#{
            <<"roles">> => [#{<<"id">> => 100, <<"permissions">> => 0}],
            <<"emojis">> => [#{<<"id">> => 700, <<"name">> => <<"new">>}]
        }),
        Updated = State#{data => UpdatedData},
        ok = guild_read_model:update(State, Updated),
        {reply, Expected, _} = guild_data:get_guild_data(#{user_id => 200}, Updated),
        ?assertEqual(
            {ok, Expected}, guild_read_model:query(100, {get_guild_data, #{user_id => 200}})
        ),
        #{guild_data := GuildData} = Expected,
        ?assertEqual([], maps:get(<<"channels">>, GuildData))
    after
        cleanup_read_model(State)
    end.

read_model_survives_blocked_owner_and_rejects_dead_tables_test() ->
    State = read_model_state(),
    Self = self(),
    Owner = spawn(fun() ->
        ok = guild_read_model:put_state(State),
        Self ! published,
        receive
            stop -> ok
        end
    end),
    try
        receive
            published -> ok
        after 1000 -> error(publish_timeout)
        end,
        ?assertMatch(
            {ok, #{auth_context := #{}}},
            guild_read_model:query(
                100, {get_guild_auth_context, #{user_id => 200, channel_id => 500}}
            )
        ),
        ?assertEqual({message_queue_len, 0}, process_info(Owner, message_queue_len)),
        ets:delete(maps:get(members_ets, maps:get(data, State))),
        ?assertEqual(miss, guild_read_model:query(100, {has_member, #{user_id => 200}}))
    after
        Owner ! stop,
        cleanup_read_model(State)
    end.

read_model_state() ->
    State = test_state(),
    Data = guild_data_index:normalize_map(maps:get(data, State)),
    Members = guild_data_index:member_map(Data),
    Tab = ets:new(read_model_members, [set, public]),
    ets:insert(Tab, maps:to_list(Members)),
    State#{member_count => map_size(Members), data => Data#{members_ets => Tab}}.

cleanup_read_model(#{data := Data} = State) ->
    guild_read_model:delete(100),
    catch ets:delete(maps:get(members_ets, Data)),
    ets:delete(maps:get(member_presence, State)).

get_guild_data_membership_gate_test() ->
    State = test_state(),
    {reply, Reply1, _} = guild_data:get_guild_data(#{user_id => 999}, State),
    ?assertEqual(null, maps:get(guild_data, Reply1)),
    ?assertEqual(<<"forbidden">>, maps:get(error_reason, Reply1)),
    {reply, Reply2, _} = guild_data:get_guild_data(#{user_id => 200}, State),
    Guild = maps:get(guild_data, Reply2),
    ?assertEqual(<<"Fluxer">>, maps:get(<<"name">>, Guild)),
    Roles = maps:get(<<"roles">>, Guild, []),
    ?assertMatch([_ | _], Roles).

get_auth_context_membership_gate_test() ->
    State = test_state(),
    {reply, Reply1, _} = guild_data:get_auth_context(
        #{user_id => 999, channel_id => null}, State
    ),
    ?assertEqual(null, maps:get(auth_context, Reply1)),
    ?assertEqual(<<"forbidden">>, maps:get(error_reason, Reply1)),
    {reply, Reply2, _} = guild_data:get_auth_context(
        #{user_id => 200, channel_id => null}, State
    ),
    Context = maps:get(auth_context, Reply2),
    Guild = maps:get(<<"guild">>, Context),
    ?assertEqual(<<"Fluxer">>, maps:get(<<"name">>, Guild)),
    ?assertMatch([_ | _], maps:get(<<"roles">>, Guild, [])),
    ?assertEqual(null, maps:get(<<"parent_channel">>, Context)).

get_auth_context_omits_collections_test() ->
    State = test_state(),
    {reply, Reply, _} = guild_data:get_auth_context(
        #{user_id => 200, channel_id => null}, State
    ),
    Guild = maps:get(<<"guild">>, maps:get(auth_context, Reply)),
    ?assertNot(maps:is_key(<<"channels">>, Guild)),
    ?assertNot(maps:is_key(<<"emojis">>, Guild)),
    ?assertNot(maps:is_key(<<"stickers">>, Guild)).

get_auth_context_resolves_requested_channel_test() ->
    State = test_state(),
    {reply, Reply, _} = guild_data:get_auth_context(
        #{user_id => 200, channel_id => 500}, State
    ),
    Parent = maps:get(<<"parent_channel">>, maps:get(auth_context, Reply)),
    ?assertEqual(500, maps:get(<<"id">>, Parent)),
    ?assertEqual(0, maps:get(<<"type">>, Parent)).

get_auth_context_unknown_channel_is_null_test() ->
    State = test_state(),
    {reply, Reply, _} = guild_data:get_auth_context(
        #{user_id => 200, channel_id => 999}, State
    ),
    ?assertEqual(null, maps:get(<<"parent_channel">>, maps:get(auth_context, Reply))).

get_auth_context_null_user_skips_membership_test() ->
    State = test_state(),
    {reply, Reply, _} = guild_data:get_auth_context(
        #{user_id => null, channel_id => null}, State
    ),
    Guild = maps:get(<<"guild">>, maps:get(auth_context, Reply)),
    ?assertEqual(<<"Fluxer">>, maps:get(<<"name">>, Guild)).

get_auth_context_matches_get_data_scalars_test() ->
    State = test_state(),
    {reply, DataReply, _} = guild_data:get_guild_data(#{user_id => 200}, State),
    {reply, ContextReply, _} = guild_data:get_auth_context(
        #{user_id => 200, channel_id => null}, State
    ),
    Full = maps:get(guild_data, DataReply),
    Lean = maps:get(<<"guild">>, maps:get(auth_context, ContextReply)),
    Collections = [<<"channels">>, <<"emojis">>, <<"stickers">>],
    Expected = maps:without(Collections, Full),
    ?assertEqual(Expected, Lean).

get_auth_context_is_reachable_through_the_query_handler_test() ->
    State = test_state(),
    Request = {get_guild_auth_context, #{user_id => 200, channel_id => 500}},
    {reply, #{auth_context := #{<<"parent_channel">> := #{<<"id">> := ParentChannelId}}}, _} =
        guild_query_handler:handle_call(Request, {self(), make_ref()}, State),
    ?assertEqual(500, ParentChannelId).

get_guild_state_filters_channels_test() ->
    State = test_state(),
    GuildState = guild_data:get_guild_state(200, State),
    Channels = maps:get(<<"channels">>, GuildState),
    ?assert(lists:any(fun(Chan) -> maps:get(<<"id">>, Chan) =:= 500 end, Channels)),
    ?assertEqual(<<"2024-01-01T00:00:00Z">>, maps:get(<<"joined_at">>, GuildState)).

has_own_member(UserId, GuildState) ->
    Bin = integer_to_binary(UserId),
    lists:any(
        fun(M) -> maps:get(<<"id">>, maps:get(<<"user">>, M, #{}), undefined) =:= Bin end,
        maps:get(<<"members">>, GuildState, [])
    ).

get_guild_state_non_member_returns_partial_guild_create_test() ->
    State = test_state(),
    MemberView = guild_data:get_guild_state(200, State),
    NonMemberView = guild_data:get_guild_state(999, State),
    ?assert(has_own_member(200, MemberView)),
    ?assertMatch([_ | _], maps:get(<<"channels">>, MemberView, [])),
    ?assertNot(has_own_member(999, NonMemberView)),
    ?assertEqual([], maps:get(<<"channels">>, NonMemberView, [])),
    ?assertEqual(false, maps:get(<<"unavailable">>, NonMemberView, false)),
    {reply, Reply, _} = guild_data:get_guild_data(#{user_id => 999}, State),
    ?assertEqual(<<"forbidden">>, maps:get(error_reason, Reply)).

find_everyone_viewable_text_channel_test() ->
    State = test_state(),
    Data = guild_data_index:ensure_data_map(State),
    Channels = maps:get(<<"channels">>, Data),
    ChannelId = guild_data:find_everyone_viewable_text_channel(Channels, State),
    ?assertEqual(500, ChannelId).

find_everyone_viewable_text_channel_uses_category_order_test() ->
    GuildId = 100,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                }
            ]
        }
    },
    Channels = [
        #{
            <<"id">> => <<"10">>,
            <<"type">> => 4,
            <<"position">> => 1,
            <<"permission_overwrites">> => []
        },
        #{
            <<"id">> => <<"20">>,
            <<"type">> => 4,
            <<"position">> => 2,
            <<"permission_overwrites">> => []
        },
        #{
            <<"id">> => <<"21">>,
            <<"type">> => 0,
            <<"parent_id">> => <<"20">>,
            <<"position">> => 3,
            <<"permission_overwrites">> => []
        },
        #{
            <<"id">> => <<"11">>,
            <<"type">> => 0,
            <<"parent_id">> => <<"10">>,
            <<"position">> => 50,
            <<"permission_overwrites">> => []
        }
    ],
    ChannelId = guild_data:find_everyone_viewable_text_channel(Channels, State),
    ?assertEqual(11, ChannelId).

find_everyone_viewable_text_channel_prefers_root_before_categories_test() ->
    GuildId = 100,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                }
            ]
        }
    },
    Channels = [
        #{
            <<"id">> => <<"10">>,
            <<"type">> => 4,
            <<"position">> => 1,
            <<"permission_overwrites">> => []
        },
        #{
            <<"id">> => <<"11">>,
            <<"type">> => 0,
            <<"parent_id">> => <<"10">>,
            <<"position">> => 2,
            <<"permission_overwrites">> => []
        },
        #{
            <<"id">> => <<"9">>,
            <<"type">> => 0,
            <<"position">> => 50,
            <<"permission_overwrites">> => []
        }
    ],
    ChannelId = guild_data:find_everyone_viewable_text_channel(Channels, State),
    ?assertEqual(9, ChannelId).

find_everyone_viewable_text_channel_skips_invalid_channel_id_test() ->
    GuildId = 100,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                }
            ]
        }
    },
    Channels = [
        #{
            <<"id">> => <<"001">>,
            <<"type">> => 0,
            <<"position">> => 1,
            <<"permission_overwrites">> => []
        },
        #{
            <<"id">> => <<"12">>,
            <<"type">> => 0,
            <<"position">> => 2,
            <<"permission_overwrites">> => []
        }
    ],
    ChannelId = guild_data:find_everyone_viewable_text_channel(Channels, State),
    ?assertEqual(12, ChannelId).

find_everyone_viewable_text_channel_ignores_user_overwrite_for_guild_id_test() ->
    GuildId = 100,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                }
            ]
        }
    },
    Channels = [
        #{
            <<"id">> => <<"12">>,
            <<"type">> => 0,
            <<"permission_overwrites">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"type">> => 1,
                    <<"allow">> => <<"0">>,
                    <<"deny">> => integer_to_binary(ViewPerm)
                }
            ]
        }
    ],
    ChannelId = guild_data:find_everyone_viewable_text_channel(Channels, State),
    ?assertEqual(12, ChannelId).

find_everyone_viewable_text_channel_accepts_voice_when_no_text_channel_test() ->
    GuildId = 100,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                }
            ]
        }
    },
    Channels = [
        #{<<"id">> => <<"501">>, <<"type">> => 2, <<"permission_overwrites">> => []}
    ],
    ChannelId = guild_data:find_everyone_viewable_text_channel(Channels, State),
    ?assertEqual(501, ChannelId).

find_everyone_viewable_text_channel_skips_link_channel_test() ->
    GuildId = 100,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                }
            ]
        }
    },
    Channels = [
        #{<<"id">> => <<"998">>, <<"type">> => 998, <<"permission_overwrites">> => []}
    ],
    ChannelId = guild_data:find_everyone_viewable_text_channel(Channels, State),
    ?assertEqual(null, ChannelId).

find_everyone_viewable_text_channel_accepts_announcement_channel_test() ->
    GuildId = 100,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                }
            ]
        }
    },
    Channels = [
        #{
            <<"id">> => <<"501">>,
            <<"type">> => 2,
            <<"position">> => 0,
            <<"permission_overwrites">> => []
        },
        #{
            <<"id">> => <<"502">>,
            <<"type">> => 5,
            <<"position">> => 1,
            <<"permission_overwrites">> => []
        }
    ],
    ChannelId = guild_data:find_everyone_viewable_text_channel(Channels, State),
    ?assertEqual(502, ChannelId).

sort_channels_for_ordering_places_announcement_channels_with_text_test() ->
    Channels = [
        #{<<"id">> => <<"1">>, <<"type">> => 2, <<"position">> => 0},
        #{<<"id">> => <<"2">>, <<"type">> => 5, <<"position">> => 2},
        #{<<"id">> => <<"3">>, <<"type">> => 0, <<"position">> => 1},
        #{<<"id">> => <<"4">>, <<"type">> => 4, <<"position">> => 3},
        #{
            <<"id">> => <<"5">>,
            <<"type">> => 2,
            <<"position">> => 0,
            <<"parent_id">> => <<"4">>
        },
        #{<<"id">> => <<"6">>, <<"type">> => 5, <<"position">> => 1, <<"parent_id">> => <<"4">>}
    ],
    Ordered = guild_data_channels:sort_channels_for_ordering(Channels),
    ?assertEqual(
        [<<"3">>, <<"2">>, <<"1">>, <<"4">>, <<"6">>, <<"5">>],
        [maps:get(<<"id">>, C) || C <- Ordered]
    ).

voice_members_from_states_reads_embedded_member_test() ->
    EmbeddedMember = #{<<"user">> => #{<<"id">> => <<"300">>}, <<"roles">> => []},
    IndexedMember = #{<<"user">> => #{<<"id">> => <<"200">>}, <<"roles">> => []},
    VoiceStates = [
        #{<<"user_id">> => <<"300">>, <<"member">> => EmbeddedMember},
        #{<<"user_id">> => <<"200">>}
    ],
    VoiceMembers = guild_data_channels:voice_members_from_states(VoiceStates, [IndexedMember]),
    ?assertEqual([EmbeddedMember, IndexedMember], VoiceMembers).

paginate_members_test() ->
    Members = [#{<<"id">> => 1}, #{<<"id">> => 2}, #{<<"id">> => 3}],
    ?assertEqual(
        [#{<<"id">> => 1}, #{<<"id">> => 2}],
        guild_data_members:paginate_members(Members, 2, 0)
    ),
    ?assertEqual(
        [#{<<"id">> => 2}, #{<<"id">> => 3}],
        guild_data_members:paginate_members(Members, 2, 1)
    ),
    ?assertEqual(
        [#{<<"id">> => 3}],
        guild_data_members:paginate_members(Members, 2, 2)
    ),
    ?assertEqual(
        [],
        guild_data_members:paginate_members(Members, 2, 5)
    ).

search_guild_members_limits_prefix_matches_without_paginating_all_test() ->
    State = #{
        data => #{
            <<"members">> => [
                member(1, <<"Alice">>),
                member(2, <<"Alicia">>),
                member(3, <<"Bob">>)
            ]
        }
    },
    {reply, Reply, _State} = guild_data:search_guild_members(
        #{query => <<"ali">>, limit => 1}, State
    ),
    Members = maps:get(members, Reply),
    ?assertEqual(1, length(Members)),
    ?assertEqual(3, maps:get(total, Reply)),
    ?assert(
        lists:all(
            fun(Member) ->
                guild_request_members_search:member_matches_normalized_query(Member, <<"ali">>)
            end,
            Members
        )
    ).

search_guild_members_matches_username_of_nicknamed_member_test() ->
    Nicknamed = (member(1, <<"jiralite">>))#{<<"nick">> => <<"Specsaver engineer">>},
    State = #{data => #{<<"members">> => [Nicknamed, member(2, <<"Bob">>)]}},
    {reply, Reply, _State} = guild_data:search_guild_members(
        #{query => <<"jiralite">>, limit => 25}, State
    ),
    ?assertEqual(
        [1],
        [guild_request_members_search:extract_user_id(M) || M <- maps:get(members, Reply)]
    ).

get_guild_state_includes_parent_category_when_child_channel_is_visible_test() ->
    GuildId = 50,
    UserId = 300,
    RoleId = 77,
    CategoryId = 600,
    ChannelId = 601,
    ViewPerm = constants:view_channel_permission(),
    State = parent_category_visible_state(
        GuildId, UserId, RoleId, CategoryId, ChannelId, ViewPerm
    ),
    GuildState = guild_data:get_guild_state(UserId, State),
    Channels = maps:get(<<"channels">>, GuildState),
    ChannelIds = guild_state_channel_ids(Channels),
    ?assertEqual([CategoryId, ChannelId], ChannelIds).

parent_category_visible_state(GuildId, UserId, RoleId, CategoryId, ChannelId, ViewPerm) ->
    #{
        id => GuildId,
        member_presence => ets:new(test_member_presence, [set, public]),
        data => #{
            <<"guild">> => #{<<"name">> => <<"Test Guild">>},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>},
                #{
                    <<"id">> => integer_to_binary(RoleId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                }
            ],
            <<"channels">> => [
                parent_category_channel(CategoryId, RoleId, ViewPerm),
                child_visible_channel(ChannelId, CategoryId, RoleId, ViewPerm)
            ],
            <<"members">> => [
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                    <<"roles">> => [integer_to_binary(RoleId)],
                    <<"joined_at">> => <<"2024-01-01T00:00:00Z">>
                }
            ],
            <<"emojis">> => [],
            <<"stickers">> => []
        }
    }.

parent_category_channel(CategoryId, RoleId, ViewPerm) ->
    #{
        <<"id">> => integer_to_binary(CategoryId),
        <<"type">> => 4,
        <<"permission_overwrites">> => [
            role_overwrite(RoleId, <<"0">>, integer_to_binary(ViewPerm))
        ]
    }.

child_visible_channel(ChannelId, CategoryId, RoleId, ViewPerm) ->
    #{
        <<"id">> => integer_to_binary(ChannelId),
        <<"type">> => 0,
        <<"parent_id">> => integer_to_binary(CategoryId),
        <<"permission_overwrites">> => [
            role_overwrite(RoleId, integer_to_binary(ViewPerm), <<"0">>)
        ]
    }.

role_overwrite(RoleId, Allow, Deny) ->
    #{
        <<"id">> => integer_to_binary(RoleId),
        <<"type">> => 0,
        <<"allow">> => Allow,
        <<"deny">> => Deny
    }.

guild_state_channel_ids(Channels) ->
    lists:sort([snowflake_id:parse(maps:get(<<"id">>, C)) || C <- Channels]).

member(UserId, Username) ->
    #{
        <<"user">> => #{
            <<"id">> => integer_to_binary(UserId),
            <<"username">> => Username
        },
        <<"roles">> => []
    }.

test_state() ->
    GuildId = 100,
    ViewPerm = constants:view_channel_permission(),
    #{
        id => GuildId,
        member_presence => ets:new(test_member_presence, [set, public]),
        data => #{
            <<"guild">> => #{<<"name">> => <<"Fluxer">>},
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                }
            ],
            <<"channels">> => [
                #{<<"id">> => <<"500">>, <<"type">> => 0, <<"permission_overwrites">> => []},
                #{<<"id">> => <<"501">>, <<"type">> => 2, <<"permission_overwrites">> => []}
            ],
            <<"members">> => [
                #{
                    <<"user">> => #{<<"id">> => <<"200">>},
                    <<"roles">> => [integer_to_binary(GuildId)],
                    <<"joined_at">> => <<"2024-01-01T00:00:00Z">>
                }
            ],
            <<"emojis">> => [],
            <<"stickers">> => []
        }
    }.
