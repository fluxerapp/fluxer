%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_permissions_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

owner_receives_full_permissions_test() ->
    OwnerId = 1,
    GuildId = 100,
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => integer_to_binary(OwnerId)},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>}
            ]
        }
    },
    ?assertEqual(
        all_permissions(), guild_permissions:get_member_permissions(OwnerId, undefined, State)
    ).

channel_scope_permissions_test() ->
    GuildId = 42,
    UserId = 600,
    ChannelId = 700,
    RoleId = 800,
    View = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => integer_to_binary(GuildId + 1)},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>},
                #{<<"id">> => integer_to_binary(RoleId), <<"permissions">> => <<"0">>}
            ],
            <<"members">> => [
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                    <<"roles">> => [integer_to_binary(RoleId)]
                }
            ],
            <<"channels">> => [
                #{
                    <<"id">> => integer_to_binary(ChannelId),
                    <<"permission_overwrites">> => [
                        #{
                            <<"id">> => integer_to_binary(GuildId),
                            <<"type">> => 0,
                            <<"allow">> => <<"0">>,
                            <<"deny">> => <<"0">>
                        },
                        #{
                            <<"id">> => integer_to_binary(RoleId),
                            <<"type">> => 0,
                            <<"allow">> => integer_to_binary(View),
                            <<"deny">> => <<"0">>
                        }
                    ]
                }
            ]
        }
    },
    ?assertEqual(0, guild_permissions:get_member_permissions(UserId, undefined, State)),
    ChannelPerms = guild_permissions:get_member_permissions(UserId, ChannelId, State),
    ?assert((ChannelPerms band View) =/= 0).

administrator_role_grants_all_permissions_test() ->
    Admin = constants:administrator_permission(),
    GuildId = 100,
    UserId = 200,
    ChannelId = 300,
    OwnerId = 999,
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => integer_to_binary(OwnerId)},
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(Admin)
                }
            ],
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => []}
            ],
            <<"channels">> => [
                #{<<"id">> => integer_to_binary(ChannelId), <<"permission_overwrites">> => []}
            ]
        }
    },
    ?assertEqual(
        all_permissions(), guild_permissions:get_member_permissions(UserId, undefined, State)
    ),
    ?assertEqual(
        all_permissions(), guild_permissions:get_member_permissions(UserId, ChannelId, State)
    ),
    ?assert(guild_permissions_check:can_view_channel(UserId, ChannelId, undefined, State)).

cached_role_permissions_matches_uncached_test() ->
    Roles = [
        #{<<"id">> => <<"100">>, <<"permissions">> => <<"1024">>},
        #{<<"id">> => <<"200">>, <<"permissions">> => <<"8">>},
        #{<<"id">> => <<"300">>, <<"permissions">> => <<"2048">>}
    ],
    RoleIndex = guild_data_index:build_id_index(Roles),
    Cache = guild_data_index_roles:build_role_perms_cache(Roles),
    MemberRoles = [100, 200, 300],
    Uncached = uncached_role_permissions(MemberRoles, RoleIndex, 0),
    Cached = guild_permissions:aggregate_role_permissions_cached(
        MemberRoles, Cache, RoleIndex, 0
    ),
    ?assertEqual(Uncached, Cached).

cached_role_permissions_with_base_test() ->
    Roles = [
        #{<<"id">> => <<"5">>, <<"permissions">> => <<"64">>},
        #{<<"id">> => <<"10">>, <<"permissions">> => <<"128">>}
    ],
    RoleIndex = guild_data_index:build_id_index(Roles),
    Cache = guild_data_index_roles:build_role_perms_cache(Roles),
    BasePerms = 64,
    MemberRoles = [10],
    Uncached = uncached_role_permissions(MemberRoles, RoleIndex, BasePerms),
    Cached = guild_permissions:aggregate_role_permissions_cached(
        MemberRoles, Cache, RoleIndex, BasePerms
    ),
    ?assertEqual(Uncached, Cached).

uncached_role_permissions(MemberRoles, RoleIndex, BasePerms) ->
    lists:foldl(
        fun(RoleId, Acc) -> add_role_permissions(RoleId, RoleIndex, Acc) end,
        BasePerms,
        MemberRoles
    ).

add_role_permissions(RoleId, RoleIndex, Acc) ->
    case guild_permissions_check:find_role_by_id(RoleId, RoleIndex) of
        undefined -> Acc;
        Role -> Acc bor guild_permissions_common:role_permissions(Role)
    end.

cached_full_permission_computation_matches_uncached_test() ->
    View = constants:view_channel_permission(),
    GuildId = 100,
    UserId = 42,
    Roles = [
        #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => integer_to_binary(View)},
        #{<<"id">> => <<"200">>, <<"permissions">> => <<"8">>}
    ],
    Channels = [
        #{
            <<"id">> => <<"500">>,
            <<"permission_overwrites">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"type">> => 0,
                    <<"allow">> => <<"0">>,
                    <<"deny">> => integer_to_binary(View)
                },
                #{
                    <<"id">> => <<"200">>,
                    <<"type">> => 0,
                    <<"allow">> => integer_to_binary(View),
                    <<"deny">> => <<"0">>
                }
            ]
        }
    ],
    Members = #{
        UserId => #{
            <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
            <<"roles">> => [<<"200">>]
        }
    },
    RawData = #{
        <<"guild">> => #{<<"owner_id">> => <<"999">>},
        <<"roles">> => Roles,
        <<"channels">> => Channels,
        <<"members">> => Members
    },
    RawState = #{id => GuildId, data => RawData},
    UncachedPerms = guild_permissions:get_member_permissions(UserId, 500, RawState),
    NormData = guild_data_index:normalize_data(RawData),
    CachedState = #{id => GuildId, data => NormData},
    CachedPerms = guild_permissions:get_member_permissions(UserId, 500, CachedState),
    ?assertEqual(UncachedPerms, CachedPerms),
    ?assert(UncachedPerms > 0).

can_view_channel_members_grants_when_bit_set_test() ->
    Bit = constants:view_channel_members_permission(),
    GuildId = 100,
    UserId = 200,
    OwnerId = 999,
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => integer_to_binary(OwnerId)},
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(Bit)
                }
            ],
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => []}
            ],
            <<"channels">> => [#{<<"id">> => <<"500">>, <<"permission_overwrites">> => []}]
        }
    },
    ?assert(guild_permissions_check:can_view_channel_members(UserId, 500, undefined, State)).

can_view_channel_members_denies_when_bit_unset_test() ->
    GuildId = 101,
    UserId = 201,
    OwnerId = 998,
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => integer_to_binary(OwnerId)},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>}
            ],
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => []}
            ],
            <<"channels">> => [#{<<"id">> => <<"500">>, <<"permission_overwrites">> => []}]
        }
    },
    ?assertNot(guild_permissions_check:can_view_channel_members(UserId, 500, undefined, State)).

can_view_channel_members_owner_always_true_test() ->
    GuildId = 102,
    OwnerId = 102,
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => integer_to_binary(OwnerId)},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>}
            ],
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => integer_to_binary(OwnerId)}, <<"roles">> => []}
            ],
            <<"channels">> => [#{<<"id">> => <<"500">>, <<"permission_overwrites">> => []}]
        }
    },
    ?assert(guild_permissions_check:can_view_channel_members(OwnerId, 500, undefined, State)).

can_view_channel_members_admin_grants_test() ->
    Admin = constants:administrator_permission(),
    GuildId = 103,
    UserId = 203,
    OwnerId = 997,
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => integer_to_binary(OwnerId)},
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(Admin)
                }
            ],
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => []}
            ],
            <<"channels">> => [#{<<"id">> => <<"500">>, <<"permission_overwrites">> => []}]
        }
    },
    ?assert(guild_permissions_check:can_view_channel_members(UserId, 500, undefined, State)).

can_view_channel_members_channel_deny_overwrite_blocks_test() ->
    Bit = constants:view_channel_members_permission(),
    View = constants:view_channel_permission(),
    GuildId = 104,
    UserId = 204,
    OwnerId = 996,
    Base = Bit bor View,
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => integer_to_binary(OwnerId)},
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(Base)
                }
            ],
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => []}
            ],
            <<"channels">> => [
                #{
                    <<"id">> => <<"500">>,
                    <<"permission_overwrites">> => [
                        #{
                            <<"id">> => integer_to_binary(GuildId),
                            <<"type">> => 0,
                            <<"allow">> => <<"0">>,
                            <<"deny">> => integer_to_binary(Bit)
                        }
                    ]
                }
            ]
        }
    },
    ?assertNot(guild_permissions_check:can_view_channel_members(UserId, 500, undefined, State)).

can_access_message_with_read_history_test() ->
    ReadHistory = constants:read_message_history_permission(),
    State = #{data => #{<<"guild">> => #{}}},
    ?assertEqual(
        true,
        guild_permissions_check:can_access_message_by_permissions(ReadHistory, <<"100">>, State)
    ).

can_access_message_no_read_history_no_cutoff_test() ->
    State = #{data => #{<<"guild">> => #{}}},
    ?assertEqual(
        false, guild_permissions_check:can_access_message_by_permissions(0, <<"100">>, State)
    ).

can_access_message_no_read_history_null_cutoff_test() ->
    State = #{data => #{<<"guild">> => #{<<"message_history_cutoff">> => null}}},
    ?assertEqual(
        false, guild_permissions_check:can_access_message_by_permissions(0, <<"100">>, State)
    ).

can_access_message_no_read_history_message_before_cutoff_test() ->
    CutoffMs = 1704067200000,
    BeforeCutoffTimestamp = CutoffMs - 60000,
    FluxerEpoch = 1420070400000,
    RelativeTs = BeforeCutoffTimestamp - FluxerEpoch,
    Snowflake = RelativeTs bsl 22,
    MessageId = integer_to_binary(Snowflake),
    State = #{data => #{<<"guild">> => #{<<"message_history_cutoff">> => CutoffMs}}},
    ?assertEqual(
        false, guild_permissions_check:can_access_message_by_permissions(0, MessageId, State)
    ).

can_access_message_no_read_history_message_after_cutoff_test() ->
    CutoffMs = 1704067200000,
    AfterCutoffTimestamp = CutoffMs + 60000,
    FluxerEpoch = 1420070400000,
    RelativeTs = AfterCutoffTimestamp - FluxerEpoch,
    Snowflake = RelativeTs bsl 22,
    MessageId = integer_to_binary(Snowflake),
    State = #{data => #{<<"guild">> => #{<<"message_history_cutoff">> => CutoffMs}}},
    ?assertEqual(
        true, guild_permissions_check:can_access_message_by_permissions(0, MessageId, State)
    ).

can_access_message_no_read_history_message_at_cutoff_test() ->
    CutoffMs = 1704067200000,
    FluxerEpoch = 1420070400000,
    RelativeTs = CutoffMs - FluxerEpoch,
    Snowflake = RelativeTs bsl 22,
    MessageId = integer_to_binary(Snowflake),
    State = #{data => #{<<"guild">> => #{<<"message_history_cutoff">> => CutoffMs}}},
    ?assertEqual(
        true, guild_permissions_check:can_access_message_by_permissions(0, MessageId, State)
    ).

can_access_message_with_rfc3339_cutoff_test() ->
    CutoffBin = <<"2024-01-01T00:00:00Z">>,
    CutoffMs = calendar:rfc3339_to_system_time("2024-01-01T00:00:00Z", [{unit, millisecond}]),
    AfterCutoffTimestamp = CutoffMs + 60000,
    FluxerEpoch = 1420070400000,
    RelativeTs = AfterCutoffTimestamp - FluxerEpoch,
    Snowflake = RelativeTs bsl 22,
    MessageId = integer_to_binary(Snowflake),
    State = #{data => #{<<"guild">> => #{<<"message_history_cutoff">> => CutoffBin}}},
    ?assertEqual(
        true, guild_permissions_check:can_access_message_by_permissions(0, MessageId, State)
    ).

all_permissions() ->
    16#FFFFFFFFFFFFFFFF.
