%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_visibility_permission_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

channel_overwrite_denies_view_test() ->
    GuildId = 80,
    UserId = 10,
    RoleId = 200,
    ChannelId = 400,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                },
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
                            <<"id">> => integer_to_binary(RoleId),
                            <<"type">> => 0,
                            <<"allow">> => <<"0">>,
                            <<"deny">> => integer_to_binary(ViewPerm)
                        }
                    ]
                }
            ]
        }
    },
    Channels = guild_visibility:get_user_viewable_channels(UserId, State),
    ?assertEqual([], Channels).

role_overwrite_allows_view_test() ->
    GuildId = 90,
    UserId = 10,
    RoleId = 300,
    ChannelId = 500,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                },
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
                            <<"deny">> => integer_to_binary(ViewPerm)
                        },
                        #{
                            <<"id">> => integer_to_binary(RoleId),
                            <<"type">> => 0,
                            <<"allow">> => integer_to_binary(ViewPerm),
                            <<"deny">> => <<"0">>
                        }
                    ]
                }
            ]
        }
    },
    Channels = guild_visibility:get_user_viewable_channels(UserId, State),
    ?assertEqual([ChannelId], Channels).

user_overwrite_denies_view_test() ->
    GuildId = 91,
    UserId = 10,
    RoleId = 301,
    ChannelId = 501,
    ViewPerm = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                },
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
                            <<"id">> => integer_to_binary(RoleId),
                            <<"type">> => 0,
                            <<"allow">> => integer_to_binary(ViewPerm),
                            <<"deny">> => <<"0">>
                        },
                        #{
                            <<"id">> => integer_to_binary(UserId),
                            <<"type">> => 1,
                            <<"allow">> => <<"0">>,
                            <<"deny">> => integer_to_binary(ViewPerm)
                        }
                    ]
                }
            ]
        }
    },
    Channels = guild_visibility:get_user_viewable_channels(UserId, State),
    ?assertEqual([], Channels).

parent_category_included_when_child_channel_is_visible_test() ->
    GuildId = 40,
    UserId = 11,
    RoleId = 99,
    CategoryId = 200,
    ChannelId = 201,
    ViewPerm = constants:view_channel_permission(),
    State = parent_category_visible_state(
        GuildId, UserId, RoleId, CategoryId, ChannelId, ViewPerm
    ),
    Channels = lists:sort(guild_visibility:get_user_viewable_channels(UserId, State)),
    ?assertEqual([CategoryId, ChannelId], Channels).

parent_category_visible_state(GuildId, UserId, RoleId, CategoryId, ChannelId, ViewPerm) ->
    #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>},
                #{
                    <<"id">> => integer_to_binary(RoleId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                }
            ],
            <<"members">> => [
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                    <<"roles">> => [integer_to_binary(RoleId)]
                }
            ],
            <<"channels">> => [
                parent_category_channel(CategoryId, RoleId, ViewPerm),
                child_visible_channel(ChannelId, CategoryId, RoleId, ViewPerm)
            ]
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
