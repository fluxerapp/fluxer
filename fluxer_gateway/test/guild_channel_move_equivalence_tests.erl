%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_channel_move_equivalence_tests).

-include_lib("eunit/include/eunit.hrl").

-define(GUILD_ID, 7100000).
-define(USER_BASE, 7200000).
-define(CHAN_BASE, 7300000).
-define(ROLE_BASE, 7400000).
-define(DANGLING_PARENT, 7399999).
-define(RECORDER, guild_channel_move_equivalence_recorder).
-define(MODE, guild_channel_move_equivalence_mode).
-define(DEFAULT_SEEDS, 300).
-define(SENSITIVITY_SEEDS, 150).

equivalence_test_() ->
    {timeout, 1800, fun() ->
        with_harness(fun() ->
            First = env_int("CHMOVE_FIRST_SEED", 1),
            lists:foreach(fun check_seed/1, lists:seq(First, First + seed_count() - 1))
        end)
    end}.

sensitivity_test_() ->
    {timeout, 1800, fun() ->
        with_harness(fun() ->
            Undetected = [Name || Name <- broken_modes(), not diverges_for_some_case(Name)],
            ?assertEqual([], Undetected)
        end)
    end}.

move_into_public_category_does_no_per_member_work_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            Spec = ticker_spec(),
            [Moved | _] = ticker_ids(),
            Event = bulk_event(
                move_channels([Moved], {parent, ticker_category()}, layout(Spec))
            ),
            Results = check_spec(Spec, [Event]),
            [#{counts := Counts}] = maps:get(optimized, Results),
            [#{counts := RefCounts}] = maps:get(reference, Results),
            ?assertEqual(0, count(bulk_load, Counts)),
            ?assertEqual(1, count(voice_fetch, Counts)),
            ?assert(count(bulk_load, RefCounts) > 0),
            ?assert(count(voice_fetch, RefCounts) >= 1),
            ?assert(count(can_view, Counts) * 4 < count(can_view, RefCounts))
        end)
    end}.

position_only_bulk_rebuilds_no_member_list_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            Spec = ticker_spec(),
            Results = check_spec(Spec, [position_move(Spec)]),
            [#{counts := Counts}] = maps:get(optimized, Results),
            [#{counts := RefCounts}] = maps:get(reference, Results),
            ?assertEqual(0, count(bulk_load, Counts)),
            ?assert(count(bulk_load, RefCounts) > 0),
            ?assertEqual(1, count(prune, Counts)),
            ?assertEqual(1, count(visibility, Counts))
        end)
    end}.

overwrite_change_on_a_subscribed_list_rebuilds_and_syncs_it_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            Spec = ticker_spec(),
            Text = ticker_category() + 1,
            Changed = (find(Text, layout(Spec)))#{
                <<"permission_overwrites">> => [overwrite(role_id(1), 0, 0, view())]
            },
            Results = check_spec(Spec, [{channel_update, Changed}]),
            [#{log := Log, counts := Counts}] = maps:get(optimized, Results),
            ?assertEqual(1, count(bulk_load, Counts)),
            ?assert(lists:member(guild_member_list_update, [relay_event(E) || E <- Log]))
        end)
    end}.

created_channel_is_in_the_bulk_after_a_move_into_a_category_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            Spec = (create_spec())#{hooks => [{channel_create, new_public_channel()}]},
            Layout = layout(Spec) ++ [new_public_channel()],
            Event = bulk_event(renumber(move_channels([new_id()], {parent, cat_id()}, Layout))),
            Results = check_spec(Spec, [Event]),
            [#{log := Log, state := {_, State}}] = maps:get(optimized, Results),
            Bulks = [
                Ids
             || {relay, _, [_, channel_update_bulk, Payload | _]} <- Log,
                Ids <- [payload_channel_ids(Payload)]
            ],
            ?assertMatch([_ | _], Bulks),
            [?assert(lists:member(new_id(), Ids)) || Ids <- Bulks],
            [
                ?assert(maps:is_key(new_id(), session_map(session_id(L), State)))
             || L <- [1, 2, 3]
            ]
        end)
    end}.

created_channel_survives_a_position_only_move_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            Spec = (create_spec())#{hooks => [{channel_create, new_public_channel()}]},
            Layout = layout(Spec) ++ [new_public_channel()],
            Results = check_spec(Spec, [bulk_event(renumber(lists:reverse(Layout)))]),
            [#{state := {_, State}}] = maps:get(optimized, Results),
            ?assert(maps:is_key(new_id(), session_map(session_id(3), State)))
        end)
    end}.

old_owner_does_not_receive_channels_it_lost_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            Spec = (owner_spec())#{hooks => [{owner, user_id(2)}]},
            Results = check_spec(Spec, [position_move(Spec)]),
            [#{log := Log, state := {_, State}}] = maps:get(optimized, Results),
            ToOldOwner = [
                E
             || {relay, _, [Targets, channel_update_bulk | _]} = E <- Log,
                lists:member({session_pid, 1}, lists:flatten([Targets]))
            ],
            ?assertEqual([], ToOldOwner),
            ?assertEqual(#{}, session_map(session_id(1), State)),
            ?assertNot(maps:is_key(user_id(4), maps:get(member_subscriptions, State)))
        end)
    end}.

virtual_access_flip_leaves_no_stale_member_row_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            Spec = flip_spec(),
            Results = check_spec(Spec, [position_move(Spec)]),
            [#{state := {Engines, _}, counts := Counts}] = maps:get(optimized, Results),
            ?assertEqual(1, count(bulk_load, Counts)),
            ?assertNot(lists:member(user_id(4), engine_user_ids(flip_channel(), Engines)))
        end)
    end}.

owner_flip_leaves_no_stale_member_row_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            Spec = (flip_spec())#{
                hooks => owner_flip_hooks()
            },
            Results = check_spec(Spec, [position_move(Spec)]),
            [#{state := {Engines, _}}] = maps:get(optimized, Results),
            ?assert(lists:member(user_id(1), engine_user_ids(flip_channel(), Engines)))
        end)
    end}.

role_flip_on_an_unsubscribed_list_leaves_no_stale_member_row_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            Spec = (flip_spec())#{
                list_subscriptions => [],
                hooks => [
                    {ensure_list, flip_channel()},
                    {role_permissions, 1, 0},
                    {member_update, user_id(2)},
                    {role_permissions, 1, view()}
                ]
            },
            Results = check_spec(Spec, [position_move(Spec)]),
            [#{state := {Engines, _}}] = maps:get(optimized, Results),
            ?assert(lists:member(user_id(2), engine_user_ids(flip_channel(), Engines)))
        end)
    end}.

single_update_then_bulk_rebuilds_the_category_it_left_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            Spec0 = gap_spec(?CHAN_BASE + 500),
            Spec = Spec0#{
                list_subscriptions => [{session_id(1), integer_to_binary(?CHAN_BASE + 500)}]
            },
            Moved = (find(?CHAN_BASE + 501, layout(Spec)))#{
                <<"parent_id">> => integer_to_binary(?CHAN_BASE + 500)
            },
            After = replace(Moved, layout(Spec)),
            Events = [{channel_update, Moved}, bulk_event(renumber(lists:reverse(After)))],
            Results = check_spec(Spec, Events),
            [_, #{counts := Counts, log := Log}] = maps:get(optimized, Results),
            ?assertEqual(1, count(bulk_load, Counts)),
            ?assert(lists:member(guild_member_list_update, [relay_event(E) || E <- Log]))
        end)
    end}.

category_flip_between_single_updates_leaves_no_stale_member_row_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            {Spec, Events} = category_flip_case(),
            Results = check_spec(Spec, Events),
            [_, _, _, #{state := {Engines, _}, counts := Counts}] = maps:get(
                optimized, Results
            ),
            ?assertEqual(1, count(bulk_load, Counts)),
            ?assert(lists:member(user_id(2), engine_user_ids(?CHAN_BASE + 500, Engines)))
        end)
    end}.

reload_with_changed_member_roles_rebuilds_every_list_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            {Spec, Events} = reload_case(),
            Results = check_spec(Spec, Events),
            [#{state := {Engines, _}, counts := Counts}] = maps:get(optimized, Results),
            ?assertEqual(1, count(bulk_load, Counts)),
            ?assertNot(lists:member(user_id(2), engine_user_ids(flip_channel(), Engines)))
        end)
    end}.

reload_case() ->
    Spec0 = flip_spec(),
    Data = maps:get(data, Spec0),
    Target = integer_to_binary(user_id(2)),
    Reloaded = Data#{
        <<"members">> => [
            case M of
                #{<<"user">> := #{<<"id">> := Target}} -> M#{<<"roles">> => []};
                _ -> M
            end
         || M <- maps:get(<<"members">>, Data)
        ]
    },
    Spec = Spec0#{hooks => [{reload, Reloaded}]},
    {Spec, [position_move(Spec)]}.

category_flip_case() ->
    Spec = (gap_spec(?CHAN_BASE + 501))#{
        list_subscriptions => [{session_id(1), integer_to_binary(?CHAN_BASE + 500)}]
    },
    Child = find(?CHAN_BASE + 501, layout(Spec)),
    {Spec, [
        {channel_update, Child#{<<"parent_id">> => null}},
        {hook, {member_update, user_id(2)}},
        {channel_update, Child},
        position_move(Spec)
    ]}.

bulk_category_flip_leaves_no_stale_member_row_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            {Spec, Events} = bulk_category_flip_case(),
            Results = check_spec(Spec, Events),
            [_, _, _, #{state := {Engines, _}, counts := Counts}] = maps:get(
                optimized, Results
            ),
            ?assertEqual(1, count(bulk_load, Counts)),
            ?assert(lists:member(user_id(2), engine_user_ids(?CHAN_BASE + 500, Engines)))
        end)
    end}.

bulk_category_flip_case() ->
    Spec = (gap_spec(?CHAN_BASE + 501))#{
        list_subscriptions => [{session_id(1), integer_to_binary(?CHAN_BASE + 500)}]
    },
    Child = find(?CHAN_BASE + 501, layout(Spec)),
    {Spec, [
        bulk_event([Child#{<<"parent_id">> => null}]),
        {hook, {member_update, user_id(2)}},
        bulk_event([Child]),
        position_move(Spec)
    ]}.

everyone_patch_flip_leaves_no_stale_member_row_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            {Spec, Events} = everyone_patch_flip_case(),
            Results = check_spec(Spec, Events),
            [#{state := {Engines, _}, counts := Counts}] = maps:get(optimized, Results),
            ?assertEqual(1, count(bulk_load, Counts)),
            ?assertNot(lists:member(user_id(4), engine_user_ids(flip_channel(), Engines)))
        end)
    end}.

everyone_patch_flip_case() ->
    Spec = (flip_spec())#{
        list_subscriptions => [],
        hooks => [
            {ensure_list, flip_channel()},
            {patch_everyone, view()},
            {member_update, user_id(4)},
            {everyone_permissions, members_view() bor connect() bor speak()}
        ]
    },
    {Spec, [position_move(Spec)]}.

created_child_flip_leaves_no_stale_member_row_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            {Spec, Events} = created_child_flip_case(),
            Results = check_spec(Spec, Events),
            [_, #{state := {Engines, _}, counts := Counts}] = maps:get(optimized, Results),
            ?assertEqual(1, count(bulk_load, Counts)),
            ?assertNot(lists:member(user_id(4), engine_user_ids(?CHAN_BASE + 500, Engines)))
        end)
    end}.

created_child_flip_case() ->
    Spec0 = (owner_spec())#{
        data => owner_data(
            renumber([
                channel(?CHAN_BASE + 10, 0, null, []),
                channel(?CHAN_BASE + 500, 4, null, [])
            ])
        ),
        member_subscriptions => []
    },
    Child = channel(new_id(), 0, ?CHAN_BASE + 500, [overwrite(?GUILD_ID, 0, view(), 0)]),
    Spec = Spec0#{
        hooks => [
            {ensure_list, ?CHAN_BASE + 500},
            {channel_create, Child},
            {member_update, user_id(4)}
        ]
    },
    Moved = Child#{<<"parent_id">> => null},
    {Spec, [
        {channel_update, Moved},
        bulk_event(renumber(lists:reverse(layout(Spec0) ++ [Moved])))
    ]}.

subscribed_hidden_voice_list_is_rebuilt_after_access_is_granted_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            {Spec, Events} = subscribed_hidden_voice_case(),
            Results = check_spec(Spec, Events),
            [#{state := {Engines, _}}] = maps:get(optimized, Results),
            ?assert(lists:member(user_id(5), engine_user_ids(?CHAN_BASE + 202, Engines)))
        end)
    end}.

subscribed_hidden_voice_case() ->
    Spec0 = hidden_voice_spec(),
    Spec = Spec0#{
        list_subscriptions => [{session_id(1), integer_to_binary(?CHAN_BASE + 202)}]
    },
    {Spec, [position_move(Spec)]}.

bulk_move_resyncs_voice_permissions_in_every_voice_channel_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            Spec0 = ticker_spec(),
            Voice = maps:merge(
                gen_voice_states([user_id(I) || I <- lists:seq(1, 10)], [ticker_category() + 2]),
                maps:from_list([
                    {<<"t", (integer_to_binary(I))/binary>>, #{
                        <<"user_id">> => integer_to_binary(user_id(20 + I)),
                        <<"channel_id">> => integer_to_binary(Id),
                        <<"guild_id">> => integer_to_binary(?GUILD_ID),
                        <<"connection_id">> => <<"t", (integer_to_binary(I))/binary>>,
                        <<"session_id">> => <<"ts", (integer_to_binary(I))/binary>>,
                        <<"deaf">> => false
                    }}
                 || {I, Id} <- lists:zip(lists:seq(1, 3), ticker_ids())
                ])
            ),
            Spec = Spec0#{voice_states => Voice},
            Results = check_spec(Spec, [position_move(Spec)]),
            [#{log := Log}] = maps:get(optimized, Results),
            Synced = lists:usort([Conn || {voice_hook, _, _, _, Conn, _} <- Log]),
            ?assertEqual(lists:sort(maps:keys(Voice)), Synced)
        end)
    end}.

voice_user_without_view_is_granted_access_like_the_full_pass_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            Spec = hidden_voice_spec(),
            Results = check_spec(Spec, [position_move(Spec)]),
            [#{state := {_, State}, log := Log}] = maps:get(optimized, Results),
            ?assert(lists:any(fun(E) -> element(1, E) =:= voice_hook end, Log)),
            ?assert(
                guild_virtual_channel_access:has_virtual_access(
                    user_id(5), ?CHAN_BASE + 202, State
                )
            )
        end)
    end}.

members_sharing_a_role_set_with_the_owner_see_their_own_channels_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            ?assertEqual(
                [{session_pid, 3}, {session_pid, 4}],
                shared_roles_deletes(
                    shared_roles_case(user_allow(), [hidden_overwrite() | user_allow()])
                )
            )
        end)
    end}.

user_overwrite_added_by_the_event_is_not_shared_through_the_role_memo_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            ?assertEqual(
                [{session_pid, 3}, {session_pid, 4}],
                shared_roles_deletes(shared_roles_case([], [hidden_overwrite() | user_allow()]))
            )
        end)
    end}.

user_overwrite_removed_by_the_event_is_not_shared_through_the_role_memo_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            ?assertEqual(
                [{session_pid, 2}],
                shared_roles_deletes(
                    shared_roles_case([hidden_overwrite() | user_allow()], [hidden_overwrite()])
                )
            )
        end)
    end}.

shared_roles_deletes({Spec, Events}) ->
    Results = check_spec(Spec, Events),
    [#{log := Log}] = maps:get(optimized, Results),
    lists:sort([Target || {relay, dispatch, [Target, channel_delete, _, _]} <- Log]).

user_allow() ->
    [overwrite(user_id(2), 1, view(), 0)].

stale_detection_tracks_every_permission_input_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            Spec = (flip_spec())#{list_subscriptions => []},
            {State0, Env} = materialize(Spec),
            ListId = integer_to_binary(flip_channel()),
            State = guild_member_list_channel_engine:ensure(ListId, State0),
            try
                ?assertNot(guild_member_list_engine_inputs:is_stale(ListId, State)),
                ?assertEqual(
                    [],
                    [
                        Name
                     || {Name, Change} <- input_changes(),
                        not guild_member_list_engine_inputs:is_stale(ListId, Change(State))
                    ]
                ),
                ?assert(
                    guild_member_list_engine_inputs:is_stale(
                        ListId,
                        guild_member_list_engine_inputs:mark_stale(flip_channel(), State)
                    )
                ),
                ?assertNot(
                    guild_member_list_engine_inputs:is_stale(
                        ListId, guild_member_list_engine_inputs:latch_stale(State)
                    )
                ),
                [
                    ?assert(
                        guild_member_list_engine_inputs:is_stale(
                            ListId,
                            (guild_member_list_engine_inputs:latch_stale(Change(State)))#{
                                data => maps:get(data, State),
                                virtual_channel_access => maps:get(
                                    virtual_channel_access, State
                                )
                            }
                        )
                    )
                 || {_Name, Change} <- input_changes()
                ],
                ?assertNot(
                    guild_member_list_engine_inputs:is_stale(
                        ListId,
                        with_channels(
                            fun(C) -> C#{<<"name">> => <<"x">>, <<"position">> => 99} end, State
                        )
                    )
                )
            after
                teardown(State, Env)
            end
        end)
    end}.

input_changes() ->
    Flip = flip_channel(),
    [
        {owner,
            with_data(fun(D) ->
                G = maps:get(<<"guild">>, D),
                D#{<<"guild">> => G#{<<"owner_id">> => integer_to_binary(user_id(2))}}
            end)},
        {roles,
            with_data(fun(D) ->
                [E | Rest] = guild_data_index:role_list(D),
                guild_data_index:put_roles([E#{<<"permissions">> => <<"0">>} | Rest], D)
            end)},
        {role_order,
            with_data(fun(D) -> D#{<<"roles">> => lists:reverse(maps:get(<<"roles">>, D))} end)},
        {role_index, with_data(fun(D) -> D#{<<"role_index">> => #{}} end)},
        {indexed_overwrites_only,
            with_data(fun(D) ->
                Index = maps:get(<<"channel_index">>, D),
                Channel = maps:get(Flip, Index),
                D#{
                    <<"channel_index">> => Index#{
                        Flip => Channel#{<<"permission_overwrites">> => [hidden_overwrite()]}
                    }
                }
            end)},
        {role_perms_cache, with_data(fun(D) -> D#{role_perms_cache => #{}} end)},
        {own_overwrites,
            with_channels(fun(C) ->
                case channel_int_id(C) of
                    Flip -> C#{<<"permission_overwrites">> => [hidden_overwrite()]};
                    _ -> C
                end
            end)},
        {own_type,
            with_channels(fun(C) ->
                case channel_int_id(C) of
                    Flip -> C#{<<"type">> => 0};
                    _ -> C
                end
            end)},
        {overwrite_cache,
            with_data(fun(D) ->
                D#{
                    overwrite_perms_cache => maps:remove(
                        Flip, maps:get(overwrite_perms_cache, D)
                    )
                }
            end)},
        {virtual_access, fun(S) ->
            S#{virtual_channel_access => #{user_id(9) => sets:from_list([Flip])}}
        end}
    ].

category_children_are_permission_inputs_test_() ->
    {timeout, 120, fun() ->
        with_harness(fun() ->
            Spec = (gap_spec(?CHAN_BASE + 501))#{list_subscriptions => []},
            {State0, Env} = materialize(Spec),
            ListId = integer_to_binary(?CHAN_BASE + 500),
            State = guild_member_list_channel_engine:ensure(ListId, State0),
            try
                ?assertNot(guild_member_list_engine_inputs:is_stale(ListId, State)),
                Reparented = with_channels(
                    fun(C) ->
                        case channel_int_id(C) of
                            501 + ?CHAN_BASE -> C#{<<"parent_id">> => null};
                            _ -> C
                        end
                    end,
                    State
                ),
                ?assert(guild_member_list_engine_inputs:is_stale(ListId, Reparented)),
                ChildOverwrites = with_channels(
                    fun(C) ->
                        case channel_int_id(C) of
                            501 + ?CHAN_BASE ->
                                C#{<<"permission_overwrites">> => [hidden_overwrite()]};
                            _ ->
                                C
                        end
                    end,
                    State
                ),
                ?assert(guild_member_list_engine_inputs:is_stale(ListId, ChildOverwrites))
            after
                teardown(State, Env)
            end
        end)
    end}.

with_data(Fun) ->
    fun(State) -> with_data(Fun, State) end.

with_data(Fun, State) ->
    State#{data => Fun(maps:get(data, State))}.

with_channels(Fun) ->
    fun(State) -> with_channels(Fun, State) end.

with_channels(Fun, State) ->
    with_data(
        fun(D) ->
            guild_data_index:put_channels([Fun(C) || C <- guild_data_index:channel_list(D)], D)
        end,
        State
    ).

engine_user_ids(ChannelId, Engines) ->
    {_, UserIds, _, _} = maps:get(integer_to_binary(ChannelId), Engines),
    UserIds.

payload_channel_ids(#{<<"channels">> := Channels}) ->
    [channel_int_id(C) || C <- Channels];
payload_channel_ids({pre_encoded, Bin}) ->
    payload_channel_ids(json:decode(Bin));
payload_channel_ids(_) ->
    [].

new_id() -> ?CHAN_BASE + 950.

cat_id() -> ?CHAN_BASE + 1.

new_public_channel() ->
    channel(new_id(), 0, null, [overwrite(?GUILD_ID, 0, view(), 0)]).

create_spec() ->
    (owner_spec())#{
        data => owner_data(
            renumber([
                channel(cat_id(), 4, null, [overwrite(?GUILD_ID, 0, view(), 0)]),
                channel(?CHAN_BASE + 10, 0, null, []),
                channel(?CHAN_BASE + 30, 0, null, [overwrite(user_id(4), 1, view(), 0)]),
                channel(?CHAN_BASE + 40, 0, null, [overwrite(?GUILD_ID, 0, view(), 0)])
            ])
        ),
        sessions => [
            #{label => 1, user_id => user_id(1), pending => false},
            #{label => 2, user_id => user_id(2), pending => false},
            #{label => 3, user_id => user_id(4), pending => false}
        ]
    }.

owner_spec() ->
    #{
        data => owner_data(
            renumber([
                channel(?CHAN_BASE + 10, 0, null, []),
                channel(?CHAN_BASE + 30, 0, null, [overwrite(user_id(4), 1, view(), 0)])
            ])
        ),
        sessions => [
            #{label => 1, user_id => user_id(1), pending => false},
            #{label => 2, user_id => user_id(2), pending => false}
        ],
        virtual_access => #{},
        voice_states => #{},
        list_subscriptions => [],
        member_subscriptions => [{session_id(1), user_id(4)}],
        dm_partners => []
    }.

owner_data(Channels) ->
    #{
        <<"guild">> => guild(user_id(1)),
        <<"roles">> => [
            #{
                <<"id">> => integer_to_binary(?GUILD_ID),
                <<"name">> => <<"@everyone">>,
                <<"permissions">> => integer_to_binary(
                    members_view() bor connect() bor speak()
                ),
                <<"position">> => 0,
                <<"hoist">> => false
            },
            (plain_role(1, role_id(1)))#{<<"permissions">> => integer_to_binary(view())}
        ],
        <<"members">> => [
            member(user_id(1), []),
            member(user_id(2), [role_id(1)]),
            member(user_id(3), [role_id(1)]),
            member(user_id(4), [])
        ],
        <<"channels">> => Channels,
        <<"emojis">> => [],
        <<"stickers">> => []
    }.

guild(Owner) ->
    #{
        <<"id">> => integer_to_binary(?GUILD_ID),
        <<"name">> => <<"equivalence">>,
        <<"owner_id">> => integer_to_binary(Owner),
        <<"features">> => [],
        <<"disabled_operations">> => 0
    }.

flip_channel() -> ?CHAN_BASE + 20.

owner_flip_hooks() ->
    [
        {owner, user_id(1)},
        {role_permissions, 1, view()},
        {owner, user_id(3)},
        {member_update, user_id(1)},
        {owner, user_id(1)}
    ].

flip_spec() ->
    Hidden = flip_channel(),
    (owner_spec())#{
        data => owner_data(
            renumber([
                channel(?CHAN_BASE + 10, 0, null, []),
                channel(Hidden, 2, null, [])
            ])
        ),
        sessions => [#{label => 1, user_id => user_id(2), pending => false}],
        list_subscriptions => [{session_id(1), integer_to_binary(Hidden)}],
        member_subscriptions => [],
        hooks => [
            {add_virtual_channel_access, user_id(4), Hidden},
            {member_update, user_id(4)},
            {remove_virtual_channel_access, user_id(4), Hidden}
        ]
    }.

shared_roles_case(Before, After) ->
    Spec = single_session_spec(
        renumber([
            channel(?CHAN_BASE + 600, 0, null, []),
            channel(?CHAN_BASE + 601, 0, null, Before)
        ])
    ),
    Data = maps:get(data, Spec),
    {
        Spec#{
            data => Data#{
                <<"guild">> => guild(user_id(1)),
                <<"members">> => [member(user_id(I), []) || I <- lists:seq(1, 4)]
            },
            sessions => [
                #{label => L, user_id => user_id(L), pending => false}
             || L <- [1, 2, 3, 4]
            ]
        },
        [{channel_update, channel(?CHAN_BASE + 601, 0, null, After)}]
    }.

check_seed(Seed) ->
    Spec = gen_spec(Seed),
    Events = gen_events(Seed, spec_layout_after_hooks(Spec)),
    try
        check_spec(Spec, Events)
    catch
        Class:Reason:Stack ->
            io:format(
                user, "~nchannel move equivalence failed: seed=~p hooks=~p events=~p~n", [
                    Seed, maps:get(hooks, Spec, []), [summarize_event(E) || E <- Events]
                ]
            ),
            erlang:raise(Class, Reason, Stack)
    end.

spec_layout_after_hooks(Spec) ->
    layout(Spec) ++ [C || {channel_create, C} <- maps:get(hooks, Spec, [])].

check_spec(Spec, Events) ->
    check_spec(Spec, Events, optimized).

check_spec(Spec, Events, Mode) ->
    {RefHooks, Reference} = run(Spec, Events, reference),
    {CandHooks, Candidate} = run(Spec, Events, Mode),
    assert_same(hooks, log, RefHooks, CandHooks),
    compare_runs(Reference, Candidate, Events, 1),
    #{reference => Reference, optimized => Candidate}.

diverges_for_some_case(Broken) ->
    Cases =
        hand_cases() ++
            [
                begin
                    Spec = gen_spec(Seed),
                    {Spec, gen_events(Seed, spec_layout_after_hooks(Spec))}
                end
             || Seed <- lists:seq(1, ?SENSITIVITY_SEEDS)
            ],
    lists:any(
        fun({Spec, Events}) ->
            try check_spec(Spec, Events, {broken, Broken}) of
                _ -> false
            catch
                error:_ -> true
            end
        end,
        Cases
    ).

broken_modes() ->
    [never_stale, vca_marks_off, latch_off, reload_marks_off].

hand_cases() ->
    Create = (create_spec())#{hooks => [{channel_create, new_public_channel()}]},
    CreateLayout = layout(Create) ++ [new_public_channel()],
    Owner = (owner_spec())#{hooks => [{owner, user_id(2)}]},
    OwnerFlip = (flip_spec())#{
        hooks => owner_flip_hooks()
    },
    RoleFlip = (flip_spec())#{
        list_subscriptions => [],
        hooks => [
            {ensure_list, flip_channel()},
            {role_permissions, 1, 0},
            {member_update, user_id(2)},
            {role_permissions, 1, view()}
        ]
    },
    Gap = (gap_spec(?CHAN_BASE + 500))#{
        list_subscriptions => [{session_id(1), integer_to_binary(?CHAN_BASE + 500)}]
    },
    GapMoved = (find(?CHAN_BASE + 501, layout(Gap)))#{
        <<"parent_id">> => integer_to_binary(?CHAN_BASE + 500)
    },
    Text = ticker_category() + 1,
    [
        {Create, [
            bulk_event(renumber(move_channels([new_id()], {parent, cat_id()}, CreateLayout)))
        ]},
        {Owner, [position_move(Owner)]},
        {flip_spec(), [position_move(flip_spec())]},
        {OwnerFlip, [position_move(OwnerFlip)]},
        {RoleFlip, [position_move(RoleFlip)]},
        {Gap, [
            {channel_update, GapMoved},
            bulk_event(renumber(lists:reverse(replace(GapMoved, layout(Gap)))))
        ]},
        {ticker_spec(), [
            {channel_update, (find(Text, layout(ticker_spec())))#{
                <<"permission_overwrites">> => [overwrite(role_id(1), 0, 0, view())]
            }}
        ]},
        {hidden_voice_spec(), [position_move(hidden_voice_spec())]},
        category_flip_case(),
        bulk_category_flip_case(),
        everyone_patch_flip_case(),
        reload_case(),
        created_child_flip_case(),
        subscribed_hidden_voice_case(),
        shared_roles_case(user_allow(), [hidden_overwrite() | user_allow()]),
        shared_roles_case([], [hidden_overwrite() | user_allow()]),
        shared_roles_case([hidden_overwrite() | user_allow()], [hidden_overwrite()])
    ].

compare_runs([], [], [], _N) ->
    ok;
compare_runs([F | Fs], [O | Os], [Event | Events], N) ->
    Context = {event, N, summarize_event(Event)},
    lists:foreach(
        fun(Key) -> assert_same(Context, Key, maps:get(Key, F), maps:get(Key, O)) end,
        [state, log, direct, perm_cache]
    ),
    compare_runs(Fs, Os, Events, N + 1).

assert_same(Context, Key, Full, Optimized) ->
    case first_difference(Full, Optimized, []) of
        none -> ok;
        {Path, A, B} -> erlang:error({Context, Key, Path, trim(A), trim(B)})
    end.

trim(Term) ->
    lists:flatten(io_lib:format("~P", [Term, 12])).

first_difference(Same, Same, _Path) ->
    none;
first_difference(A, B, Path) when is_map(A), is_map(B) ->
    Keys = lists:usort(maps:keys(A) ++ maps:keys(B)),
    first_of(
        fun(K) ->
            first_difference(maps:get(K, A, '$missing'), maps:get(K, B, '$missing'), [K | Path])
        end,
        Keys
    );
first_difference(A, B, Path) when is_tuple(A), is_tuple(B), tuple_size(A) =:= tuple_size(B) ->
    first_difference(tuple_to_list(A), tuple_to_list(B), Path);
first_difference(A, B, Path) when is_list(A), is_list(B), length(A) =:= length(B) ->
    first_of(
        fun({I, X, Y}) -> first_difference(X, Y, [I | Path]) end,
        lists:zip3(lists:seq(1, length(A)), A, B)
    );
first_difference(A, B, Path) ->
    {lists:reverse(Path), A, B}.

first_of(_Fun, []) ->
    none;
first_of(Fun, [H | T]) ->
    case Fun(H) of
        none -> first_of(Fun, T);
        Found -> Found
    end.

run(Spec, Events, Mode) ->
    put(?MODE, Mode),
    try
        {State00, Env} = materialize(Spec),
        {State0, HookLog} = apply_hooks(maps:get(hooks, Spec, []), State00, Env),
        {StateN, Results} = lists:foldl(
            fun({Kind, Data}, {S, Acc}) ->
                {S1, Result} = run_event(Kind, Data, S, Env),
                {S1, [Result | Acc]}
            end,
            {State0, []},
            Events
        ),
        teardown(StateN, Env),
        {HookLog, lists:reverse(Results)}
    after
        erase(?MODE)
    end.

apply_hooks(Hooks, State, Env) ->
    State1 = lists:foldl(fun apply_hook/2, State, Hooks),
    State2 = guild_member_list:flush_pending_member_list_syncs(State1),
    {Log, Direct, _Fetches} = take_log(Env),
    {State2, {normalize(Log, Env), normalize(Direct, Env)}}.

apply_hook({Action, UserId, ChannelId}, State) when
    Action =:= add_virtual_channel_access; Action =:= remove_virtual_channel_access
->
    {noreply, Next} = guild:handle_cast({Action, UserId, ChannelId}, State),
    Next;
apply_hook({channel_create, Channel}, State) ->
    guild:dispatch_event(channel_create, Channel, State);
apply_hook({owner, UserId}, State) ->
    guild:dispatch_event(guild_update, #{<<"owner_id">> => integer_to_binary(UserId)}, State);
apply_hook({member_update, UserId}, State) ->
    Member = guild_data_index:get_member(UserId, maps:get(data, State)),
    Nick =
        case maps:get(<<"nick">>, Member, null) of
            Previous when is_binary(Previous) -> <<Previous/binary, "x">>;
            _ -> <<"n">>
        end,
    guild:dispatch_event(
        guild_member_update, member_payload(Member#{<<"nick">> => Nick}), State
    );
apply_hook({role_permissions, Index, Perms}, State) ->
    Role = maps:get(role_id(Index), guild_data_index:role_index(maps:get(data, State))),
    guild:dispatch_event(
        guild_role_update,
        #{<<"role">> => Role#{<<"permissions">> => integer_to_binary(Perms)}},
        State
    );
apply_hook({patch_everyone, Bit}, State) ->
    {noreply, Next} = guild:handle_cast({patch_everyone_perms, Bit}, State),
    Next;
apply_hook({everyone_permissions, Perms}, State) ->
    Role = maps:get(?GUILD_ID, guild_data_index:role_index(maps:get(data, State))),
    guild:dispatch_event(
        guild_role_update,
        #{<<"role">> => Role#{<<"permissions">> => integer_to_binary(Perms)}},
        State
    );
apply_hook({reload, Data}, State) ->
    {reply, ok, Next} = guild:handle_call({reload, Data}, {self(), make_ref()}, State),
    Next;
apply_hook({ensure_list, ChannelId}, State) ->
    guild_member_list_channel_engine:ensure(integer_to_binary(ChannelId), State).

member_payload(Member) ->
    maps:with([<<"user">>, <<"roles">>, <<"nick">>, <<"joined_at">>], Member).

run_event(Kind, Data, State, Env) ->
    reset_counters(),
    State1 =
        case Kind of
            hook -> apply_hook(Data, State);
            _ -> guild:dispatch_event(Kind, Data, State)
        end,
    State2 = guild_member_list:flush_pending_member_list_syncs(State1),
    {Log, Direct, Fetches} = take_log(Env),
    Counts = read_counters(),
    Result = #{
        log => normalize(Log, Env),
        direct => normalize(Direct, Env),
        counts => Counts#{voice_fetch => Fetches},
        state => normalize_state(State2, Env),
        perm_cache => normalize(guild_permission_cache:get_snapshot(?GUILD_ID), Env)
    },
    {State2, Result}.

with_harness(Fun) ->
    Recorder = spawn(fun() -> recorder_loop(empty_log()) end),
    register(?RECORDER, Recorder),
    meck:new(gateway_dispatch_relay, [no_link, no_history]),
    Record = fun(Name, Args) ->
        ?RECORDER ! {rec, self(), {relay, Name, Args}},
        ok
    end,
    meck:expect(gateway_dispatch_relay, dispatch, fun(A, B, C) ->
        Record(dispatch, [A, B, C])
    end),
    meck:expect(gateway_dispatch_relay, dispatch, fun(A, B, C, D) ->
        Record(dispatch, [A, B, C, D])
    end),
    meck:expect(gateway_dispatch_relay, dispatch_many, fun(A, B, C) ->
        Record(dispatch_many, [A, B, C])
    end),
    meck:expect(gateway_dispatch_relay, dispatch_many, fun(A, B, C, D) ->
        Record(dispatch_many, [A, B, C, D])
    end),
    meck:expect(gateway_dispatch_relay, dispatch_direct, fun(A, B, C) ->
        Record(dispatch_direct, [A, B, C])
    end),
    meck:expect(gateway_dispatch_relay, dispatch_grouped, fun(A, B, C, D) ->
        Record(dispatch_grouped, [A, B, C, D])
    end),
    install_modes(),
    ok = start_counters(),
    try
        Fun()
    after
        stop_counters(),
        meck:unload([
            gateway_dispatch_relay, guild_visibility_memo, guild_member_list_engine_inputs
        ]),
        unregister(?RECORDER),
        exit(Recorder, kill)
    end.

install_modes() ->
    meck:new(guild_visibility_memo, [no_link, passthrough, no_history]),
    meck:expect(guild_visibility_memo, new, fun(Ids, Old, New) ->
        case get(?MODE) of
            reference -> disabled;
            _ -> meck:passthrough([Ids, Old, New])
        end
    end),
    meck:new(guild_member_list_engine_inputs, [no_link, passthrough, no_history]),
    meck:expect(guild_member_list_engine_inputs, is_stale, fun(ListId, State) ->
        case get(?MODE) of
            reference -> true;
            {broken, never_stale} -> false;
            _ -> meck:passthrough([ListId, State])
        end
    end),
    Unmarked = fun(Broken, Args) ->
        case get(?MODE) of
            {broken, Broken} -> lists:last(Args);
            _ -> meck:passthrough(Args)
        end
    end,
    meck:expect(guild_member_list_engine_inputs, mark_stale, fun(ChannelId, State) ->
        Unmarked(vca_marks_off, [ChannelId, State])
    end),
    meck:expect(guild_member_list_engine_inputs, latch_stale, fun(State) ->
        Unmarked(latch_off, [State])
    end),
    meck:expect(guild_member_list_engine_inputs, forget_all, fun(State) ->
        Unmarked(reload_marks_off, [State])
    end).

empty_log() ->
    #{main => [], direct => #{}, fetches => 0}.

recorder_loop(Log = #{main := Main, direct := Direct, fetches := Fetches}) ->
    receive
        {rec, _From, Entry} ->
            recorder_loop(Log#{main := [Entry | Main]});
        {direct, Label, Msg} ->
            recorder_loop(Log#{direct := Direct#{Label => [Msg | maps:get(Label, Direct, [])]}});
        voice_fetch ->
            recorder_loop(Log#{fetches := Fetches + 1});
        {take, From} ->
            From ! {log, Log},
            recorder_loop(empty_log())
    end.

take_log(#{collectors := Collectors}) ->
    lists:foreach(
        fun(Pid) ->
            Ref = make_ref(),
            Pid ! {barrier, self(), Ref},
            receive
                {barrier_done, Ref} -> ok
            after 5000 -> error(collector_timeout)
            end
        end,
        Collectors
    ),
    ?RECORDER ! {take, self()},
    receive
        {log, #{main := Main, direct := Direct, fetches := Fetches}} ->
            {lists:reverse(Main), maps:map(fun(_, V) -> lists:reverse(V) end, Direct), Fetches}
    after 5000 -> error(recorder_timeout)
    end.

collector_loop(Label) ->
    receive
        stop ->
            ok;
        {barrier, From, Ref} ->
            From ! {barrier_done, Ref},
            collector_loop(Label);
        Msg ->
            ?RECORDER ! {direct, Label, Msg},
            collector_loop(Label)
    end.

voice_server_loop(VoiceStates) ->
    receive
        {'$gen_call', From, {get_voice_states_map}} ->
            ?RECORDER ! voice_fetch,
            gen_server:reply(From, VoiceStates),
            voice_server_loop(VoiceStates);
        stop ->
            ok;
        _ ->
            voice_server_loop(VoiceStates)
    end.

-define(COUNTED, [
    {bulk_load, {guild_member_list_engine, bulk_load, 3}},
    {perm_put, {guild_permission_cache, put_state, 1}},
    {visibility,
        {guild_visibility_overwrites, compute_and_dispatch_visibility_changes_for_channels, 3}},
    {prune, {guild_maintenance, prune_invalid_member_subscriptions, 1}},
    {can_view, {guild_visibility_channels, channel_is_visible, 4}}
]).

start_counters() ->
    [erlang:trace_pattern(MFA, true, [local, call_count]) || {_, MFA} <- ?COUNTED],
    _ = erlang:trace(self(), true, [call]),
    ok.

stop_counters() ->
    _ = erlang:trace(self(), false, [call]),
    [erlang:trace_pattern(MFA, false, [local, call_count]) || {_, MFA} <- ?COUNTED],
    ok.

reset_counters() ->
    [erlang:trace_pattern(MFA, restart, [local, call_count]) || {_, MFA} <- ?COUNTED],
    ok.

read_counters() ->
    maps:from_list([
        {Name, count_or_zero(element(2, erlang:trace_info(MFA, call_count)))}
     || {Name, MFA} <- ?COUNTED
    ]).

count(Name, Counts) ->
    maps:get(Name, Counts).

count_or_zero(N) when is_integer(N) -> N;
count_or_zero(_) -> 0.

relay_event({relay, _Name, [_Target, Event | _]}) -> Event;
relay_event(Other) -> Other.

materialize(Spec) ->
    #{
        data := Data,
        sessions := SessionSpecs,
        virtual_access := VirtualAccess,
        voice_states := VoiceStates,
        list_subscriptions := ListSubs,
        member_subscriptions := MemberSubs,
        dm_partners := DmPartners
    } = Spec,
    Collectors = maps:from_list([
        {Label, spawn(fun() -> collector_loop(Label) end)}
     || #{label := Label} <- SessionSpecs
    ]),
    VoicePid = spawn(fun() -> voice_server_loop(VoiceStates) end),
    Base = guild_init:init_base_state(#{
        id => ?GUILD_ID,
        data => Data,
        sessions => #{},
        voice_states => VoiceStates,
        disable_permission_cache_updates => false,
        disable_guild_count_cache_updates => true
    }),
    S1 = guild_init:init_member_list(Base),
    S2 = S1#{
        virtual_channel_access => maps:map(
            fun(_, Chs) -> sets:from_list(Chs) end, VirtualAccess
        ),
        voice_server_pid => VoicePid,
        test_permission_sync_fun => fun(G, C, U, Conn, P) ->
            ?RECORDER ! {rec, self(), {voice_hook, G, C, U, Conn, P}},
            ok
        end
    },
    S3 = lists:foldl(
        fun(SessionSpec, Acc) -> add_session(SessionSpec, Collectors, Acc) end, S2, SessionSpecs
    ),
    S4 = lists:foldl(
        fun({Sid, ListId}, Acc) ->
            {Acc1, _, _} = guild_member_list:subscribe_ranges(Sid, ListId, [{0, 99}], Acc),
            Acc1
        end,
        S3,
        ListSubs
    ),
    S5 = S4#{
        member_subscriptions => lists:foldl(
            fun({Sid, UserId}, Acc) -> guild_subscriptions:subscribe(Sid, UserId, Acc) end,
            guild_subscriptions:init_state(),
            MemberSubs
        ),
        dm_partners => maps:from_list([
            {Sid, #{
                user_id => UserId,
                pid => maps:get(Label, Collectors),
                partners => maps:from_keys(Partners, true),
                eligible => #{}
            }}
         || {Sid, Label, UserId, Partners} <- DmPartners
        ])
    },
    S6 = guild_dm_partners:maybe_reevaluate(
        guild_role_update,
        #{},
        #{},
        guild_maintenance:maybe_prune_invalid_member_subscriptions(
            guild_role_update, guild_member_list:flush_pending_member_list_syncs(S5)
        )
    ),
    ok = guild_permission_cache:put_state(S6),
    Labels = maps:merge(
        maps:from_list([{Pid, {session_pid, Label}} || {Label, Pid} <- maps:to_list(Collectors)]),
        #{VoicePid => voice_server, self() => guild}
    ),
    Env = #{collectors => maps:values(Collectors), labels => Labels, voice_pid => VoicePid},
    _ = take_log(Env),
    {S6, Env}.

add_session(#{label := Label, user_id := UserId, pending := Pending}, Collectors, State) ->
    Sid = session_id(Label),
    Pid = maps:get(Label, Collectors),
    Viewable =
        case Pending of
            true ->
                #{};
            false ->
                guild_sessions:build_viewable_channel_map(
                    guild_visibility:get_user_viewable_channels(UserId, State)
                )
        end,
    Session = #{
        session_id => Sid,
        user_id => UserId,
        pid => Pid,
        mref => make_ref(),
        active_guilds => sets:new(),
        user_roles => [],
        bot => false,
        is_staff => false,
        pending_connect => Pending,
        viewable_channels => Viewable
    },
    ets:insert(maps:get(member_presence, State), {UserId, #{<<"status">> => <<"online">>}}),
    Sessions = maps:get(sessions, State, #{}),
    Counts = maps:get(user_session_counts, State, #{}),
    State#{
        sessions => Sessions#{Sid => Session},
        connected_user_ids => sets:add_element(UserId, maps:get(connected_user_ids, State)),
        user_session_counts => Counts#{UserId => maps:get(UserId, Counts, 0) + 1}
    }.

teardown(State, #{collectors := Collectors, voice_pid := VoicePid}) ->
    _ = guild_member_list_channel_engine:destroy_all(State),
    Data = maps:get(data, State),
    lists:foreach(
        fun(Tab) -> catch ets:delete(Tab) end,
        [
            maps:get(members_ets, Data),
            maps:get(member_presence, State),
            maps:get(member_list_subscriptions, State),
            maps:get(viewable_channels_cache, State)
        ]
    ),
    [Pid ! stop || Pid <- Collectors],
    VoicePid ! stop,
    guild_permission_cache:delete(?GUILD_ID),
    ok.

normalize_state(State, Env) ->
    Engines = maps:get(channel_member_list_engines, State, #{}),
    EngineSnapshots = maps:map(
        fun(ListId, Ref) ->
            {
                guild_member_list:member_list_snapshot(ListId, State),
                guild_member_list_engine:get_sorted_user_ids(Ref),
                guild_member_list_engine:get_counts(Ref),
                guild_member_list_engine:get_groups(Ref)
            }
        end,
        Engines
    ),
    Stripped = maps:without(
        [
            channel_member_list_engines,
            channel_member_list_engine_inputs,
            member_list_engine,
            test_permission_sync_fun
        ],
        State
    ),
    {EngineSnapshots, normalize(Stripped, Env)}.

normalize(Term, #{labels := Labels}) ->
    normalize_term(Term, Labels).

normalize_term(Pid, Labels) when is_pid(Pid) ->
    maps:get(Pid, Labels, pid);
normalize_term(Ref, Labels) when is_reference(Ref) ->
    case catch ets:tab2list(Ref) of
        Rows when is_list(Rows) -> {ets, lists:sort(normalize_term(Rows, Labels))};
        _ -> ref
    end;
normalize_term(Fun, _Labels) when is_function(Fun) ->
    'fun';
normalize_term({relay, dispatch_many, [Pids | Rest]}, Labels) when is_list(Pids) ->
    {relay, dispatch_many, [
        lists:sort(normalize_term(Pids, Labels)) | normalize_term(Rest, Labels)
    ]};
normalize_term(List, Labels) when is_list(List) ->
    [normalize_term(E, Labels) || E <- List];
normalize_term(Tuple, Labels) when is_tuple(Tuple) ->
    list_to_tuple(normalize_term(tuple_to_list(Tuple), Labels));
normalize_term(Map, Labels) when is_map(Map) ->
    maps:from_list([
        {normalize_term(K, Labels), normalize_term(V, Labels)}
     || {K, V} <- maps:to_list(Map)
    ]);
normalize_term(Other, _Labels) ->
    Other.

summarize_event({channel_update, Data}) ->
    {channel_update, maps:get(<<"id">>, Data)};
summarize_event({channel_update_bulk, #{<<"channels">> := Channels}}) ->
    {channel_update_bulk, length(Channels)};
summarize_event({hook, Hook}) ->
    {hook, Hook};
summarize_event({Kind, Data}) ->
    {Kind, Data}.

seed_count() ->
    env_int("CHMOVE_SEEDS", ?DEFAULT_SEEDS).

env_int(Name, Default) ->
    case os:getenv(Name) of
        false -> Default;
        "" -> Default;
        Value -> list_to_integer(Value)
    end.

session_id(Label) ->
    <<"s", (integer_to_binary(Label))/binary>>.

session_map(Sid, State) ->
    maps:get(viewable_channels, maps:get(Sid, maps:get(sessions, State))).

user_id(I) -> ?USER_BASE + I.

role_id(I) -> ?ROLE_BASE + I.

view() -> constants:view_channel_permission().
connect() -> constants:connect_permission().
speak() -> constants:speak_permission().
stream() -> constants:stream_permission().
members_view() -> constants:view_channel_members_permission().
admin() -> constants:administrator_permission().

hidden_overwrite() ->
    overwrite(?GUILD_ID, 0, 0, view()).

position_move(Spec) ->
    bulk_event(renumber(lists:reverse(spec_layout_after_hooks(Spec)))).

gap_spec(InitialParent) ->
    Hidden = ?CHAN_BASE + 500,
    Parent =
        case InitialParent of
            Hidden -> null;
            _ -> Hidden
        end,
    Layout = renumber([
        channel(Hidden, 4, null, [overwrite(?GUILD_ID, 0, 0, view())]),
        channel(?CHAN_BASE + 501, 0, Parent, [])
    ]),
    single_session_spec(Layout).

single_session_spec(Layout) ->
    Spec = ticker_spec(),
    Data = maps:get(data, Spec),
    Spec#{
        data => Data#{<<"channels">> => Layout},
        voice_states => #{},
        list_subscriptions => [],
        sessions => [
            #{label => 1, user_id => user_id(2), pending => false}
        ],
        member_subscriptions => []
    }.

replace(Channel, Layout) ->
    Id = channel_int_id(Channel),
    [
        case channel_int_id(C) of
            Id -> Channel;
            _ -> C
        end
     || C <- Layout
    ].

gen_spec(Seed) ->
    rand:seed(exsss, {Seed, 104729, 7919}),
    NumRoles = between(3, 8),
    NumMembers = between(20, 150),
    RoleIds = [role_id(R) || R <- lists:seq(1, NumRoles)],
    UserIds = [user_id(I) || I <- lists:seq(1, NumMembers)],
    Roles = [
        everyone_role() | [role(R, Id) || {R, Id} <- lists:zip(lists:seq(1, NumRoles), RoleIds)]
    ],
    Members = [member(U, pick_subset(RoleIds, 0.25)) || U <- UserIds],
    Layout = gen_layout(RoleIds, UserIds),
    Data = #{
        <<"guild">> => guild(user_id(1)),
        <<"roles">> => Roles,
        <<"members">> => Members,
        <<"channels">> => Layout,
        <<"emojis">> => [],
        <<"stickers">> => []
    },
    ChannelIds = [channel_int_id(C) || C <- Layout],
    VoiceChannelIds = [channel_int_id(C) || C <- Layout, maps:get(<<"type">>, C) =:= 2],
    SessionSpecs = [gen_session(L, UserIds) || L <- lists:seq(1, between(5, 40))],
    SessionUsers = [U || #{user_id := U} <- SessionSpecs],
    VirtualAccess = gen_virtual_access(SessionUsers, ChannelIds),
    #{
        data => Data,
        sessions => SessionSpecs,
        virtual_access => VirtualAccess,
        hooks => gen_hooks(VirtualAccess, SessionUsers, UserIds, NumRoles, Layout),
        voice_states => gen_voice_states(SessionUsers ++ UserIds, VoiceChannelIds),
        list_subscriptions => [
            {session_id(L), integer_to_binary(pick(ChannelIds))}
         || #{label := L} <- SessionSpecs, rand:uniform() < 0.5
        ],
        member_subscriptions => [
            {session_id(L), pick(UserIds)}
         || #{label := L} <- SessionSpecs, _ <- lists:seq(1, between(0, 4))
        ],
        dm_partners => [
            {session_id(L), L, U, [pick(UserIds) || _ <- lists:seq(1, between(1, 5))]}
         || #{label := L, user_id := U} <- SessionSpecs, rand:uniform() < 0.3
        ]
    }.

layout(#{data := #{<<"channels">> := Layout}}) ->
    Layout.

everyone_role() ->
    Base = view() bor members_view() bor connect() bor speak(),
    Perms =
        case rand:uniform() < 0.15 of
            true -> Base band bnot view();
            false -> Base
        end,
    #{
        <<"id">> => integer_to_binary(?GUILD_ID),
        <<"name">> => <<"@everyone">>,
        <<"permissions">> => integer_to_binary(Perms),
        <<"position">> => 0,
        <<"hoist">> => false
    }.

role(Position, Id) ->
    #{
        <<"id">> => integer_to_binary(Id),
        <<"name">> => <<"r", (integer_to_binary(Position))/binary>>,
        <<"permissions">> => integer_to_binary(random_role_permissions()),
        <<"position">> => Position,
        <<"hoist">> => rand:uniform() < 0.4,
        <<"color">> => 0
    }.

random_role_permissions() ->
    Bits = lists:foldl(
        fun(Bit, Acc) -> Acc bor Bit end,
        0,
        pick_subset([view(), connect(), speak(), stream(), members_view()], 0.4)
    ),
    case rand:uniform() < 0.05 of
        true -> Bits bor admin();
        false -> Bits
    end.

plain_role(Position, Id) ->
    (role(Position, Id))#{<<"permissions">> => <<"0">>, <<"hoist">> => false}.

member(UserId, Roles) ->
    #{
        <<"user">> => #{
            <<"id">> => integer_to_binary(UserId),
            <<"username">> => <<"u", (integer_to_binary(UserId))/binary>>,
            <<"global_name">> => null,
            <<"avatar">> => null,
            <<"discriminator">> => <<"0001">>,
            <<"bot">> => false
        },
        <<"roles">> => [integer_to_binary(R) || R <- Roles],
        <<"nick">> => null,
        <<"joined_at">> => <<"2026-08-01T00:00:00Z">>
    }.

gen_layout(RoleIds, UserIds) ->
    NumCats = between(2, 4),
    Top = [
        channel(?CHAN_BASE + 10 + I, pick([0, 0, 2]), null, gen_overwrites(RoleIds, UserIds))
     || I <- lists:seq(1, between(1, 3))
    ],
    Cats = lists:append([
        gen_category(C, RoleIds, UserIds)
     || C <- lists:seq(1, NumCats)
    ]),
    Odd =
        case rand:uniform() < 0.3 of
            true ->
                [TopParent | _] = Top,
                [
                    channel(
                        ?CHAN_BASE + 900,
                        0,
                        pick([channel_int_id(TopParent), ?DANGLING_PARENT]),
                        gen_overwrites(RoleIds, UserIds)
                    )
                ];
            false ->
                []
        end,
    renumber(Top ++ Cats ++ Odd).

gen_category(C, RoleIds, UserIds) ->
    CatId = ?CHAN_BASE + 100 * C,
    CatOverwrites =
        case rand:uniform() < 0.4 of
            true -> [overwrite(?GUILD_ID, 0, 0, view())];
            false -> gen_overwrites(RoleIds, UserIds)
        end,
    Children = [
        channel(CatId + I, pick([0, 2]), CatId, gen_child_overwrites(RoleIds, UserIds))
     || I <- lists:seq(1, between(0, 4))
    ],
    [channel(CatId, 4, null, CatOverwrites) | Children].

gen_child_overwrites(RoleIds, UserIds) ->
    case rand:uniform() of
        X when X < 0.2 ->
            [overwrite(pick(RoleIds), 0, view(), 0)];
        X when X < 0.3 ->
            [overwrite(pick(UserIds), 1, view(), 0)];
        _ ->
            gen_overwrites(RoleIds, UserIds)
    end.

gen_overwrites(RoleIds, UserIds) ->
    [gen_overwrite(RoleIds, UserIds) || _ <- lists:seq(1, between(0, 3))].

gen_overwrite(RoleIds, UserIds) ->
    {Id, Type} =
        case rand:uniform(3) of
            1 -> {?GUILD_ID, 0};
            2 -> {pick(RoleIds), 0};
            3 -> {pick(UserIds), 1}
        end,
    Bits = [view(), connect(), speak(), stream()],
    Allow = lists:foldl(fun(B, A) -> A bor B end, 0, pick_subset(Bits, 0.3)),
    Deny = lists:foldl(fun(B, A) -> A bor B end, 0, pick_subset(Bits, 0.3)) band bnot Allow,
    overwrite(Id, Type, Allow, Deny).

overwrite(Id, Type, Allow, Deny) ->
    #{
        <<"id">> => integer_to_binary(Id),
        <<"type">> => Type,
        <<"allow">> => integer_to_binary(Allow),
        <<"deny">> => integer_to_binary(Deny)
    }.

channel(Id, Type, Parent, Overwrites) ->
    Base = #{
        <<"id">> => integer_to_binary(Id),
        <<"guild_id">> => integer_to_binary(?GUILD_ID),
        <<"type">> => Type,
        <<"name">> => <<"ch-", (integer_to_binary(Id))/binary>>,
        <<"position">> => 0,
        <<"permission_overwrites">> => Overwrites
    },
    case Type of
        4 -> Base;
        _ -> Base#{<<"parent_id">> => parent_bin(Parent)}
    end.

parent_bin(null) -> null;
parent_bin(Id) -> integer_to_binary(Id).

channel_int_id(Channel) ->
    snowflake_id:parse_maybe(maps:get(<<"id">>, Channel)).

channel_parent(Channel) ->
    case maps:get(<<"parent_id">>, Channel, null) of
        null -> null;
        P -> binary_to_integer(P)
    end.

renumber(Layout) ->
    [C#{<<"position">> => P} || {P, C} <- lists:zip(lists:seq(1, length(Layout)), Layout)].

gen_session(Label, UserIds) ->
    #{
        label => Label,
        user_id => pick(UserIds),
        pending => rand:uniform() < 0.05
    }.

gen_virtual_access(UserIds, ChannelIds) ->
    maps:from_list([
        {pick(UserIds), [pick(ChannelIds) || _ <- lists:seq(1, between(1, 2))]}
     || _ <- lists:seq(1, between(0, 4))
    ]).

gen_hooks(VirtualAccess, SessionUsers, UserIds, NumRoles, Layout) ->
    ChannelIds = [channel_int_id(C) || C <- Layout],
    Held = [{U, C} || {U, Cs} <- maps:to_list(VirtualAccess), C <- Cs],
    lists:append([
        gen_hook(Held, SessionUsers, UserIds, NumRoles, Layout, ChannelIds, I)
     || I <- lists:seq(1, pick([0, 0, 1, 2, 3]))
    ]).

gen_hook(Held, SessionUsers, UserIds, NumRoles, Layout, ChannelIds, I) ->
    case rand:uniform(10) of
        1 when Held =/= [] ->
            {U, C} = pick(Held),
            [{remove_virtual_channel_access, U, C}];
        2 ->
            [{add_virtual_channel_access, pick(UserIds), pick(ChannelIds)}];
        3 ->
            U = pick(UserIds),
            C = pick(ChannelIds),
            [
                {add_virtual_channel_access, U, C},
                {member_update, U},
                {remove_virtual_channel_access, U, C}
            ];
        4 ->
            [{owner, pick(SessionUsers ++ UserIds)}];
        5 ->
            Other = pick(UserIds),
            [
                {owner, user_id(1)},
                {role_permissions, 1, random_role_permissions()},
                {owner, Other},
                {member_update, user_id(1)},
                {member_update, Other},
                {owner, user_id(1)}
            ];
        6 ->
            [{channel_create, created_channel(Layout, I)}];
        7 ->
            [{ensure_list, pick(ChannelIds)}];
        8 ->
            Index = between(1, NumRoles),
            [
                {role_permissions, Index, random_role_permissions()},
                {member_update, pick(UserIds)}
            ];
        9 ->
            [{member_update, pick(UserIds)}];
        _ ->
            [
                {ensure_list, pick(ChannelIds)},
                {add_virtual_channel_access, pick(SessionUsers ++ UserIds), pick(ChannelIds)}
            ]
    end.

created_channel(Layout, I) ->
    Cats = [channel_int_id(C) || C <- Layout, maps:get(<<"type">>, C) =:= 4],
    Parent =
        case Cats of
            [] -> null;
            _ -> pick([null | Cats])
        end,
    RoleIds = [role_id(R) || R <- lists:seq(1, 3)],
    UserIds = [user_id(U) || U <- lists:seq(1, 20)],
    channel(?CHAN_BASE + 950 + I, pick([0, 2]), Parent, gen_overwrites(RoleIds, UserIds)).

gen_voice_states(_UserIds, []) ->
    #{};
gen_voice_states(UserIds, VoiceChannelIds) ->
    maps:from_list([
        begin
            Conn = <<"c", (integer_to_binary(I))/binary>>,
            {Conn, #{
                <<"user_id">> => integer_to_binary(pick(UserIds)),
                <<"channel_id">> => integer_to_binary(pick(VoiceChannelIds)),
                <<"guild_id">> => integer_to_binary(?GUILD_ID),
                <<"connection_id">> => Conn,
                <<"session_id">> => <<"vs", (integer_to_binary(I))/binary>>,
                <<"self_mute">> => false,
                <<"self_deaf">> => false,
                <<"self_stream">> => rand:uniform() < 0.3,
                <<"self_video">> => false,
                <<"deaf">> => false,
                <<"mute">> => false
            }}
        end
     || I <- lists:seq(1, between(0, 8))
    ]).

gen_events(Seed, Layout0) ->
    rand:seed(exsss, {Seed, 15485863, 32452843}),
    {Events, _} = lists:foldl(
        fun(_, {Acc, Layout}) ->
            {Event, Layout1} = gen_event(Layout),
            Interleaved =
                case rand:uniform() < 0.25 of
                    true -> [Event, {hook, {member_update, user_id(between(1, 20))}}];
                    false -> [Event]
                end,
            {lists:reverse(Interleaved, Acc), Layout1}
        end,
        {[], Layout0},
        lists:seq(1, between(1, 4))
    ),
    lists:reverse(Events).

gen_event(Layout) ->
    NonCats = [C || C <- Layout, maps:get(<<"type">>, C) =/= 4],
    Kinds =
        [position_only, noop, overwrite, name, subset, category_overwrite] ++
            case NonCats of
                [] ->
                    [];
                _ ->
                    [
                        move,
                        move,
                        move,
                        single_move,
                        single_move,
                        subset_move,
                        type_change,
                        mixed
                    ]
            end,
    gen_event(pick(Kinds), Layout, NonCats).

gen_event(position_only, Layout, _NonCats) ->
    New = renumber(rotate(Layout)),
    {bulk_event(New), New};
gen_event(noop, Layout, _NonCats) ->
    {bulk_event(Layout), Layout};
gen_event(move, Layout, NonCats) ->
    Ids = lists:usort([channel_int_id(pick(NonCats)) || _ <- lists:seq(1, between(1, 3))]),
    New = move_channels(Ids, pick_target(Layout), Layout),
    {bulk_event(New), New};
gen_event(single_move, Layout, NonCats) ->
    Id = channel_int_id(pick(NonCats)),
    New = move_channels([Id], pick_target(Layout), Layout),
    {{channel_update, find(Id, New)}, New};
gen_event(subset_move, Layout, NonCats) ->
    Ids = lists:usort([channel_int_id(pick(NonCats)) || _ <- lists:seq(1, between(1, 3))]),
    New = move_channels(Ids, pick_target(Layout), Layout),
    {bulk_event([C || C <- New, lists:member(channel_int_id(C), Ids)]), New};
gen_event(overwrite, Layout, _NonCats) ->
    Id = channel_int_id(pick(Layout)),
    New = update_channel(
        Id, fun(C) -> C#{<<"permission_overwrites">> => random_overwrites(Layout)} end, Layout
    ),
    single_or_bulk(Id, New);
gen_event(category_overwrite, Layout, _NonCats) ->
    case [C || C <- Layout, maps:get(<<"type">>, C) =:= 4] of
        [] ->
            {bulk_event(Layout), Layout};
        Cats ->
            Id = channel_int_id(pick(Cats)),
            Toggle = fun(C) ->
                case maps:get(<<"permission_overwrites">>, C) of
                    [] ->
                        C#{<<"permission_overwrites">> => [overwrite(?GUILD_ID, 0, 0, view())]};
                    _ ->
                        C#{<<"permission_overwrites">> => []}
                end
            end,
            single_or_bulk(Id, update_channel(Id, Toggle, Layout))
    end;
gen_event(name, Layout, _NonCats) ->
    Id = channel_int_id(pick(Layout)),
    New = update_channel(Id, fun(C) -> C#{<<"name">> => <<"renamed">>} end, Layout),
    single_or_bulk(Id, New);
gen_event(type_change, Layout, NonCats) ->
    Id = channel_int_id(pick(NonCats)),
    Flip = fun(C) ->
        case maps:get(<<"type">>, C) of
            0 -> C#{<<"type">> => 2};
            _ -> C#{<<"type">> => 0}
        end
    end,
    single_or_bulk(Id, update_channel(Id, Flip, Layout));
gen_event(subset, Layout, _NonCats) ->
    Subset = pick_subset(Layout, 0.4),
    SubsetIds = [channel_int_id(C) || C <- Subset],
    New = renumber(rotate(Layout)),
    {
        bulk_event([C || C <- New, lists:member(channel_int_id(C), SubsetIds)]),
        merge_subset(Layout, New, SubsetIds)
    };
gen_event(mixed, Layout, NonCats) ->
    Id = channel_int_id(pick(NonCats)),
    Moved = move_channels([Id], pick_target(Layout), Layout),
    Other = channel_int_id(pick(Moved)),
    New = update_channel(
        Other, fun(C) -> C#{<<"permission_overwrites">> => random_overwrites(Moved)} end, Moved
    ),
    {bulk_event(New), New}.

single_or_bulk(Id, New) ->
    case rand:uniform() < 0.5 of
        true -> {{channel_update, find(Id, New)}, New};
        false -> {bulk_event(New), New}
    end.

bulk_event(Channels) ->
    {channel_update_bulk, #{<<"channels">> => Channels}}.

merge_subset(Old, New, SubsetIds) ->
    [
        case lists:member(channel_int_id(C), SubsetIds) of
            true -> find(channel_int_id(C), New);
            false -> C
        end
     || C <- Old
    ].

random_overwrites(Layout) ->
    RoleIds = [role_id(R) || R <- lists:seq(1, 3)],
    UserIds = [user_id(I) || I <- lists:seq(1, 20)],
    case rand:uniform() < 0.2 of
        true ->
            Existing = maps:get(<<"permission_overwrites">>, pick(Layout)),
            Existing;
        false ->
            gen_overwrites(RoleIds, UserIds)
    end.

pick_target(Layout) ->
    Cats = [channel_int_id(C) || C <- Layout, maps:get(<<"type">>, C) =:= 4],
    NonCats = [channel_int_id(C) || C <- Layout, maps:get(<<"type">>, C) =/= 4],
    case rand:uniform() of
        X when X < 0.65, Cats =/= [] -> {parent, pick(Cats)};
        X when X < 0.85 -> {parent, null};
        X when X < 0.93, NonCats =/= [] -> {parent, pick(NonCats)};
        _ -> {parent, ?DANGLING_PARENT}
    end.

move_channels(Ids, {parent, Target}, Layout) ->
    Moving = [
        C#{<<"parent_id">> => parent_bin(Target)}
     || C <- Layout, lists:member(channel_int_id(C), Ids)
    ],
    Rest = [C || C <- Layout, not lists:member(channel_int_id(C), Ids)],
    renumber(insert_after(Target, Moving, Rest)).

insert_after(Target, Moving, Rest) ->
    case lists:splitwith(fun(C) -> channel_int_id(C) =/= Target end, Rest) of
        {Before, [T | After]} ->
            {Kids, Tail} = lists:splitwith(fun(C) -> channel_parent(C) =:= Target end, After),
            Before ++ [T | Kids] ++ Moving ++ Tail;
        {_, []} ->
            Moving ++ Rest
    end.

update_channel(Id, Fun, Layout) ->
    [
        case channel_int_id(C) of
            Id -> Fun(C);
            _ -> C
        end
     || C <- Layout
    ].

find(Id, Layout) ->
    hd([C || C <- Layout, channel_int_id(C) =:= Id]).

rotate([]) -> [];
rotate([H | T]) -> T ++ [H].

between(Lo, Hi) ->
    Lo + rand:uniform(Hi - Lo + 1) - 1.

pick(List) ->
    lists:nth(rand:uniform(length(List)), List).

pick_subset(List, P) ->
    [E || E <- List, rand:uniform() < P].

ticker_category() ->
    ?CHAN_BASE + 100.

ticker_ids() ->
    [?CHAN_BASE + 60 + I || I <- lists:seq(1, 3)].

ticker_spec() ->
    rand:seed(exsss, {1, 2, 3}),
    UserIds = [user_id(I) || I <- lists:seq(1, 60)],
    RoleIds = [role_id(1), role_id(2)],
    Tickers = [
        channel(Id, 2, null, [overwrite(?GUILD_ID, 0, 0, connect())])
     || Id <- ticker_ids()
    ],
    Public = [
        channel(ticker_category(), 4, null, []),
        channel(ticker_category() + 1, 0, ticker_category(), []),
        channel(ticker_category() + 2, 2, ticker_category(), [])
    ],
    Private = [
        channel(?CHAN_BASE + 200, 4, null, [overwrite(?GUILD_ID, 0, 0, view())]),
        channel(?CHAN_BASE + 201, 0, ?CHAN_BASE + 200, [
            overwrite(?GUILD_ID, 0, 0, view()), overwrite(role_id(1), 0, view(), 0)
        ])
    ],
    Layout = renumber(Tickers ++ Public ++ Private),
    Sessions = [
        #{label => L, user_id => user_id(L), pending => false}
     || L <- lists:seq(1, 20)
    ],
    #{
        data => #{
            <<"guild">> => guild(user_id(1)),
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(?GUILD_ID),
                    <<"permissions">> => integer_to_binary(
                        view() bor members_view() bor connect() bor speak()
                    ),
                    <<"position">> => 0
                }
                | [role(P, Id) || {P, Id} <- lists:zip([1, 2], RoleIds)]
            ],
            <<"members">> => [member(U, pick_subset(RoleIds, 0.3)) || U <- UserIds],
            <<"channels">> => Layout,
            <<"emojis">> => [],
            <<"stickers">> => []
        },
        sessions => Sessions,
        virtual_access => #{},
        voice_states => gen_voice_states(UserIds, [ticker_category() + 2]),
        list_subscriptions => [
            {
                session_id(L),
                integer_to_binary(pick([ticker_category() + 1, ticker_category() + 2]))
            }
         || L <- lists:seq(1, 20)
        ],
        member_subscriptions => [{session_id(L), pick(UserIds)} || L <- lists:seq(1, 20)],
        dm_partners => []
    }.

hidden_voice_spec() ->
    Spec = ticker_spec(),
    Hidden = ?CHAN_BASE + 202,
    Data = maps:get(data, Spec),
    Layout = renumber(
        maps:get(<<"channels">>, Data) ++
            [channel(Hidden, 2, ?CHAN_BASE + 200, [overwrite(?GUILD_ID, 0, 0, view())])]
    ),
    Voice = #{
        <<"hv">> => #{
            <<"user_id">> => integer_to_binary(user_id(5)),
            <<"channel_id">> => integer_to_binary(Hidden),
            <<"guild_id">> => integer_to_binary(?GUILD_ID),
            <<"connection_id">> => <<"hv">>,
            <<"session_id">> => <<"hvs">>,
            <<"self_stream">> => true,
            <<"deaf">> => false
        }
    },
    Spec#{data => Data#{<<"channels">> => Layout}, voice_states => Voice}.
