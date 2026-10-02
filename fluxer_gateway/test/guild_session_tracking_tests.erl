%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_session_tracking_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(GUILD_ID, 910100).
-define(CHANNEL_ID, 910500).
-define(USER_A, 810001).
-define(USER_B, 810002).
-define(USER_C, 810003).

handoff_reconnect_counts_each_session_once_test_() ->
    {timeout, 60, fun handoff_reconnect_counts_each_session_once/0}.

handoff_reconnect_overflow_keeps_tracking_test_() ->
    {timeout, 60, fun handoff_reconnect_overflow_keeps_tracking/0}.

reconnect_of_connected_session_counts_once_test_() ->
    {timeout, 60, fun reconnect_of_connected_session_counts_once/0}.

not_member_reconnect_releases_tracking_test_() ->
    {timeout, 60, fun not_member_reconnect_releases_tracking/0}.

reconnecting_session_down_releases_tracking_test_() ->
    {timeout, 60, fun reconnecting_session_down_releases_tracking/0}.

fresh_pending_down_keeps_reconnecting_owner_tracked_test_() ->
    {timeout, 60, fun fresh_pending_down_keeps_reconnecting_owner_tracked/0}.

unavailable_removal_of_fresh_pending_keeps_owner_tracked_test_() ->
    {timeout, 60, fun unavailable_removal_of_fresh_pending_keeps_owner_tracked/0}.

fresh_pending_sibling_keeps_mutual_members_test_() ->
    {timeout, 60, fun fresh_pending_sibling_keeps_mutual_members/0}.

handoff_reconnect_counts_each_session_once() ->
    with_sessions(
        [{<<"a1">>, ?USER_A}, {<<"a2">>, ?USER_A}, {<<"b1">>, ?USER_B}],
        fun(Sessions) ->
            with_guild(base_state(Sessions), fun(Source) ->
                await_tracking(#{?USER_A => 2, ?USER_B => 1}, Source),
                {ok, Exported} = gen_server:call(Source, export_handoff_state, 10000),
                with_guild(transferred_state(Exported), fun(Target) ->
                    await_tracking(#{?USER_A => 2, ?USER_B => 1}, Target),
                    reconnect_all(Target, Sessions, 0),
                    await_tracking(#{?USER_A => 2, ?USER_B => 1}, Target),
                    kill_session(<<"b1">>, Sessions),
                    await_tracking(#{?USER_A => 2}, Target),
                    kill_session(<<"a1">>, Sessions),
                    await_tracking(#{?USER_A => 1}, Target),
                    kill_session(<<"a2">>, Sessions),
                    await_tracking(#{}, Target)
                end)
            end)
        end
    ).

handoff_reconnect_overflow_keeps_tracking() ->
    with_sessions(
        [{<<"a1">>, ?USER_A}, {<<"b1">>, ?USER_B}, {<<"c1">>, ?USER_C}],
        fun(Sessions) ->
            with_guild(base_state(Sessions), fun(Source) ->
                {ok, Exported} = gen_server:call(Source, export_handoff_state, 10000),
                with_guild(transferred_state(Exported), fun(Target) ->
                    Expected = #{?USER_A => 1, ?USER_B => 1, ?USER_C => 1},
                    await_tracking(Expected, Target),
                    hold_connect_workers(Target, 1),
                    [
                        send_connect(Target, Sid, Sessions, 0)
                     || Sid <- [<<"a1">>, <<"b1">>, <<"c1">>]
                    ],
                    ?assertEqual({error, overloaded}, await_result(<<"a1">>, 0)),
                    ?assertEqual({error, overloaded}, await_result(<<"b1">>, 0)),
                    State = get_state(Target),
                    Dropped = maps:get(<<"a1">>, maps:get(sessions, State)),
                    ?assertEqual(true, maps:get(pending_connect, Dropped)),
                    ?assertNot(maps:is_key(<<"a1">>, maps:get(session_connect_pending, State))),
                    assert_tracking(Expected, State),
                    release_connect_workers(Target),
                    ?assertMatch({ok, _, _}, await_result(<<"c1">>, 0)),
                    send_connect(Target, <<"a1">>, Sessions, 1),
                    send_connect(Target, <<"b1">>, Sessions, 1),
                    ?assertMatch({ok, _, _}, await_result(<<"a1">>, 1)),
                    ?assertMatch({ok, _, _}, await_result(<<"b1">>, 1)),
                    await_tracking(Expected, Target),
                    kill_session(<<"a1">>, Sessions),
                    await_tracking(#{?USER_B => 1, ?USER_C => 1}, Target)
                end)
            end)
        end
    ).

reconnect_of_connected_session_counts_once() ->
    with_sessions(
        [{<<"a1">>, ?USER_A}, {<<"b1">>, ?USER_B}],
        fun(Sessions) ->
            with_guild(base_state(Sessions), fun(Guild) ->
                reconnect_all(Guild, Sessions, 1),
                reconnect_all(Guild, Sessions, 2),
                State = get_state(Guild),
                assert_tracking(#{?USER_A => 1, ?USER_B => 1}, State),
                [
                    ?assertEqual(false, maps:get(pending_connect, Entry))
                 || Entry <- maps:values(maps:get(sessions, State))
                ],
                kill_session(<<"b1">>, Sessions),
                await_tracking(#{?USER_A => 1}, Guild)
            end)
        end
    ).

not_member_reconnect_releases_tracking() ->
    with_sessions(
        [{<<"a1">>, ?USER_A}, {<<"b1">>, ?USER_B}],
        fun(Sessions) ->
            with_guild(base_state(Sessions), fun(Guild) ->
                sys:replace_state(Guild, fun(S) ->
                    S#{data => guild_data_index:remove_member(?USER_B, maps:get(data, S))}
                end),
                send_connect(Guild, <<"b1">>, Sessions, 1),
                ?assertEqual({error, not_member}, await_result(<<"b1">>, 1)),
                State = get_state(Guild),
                ?assertNot(maps:is_key(<<"b1">>, maps:get(sessions, State))),
                assert_tracking(#{?USER_A => 1}, State)
            end)
        end
    ).

reconnecting_session_down_releases_tracking() ->
    with_sessions(
        [{<<"a1">>, ?USER_A}, {<<"b1">>, ?USER_B}],
        fun(Sessions) ->
            with_guild(base_state(Sessions), fun(Guild) ->
                hold_connect_workers(Guild, 16),
                send_connect(Guild, <<"b1">>, Sessions, 1),
                Pending = maps:get(<<"b1">>, maps:get(sessions, get_state(Guild))),
                ?assertEqual(true, maps:get(pending_connect, Pending)),
                await_tracking(#{?USER_A => 1, ?USER_B => 1}, Guild),
                kill_session(<<"b1">>, Sessions),
                await_tracking(#{?USER_A => 1}, Guild)
            end)
        end
    ).

fresh_pending_down_keeps_reconnecting_owner_tracked() ->
    with_sessions(
        [{<<"a1">>, ?USER_A}, {<<"a2">>, ?USER_A}, {<<"c1">>, ?USER_C}],
        fun(Sessions) ->
            with_guild(base_state(without(<<"a1">>, Sessions)), fun(Guild) ->
                hold_connect_workers(Guild, 16),
                send_connect(Guild, <<"a2">>, Sessions, 1),
                send_connect(Guild, <<"a1">>, Sessions, 1),
                kill_session(<<"a1">>, Sessions),
                await_tracking(#{?USER_A => 1, ?USER_C => 1}, Guild),
                release_connect_workers(Guild),
                ?assertMatch({ok, _, _}, await_result(<<"a2">>, 1)),
                await_tracking(#{?USER_A => 1, ?USER_C => 1}, Guild),
                kill_session(<<"a2">>, Sessions),
                await_tracking(#{?USER_C => 1}, Guild)
            end)
        end
    ).

unavailable_removal_of_fresh_pending_keeps_owner_tracked() ->
    with_sessions(
        [{<<"a1">>, ?USER_A}, {<<"a2">>, ?USER_A}],
        fun(Sessions) ->
            with_guild(base_state(without(<<"a1">>, Sessions)), fun(Guild) ->
                hold_connect_workers(Guild, 16),
                send_connect(Guild, <<"a1">>, Sessions, 1),
                sys:replace_state(Guild, fun(S) ->
                    guild_sessions:remove_session(<<"a1">>, S)
                end),
                State = get_state(Guild),
                ?assertNot(maps:is_key(<<"a1">>, maps:get(sessions, State))),
                assert_tracking(#{?USER_A => 1}, State)
            end)
        end
    ).

fresh_pending_sibling_keeps_mutual_members() ->
    with_sessions(
        [{<<"a1">>, ?USER_A}, {<<"a2">>, ?USER_A}, {<<"c1">>, ?USER_C}],
        fun(Sessions) ->
            with_guild(base_state(without(<<"a1">>, Sessions)), fun(Guild) ->
                send_connect(Guild, <<"a2">>, Sessions, 1),
                ?assertMatch({ok, _, _}, await_result(<<"a2">>, 1)),
                hold_connect_workers(Guild, 16),
                send_connect(Guild, <<"a1">>, Sessions, 1),
                State = get_state(Guild),
                SessionMap = maps:get(<<"a2">>, maps:get(sessions, State)),
                ?assertEqual(
                    #{?CHANNEL_ID => true}, maps:get(viewable_channels, SessionMap)
                ),
                ?assertEqual(
                    true,
                    maps:get(pending_connect, maps:get(<<"a1">>, maps:get(sessions, State)))
                ),
                ?assertEqual(
                    #{?CHANNEL_ID => true},
                    guild_visibility_channels:get_cached_viewable_channel_map(?USER_A, State)
                ),
                ?assertEqual(
                    [?USER_C],
                    guild_subscription_mutual_channels:filter_member_ids(
                        ?USER_A, [?USER_C], State
                    )
                )
            end)
        end
    ).

await_tracking(Expected, Guild) ->
    await_tracking(Expected, Guild, 100).

await_tracking(Expected, Guild, 0) ->
    assert_tracking(Expected, get_state(Guild));
await_tracking(Expected, Guild, Tries) ->
    case tracking(get_state(Guild)) =:= expected_tracking(Expected) of
        true ->
            ok;
        false ->
            timer:sleep(20),
            await_tracking(Expected, Guild, Tries - 1)
    end.

assert_tracking(Expected, State) ->
    ?assertEqual(expected_tracking(Expected), tracking(State)).

expected_tracking(Expected) ->
    {Expected, lists:sort(maps:keys(Expected)), Expected}.

tracking(State) ->
    {
        maps:get(user_session_counts, State),
        lists:sort(sets:to_list(maps:get(connected_user_ids, State))),
        maps:get(presence_subscriptions, State)
    }.

with_sessions(Specs, Fun) ->
    Parent = self(),
    Sessions = [
        {Sid, UserId, spawn(fun() -> session_loop(Parent, Sid) end)}
     || {Sid, UserId} <- Specs
    ],
    try
        Fun(Sessions)
    after
        [exit(Pid, kill) || {_, _, Pid} <- Sessions],
        flush()
    end.

session_loop(Parent, Sid) ->
    receive
        {guild_connect_result, _GuildId, Attempt, Reply} ->
            Parent ! {connect_result, Sid, Attempt, Reply},
            session_loop(Parent, Sid);
        _ ->
            session_loop(Parent, Sid)
    end.

without(Sid, Sessions) ->
    lists:keydelete(Sid, 1, Sessions).

with_guild(State, Fun) ->
    {ok, Pid} = gen_server:start(guild, State, []),
    try
        Fun(Pid)
    after
        catch gen_server:call(Pid, {terminate}, 5000)
    end.

get_state(Guild) ->
    gen_server:call(Guild, {get_sessions}, 10000).

hold_connect_workers(Guild, MaxQueue) ->
    sys:replace_state(Guild, fun(S) ->
        S#{session_connect_inflight => 8, session_connect_max_queue => MaxQueue}
    end),
    ok.

release_connect_workers(Guild) ->
    sys:replace_state(Guild, fun(S) ->
        guild_connect_async:maybe_start_session_connect_workers(
            S#{session_connect_inflight => 0, session_connect_max_queue => 16}
        )
    end),
    ok.

reconnect_all(Guild, Sessions, Attempt) ->
    [send_connect(Guild, Sid, Sessions, Attempt) || {Sid, _, _} <- Sessions],
    [?assertMatch({ok, _, _}, await_result(Sid, Attempt)) || {Sid, _, _} <- Sessions],
    ok.

send_connect(Guild, Sid, Sessions, Attempt) ->
    {Sid, UserId, Pid} = lists:keyfind(Sid, 1, Sessions),
    Request = #{
        session_id => Sid,
        user_id => UserId,
        session_pid => Pid,
        bot => false,
        is_staff => false,
        initial_guild_id => ?GUILD_ID,
        active_guilds => sets:from_list([?GUILD_ID])
    },
    gen_server:cast(
        Guild,
        {session_connect_async, #{
            guild_id => ?GUILD_ID, attempt => Attempt, request => Request
        }}
    ).

await_result(Sid, Attempt) ->
    receive
        {connect_result, Sid, Attempt, Reply} -> Reply
    after 10000 ->
        error({no_connect_result, Sid, Attempt})
    end.

kill_session(Sid, Sessions) ->
    {Sid, _UserId, Pid} = lists:keyfind(Sid, 1, Sessions),
    Ref = monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    end.

flush() ->
    receive
        {connect_result, _, _, _} -> flush()
    after 0 ->
        ok
    end.

transferred_state(Exported) ->
    guild_manager_shard_lifecycle:normalize_transferred_guild_state(?GUILD_ID, Exported).

base_state(Sessions) ->
    Members = [member(UserId) || UserId <- [?USER_A, ?USER_B, ?USER_C]],
    #{
        id => ?GUILD_ID,
        member_count => length(Members),
        sessions => maps:from_list([
            {Sid, #{
                session_id => Sid,
                user_id => UserId,
                pid => Pid,
                active_guilds => sets:from_list([?GUILD_ID])
            }}
         || {Sid, UserId, Pid} <- Sessions
        ]),
        data => #{
            <<"guild">> => #{
                <<"id">> => ?GUILD_ID,
                <<"owner_id">> => ?USER_A,
                <<"features">> => [],
                <<"member_count">> => length(Members)
            },
            <<"roles">> => [
                #{
                    <<"id">> => ?GUILD_ID,
                    <<"name">> => <<"everyone">>,
                    <<"permissions">> =>
                        constants:view_channel_permission() bor
                        constants:view_channel_members_permission(),
                    <<"hoist">> => false,
                    <<"position">> => 0
                }
            ],
            <<"channels">> => [
                #{
                    <<"id">> => ?CHANNEL_ID,
                    <<"name">> => <<"general">>,
                    <<"type">> => 0,
                    <<"permission_overwrites">> => []
                }
            ],
            <<"members">> => Members
        }
    }.

member(UserId) ->
    Name = integer_to_binary(UserId),
    #{
        <<"user">> => #{
            <<"id">> => UserId,
            <<"username">> => <<"u", Name/binary>>,
            <<"global_name">> => <<"U", Name/binary>>,
            <<"bot">> => false
        },
        <<"nick">> => null,
        <<"roles">> => [],
        <<"joined_at">> => <<"2026-01-01T00:00:00.000000+00:00">>
    }.
