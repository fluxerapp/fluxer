%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(channel_threads_flip_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

guild_fan_out_touches_only_guilds_whose_activity_changed_test() ->
    Previous = config(#{<<"enabled_guild_ids">> => [<<"1">>, <<"2">>]}),
    Current = config(#{<<"enabled_guild_ids">> => [<<"2">>, <<"3">>]}),
    Candidates = [{guild, Id, self()} || Id <- [1, 2, 3, 4]] ++ [{user, 1, self()}],
    ?assertEqual(
        [{guild, 1, self()}, {guild, 3, self()}],
        channel_threads_flip:affected_targets(Previous, Current, Candidates)
    ).

user_fan_out_touches_only_users_whose_activity_or_exclusion_changed_test() ->
    Previous = config(#{<<"included_user_ids">> => [<<"1">>]}),
    Current = config(#{
        <<"included_user_ids">> => [<<"1">>, <<"2">>], <<"excluded_user_ids">> => [<<"3">>]
    }),
    Candidates = [{user, Id, self()} || Id <- [1, 2, 3, 4]] ++ [{guild, 2, self()}],
    ?assertEqual(
        [{user, 2, self()}, {user, 3, self()}],
        channel_threads_flip:affected_targets(Previous, Current, Candidates)
    ).

user_only_change_also_recomputes_active_guilds_test() ->
    Previous = config(#{<<"enabled_guild_ids">> => [<<"1">>]}),
    Current = Previous#{excluded_users := #{<<"7">> => true}, config_version := 2},
    Candidates = [{guild, 1, self()}, {guild, 2, self()}, {user, 7, self()}],
    ?assertEqual(
        [{viewers, 1, self()}, {user, 7, self()}],
        channel_threads_flip:affected_targets(Previous, Current, Candidates)
    ).

guild_flip_is_not_queued_behind_a_viewer_sweep_test() ->
    Sweep = [{viewers, Id, self()} || Id <- lists:seq(1, 100)],
    Pending0 = channel_threads_flip:schedule(Sweep, 0, empty(), fun() -> 0 end),
    Pending1 = channel_threads_flip:schedule([{guild, 500, self()}], 10, Pending0, fun() ->
        0
    end),
    {Due, _Pending2} = channel_threads_flip:take_due(Pending1, 10, limits(10, 1000)),
    ?assert(lists:member({guild, 500, self()}, Due)),
    ?assertEqual(10, length([T || {viewers, _, _} = T <- Due])).

first_load_flips_guilds_but_not_users_test() ->
    Previous = channel_threads_config:default_config(),
    Current = config(#{
        <<"config_version">> => 3,
        <<"enabled_guild_ids">> => [<<"1">>],
        <<"user_basis_points">> => 10000
    }),
    Candidates = [{guild, 1, self()}, {guild, 2, self()}, {user, 7, self()}],
    ?assertEqual(
        [{guild, 1, self()}],
        channel_threads_flip:affected_targets(Previous, Current, Candidates)
    ).

version_only_change_touches_nothing_test() ->
    Previous = config(#{<<"guild_basis_points">> => 10000, <<"user_basis_points">> => 10000}),
    Current = Previous#{config_version := 42},
    Candidates =
        [{guild, Id, self()} || Id <- lists:seq(1, 50)] ++
            [{user, Id, self()} || Id <- lists:seq(1, 50)],
    ?assertEqual([], channel_threads_flip:affected_targets(Previous, Current, Candidates)).

changes_while_disabled_touch_nothing_test() ->
    Previous = config(#{<<"enabled">> => false}),
    Current = config(#{
        <<"enabled">> => false,
        <<"enabled_guild_ids">> => [<<"1">>],
        <<"included_user_ids">> => [<<"1">>],
        <<"excluded_user_ids">> => [<<"2">>]
    }),
    Candidates = [{guild, 1, self()}, {user, 1, self()}, {user, 2, self()}],
    ?assertEqual([], channel_threads_flip:affected_targets(Previous, Current, Candidates)).

disabled_config_change_skips_registry_scan_test() ->
    process_registry:init(),
    Entries = [{{guild, 9200001}, self()}, {{presence, 9200001}, self()}],
    ets:insert(process_registry_table, Entries),
    try
        Previous = config(#{<<"enabled">> => false}),
        Current = config(#{
            <<"enabled">> => false,
            <<"enabled_guild_ids">> => [<<"9200001">>],
            <<"included_user_ids">> => [<<"9200001">>]
        }),
        State0 = #{pending => empty(), timer => undefined, healed => #{}},
        ?assertEqual(
            {noreply, State0},
            channel_threads_flip:handle_cast({config_changed, Previous, Current}, State0)
        )
    after
        [ets:delete(process_registry_table, Key) || {Key, _} <- Entries]
    end.

global_kill_flips_every_active_guild_test() ->
    Previous = config(#{<<"guild_basis_points">> => 10000}),
    Current = Previous#{enabled := false, config_version := 2},
    Candidates = [{guild, Id, self()} || Id <- lists:seq(1, 20)],
    ?assertEqual(
        Candidates, channel_threads_flip:affected_targets(Previous, Current, Candidates)
    ).

schedule_applies_jitter_and_dedupes_targets_test() ->
    Jitters = counter_jitter([500, 100, 900]),
    Pending0 = channel_threads_flip:schedule(
        [{guild, 1, self()}, {guild, 2, self()}, {guild, 1, self()}, {user, 1, self()}],
        1000,
        empty(),
        Jitters
    ),
    ?assertEqual(#{{guild, 1} => 1500, {guild, 2} => 1100, {user, 1} => 1900}, keys(Pending0)),
    {Due, Pending1} = channel_threads_flip:take_due(Pending0, 1600, limits(10, 10)),
    ?assertEqual([{guild, 2, self()}, {guild, 1, self()}], Due),
    ?assertEqual(#{{user, 1} => 1900}, keys(Pending1)).

rescheduling_a_restarted_process_keeps_its_slot_and_takes_the_new_pid_test() ->
    Stale = spawn(fun() -> ok end),
    Pending0 = channel_threads_flip:schedule([{guild, 1, Stale}], 0, empty(), fun() -> 500 end),
    Pending1 = channel_threads_flip:schedule([{guild, 1, self()}], 100, Pending0, fun() ->
        900
    end),
    ?assertEqual(#{{guild, 1} => 500}, keys(Pending1)),
    ?assertEqual(
        {[{guild, 1, self()}], empty()},
        channel_threads_flip:take_due(Pending1, 500, limits(10, 10))
    ).

take_due_respects_the_batch_size_test() ->
    Pending0 = channel_threads_flip:schedule(
        [{guild, Id, self()} || Id <- lists:seq(1, 25)], 0, empty(), fun() -> 0 end
    ),
    {First, Pending1} = channel_threads_flip:take_due(Pending0, 0, limits(10, 1000)),
    {Second, Pending2} = channel_threads_flip:take_due(Pending1, 0, limits(10, 1000)),
    {Third, Pending3} = channel_threads_flip:take_due(Pending2, 0, limits(10, 1000)),
    ?assertEqual([10, 10, 5], [length(First), length(Second), length(Third)]),
    ?assertEqual(#{}, keys(Pending3)),
    ?assertEqual(
        lists:seq(1, 25), lists:sort([Id || {guild, Id, _} <- First ++ Second ++ Third])
    ).

user_flips_drain_in_large_batches_behind_throttled_guilds_test() ->
    Targets =
        [{guild, Id, self()} || Id <- lists:seq(1, 50)] ++
            [{user, Id, self()} || Id <- lists:seq(1, 2500)],
    Pending0 = channel_threads_flip:schedule(Targets, 0, empty(), fun() -> 0 end),
    {First, Pending1} = channel_threads_flip:take_due(Pending0, 0, limits(10, 1000)),
    ?assertEqual({10, 1000}, kind_counts(First)),
    {Second, Pending2} = channel_threads_flip:take_due(Pending1, 0, limits(10, 1000)),
    {Third, Pending3} = channel_threads_flip:take_due(Pending2, 0, limits(10, 1000)),
    ?assertEqual([{10, 1000}, {10, 500}], [kind_counts(Second), kind_counts(Third)]),
    ?assertEqual(20, map_size(keys(Pending3))),
    ?assert(lists:all(fun({Kind, _Id}) -> Kind =:= guild end, maps:keys(keys(Pending3)))).

self_heal_flips_immediately_once_per_cooldown_test() ->
    Pending0 = channel_threads_flip:schedule(
        [{guild, 7, self()}, {guild, 8, self()}], 0, empty(), fun() -> 50000 end
    ),
    State0 = #{pending => Pending0, timer => undefined, healed => #{}},
    {Healed1, Pending1} = channel_threads_flip:heal(7, self(), 1000, State0),
    ?assertEqual(ok, receive_flip()),
    ?assertEqual(#{{guild, 8} => 50000}, keys(Pending1)),
    State1 = State0#{healed := Healed1, pending := Pending1},
    {Healed2, Pending2} = channel_threads_flip:heal(7, self(), 20000, State1),
    ?assertEqual(none, receive_flip()),
    ?assertEqual(Pending1, Pending2),
    {_Healed3, _Pending3} = channel_threads_flip:heal(
        7, self(), 40000, State1#{healed := Healed2}
    ),
    ?assertEqual(ok, receive_flip()).

config_change_flips_only_local_registry_targets_test() ->
    process_registry:init(),
    Parent = self(),
    [LocalGuild, OtherGuild, LocalUser, ExcludedUser, Session] =
        [spawn(fun() -> forward(Parent) end) || _ <- lists:seq(1, 5)],
    Remote = remote_pid(),
    Entries = [
        {{guild, 9100001}, LocalGuild},
        {{guild, 9100002}, Remote},
        {{guild, 9100003}, OtherGuild},
        {{guild, 9100004}, loading},
        {{guild, <<"9100005">>}, OtherGuild},
        {{presence, 9100001}, LocalUser},
        {{presence, 9100002}, ExcludedUser},
        {{presence, 9100003}, Remote},
        {{session, 9100001}, Session}
    ],
    ets:insert(process_registry_table, Entries),
    try
        Previous = config(#{}),
        Current = config(#{
            <<"enabled_guild_ids">> => [<<"9100001">>, <<"9100002">>, <<"9100004">>],
            <<"included_user_ids">> => [<<"9100001">>, <<"9100003">>],
            <<"excluded_user_ids">> => [<<"9100002">>]
        }),
        State0 = #{pending => empty(), timer => undefined, healed => #{}},
        {noreply, #{pending := Scheduled, timer := Timer}} =
            channel_threads_flip:handle_cast({config_changed, Previous, Current}, State0),
        _ = erlang:cancel_timer(Timer),
        Expected = [
            {guild, 9100001, LocalGuild},
            {user, 9100001, LocalUser},
            {user, 9100002, ExcludedUser}
        ],
        ?assertEqual(lists:sort(Expected), lists:sort(ours(Scheduled))),
        Due = channel_threads_flip:schedule(
            Expected, erlang:monotonic_time(millisecond), empty(), fun() -> 0 end
        ),
        {noreply, #{pending := Rest}} =
            channel_threads_flip:handle_info(flip_tick, State0#{pending := Due}),
        ?assertEqual(#{}, keys(Rest)),
        Version = channel_threads_config:version(),
        ?assertEqual(
            lists:sort([
                {LocalGuild, {thread_gate_flip, Version}},
                {LocalUser, {thread_user_flip, Version}},
                {ExcludedUser, {thread_user_flip, Version}}
            ]),
            lists:sort(collect(3))
        ),
        ?assertEqual([], collect(1))
    after
        [ets:delete(process_registry_table, Key) || {Key, _} <- Entries],
        [exit(Pid, kill) || Pid <- [LocalGuild, OtherGuild, LocalUser, ExcludedUser, Session]]
    end.

ours(#{due := Due}) ->
    [
        {Kind, Id, Pid}
     || KindDue <- maps:values(Due),
        {_At, {Kind, Id}, Pid} <- gb_sets:to_list(KindDue),
        Id div 100 =:= 91000
    ].

limits(Guilds, Users) ->
    #{guild => Guilds, viewers => Guilds, user => Users}.

kind_counts(Targets) ->
    {length([T || {guild, _, _} = T <- Targets]), length([T || {user, _, _} = T <- Targets])}.

forward(Parent) ->
    receive
        Msg ->
            Parent ! {self(), Msg},
            forward(Parent)
    end.

collect(0) ->
    [];
collect(N) ->
    receive
        {Pid, {Tag, _} = Msg} when
            is_pid(Pid), (Tag =:= thread_gate_flip orelse Tag =:= thread_user_flip)
        ->
            [{Pid, Msg} | collect(N - 1)]
    after 200 -> []
    end.

remote_pid() ->
    Node = <<"remote@nowhere">>,
    binary_to_term(
        <<131, 88, 119, (byte_size(Node)), Node/binary, 1:32, 0:32, 1:32>>
    ).

receive_flip() ->
    receive
        {thread_gate_flip, Version} when is_integer(Version) -> ok
    after 0 -> none
    end.

counter_jitter(Values) ->
    Ref = make_ref(),
    put(Ref, Values),
    fun() ->
        [Next | Rest] = get(Ref),
        put(Ref, Rest),
        Next
    end.

empty() ->
    #{
        due => #{guild => gb_sets:new(), viewers => gb_sets:new(), user => gb_sets:new()},
        keys => #{}
    }.

keys(#{keys := Keys}) ->
    maps:map(fun(_Key, {At, _Pid}) -> At end, Keys).

config(Overrides) ->
    {ok, Config} = channel_threads_config:validate_config(
        maps:merge(#{<<"enabled">> => true}, Overrides)
    ),
    Config.
