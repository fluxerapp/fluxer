%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_chaos_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(GUILD_ID, 900100).
-define(PUBLIC_CHANNEL_ID, 900500).
-define(PRIVATE_CHANNEL_ID, 900501).
-define(HOISTED_ROLE_ID, 900200).
-define(PRIVATE_ROLE_ID, 900201).
-define(NEW_HOISTED_ROLE_ID, 900202).
-define(INITIAL_MEMBER_COUNT, 480).
-define(SESSION_COUNT, 24).
-define(WORKER_COUNT, 24).

guild_member_add_batched_sync_test_() ->
    {timeout, 30, fun guild_member_add_batched_sync/0}.

concurrent_member_role_channel_chaos_stress_test_() ->
    {timeout, 120, fun concurrent_member_role_channel_chaos_stress/0}.

guild_member_add_batched_sync() ->
    Parent = self(),
    SessionPid = start_capture(Parent),
    try
        InitialMembers = [
            internal_member(1000 + I, I, roles_for_member(I))
         || I <- lists:seq(1, 32)
        ],
        State = base_state(InitialMembers, [{<<"s1">>, 1001, SessionPid}]),
        with_guild(State, fun(GuildPid) ->
            subscribe_and_drain(GuildPid, [
                {<<"s1">>, ?PUBLIC_CHANNEL_ID, [{0, 99}]}
            ]),
            JoinedUserId = 199999,
            JoinedMember = event_member(JoinedUserId, 0, []),
            ok = dispatch_event(GuildPid, guild_member_add, JoinedMember),
            StateAfterAdd = get_guild_state(GuildPid),
            AddedMember = guild_data_index:get_member(
                JoinedUserId, maps:get(data, StateAfterAdd, #{})
            ),
            ?assert(
                guild_permissions:can_view_channel(
                    JoinedUserId, ?PUBLIC_CHANNEL_ID, AddedMember, StateAfterAdd
                )
            ),
            assert_engine_has_user(default, JoinedUserId, StateAfterAdd),
            assert_engine_has_user(?PUBLIC_CHANNEL_ID, JoinedUserId, StateAfterAdd),
            Payload = wait_for_member_list_payload(
                fun(P) ->
                    payload_channel_id(P) =:= ?PUBLIC_CHANNEL_ID andalso
                        payload_has_member(JoinedUserId, P)
                end,
                2000
            ),
            ?assertEqual(length(InitialMembers) + 1, maps:get(<<"member_count">>, Payload)),
            FinalState = get_guild_state(GuildPid),
            assert_default_engine_consistent(FinalState),
            assert_channel_engine_consistent(?PUBLIC_CHANNEL_ID, FinalState)
        end)
    after
        SessionPid ! stop,
        flush_captures()
    end.

concurrent_member_role_channel_chaos_stress() ->
    Parent = self(),
    CapturePids = [start_capture(Parent) || _ <- lists:seq(1, ?SESSION_COUNT)],
    try
        InitialMembers = [
            internal_member(100000 + I, I, roles_for_member(I))
         || I <- lists:seq(1, ?INITIAL_MEMBER_COUNT)
        ],
        Sessions = build_sessions(CapturePids),
        State = base_state(InitialMembers, Sessions),
        with_guild(State, fun(GuildPid) ->
            subscribe_and_drain(GuildPid, subscription_requests(Sessions)),
            Ops = chaos_ops(),
            StartedAt = erlang:monotonic_time(millisecond),
            apply_concurrent_ops(GuildPid, Ops, ?WORKER_COUNT),
            ElapsedMs = erlang:monotonic_time(millisecond) - StartedAt,
            FinalState = get_guild_state(GuildPid),
            Captures = collect_captures(1500),
            MemberListUpdates = [
                Payload
             || {member_list_capture, _Pid, guild_member_list_update, Payload} <- Captures
            ],
            ?assertMatch([_ | _], MemberListUpdates),
            ?assert(payloads_include_channel(?PUBLIC_CHANNEL_ID, MemberListUpdates)),
            ?assert(payloads_include_channel(?PRIVATE_CHANNEL_ID, MemberListUpdates)),
            ?assert(ElapsedMs < 60000),
            assert_default_engine_consistent(FinalState),
            assert_channel_engine_consistent(?PUBLIC_CHANNEL_ID, FinalState),
            assert_channel_engine_consistent(?PRIVATE_CHANNEL_ID, FinalState),
            assert_removed_members_absent(FinalState),
            assert_added_members_present(FinalState)
        end)
    after
        lists:foreach(fun(Pid) -> Pid ! stop end, CapturePids),
        flush_captures()
    end.

with_guild(State, Fun) ->
    {ok, GuildPid} = guild:start_link(State),
    try
        Fun(GuildPid)
    after
        safe_stop_guild(GuildPid)
    end.

safe_stop_guild(GuildPid) ->
    try gen_server:call(GuildPid, {terminate}, 5000) of
        _ -> ok
    catch
        exit:_ -> ok
    end.

start_capture(Parent) ->
    spawn_link(fun() -> capture_loop(Parent) end).

capture_loop(Parent) ->
    receive
        stop ->
            ok;
        {'$gen_cast', {dispatch, Event, Payload}} ->
            Parent ! {member_list_capture, self(), Event, decode_payload(Payload)},
            capture_loop(Parent);
        _Other ->
            capture_loop(Parent)
    after 30000 ->
        ok
    end.

decode_payload({pre_encoded, Bin}) when is_binary(Bin) ->
    json:decode(Bin);
decode_payload(Payload) ->
    Payload.

subscribe_and_drain(GuildPid, Requests) ->
    lists:foreach(
        fun({SessionId, ChannelId, Ranges}) ->
            ok = gen_server:call(
                GuildPid,
                {lazy_subscribe, #{
                    session_id => SessionId,
                    channel_id => ChannelId,
                    ranges => Ranges
                }},
                30000
            )
        end,
        Requests
    ),
    SubscribedState = drain_lazy_subscribe_buffer(GuildPid),
    assert_requests_subscribed(Requests, SubscribedState),
    await_initial_syncs(Requests, SubscribedState),
    _ = collect_captures(100),
    ok.

assert_requests_subscribed(Requests, State) ->
    ExpectedRequests = retained_subscription_requests(Requests),
    SubsTab = maps:get(member_list_subscriptions, State),
    lists:foreach(
        fun({SessionId, ChannelId, Ranges}) ->
            case Ranges of
                [] ->
                    ok;
                _ ->
                    ListId = integer_to_binary(ChannelId),
                    ?assert(
                        guild_member_list_subs:is_subscribed(ListId, SessionId, SubsTab),
                        {not_subscribed, SessionId, ChannelId}
                    )
            end
        end,
        ExpectedRequests
    ).

retained_subscription_requests(Requests) ->
    maps:values(lists:foldl(fun retain_subscription_request/2, #{}, Requests)).

retain_subscription_request({SessionId, ChannelId, []}, Acc) ->
    case maps:get(SessionId, Acc, undefined) of
        {SessionId, ChannelId, _Ranges} -> maps:remove(SessionId, Acc);
        _ -> Acc
    end;
retain_subscription_request({SessionId, _ChannelId, _Ranges} = Request, Acc) ->
    Acc#{SessionId => Request}.

await_initial_syncs(Requests, State) ->
    Expected = initial_sync_expectations(Requests, State),
    Deadline = erlang:monotonic_time(millisecond) + 5000,
    wait_for_initial_syncs(Expected, Deadline, []).

initial_sync_expectations(Requests, State) ->
    SubsTab = maps:get(member_list_subscriptions, State),
    lists:sort(
        lists:filtermap(
            fun({SessionId, ChannelId, Ranges}) ->
                initial_sync_expectation(SessionId, ChannelId, Ranges, SubsTab, State)
            end,
            Requests
        )
    ).

initial_sync_expectation(_SessionId, _ChannelId, [], _SubsTab, _State) ->
    false;
initial_sync_expectation(SessionId, ChannelId, _Ranges, SubsTab, State) ->
    ListId = integer_to_binary(ChannelId),
    case guild_member_list_subs:is_subscribed(ListId, SessionId, SubsTab) of
        true -> {true, {ChannelId, length(expected_channel_member_ids(ChannelId, State))}};
        false -> false
    end.

wait_for_initial_syncs([], _Deadline, _Seen) ->
    ok;
wait_for_initial_syncs(Expected, Deadline, Seen) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {member_list_capture, _Pid, guild_member_list_update, Payload} ->
            case take_initial_sync_match(Payload, Expected) of
                {matched, RemainingExpected} ->
                    wait_for_initial_syncs(RemainingExpected, Deadline, [Payload | Seen]);
                unmatched ->
                    wait_for_initial_syncs(Expected, Deadline, [Payload | Seen])
            end;
        _Other ->
            wait_for_initial_syncs(Expected, Deadline, Seen)
    after Remaining ->
        erlang:error(
            {initial_member_list_syncs_not_received, Expected, summarize_payloads(Seen)}
        )
    end.

take_initial_sync_match(_Payload, []) ->
    unmatched;
take_initial_sync_match(Payload, [{ChannelId, MemberCount} | Rest]) ->
    case
        payload_channel_id(Payload) =:= ChannelId andalso
            payload_member_count(Payload) =:= MemberCount
    of
        true ->
            {matched, Rest};
        false ->
            case take_initial_sync_match(Payload, Rest) of
                {matched, Remaining} -> {matched, [{ChannelId, MemberCount} | Remaining]};
                unmatched -> unmatched
            end
    end.

dispatch_event(GuildPid, Event, Data) ->
    gen_server:call(GuildPid, {dispatch, #{event => Event, data => Data}}, 60000).

presence_update(GuildPid, UserId, Status) ->
    GuildPid ! {presence, UserId, #{<<"status">> => Status, <<"mobile">> => false}},
    _ = gen_server:call(GuildPid, {get_counts}, 60000),
    ok.

drain_lazy_subscribe_buffer(GuildPid) ->
    GuildPid ! flush_lazy_subscribe_buffer,
    State = get_guild_state(GuildPid),
    case maps:is_key(lazy_subscribe_buffer, State) of
        true -> drain_lazy_subscribe_buffer(GuildPid);
        false -> State
    end.

get_guild_state(GuildPid) ->
    gen_server:call(GuildPid, {get_sessions}, 60000).

apply_concurrent_ops(GuildPid, Ops, WorkerCount) ->
    Parent = self(),
    Chunks = chunk_round_robin(Ops, WorkerCount),
    Workers = [spawn_chaos_worker(Parent, GuildPid, Chunk) || Chunk <- Chunks],
    lists:foreach(fun(Pid) -> Pid ! go end, Workers),
    wait_workers(Workers).

spawn_chaos_worker(Parent, GuildPid, Chunk) ->
    spawn_link(fun() -> chaos_worker_loop(Parent, GuildPid, Chunk) end).

chaos_worker_loop(Parent, GuildPid, Chunk) ->
    receive
        go ->
            Parent ! {chaos_worker_done, self(), apply_ops_safely(GuildPid, Chunk)}
    after 60000 ->
        Parent ! {chaos_worker_done, self(), {error, start_timeout}}
    end.

apply_ops_safely(GuildPid, Ops) ->
    try apply_ops(GuildPid, Ops) of
        ok -> ok
    catch
        Class:Reason:Stacktrace -> {error, {Class, Reason, Stacktrace}}
    end.

apply_ops(GuildPid, Ops) ->
    lists:foreach(fun(Op) -> apply_op(GuildPid, Op) end, Ops).

apply_op(GuildPid, {presence, UserId, Status}) ->
    presence_update(GuildPid, UserId, Status);
apply_op(GuildPid, {Event, Data}) ->
    ok = dispatch_event(GuildPid, Event, Data).

wait_workers([]) ->
    ok;
wait_workers(Workers) ->
    receive
        {chaos_worker_done, Pid, ok} ->
            wait_workers(lists:delete(Pid, Workers));
        {chaos_worker_done, _Pid, {error, Reason}} ->
            ?assert(false, {chaos_worker_failed, Reason});
        {chaos_worker_done, _Pid, Other} ->
            ?assert(false, {chaos_worker_returned, Other})
    after 60000 ->
        ?assert(false, {chaos_workers_timed_out, length(Workers)})
    end.

chunk_round_robin(Ops, Count) ->
    Indexed = lists:zip(lists:seq(1, length(Ops)), Ops),
    [
        [Op || {Idx, Op} <- Indexed, (Idx - 1) rem Count =:= Worker]
     || Worker <- lists:seq(0, Count - 1)
    ].

wait_for_member_list_payload(Pred, TimeoutMs) ->
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    wait_for_member_list_payload_until(Pred, Deadline, []).

wait_for_member_list_payload_until(Pred, Deadline, Seen) ->
    Remaining = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {member_list_capture, _Pid, guild_member_list_update, Payload} ->
            case Pred(Payload) of
                true -> Payload;
                false -> wait_for_member_list_payload_until(Pred, Deadline, [Payload | Seen])
            end;
        _Other ->
            wait_for_member_list_payload_until(Pred, Deadline, Seen)
    after Remaining ->
        erlang:error(
            {member_list_update_not_received, summarize_payloads(lists:reverse(Seen)),
                summarize_captures(collect_captures(0))}
        )
    end.

collect_captures(QuietMs) ->
    collect_captures(QuietMs, []).

collect_captures(QuietMs, Acc) ->
    receive
        {member_list_capture, _Pid, _Event, _Payload} = Msg ->
            collect_captures(QuietMs, [Msg | Acc]);
        _Other ->
            collect_captures(QuietMs, Acc)
    after QuietMs ->
        lists:reverse(Acc)
    end.

flush_captures() ->
    receive
        {member_list_capture, _Pid, _Event, _Payload} ->
            flush_captures();
        _Other ->
            flush_captures()
    after 0 ->
        ok
    end.

payload_channel_id(Payload) ->
    case maps:get(<<"channel_id">>, Payload, undefined) of
        Bin when is_binary(Bin) -> snowflake_id:parse(Bin);
        Int when is_integer(Int) -> Int;
        _ -> undefined
    end.

payload_member_count(Payload) ->
    maps:get(<<"member_count">>, Payload, undefined).

payloads_include_channel(ChannelId, Payloads) ->
    lists:any(fun(Payload) -> payload_channel_id(Payload) =:= ChannelId end, Payloads).

payload_has_member(UserId, Payload) ->
    lists:any(
        fun(Op) -> op_has_member(UserId, Op) end,
        maps:get(<<"ops">>, Payload, [])
    ).

op_has_member(UserId, Op) ->
    lists:any(
        fun(Item) -> item_user_id(Item) =:= UserId end,
        maps:get(<<"items">>, Op, [])
    ).

item_user_id(#{<<"member">> := Member}) ->
    guild_member_list_common:get_member_user_id(Member);
item_user_id(_) ->
    undefined.

summarize_captures(Captures) ->
    [
        {Event, summarize_payload(Payload)}
     || {member_list_capture, _Pid, Event, Payload} <- Captures
    ].

summarize_payloads(Payloads) ->
    [summarize_payload(Payload) || Payload <- Payloads].

summarize_payload(Payload) ->
    #{
        channel_id => payload_channel_id(Payload),
        member_count => payload_member_count(Payload),
        op_count => length(maps:get(<<"ops">>, Payload, [])),
        sample_user_ids => lists:sublist(payload_member_ids(Payload), 8)
    }.

payload_member_ids(Payload) ->
    [
        UserId
     || Op <- maps:get(<<"ops">>, Payload, []),
        Item <- maps:get(<<"items">>, Op, []),
        UserId <- [item_user_id(Item)],
        is_integer(UserId)
    ].

assert_default_engine_consistent(State) ->
    Data = maps:get(data, State, #{}),
    MemberMap = guild_data_index:member_map(Data),
    Ref = maps:get(member_list_engine, State),
    assert_engine_matches_ids(<<"0">>, Ref, maps:keys(MemberMap), State).

assert_channel_engine_consistent(ChannelId, State) ->
    ListId = integer_to_binary(ChannelId),
    Engines = maps:get(channel_member_list_engines, State, #{}),
    ?assert(maps:is_key(ListId, Engines)),
    Ref = maps:get(ListId, Engines),
    ExpectedIds = expected_channel_member_ids(ChannelId, State),
    assert_engine_matches_ids(ListId, Ref, ExpectedIds, State).

assert_engine_has_user(default, UserId, State) ->
    Ref = maps:get(member_list_engine, State),
    assert_ref_has_user(default, Ref, UserId, State);
assert_engine_has_user(ChannelId, UserId, State) ->
    ListId = integer_to_binary(ChannelId),
    Engines = maps:get(channel_member_list_engines, State, #{}),
    Ref = maps:get(ListId, Engines),
    assert_ref_has_user(ChannelId, Ref, UserId, State).

assert_ref_has_user(Label, Ref, UserId, State) ->
    Ids = guild_member_list_engine:get_sorted_user_ids(Ref),
    case lists:member(UserId, Ids) of
        true ->
            ok;
        false ->
            Data = maps:get(data, State, #{}),
            erlang:error({
                engine_missing_user,
                Label,
                UserId,
                guild_data_index:get_member(UserId, Data),
                guild_member_list_engine:get_counts(Ref),
                lists:sublist(lists:sort(Ids), 10)
            })
    end.

assert_engine_matches_ids(ListId, Ref, ExpectedIds0, State) ->
    ExpectedIds = lists:sort(ExpectedIds0),
    ActualIds = lists:sort(guild_member_list_engine:get_sorted_user_ids(Ref)),
    ?assertEqual(ExpectedIds, ActualIds),
    ExpectedOnline = length([U || U <- ExpectedIds, expected_online(U, State)]),
    ?assertEqual(
        {length(ExpectedIds), ExpectedOnline},
        guild_member_list_engine:get_counts(Ref)
    ),
    ?assertEqual(
        expected_groups(ExpectedIds, State),
        guild_member_list_engine:get_groups(Ref)
    ),
    assert_snapshot_matches_engine(ListId, Ref, State).

assert_snapshot_matches_engine(ListId, Ref, State) ->
    {Total, Online} = guild_member_list_engine:get_counts(Ref),
    {SnapshotTotal, SnapshotOnline, SnapshotGroups, SnapshotItems} =
        guild_member_list:member_list_snapshot(ListId, State),
    ?assertEqual(Total, SnapshotTotal),
    ?assertEqual(Online, SnapshotOnline),
    ?assertEqual(
        visible_groups(guild_member_list_engine:get_groups(Ref)),
        SnapshotGroups
    ),
    SnapshotIds = lists:sort([
        UserId
     || #{<<"member">> := Member} <- SnapshotItems,
        UserId <- [guild_member_list_common:get_member_user_id(Member)],
        is_integer(UserId)
    ]),
    ?assertEqual(SnapshotIds, lists:usort(SnapshotIds)).

visible_groups(Groups) ->
    Threshold = guild_member_list_offline:threshold(),
    [
        #{<<"id">> => Id, <<"count">> => Count}
     || {Id, Count} <- Groups,
        Count > 0,
        Id =/= <<"offline">> orelse Count =< Threshold
    ].

expected_channel_member_ids(ChannelId, State) ->
    Data = maps:get(data, State, #{}),
    MemberMap = guild_data_index:member_map(Data),
    [
        UserId
     || {UserId, Member} <- maps:to_list(MemberMap),
        can_view_channel(UserId, ChannelId, Member, State)
    ].

can_view_channel(UserId, ChannelId, Member, State) ->
    try guild_permissions:can_view_channel(UserId, ChannelId, Member, State) of
        Bool when is_boolean(Bool) -> Bool;
        _ -> false
    catch
        _:_ -> false
    end.

expected_groups(ExpectedIds, State) ->
    Data = maps:get(data, State, #{}),
    Roles = guild_data_index:role_list(Data),
    HoistedRoleIds = guild_member_list_store:prepare_hoisted_role_ids(
        Roles, maps:get(id, State, undefined)
    ),
    InitialCounts = maps:from_list(
        [{integer_to_binary(RoleId), 0} || RoleId <- HoistedRoleIds] ++
            [{<<"online">>, 0}, {<<"offline">>, 0}]
    ),
    Counts = lists:foldl(
        fun(UserId, Acc) ->
            Member = guild_data_index:get_member(UserId, Data),
            GroupId = expected_group_id(Member, HoistedRoleIds, UserId, State),
            maps:update_with(GroupId, fun(N) -> N + 1 end, 1, Acc)
        end,
        InitialCounts,
        ExpectedIds
    ),
    [
        {integer_to_binary(RoleId), maps:get(integer_to_binary(RoleId), Counts, 0)}
     || RoleId <- HoistedRoleIds
    ] ++
        [
            {<<"online">>, maps:get(<<"online">>, Counts, 0)},
            {<<"offline">>, maps:get(<<"offline">>, Counts, 0)}
        ].

expected_group_id(Member, _HoistedRoleIds, _UserId, _State) when not is_map(Member) ->
    <<"offline">>;
expected_group_id(Member, HoistedRoleIds, UserId, State) ->
    case expected_online(UserId, State) of
        false ->
            <<"offline">>;
        true ->
            RoleIds = guild_member_list_store:extract_role_ids(Member),
            case guild_member_list_groups:find_top_hoisted_role(RoleIds, HoistedRoleIds) of
                undefined -> <<"online">>;
                RoleId -> integer_to_binary(RoleId)
            end
    end.

expected_online(UserId, State) ->
    Connected = maps:get(connected_user_ids, State, sets:new()),
    Presence = guild_state_member:lookup_presence(maps:get(member_presence, State), UserId),
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    sets:is_element(UserId, Connected) andalso
        Status =/= <<"offline">> andalso Status =/= <<"invisible">>.

assert_removed_members_absent(State) ->
    Data = maps:get(data, State, #{}),
    lists:foreach(
        fun(UserId) ->
            ?assertEqual(undefined, guild_data_index:get_member(UserId, Data))
        end,
        removed_user_ids()
    ).

assert_added_members_present(State) ->
    Data = maps:get(data, State, #{}),
    lists:foreach(
        fun(UserId) ->
            ?assertMatch(#{}, guild_data_index:get_member(UserId, Data))
        end,
        added_user_ids()
    ).

chaos_ops() ->
    AddOps = [
        {guild_member_add, event_member(UserId, UserId - 200000, add_roles(UserId))}
     || UserId <- added_user_ids()
    ],
    UpdateOps = [
        {guild_member_update, updated_member(UserId, Seq)}
     || {UserId, Seq} <- lists:zip(updated_user_ids(), lists:seq(1, length(updated_user_ids())))
    ],
    RemoveOps = [
        {guild_member_remove, #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}}}
     || UserId <- removed_user_ids()
    ],
    PresenceOps = [
        {presence, UserId, presence_status(Seq)}
     || {UserId, Seq} <- lists:zip(session_user_ids(), lists:seq(1, ?SESSION_COUNT))
    ],
    RoleOps = role_ops(),
    ChannelOps = channel_ops(),
    interleave_ops([AddOps, UpdateOps, RemoveOps, PresenceOps, RoleOps, ChannelOps]).

interleave_ops(OpLists) ->
    interleave_ops(OpLists, []).

interleave_ops(OpLists, Acc) ->
    Heads = [Head || [Head | _Tail] <- OpLists],
    Tails = [Tail || [_Head | Tail] <- OpLists, Tail =/= []],
    case Heads of
        [] -> lists:reverse(Acc);
        _ -> interleave_ops(Tails, lists:reverse(Heads) ++ Acc)
    end.

added_user_ids() ->
    lists:seq(200001, 200060).

updated_user_ids() ->
    lists:seq(100220, 100299).

removed_user_ids() ->
    lists:seq(100120, 100159).

session_user_ids() ->
    [100000 + I || I <- lists:seq(1, ?SESSION_COUNT)].

role_ops() ->
    View = constants:view_channel_permission(),
    ViewMembers = constants:view_channel_members_permission(),
    [
        {guild_role_update, #{
            <<"role">> =>
                event_role(?HOISTED_ROLE_ID, <<"Signal">>, View bor ViewMembers, true, 80)
        }},
        {guild_role_update, #{
            <<"role">> =>
                event_role(?PRIVATE_ROLE_ID, <<"Operators">>, View bor ViewMembers, true, 75)
        }},
        {guild_role_create, #{
            <<"role">> =>
                event_role(
                    ?NEW_HOISTED_ROLE_ID,
                    <<"New arrivals">>,
                    View bor ViewMembers,
                    true,
                    5
                )
        }},
        {guild_role_delete, #{<<"role_id">> => integer_to_binary(?NEW_HOISTED_ROLE_ID)}}
    ].

channel_ops() ->
    [
        {channel_update,
            event_private_channel(?PRIVATE_CHANNEL_ID, ?PRIVATE_ROLE_ID, ?HOISTED_ROLE_ID)}
    ].

presence_status(Seq) when Seq rem 5 =:= 0 ->
    <<"offline">>;
presence_status(Seq) when Seq rem 3 =:= 0 ->
    <<"dnd">>;
presence_status(Seq) when Seq rem 2 =:= 0 ->
    <<"idle">>;
presence_status(_Seq) ->
    <<"online">>.

subscription_requests(Sessions) ->
    Public = [
        {SessionId, ?PUBLIC_CHANNEL_ID, [{0, 99}, {100, 199}]}
     || {SessionId, _UserId, _Pid} <- Sessions
    ],
    Private = [
        {SessionId, ?PRIVATE_CHANNEL_ID, [{0, 99}]}
     || {SessionId, UserId, _Pid} <- Sessions,
        lists:member(?PRIVATE_ROLE_ID, roles_for_user_id(UserId))
    ],
    Public ++ Private.

build_sessions(CapturePids) ->
    UserIds = session_user_ids(),
    [
        {session_id(I), UserId, Pid}
     || {I, UserId, Pid} <- zip3(lists:seq(1, length(CapturePids)), UserIds, CapturePids)
    ].

base_state(Members, Sessions) ->
    #{
        id => ?GUILD_ID,
        member_count => length(Members),
        sessions => maps:from_list([
            {SessionId, #{
                user_id => UserId,
                pid => Pid,
                active_guilds => sets:from_list([?GUILD_ID])
            }}
         || {SessionId, UserId, Pid} <- Sessions
        ]),
        data => #{
            <<"guild">> => #{
                <<"id">> => ?GUILD_ID,
                <<"owner_id">> => 100001,
                <<"features">> => [],
                <<"member_count">> => length(Members)
            },
            <<"roles">> => initial_roles(),
            <<"channels">> => initial_channels(),
            <<"members">> => Members
        }
    }.

initial_roles() ->
    View = constants:view_channel_permission(),
    ViewMembers = constants:view_channel_members_permission(),
    [
        internal_role(?GUILD_ID, <<"everyone">>, View bor ViewMembers, false, 0),
        internal_role(?HOISTED_ROLE_ID, <<"Signal">>, View bor ViewMembers, true, 60),
        internal_role(?PRIVATE_ROLE_ID, <<"Operators">>, View bor ViewMembers, true, 70)
    ].

initial_channels() ->
    [
        #{
            <<"id">> => ?PUBLIC_CHANNEL_ID,
            <<"name">> => <<"general">>,
            <<"type">> => 0,
            <<"permission_overwrites">> => []
        },
        internal_private_channel(?PRIVATE_CHANNEL_ID, ?PRIVATE_ROLE_ID, undefined)
    ].

internal_private_channel(ChannelId, AllowRoleId, ExtraAllowRoleId) ->
    View = constants:view_channel_permission(),
    ViewMembers = constants:view_channel_members_permission(),
    AllowBits = View bor ViewMembers,
    RoleOverwrites =
        [
            internal_role_overwrite(AllowRoleId, AllowBits, 0)
         || is_integer(AllowRoleId)
        ] ++
            [
                internal_role_overwrite(ExtraAllowRoleId, AllowBits, 0)
             || is_integer(ExtraAllowRoleId)
            ],
    #{
        <<"id">> => ChannelId,
        <<"name">> => <<"ops">>,
        <<"type">> => 0,
        <<"permission_overwrites">> =>
            [internal_role_overwrite(?GUILD_ID, 0, View) | RoleOverwrites]
    }.

event_private_channel(ChannelId, AllowRoleId, ExtraAllowRoleId) ->
    View = constants:view_channel_permission(),
    ViewMembers = constants:view_channel_members_permission(),
    AllowBits = View bor ViewMembers,
    RoleOverwrites =
        [
            event_role_overwrite(AllowRoleId, AllowBits, 0)
         || is_integer(AllowRoleId)
        ] ++
            [
                event_role_overwrite(ExtraAllowRoleId, AllowBits, 0)
             || is_integer(ExtraAllowRoleId)
            ],
    #{
        <<"id">> => integer_to_binary(ChannelId),
        <<"name">> => <<"ops">>,
        <<"type">> => 0,
        <<"permission_overwrites">> =>
            [event_role_overwrite(?GUILD_ID, 0, View) | RoleOverwrites]
    }.

internal_role_overwrite(RoleId, Allow, Deny) ->
    #{
        <<"id">> => RoleId,
        <<"type">> => 0,
        <<"allow">> => Allow,
        <<"deny">> => Deny
    }.

event_role_overwrite(RoleId, Allow, Deny) ->
    #{
        <<"id">> => integer_to_binary(RoleId),
        <<"type">> => 0,
        <<"allow">> => integer_to_binary(Allow),
        <<"deny">> => integer_to_binary(Deny)
    }.

internal_role(RoleId, Name, Permissions, Hoist, Position) ->
    #{
        <<"id">> => RoleId,
        <<"name">> => Name,
        <<"permissions">> => Permissions,
        <<"hoist">> => Hoist,
        <<"position">> => Position
    }.

event_role(RoleId, Name, Permissions, Hoist, Position) ->
    #{
        <<"id">> => integer_to_binary(RoleId),
        <<"name">> => Name,
        <<"permissions">> => integer_to_binary(Permissions),
        <<"hoist">> => Hoist,
        <<"position">> => Position
    }.

internal_member(UserId, Seq, Roles) ->
    #{
        <<"user">> => #{
            <<"id">> => UserId,
            <<"username">> => username(Seq),
            <<"global_name">> => display_name(Seq),
            <<"bot">> => Seq rem 37 =:= 0
        },
        <<"nick">> => nick(Seq),
        <<"roles">> => Roles,
        <<"joined_at">> => joined_at(Seq)
    }.

event_member(UserId, Seq, Roles) ->
    #{
        <<"user">> => #{
            <<"id">> => integer_to_binary(UserId),
            <<"username">> => username(Seq),
            <<"global_name">> => display_name(Seq),
            <<"bot">> => Seq rem 37 =:= 0
        },
        <<"nick">> => nick(Seq),
        <<"roles">> => [integer_to_binary(RoleId) || RoleId <- Roles],
        <<"joined_at">> => joined_at(Seq)
    }.

updated_member(UserId, Seq) ->
    Base = event_member(UserId, 7000 + Seq, update_roles(Seq)),
    Base#{<<"nick">> => <<"renamed_", (integer_to_binary(Seq))/binary>>}.

roles_for_member(Seq) ->
    roles_from_flags(Seq rem 5 =:= 0, Seq rem 7 =:= 0).

roles_for_user_id(UserId) ->
    roles_for_member(UserId - 100000).

add_roles(UserId) ->
    Seq = UserId - 200000,
    roles_from_flags(Seq rem 4 =:= 0, Seq rem 6 =:= 0).

update_roles(Seq) ->
    roles_from_flags(Seq rem 3 =:= 0, Seq rem 4 =:= 0).

roles_from_flags(HasHoisted, HasPrivate) ->
    MaybeHoisted =
        case HasHoisted of
            true -> [?HOISTED_ROLE_ID];
            false -> []
        end,
    MaybePrivate =
        case HasPrivate of
            true -> [?PRIVATE_ROLE_ID];
            false -> []
        end,
    MaybeHoisted ++ MaybePrivate.

username(Seq) ->
    iolist_to_binary(io_lib:format("member_~6..0B", [Seq])).

display_name(Seq) ->
    iolist_to_binary(io_lib:format("Member ~6..0B", [Seq])).

nick(Seq) when Seq rem 4 =:= 0 ->
    iolist_to_binary(io_lib:format("Team ~6..0B", [Seq]));
nick(_Seq) ->
    null.

joined_at(Seq) ->
    Day = (Seq rem 28) + 1,
    iolist_to_binary(io_lib:format("2026-05-~2..0BT12:00:00Z", [Day])).

session_id(I) ->
    <<"s", (integer_to_binary(I))/binary>>.

zip3([A | As], [B | Bs], [C | Cs]) ->
    [{A, B, C} | zip3(As, Bs, Cs)];
zip3([], [], []) ->
    [].
