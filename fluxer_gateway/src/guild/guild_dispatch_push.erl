%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_dispatch_push).
-typing([eqwalizer]).

-export([
    maybe_send_push_notifications/4,
    collect_and_send_push_notifications/3
]).

-define(MAX_FORMAT_MEMBERS, 50).
-define(CONCURRENCY_LIMIT_KEY, guild_push_concurrency_limit).
-define(DEFAULT_PUSH_CONCURRENCY, 2).
-define(MAX_PUSH_CONCURRENCY, 8).
-define(PUSH_WORKER_MAX_AGE_MS, 60000).
-define(PUSH_COUNTERS, guild_push_counters).
-define(PUSH_COUNTER_KEYS, [
    worker_started,
    worker_completed,
    worker_failed,
    dropped_at_limit,
    spawn_failed,
    slot_reclaimed,
    slot_deduped,
    members_table_missing,
    scan_table_unavailable
]).

-type event() :: atom().
-type event_data() :: map().
-type guild_state() :: map().
-type guild_id() :: integer().
-type user_id() :: integer().
-type push_worker() :: {integer(), pid(), integer()}.
-export_type([event/0, event_data/0, guild_state/0, guild_id/0]).

-spec maybe_send_push_notifications(event(), event_data(), guild_id(), guild_state()) -> ok.
maybe_send_push_notifications(message_create, FinalData, GuildId, UpdatedState) ->
    case maps:get(disable_push_notifications, UpdatedState, false) of
        true -> ok;
        false -> maybe_spawn_push(FinalData, GuildId, UpdatedState)
    end;
maybe_send_push_notifications(_Event, _FinalData, _GuildId, _UpdatedState) ->
    ok.

-spec maybe_spawn_push(event_data(), guild_id(), guild_state()) -> ok.
maybe_spawn_push(FinalData, GuildId, UpdatedState) ->
    Limit = push_concurrency_limit(),
    Workers = live_push_workers(),
    put(push_inflight_workers, Workers),
    case length(Workers) < Limit of
        true -> spawn_push(FinalData, GuildId, UpdatedState);
        false -> count_push_event(dropped_at_limit)
    end.

-spec local_process_alive(pid()) -> boolean().
local_process_alive(Pid) ->
    node(Pid) =:= node() andalso erlang:is_process_alive(Pid).

-spec push_concurrency_limit() -> pos_integer().
push_concurrency_limit() ->
    case application:get_env(fluxer_gateway, ?CONCURRENCY_LIMIT_KEY, undefined) of
        Value when is_integer(Value), Value > 0 -> min(Value, ?MAX_PUSH_CONCURRENCY);
        _ -> ?DEFAULT_PUSH_CONCURRENCY
    end.

-spec live_push_workers() -> [push_worker()].
live_push_workers() ->
    Now = erlang:monotonic_time(millisecond),
    prune_push_workers(tracked_push_workers(Now), Now, #{}, []).

-spec tracked_push_workers(integer()) -> [term()].
tracked_push_workers(Now) ->
    merge_inflight_push_worker(get(push_inflight), tracked_worker_list(), Now).

-spec tracked_worker_list() -> [term()].
tracked_worker_list() ->
    case get(push_inflight_workers) of
        Workers when is_list(Workers) -> Workers;
        _ -> []
    end.

-spec merge_inflight_push_worker(term(), [term()], integer()) -> [term()].
merge_inflight_push_worker(Pid, Workers, Now) when is_pid(Pid) ->
    case lists:any(fun(Worker) -> push_worker_pid(Worker) =:= Pid end, Workers) of
        true -> Workers;
        false -> Workers ++ [{0, Pid, Now}]
    end;
merge_inflight_push_worker(_Inflight, Workers, _Now) ->
    Workers.

-spec push_worker_pid(term()) -> pid() | undefined.
push_worker_pid({_Gen, Pid, _StartedMs}) when is_pid(Pid) -> Pid;
push_worker_pid(_Worker) -> undefined.

-spec prune_push_workers([term()], integer(), map(), [push_worker()]) -> [push_worker()].
prune_push_workers([], _Now, _Seen, Acc) ->
    lists:reverse(Acc);
prune_push_workers([Worker | Rest], Now, Seen, Acc) ->
    case keep_push_worker(Worker, Now, Seen) of
        {keep, {_Gen, Pid, _StartedMs} = Kept} ->
            prune_push_workers(Rest, Now, Seen#{Pid => true}, [Kept | Acc]);
        drop ->
            prune_push_workers(Rest, Now, Seen, Acc)
    end.

-spec keep_push_worker(term(), integer(), map()) -> {keep, push_worker()} | drop.
keep_push_worker({Gen, Pid, StartedMs} = Worker, Now, Seen) when
    is_integer(Gen), is_pid(Pid), is_integer(StartedMs)
->
    case maps:is_key(Pid, Seen) of
        true -> drop_duplicate_push_worker();
        false -> keep_live_push_worker(Worker, Now - StartedMs, local_process_alive(Pid))
    end;
keep_push_worker(_Worker, _Now, _Seen) ->
    drop.

-spec drop_duplicate_push_worker() -> drop.
drop_duplicate_push_worker() ->
    count_push_event(slot_deduped),
    drop.

-spec keep_live_push_worker(push_worker(), integer(), boolean()) ->
    {keep, push_worker()} | drop.
keep_live_push_worker(_Worker, _AgeMs, false) ->
    drop;
keep_live_push_worker(Worker, AgeMs, true) when AgeMs < ?PUSH_WORKER_MAX_AGE_MS ->
    {keep, Worker};
keep_live_push_worker(_Worker, _AgeMs, true) ->
    count_push_event(slot_reclaimed),
    drop.

-spec note_push_worker(pid()) -> ok.
note_push_worker(Pid) ->
    Gen = erlang:unique_integer([monotonic, positive]),
    StartedMs = erlang:monotonic_time(millisecond),
    put(push_inflight_workers, [{Gen, Pid, StartedMs} | tracked_worker_list()]),
    ok.

-spec count_push_event(atom()) -> ok.
count_push_event(Key) ->
    bump_push_counter(Key, true).

-spec bump_push_counter(atom(), boolean()) -> ok.
bump_push_counter(Key, Retry) ->
    try ets:update_counter(?PUSH_COUNTERS, Key, {2, 1}) of
        _ -> ok
    catch
        error:badarg -> retry_push_counter(Key, Retry)
    end.

-spec retry_push_counter(atom(), boolean()) -> ok.
retry_push_counter(Key, true) ->
    ok = ensure_push_counters(),
    bump_push_counter(Key, false);
retry_push_counter(_Key, false) ->
    ok.

-spec ensure_push_counters() -> ok.
ensure_push_counters() ->
    case ets:whereis(?PUSH_COUNTERS) of
        undefined -> create_push_counters();
        _ -> ensure_push_counter_keys()
    end.

-spec create_push_counters() -> ok.
create_push_counters() ->
    ok = guild_ets_utils:ensure_table(?PUSH_COUNTERS, push_counter_table_options()),
    ensure_push_counter_keys().

-spec push_counter_table_options() -> [term()].
push_counter_table_options() ->
    [named_table, public, set, {read_concurrency, true}, {write_concurrency, true}].

-spec ensure_push_counter_keys() -> ok.
ensure_push_counter_keys() ->
    lists:foreach(fun ensure_push_counter_key/1, ?PUSH_COUNTER_KEYS).

-spec ensure_push_counter_key(atom()) -> ok.
ensure_push_counter_key(Key) ->
    try
        _ = ets:insert_new(?PUSH_COUNTERS, {Key, 0}),
        ok
    catch
        error:badarg -> ok
    end.

-spec spawn_push(event_data(), guild_id(), guild_state()) -> ok.
spawn_push(FinalData, GuildId, UpdatedState) ->
    Data = maps:get(data, UpdatedState, #{}),
    case maps:get(members_ets, Data, undefined) of
        MembersTab when is_reference(MembersTab) ->
            CompactState = compact_push_state(
                eqwalizer:dynamic_cast(MembersTab), Data, GuildId, UpdatedState
            ),
            spawn_compact_push(FinalData, GuildId, CompactState);
        _ ->
            missing_members_table(FinalData, GuildId, UpdatedState)
    end.

-spec compact_push_state(ets:tid(), map(), guild_id(), guild_state()) -> guild_state().
compact_push_state(MembersTab, Data, GuildId, UpdatedState) ->
    Sessions = maps:get(sessions, UpdatedState, #{}),
    #{
        id => maps:get(id, UpdatedState, GuildId),
        data => compact_push_data(Data),
        virtual_channel_access => maps:get(virtual_channel_access, UpdatedState, #{}),
        members_ets => MembersTab,
        member_presence => maps:get(member_presence, UpdatedState, undefined),
        session_eligibility => build_push_session_eligibility(Sessions, UpdatedState),
        member_count => maps:get(member_count, UpdatedState, undefined)
    }.

-spec compact_push_data(map()) -> map().
compact_push_data(Data) ->
    maps:with(
        [
            <<"guild">>,
            <<"roles">>,
            <<"role_index">>,
            role_perms_cache,
            <<"channels">>,
            <<"channel_index">>,
            overwrite_perms_cache,
            members_ets
        ],
        Data
    ).

-spec spawn_compact_push(event_data(), guild_id(), guild_state()) -> ok.
spawn_compact_push(FinalData, GuildId, CompactState) ->
    spawn_push_worker(
        fun() ->
            collect_and_send_compact_push_notifications(FinalData, GuildId, CompactState)
        end,
        GuildId
    ).

-spec missing_members_table(event_data(), guild_id(), guild_state()) -> ok.
missing_members_table(FinalData, GuildId, UpdatedState) ->
    count_push_event(members_table_missing),
    logger:warning(
        "guild_push_members_table_unavailable: guild_id=~p phase=spawn",
        [GuildId]
    ),
    spawn_legacy_push(FinalData, GuildId, UpdatedState).

-spec spawn_legacy_push(event_data(), guild_id(), guild_state()) -> ok.
spawn_legacy_push(FinalData, GuildId, UpdatedState) ->
    LegacyState = legacy_push_state(GuildId, UpdatedState),
    spawn_push_worker(
        fun() ->
            collect_and_send_push_notifications(FinalData, GuildId, LegacyState)
        end,
        GuildId
    ).

-spec legacy_push_state(guild_id(), guild_state()) -> guild_state().
legacy_push_state(GuildId, UpdatedState) ->
    #{
        id => maps:get(id, UpdatedState, GuildId),
        data => maps:get(data, UpdatedState, #{}),
        sessions => maps:get(sessions, UpdatedState, #{}),
        member_presence => maps:get(member_presence, UpdatedState, undefined),
        virtual_channel_access => maps:get(virtual_channel_access, UpdatedState, #{}),
        member_count => maps:get(member_count, UpdatedState, undefined)
    }.

-spec spawn_push_worker(fun(() -> ok), guild_id()) -> ok.
spawn_push_worker(Worker, GuildId) ->
    Counted = fun() -> run_counted_push_worker(Worker) end,
    case try_spawn_push_worker(Counted, GuildId) of
        {ok, Pid} ->
            put(push_inflight, Pid),
            note_push_worker(Pid),
            count_push_event(worker_started);
        error ->
            count_push_event(spawn_failed)
    end.

-spec run_counted_push_worker(fun(() -> ok)) -> ok.
run_counted_push_worker(Worker) ->
    try Worker() of
        _ -> count_push_event(worker_completed)
    catch
        _Class:_Reason -> count_push_event(worker_failed)
    end.

-spec try_spawn_push_worker(fun(() -> ok), guild_id()) -> {ok, pid()} | error.
try_spawn_push_worker(Worker, GuildId) ->
    try
        {ok, spawn(fun() -> run_deprioritised_push_worker(Worker) end)}
    catch
        error:system_limit ->
            logger:warning(
                "guild_push_worker_spawn_failed: guild_id=~p reason=system_limit",
                [GuildId]
            ),
            error
    end.

-spec run_deprioritised_push_worker(fun(() -> ok)) -> ok.
run_deprioritised_push_worker(Worker) ->
    ok = apply_push_worker_priority(),
    Worker().

-spec apply_push_worker_priority() -> ok.
apply_push_worker_priority() ->
    _ = erlang:process_flag(priority, low),
    ok.

-spec large_guild_meta(guild_state()) -> map() | undefined.
large_guild_meta(State) ->
    case maps:get(member_count, State, undefined) of
        MemberCount when is_integer(MemberCount), MemberCount >= 0 ->
            #{member_count => MemberCount, features => guild_features(State)};
        _ ->
            undefined
    end.

-spec guild_features(guild_state()) -> [binary()].
guild_features(State) ->
    Data = maps:get(data, State, #{}),
    Guild = maps:get(<<"guild">>, Data, #{}),
    case maps:get(<<"features">>, Guild, []) of
        Features when is_list(Features) -> Features;
        _ -> []
    end.

-spec collect_and_send_compact_push_notifications(event_data(), guild_id(), guild_state()) ->
    ok.
collect_and_send_compact_push_notifications(MessageData, GuildId, State) ->
    case guild_dispatch_config:should_send_push_notifications(State) of
        false -> ok;
        true -> send_compact_push_notifications(MessageData, GuildId, State)
    end.

-spec send_compact_push_notifications(event_data(), guild_id(), guild_state()) -> ok.
send_compact_push_notifications(MessageData, GuildId, State) ->
    ChannelIdBin = maps:get(<<"channel_id">>, MessageData, undefined),
    case guild_dispatch_decorate:parse_snowflake(<<"channel_id">>, ChannelIdBin) of
        undefined -> ok;
        ChannelId -> scan_and_send_compact_push(MessageData, GuildId, ChannelId, State)
    end.

-spec scan_and_send_compact_push(event_data(), guild_id(), integer(), guild_state()) -> ok.
scan_and_send_compact_push(MessageData, GuildId, ChannelId, State) ->
    Context = compact_scan_context(MessageData, ChannelId, State),
    MembersTab = eqwalizer:dynamic_cast(maps:get(members_ets, State)),
    case scan_push_members(MembersTab, Context, State) of
        {ok, #{eligible_user_ids := []}} ->
            ok;
        {ok, Scan} ->
            send_compact_scanned_push(MessageData, GuildId, Scan, State);
        {error, table_unavailable} ->
            scan_table_unavailable(GuildId)
    end.

-spec scan_table_unavailable(guild_id()) -> ok.
scan_table_unavailable(GuildId) ->
    count_push_event(scan_table_unavailable),
    logger:warning(
        "guild_push_members_table_unavailable: guild_id=~p phase=scan",
        [GuildId]
    ),
    ok.

-spec compact_scan_context(event_data(), integer(), guild_state()) -> map().
compact_scan_context(MessageData, ChannelId, State) ->
    MentionRoles = maps:get(<<"mention_roles">>, MessageData, []),
    #{
        mention_everyone => maps:get(<<"mention_everyone">>, MessageData, false) =:= true,
        direct_mentions => direct_mention_id_set(MessageData),
        mention_roles => mention_role_id_set(MentionRoles),
        format_members => format_member_id_set(MessageData),
        channel_id => ChannelId,
        session_eligibility => maps:get(session_eligibility, State)
    }.

-spec scan_push_members(ets:tid(), map(), guild_state()) ->
    {ok, map()} | {error, table_unavailable}.
scan_push_members(MembersTab, Context, State) ->
    try
        {ok, fold_push_members(member_id_snapshot(MembersTab), MembersTab, Context, State)}
    catch
        error:badarg -> {error, table_unavailable}
    end.

-spec initial_scan_acc() -> map().
initial_scan_acc() ->
    #{eligible_user_ids => [], user_roles => #{}, format_members => #{}}.

-spec member_id_snapshot(ets:tid()) -> [user_id()].
member_id_snapshot(MembersTab) ->
    ets:select(MembersTab, [{{'$1', '_'}, [], ['$1']}]).

-spec fold_push_members([user_id()], ets:tid(), map(), guild_state()) -> map().
fold_push_members(UserIds, MembersTab, Context, State) ->
    lists:foldl(
        fun(UserId, Acc) -> scan_push_member_id(UserId, MembersTab, Context, State, Acc) end,
        initial_scan_acc(),
        UserIds
    ).

-spec scan_push_member_id(user_id(), ets:tid(), map(), guild_state(), map()) -> map().
scan_push_member_id(UserId, MembersTab, Context, State, Acc) ->
    case ets:lookup(MembersTab, UserId) of
        [Row] -> scan_push_member(Row, Context, State, Acc);
        _ -> Acc
    end.

-spec scan_push_member(term(), map(), guild_state(), map()) -> map().
scan_push_member({UserId, Member}, Context, State, Acc) when
    is_integer(UserId), UserId > 0, is_map(Member)
->
    Acc1 = maybe_collect_format_member(UserId, Member, Context, Acc),
    case scanned_member_is_eligible(UserId, Member, Context, State) of
        true -> add_scanned_member(UserId, Member, Acc1);
        false -> Acc1
    end;
scan_push_member(_Row, _Context, _State, Acc) ->
    Acc.

-spec scanned_member_is_eligible(user_id(), map(), map(), guild_state()) -> boolean().
scanned_member_is_eligible(UserId, Member, Context, State) ->
    is_push_candidate(UserId, Member, Context) andalso
        guild_permissions:can_view_channel(
            UserId, maps:get(channel_id, Context), Member, State
        ).

-spec add_scanned_member(user_id(), map(), map()) -> map().
add_scanned_member(UserId, Member, Acc) ->
    Eligible = maps:get(eligible_user_ids, Acc),
    UserRoles = maps:get(user_roles, Acc),
    Acc#{
        eligible_user_ids := [UserId | Eligible],
        user_roles := UserRoles#{UserId => extract_role_ids(Member)}
    }.

-spec maybe_collect_format_member(user_id(), map(), map(), map()) -> map().
maybe_collect_format_member(UserId, Member, Context, Acc) ->
    case maps:is_key(UserId, maps:get(format_members, Context)) of
        true ->
            Members = maps:get(format_members, Acc),
            Acc#{format_members := Members#{UserId => Member}};
        false ->
            Acc
    end.

-spec is_push_candidate(user_id(), map(), map()) -> boolean().
is_push_candidate(UserId, Member, Context) ->
    maps:get(UserId, maps:get(session_eligibility, Context), true) orelse
        (not mentions_respect_active_session() andalso
            is_mention_candidate(UserId, Member, Context)).

-spec is_mention_candidate(user_id(), map(), map()) -> boolean().
is_mention_candidate(UserId, Member, Context) ->
    maps:get(mention_everyone, Context) orelse
        maps:is_key(UserId, maps:get(direct_mentions, Context)) orelse
        member_has_mentioned_role(Member, maps:get(mention_roles, Context)).

-spec mentions_respect_active_session() -> boolean().
mentions_respect_active_session() ->
    application:get_env(fluxer_gateway, push_mentions_respect_active_session, true) =:= true.

-spec send_compact_scanned_push(event_data(), guild_id(), map(), guild_state()) -> ok.
send_compact_scanned_push(MessageData, GuildId, Scan, State) ->
    FormatData = compact_format_data(maps:get(data, State), maps:get(format_members, Scan)),
    send_push_to_eligible_users(
        MessageData,
        GuildId,
        lists:reverse(maps:get(eligible_user_ids, Scan)),
        maps:get(user_roles, Scan),
        maps:get(session_eligibility, State),
        FormatData,
        large_guild_meta(State)
    ).

-spec compact_format_data(map(), map()) -> map().
compact_format_data(Data, FormatMembers) ->
    Data#{
        <<"members">> => FormatMembers,
        members_normalized => FormatMembers,
        members_sorted_ids => lists:sort(maps:keys(FormatMembers)),
        <<"member_role_index">> =>
            guild_data_index_members:build_member_role_index(FormatMembers)
    }.

-spec direct_mention_id_set(event_data()) -> map().
direct_mention_id_set(MessageData) ->
    lists:foldl(
        fun add_direct_mention_id/2,
        #{},
        maps:get(<<"mentions">>, MessageData, [])
    ).

-spec add_direct_mention_id(term(), map()) -> map().
add_direct_mention_id(Mention, Acc) ->
    case direct_mention_user_id(Mention) of
        {true, UserId} -> Acc#{UserId => true};
        false -> Acc
    end.

-spec format_member_id_set(event_data()) -> map().
format_member_id_set(MessageData) ->
    AuthorSet = author_format_id_set(maps:get(<<"author">>, MessageData, undefined)),
    collect_format_mention_ids(
        maps:get(<<"mentions">>, MessageData, []),
        ?MAX_FORMAT_MEMBERS,
        AuthorSet
    ).

-spec author_format_id_set(term()) -> map().
author_format_id_set(Author) when is_map(Author) ->
    case direct_mention_user_id(Author) of
        {true, UserId} -> #{UserId => true};
        false -> #{}
    end;
author_format_id_set(_Author) ->
    #{}.

-spec collect_format_mention_ids([term()], non_neg_integer(), map()) -> map().
collect_format_mention_ids(_Mentions, 0, Acc) ->
    Acc;
collect_format_mention_ids([], _Remaining, Acc) ->
    Acc;
collect_format_mention_ids([Mention | Rest], Remaining, Acc) ->
    case direct_mention_user_id(Mention) of
        {true, UserId} -> collect_format_mention_id(UserId, Rest, Remaining, Acc);
        false -> collect_format_mention_ids(Rest, Remaining, Acc)
    end.

-spec collect_format_mention_id(user_id(), [term()], non_neg_integer(), map()) -> map().
collect_format_mention_id(UserId, Rest, Remaining, Acc) ->
    case maps:is_key(UserId, Acc) of
        true -> collect_format_mention_ids(Rest, Remaining, Acc);
        false -> collect_format_mention_ids(Rest, Remaining - 1, Acc#{UserId => true})
    end.

-spec collect_and_send_push_notifications(event_data(), guild_id(), guild_state()) -> ok.
collect_and_send_push_notifications(MessageData, GuildId, State) ->
    case guild_dispatch_config:should_send_push_notifications(State) of
        false -> ok;
        true -> send_push_notifications(MessageData, GuildId, State)
    end.

-spec send_push_notifications(event_data(), guild_id(), guild_state()) -> ok.
send_push_notifications(MessageData, GuildId, State) ->
    Data = maps:get(data, State),
    Members = guild_data_index:member_map(Data),
    Sessions = maps:get(sessions, State, #{}),
    SessionEligibility = build_push_session_eligibility(Sessions, State),
    CandidateUserIds = push_candidate_user_ids(Members, SessionEligibility, MessageData),
    ChannelIdBin = maps:get(<<"channel_id">>, MessageData, undefined),
    case guild_dispatch_decorate:parse_snowflake(<<"channel_id">>, ChannelIdBin) of
        undefined ->
            ok;
        ChannelId ->
            send_to_eligible(
                MessageData,
                GuildId,
                Members,
                CandidateUserIds,
                ChannelId,
                SessionEligibility,
                Data,
                State
            )
    end.

-spec send_to_eligible(
    event_data(),
    guild_id(),
    map(),
    [user_id()],
    integer(),
    map(),
    map(),
    guild_state()
) -> ok.
send_to_eligible(
    MessageData,
    GuildId,
    Members,
    CandidateUserIds,
    ChannelId,
    SessionEligibility,
    Data,
    State
) ->
    case find_eligible_users_for_push(Members, CandidateUserIds, ChannelId, State) of
        [] ->
            ok;
        EligibleUserIds ->
            UserRolesMap = build_user_roles_map(Members, EligibleUserIds),
            send_push_to_eligible_users(
                MessageData,
                GuildId,
                EligibleUserIds,
                UserRolesMap,
                SessionEligibility,
                Data,
                large_guild_meta(State)
            )
    end.

-spec push_candidate_user_ids(map(), map(), event_data()) -> [user_id()].
push_candidate_user_ids(Members, SessionEligibility, MessageData) ->
    push_candidate_user_ids(
        maps:get(<<"mention_everyone">>, MessageData, false),
        Members,
        SessionEligibility,
        MessageData
    ).

-spec push_candidate_user_ids(term(), map(), map(), event_data()) -> [user_id()].
push_candidate_user_ids(true, Members, SessionEligibility, _MessageData) ->
    {CandidateUserIds, _Seen} = base_candidate_acc(Members, SessionEligibility),
    lists:reverse(CandidateUserIds);
push_candidate_user_ids(_MentionEveryone, Members, SessionEligibility, MessageData) ->
    BaseAcc = base_candidate_acc(Members, SessionEligibility),
    {CandidateUserIds, _Seen} =
        add_mentioned_candidate_user_ids(Members, MessageData, BaseAcc),
    lists:reverse(CandidateUserIds).

-spec base_candidate_acc(map(), map()) -> {[user_id()], map()}.
base_candidate_acc(Members, SessionEligibility) ->
    maps:fold(
        fun(UserId, _Member, Acc) ->
            maybe_add_session_candidate(UserId, SessionEligibility, Acc)
        end,
        {[], #{}},
        Members
    ).

-spec maybe_add_session_candidate(user_id(), map(), {[user_id()], map()}) ->
    {[user_id()], map()}.
maybe_add_session_candidate(UserId, SessionEligibility, Acc) ->
    case maps:get(UserId, SessionEligibility, true) of
        true -> add_candidate_user_id(UserId, Acc);
        false -> Acc
    end.

-spec add_candidate_user_id(user_id(), {[user_id()], map()}) -> {[user_id()], map()}.
add_candidate_user_id(UserId, {UserIds, Seen} = Acc) ->
    case maps:is_key(UserId, Seen) of
        true -> Acc;
        false -> {[UserId | UserIds], Seen#{UserId => true}}
    end.

-spec add_mentioned_candidate_user_ids(map(), event_data(), {[user_id()], map()}) ->
    {[user_id()], map()}.
add_mentioned_candidate_user_ids(Members, MessageData, Acc) ->
    Acc1 = add_direct_mention_user_ids(MessageData, Acc),
    add_role_mention_user_ids(Members, MessageData, Acc1).

-spec add_direct_mention_user_ids(event_data(), {[user_id()], map()}) ->
    {[user_id()], map()}.
add_direct_mention_user_ids(MessageData, Acc) ->
    lists:foldl(
        fun add_direct_mention_user/2,
        Acc,
        maps:get(<<"mentions">>, MessageData, [])
    ).

-spec add_direct_mention_user(term(), {[user_id()], map()}) -> {[user_id()], map()}.
add_direct_mention_user(Mention, Acc) ->
    case direct_mention_user_id(Mention) of
        {true, UserId} -> add_candidate_user_id(UserId, Acc);
        false -> Acc
    end.

-spec direct_mention_user_id(term()) -> {true, user_id()} | false.
direct_mention_user_id(Mention) when is_map(Mention) ->
    case
        validation:validate_snowflake(<<"mention.id">>, maps:get(<<"id">>, Mention, undefined))
    of
        {ok, UserId} -> {true, UserId};
        _ -> false
    end;
direct_mention_user_id(_) ->
    false.

-spec add_role_mention_user_ids(map(), event_data(), {[user_id()], map()}) ->
    {[user_id()], map()}.
add_role_mention_user_ids(Members, MessageData, Acc) ->
    MentionRoleSet = mention_role_id_set(maps:get(<<"mention_roles">>, MessageData, [])),
    add_role_mention_user_ids_for_set(MentionRoleSet, Members, Acc).

-spec add_role_mention_user_ids_for_set(map(), map(), {[user_id()], map()}) ->
    {[user_id()], map()}.
add_role_mention_user_ids_for_set(MentionRoleSet, _Members, Acc) when
    map_size(MentionRoleSet) =:= 0
->
    Acc;
add_role_mention_user_ids_for_set(MentionRoleSet, Members, Acc) ->
    maps:fold(
        fun(UserId, Member, AccIn) ->
            maybe_add_role_mention_user(UserId, Member, MentionRoleSet, AccIn)
        end,
        Acc,
        Members
    ).

-spec maybe_add_role_mention_user(user_id(), map(), map(), {[user_id()], map()}) ->
    {[user_id()], map()}.
maybe_add_role_mention_user(UserId, Member, MentionRoleSet, Acc) ->
    HasMentionedRole = member_has_mentioned_role(Member, MentionRoleSet),
    case HasMentionedRole of
        true -> add_candidate_user_id(UserId, Acc);
        false -> Acc
    end.

-spec mention_role_id_set(list()) -> map().
mention_role_id_set(MentionRoles) ->
    lists:foldl(fun add_mention_role_id/2, #{}, MentionRoles).

-spec add_mention_role_id(term(), map()) -> map().
add_mention_role_id(RoleId, Acc) ->
    case snowflake_id:parse_optional(RoleId) of
        Id when is_integer(Id), Id > 0 -> Acc#{Id => true};
        _ -> Acc
    end.

-spec member_has_mentioned_role(map(), map()) -> boolean().
member_has_mentioned_role(Member, MentionRoleSet) ->
    member_roles_include_mentioned(maps:get(<<"roles">>, Member, []), MentionRoleSet).

-spec member_roles_include_mentioned(list(), map()) -> boolean().
member_roles_include_mentioned([], _MentionRoleSet) ->
    false;
member_roles_include_mentioned([Role | Rest], MentionRoleSet) ->
    case snowflake_id:parse_optional(Role) of
        RoleId when is_integer(RoleId), RoleId > 0 ->
            maps:is_key(RoleId, MentionRoleSet) orelse
                member_roles_include_mentioned(Rest, MentionRoleSet);
        _ ->
            member_roles_include_mentioned(Rest, MentionRoleSet)
    end.

-spec find_eligible_users_for_push(map(), [user_id()], integer(), guild_state()) -> [user_id()].
find_eligible_users_for_push(Members, CandidateUserIds, ChannelId, State) ->
    lists:filtermap(
        fun(UserId) -> is_push_eligible(UserId, Members, ChannelId, State) end,
        CandidateUserIds
    ).

-spec is_push_eligible(user_id(), map(), integer(), guild_state()) -> {true, user_id()} | false.
is_push_eligible(UserId, Members, ChannelId, State) ->
    case maps:get(UserId, Members, undefined) of
        undefined ->
            false;
        Member ->
            view_to_filtermap(UserId, ChannelId, Member, State)
    end.

-spec view_to_filtermap(
    user_id(), integer(), map(), guild_state()
) -> {true, user_id()} | false.
view_to_filtermap(UserId, ChannelId, Member, State) ->
    case guild_permissions:can_view_channel(UserId, ChannelId, Member, State) of
        true -> {true, UserId};
        false -> false
    end.

-spec build_push_session_eligibility(map(), guild_state()) -> #{user_id() => boolean()}.
build_push_session_eligibility(Sessions, State) ->
    case presence_eligibility_enabled() of
        true -> build_push_presence_eligibility(Sessions, State);
        false -> build_push_session_eligibility(Sessions)
    end.

-spec presence_eligibility_enabled() -> boolean().
presence_eligibility_enabled() ->
    application:get_env(fluxer_gateway, push_presence_eligibility, true) =:= true.

-spec build_push_presence_eligibility(map(), guild_state()) -> #{user_id() => boolean()}.
build_push_presence_eligibility(Sessions, State) ->
    Presences = maps:get(member_presence, State, undefined),
    maps:fold(
        fun(_Sid, Session, Acc) ->
            accumulate_presence_eligibility(Session, Presences, Acc)
        end,
        #{},
        Sessions
    ).

-spec accumulate_presence_eligibility(map(), term(), #{user_id() => boolean()}) ->
    #{user_id() => boolean()}.
accumulate_presence_eligibility(Session, Presences, Acc) ->
    case maps:get(user_id, Session, undefined) of
        UserId when is_integer(UserId) ->
            Acc#{
                UserId =>
                    maps:get(UserId, Acc, true) andalso
                    not actively_engaged(UserId, Presences)
            };
        _ ->
            Acc
    end.

-spec actively_engaged(user_id(), term()) -> boolean().
actively_engaged(_UserId, undefined) ->
    false;
actively_engaged(UserId, Presences) ->
    case lookup_presence_safe(UserId, Presences) of
        undefined -> false;
        Presence -> presence_is_active(Presence)
    end.

-spec lookup_presence_safe(user_id(), term()) -> map() | undefined.
lookup_presence_safe(UserId, Presences) ->
    try guild_state_member:lookup_presence(Presences, UserId) of
        Presence -> Presence
    catch
        error:badarg -> undefined
    end.

-spec presence_is_active(map()) -> boolean().
presence_is_active(Presence) ->
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    Afk = maps:get(<<"afk">>, Presence, false) =:= true,
    Mobile = maps:get(<<"mobile">>, Presence, false) =:= true,
    is_engaged_status(Status) andalso not Afk andalso not Mobile.

-spec is_engaged_status(term()) -> boolean().
is_engaged_status(<<"online">>) -> true;
is_engaged_status(<<"dnd">>) -> true;
is_engaged_status(_Status) -> false.

-spec build_push_session_eligibility(map()) -> #{user_id() => boolean()}.
build_push_session_eligibility(Sessions) ->
    maps:fold(
        fun(_Sid, Session, Acc) ->
            accumulate_session_eligibility(Session, Acc)
        end,
        #{},
        Sessions
    ).

-spec accumulate_session_eligibility(map(), #{user_id() => boolean()}) ->
    #{user_id() => boolean()}.
accumulate_session_eligibility(Session, Acc) ->
    case maps:get(user_id, Session, undefined) of
        UserId when is_integer(UserId) ->
            Acc#{
                UserId =>
                    maps:get(UserId, Acc, true) andalso maps:get(afk, Session, false)
            };
        _ ->
            Acc
    end.

-spec send_push_to_eligible_users(
    event_data(), guild_id(), [user_id()], map(), map(), map(), map() | undefined
) -> ok.
send_push_to_eligible_users(
    MessageData,
    GuildId,
    EligibleUserIds,
    UserRolesMap,
    ConnectedUsers,
    Data,
    LargeGuildMeta
) ->
    AuthorIdBin = maps:get(<<"id">>, maps:get(<<"author">>, MessageData, #{}), undefined),
    case guild_dispatch_decorate:parse_snowflake(<<"author.id">>, AuthorIdBin) of
        undefined ->
            ok;
        AuthorId ->
            ChannelIdBin = maps:get(<<"channel_id">>, MessageData),
            ChannelName = find_channel_name(ChannelIdBin, Data),
            RoleNames = build_role_names_map(Data),
            do_send_push(
                MessageData,
                GuildId,
                EligibleUserIds,
                UserRolesMap,
                ConnectedUsers,
                ChannelName,
                RoleNames,
                Data,
                AuthorId,
                LargeGuildMeta
            )
    end.

-spec do_send_push(
    event_data(),
    guild_id(),
    [user_id()],
    map(),
    map(),
    binary(),
    map(),
    map(),
    integer(),
    map() | undefined
) -> ok.
do_send_push(
    MessageData,
    GuildId,
    EligibleUserIds,
    UserRolesMap,
    ConnectedUsers,
    ChannelName,
    RoleNames,
    Data,
    AuthorId,
    LargeGuildMeta
) ->
    Guild = maps:get(<<"guild">>, Data),
    DefaultMessageNotifications = maps:get(<<"default_message_notifications">>, Guild, 0),
    GuildName = maps:get(<<"name">>, Guild, <<"Unknown">>),
    push:handle_message_create(#{
        message_data => MessageData,
        user_ids => EligibleUserIds,
        guild_id => GuildId,
        author_id => AuthorId,
        guild_default_notifications => DefaultMessageNotifications,
        guild_name => GuildName,
        channel_name => ChannelName,
        role_names => RoleNames,
        markdown_context =>
            push_notification_format:build_markdown_context(
                MessageData, GuildId, RoleNames, Data
            ),
        user_roles => UserRolesMap,
        connected_users => ConnectedUsers,
        guild_member_count => meta_member_count(LargeGuildMeta),
        guild_features => meta_features(LargeGuildMeta)
    }).

-spec meta_member_count(map() | undefined) -> non_neg_integer() | undefined.
meta_member_count(#{member_count := MemberCount}) -> MemberCount;
meta_member_count(_Meta) -> undefined.

-spec meta_features(map() | undefined) -> [binary()] | undefined.
meta_features(#{features := Features}) -> Features;
meta_features(_Meta) -> undefined.

-spec find_channel_name(binary(), map()) -> binary().
find_channel_name(ChannelIdBin, Data) ->
    case guild_dispatch_decorate:parse_snowflake(<<"channel_id">>, ChannelIdBin) of
        undefined ->
            <<"unknown">>;
        ChannelId ->
            lookup_channel_name(ChannelId, Data)
    end.

-spec lookup_channel_name(integer(), map()) -> binary().
lookup_channel_name(ChannelId, Data) ->
    Channels = guild_data_index:channel_index(Data),
    case maps:get(ChannelId, Channels, undefined) of
        undefined -> <<"unknown">>;
        Channel -> maps:get(<<"name">>, Channel, <<"unknown">>)
    end.

-spec build_role_names_map(map()) -> #{integer() => binary()}.
build_role_names_map(Data) ->
    maps:fold(
        fun add_role_name/3,
        #{},
        guild_data_index:role_index(Data)
    ).

-spec add_role_name(term(), term(), #{integer() => binary()}) -> #{integer() => binary()}.
add_role_name(RoleId, Role, Acc) when is_integer(RoleId), is_map(Role) ->
    case push_utils:normalize_binary(maps:get(<<"name">>, Role, undefined)) of
        Name when is_binary(Name), byte_size(Name) > 0 -> Acc#{RoleId => Name};
        _ -> Acc
    end;
add_role_name(_RoleId, _Role, Acc) ->
    Acc.

-spec build_user_roles_map(map(), [user_id()]) -> #{user_id() => [integer()]}.
build_user_roles_map(Members, EligibleUserIds) ->
    lists:foldl(
        fun(UserId, Acc) -> add_user_roles(UserId, Members, Acc) end,
        #{},
        EligibleUserIds
    ).

-spec add_user_roles(user_id(), map(), #{user_id() => [integer()]}) ->
    #{user_id() => [integer()]}.
add_user_roles(UserId, Members, Acc) ->
    case maps:get(UserId, Members, undefined) of
        undefined -> Acc;
        Member -> Acc#{UserId => extract_role_ids(Member)}
    end.

-spec extract_role_ids(map()) -> [integer()].
extract_role_ids(Member) ->
    Roles = maps:get(<<"roles">>, Member, []),
    lists:foldl(
        fun collect_role_id/2,
        [],
        Roles
    ).

-spec collect_role_id(term(), [integer()]) -> [integer()].
collect_role_id(Role, Acc) ->
    case validation:validate_snowflake(<<"role">>, Role) of
        {ok, RoleId} -> [RoleId | Acc];
        _ -> Acc
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

presence_eligibility_suppresses_active_desktop_test() ->
    Tab = ets:new(test_member_presence, [set, public]),
    try
        ets:insert(
            Tab, {1, #{<<"status">> => <<"online">>, <<"mobile">> => false, <<"afk">> => false}}
        ),
        ?assertEqual(
            #{1 => false},
            build_push_presence_eligibility(#{<<"s1">> => #{user_id => 1}}, #{
                member_presence => Tab
            })
        )
    after
        ets:delete(Tab)
    end.

presence_eligibility_allows_mobile_only_session_test() ->
    Tab = ets:new(test_member_presence, [set, public]),
    try
        ets:insert(
            Tab, {1, #{<<"status">> => <<"online">>, <<"mobile">> => true, <<"afk">> => false}}
        ),
        ?assertEqual(
            #{1 => true},
            build_push_presence_eligibility(#{<<"s1">> => #{user_id => 1}}, #{
                member_presence => Tab
            })
        )
    after
        ets:delete(Tab)
    end.

presence_eligibility_allows_idle_and_afk_test() ->
    Tab = ets:new(test_member_presence, [set, public]),
    try
        ets:insert(
            Tab, {1, #{<<"status">> => <<"idle">>, <<"mobile">> => false, <<"afk">> => false}}
        ),
        ets:insert(
            Tab, {2, #{<<"status">> => <<"online">>, <<"mobile">> => false, <<"afk">> => true}}
        ),
        ?assertEqual(
            #{1 => true, 2 => true},
            build_push_presence_eligibility(
                #{<<"s1">> => #{user_id => 1}, <<"s2">> => #{user_id => 2}},
                #{member_presence => Tab}
            )
        )
    after
        ets:delete(Tab)
    end.

presence_eligibility_allows_unknown_presence_test() ->
    Tab = ets:new(test_member_presence, [set, public]),
    try
        ?assertEqual(
            #{7 => true},
            build_push_presence_eligibility(#{<<"s1">> => #{user_id => 7}}, #{
                member_presence => Tab
            })
        )
    after
        ets:delete(Tab)
    end.

presence_eligibility_allows_when_presence_table_missing_test() ->
    ?assertEqual(
        #{1 => true},
        build_push_presence_eligibility(#{<<"s1">> => #{user_id => 1}}, #{})
    ).

legacy_session_eligibility_suppresses_every_real_session_test() ->
    RealSession = #{
        session_id => <<"s1">>,
        user_id => 1,
        pid => self(),
        active_guilds => sets:new(),
        bot => false,
        is_staff => false,
        pending_connect => false,
        viewable_channels => #{}
    },
    ?assertEqual(#{1 => false}, build_push_session_eligibility(#{<<"s1">> => RealSession})).

is_push_candidate_suppresses_mention_for_active_user_test() ->
    Context = #{
        session_eligibility => #{1 => false},
        mention_everyone => true,
        direct_mentions => #{1 => true},
        mention_roles => #{}
    },
    ?assertEqual(false, is_push_candidate(1, #{}, Context)).

is_push_candidate_allows_eligible_user_test() ->
    Context = #{
        session_eligibility => #{1 => true},
        mention_everyone => false,
        direct_mentions => #{},
        mention_roles => #{}
    },
    ?assertEqual(true, is_push_candidate(1, #{}, Context)).

is_push_candidate_allows_sessionless_user_test() ->
    Context = #{
        session_eligibility => #{},
        mention_everyone => false,
        direct_mentions => #{},
        mention_roles => #{}
    },
    ?assertEqual(true, is_push_candidate(9, #{}, Context)).

push_candidate_user_ids_everyone_respects_active_sessions_test() ->
    Members = #{1 => #{}, 2 => #{}, 3 => #{}},
    ?assertEqual(
        [],
        push_candidate_user_ids(
            Members, #{1 => false, 2 => false, 3 => false}, #{<<"mention_everyone">> => true}
        )
    ),
    ?assertEqual(
        [1, 3],
        lists:sort(
            push_candidate_user_ids(
                Members, #{1 => true, 2 => false}, #{<<"mention_everyone">> => true}
            )
        )
    ).

build_push_session_eligibility_test() ->
    Sessions = #{
        <<"s1">> => #{user_id => 1, mobile => false, afk => true},
        <<"s2">> => #{user_id => 1, mobile => false, afk => true},
        <<"s3">> => #{user_id => 2, mobile => true, afk => true},
        <<"s4">> => #{user_id => 3, mobile => false, afk => false}
    },
    Eligibility = build_push_session_eligibility(Sessions),
    ?assertEqual(true, maps:get(1, Eligibility)),
    ?assertEqual(true, maps:get(2, Eligibility)),
    ?assertEqual(false, maps:get(3, Eligibility)).

push_candidate_user_ids_prefers_sessionless_and_eligible_sessions_test() ->
    Members = #{1 => #{}, 2 => #{}, 3 => #{}, 4 => #{}},
    SessionEligibility = #{1 => false, 2 => true},
    CandidateUserIds = push_candidate_user_ids(Members, SessionEligibility, #{}),
    ?assertEqual([2, 3, 4], lists:sort(CandidateUserIds)).

push_candidate_user_ids_includes_connected_mentioned_users_test() ->
    Members = #{
        1 => #{<<"roles">> => [<<"10">>]},
        2 => #{<<"roles">> => [<<"20">>]},
        3 => #{<<"roles">> => []},
        4 => #{<<"roles">> => []}
    },
    SessionEligibility = #{1 => false, 2 => false, 3 => false, 4 => false},
    MessageData = #{
        <<"mentions">> => [#{<<"id">> => <<"3">>}],
        <<"mention_roles">> => [<<"10">>]
    },
    CandidateUserIds = push_candidate_user_ids(Members, SessionEligibility, MessageData),
    ?assertEqual([1, 3], lists:sort(CandidateUserIds)).

push_candidate_user_ids_deduplicates_mentions_test() ->
    Members = #{1 => #{<<"roles">> => [<<"10">>]}, 2 => #{<<"roles">> => []}},
    SessionEligibility = #{1 => false, 2 => false},
    MessageData = #{
        <<"mentions">> => [#{<<"id">> => <<"1">>}, #{<<"id">> => <<"1">>}],
        <<"mention_roles">> => [<<"10">>]
    },
    CandidateUserIds = push_candidate_user_ids(Members, SessionEligibility, MessageData),
    ?assertEqual([1], CandidateUserIds).

build_user_roles_map_uses_member_map_test() ->
    Members = #{
        1 => #{<<"roles">> => [<<"10">>, <<"11">>]},
        2 => #{<<"roles">> => [<<"20">>]}
    },
    Result = build_user_roles_map(Members, [2, 1]),
    ?assertEqual([10, 11], lists:sort(maps:get(1, Result))),
    ?assertEqual([20], maps:get(2, Result)).

find_channel_name_found_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"general">>},
            #{<<"id">> => <<"101">>, <<"name">> => <<"random">>}
        ]
    },
    ?assertEqual(<<"general">>, find_channel_name(<<"100">>, Data)).

find_channel_name_not_found_test() ->
    Data = #{<<"channels">> => []},
    ?assertEqual(<<"unknown">>, find_channel_name(<<"100">>, Data)).

find_channel_name_uses_index_test() ->
    Data = #{
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"general">>}
        ],
        <<"channel_index">> => #{100 => #{<<"id">> => <<"100">>, <<"name">> => <<"general">>}}
    },
    ?assertEqual(<<"general">>, find_channel_name(<<"100">>, Data)).

send_push_to_eligible_users_uses_full_data_for_channel_name_test() ->
    Self = self(),
    ok = meck:new(push, [passthrough, no_link]),
    try
        ok = meck:expect(push, handle_message_create, fun(Params) ->
            Self ! {push_params, Params},
            ok
        end),
        MessageData = #{
            <<"channel_id">> => <<"100">>,
            <<"author">> => #{<<"id">> => <<"42">>}
        },
        Data = #{
            <<"guild">> => #{
                <<"name">> => <<"Test Guild">>,
                <<"default_message_notifications">> => 0
            },
            <<"channels">> => [
                #{<<"id">> => <<"100">>, <<"name">> => <<"general">>}
            ],
            <<"channel_index">> => #{
                100 => #{<<"id">> => <<"100">>, <<"name">> => <<"general">>}
            },
            <<"roles">> => [
                #{<<"id">> => <<"200">>, <<"name">> => <<"Alerts">>}
            ],
            <<"role_index">> => #{
                200 => #{<<"id">> => <<"200">>, <<"name">> => <<"Alerts">>}
            }
        },
        ?assertEqual(
            ok,
            send_push_to_eligible_users(
                MessageData, 10, [1], #{1 => []}, #{}, Data, undefined
            )
        ),
        receive
            {push_params, Params} ->
                ?assertEqual(<<"general">>, maps:get(channel_name, Params)),
                ?assertEqual(<<"Test Guild">>, maps:get(guild_name, Params)),
                ?assertEqual(#{200 => <<"Alerts">>}, maps:get(role_names, Params)),
                ?assertEqual(undefined, maps:get(guild_member_count, Params)),
                ?assertEqual(undefined, maps:get(guild_features, Params))
        after 1000 ->
            ?assert(false)
        end,
        ?assert(meck:validate(push))
    after
        meck:unload(push)
    end.

find_channel_name_invalid_id_test() ->
    Data = #{<<"channels">> => []},
    ?assertEqual(<<"unknown">>, find_channel_name(<<"invalid">>, Data)).

build_role_names_map_uses_role_index_test() ->
    Data = #{
        <<"roles">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"Fallback">>}
        ],
        <<"role_index">> => #{
            100 => #{<<"id">> => <<"100">>, <<"name">> => <<"Mods">>},
            200 => #{<<"id">> => <<"200">>, <<"name">> => <<>>},
            bad => #{<<"id">> => <<"300">>, <<"name">> => <<"Bad">>}
        }
    },
    ?assertEqual(#{100 => <<"Mods">>}, build_role_names_map(Data)).

extract_role_ids_test() ->
    Member = #{<<"roles">> => [<<"10">>, <<"20">>, <<"invalid">>]},
    Result = lists:sort(extract_role_ids(Member)),
    ?assertEqual([10, 20], Result).

extract_role_ids_empty_test() ->
    Member = #{<<"roles">> => []},
    ?assertEqual([], extract_role_ids(Member)).

extract_role_ids_missing_key_test() ->
    Member = #{},
    ?assertEqual([], extract_role_ids(Member)).

meta_accessors_default_to_undefined_test() ->
    ?assertEqual(undefined, meta_member_count(undefined)),
    ?assertEqual(undefined, meta_features(undefined)),
    ?assertEqual(5, meta_member_count(#{member_count => 5, features => []})),
    ?assertEqual([], meta_features(#{member_count => 5, features => []})).

compact_push_state_drops_members_and_keeps_member_count_test() ->
    Tab = ets:new(test_members, [set, public]),
    try
        Data = compact_test_data(Tab),
        UpdatedState = #{
            id => 7,
            data => Data,
            sessions => #{<<"s1">> => #{user_id => 1}},
            virtual_channel_access => #{},
            member_count => 49435
        },
        Compact = compact_push_state(Tab, Data, 7, UpdatedState),
        CompactData = maps:get(data, Compact),
        ?assertEqual(49435, maps:get(member_count, Compact)),
        ?assertEqual(Tab, maps:get(members_ets, Compact)),
        ?assertNot(maps:is_key(sessions, Compact)),
        ?assertEqual(#{1 => true}, maps:get(session_eligibility, Compact)),
        ?assertNot(maps:is_key(<<"members">>, CompactData)),
        ?assertNot(maps:is_key(members_normalized, CompactData)),
        ?assertNot(maps:is_key(<<"member_role_index">>, CompactData)),
        ?assertEqual(
            #{member_count => 49435, features => [<<"COMMUNITY">>]},
            large_guild_meta(Compact)
        )
    after
        ets:delete(Tab)
    end.

legacy_push_state_carries_member_count_test() ->
    UpdatedState = #{id => 7, data => #{}, sessions => #{}, member_count => 1234},
    Legacy = legacy_push_state(7, UpdatedState),
    ?assertEqual(1234, maps:get(member_count, Legacy)),
    ?assertEqual(#{}, maps:get(sessions, Legacy)),
    ?assertEqual(#{member_count => 1234, features => []}, large_guild_meta(Legacy)).

compact_scan_collects_eligible_users_and_format_members_test() ->
    Tab = ets:new(test_members, [set, public]),
    try
        true = ets:insert(Tab, {1, #{<<"roles">> => []}}),
        true = ets:insert(Tab, {2, #{<<"roles">> => []}}),
        true = ets:insert(Tab, {3, #{<<"roles">> => [<<"200">>]}}),
        State = compact_scan_state(Tab),
        MessageData = #{<<"mentions">> => [#{<<"id">> => <<"3">>}]},
        Context = compact_scan_context(MessageData, 10, State),
        {ok, Scan} = scan_push_members(Tab, Context, State),
        ?assertEqual([1], lists:sort(maps:get(eligible_user_ids, Scan))),
        ?assertEqual(#{3 => #{<<"roles">> => [<<"200">>]}}, maps:get(format_members, Scan)),
        ?assertNot(maps:is_key(3, maps:get(user_roles, Scan))),
        ?assertEqual([], maps:get(1, maps:get(user_roles, Scan)))
    after
        ets:delete(Tab)
    end.

compact_scan_includes_mentioned_user_when_bypass_is_re_enabled_test() ->
    Tab = ets:new(test_members, [set, public]),
    application:set_env(fluxer_gateway, push_mentions_respect_active_session, false),
    try
        true = ets:insert(Tab, {1, #{<<"roles">> => []}}),
        true = ets:insert(Tab, {3, #{<<"roles">> => [<<"200">>]}}),
        State = compact_scan_state(Tab),
        MessageData = #{<<"mentions">> => [#{<<"id">> => <<"3">>}]},
        Context = compact_scan_context(MessageData, 10, State),
        {ok, Scan} = scan_push_members(Tab, Context, State),
        ?assertEqual([1, 3], lists:sort(maps:get(eligible_user_ids, Scan)))
    after
        application:unset_env(fluxer_gateway, push_mentions_respect_active_session),
        ets:delete(Tab)
    end.

scan_push_members_reports_unavailable_table_test() ->
    Tab = ets:new(test_members, [set, public]),
    State = compact_scan_state(Tab),
    Context = compact_scan_context(#{}, 10, State),
    true = ets:delete(Tab),
    ?assertEqual({error, table_unavailable}, scan_push_members(Tab, Context, State)).

collect_format_mention_ids_is_bounded_test() ->
    Mentions = [#{<<"id">> => integer_to_binary(Id)} || Id <- lists:seq(1, 80)],
    Result = collect_format_mention_ids(Mentions, ?MAX_FORMAT_MEMBERS, #{}),
    ?assertEqual(?MAX_FORMAT_MEMBERS, map_size(Result)).

format_member_id_set_includes_author_test() ->
    MessageData = #{
        <<"author">> => #{<<"id">> => <<"99">>},
        <<"mentions">> => [#{<<"id">> => <<"1">>}, #{<<"id">> => <<"1">>}]
    },
    ?assertEqual(#{1 => true, 99 => true}, format_member_id_set(MessageData)).

spawn_push_without_members_table_falls_back_to_legacy_push_test() ->
    reset_push_worker_state(),
    try
        ?assertEqual(ok, spawn_push(#{}, 7, #{id => 7, data => #{}, sessions => #{}})),
        ?assert(is_pid(get(push_inflight)))
    after
        reset_push_worker_state()
    end.

compact_push_carries_large_guild_metadata_test() ->
    Self = self(),
    Tab = ets:new(test_members, [set, public]),
    ok = meck:new(push, [passthrough, no_link]),
    try
        ok = meck:expect(push, handle_message_create, fun(Params) ->
            Self ! {push_params, Params},
            ok
        end),
        State = compact_scan_state(Tab),
        Scan = #{
            eligible_user_ids => [1],
            user_roles => #{1 => []},
            format_members => #{1 => #{<<"nick">> => <<"one">>}}
        },
        MessageData = #{
            <<"channel_id">> => <<"10">>,
            <<"author">> => #{<<"id">> => <<"42">>}
        },
        ?assertEqual(ok, send_compact_scanned_push(MessageData, 7, Scan, State)),
        receive
            {push_params, Params} ->
                ?assertEqual(49435, maps:get(guild_member_count, Params)),
                ?assertEqual([<<"COMMUNITY">>], maps:get(guild_features, Params)),
                ?assertEqual([1], maps:get(user_ids, Params)),
                ?assertEqual(<<"general">>, maps:get(channel_name, Params))
        after 1000 ->
            ?assert(false)
        end,
        ?assert(meck:validate(push))
    after
        meck:unload(push),
        ets:delete(Tab)
    end.

compact_format_data_restricts_member_map_test() ->
    Data = #{<<"guild">> => #{}, <<"members">> => #{1 => #{}, 2 => #{}, 3 => #{}}},
    FormatMembers = #{2 => #{<<"roles">> => [<<"200">>]}},
    Result = compact_format_data(Data, FormatMembers),
    ?assertEqual(FormatMembers, maps:get(<<"members">>, Result)),
    ?assertEqual(FormatMembers, maps:get(members_normalized, Result)),
    ?assertEqual([2], maps:get(members_sorted_ids, Result)),
    ?assertEqual(#{200 => #{2 => true}}, maps:get(<<"member_role_index">>, Result)).

inflight_pid_without_a_slot_still_counts_against_the_limit_test() ->
    reset_push_worker_state(),
    Blocker = blocking_push_worker(),
    put(push_inflight, Blocker),
    ok = application:set_env(fluxer_gateway, ?CONCURRENCY_LIMIT_KEY, 1),
    try
        ?assertEqual([Blocker], worker_pids(live_push_workers())),
        ?assertEqual(ok, maybe_spawn_push(#{}, 7, legacy_test_state())),
        ?assertEqual(Blocker, get(push_inflight)),
        ?assertEqual([Blocker], worker_pids(get(push_inflight_workers)))
    after
        stop_push_worker(Blocker),
        reset_push_worker_state()
    end.

unknown_worker_entries_are_dropped_test() ->
    reset_push_worker_state(),
    Blocker = blocking_push_worker(),
    put(push_inflight_workers, [Blocker, {Blocker, 0}, not_a_worker]),
    try
        ?assertEqual([], live_push_workers())
    after
        stop_push_worker(Blocker),
        reset_push_worker_state()
    end.

remote_worker_pid_is_dropped_without_badarg_test() ->
    reset_push_worker_state(),
    Remote = remote_test_pid(),
    put(push_inflight_workers, [{1, Remote, erlang:monotonic_time(millisecond)}]),
    try
        ?assertNotEqual(node(), node(Remote)),
        ?assertError(badarg, erlang:is_process_alive(Remote)),
        ?assertEqual(false, local_process_alive(Remote)),
        ?assertEqual([], live_push_workers())
    after
        reset_push_worker_state()
    end.

reused_worker_pid_consumes_only_one_slot_test() ->
    reset_push_worker_state(),
    Blocker = blocking_push_worker(),
    Now = erlang:monotonic_time(millisecond),
    put(push_inflight_workers, [{2, Blocker, Now}, {1, Blocker, Now - 10}]),
    Before = read_push_counter(slot_deduped),
    try
        ?assertEqual([{2, Blocker, Now}], live_push_workers()),
        ?assertEqual(Before + 1, read_push_counter(slot_deduped))
    after
        stop_push_worker(Blocker),
        reset_push_worker_state()
    end.

stale_push_worker_slot_is_reclaimed_and_counted_test() ->
    reset_push_worker_state(),
    Stale = blocking_push_worker(),
    Aged = erlang:monotonic_time(millisecond) - ?PUSH_WORKER_MAX_AGE_MS - 1,
    put(push_inflight_workers, [{1, Stale, Aged}]),
    Before = read_push_counter(slot_reclaimed),
    try
        ?assertEqual(ok, maybe_spawn_push(#{}, 7, legacy_test_state())),
        ?assertEqual(Before + 1, read_push_counter(slot_reclaimed)),
        ?assertEqual(1, length(get(push_inflight_workers))),
        ?assertNot(lists:member(Stale, worker_pids(get(push_inflight_workers))))
    after
        stop_push_worker(Stale),
        reset_push_worker_state()
    end.

push_drop_is_readable_from_named_ets_table_test() ->
    reset_push_worker_state(),
    ok = application:set_env(fluxer_gateway, ?CONCURRENCY_LIMIT_KEY, 1),
    Blocker = blocking_push_worker(),
    put(push_inflight_workers, [{1, Blocker, erlang:monotonic_time(millisecond)}]),
    Before = read_push_counter(dropped_at_limit),
    try
        ?assertEqual(ok, maybe_spawn_push(#{}, 7, legacy_test_state())),
        ?assertEqual(Before + 1, read_push_counter(dropped_at_limit)),
        ?assertMatch([{dropped_at_limit, _}], ets:lookup(?PUSH_COUNTERS, dropped_at_limit)),
        ?assertEqual(
            lists:sort(?PUSH_COUNTER_KEYS),
            lists:sort([Key || {Key, _} <- ets:tab2list(?PUSH_COUNTERS)])
        )
    after
        stop_push_worker(Blocker),
        reset_push_worker_state()
    end.

started_push_worker_is_counted_test() ->
    reset_push_worker_state(),
    Before = read_push_counter(worker_started),
    try
        ?assertEqual(ok, maybe_spawn_push(#{}, 7, legacy_test_state())),
        ?assertEqual(Before + 1, read_push_counter(worker_started)),
        ?assert(is_pid(get(push_inflight))),
        ?assertEqual(1, length(get(push_inflight_workers)))
    after
        reset_push_worker_state()
    end.

worker_completion_is_counted_test() ->
    reset_push_worker_state(),
    Before = read_push_counter(worker_completed),
    BeforeFailed = read_push_counter(worker_failed),
    try
        ?assertEqual(ok, run_counted_push_worker(fun() -> ok end)),
        ?assertEqual(Before + 1, read_push_counter(worker_completed)),
        ?assertEqual(BeforeFailed, read_push_counter(worker_failed))
    after
        reset_push_worker_state()
    end.

worker_crash_is_counted_as_failure_not_completion_test() ->
    reset_push_worker_state(),
    Before = read_push_counter(worker_failed),
    BeforeCompleted = read_push_counter(worker_completed),
    try
        ?assertEqual(ok, run_counted_push_worker(fun() -> error(boom) end)),
        ?assertEqual(Before + 1, read_push_counter(worker_failed)),
        ?assertEqual(BeforeCompleted, read_push_counter(worker_completed))
    after
        reset_push_worker_state()
    end.

spawned_worker_reports_started_and_completed_test() ->
    reset_push_worker_state(),
    Self = self(),
    Started = read_push_counter(worker_started),
    Completed = read_push_counter(worker_completed),
    try
        ?assertEqual(
            ok,
            spawn_push_worker(
                fun() ->
                    Self ! {ran, self()},
                    ok
                end,
                7
            )
        ),
        Pid = get(push_inflight),
        receive
            {ran, Pid} -> ok
        after 1000 -> ?assert(false)
        end,
        wait_for_counter(worker_completed, Completed + 1),
        ?assertEqual(Started + 1, read_push_counter(worker_started)),
        ?assertEqual(Completed + 1, read_push_counter(worker_completed)),
        Messages = element(2, process_info(self(), messages)),
        ?assertEqual([], [M || {'DOWN', _, process, P, _} = M <- Messages, P =:= Pid])
    after
        reset_push_worker_state()
    end.

missing_members_table_is_counted_test() ->
    reset_push_worker_state(),
    Before = read_push_counter(members_table_missing),
    try
        ?assertEqual(ok, maybe_spawn_push(#{}, 7, legacy_test_state())),
        ?assertEqual(Before + 1, read_push_counter(members_table_missing))
    after
        reset_push_worker_state()
    end.

scan_table_unavailable_is_counted_test() ->
    reset_push_worker_state(),
    Tab = ets:new(test_members, [set, public]),
    State = compact_scan_state(Tab),
    true = ets:delete(Tab),
    Before = read_push_counter(scan_table_unavailable),
    try
        MessageData = #{<<"channel_id">> => <<"10">>},
        ?assertEqual(ok, send_compact_push_notifications(MessageData, 7, State)),
        ?assertEqual(Before + 1, read_push_counter(scan_table_unavailable))
    after
        reset_push_worker_state()
    end.

bounded_scan_leaves_the_members_table_unfixed_test() ->
    reset_push_worker_state(),
    Tab = ets:new(test_members, [set, public]),
    try
        true = ets:insert(Tab, {1, #{<<"roles">> => []}}),
        true = ets:insert(Tab, {2, #{<<"roles">> => []}}),
        State = compact_scan_state(Tab),
        Context = compact_scan_context(#{}, 10, State),
        ?assertEqual(false, ets:info(Tab, fixed)),
        {ok, Scan} = scan_push_members(Tab, Context, State),
        ?assertEqual([1], lists:sort(maps:get(eligible_user_ids, Scan))),
        ?assertEqual(false, ets:info(Tab, fixed)),
        ?assertEqual(false, ets:info(Tab, safe_fixed_monotonic_time))
    after
        ets:delete(Tab),
        reset_push_worker_state()
    end.

bounded_push_concurrency_spawns_second_worker_under_limit_test() ->
    reset_push_worker_state(),
    ok = application:set_env(fluxer_gateway, ?CONCURRENCY_LIMIT_KEY, 3),
    Blocker = blocking_push_worker(),
    put(push_inflight, Blocker),
    put(push_inflight_workers, [{1, Blocker, erlang:monotonic_time(millisecond)}]),
    try
        ?assertEqual(ok, maybe_spawn_push(#{}, 7, legacy_test_state())),
        ?assertNotEqual(Blocker, get(push_inflight)),
        ?assertEqual(2, length(get(push_inflight_workers))),
        ?assert(lists:member(Blocker, worker_pids(get(push_inflight_workers))))
    after
        stop_push_worker(Blocker),
        reset_push_worker_state()
    end.

bounded_push_concurrency_counts_drops_at_limit_test() ->
    reset_push_worker_state(),
    ok = application:set_env(fluxer_gateway, ?CONCURRENCY_LIMIT_KEY, 2),
    First = blocking_push_worker(),
    Second = blocking_push_worker(),
    Now = erlang:monotonic_time(millisecond),
    put(push_inflight, Second),
    put(push_inflight_workers, [{2, Second, Now}, {1, First, Now}]),
    Before = read_push_counter(dropped_at_limit),
    try
        ?assertEqual(ok, maybe_spawn_push(#{}, 7, legacy_test_state())),
        ?assertEqual(Second, get(push_inflight)),
        ?assertEqual([Second, First], worker_pids(get(push_inflight_workers))),
        ?assertEqual(Before + 1, read_push_counter(dropped_at_limit)),
        ?assertEqual(ok, maybe_spawn_push(#{}, 7, legacy_test_state())),
        ?assertEqual(Before + 2, read_push_counter(dropped_at_limit)),
        ?assertEqual([Second, First], worker_pids(get(push_inflight_workers)))
    after
        stop_push_worker(First),
        stop_push_worker(Second),
        reset_push_worker_state()
    end.

bounded_push_concurrency_reaps_finished_workers_test() ->
    reset_push_worker_state(),
    ok = application:set_env(fluxer_gateway, ?CONCURRENCY_LIMIT_KEY, 1),
    Finished = blocking_push_worker(),
    stop_push_worker(Finished),
    put(push_inflight, Finished),
    put(push_inflight_workers, [{1, Finished, erlang:monotonic_time(millisecond)}]),
    try
        ?assertEqual(ok, maybe_spawn_push(#{}, 7, legacy_test_state())),
        ?assertNot(lists:member(Finished, worker_pids(get(push_inflight_workers))))
    after
        reset_push_worker_state()
    end.

bounded_worker_list_stays_bounded_by_the_limit_test() ->
    reset_push_worker_state(),
    ok = application:set_env(fluxer_gateway, ?CONCURRENCY_LIMIT_KEY, 2),
    Blockers = [blocking_push_worker() || _ <- lists:seq(1, 6)],
    Now = erlang:monotonic_time(millisecond),
    put(push_inflight_workers, [
        {Gen, Pid, Now}
     || {Gen, Pid} <- lists:zip(lists:seq(1, 6), Blockers)
    ]),
    try
        lists:foreach(
            fun(_) -> ?assertEqual(ok, maybe_spawn_push(#{}, 7, legacy_test_state())) end,
            lists:seq(1, 20)
        ),
        ?assert(length(get(push_inflight_workers)) =< 6)
    after
        lists:foreach(fun stop_push_worker/1, Blockers),
        reset_push_worker_state()
    end.

push_concurrency_limit_defaults_and_clamps_test() ->
    reset_push_worker_state(),
    try
        ?assertEqual(?DEFAULT_PUSH_CONCURRENCY, push_concurrency_limit()),
        ok = application:set_env(fluxer_gateway, ?CONCURRENCY_LIMIT_KEY, 0),
        ?assertEqual(?DEFAULT_PUSH_CONCURRENCY, push_concurrency_limit()),
        ok = application:set_env(fluxer_gateway, ?CONCURRENCY_LIMIT_KEY, not_a_number),
        ?assertEqual(?DEFAULT_PUSH_CONCURRENCY, push_concurrency_limit()),
        ok = application:set_env(fluxer_gateway, ?CONCURRENCY_LIMIT_KEY, 1000),
        ?assertEqual(?MAX_PUSH_CONCURRENCY, push_concurrency_limit()),
        ok = application:set_env(fluxer_gateway, ?CONCURRENCY_LIMIT_KEY, 6),
        ?assertEqual(6, push_concurrency_limit())
    after
        reset_push_worker_state()
    end.

legacy_test_state() ->
    #{id => 7, data => #{}, sessions => #{}}.

worker_pids(Workers) ->
    [Pid || {_Gen, Pid, _StartedMs} <- Workers].

wait_for_counter(Key, Target) ->
    wait_for_counter(Key, Target, 200).

wait_for_counter(Key, Target, 0) ->
    ?assertEqual(Target, read_push_counter(Key));
wait_for_counter(Key, Target, Retries) ->
    case read_push_counter(Key) >= Target of
        true ->
            ok;
        false ->
            timer:sleep(5),
            wait_for_counter(Key, Target, Retries - 1)
    end.

remote_test_pid() ->
    binary_to_term(<<131, 88, 119, 12, "fake@nowhere", 1:32, 0:32, 1:32>>).

read_push_counter(Key) ->
    try ets:lookup(?PUSH_COUNTERS, Key) of
        [{Key, Value}] when is_integer(Value) -> Value;
        _ -> 0
    catch
        error:badarg -> 0
    end.

blocking_push_worker() ->
    spawn(fun() ->
        receive
            stop -> ok
        end
    end).

stop_push_worker(Pid) ->
    Ref = monitor(process, Pid),
    Pid ! stop,
    receive
        {'DOWN', Ref, process, Pid, _Reason} -> ok
    after 5000 -> error(push_worker_did_not_stop)
    end.

reset_push_worker_state() ->
    erase(push_inflight),
    erase(push_inflight_workers),
    ok = application:unset_env(fluxer_gateway, ?CONCURRENCY_LIMIT_KEY).

compact_test_data(Tab) ->
    #{
        <<"guild">> => #{
            <<"id">> => <<"7">>,
            <<"owner_id">> => <<"999">>,
            <<"features">> => [<<"COMMUNITY">>],
            <<"default_message_notifications">> => 0
        },
        <<"roles">> => [],
        <<"role_index">> => #{},
        <<"channels">> => [#{<<"id">> => <<"10">>, <<"name">> => <<"general">>}],
        <<"channel_index">> => #{10 => #{<<"id">> => <<"10">>, <<"name">> => <<"general">>}},
        <<"members">> => #{1 => #{}, 2 => #{}, 3 => #{}},
        members_normalized => #{1 => #{}, 2 => #{}, 3 => #{}},
        <<"member_role_index">> => #{},
        members_ets => Tab
    }.

compact_scan_state(Tab) ->
    #{
        id => 7,
        data => compact_push_data(compact_test_data(Tab)),
        virtual_channel_access => #{
            1 => sets:from_list([10]),
            3 => sets:from_list([10])
        },
        members_ets => Tab,
        session_eligibility => #{1 => true, 2 => true, 3 => false},
        member_count => 49435
    }.

-endif.
