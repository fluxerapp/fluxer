%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_recipients).
-typing([eqwalizer]).

-export([filter_eligible/9, counters/0]).

-define(CNT_FETCH_ATTEMPTS, push_blocked_ids_fetch_attempts).
-define(CNT_FETCH_FAILURES, push_blocked_ids_fetch_failures).
-define(CNT_SUPPRESSED, push_blocked_ids_suppressed).
-define(CNT_BUDGET_EXHAUSTED, push_blocked_ids_budget_exhausted).
-define(MAX_FETCH_RPCS, 8).
-define(DEFAULT_FETCH_USERS, 2000).
-define(MAX_FETCH_USERS, 5000).
-define(DEFAULT_FETCH_CHUNK, 500).
-define(MIN_FETCH_CHUNK, 50).
-define(MAX_FETCH_CHUNK, 1000).

-spec filter_eligible(
    [integer()],
    integer(),
    integer(),
    integer(),
    map(),
    integer(),
    map(),
    map(),
    map() | undefined
) -> [integer()].
filter_eligible(
    UserIds,
    AuthorId,
    GuildId,
    ChannelId,
    MessageData,
    GuildDefaultNotifications,
    UserRolesMap,
    ConnectedUsers,
    SuppliedMetadata
) ->
    LargeGuildMetadata = resolve_large_guild_metadata(GuildId, SuppliedMetadata),
    Candidates = drop_blocked_recipients(UserIds, AuthorId, #{}),
    push_eligibility:prefetch_user_guild_settings(Candidates, AuthorId, GuildId),
    EligibleUsers = lists:filter(
        fun(UserId) ->
            push_eligibility:is_eligible_for_push(
                UserId,
                AuthorId,
                GuildId,
                ChannelId,
                MessageData,
                GuildDefaultNotifications,
                UserRolesMap,
                ConnectedUsers,
                LargeGuildMetadata
            )
        end,
        Candidates
    ),
    drop_blocked_recipients(EligibleUsers, AuthorId, fetch_missing_blocked_ids(EligibleUsers)).

-spec resolve_large_guild_metadata(integer(), map() | undefined) -> map() | undefined.
resolve_large_guild_metadata(0, _SuppliedMetadata) ->
    undefined;
resolve_large_guild_metadata(GuildId, SuppliedMetadata) ->
    supplied_or_local_metadata(GuildId, SuppliedMetadata).

-spec supplied_or_local_metadata(integer(), map() | undefined) -> map() | undefined.
supplied_or_local_metadata(GuildId, undefined) ->
    large_guild_metadata_local(GuildId);
supplied_or_local_metadata(_GuildId, SuppliedMetadata) when is_map(SuppliedMetadata) ->
    SuppliedMetadata;
supplied_or_local_metadata(GuildId, _SuppliedMetadata) ->
    large_guild_metadata_local(GuildId).

-spec large_guild_metadata_local(integer()) -> map() | undefined.
large_guild_metadata_local(GuildId) ->
    push_eligibility_checks:get_guild_large_metadata(GuildId).

-spec drop_blocked_recipients([integer()], integer(), #{integer() => [integer()]}) ->
    [integer()].
drop_blocked_recipients(UserIds, AuthorId, Fetched) ->
    Kept = lists:filter(
        fun(UserId) -> not is_blocked_recipient(UserId, AuthorId, Fetched) end,
        UserIds
    ),
    count_suppressed(length(UserIds) - length(Kept)),
    Kept.

-spec is_blocked_recipient(integer(), integer(), #{integer() => [integer()]}) -> boolean().
is_blocked_recipient(UserId, AuthorId, Fetched) ->
    lists:member(AuthorId, maps:get(UserId, Fetched, [])) orelse
        push_eligibility:is_user_blocked(UserId, AuthorId).

-spec count_suppressed(non_neg_integer()) -> ok.
count_suppressed(0) ->
    ok;
count_suppressed(Suppressed) ->
    push_worker_pool:bump_counter(?CNT_SUPPRESSED, Suppressed).

-spec fetch_missing_blocked_ids([integer()]) -> #{integer() => [integer()]}.
fetch_missing_blocked_ids(UserIds) ->
    case missing_blocked_ids(UserIds) of
        [] -> #{};
        Missing -> fetch_blocked_ids_within_budget(Missing)
    end.

-spec missing_blocked_ids([integer()]) -> [integer()].
missing_blocked_ids(UserIds) ->
    lists:usort(lists:filter(fun is_blocked_ids_cache_miss/1, UserIds)).

-spec is_blocked_ids_cache_miss(integer()) -> boolean().
is_blocked_ids_cache_miss(UserId) ->
    push_ets_cache:get_blocked_ids(UserId) =:= undefined.

-spec fetch_blocked_ids_within_budget([integer()]) -> #{integer() => [integer()]}.
fetch_blocked_ids_within_budget(Missing) ->
    {Budget, ChunkSize} = blocked_ids_fetch_budget(),
    Budgeted = lists:sublist(Missing, Budget),
    count_budget_exhausted(length(Missing) - length(Budgeted)),
    Fill = push_ets_cache:reserve_blocked_ids(Budgeted),
    try
        fetch_blocked_ids_chunks(chunk_user_ids(Budgeted, ChunkSize, []), Fill, #{})
    catch
        Class:Reason -> blocked_ids_fetch_crashed(Class, Reason)
    after
        push_ets_cache:release(Fill)
    end.

-spec blocked_ids_fetch_crashed(throw | error | exit, term()) -> #{}.
blocked_ids_fetch_crashed(Class, Reason) ->
    push_worker_pool:bump_counter(?CNT_FETCH_FAILURES, 1),
    logger:debug("Push: blocked id fetch crashed", #{class => Class, reason => Reason}),
    #{}.

-spec blocked_ids_fetch_budget() -> {pos_integer(), pos_integer()}.
blocked_ids_fetch_budget() ->
    ChunkSize = blocked_ids_fetch_chunk(),
    MaxUsers = blocked_ids_fetch_max_users(),
    {min(MaxUsers, ChunkSize * ?MAX_FETCH_RPCS), ChunkSize}.

-spec count_budget_exhausted(non_neg_integer()) -> ok.
count_budget_exhausted(0) ->
    ok;
count_budget_exhausted(Skipped) ->
    push_worker_pool:bump_counter(?CNT_BUDGET_EXHAUSTED, Skipped).

-spec fetch_blocked_ids_chunks(
    [[integer()]], push_ets_cache:fill(), #{integer() => [integer()]}
) -> #{integer() => [integer()]}.
fetch_blocked_ids_chunks([], _Fill, Fetched) ->
    Fetched;
fetch_blocked_ids_chunks([Chunk | Rest], Fill, Fetched) ->
    case fetch_blocked_ids_chunk(Chunk, Fill) of
        {ok, ChunkFetched} ->
            fetch_blocked_ids_chunks(Rest, Fill, maps:merge(Fetched, ChunkFetched));
        error ->
            Fetched
    end.

-spec chunk_user_ids([integer()], pos_integer(), [[integer()]]) -> [[integer()]].
chunk_user_ids([], _ChunkSize, Acc) ->
    lists:reverse(Acc);
chunk_user_ids(UserIds, ChunkSize, Acc) ->
    Chunk = lists:sublist(UserIds, ChunkSize),
    chunk_user_ids(drop_prefix(UserIds, ChunkSize), ChunkSize, [Chunk | Acc]).

-spec drop_prefix([integer()], non_neg_integer()) -> [integer()].
drop_prefix(UserIds, 0) ->
    UserIds;
drop_prefix([], _N) ->
    [];
drop_prefix([_UserId | Rest], N) ->
    drop_prefix(Rest, N - 1).

-spec fetch_blocked_ids_chunk([integer()], push_ets_cache:fill()) ->
    {ok, #{integer() => [integer()]}} | error.
fetch_blocked_ids_chunk([], _Fill) ->
    {ok, #{}};
fetch_blocked_ids_chunk(UserIds, Fill) ->
    push_worker_pool:bump_counter(?CNT_FETCH_ATTEMPTS, 1),
    Request = #{
        <<"type">> => <<"get_user_blocked_ids">>,
        <<"user_ids">> => [integer_to_binary(UserId) || UserId <- UserIds]
    },
    case rpc_client:call(Request) of
        {ok, Data} ->
            {ok, cache_blocked_ids_response(UserIds, Data, Fill)};
        {error, Reason} ->
            fetch_blocked_ids_failed(Reason, length(UserIds))
    end.

-spec fetch_blocked_ids_failed(term(), non_neg_integer()) -> error.
fetch_blocked_ids_failed(Reason, UserCount) ->
    push_worker_pool:bump_counter(?CNT_FETCH_FAILURES, 1),
    logger:debug("Push: blocked id fetch failed", #{
        reason => Reason, user_count => UserCount
    }),
    error.

-spec cache_blocked_ids_response([integer()], map(), push_ets_cache:fill()) ->
    #{integer() => [integer()]}.
cache_blocked_ids_response(UserIds, Data, Fill) ->
    maps:from_list([{UserId, cache_blocked_ids_entry(UserId, Data, Fill)} || UserId <- UserIds]).

-spec cache_blocked_ids_entry(integer(), map(), push_ets_cache:fill()) -> [integer()].
cache_blocked_ids_entry(UserId, Data, Fill) ->
    BlockedIds = blocked_ids_from_response(maps:get(integer_to_binary(UserId), Data, [])),
    ok = push_ets_cache:put_blocked_ids_fetched(UserId, BlockedIds, Fill),
    BlockedIds.

-spec blocked_ids_from_response(term()) -> [integer()].
blocked_ids_from_response(Values) when is_list(Values) ->
    lists:filtermap(fun snowflake_id:filter/1, Values);
blocked_ids_from_response(_Values) ->
    [].

-spec blocked_ids_fetch_max_users() -> pos_integer().
blocked_ids_fetch_max_users() ->
    Value = push_utils:app_pos_integer(push_blocked_ids_fetch_max_users, ?DEFAULT_FETCH_USERS),
    min(Value, ?MAX_FETCH_USERS).

-spec blocked_ids_fetch_chunk() -> pos_integer().
blocked_ids_fetch_chunk() ->
    Value = push_utils:app_pos_integer(push_blocked_ids_fetch_chunk, ?DEFAULT_FETCH_CHUNK),
    min(max(Value, ?MIN_FETCH_CHUNK), ?MAX_FETCH_CHUNK).

-spec counters() -> map().
counters() ->
    #{
        blocked_ids_fetch_attempts => push_worker_pool:read_counter(?CNT_FETCH_ATTEMPTS),
        blocked_ids_fetch_failures => push_worker_pool:read_counter(?CNT_FETCH_FAILURES),
        blocked_ids_suppressed => push_worker_pool:read_counter(?CNT_SUPPRESSED),
        blocked_ids_budget_exhausted => push_worker_pool:read_counter(?CNT_BUDGET_EXHAUSTED)
    }.
