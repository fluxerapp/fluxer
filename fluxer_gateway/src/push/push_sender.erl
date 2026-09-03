%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_sender).
-typing([eqwalizer]).

-export([
    send_to_user_subscriptions/3,
    send_clear_to_user_subscriptions/5,
    send_push_notifications/1,
    send_push_notifications/8,
    send_clear_channel_notifications/4
]).

-export_type([send_context/0]).

-define(DEFAULT_BADGE_FETCH_BATCH, 2000).
-define(BADGE_FETCH_MAX_CONSECUTIVE_FAILURES, 3).
-define(DEFAULT_BADGE_FETCH_BUDGET_MS, 120000).
-define(PUSH_COUNTERS, push_worker_counter).

-type send_context() :: #{
    message_data := map(),
    guild_id := integer(),
    channel_id := integer(),
    message_id := integer(),
    guild_name := binary() | undefined,
    channel_name := binary() | undefined,
    badge_count := non_neg_integer(),
    content_preview := binary(),
    markdown_context := map()
}.

-spec send_to_user_subscriptions(integer(), list(), send_context()) -> ok.
send_to_user_subscriptions(UserId, Subscriptions, SendContext) ->
    BadgeCount = maps:get(badge_count, SendContext),
    NotificationPayload = notification_payload(UserId, SendContext),
    logger:debug("Push: sending to user subscriptions", #{
        user_id => UserId,
        subscription_count => length(Subscriptions),
        badge_count => BadgeCount
    }),
    FailedSubscriptions = send_subscriptions(UserId, NotificationPayload, Subscriptions, []),
    handle_failed_subscriptions(UserId, FailedSubscriptions).

-spec notification_payload(integer(), send_context()) -> map().
notification_payload(UserId, SendContext) ->
    #{
        message_data := MessageData,
        guild_id := GuildId,
        channel_id := ChannelId,
        message_id := MessageId,
        guild_name := GuildName,
        channel_name := ChannelName,
        badge_count := BadgeCount,
        content_preview := ContentPreview,
        markdown_context := MarkdownContext
    } = SendContext,
    AuthorData = maps:get(<<"author">>, MessageData, #{}),
    AuthorUsername = maps:get(<<"username">>, AuthorData, <<"Unknown">>),
    AuthorAvatar = maps:get(<<"avatar">>, AuthorData, null),
    AuthorAvatarUrl = resolve_avatar_url(AuthorData, AuthorAvatar),
    push_notification:build_notification_payload(#{
        message_data => MessageData,
        guild_id => GuildId,
        channel_id => ChannelId,
        message_id => MessageId,
        guild_name => GuildName,
        channel_name => ChannelName,
        author_username => AuthorUsername,
        author_avatar_url => AuthorAvatarUrl,
        target_user_id => UserId,
        badge_count => BadgeCount,
        content_preview => ContentPreview,
        markdown_context => MarkdownContext
    }).

-spec send_clear_to_user_subscriptions(
    integer(), list(), integer(), integer(), non_neg_integer()
) -> ok.
send_clear_to_user_subscriptions(UserId, Subscriptions, ChannelId, MessageId, BadgeCount) ->
    Payload = push_notification:build_clear_notification_payload(
        UserId, ChannelId, MessageId, BadgeCount
    ),
    FailedSubscriptions = send_subscriptions(UserId, Payload, Subscriptions, []),
    handle_failed_subscriptions(UserId, FailedSubscriptions).

-spec send_push_notifications(
    [integer()],
    map(),
    integer(),
    integer(),
    integer(),
    binary() | undefined,
    binary() | undefined,
    non_neg_integer()
) -> ok.
send_push_notifications(
    UserIds,
    MessageData,
    GuildId,
    ChannelId,
    MessageId,
    GuildName,
    ChannelName,
    BadgeCountsTtlSeconds
) ->
    send_push_notifications(#{
        user_ids => UserIds,
        message_data => MessageData,
        markdown_context => #{},
        guild_id => GuildId,
        channel_id => ChannelId,
        message_id => MessageId,
        guild_name => GuildName,
        channel_name => ChannelName,
        badge_counts_ttl_seconds => BadgeCountsTtlSeconds
    }).

-spec send_push_notifications(map()) -> ok.
send_push_notifications(#{
    user_ids := UserIds,
    message_data := MessageData,
    markdown_context := MarkdownContext,
    guild_id := GuildId,
    channel_id := ChannelId,
    message_id := MessageId,
    guild_name := GuildName,
    channel_name := ChannelName,
    badge_counts_ttl_seconds := BadgeCountsTtlSeconds
}) ->
    logger:debug("Push: send_push_notifications starting", #{
        message_id => MessageId,
        channel_id => ChannelId,
        guild_id => GuildId,
        user_count => length(UserIds)
    }),
    BadgeCounts = ensure_badge_counts(UserIds, BadgeCountsTtlSeconds),
    logger:debug(
        "Push: badge counts fetched",
        #{message_id => MessageId, badge_count_users => map_size(BadgeCounts)}
    ),
    push_subscriptions:fetch_and_send_subscriptions(
        UserIds,
        MessageData,
        GuildId,
        ChannelId,
        MessageId,
        GuildName,
        ChannelName,
        MarkdownContext,
        BadgeCounts
    ),
    ok.

-spec send_clear_channel_notifications(integer(), integer(), integer(), non_neg_integer()) ->
    ok.
send_clear_channel_notifications(UserId, ChannelId, MessageId, BadgeCountsTtlSeconds) ->
    BadgeCounts = ensure_badge_counts([UserId], BadgeCountsTtlSeconds),
    BadgeCount = maps:get(UserId, BadgeCounts, 0),
    push_subscriptions:fetch_and_send_clear_notification(
        UserId, ChannelId, MessageId, BadgeCount
    ),
    ok.

-spec resolve_avatar_url(map(), binary() | null) -> binary().
resolve_avatar_url(AuthorData, null) ->
    default_avatar_url(author_id_binary(AuthorData));
resolve_avatar_url(AuthorData, Hash) ->
    case author_id_binary(AuthorData) of
        undefined -> default_avatar_url(undefined);
        UserId -> push_utils:construct_avatar_url(UserId, Hash)
    end.

-spec author_id_binary(map()) -> binary() | undefined.
author_id_binary(AuthorData) ->
    case snowflake_id:parse_optional(maps:get(<<"id">>, AuthorData, undefined)) of
        undefined -> undefined;
        UserId -> integer_to_binary(UserId)
    end.

-spec default_avatar_url(binary() | undefined) -> binary().
default_avatar_url(undefined) ->
    push_utils:get_default_avatar_url(<<>>);
default_avatar_url(UserId) ->
    push_utils:get_default_avatar_url(UserId).

-spec handle_failed_subscriptions(integer(), list()) -> ok.
handle_failed_subscriptions(_UserId, []) ->
    ok;
handle_failed_subscriptions(UserId, FailedSubscriptions) ->
    logger:debug(
        "Push: removing failed subscriptions",
        #{user_id => UserId, failed_count => length(FailedSubscriptions)}
    ),
    _ = push_subscriptions:delete_failed_subscriptions(FailedSubscriptions),
    ok.

-spec send_subscriptions(integer(), map(), list(), [map()]) -> [map()].
send_subscriptions(_UserId, _Payload, [], FailedAcc) ->
    lists:reverse(FailedAcc);
send_subscriptions(UserId, Payload, [Subscription | Rest], FailedAcc) ->
    case send_notification_to_subscription(UserId, Subscription, Payload) of
        {true, FailedSubscription} ->
            send_subscriptions(UserId, Payload, Rest, [FailedSubscription | FailedAcc]);
        false ->
            send_subscriptions(UserId, Payload, Rest, FailedAcc)
    end.

-spec send_notification_to_subscription(integer(), map(), map()) -> false | {true, map()}.
send_notification_to_subscription(UserId, Subscription, Payload) ->
    logger:debug("Push: sending to subscription", #{
        user_id => UserId,
        endpoint => maps:get(<<"endpoint">>, Subscription, undefined),
        platform => subscription_platform(Subscription)
    }),
    case subscription_platform(Subscription) of
        <<"web_push">> ->
            push_sender_delivery:send_webpush_notification(UserId, Subscription, Payload);
        <<"android_unified_push">> ->
            push_sender_delivery:send_webpush_notification(UserId, Subscription, Payload);
        <<"android_fcm">> ->
            push_fcm:send(UserId, Subscription, Payload);
        <<"ios_apns">> ->
            push_apns:send(UserId, Subscription, Payload);
        Platform ->
            logger:warning(
                "Push: unsupported subscription platform",
                #{user_id => UserId, platform => Platform}
            ),
            false
    end.

-spec subscription_platform(map()) -> binary().
subscription_platform(Subscription) ->
    Platform = maps:get(<<"platform">>, Subscription, <<"web_push">>),
    push_utils:normalize_binary(Platform, <<"web_push">>).

-spec ensure_badge_counts([integer()], non_neg_integer()) -> map().
ensure_badge_counts(UserIds, TTL) ->
    Now = erlang:system_time(second),
    {CachedCounts, Missing} = lists:foldl(
        fun(UserId, {Acc, MissingAcc}) ->
            check_badge_cache(UserId, TTL, Now, Acc, MissingAcc)
        end,
        {#{}, []},
        UserIds
    ),
    case lists:usort(Missing) of
        [] -> CachedCounts;
        UniqueMissing -> fetch_badge_counts(UniqueMissing, CachedCounts, Now)
    end.

-spec check_badge_cache(integer(), non_neg_integer(), integer(), map(), [integer()]) ->
    {map(), [integer()]}.
check_badge_cache(UserId, TTL, Now, Acc, MissingAcc) ->
    case push_ets_cache:get_badge_count(UserId) of
        {Count, Timestamp} when TTL > 0, Now - Timestamp < TTL ->
            {Acc#{UserId => Count}, MissingAcc};
        _ ->
            {Acc, [UserId | MissingAcc]}
    end.

-spec fetch_badge_counts([integer()], map(), integer()) -> map().
fetch_badge_counts(UserIds, Counts, CachedAt) ->
    Batches = chunk_badge_user_ids(UserIds, badge_fetch_batch_size(), []),
    {Counted, FailedBatches, DefaultedUsers, _Consecutive} =
        fetch_badge_count_batches(Batches, {Counts, 0, 0, 0}, CachedAt),
    report_badge_fetch_failures(FailedBatches, DefaultedUsers),
    Counted.

-type badge_batch_acc() :: {map(), non_neg_integer(), non_neg_integer(), non_neg_integer()}.

-spec fetch_badge_count_batches([[integer()]], badge_batch_acc(), integer()) ->
    badge_batch_acc().
fetch_badge_count_batches(Batches, Acc, CachedAt) ->
    Deadline = erlang:monotonic_time(millisecond) + badge_fetch_budget_ms(),
    fetch_badge_count_batches(Batches, Acc, CachedAt, Deadline).

-spec fetch_badge_count_batches([[integer()]], badge_batch_acc(), integer(), integer()) ->
    badge_batch_acc().
fetch_badge_count_batches([], Acc, _CachedAt, _Deadline) ->
    Acc;
fetch_badge_count_batches(Remaining, Acc, CachedAt, Deadline) ->
    case badge_fetch_exhausted(Acc, Deadline) of
        true -> abandon_badge_batches(Remaining, Acc);
        false -> fetch_next_badge_batch(Remaining, Acc, CachedAt, Deadline)
    end.

-spec badge_fetch_exhausted(badge_batch_acc(), integer()) -> boolean().
badge_fetch_exhausted({_Counts, _FailedBatches, _DefaultedUsers, Consecutive}, Deadline) ->
    Consecutive >= ?BADGE_FETCH_MAX_CONSECUTIVE_FAILURES orelse
        erlang:monotonic_time(millisecond) >= Deadline.

-spec abandon_badge_batches([[integer()]], badge_batch_acc()) -> badge_batch_acc().
abandon_badge_batches(Remaining, {Counts, FailedBatches, DefaultedUsers, Consecutive}) ->
    {Counts, FailedBatches + length(Remaining),
        DefaultedUsers + badge_batched_user_count(Remaining, 0), Consecutive}.

-spec badge_batched_user_count([[integer()]], non_neg_integer()) -> non_neg_integer().
badge_batched_user_count([], Acc) ->
    Acc;
badge_batched_user_count([Batch | Rest], Acc) ->
    badge_batched_user_count(Rest, Acc + length(Batch)).

-spec fetch_next_badge_batch([[integer()]], badge_batch_acc(), integer(), integer()) ->
    badge_batch_acc().
fetch_next_badge_batch(
    [Batch | Rest], {Counts, FailedBatches, DefaultedUsers, Consecutive}, CachedAt, Deadline
) ->
    Request = #{
        <<"type">> => <<"get_badge_counts">>,
        <<"user_ids">> => [integer_to_binary(UserId) || UserId <- Batch]
    },
    case rpc_client:call(Request) of
        {ok, Data} ->
            BadgeData = maps:get(<<"badge_counts">>, Data, #{}),
            Merged = merge_badge_data(Batch, BadgeData, Counts, CachedAt),
            fetch_badge_count_batches(
                Rest, {Merged, FailedBatches, DefaultedUsers, 0}, CachedAt, Deadline
            );
        {error, _Reason} ->
            fetch_badge_count_batches(
                Rest,
                {Counts, FailedBatches + 1, DefaultedUsers + length(Batch), Consecutive + 1},
                CachedAt,
                Deadline
            )
    end.

-spec report_badge_fetch_failures(non_neg_integer(), non_neg_integer()) -> ok.
report_badge_fetch_failures(0, _DefaultedUsers) ->
    ok;
report_badge_fetch_failures(FailedBatches, DefaultedUsers) ->
    bump_counter(badge_fetch_calls_failed, FailedBatches),
    bump_counter(badge_fetch_users_defaulted, DefaultedUsers),
    logger:warning("Push: badge count batches failed; users default to zero badge", #{
        failed_batches => FailedBatches, users_defaulted => DefaultedUsers
    }).

-spec bump_counter(atom(), integer()) -> ok.
bump_counter(Key, Delta) ->
    try
        _ = ets:update_counter(?PUSH_COUNTERS, Key, {2, Delta}),
        ok
    catch
        error:badarg -> seed_and_bump_counter(Key, Delta)
    end.

-spec seed_and_bump_counter(atom(), integer()) -> ok.
seed_and_bump_counter(Key, Delta) ->
    try
        _ = ets:insert_new(?PUSH_COUNTERS, {Key, 0}),
        _ = ets:update_counter(?PUSH_COUNTERS, Key, {2, Delta}),
        ok
    catch
        error:badarg -> ok
    end.

-spec chunk_badge_user_ids([integer()], pos_integer(), [[integer()]]) -> [[integer()]].
chunk_badge_user_ids([], _BatchSize, Acc) ->
    lists:reverse(Acc);
chunk_badge_user_ids(UserIds, BatchSize, Acc) ->
    {Batch, Rest} = take_badge_user_id_batch(UserIds, BatchSize, []),
    chunk_badge_user_ids(Rest, BatchSize, [Batch | Acc]).

-spec take_badge_user_id_batch([integer()], non_neg_integer(), [integer()]) ->
    {[integer()], [integer()]}.
take_badge_user_id_batch(Rest, 0, Acc) ->
    {lists:reverse(Acc), Rest};
take_badge_user_id_batch([], _Remaining, Acc) ->
    {lists:reverse(Acc), []};
take_badge_user_id_batch([UserId | Rest], Remaining, Acc) ->
    take_badge_user_id_batch(Rest, Remaining - 1, [UserId | Acc]).

-spec badge_fetch_batch_size() -> pos_integer().
badge_fetch_batch_size() ->
    case application:get_env(fluxer_gateway, push_badge_fetch_batch_size, undefined) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> ?DEFAULT_BADGE_FETCH_BATCH
    end.

-spec badge_fetch_budget_ms() -> pos_integer().
badge_fetch_budget_ms() ->
    case application:get_env(fluxer_gateway, push_badge_fetch_budget_ms, undefined) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> ?DEFAULT_BADGE_FETCH_BUDGET_MS
    end.

-spec merge_badge_data([integer()], map(), map(), integer()) -> map().
merge_badge_data(UserIds, BadgeData, Counts, CachedAt) ->
    lists:foldl(
        fun(UserId, Acc) ->
            UserIdBin = integer_to_binary(UserId),
            Count = normalize_badge_count(maps:get(UserIdBin, BadgeData, 0)),
            push_ets_cache:put_badge_count(UserId, Count, CachedAt),
            Acc#{UserId => Count}
        end,
        Counts,
        UserIds
    ).

-spec normalize_badge_count(integer() | term()) -> non_neg_integer().
normalize_badge_count(Value) when is_integer(Value), Value >= 0 -> Value;
normalize_badge_count(_) -> 0.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

chunk_badge_user_ids_uses_bounded_batches_test() ->
    ?assertEqual([[1, 2], [3, 4], [5]], chunk_badge_user_ids([1, 2, 3, 4, 5], 2, [])),
    ?assertEqual([], chunk_badge_user_ids([], 2, [])).

fetch_badge_counts_in_batches_keeps_successful_batches_test() ->
    ok = meck:new(rpc_client, [passthrough, no_link]),
    ok = meck:new(push_ets_cache, [passthrough, no_link]),
    application:set_env(fluxer_gateway, push_badge_fetch_batch_size, 2),
    try
        ok = meck:expect(push_ets_cache, put_badge_count, fun(_UserId, _Count, _At) -> ok end),
        ok = meck:expect(rpc_client, call, fun(#{<<"user_ids">> := Ids}) ->
            case Ids of
                [<<"1">>, <<"2">>] ->
                    {ok, #{<<"badge_counts">> => #{<<"1">> => 3, <<"2">> => 4}}};
                _ ->
                    {error, unavailable}
            end
        end),
        ?assertEqual(#{1 => 3, 2 => 4}, fetch_badge_counts([1, 2, 3, 4], #{}, 0))
    after
        application:unset_env(fluxer_gateway, push_badge_fetch_batch_size),
        meck:unload(push_ets_cache),
        meck:unload(rpc_client)
    end.

fetch_badge_count_batches_stops_after_consecutive_failures_test() ->
    ok = meck:new(rpc_client, [passthrough, no_link]),
    try
        ok = meck:expect(rpc_client, call, fun(_Req) -> {error, unavailable} end),
        Batches = [[N] || N <- lists:seq(1, 10)],
        ?assertEqual(
            {#{}, 10, 10, ?BADGE_FETCH_MAX_CONSECUTIVE_FAILURES},
            fetch_badge_count_batches(Batches, {#{}, 0, 0, 0}, 0)
        ),
        ?assertEqual(?BADGE_FETCH_MAX_CONSECUTIVE_FAILURES, length(meck:history(rpc_client)))
    after
        meck:unload(rpc_client)
    end.

fetch_badge_count_batches_stops_at_the_wall_clock_budget_test() ->
    ok = meck:new(rpc_client, [passthrough, no_link]),
    application:set_env(fluxer_gateway, push_badge_fetch_budget_ms, 1),
    try
        ok = meck:expect(rpc_client, call, fun(_Req) ->
            {ok, #{<<"badge_counts">> => #{}}}
        end),
        Batches = [[N] || N <- lists:seq(1, 10)],
        Deadline = erlang:monotonic_time(millisecond) - 1,
        ?assertEqual(
            {#{}, 10, 10, 0},
            fetch_badge_count_batches(Batches, {#{}, 0, 0, 0}, 0, Deadline)
        ),
        ?assertEqual(0, length(meck:history(rpc_client)))
    after
        application:unset_env(fluxer_gateway, push_badge_fetch_budget_ms),
        meck:unload(rpc_client)
    end.

badge_fetch_defaults_missing_users_to_zero_test() ->
    ok = meck:new(rpc_client, [passthrough, no_link]),
    ok = meck:new(push_ets_cache, [passthrough, no_link]),
    try
        ok = meck:expect(push_ets_cache, put_badge_count, fun(_UserId, _Count, _At) -> ok end),
        ok = meck:expect(rpc_client, call, fun(#{<<"user_ids">> := Ids}) ->
            ?assertEqual([<<"1">>, <<"2">>, <<"3">>], Ids),
            {ok, #{<<"badge_counts">> => #{<<"1">> => 1}}}
        end),
        ?assertEqual(#{1 => 1, 2 => 0, 3 => 0}, fetch_badge_counts([1, 2, 3], #{}, 0))
    after
        meck:unload(push_ets_cache),
        meck:unload(rpc_client)
    end.

badge_fetch_failure_counts_defaulted_users_test() ->
    ok = meck:new(rpc_client, [passthrough, no_link]),
    ensure_test_counter_table(),
    ok = reset_test_counter(badge_fetch_calls_failed),
    ok = reset_test_counter(badge_fetch_users_defaulted),
    try
        ok = meck:expect(rpc_client, call, fun(_Req) -> {error, unavailable} end),
        ?assertEqual(#{}, fetch_badge_counts([1, 2, 3], #{}, 0)),
        ?assertEqual(1, counter_value(badge_fetch_calls_failed)),
        ?assertEqual(3, counter_value(badge_fetch_users_defaulted))
    after
        meck:unload(rpc_client)
    end.

counter_value(Key) ->
    try ets:lookup(?PUSH_COUNTERS, Key) of
        [{Key, Value}] when is_integer(Value) -> Value;
        _ -> 0
    catch
        error:badarg -> 0
    end.

ensure_test_counter_table() ->
    case ets:info(?PUSH_COUNTERS, name) of
        undefined ->
            _ = ets:new(?PUSH_COUNTERS, [named_table, public, set, {write_concurrency, true}]),
            ok;
        _ ->
            ok
    end.

reset_test_counter(Key) ->
    try ets:insert(?PUSH_COUNTERS, {Key, 0}) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-endif.
