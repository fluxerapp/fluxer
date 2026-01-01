%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(push_sender).

-import(push_cache, [
    get_user_badge_count/2,
    cache_user_badge_count/4
]).
-import(rpc_client, [call/1]).

-export([send_to_user_subscriptions/9, send_push_notifications/8]).

send_to_user_subscriptions(
    UserId,
    Subscriptions,
    MessageData,
    GuildId,
    ChannelId,
    MessageId,
    GuildName,
    ChannelName,
    BadgeCount
) ->
    AuthorData = maps:get(<<"author">>, MessageData, #{}),
    AuthorUsername = maps:get(<<"username">>, AuthorData, <<"Unknown">>),
    AuthorAvatar = maps:get(<<"avatar">>, AuthorData, null),

    AuthorAvatarUrl =
        case AuthorAvatar of
            null -> push_utils:get_default_avatar_url(maps:get(<<"id">>, AuthorData, <<"0">>));
            Hash -> push_utils:construct_avatar_url(maps:get(<<"id">>, AuthorData, <<"0">>), Hash)
        end,

    NotificationPayload = push_notification:build_notification_payload(
        MessageData,
        GuildId,
        ChannelId,
        MessageId,
        GuildName,
        ChannelName,
        AuthorUsername,
        AuthorAvatarUrl,
        UserId,
        BadgeCount
    ),

    case ensure_vapid_credentials() of
        {ok, VapidEmail, VapidPublicKey, VapidPrivateKey} ->
            FailedSubscriptions = lists:filtermap(
                fun(Sub) ->
                    send_notification_to_subscription(
                        UserId,
                        Sub,
                        NotificationPayload,
                        VapidEmail,
                        VapidPublicKey,
                        VapidPrivateKey
                    )
                end,
                Subscriptions
            ),

            case FailedSubscriptions of
                [] ->
                    ok;
                _ ->
                    logger:debug("[push] Deleting ~p failed subscriptions", [
                        length(FailedSubscriptions)
                    ]),
                    push_subscriptions:delete_failed_subscriptions(FailedSubscriptions)
            end;
        {error, Reason} ->
            logger:warning("[push] %s - skipping push send", [Reason])
    end.
send_push_notifications(
    UserIds, MessageData, GuildId, ChannelId, MessageId, GuildName, ChannelName, State
) ->
    {BadgeCounts, StateWithBadgeCounts} = ensure_badge_counts(UserIds, State),
    {UncachedUsers, CachedState} = lists:foldl(
        fun(UserId, {Uncached, S}) ->
            Key = {subscriptions, UserId},
            PushSubscriptionsCache = maps:get(push_subscriptions_cache, S, #{}),
            case maps:is_key(Key, PushSubscriptionsCache) of
                true ->
                    Subscriptions = push_cache:get_user_push_subscriptions(UserId, S),
                    logger:debug(
                        "[push] Using cached subscriptions for user ~p (~p subs)",
                        [UserId, length(Subscriptions)]
                    ),
                    BadgeCount = maps:get(UserId, BadgeCounts, 0),
                    case Subscriptions of
                        [] ->
                            ok;
                        _ ->
                            send_to_user_subscriptions(
                                UserId,
                                Subscriptions,
                                MessageData,
                                GuildId,
                                ChannelId,
                                MessageId,
                                GuildName,
                                ChannelName,
                                BadgeCount
                            )
                    end,
                    {Uncached, S};
                false ->
                    {[UserId | Uncached], S}
            end
        end,
        {[], StateWithBadgeCounts},
        UserIds
    ),

    case UncachedUsers of
        [] ->
            CachedState;
        _ ->
            push_subscriptions:fetch_and_send_subscriptions(
                UncachedUsers,
                MessageData,
                GuildId,
                ChannelId,
                MessageId,
                GuildName,
                ChannelName,
                CachedState,
                BadgeCounts
            )
    end.

ensure_badge_counts(UserIds, State) ->
    Now = erlang:system_time(second),
    TTL = maps:get(badge_counts_ttl_seconds, State, 0),
    {CachedCounts, Missing} =
        lists:foldl(
            fun(UserId, {Acc, MissingAcc}) ->
                case get_user_badge_count(UserId, State) of
                    {Count, Timestamp} when TTL > 0, Now - Timestamp < TTL ->
                        {maps:put(UserId, Count, Acc), MissingAcc};
                    _ ->
                        {Acc, [UserId | MissingAcc]}
                end
            end,
            {#{}, []},
            UserIds
        ),
    UniqueMissing = lists:usort(Missing),
    case UniqueMissing of
        [] ->
            {CachedCounts, State};
        _ ->
            fetch_badge_counts(UniqueMissing, CachedCounts, State, Now)
    end.

fetch_badge_counts(UserIds, Counts, State, CachedAt) ->
    Request = #{
        <<"type">> => <<"get_badge_counts">>,
        <<"user_ids">> => [integer_to_binary(UserId) || UserId <- UserIds]
    },
    case call(Request) of
        {ok, Data} ->
            BadgeData = maps:get(<<"badge_counts">>, Data, #{}),
            lists:foldl(
                fun(UserId, {Acc, S}) ->
                    UserIdBin = integer_to_binary(UserId),
                    Count = normalize_badge_count(maps:get(UserIdBin, BadgeData, 0)),
                    NewState = cache_user_badge_count(UserId, Count, CachedAt, S),
                    {maps:put(UserId, Count, Acc), NewState}
                end,
                {Counts, State},
                UserIds
            );
        {error, Reason} ->
            logger:error("[push] Failed to fetch badge counts: ~p", [Reason]),
            {Counts, State}
    end.

normalize_badge_count(Value) when is_integer(Value), Value >= 0 ->
    Value;
normalize_badge_count(_) ->
    0.


-define(PUSH_TTL, <<"86400">>).

ensure_vapid_credentials() ->
    Email = fluxer_gateway_env:get(vapid_email),
    Public = fluxer_gateway_env:get(vapid_public_key),
    Private = fluxer_gateway_env:get(vapid_private_key),
    case {Email, Public, Private} of
        {Email0, Public0, Private0}
        when is_binary(Email0) andalso is_binary(Public0) andalso is_binary(Private0) andalso
                 byte_size(Public0) > 0 andalso byte_size(Private0) > 0 ->
            {ok, Email0, Public0, Private0};
        _ ->
            {error, "Missing VAPID credentials"}
    end.

send_notification_to_subscription(
    UserId,
    Subscription,
    Payload,
    VapidEmail,
    VapidPublicKey,
    VapidPrivateKey
) ->
    case extract_subscription_fields(Subscription) of
        {ok, Endpoint, P256dhKey, AuthKey, SubscriptionId} ->
            logger:debug("[push] Sending to endpoint ~p for user ~p", [Endpoint, UserId]),
            VapidClaims = #{
                <<"sub">> => <<"mailto:", VapidEmail/binary>>,
                <<"aud">> => push_utils:extract_origin(Endpoint),
                <<"exp">> => erlang:system_time(second) + 43200
            },
            VapidTokenResult =
                try
                    {ok, push_utils:generate_vapid_token(VapidClaims, VapidPublicKey, VapidPrivateKey)}
                catch
                    C:R ->
                        logger:error("[push] VAPID token generation failed: ~p:~p", [C, R]),
                        {error, {C, R}}
                end,
            case VapidTokenResult of
                {ok, VapidToken} ->
                    case push_utils:encrypt_payload(jsx:encode(Payload), P256dhKey, AuthKey, 4096) of
                        {ok, EncryptedBody} ->
                            Headers = build_push_headers(VapidToken, VapidPublicKey),
                            handle_push_response(UserId, SubscriptionId, Endpoint, Headers, EncryptedBody);
                        {error, EncryptError} ->
                            logger:error("[push] Failed to encrypt payload: ~p", [EncryptError]),
                            metrics_client:counter(<<"push.failure">>, #{<<"reason">> => <<"encryption_error">>}),
                            false
                    end;
                {error, _} ->
                    metrics_client:counter(<<"push.failure">>, #{<<"reason">> => <<"vapid_error">>}),
                    false
            end;
        {error, Reason} ->
            logger:error("[push] Invalid subscription for user ~p: ~s", [UserId, Reason]),
            metrics_client:counter(<<"push.failure">>, #{<<"reason">> => <<"invalid_subscription">>}),
            false
    end.

extract_subscription_fields(Subscription) ->
    case
        {
            maps:get(<<"endpoint">>, Subscription, undefined),
            maps:get(<<"p256dh_key">>, Subscription, undefined),
            maps:get(<<"auth_key">>, Subscription, undefined),
            maps:get(<<"subscription_id">>, Subscription, undefined)
        }
    of
        {Endpoint, P256dhKey, AuthKey, SubscriptionId}
        when is_binary(Endpoint) andalso is_binary(P256dhKey)
                andalso is_binary(AuthKey) andalso is_binary(SubscriptionId) ->
            {ok, Endpoint, P256dhKey, AuthKey, SubscriptionId};
        _ ->
            {error, "missing keys"}
    end.

build_push_headers(VapidToken, VapidPublicKey) ->
    [
        {<<"TTL">>, ?PUSH_TTL},
        {<<"Content-Type">>, <<"application/octet-stream">>},
        {<<"Content-Encoding">>, <<"aes128gcm">>},
        {<<"Authorization">>,
            <<"vapid t=", VapidToken/binary, ", k=", VapidPublicKey/binary>>}
    ].

handle_push_response(UserId, SubscriptionId, Endpoint, Headers, Body) ->
    case hackney:request(post, binary_to_list(Endpoint), Headers, Body, []) of
        {ok, Status, _, _} when Status >= 200, Status < 300 ->
            logger:debug("[push] Push sent successfully (%p) for user %p", [Status, UserId]),
            metrics_client:counter(<<"push.success">>),
            false;
        {ok, 410, _, _} ->
            logger:debug("[push] Subscription expired (410) for user ~p", [UserId]),
            metrics_client:counter(<<"push.failure">>, #{<<"reason">> => <<"expired">>}),
            {true, delete_payload(UserId, SubscriptionId)};
        {ok, 404, _, _} ->
            logger:debug("[push] Subscription not found (404) for user ~p", [UserId]),
            metrics_client:counter(<<"push.failure">>, #{<<"reason">> => <<"not_found">>}),
            {true, delete_payload(UserId, SubscriptionId)};
        {ok, Status, _, ClientRef} ->
            {ok, ErrorBody} = hackney:body(ClientRef),
            logger:error("[push] Push failed with status ~p for user ~p (%s)", [
                Status,
                UserId,
                ErrorBody
            ]),
            metrics_client:counter(<<"push.failure">>, #{<<"reason">> => <<"http_error">>}),
            false;
        {error, Reason} ->
            logger:error("[push] Failed to send push for user ~p: ~p", [UserId, Reason]),
            metrics_client:counter(<<"push.failure">>, #{<<"reason">> => <<"network_error">>}),
            false
    end.

delete_payload(UserId, SubscriptionId) ->
    #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"subscription_id">> => SubscriptionId
    }.
