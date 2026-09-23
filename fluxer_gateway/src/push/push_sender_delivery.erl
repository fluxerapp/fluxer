%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_sender_delivery).
-typing([eqwalizer]).

-export([
    send_webpush_notification/3,
    is_transient_error/1
]).

-export_type([push_response/0]).

-define(PUSH_TTL, <<"86400">>).
-define(ALERT_URGENCY, <<"high">>).
-define(CLEAR_URGENCY, <<"low">>).
-define(MAX_TRANSIENT_RETRIES, 2).
-define(MAX_OVERLOAD_RETRIES, 3).
-define(BASE_RETRY_DELAY_MS, 200).
-define(MAX_RETRY_DELAY_MS, 2000).
-define(OVERLOAD_BASE_DELAY_MS, 250).
-define(OVERLOAD_MAX_DELAY_MS, 4000).
-define(VAPID_TOKEN_TTL_SECONDS, 43200).
-define(VAPID_TOKEN_SKEW_SECONDS, 60).

-type push_response() :: {ok, integer(), term(), binary()} | {error, term()}.

-spec send_webpush_notification(integer(), map(), map()) -> false | {true, map()}.
send_webpush_notification(UserId, Subscription, Payload) ->
    case extract_subscription_fields(Subscription) of
        {ok, Endpoint, P256dhKey, AuthKey, SubscriptionId} ->
            send_to_allowed_endpoint(
                UserId, Endpoint, P256dhKey, AuthKey, SubscriptionId, Payload
            );
        {error, _Reason} ->
            false
    end.

-spec send_to_allowed_endpoint(integer(), binary(), binary(), binary(), binary(), map()) ->
    false | {true, map()}.
send_to_allowed_endpoint(UserId, Endpoint, P256dhKey, AuthKey, SubscriptionId, Payload) ->
    case push_endpoint_guard:check(Endpoint) of
        ok ->
            send_with_vapid(UserId, Endpoint, P256dhKey, AuthKey, SubscriptionId, Payload);
        {error, Reason} ->
            log_endpoint_rejected(UserId, SubscriptionId, Reason),
            false
    end.

-spec log_endpoint_rejected(integer(), binary(), term()) -> ok.
log_endpoint_rejected(UserId, SubscriptionId, Reason) ->
    logger:debug(
        "Push: endpoint rejected",
        #{user_id => UserId, subscription_id => SubscriptionId, reason => Reason}
    ),
    ok.

-spec send_with_vapid(integer(), binary(), binary(), binary(), binary(), map()) ->
    false | {true, map()}.
send_with_vapid(UserId, Endpoint, P256dhKey, AuthKey, SubscriptionId, Payload) ->
    maybe
        {ok, VapidEmail, VapidPublicKey, VapidPrivateKey} ?= ensure_vapid_credentials(),
        Aud = push_utils:extract_origin(Endpoint),
        {ok, VapidToken} ?=
            cached_vapid_token(Aud, VapidEmail, VapidPublicKey, VapidPrivateKey),
        Headers = build_push_headers(VapidToken, VapidPublicKey, push_urgency(Payload)),
        send_encrypted_push(
            UserId,
            SubscriptionId,
            Endpoint,
            Headers,
            Payload,
            P256dhKey,
            AuthKey,
            push_sender_retry:initial_record_size(),
            0
        )
    else
        {error, {vapid_credentials, Reason}} ->
            log_vapid_unavailable(UserId, Reason),
            false;
        {error, _} ->
            false
    end.

-spec cached_vapid_token(binary(), binary(), binary(), binary()) ->
    {ok, binary()} | {error, term()}.
cached_vapid_token(Aud, VapidEmail, VapidPublicKey, VapidPrivateKey) ->
    cached_vapid_token(
        Aud,
        VapidEmail,
        VapidPublicKey,
        VapidPrivateKey,
        erlang:system_time(second)
    ).

-spec cached_vapid_token(binary(), binary(), binary(), binary(), integer()) ->
    {ok, binary()} | {error, term()}.
cached_vapid_token(Aud, VapidEmail, VapidPublicKey, VapidPrivateKey, Now) ->
    CacheKey = vapid_cache_key(Aud, VapidEmail, VapidPublicKey),
    case push_ets_cache:get_bearer_token(CacheKey) of
        {ok, Token, ExpiresAt} when
            is_binary(Token), ExpiresAt - ?VAPID_TOKEN_SKEW_SECONDS > Now
        ->
            {ok, Token};
        _ ->
            generate_cached_vapid_token(
                CacheKey, Aud, VapidEmail, VapidPublicKey, VapidPrivateKey, Now
            )
    end.

-spec generate_cached_vapid_token(
    term(), binary(), binary(), binary(), binary(), integer()
) -> {ok, binary()} | {error, term()}.
generate_cached_vapid_token(CacheKey, Aud, VapidEmail, VapidPublicKey, VapidPrivateKey, Now) ->
    ExpiresAt = Now + ?VAPID_TOKEN_TTL_SECONDS,
    VapidClaims = #{
        <<"sub">> => <<"mailto:", VapidEmail/binary>>,
        <<"aud">> => Aud,
        <<"exp">> => ExpiresAt
    },
    case safe_generate_vapid_token(VapidClaims, VapidPublicKey, VapidPrivateKey) of
        {ok, Token} ->
            push_ets_cache:put_bearer_token(CacheKey, Token, ExpiresAt),
            {ok, Token};
        {error, Reason} ->
            {error, Reason}
    end.

-spec vapid_cache_key(binary(), binary(), binary()) -> term().
vapid_cache_key(Aud, VapidEmail, VapidPublicKey) ->
    {?MODULE, vapid_token, Aud, VapidEmail, VapidPublicKey}.

-spec safe_generate_vapid_token(map(), binary(), binary()) -> {ok, binary()} | {error, term()}.
safe_generate_vapid_token(VapidClaims, VapidPublicKey, VapidPrivateKey) ->
    try
        {ok, push_utils:generate_vapid_token(VapidClaims, VapidPublicKey, VapidPrivateKey)}
    catch
        error:Err -> {error, {error, Err}};
        throw:Thr -> {error, {throw, Thr}};
        exit:Ex -> {error, {exit, Ex}}
    end.

-spec send_encrypted_push(
    integer(),
    binary(),
    binary(),
    [{binary(), binary()}],
    map(),
    binary(),
    binary(),
    pos_integer(),
    non_neg_integer()
) -> false | {true, map()}.
send_encrypted_push(
    UserId,
    SubscriptionId,
    Endpoint,
    Headers,
    Payload,
    P256dhKey,
    AuthKey,
    RecordSize,
    Attempt
) ->
    PayloadJson = fit_payload_json(Payload, RecordSize),
    case push_utils:encrypt_payload(PayloadJson, P256dhKey, AuthKey, RecordSize) of
        {ok, EncryptedBody} ->
            Response = request_push_endpoint(Endpoint, Headers, EncryptedBody),
            handle_encrypted_response(
                UserId,
                SubscriptionId,
                Endpoint,
                Headers,
                Payload,
                P256dhKey,
                AuthKey,
                RecordSize,
                Attempt,
                Response
            );
        {error, _EncryptError} ->
            false
    end.

-spec fit_payload_json(map(), pos_integer()) -> binary().
fit_payload_json(Payload, RecordSize) ->
    push_notification:fit_payload_json(Payload, push_utils:plaintext_budget(RecordSize)).

-spec push_urgency(map()) -> binary().
push_urgency(Payload) ->
    case push_notification:is_clear(Payload) of
        true -> ?CLEAR_URGENCY;
        false -> ?ALERT_URGENCY
    end.

-spec handle_encrypted_response(
    integer(),
    binary(),
    binary(),
    [{binary(), binary()}],
    map(),
    binary(),
    binary(),
    pos_integer(),
    non_neg_integer(),
    push_response()
) -> false | {true, map()}.
handle_encrypted_response(
    UserId,
    SubscriptionId,
    Endpoint,
    Headers,
    Payload,
    P256dhKey,
    AuthKey,
    RecordSize,
    Attempt,
    Response
) ->
    Ctx = #{
        user_id => UserId,
        subscription_id => SubscriptionId,
        endpoint => Endpoint,
        headers => Headers,
        payload => Payload,
        p256dh_key => P256dhKey,
        auth_key => AuthKey
    },
    RetryResult = push_sender_retry:maybe_retry_with_smaller_record_size(
        Response, RecordSize, Attempt
    ),
    case RetryResult of
        {retry, NextRecordSize} ->
            retry_encrypted_push(Ctx, NextRecordSize, Attempt);
        no_retry ->
            maybe_retry_transient(Ctx, RecordSize, Attempt, Response)
    end.

-spec maybe_retry_transient(map(), pos_integer(), non_neg_integer(), push_response()) ->
    false | {true, map()}.
maybe_retry_transient(
    #{user_id := UserId, subscription_id := SubscriptionId} = Ctx,
    RecordSize,
    Attempt,
    Response
) ->
    case is_local_backpressure(Response) of
        true ->
            maybe_retry_overload(Ctx, RecordSize, Response);
        false ->
            maybe_retry_transient_error(
                Ctx, RecordSize, Attempt, Response, UserId, SubscriptionId
            )
    end.

-spec maybe_retry_transient_error(
    map(), pos_integer(), non_neg_integer(), push_response(), integer(), binary()
) -> false | {true, map()}.
maybe_retry_transient_error(Ctx, RecordSize, Attempt, Response, UserId, SubscriptionId) ->
    case is_transient_error(Response) andalso Attempt < ?MAX_TRANSIENT_RETRIES of
        true ->
            Delay = retry_delay(Attempt),
            ok = gateway_retry_timer:wait(Delay),
            retry_encrypted_push(Ctx, RecordSize, Attempt);
        false ->
            handle_push_response(UserId, SubscriptionId, Response)
    end.

-spec is_local_backpressure(push_response()) -> boolean().
is_local_backpressure({error, overloaded}) -> true;
is_local_backpressure({error, circuit_open}) -> true;
is_local_backpressure(_) -> false.

-spec maybe_retry_overload(map(), pos_integer(), push_response()) -> false | {true, map()}.
maybe_retry_overload(Ctx, RecordSize, Response) ->
    OverloadAttempt = maps:get(overload_attempt, Ctx, 0),
    case OverloadAttempt < ?MAX_OVERLOAD_RETRIES of
        true ->
            Delay = overload_retry_delay(OverloadAttempt),
            ok = gateway_retry_timer:wait(Delay),
            retry_overload(Ctx, RecordSize, OverloadAttempt + 1);
        false ->
            log_overload_dropped(Ctx, Response),
            false
    end.

-spec retry_overload(map(), pos_integer(), non_neg_integer()) -> false | {true, map()}.
retry_overload(
    #{
        endpoint := Endpoint,
        headers := Headers,
        payload := Payload,
        p256dh_key := P256dhKey,
        auth_key := AuthKey
    } = Ctx,
    RecordSize,
    OverloadAttempt
) ->
    PayloadJson = fit_payload_json(Payload, RecordSize),
    case push_utils:encrypt_payload(PayloadJson, P256dhKey, AuthKey, RecordSize) of
        {ok, EncryptedBody} ->
            Response = request_push_endpoint(Endpoint, Headers, EncryptedBody),
            handle_overload_retry_response(Ctx, RecordSize, OverloadAttempt, Response);
        {error, _EncryptError} ->
            false
    end.

-spec handle_overload_retry_response(map(), pos_integer(), non_neg_integer(), push_response()) ->
    false | {true, map()}.
handle_overload_retry_response(Ctx, RecordSize, OverloadAttempt, Response) ->
    case is_local_backpressure(Response) of
        true ->
            maybe_retry_overload(
                Ctx#{overload_attempt => OverloadAttempt}, RecordSize, Response
            );
        false ->
            handle_push_response(
                maps:get(user_id, Ctx), maps:get(subscription_id, Ctx), Response
            )
    end.

-spec overload_retry_delay(non_neg_integer()) -> pos_integer().
overload_retry_delay(Attempt) ->
    Base = min(?OVERLOAD_MAX_DELAY_MS, ?OVERLOAD_BASE_DELAY_MS * (1 bsl Attempt)),
    Jitter = rand:uniform(max(1, Base div 4)),
    min(?OVERLOAD_MAX_DELAY_MS, Base + Jitter - 1).

-spec log_overload_dropped(map(), push_response()) -> ok.
log_overload_dropped(#{user_id := UserId, subscription_id := SubscriptionId}, Response) ->
    Reason = backpressure_reason(Response),
    logger:warning(
        "Push: notification dropped after overload retries exhausted",
        #{user_id => UserId, subscription_id => SubscriptionId, reason => Reason}
    ),
    ok.

-spec backpressure_reason(push_response()) -> binary().
backpressure_reason({error, overloaded}) -> <<"client_overloaded">>;
backpressure_reason({error, circuit_open}) -> <<"circuit_open">>;
backpressure_reason(_) -> <<"backpressure">>.

-spec retry_encrypted_push(map(), pos_integer(), non_neg_integer()) -> false | {true, map()}.
retry_encrypted_push(
    #{
        user_id := UserId,
        subscription_id := SubscriptionId,
        endpoint := Endpoint,
        headers := Headers,
        payload := Payload,
        p256dh_key := P256dhKey,
        auth_key := AuthKey
    },
    RecordSize,
    Attempt
) ->
    send_encrypted_push(
        UserId,
        SubscriptionId,
        Endpoint,
        Headers,
        Payload,
        P256dhKey,
        AuthKey,
        RecordSize,
        Attempt + 1
    ).

-spec is_transient_error(push_response()) -> boolean().
is_transient_error({ok, Status, _, _}) when Status >= 500 -> true;
is_transient_error({ok, 429, _, _}) -> true;
is_transient_error({error, overloaded}) -> false;
is_transient_error({error, circuit_open}) -> false;
is_transient_error({error, timeout}) -> true;
is_transient_error({error, {timeout, _}}) -> true;
is_transient_error({error, closed}) -> true;
is_transient_error({error, econnrefused}) -> true;
is_transient_error({error, econnreset}) -> true;
is_transient_error({error, ehostunreach}) -> true;
is_transient_error({error, enetunreach}) -> true;
is_transient_error({error, etimedout}) -> true;
is_transient_error({error, {failed_connect, _}}) -> true;
is_transient_error({error, nxdomain}) -> true;
is_transient_error({error, _}) -> false;
is_transient_error(_) -> false.

-spec retry_delay(non_neg_integer()) -> pos_integer().
retry_delay(Attempt) ->
    Base = min(?MAX_RETRY_DELAY_MS, ?BASE_RETRY_DELAY_MS * (1 bsl Attempt)),
    Jitter = rand:uniform(max(1, Base div 4)),
    min(?MAX_RETRY_DELAY_MS, Base + Jitter - 1).

-spec handle_push_response(integer(), binary(), push_response()) -> false | {true, map()}.
handle_push_response(UserId, SubscriptionId, {ok, Status, _, _}) when
    Status >= 200, Status < 300
->
    logger:debug(
        "Push: delivery succeeded",
        #{user_id => UserId, subscription_id => SubscriptionId, status => Status}
    ),
    false;
handle_push_response(UserId, SubscriptionId, {ok, 410, _, _}) ->
    log_and_delete(UserId, SubscriptionId, <<"expired">>);
handle_push_response(UserId, SubscriptionId, {ok, 404, _, _}) ->
    log_and_delete(UserId, SubscriptionId, <<"not_found">>);
handle_push_response(UserId, SubscriptionId, {ok, Status, _, Body}) ->
    log_http_error(UserId, SubscriptionId, Status, Body);
handle_push_response(UserId, SubscriptionId, {error, overloaded}) ->
    log_push_error(
        UserId, SubscriptionId, <<"client_overloaded">>, "Push: HTTP client overloaded"
    );
handle_push_response(UserId, SubscriptionId, {error, circuit_open}) ->
    log_push_error(UserId, SubscriptionId, <<"circuit_open">>, "Push: circuit breaker open");
handle_push_response(UserId, SubscriptionId, {error, Reason}) ->
    logger:debug(
        "Push: network error",
        #{user_id => UserId, subscription_id => SubscriptionId, reason => Reason}
    ),
    false.

-spec log_and_delete(integer(), binary(), binary()) -> {true, map()}.
log_and_delete(UserId, SubscriptionId, Reason) ->
    logger:debug(
        "Push: subscription gone, will delete",
        #{user_id => UserId, subscription_id => SubscriptionId, reason => Reason}
    ),
    {true, delete_payload(UserId, SubscriptionId)}.

-spec log_push_error(integer(), binary(), binary(), string()) -> false.
log_push_error(UserId, SubscriptionId, _Reason, Message) ->
    logger:debug(
        Message,
        #{user_id => UserId, subscription_id => SubscriptionId}
    ),
    false.

-spec log_http_error(integer(), binary(), integer(), binary()) -> false.
log_http_error(UserId, SubscriptionId, Status, Body) ->
    logger:debug(
        "Push: delivery failed with HTTP error",
        #{user_id => UserId, subscription_id => SubscriptionId, status => Status, body => Body}
    ),
    false.

-spec ensure_vapid_credentials() ->
    {ok, binary(), binary(), binary()} | {error, {vapid_credentials, string()}}.
ensure_vapid_credentials() ->
    Email = fluxer_gateway_env:get(vapid_email),
    Public = fluxer_gateway_env:get(vapid_public_key),
    Private = fluxer_gateway_env:get(vapid_private_key),
    case {Email, Public, Private} of
        {Email0, Public0, Private0} when
            is_binary(Email0),
            is_binary(Public0),
            is_binary(Private0),
            byte_size(Public0) > 0,
            byte_size(Private0) > 0
        ->
            {ok, Email0, Public0, Private0};
        _ ->
            {error, {vapid_credentials, "Missing VAPID credentials"}}
    end.

-spec log_vapid_unavailable(integer(), term()) -> ok.
log_vapid_unavailable(UserId, Reason) ->
    Now = erlang:monotonic_time(second),
    Last =
        case persistent_term:get({?MODULE, vapid_warn_at}, undefined) of
            undefined -> 0;
            V -> V
        end,
    case Now - Last >= 60 of
        true ->
            persistent_term:put({?MODULE, vapid_warn_at}, Now),
            logger:error(
                "Push: VAPID credentials unavailable; pushes are being silently dropped",
                #{user_id => UserId, reason => Reason}
            );
        false ->
            ok
    end,
    ok.

-spec extract_subscription_fields(map()) ->
    {ok, binary(), binary(), binary(), binary()} | {error, string()}.
extract_subscription_fields(Subscription) ->
    Endpoint = maps:get(<<"endpoint">>, Subscription, undefined),
    P256dhKey = maps:get(<<"p256dh_key">>, Subscription, undefined),
    AuthKey = maps:get(<<"auth_key">>, Subscription, undefined),
    SubscriptionId = maps:get(<<"subscription_id">>, Subscription, undefined),
    case {Endpoint, P256dhKey, AuthKey, SubscriptionId} of
        {E, P, A, S} when is_binary(E), is_binary(P), is_binary(A), is_binary(S) ->
            {ok, E, P, A, S};
        _ ->
            {error, "missing keys"}
    end.

-spec build_push_headers(binary(), binary(), binary()) -> [{binary(), binary()}].
build_push_headers(VapidToken, VapidPublicKey, Urgency) ->
    [
        {<<"TTL">>, ?PUSH_TTL},
        {<<"Urgency">>, Urgency},
        {<<"Content-Type">>, <<"application/octet-stream">>},
        {<<"Content-Encoding">>, <<"aes128gcm">>},
        {<<"Authorization">>, <<"vapid t=", VapidToken/binary, ", k=", VapidPublicKey/binary>>}
    ].

-spec request_push_endpoint(binary(), [{binary(), binary()}], binary()) ->
    {ok, non_neg_integer(), [{binary(), binary()}], binary()} | {error, term()}.
request_push_endpoint(Endpoint, Headers, Body) ->
    gateway_http_client:request(
        push,
        post,
        Endpoint,
        Headers,
        Body,
        #{content_type => <<"application/octet-stream">>}
    ).

-spec delete_payload(integer(), binary()) -> map().
delete_payload(UserId, SubscriptionId) ->
    #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"subscription_id">> => SubscriptionId
    }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_transient_error_5xx_test() ->
    ?assertEqual(true, is_transient_error({ok, 500, [], <<>>})),
    ?assertEqual(true, is_transient_error({ok, 502, [], <<>>})),
    ?assertEqual(true, is_transient_error({ok, 503, [], <<>>})).

is_transient_error_429_test() ->
    ?assertEqual(true, is_transient_error({ok, 429, [], <<>>})).

is_transient_error_network_test() ->
    ?assertEqual(true, is_transient_error({error, timeout})),
    ?assertEqual(true, is_transient_error({error, econnrefused})).

is_transient_error_local_backpressure_test() ->
    ?assertEqual(false, is_transient_error({error, overloaded})),
    ?assertEqual(false, is_transient_error({error, circuit_open})).

is_transient_error_permanent_test() ->
    ?assertEqual(false, is_transient_error({ok, 200, [], <<>>})),
    ?assertEqual(false, is_transient_error({ok, 201, [], <<>>})),
    ?assertEqual(false, is_transient_error({ok, 400, [], <<>>})),
    ?assertEqual(false, is_transient_error({ok, 404, [], <<>>})),
    ?assertEqual(false, is_transient_error({ok, 410, [], <<>>})).

retry_delay_exponential_backoff_test() ->
    D0 = retry_delay(0),
    D1 = retry_delay(1),
    D2 = retry_delay(2),
    ?assert(D0 >= ?BASE_RETRY_DELAY_MS),
    ?assert(D0 < ?BASE_RETRY_DELAY_MS * 2),
    ?assert(D1 >= ?BASE_RETRY_DELAY_MS * 2),
    ?assert(D1 < ?BASE_RETRY_DELAY_MS * 3),
    ?assert(D2 =< ?MAX_RETRY_DELAY_MS).

retry_delay_capped_test() ->
    D10 = retry_delay(10),
    ?assert(D10 =< ?MAX_RETRY_DELAY_MS),
    ?assert(D10 >= ?MAX_RETRY_DELAY_MS - (?MAX_RETRY_DELAY_MS div 4)).

maybe_retry_transient_waits_before_retry_test() ->
    Self = self(),
    Endpoint = <<"https://push.example/sub-1">>,
    RecordSize = push_sender_retry:initial_record_size(),
    Ctx = #{
        user_id => 42,
        subscription_id => <<"sub-1">>,
        endpoint => Endpoint,
        headers => [],
        payload => #{},
        p256dh_key => <<"p256dh">>,
        auth_key => <<"auth">>
    },
    ok = meck:new(gateway_retry_timer, [passthrough, no_link]),
    ok = meck:new(push_utils, [passthrough, no_link]),
    ok = meck:new(gateway_http_client, [passthrough, no_link]),
    try
        ok = meck:expect(gateway_retry_timer, wait, fun(DelayMs) ->
            Self ! {retry_wait, DelayMs},
            ok
        end),
        ok = meck:expect(push_utils, encrypt_payload, fun(
            <<"{}">>, <<"p256dh">>, <<"auth">>, ActualRecordSize
        ) when ActualRecordSize =:= RecordSize ->
            {ok, <<"encrypted">>}
        end),
        ok = meck:expect(
            gateway_http_client,
            request,
            fun retry_request_meck/6
        ),
        ?assertEqual(false, maybe_retry_transient(Ctx, RecordSize, 0, {error, timeout})),
        %% retry_delay/1 adds rand:uniform(Base div 4) of jitter, so the first
        %% retry is anywhere in [Base, Base + Base div 4 - 1].
        {retry_wait, DelayMs} = receive_retry_wait(),
        ?assert(DelayMs >= ?BASE_RETRY_DELAY_MS),
        ?assert(DelayMs =< ?BASE_RETRY_DELAY_MS + (?BASE_RETRY_DELAY_MS div 4) - 1),
        ?assertEqual(ok, receive_retried_push_request(Endpoint)),
        ?assert(meck:validate(gateway_retry_timer)),
        ?assert(meck:validate(push_utils)),
        ?assert(meck:validate(gateway_http_client))
    after
        meck:unload(gateway_http_client),
        meck:unload(push_utils),
        meck:unload(gateway_retry_timer)
    end.

-spec retry_request_meck(atom(), atom(), binary(), list(), binary(), term()) ->
    {ok, non_neg_integer(), list(), binary()}.
retry_request_meck(push, post, Endpoint, [], <<"encrypted">>, _Opts) ->
    self() ! {retried_push_request, Endpoint},
    {ok, 201, [], <<>>}.

extract_subscription_fields_ok_test() ->
    Sub = #{
        <<"endpoint">> => <<"https://push.example.com/sub1">>,
        <<"p256dh_key">> => <<"key1">>,
        <<"auth_key">> => <<"auth1">>,
        <<"subscription_id">> => <<"sub1">>
    },
    ?assertMatch({ok, _, _, _, _}, extract_subscription_fields(Sub)).

extract_subscription_fields_missing_test() ->
    ?assertMatch({error, _}, extract_subscription_fields(#{})).

cached_vapid_token_reuses_unexpired_token_test() ->
    Aud = <<"https://push.example">>,
    VapidEmail = <<"ops@example.com">>,
    VapidPublicKey = <<"public">>,
    VapidPrivateKey = <<"private">>,
    CacheKey = vapid_cache_key(Aud, VapidEmail, VapidPublicKey),
    erase_token_cache(CacheKey),
    Self = self(),
    ok = meck:new(push_utils, [passthrough, no_link]),
    try
        ok = meck:expect(push_utils, generate_vapid_token, fun(
            _Claims, _PublicKey, _PrivateKey
        ) ->
            Self ! vapid_generated,
            <<"cached-token">>
        end),
        ?assertEqual(
            {ok, <<"cached-token">>},
            cached_vapid_token(Aud, VapidEmail, VapidPublicKey, VapidPrivateKey, 1000)
        ),
        ?assertEqual(
            {ok, <<"cached-token">>},
            cached_vapid_token(Aud, VapidEmail, VapidPublicKey, VapidPrivateKey, 1001)
        ),
        ?assertEqual(1, drain_vapid_generated(0)),
        ?assert(meck:validate(push_utils))
    after
        meck:unload(push_utils),
        erase_token_cache(CacheKey)
    end.

drain_vapid_generated(Count) ->
    receive
        vapid_generated -> drain_vapid_generated(Count + 1)
    after 0 ->
        Count
    end.

receive_retry_wait() ->
    receive
        {retry_wait, DelayMs} -> {retry_wait, DelayMs}
    after 100 ->
        timeout
    end.

receive_retried_push_request(ExpectedEndpoint) ->
    receive
        {retried_push_request, ExpectedEndpoint} -> ok
    after 100 ->
        timeout
    end.

erase_token_cache(Key) ->
    ok = push_ets_cache:init(),
    true = ets:delete(push_bearer_tokens, Key),
    ok.

relay_endpoint_gets_the_shared_record_size_test() ->
    {Body, _Headers} = capture_web_push(alert_payload(<<"Hello">>)),
    ?assertEqual(2816, byte_size(Body)).

alert_notification_sends_high_urgency_test() ->
    {_Body, Headers} = capture_web_push(alert_payload(<<"Hello">>)),
    ?assertEqual(?ALERT_URGENCY, header_value(<<"Urgency">>, Headers)).

clear_notification_sends_low_urgency_test() ->
    Payload = push_notification:build_clear_notification_payload(999, 456, 789, 2),
    {_Body, Headers} = capture_web_push(Payload),
    ?assertEqual(?CLEAR_URGENCY, header_value(<<"Urgency">>, Headers)).

payload_over_the_budget_is_shrunk_instead_of_dropped_test() ->
    Budget = push_utils:plaintext_budget(push_sender_retry:initial_record_size()),
    Payload = alert_payload(binary:copy(<<"x">>, Budget * 2)),
    ?assert(byte_size(iolist_to_binary(json:encode(Payload))) > Budget),
    {Body, _Headers} = capture_web_push(Payload),
    ?assertEqual(2816, byte_size(Body)).

payload_at_the_budget_boundary_is_sent_unshrunk_test() ->
    Budget = push_utils:plaintext_budget(push_sender_retry:initial_record_size()),
    Payload = boundary_payload(Budget),
    ?assertEqual(2713, Budget),
    ?assertEqual(Budget, byte_size(iolist_to_binary(json:encode(Payload)))),
    {Body, _Headers} = capture_web_push(Payload),
    ?assertEqual(2816, byte_size(Body)).

boundary_payload(Budget) ->
    Base = #{<<"web_push">> => 8030, <<"title">> => <<>>},
    Overhead = byte_size(iolist_to_binary(json:encode(Base))),
    Base#{<<"title">> => binary:copy(<<"x">>, Budget - Overhead)}.

alert_payload(Body) ->
    #{
        <<"web_push">> => 8030,
        <<"title">> => <<"Alice">>,
        <<"body">> => Body,
        <<"tag">> => <<"channel:456:789">>,
        <<"data">> => #{
            <<"channel_id">> => <<"456">>,
            <<"message_id">> => <<"789">>,
            <<"url">> => <<"/channels/123/456/789">>
        }
    }.

header_value(Name, Headers) ->
    proplists:get_value(Name, Headers).

capture_web_push(Payload) ->
    Endpoint = <<
        "https://push.fluxer.app/relay/v1/apns/stable/production/",
        (binary:copy(<<"a">>, 64))/binary
    >>,
    {PeerPub, _PeerPriv} = crypto:generate_key(ecdh, prime256v1),
    Subscription = #{
        <<"endpoint">> => Endpoint,
        <<"p256dh_key">> => push_utils:base64url_encode(PeerPub),
        <<"auth_key">> => push_utils:base64url_encode(crypto:strong_rand_bytes(16)),
        <<"subscription_id">> => <<"sub-1">>
    },
    ok = push_ets_cache:init(),
    ok = meck:new(fluxer_gateway_env, [passthrough, no_link]),
    ok = meck:new(push_utils, [passthrough, no_link]),
    ok = meck:new(gateway_http_client, [passthrough, no_link]),
    ok = meck:new(push_endpoint_guard, [passthrough, no_link]),
    try
        ok = meck:expect(push_endpoint_guard, check, fun(_Endpoint) -> ok end),
        ok = meck:expect(fluxer_gateway_env, get, fun vapid_env_meck/1),
        ok = meck:expect(push_utils, generate_vapid_token, fun(_Claims, _Public, _Private) ->
            <<"vapid-token">>
        end),
        ok = meck:expect(gateway_http_client, request, fun capture_request_meck/6),
        ?assertEqual(false, send_webpush_notification(42, Subscription, Payload)),
        receive
            {captured_push, CapturedHeaders, CapturedBody} -> {CapturedBody, CapturedHeaders}
        after 1000 ->
            erlang:error(no_push_request)
        end
    after
        meck:unload(push_endpoint_guard),
        meck:unload(gateway_http_client),
        meck:unload(push_utils),
        meck:unload(fluxer_gateway_env)
    end.

vapid_env_meck(vapid_email) -> <<"ops@example.com">>;
vapid_env_meck(vapid_public_key) -> <<"public-key">>;
vapid_env_meck(vapid_private_key) -> <<"private-key">>;
vapid_env_meck(Key) -> meck:passthrough([Key]).

capture_request_meck(push, post, _Endpoint, Headers, Body, _Opts) ->
    self() ! {captured_push, Headers, Body},
    {ok, 201, [], <<>>}.

-endif.
