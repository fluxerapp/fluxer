%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_fcm).
-typing([eqwalizer]).

-export([send/3]).

-define(FCM_SCOPE, <<"https://www.googleapis.com/auth/firebase.messaging">>).
-define(DEFAULT_TOKEN_URI, <<"https://oauth2.googleapis.com/token">>).
-define(ACCESS_TOKEN_SKEW_SECONDS, 60).

-spec send(integer(), map(), map()) -> false | {true, map()}.
send(UserId, Subscription, Payload) ->
    case fluxer_gateway_env:get(fcm_enabled) of
        true ->
            send_enabled(UserId, Subscription, Payload);
        _ ->
            false
    end.

-spec send_enabled(integer(), map(), map()) -> false | {true, map()}.
send_enabled(UserId, Subscription, Payload) ->
    case extract_subscription(Subscription) of
        {ok, DeviceToken, SubscriptionId, AppId} ->
            send_with_config(UserId, DeviceToken, SubscriptionId, AppId, Payload);
        {error, Reason} ->
            log_config_error(UserId, <<"invalid_fcm_subscription">>, Reason),
            false
    end.

-spec send_with_config(integer(), binary(), binary(), binary(), map()) -> false | {true, map()}.
send_with_config(UserId, DeviceToken, SubscriptionId, AppId, Payload) ->
    case {resolve_project_id(AppId), resolve_access_token()} of
        {{ok, ProjectId}, {ok, AccessToken}} ->
            Message = push_fcm_payload:build_message(DeviceToken, Payload),
            Body = iolist_to_binary(json:encode(Message)),
            Url =
                <<"https://fcm.googleapis.com/v1/projects/", ProjectId/binary,
                    "/messages:send">>,
            Headers = [
                {<<"Authorization">>, <<"Bearer ", AccessToken/binary>>},
                {<<"Content-Type">>, <<"application/json; charset=UTF-8">>}
            ],
            Response = gateway_http_client:request(
                push,
                post,
                Url,
                Headers,
                Body,
                #{content_type => <<"application/json; charset=UTF-8">>}
            ),
            push_fcm_payload:handle_response(UserId, SubscriptionId, Response);
        {{error, Reason}, _} ->
            log_config_error(UserId, <<"fcm_project_error">>, Reason),
            false;
        {_, {error, Reason}} ->
            log_config_error(UserId, <<"fcm_auth_error">>, Reason),
            false
    end.

-spec extract_subscription(map()) -> {ok, binary(), binary(), binary()} | {error, term()}.
extract_subscription(Subscription) ->
    DeviceToken = push_utils:normalize_binary(
        maps:get(<<"endpoint">>, Subscription, undefined), undefined
    ),
    SubscriptionId = push_utils:normalize_binary(
        maps:get(<<"subscription_id">>, Subscription, undefined), undefined
    ),
    AppId = push_utils:normalize_binary(
        maps:get(<<"app_id">>, Subscription, <<"stable">>), <<"stable">>
    ),
    case {DeviceToken, SubscriptionId, AppId} of
        {Token, Id, App} when
            is_binary(Token),
            byte_size(Token) > 0,
            is_binary(Id),
            is_binary(App)
        ->
            {ok, Token, Id, App};
        _ ->
            {error, missing_fields}
    end.

-spec resolve_project_id(binary()) -> {ok, binary()} | {error, term()}.
resolve_project_id(AppId) ->
    Apps = fluxer_gateway_env:get(fcm_apps),
    case find_app(AppId, map_utils:ensure_list(Apps)) of
        App when is_map(App) -> resolve_app_project_id(App);
        undefined -> resolve_default_project_id()
    end.

-spec resolve_app_project_id(map()) -> {ok, binary()} | {error, term()}.
resolve_app_project_id(App) ->
    case push_utils:normalize_binary(get_map_value(App, <<"project_id">>), undefined) of
        ProjectId when is_binary(ProjectId), byte_size(ProjectId) > 0 -> {ok, ProjectId};
        _ -> resolve_default_project_id()
    end.

-spec resolve_default_project_id() -> {ok, binary()} | {error, term()}.
resolve_default_project_id() ->
    case fluxer_gateway_env:get(fcm_project_id) of
        ProjectId when is_binary(ProjectId), byte_size(ProjectId) > 0 -> {ok, ProjectId};
        _ -> {error, missing_project_id}
    end.

-spec resolve_access_token() -> {ok, binary()} | {error, term()}.
resolve_access_token() ->
    maybe
        {ok, ServiceAccount} ?= resolve_service_account(),
        ClientEmail = maps:get(client_email, ServiceAccount),
        TokenUri = maps:get(token_uri, ServiceAccount),
        CacheKey = {?MODULE, access_token, ClientEmail},
        Now = erlang:system_time(second),
        get_or_fetch_token(ServiceAccount, CacheKey, Now, TokenUri)
    else
        {error, Reason} -> {error, Reason}
    end.

-spec get_or_fetch_token(map(), term(), integer(), binary()) ->
    {ok, binary()} | {error, term()}.
get_or_fetch_token(ServiceAccount, CacheKey, Now, TokenUri) ->
    case push_ets_cache:get_bearer_token(CacheKey) of
        {ok, Token, ExpiresAt} when ExpiresAt - ?ACCESS_TOKEN_SKEW_SECONDS > Now ->
            {ok, Token};
        _ ->
            fetch_access_token(ServiceAccount, CacheKey, Now, TokenUri)
    end.

-spec fetch_access_token(map(), term(), integer(), binary()) ->
    {ok, binary()} | {error, term()}.
fetch_access_token(ServiceAccount, CacheKey, Now, TokenUri) ->
    Claims = #{
        <<"iss">> => maps:get(client_email, ServiceAccount),
        <<"scope">> => ?FCM_SCOPE,
        <<"aud">> => TokenUri,
        <<"iat">> => Now,
        <<"exp">> => Now + 3600
    },
    Header = #{<<"alg">> => <<"RS256">>, <<"typ">> => <<"JWT">>},
    maybe
        PrivateKey = maps:get(private_key, ServiceAccount),
        {ok, Assertion} ?= push_utils:generate_jwt_from_pem(PrivateKey, Header, Claims),
        exchange_assertion_for_token(CacheKey, Now, TokenUri, Assertion)
    else
        {error, _Reason} -> {error, jwt_signing_failed}
    end.

-spec exchange_assertion_for_token(term(), integer(), binary(), binary()) ->
    {ok, binary()} | {error, term()}.
exchange_assertion_for_token(CacheKey, Now, TokenUri, Assertion) ->
    Body =
        <<"grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer&assertion=",
            Assertion/binary>>,
    Headers = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
    case
        gateway_http_client:request(
            push,
            post,
            TokenUri,
            Headers,
            Body,
            #{content_type => <<"application/x-www-form-urlencoded">>}
        )
    of
        {ok, Status, _Headers, ResponseBody} when Status >= 200, Status < 300 ->
            parse_token_response(CacheKey, Now, ResponseBody);
        {ok, Status, _Headers, ResponseBody} ->
            {error, {token_http_error, Status, ResponseBody}};
        {error, Reason} ->
            {error, {token_request_failed, Reason}}
    end.

-spec parse_token_response(term(), integer(), binary()) -> {ok, binary()} | {error, term()}.
parse_token_response(CacheKey, Now, ResponseBody) ->
    case decode_json_map(ResponseBody) of
        #{<<"access_token">> := AccessToken} = Response when is_binary(AccessToken) ->
            ExpiresIn = normalize_expires_in(maps:get(<<"expires_in">>, Response, 3600)),
            push_ets_cache:put_bearer_token(CacheKey, AccessToken, Now + ExpiresIn),
            {ok, AccessToken};
        _ ->
            {error, invalid_token_response}
    end.

-spec resolve_service_account() -> {ok, map()} | {error, term()}.
resolve_service_account() ->
    JsonPath = fluxer_gateway_env:get(fcm_service_account_json_path),
    maybe
        {ok, Json} ?= load_json_config(JsonPath),
        build_service_account(Json)
    else
        {error, Reason} -> {error, Reason}
    end.

-spec load_json_config(binary() | term()) -> {ok, map()} | {error, term()}.
load_json_config(Path) when is_binary(Path), byte_size(Path) > 0 ->
    read_json_file(Path);
load_json_config(_) ->
    {ok, #{}}.

-spec build_service_account(map()) -> {ok, map()} | {error, term()}.
build_service_account(Json) ->
    ClientEmail = first_binary([
        get_map_value(Json, <<"client_email">>), fluxer_gateway_env:get(fcm_client_email)
    ]),
    TokenUri = first_binary([
        get_map_value(Json, <<"token_uri">>),
        fluxer_gateway_env:get(fcm_token_uri),
        ?DEFAULT_TOKEN_URI
    ]),
    PrivateKey = resolve_private_key(Json),
    validate_service_account(ClientEmail, TokenUri, PrivateKey).

-spec validate_service_account(
    binary() | undefined, binary() | undefined, {ok, binary()} | {error, term()}
) ->
    {ok, map()} | {error, term()}.
validate_service_account(Email, Uri, {ok, Key}) when
    is_binary(Email), byte_size(Email) > 0, is_binary(Uri), byte_size(Uri) > 0
->
    {ok, #{client_email => Email, token_uri => Uri, private_key => Key}};
validate_service_account(undefined, _, _) ->
    {error, missing_client_email};
validate_service_account(_, undefined, _) ->
    {error, missing_token_uri};
validate_service_account(_, _, {error, Reason}) ->
    {error, Reason};
validate_service_account(_, _, _) ->
    {error, invalid_service_account}.

-spec resolve_private_key(map()) -> {ok, binary()} | {error, term()}.
resolve_private_key(Json) ->
    case
        first_binary([
            get_map_value(Json, <<"private_key">>), fluxer_gateway_env:get(fcm_private_key)
        ])
    of
        Key when is_binary(Key), byte_size(Key) > 0 ->
            {ok, normalize_pem(Key)};
        _ ->
            resolve_private_key_from_file()
    end.

-spec resolve_private_key_from_file() -> {ok, binary()} | {error, term()}.
resolve_private_key_from_file() ->
    case fluxer_gateway_env:get(fcm_private_key_path) of
        Path when is_binary(Path), byte_size(Path) > 0 -> read_pem_file(Path);
        _ -> {error, missing_private_key}
    end.

-spec find_app(binary(), list()) -> map() | undefined.
find_app(_AppId, []) ->
    undefined;
find_app(AppId, [App | Rest]) when is_map(App) ->
    case push_utils:normalize_binary(get_map_value(App, <<"app_id">>), <<>>) of
        AppId -> App;
        _ -> find_app(AppId, Rest)
    end;
find_app(AppId, [_ | Rest]) ->
    find_app(AppId, Rest).

-spec read_json_file(binary()) -> {ok, map()} | {error, term()}.
read_json_file(Path) ->
    case file:read_file(binary_to_list(Path)) of
        {ok, Content} -> decode_json_file(Content);
        {error, Reason} -> {error, {read_json_failed, Reason}}
    end.

-spec decode_json_file(binary()) -> {ok, map()} | {error, invalid_json_file}.
decode_json_file(Content) ->
    case decode_json_map(Content) of
        Map when is_map(Map) -> {ok, Map};
        _ -> {error, invalid_json_file}
    end.

-spec read_pem_file(binary()) -> {ok, binary()} | {error, term()}.
read_pem_file(Path) ->
    case file:read_file(binary_to_list(Path)) of
        {ok, Content} -> {ok, normalize_pem(Content)};
        {error, Reason} -> {error, {read_private_key_failed, Reason}}
    end.

-spec decode_json_map(binary()) -> map() | undefined.
decode_json_map(Body) when is_binary(Body), byte_size(Body) > 0 ->
    try json:decode(Body) of
        Map when is_map(Map) -> Map;
        _ -> undefined
    catch
        error:_ -> undefined;
        throw:_ -> undefined;
        exit:_ -> undefined
    end;
decode_json_map(_) ->
    undefined.

-spec normalize_expires_in(term()) -> pos_integer().
normalize_expires_in(Value) ->
    case guild_data_normalize_schema:int(Value) of
        ExpiresIn when is_integer(ExpiresIn), ExpiresIn > 0 -> ExpiresIn;
        _ -> 3600
    end.

-spec first_binary(list()) -> binary() | undefined.
first_binary([]) ->
    undefined;
first_binary([Value | Rest]) ->
    case push_utils:normalize_binary(Value, undefined) of
        Bin when is_binary(Bin), byte_size(Bin) > 0 -> Bin;
        _ -> first_binary(Rest)
    end.

-spec get_map_value(map(), binary()) -> term().
get_map_value(Map, Key) when is_map(Map), is_binary(Key) ->
    case maps:get(Key, Map, undefined) of
        undefined -> maps:get(binary_to_list(Key), Map, undefined);
        Value -> Value
    end.

-spec normalize_pem(binary()) -> binary().
normalize_pem(Pem) -> binary:replace(Pem, <<"\\n">>, <<"\n">>, [global]).

-spec log_config_error(integer(), binary(), term()) -> ok.
log_config_error(UserId, ReasonCode, Reason) ->
    logger:debug(
        "Push: FCM delivery unavailable",
        #{user_id => UserId, reason => ReasonCode, detail => Reason}
    ),
    ok.
