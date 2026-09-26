%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_job_publisher).
-typing([eqwalizer]).

-export([publish_message/8, publish_message/10, publish_clear/3, publish_clear/5]).
-export([publish_ring/6, request/3]).

-define(SUBJECT_MESSAGE, <<"push.job.message">>).
-define(SUBJECT_CLEAR, <<"push.job.clear">>).
-define(SUBJECT_RING, <<"push.job.ring">>).
-define(JOB_VERSION, 1).
-define(NATS_MAX_PAYLOAD_BYTES, 1048576).
-define(MAX_CALLER_NAME_BYTES, 128).

-type meta() :: #{
    kind := message | clear | ring,
    user_ids := [integer()],
    channel_id := integer(),
    message_id := integer(),
    fallback := push_outbox:fallback()
}.

-spec publish_message(
    [integer()],
    map(),
    map(),
    integer(),
    integer(),
    integer(),
    binary() | undefined,
    binary() | undefined
) -> ok | {error, term()}.
publish_message(
    UserIds,
    MessageData,
    MarkdownContext,
    GuildId,
    ChannelId,
    MessageId,
    GuildName,
    ChannelName
) ->
    publish_message(
        UserIds,
        MessageData,
        MarkdownContext,
        GuildId,
        ChannelId,
        MessageId,
        GuildName,
        ChannelName,
        push_delivery_config:config_version(),
        fun ignore_fallback/1
    ).

-spec publish_message(
    [integer()],
    map(),
    map(),
    integer(),
    integer(),
    integer(),
    binary() | undefined,
    binary() | undefined,
    non_neg_integer(),
    push_outbox:fallback()
) -> ok | {error, term()}.
publish_message(
    UserIds,
    MessageData,
    MarkdownContext,
    GuildId,
    ChannelId,
    MessageId,
    GuildName,
    ChannelName,
    ConfigVersion,
    Fallback
) ->
    ChannelIdBin = integer_to_binary(ChannelId),
    MessageIdBin = integer_to_binary(MessageId),
    Job = #{
        <<"v">> => ?JOB_VERSION,
        <<"config_version">> => ConfigVersion,
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"channel_id">> => ChannelIdBin,
        <<"message_id">> => MessageIdBin,
        <<"notification">> => notification_fields(
            MessageData,
            MarkdownContext,
            GuildId,
            ChannelId,
            MessageId,
            GuildName,
            ChannelName
        ),
        <<"user_ids">> => [integer_to_binary(UserId) || UserId <- UserIds]
    },
    publish(?SUBJECT_MESSAGE, Job, #{
        kind => message,
        user_ids => UserIds,
        channel_id => ChannelId,
        message_id => MessageId,
        fallback => Fallback
    }).

-spec publish_clear(integer(), integer(), integer()) -> ok | {error, term()}.
publish_clear(UserId, ChannelId, MessageId) ->
    publish_clear(
        UserId,
        ChannelId,
        MessageId,
        push_delivery_config:config_version(),
        fun ignore_fallback/1
    ).

-spec publish_clear(
    integer(), integer(), integer(), non_neg_integer(), push_outbox:fallback()
) ->
    ok | {error, term()}.
publish_clear(UserId, ChannelId, MessageId, ConfigVersion, Fallback) ->
    Job = #{
        <<"v">> => ?JOB_VERSION,
        <<"config_version">> => ConfigVersion,
        <<"user_id">> => integer_to_binary(UserId),
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"message_id">> => integer_to_binary(MessageId)
    },
    publish(?SUBJECT_CLEAR, Job, #{
        kind => clear,
        user_ids => [UserId],
        channel_id => ChannelId,
        message_id => MessageId,
        fallback => Fallback
    }).

-spec publish_ring(integer(), integer(), integer(), integer(), integer(), map()) ->
    ok | {error, term()}.
publish_ring(UserId, ChannelId, MessageId, StartedAtMs, ExpiresAtMs, Caller) ->
    Job = maps:merge(
        #{
            <<"v">> => ?JOB_VERSION,
            <<"config_version">> => push_delivery_config:config_version(),
            <<"user_id">> => integer_to_binary(UserId),
            <<"channel_id">> => integer_to_binary(ChannelId),
            <<"message_id">> => integer_to_binary(MessageId),
            <<"started_at_ms">> => StartedAtMs,
            <<"expires_at_ms">> => ExpiresAtMs
        },
        caller_fields(Caller)
    ),
    publish(?SUBJECT_RING, Job, #{
        kind => ring,
        user_ids => [UserId],
        channel_id => ChannelId,
        message_id => MessageId,
        fallback => fun ignore_fallback/1
    }).

-spec caller_fields(map()) -> map().
caller_fields(#{caller_id := CallerId, caller_name := Name} = Caller) when
    is_integer(CallerId), is_binary(Name), byte_size(Name) > 0
->
    CallerIdBin = integer_to_binary(CallerId),
    #{
        <<"caller_id">> => CallerIdBin,
        <<"caller_name">> => push_notification_format:truncate_bytes(
            Name, ?MAX_CALLER_NAME_BYTES
        ),
        <<"caller_avatar_url">> => caller_avatar_url(
            CallerIdBin, maps:get(caller_avatar, Caller, undefined)
        )
    };
caller_fields(_Caller) ->
    #{}.

-spec caller_avatar_url(binary(), term()) -> binary().
caller_avatar_url(CallerIdBin, Hash) when is_binary(Hash), byte_size(Hash) > 0 ->
    push_notification_format:resolve_author_avatar_url(#{
        <<"id">> => CallerIdBin, <<"avatar">> => Hash
    });
caller_avatar_url(CallerIdBin, _Hash) ->
    push_notification_format:resolve_author_avatar_url(#{
        <<"id">> => CallerIdBin, <<"avatar">> => null
    }).

-spec request(binary(), binary(), pos_integer()) -> ok | {error, term()}.
request(Subject, Body, Timeout) ->
    case gateway_nats_pool_conn:get_pool_conn() of
        {ok, Conn} -> reply_result(nats:request(Conn, Subject, Body, #{timeout => Timeout}));
        {error, Reason} -> {error, Reason}
    end.

-spec reply_result({ok, {iodata(), map()}} | {error, term()}) -> ok | {error, term()}.
reply_result({ok, {Payload, _MsgOpts}}) ->
    decode_reply(Payload);
reply_result({error, Reason}) ->
    {error, Reason}.

-spec decode_reply(iodata()) -> ok | {error, term()}.
decode_reply(Payload) ->
    try json:decode(iolist_to_binary(Payload)) of
        #{<<"ok">> := true} -> ok;
        #{<<"ok">> := false} = Reply -> {error, {rejected, maps:get(<<"error">>, Reply, null)}};
        _ -> {error, invalid_reply}
    catch
        _:_ -> {error, invalid_reply}
    end.

-spec ignore_fallback([integer()]) -> ok.
ignore_fallback(_UserIds) ->
    ok.

-spec notification_fields(
    map(), map(), integer(), integer(), integer(), binary() | undefined, binary() | undefined
) -> map().
notification_fields(
    MessageData, MarkdownContext, GuildId, ChannelId, MessageId, GuildName, ChannelName
) ->
    AuthorData = maps:get(<<"author">>, MessageData, #{}),
    AuthorUsername = maps:get(<<"username">>, AuthorData, <<"Unknown">>),
    AuthorName = push_notification_format:resolve_author_name(
        MessageData, MarkdownContext, AuthorUsername
    ),
    ChannelIdBin = integer_to_binary(ChannelId),
    MessageIdBin = integer_to_binary(MessageId),
    #{
        <<"title">> => push_notification:build_notification_title(
            AuthorName, MessageData, GuildId, GuildName, ChannelName
        ),
        <<"body">> => push_notification_format:build_content_preview(
            MessageData, MarkdownContext
        ),
        <<"icon">> => push_notification_format:resolve_author_avatar_url(AuthorData),
        <<"badge">> => push_utils:construct_static_asset_url(
            <<"marketing/branding/symbol-white.svg">>
        ),
        <<"tag">> => <<"channel:", ChannelIdBin/binary, ":", MessageIdBin/binary>>,
        <<"notification_tag">> => <<"channel:", ChannelIdBin/binary>>,
        <<"url">> => push_notification_format:build_url(GuildId, ChannelId, MessageId),
        <<"image_url">> => nullable(push_notification_format:extract_image_url(MessageData))
    }.

-spec nullable(binary() | undefined) -> binary() | null.
nullable(undefined) ->
    null;
nullable(Value) ->
    Value.

-spec publish(binary(), map(), meta()) -> ok | {error, term()}.
publish(Subject, Job, Meta) ->
    case encode(Job) of
        {ok, Body} ->
            publish_bounded(Subject, Job, Body, Meta);
        {error, Reason} ->
            logger:warning("Push job encode failed", #{subject => Subject, reason => Reason}),
            {error, Reason}
    end.

-spec encode(map()) -> {ok, binary()} | {error, term()}.
encode(Job) ->
    try
        {ok, iolist_to_binary(json:encode(Job))}
    catch
        Class:Reason -> {error, {encode_failed, Class, Reason}}
    end.

-spec publish_bounded(binary(), map(), binary(), meta()) -> ok | {error, term()}.
publish_bounded(Subject, _Job, Body, _Meta) when byte_size(Body) > ?NATS_MAX_PAYLOAD_BYTES ->
    logger:warning("Push job exceeds the NATS payload limit", #{
        subject => Subject, bytes => byte_size(Body), limit => ?NATS_MAX_PAYLOAD_BYTES
    }),
    {error, {payload_too_large, byte_size(Body)}};
publish_bounded(Subject, Job, Body, Meta) ->
    case push_outbox:enqueue(outbox_job(Subject, Job, Body, Meta)) of
        ok ->
            ok;
        {error, Reason} ->
            logger:warning("Push job publish failed", #{subject => Subject, reason => Reason}),
            {error, Reason}
    end.

-spec outbox_job(binary(), map(), binary(), meta()) -> push_outbox:job().
outbox_job(Subject, Job, Body, Meta) ->
    #{
        kind := Kind,
        user_ids := UserIds,
        channel_id := ChannelId,
        message_id := MessageId,
        fallback := Fallback
    } = Meta,
    #{
        kind => Kind,
        subject => Subject,
        job => Job,
        body => Body,
        user_ids => UserIds,
        channel_id => ChannelId,
        message_id => MessageId,
        fallback => Fallback
    }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

unresolved_caller() ->
    #{caller_id => undefined, caller_name => undefined, caller_avatar => undefined}.

with_endpoint_env(Fun) ->
    ok = meck:new(fluxer_gateway_env, [passthrough, no_link]),
    try
        ok = meck:expect(fluxer_gateway_env, get, fun endpoint_env_meck/1),
        Fun()
    after
        meck:unload(fluxer_gateway_env)
    end.

endpoint_env_meck(media_proxy_endpoint) -> <<"https://media.example">>;
endpoint_env_meck(static_cdn_endpoint) -> <<"https://static.example">>;
endpoint_env_meck(Key) -> meck:passthrough([Key]).

caller_fields_omits_every_key_when_the_caller_is_unresolved_test() ->
    ?assertEqual(#{}, caller_fields(unresolved_caller())).

caller_fields_omits_every_key_when_only_the_name_resolved_test() ->
    ?assertEqual(
        #{},
        caller_fields(#{
            caller_id => undefined, caller_name => <<"Ada">>, caller_avatar => undefined
        })
    ).

caller_fields_builds_the_avatar_url_from_the_hash_test() ->
    Fields = with_endpoint_env(fun() ->
        caller_fields(#{
            caller_id => 1234567890123456789,
            caller_name => <<"Ada">>,
            caller_avatar => <<"a1b2c3d4">>
        })
    end),
    ?assertEqual(
        #{
            <<"caller_id">> => <<"1234567890123456789">>,
            <<"caller_name">> => <<"Ada">>,
            <<"caller_avatar_url">> =>
                <<"https://media.example/avatars/1234567890123456789/a1b2c3d4.png">>
        },
        Fields
    ).

caller_fields_falls_back_to_the_default_avatar_test() ->
    Fields = with_endpoint_env(fun() ->
        caller_fields(#{
            caller_id => 1234567890123456789,
            caller_name => <<"Ada">>,
            caller_avatar => undefined
        })
    end),
    ?assertMatch(
        #{<<"caller_avatar_url">> := <<"https://static.example/avatars/", _/binary>>},
        Fields
    ).

caller_fields_caps_the_caller_name_test() ->
    Name = binary:copy(<<"a">>, ?MAX_CALLER_NAME_BYTES + 32),
    Fields = with_endpoint_env(fun() ->
        caller_fields(#{
            caller_id => 1234567890123456789, caller_name => Name, caller_avatar => undefined
        })
    end),
    ?assertEqual(
        ?MAX_CALLER_NAME_BYTES, byte_size(maps:get(<<"caller_name">>, Fields))
    ).

-endif.
