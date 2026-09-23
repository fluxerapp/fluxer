%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_job_publisher).
-typing([eqwalizer]).

-export([publish_message/8, publish_message/10, publish_clear/3, publish_clear/5, request/3]).

-define(SUBJECT_MESSAGE, <<"push.job.message">>).
-define(SUBJECT_CLEAR, <<"push.job.clear">>).
-define(JOB_VERSION, 1).
-define(NATS_MAX_PAYLOAD_BYTES, 1048576).

-type meta() :: #{
    kind := message | clear,
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
