%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_job_publisher).
-typing([eqwalizer]).

-export([publish_message/8, publish_clear/3, clear_job/4]).
-export([publish_ring/6, request/3]).

-define(SUBJECT_MESSAGE, <<"push.job.message">>).
-define(SUBJECT_CLEAR, <<"push.job.clear">>).
-define(SUBJECT_RING, <<"push.job.ring">>).
-define(JOB_VERSION, 1).
-define(LEGACY_CONFIG_VERSION, 0).
-define(NATS_MAX_PAYLOAD_BYTES, 1048576).

-type meta() :: #{
    kind := push_outbox:kind(),
    user_ids := [integer()],
    channel_id := integer(),
    message_id := integer()
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
    Job = message_job(
        MessageData,
        MarkdownContext,
        GuildId,
        ChannelId,
        MessageId,
        GuildName,
        ChannelName
    ),
    publish_recipients(Job, UserIds, #{
        kind => message,
        user_ids => UserIds,
        channel_id => ChannelId,
        message_id => MessageId
    }).

-spec message_job(
    map(),
    map(),
    integer(),
    integer(),
    integer(),
    binary() | undefined,
    binary() | undefined
) -> map().
message_job(
    MessageData,
    MarkdownContext,
    GuildId,
    ChannelId,
    MessageId,
    GuildName,
    ChannelName
) ->
    Job = #{
        <<"v">> => ?JOB_VERSION,
        <<"config_version">> => ?LEGACY_CONFIG_VERSION,
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"message_id">> => integer_to_binary(MessageId),
        <<"notification">> => push_job_fields:notification(
            MessageData,
            MarkdownContext,
            GuildId,
            ChannelId,
            MessageId,
            GuildName,
            ChannelName
        )
    },
    maps:merge(Job, push_job_fields:thread(MessageData, ChannelName)).

-spec publish_recipients(map(), [integer()], meta()) -> ok | {error, term()}.
publish_recipients(Job, UserIds, Meta) ->
    Chunk = Job#{<<"user_ids">> => [integer_to_binary(UserId) || UserId <- UserIds]},
    case {encode(Chunk), UserIds} of
        {{ok, Body}, [_, _ | _]} when byte_size(Body) > ?NATS_MAX_PAYLOAD_BYTES ->
            {Left, Right} = lists:split(length(UserIds) div 2, UserIds),
            LeftResult = publish_recipients(Job, Left, Meta),
            RightResult = publish_recipients(Job, Right, Meta),
            first_error(LeftResult, RightResult);
        {Encoded, _UserIds} ->
            publish_encoded(?SUBJECT_MESSAGE, Chunk, Encoded, Meta#{user_ids := UserIds})
    end.

-spec first_error(ok | {error, term()}, ok | {error, term()}) -> ok | {error, term()}.
first_error(ok, Second) ->
    Second;
first_error({error, Reason}, _Second) ->
    {error, Reason}.

-spec publish_clear(integer(), integer(), integer()) -> ok | {error, term()}.
publish_clear(UserId, ChannelId, MessageId) ->
    publish(
        ?SUBJECT_CLEAR,
        clear_fields(UserId, ChannelId, MessageId),
        clear_meta(UserId, ChannelId, MessageId)
    ).

-spec clear_job(integer(), integer(), integer(), integer()) -> push_outbox:job().
clear_job(UserId, ChannelId, MessageId, AfterMessageId) ->
    Job = (clear_fields(UserId, ChannelId, MessageId))#{
        <<"after_message_id">> => integer_to_binary(AfterMessageId)
    },
    outbox_job(
        ?SUBJECT_CLEAR,
        Job,
        iolist_to_binary(json:encode(Job)),
        clear_meta(UserId, ChannelId, MessageId)
    ).

-spec clear_fields(integer(), integer(), integer()) -> map().
clear_fields(UserId, ChannelId, MessageId) ->
    #{
        <<"v">> => ?JOB_VERSION,
        <<"config_version">> => ?LEGACY_CONFIG_VERSION,
        <<"user_id">> => integer_to_binary(UserId),
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"message_id">> => integer_to_binary(MessageId)
    }.

-spec clear_meta(integer(), integer(), integer()) -> meta().
clear_meta(UserId, ChannelId, MessageId) ->
    #{kind => clear, user_ids => [UserId], channel_id => ChannelId, message_id => MessageId}.

-spec publish_ring(integer(), integer(), integer(), integer(), integer(), map()) ->
    ok | {error, term()}.
publish_ring(UserId, ChannelId, MessageId, StartedAtMs, ExpiresAtMs, Caller) ->
    Job = maps:merge(
        #{
            <<"v">> => ?JOB_VERSION,
            <<"config_version">> => ?LEGACY_CONFIG_VERSION,
            <<"user_id">> => integer_to_binary(UserId),
            <<"channel_id">> => integer_to_binary(ChannelId),
            <<"message_id">> => integer_to_binary(MessageId),
            <<"started_at_ms">> => StartedAtMs,
            <<"expires_at_ms">> => ExpiresAtMs
        },
        push_job_fields:caller(Caller)
    ),
    publish(?SUBJECT_RING, Job, #{
        kind => ring,
        user_ids => [UserId],
        channel_id => ChannelId,
        message_id => MessageId
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

-spec publish(binary(), map(), meta()) -> ok | {error, term()}.
publish(Subject, Job, Meta) ->
    publish_encoded(Subject, Job, encode(Job), Meta).

-spec encode(map()) -> {ok, binary()} | {error, term()}.
encode(Job) ->
    try
        {ok, iolist_to_binary(json:encode(Job))}
    catch
        Class:Reason -> {error, {encode_failed, Class, Reason}}
    end.

-spec publish_encoded(binary(), map(), {ok, binary()} | {error, term()}, meta()) ->
    ok | {error, term()}.
publish_encoded(Subject, _Job, {error, Reason}, #{kind := Kind}) ->
    logger:warning("Push job encode failed", #{subject => Subject, reason => Reason}),
    push_outbox:record_dropped(Kind, encode_failed),
    {error, Reason};
publish_encoded(Subject, _Job, {ok, Body}, #{kind := Kind}) when
    byte_size(Body) > ?NATS_MAX_PAYLOAD_BYTES
->
    logger:warning("Push job exceeds the NATS payload limit", #{
        subject => Subject, bytes => byte_size(Body), limit => ?NATS_MAX_PAYLOAD_BYTES
    }),
    push_outbox:record_dropped(Kind, payload_too_large),
    {error, {payload_too_large, byte_size(Body)}};
publish_encoded(Subject, Job, {ok, Body}, #{kind := Kind} = Meta) ->
    case push_outbox:enqueue(outbox_job(Subject, Job, Body, Meta)) of
        ok ->
            ok;
        {error, Reason} ->
            logger:warning("Push job publish failed", #{subject => Subject, reason => Reason}),
            record_enqueue_failure(Kind, Reason),
            {error, Reason}
    end.

-spec record_enqueue_failure(push_outbox:kind(), term()) -> ok.
record_enqueue_failure(_Kind, outbox_unavailable) ->
    ok;
record_enqueue_failure(Kind, {outbox_unavailable, timeout}) ->
    push_outbox:record_dropped(Kind, enqueue_timeout);
record_enqueue_failure(Kind, _Reason) ->
    push_outbox:record_dropped(Kind, outbox_unavailable).

-spec outbox_job(binary(), map(), binary(), meta()) -> push_outbox:job().
outbox_job(Subject, Job, Body, Meta) ->
    #{kind := Kind, user_ids := UserIds, channel_id := ChannelId, message_id := MessageId} =
        Meta,
    #{
        kind => Kind,
        subject => Subject,
        job => Job,
        body => Body,
        user_ids => UserIds,
        channel_id => ChannelId,
        message_id => MessageId
    }.
