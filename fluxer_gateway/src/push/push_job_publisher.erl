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
-define(MAX_CALLER_NAME_BYTES, 128).

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
    Job = #{
        <<"v">> => ?JOB_VERSION,
        <<"config_version">> => ?LEGACY_CONFIG_VERSION,
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"message_id">> => integer_to_binary(MessageId),
        <<"notification">> => notification_fields(
            MessageData,
            MarkdownContext,
            GuildId,
            ChannelId,
            MessageId,
            GuildName,
            ChannelName
        )
    },
    publish_recipients(Job, UserIds, #{
        kind => message,
        user_ids => UserIds,
        channel_id => ChannelId,
        message_id => MessageId
    }).

-spec publish_recipients(map(), [integer()], meta()) -> ok | {error, term()}.
publish_recipients(Job, UserIds, Meta) ->
    Chunk = Job#{<<"user_ids">> => [integer_to_binary(UserId) || UserId <- UserIds]},
    case encode(Chunk) of
        {ok, Body} when byte_size(Body) > ?NATS_MAX_PAYLOAD_BYTES, length(UserIds) > 1 ->
            {Left, Right} = lists:split(length(UserIds) div 2, UserIds),
            LeftResult = publish_recipients(Job, Left, Meta),
            RightResult = publish_recipients(Job, Right, Meta),
            first_error(LeftResult, RightResult);
        Encoded ->
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
        caller_fields(Caller)
    ),
    publish(?SUBJECT_RING, Job, #{
        kind => ring,
        user_ids => [UserId],
        channel_id => ChannelId,
        message_id => MessageId
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

notification_fields_carry_title_body_and_tags_test() ->
    Fields = test_notification_fields(
        #{<<"content">> => <<"Hello world">>, <<"mentions">> => []},
        123,
        <<"Server">>,
        <<"general">>
    ),
    ?assertEqual(<<"Alice (#general, Server)">>, maps:get(<<"title">>, Fields)),
    ?assertEqual(<<"Hello world">>, maps:get(<<"body">>, Fields)),
    ?assertEqual(<<"channel:456:789">>, maps:get(<<"tag">>, Fields)),
    ?assertEqual(<<"channel:456">>, maps:get(<<"notification_tag">>, Fields)),
    ?assertEqual(<<"/channels/123/456/789">>, maps:get(<<"url">>, Fields)),
    ?assertEqual(null, maps:get(<<"image_url">>, Fields)).

notification_fields_use_single_sticker_preview_test() ->
    MessageData = #{
        <<"content">> => <<>>,
        <<"mentions">> => [],
        <<"stickers">> => [
            #{<<"id">> => <<"1">>, <<"name">> => <<"Wave">>, <<"animated">> => false}
        ]
    },
    Fields = test_notification_fields(MessageData, 0, undefined, undefined),
    ?assertEqual(<<"Sticker: Wave">>, maps:get(<<"body">>, Fields)).

notification_fields_use_multiple_sticker_preview_test() ->
    MessageData = #{
        <<"content">> => <<>>,
        <<"mentions">> => [],
        <<"stickers">> => [
            #{<<"id">> => <<"1">>, <<"name">> => <<"Wave">>, <<"animated">> => false},
            #{<<"id">> => <<"2">>, <<"name">> => <<"Dance">>, <<"animated">> => false}
        ]
    },
    Fields = test_notification_fields(MessageData, 0, undefined, undefined),
    ?assertEqual(<<"Stickers: Wave and Dance">>, maps:get(<<"body">>, Fields)).

notification_fields_use_attachment_fallback_test() ->
    MessageData = #{
        <<"content">> => <<>>,
        <<"mentions">> => [],
        <<"attachments">> => [#{<<"id">> => <<"1">>, <<"filename">> => <<"report.pdf">>}]
    },
    Fields = test_notification_fields(MessageData, 0, undefined, undefined),
    ?assertEqual(<<"Attachment: report.pdf">>, maps:get(<<"body">>, Fields)).

notification_fields_use_embed_fallback_test() ->
    MessageData = #{
        <<"content">> => <<>>,
        <<"mentions">> => [],
        <<"embeds">> => [#{<<"title">> => <<"Build">>, <<"description">> => <<"green">>}]
    },
    Fields = test_notification_fields(MessageData, 0, undefined, undefined),
    ?assertEqual(<<"Build: green">>, maps:get(<<"body">>, Fields)).

notification_fields_use_markdown_plaintext_context_test() ->
    MessageData = #{<<"content">> => <<"**Hi** <@1> <@&2> <#3>">>, <<"mentions">> => []},
    Context = #{
        <<"preserve_markdown">> => true,
        <<"users">> => #{<<"1">> => <<"Ada">>},
        <<"roles">> => #{<<"2">> => <<"Ops">>},
        <<"channels">> => #{<<"3">> => <<"alerts">>}
    },
    Fields = test_notification_fields(MessageData, 123, <<"Server">>, <<"general">>, Context),
    ?assertEqual(<<"**Hi** @Ada @Ops #alerts">>, maps:get(<<"body">>, Fields)).

notification_fields_use_author_nickname_in_guild_title_test() ->
    MessageData = #{<<"content">> => <<"Hello">>, <<"mentions">> => []},
    Context = #{<<"user_nicknames">> => #{<<"42">> => <<"Guild Alice">>}},
    Fields = test_notification_fields(MessageData, 123, <<"Server">>, <<"general">>, Context),
    ?assertEqual(<<"Guild Alice (#general, Server)">>, maps:get(<<"title">>, Fields)).

notification_fields_use_author_nickname_in_group_dm_title_test() ->
    MessageData = #{
        <<"content">> => <<"Hello">>,
        <<"channel_type">> => 3,
        <<"nicks">> => #{<<"42">> => <<"Group Alice">>},
        <<"mentions">> => []
    },
    Fields = test_notification_fields(MessageData, 0, undefined, undefined),
    ?assertEqual(<<"Group Alice (Group DM)">>, maps:get(<<"title">>, Fields)).

notification_fields_include_safe_attachment_image_test() ->
    MessageData = #{
        <<"content">> => <<"Photo">>,
        <<"mentions">> => [],
        <<"attachments">> => [
            #{
                <<"content_type">> => <<"image/png">>,
                <<"proxy_url">> => <<"https://cdn.example/image.png">>
            }
        ]
    },
    Fields = test_notification_fields(MessageData, 123, <<"Server">>, <<"general">>),
    ?assertEqual(<<"https://cdn.example/image.png">>, maps:get(<<"image_url">>, Fields)).

notification_fields_omit_sensitive_attachment_image_test() ->
    MessageData = #{
        <<"content">> => <<"Spoiler">>,
        <<"mentions">> => [],
        <<"attachments">> => [
            #{
                <<"content_type">> => <<"image/png">>,
                <<"proxy_url">> => <<"https://cdn.example/spoiler.png">>,
                <<"flags">> => 8
            }
        ]
    },
    Fields = test_notification_fields(MessageData, 123, <<"Server">>, <<"general">>),
    ?assertEqual(null, maps:get(<<"image_url">>, Fields)).

test_notification_fields(MessageData, GuildId, GuildName, ChannelName) ->
    test_notification_fields(MessageData, GuildId, GuildName, ChannelName, #{}).

test_notification_fields(MessageData, GuildId, GuildName, ChannelName, MarkdownContext) ->
    Author = #{<<"id">> => <<"42">>, <<"username">> => <<"Alice">>, <<"avatar">> => null},
    with_endpoint_env(fun() ->
        notification_fields(
            MessageData#{<<"author">> => Author},
            MarkdownContext,
            GuildId,
            456,
            789,
            GuildName,
            ChannelName
        )
    end).

with_captured_enqueues(Fun) ->
    {Jobs, _Dropped} = with_captured_outbox(fun(_OutboxJob) -> ok end, Fun),
    Jobs.

with_captured_outbox(EnqueueResult, Fun) ->
    Self = self(),
    ok = meck:new(push_outbox, [passthrough, no_link]),
    try
        ok = meck:expect(push_outbox, enqueue, fun(OutboxJob) ->
            Self ! {enqueued, OutboxJob},
            EnqueueResult(OutboxJob)
        end),
        ok = meck:expect(push_outbox, record_dropped, fun(Kind, Reason) ->
            Self ! {dropped, Kind, Reason},
            ok
        end),
        Fun(),
        {drain_enqueued([]), drain_dropped([])}
    after
        meck:unload(push_outbox)
    end.

drain_enqueued(Acc) ->
    receive
        {enqueued, OutboxJob} -> drain_enqueued([OutboxJob | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

drain_dropped(Acc) ->
    receive
        {dropped, Kind, Reason} -> drain_dropped([{Kind, Reason} | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

padded_job(UserIds, TargetBytes) ->
    Job = #{
        <<"v">> => ?JOB_VERSION,
        <<"config_version">> => ?LEGACY_CONFIG_VERSION,
        <<"pad">> => <<>>
    },
    {ok, Base} = encode(Job#{<<"user_ids">> => [integer_to_binary(Id) || Id <- UserIds]}),
    Padded = Job#{<<"pad">> => binary:copy(<<"a">>, TargetBytes - byte_size(Base))},
    {ok, Body} = encode(Padded#{<<"user_ids">> => [integer_to_binary(Id) || Id <- UserIds]}),
    ?assertEqual(TargetBytes, byte_size(Body)),
    Padded.

message_meta(UserIds) ->
    #{kind => message, user_ids => UserIds, channel_id => 20, message_id => 30}.

a_body_exactly_at_the_payload_limit_is_sent_as_one_job_test() ->
    UserIds = [1, 2],
    Job = padded_job(UserIds, ?NATS_MAX_PAYLOAD_BYTES),
    Jobs = with_captured_enqueues(fun() ->
        ?assertEqual(ok, publish_recipients(Job, UserIds, message_meta(UserIds)))
    end),
    ?assertMatch([#{user_ids := [1, 2]}], Jobs),
    [#{body := Body}] = Jobs,
    ?assertEqual(?NATS_MAX_PAYLOAD_BYTES, byte_size(Body)).

a_body_one_byte_over_the_payload_limit_is_split_test() ->
    UserIds = [1, 2],
    Job = padded_job(UserIds, ?NATS_MAX_PAYLOAD_BYTES + 1),
    {Jobs, Dropped} = with_captured_outbox(fun(_OutboxJob) -> ok end, fun() ->
        ?assertEqual(ok, publish_recipients(Job, UserIds, message_meta(UserIds)))
    end),
    ?assertMatch([#{user_ids := [1]}, #{user_ids := [2]}], Jobs),
    ?assertEqual([], Dropped).

a_single_recipient_over_the_payload_limit_is_dropped_test() ->
    UserIds = [1],
    Job = padded_job(UserIds, ?NATS_MAX_PAYLOAD_BYTES + 1),
    {Jobs, Dropped} = with_captured_outbox(fun(_OutboxJob) -> ok end, fun() ->
        ?assertEqual(
            {error, {payload_too_large, ?NATS_MAX_PAYLOAD_BYTES + 1}},
            publish_recipients(Job, UserIds, message_meta(UserIds))
        )
    end),
    ?assertEqual([], Jobs),
    ?assertEqual([{message, payload_too_large}], Dropped).

a_failed_left_chunk_still_publishes_the_right_chunk_test() ->
    UserIds = [1, 2],
    Job = padded_job(UserIds, ?NATS_MAX_PAYLOAD_BYTES + 1),
    Failure = {outbox_unavailable, shutdown},
    FailLeft = fun
        (#{user_ids := [1]}) -> {error, Failure};
        (_OutboxJob) -> ok
    end,
    {Jobs, Dropped} = with_captured_outbox(FailLeft, fun() ->
        ?assertEqual(
            {error, Failure}, publish_recipients(Job, UserIds, message_meta(UserIds))
        )
    end),
    ?assertMatch([#{user_ids := [1]}, #{user_ids := [2]}], Jobs),
    ?assertEqual([{message, outbox_unavailable}], Dropped).

an_enqueue_timeout_is_counted_apart_from_an_unavailable_outbox_test() ->
    Job = #{<<"v">> => ?JOB_VERSION},
    Meta = #{kind => clear, user_ids => [1], channel_id => 20, message_id => 30},
    {_TimeoutJobs, TimeoutDropped} = with_captured_outbox(
        fun(_OutboxJob) -> {error, {outbox_unavailable, timeout}} end,
        fun() -> publish(?SUBJECT_CLEAR, Job, Meta) end
    ),
    ?assertEqual([{clear, enqueue_timeout}], TimeoutDropped),
    {_NoprocJobs, NoprocDropped} = with_captured_outbox(
        fun(_OutboxJob) -> {error, outbox_unavailable} end,
        fun() -> publish(?SUBJECT_CLEAR, Job, Meta) end
    ),
    ?assertEqual([], NoprocDropped).

oversized_recipient_lists_are_split_under_the_payload_limit_test() ->
    UserIds = lists:seq(1000000000000000000, 1000000000000000000 + 59999),
    Job = #{<<"v">> => ?JOB_VERSION, <<"config_version">> => ?LEGACY_CONFIG_VERSION},
    Meta = #{kind => message, user_ids => UserIds, channel_id => 20, message_id => 30},
    Jobs = with_captured_enqueues(fun() ->
        ?assertEqual(ok, publish_recipients(Job, UserIds, Meta))
    end),
    ?assert(length(Jobs) > 1),
    lists:foreach(
        fun(#{body := Body, user_ids := ChunkIds, job := ChunkJob}) ->
            ?assert(byte_size(Body) =< ?NATS_MAX_PAYLOAD_BYTES),
            ?assertEqual(
                [integer_to_binary(UserId) || UserId <- ChunkIds],
                maps:get(<<"user_ids">>, ChunkJob)
            ),
            ?assertEqual(?LEGACY_CONFIG_VERSION, maps:get(<<"config_version">>, ChunkJob))
        end,
        Jobs
    ),
    ?assertEqual(UserIds, lists:append([ChunkIds || #{user_ids := ChunkIds} <- Jobs])).

recipient_lists_under_the_payload_limit_stay_in_one_job_test() ->
    UserIds = [1, 2, 3],
    Job = #{<<"v">> => ?JOB_VERSION, <<"config_version">> => ?LEGACY_CONFIG_VERSION},
    Meta = #{kind => message, user_ids => UserIds, channel_id => 20, message_id => 30},
    Jobs = with_captured_enqueues(fun() ->
        ?assertEqual(ok, publish_recipients(Job, UserIds, Meta))
    end),
    ?assertMatch(
        [#{kind := message, user_ids := [1, 2, 3], subject := ?SUBJECT_MESSAGE}], Jobs
    ).

-endif.
