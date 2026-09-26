%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_notification).
-typing([eqwalizer]).

-export([
    build_notification_title/5,
    build_notification_payload/1,
    build_clear_notification_payload/4,
    fit_payload_json/2,
    is_clear/1
]).

-export_type([notification_input/0]).

-define(WEB_PUSH_MARKER, 8030).
-define(CLEAR_TYPE, <<"notification_clear">>).
-define(CLEAR_ACTION, <<"clear_channel">>).
-define(FALLBACK_TITLE, <<"Fluxer">>).
-define(FALLBACK_TAG, <<"fluxer-message">>).
-define(SHRUNK_BODY_MAX_BYTES, 40).
-define(MINIMAL_TITLE_MAX_BYTES, 120).
-define(SHRINK_STEPS, [media, icons, body, minimal]).
-define(BLOCK_KEYS, [<<"notification">>, <<"data">>]).
-define(MEDIA_KEYS, [<<"image_url">>, <<"image">>]).
-define(ICON_KEYS, [<<"icon">>, <<"badge">>, <<"author_avatar_url">>]).
-define(MINIMAL_DATA_KEYS, [
    <<"channel_id">>,
    <<"message_id">>,
    <<"guild_id">>,
    <<"target_user_id">>,
    <<"notification_tag">>,
    <<"url">>,
    <<"badge_count">>
]).

-type shrink_step() :: media | icons | body | minimal.

-type push_ctx() :: #{
    channel_id := integer(),
    message_id := integer(),
    guild_id := integer(),
    navigate_url := binary(),
    badge_value := non_neg_integer(),
    target_user_id := integer(),
    image_url := binary() | undefined,
    image_fields := map(),
    tag := binary()
}.
-type notification_input() :: #{
    message_data := map(),
    guild_id := integer(),
    channel_id := integer(),
    message_id := integer(),
    guild_name := binary() | undefined,
    channel_name := binary() | undefined,
    author_username := binary(),
    author_avatar_url := binary(),
    target_user_id := integer(),
    badge_count := non_neg_integer(),
    content_preview => binary() | undefined,
    markdown_context => map()
}.

-spec build_notification_title(
    binary(), map(), integer(), binary() | undefined, binary() | undefined
) -> binary().
build_notification_title(AuthorUsername, MessageData, GuildId, GuildName, ChannelName) ->
    ChannelType = maps:get(<<"channel_type">>, MessageData, 1),
    case GuildId of
        0 -> format_dm_title(AuthorUsername, ChannelType);
        _ -> format_guild_title(AuthorUsername, GuildName, ChannelName)
    end.

-spec format_dm_title(binary(), term()) -> binary().
format_dm_title(AuthorUsername, 3) ->
    iolist_to_binary([AuthorUsername, <<" (Group DM)">>]);
format_dm_title(AuthorUsername, _ChannelType) ->
    AuthorUsername.

-spec format_guild_title(binary(), binary() | undefined, binary() | undefined) -> binary().
format_guild_title(AuthorUsername, undefined, _) ->
    AuthorUsername;
format_guild_title(AuthorUsername, _, undefined) ->
    AuthorUsername;
format_guild_title(AuthorUsername, GName, ChanName) ->
    iolist_to_binary([AuthorUsername, <<" (#">>, ChanName, <<", ">>, GName, <<")">>]).

-spec build_notification_payload(notification_input()) -> map().
build_notification_payload(
    #{
        message_data := MessageData,
        guild_id := GuildId,
        channel_id := ChannelId,
        message_id := MessageId,
        guild_name := GuildName,
        channel_name := ChannelName,
        author_username := AuthorUsername,
        author_avatar_url := AuthorAvatarUrl,
        target_user_id := TargetUserId,
        badge_count := BadgeCount
    } = Input
) ->
    ContentPreview = resolve_content_preview(MessageData, Input),
    MarkdownContext = maps:get(markdown_context, Input, #{}),
    AuthorName = push_notification_format:resolve_author_name(
        MessageData, MarkdownContext, AuthorUsername
    ),
    Title = build_notification_title(
        AuthorName, MessageData, GuildId, GuildName, ChannelName
    ),
    Ctx = build_push_ctx(GuildId, ChannelId, MessageId, TargetUserId, BadgeCount, MessageData),
    assemble_payload(Ctx, Title, ContentPreview, AuthorAvatarUrl).

-spec resolve_content_preview(map(), notification_input()) -> binary().
resolve_content_preview(MessageData, Input) ->
    case maps:get(content_preview, Input, undefined) of
        Preview when is_binary(Preview) ->
            Preview;
        _ ->
            MarkdownContext = maps:get(markdown_context, Input, #{}),
            push_notification_format:build_content_preview(MessageData, MarkdownContext)
    end.

-spec build_channel_tag(integer()) -> binary().
build_channel_tag(ChannelId) ->
    <<"channel:", (integer_to_binary(ChannelId))/binary>>.

-spec build_message_tag(integer(), integer()) -> binary().
build_message_tag(ChannelId, MessageId) ->
    iolist_to_binary([
        <<"channel:">>,
        integer_to_binary(ChannelId),
        <<":">>,
        integer_to_binary(MessageId)
    ]).

-spec build_push_ctx(integer(), integer(), integer(), integer(), non_neg_integer(), map()) ->
    push_ctx().
build_push_ctx(GuildId, ChannelId, MessageId, TargetUserId, BadgeCount, MessageData) ->
    ImageUrl = push_notification_format:extract_image_url(MessageData),
    #{
        channel_id => ChannelId,
        message_id => MessageId,
        guild_id => GuildId,
        navigate_url => push_notification_format:build_url(GuildId, ChannelId, MessageId),
        badge_value => max(0, BadgeCount),
        target_user_id => TargetUserId,
        image_url => ImageUrl,
        image_fields => push_notification_format:maybe_image_fields(ImageUrl),
        tag => build_message_tag(ChannelId, MessageId)
    }.

-spec assemble_payload(push_ctx(), binary(), binary(), binary()) -> map().
assemble_payload(
    #{image_fields := ImageFields, tag := Tag} = Ctx,
    Title,
    ContentPreview,
    AuthorAvatarUrl
) ->
    Data = build_data(Ctx, AuthorAvatarUrl),
    Notification = build_notification_body(Ctx, Title, ContentPreview, AuthorAvatarUrl, Data),
    maps:merge(
        #{
            <<"web_push">> => ?WEB_PUSH_MARKER,
            <<"notification">> => Notification,
            <<"title">> => Title,
            <<"body">> => ContentPreview,
            <<"icon">> => AuthorAvatarUrl,
            <<"badge">> => push_badge_url(),
            <<"tag">> => Tag,
            <<"data">> => Data
        },
        ImageFields
    ).

-spec build_data(push_ctx(), binary()) -> map().
build_data(
    #{
        channel_id := ChannelId,
        message_id := MessageId,
        guild_id := GuildId,
        navigate_url := NavigateUrl,
        badge_value := BadgeValue,
        target_user_id := TargetUserId,
        image_url := ImageUrl,
        image_fields := ImageFields
    },
    AuthorAvatarUrl
) ->
    BaseData = #{
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"author_avatar_url">> => AuthorAvatarUrl,
        <<"message_id">> => integer_to_binary(MessageId),
        <<"notification_tag">> => build_channel_tag(ChannelId),
        <<"guild_id">> =>
            case GuildId of
                0 -> null;
                _ -> integer_to_binary(GuildId)
            end,
        <<"url">> => NavigateUrl,
        <<"badge_count">> => BadgeValue,
        <<"target_user_id">> => integer_to_binary(TargetUserId),
        <<"has_media">> => ImageUrl =/= undefined
    },
    maps:merge(BaseData, ImageFields).

-spec build_notification_body(push_ctx(), binary(), binary(), binary(), map()) -> map().
build_notification_body(
    #{
        navigate_url := NavigateUrl,
        badge_value := BadgeValue,
        image_fields := ImageFields,
        tag := Tag
    },
    Title,
    ContentPreview,
    AuthorAvatarUrl,
    Data
) ->
    BaseNotification = #{
        <<"title">> => Title,
        <<"body">> => ContentPreview,
        <<"icon">> => AuthorAvatarUrl,
        <<"badge">> => push_badge_url(),
        <<"tag">> => Tag,
        <<"navigate">> => NavigateUrl,
        <<"app_badge">> => integer_to_binary(BadgeValue),
        <<"data">> => Data
    },
    maps:merge(BaseNotification, ImageFields).

-spec push_badge_url() -> binary().
push_badge_url() ->
    push_utils:construct_static_asset_url(<<"marketing/branding/symbol-white.svg">>).

-spec build_clear_notification_payload(integer(), integer(), integer(), non_neg_integer()) ->
    map().
build_clear_notification_payload(TargetUserId, ChannelId, MessageId, BadgeCount) ->
    BadgeValue = max(0, BadgeCount),
    Tag = build_channel_tag(ChannelId),
    Data = #{
        <<"type">> => ?CLEAR_TYPE,
        <<"action">> => ?CLEAR_ACTION,
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"message_id">> => integer_to_binary(MessageId),
        <<"target_user_id">> => integer_to_binary(TargetUserId),
        <<"notification_tag">> => Tag,
        <<"tag">> => Tag,
        <<"badge_count">> => BadgeValue
    },
    #{
        <<"type">> => ?CLEAR_TYPE,
        <<"action">> => ?CLEAR_ACTION,
        <<"silent">> => true,
        <<"tag">> => Tag,
        <<"notification_tag">> => Tag,
        <<"data">> => Data,
        <<"badge_count">> => BadgeValue,
        <<"web_push">> => ?WEB_PUSH_MARKER,
        <<"notification">> => #{
            <<"tag">> => Tag,
            <<"data">> => Data,
            <<"silent">> => true,
            <<"close">> => true
        }
    }.

-spec is_clear(map()) -> boolean().
is_clear(Payload) ->
    maps:get(<<"type">>, Payload, undefined) =:= ?CLEAR_TYPE orelse
        maps:get(<<"action">>, Payload, undefined) =:= ?CLEAR_ACTION.

-spec fit_payload_json(map(), non_neg_integer()) -> binary().
fit_payload_json(Payload, Budget) ->
    Encoded = encode_payload(Payload),
    case byte_size(Encoded) =< Budget of
        true -> Encoded;
        false -> shrink_to_budget(Payload, Budget, ?SHRINK_STEPS)
    end.

-spec shrink_to_budget(map(), non_neg_integer(), [shrink_step()]) -> binary().
shrink_to_budget(Payload, _Budget, []) ->
    encode_payload(Payload);
shrink_to_budget(Payload, Budget, [Step | RemainingSteps]) ->
    Shrunk = shrink_payload(Payload, Step, Budget),
    Encoded = encode_payload(Shrunk),
    case byte_size(Encoded) =< Budget of
        true -> Encoded;
        false -> shrink_to_budget(Shrunk, Budget, RemainingSteps)
    end.

-spec shrink_payload(map(), shrink_step(), non_neg_integer()) -> map().
shrink_payload(Payload, media, _Budget) ->
    drop_block_keys(Payload, ?MEDIA_KEYS);
shrink_payload(Payload, icons, _Budget) ->
    drop_block_keys(Payload, ?ICON_KEYS);
shrink_payload(Payload, body, _Budget) ->
    truncate_block_bodies(Payload);
shrink_payload(Payload, minimal, Budget) ->
    minimal_payload(Payload, Budget).

-spec drop_block_keys(map(), [binary()]) -> map().
drop_block_keys(Payload, Keys) ->
    map_blocks(Payload, fun(Block) -> maps:without(Keys, Block) end).

-spec truncate_block_bodies(map()) -> map().
truncate_block_bodies(Payload) ->
    map_blocks(Payload, fun truncate_block_body/1).

-spec truncate_block_body(map()) -> map().
truncate_block_body(Block) ->
    case maps:get(<<"body">>, Block, undefined) of
        Body when is_binary(Body) ->
            Block#{
                <<"body">> => push_notification_format:truncate_bytes(
                    Body, ?SHRUNK_BODY_MAX_BYTES
                )
            };
        _ ->
            Block
    end.

-spec map_blocks(map(), fun((map()) -> map())) -> map().
map_blocks(Block, Apply) ->
    Apply(
        lists:foldl(fun(Key, Acc) -> map_nested_block(Key, Acc, Apply) end, Block, ?BLOCK_KEYS)
    ).

-spec map_nested_block(binary(), map(), fun((map()) -> map())) -> map().
map_nested_block(Key, Block, Apply) ->
    case maps:get(Key, Block, undefined) of
        Nested when is_map(Nested) -> Block#{Key => map_blocks(Nested, Apply)};
        _ -> Block
    end.

-spec minimal_payload(map(), non_neg_integer()) -> map().
minimal_payload(Payload, Budget) ->
    Title = push_notification_format:truncate_bytes(
        first_text(Payload, <<"title">>, ?FALLBACK_TITLE), ?MINIMAL_TITLE_MAX_BYTES
    ),
    Tag = first_text(Payload, <<"tag">>, ?FALLBACK_TAG),
    Url = minimal_url(Payload),
    Data = minimal_data(Payload),
    first_payload_within_budget(
        [
            minimal_envelope(Title, Tag, Url, Data),
            minimal_envelope(Title, Tag, <<>>, Data),
            minimal_envelope(Title, Tag, <<>>, #{}),
            minimal_envelope(Title, <<>>, <<>>, #{})
        ],
        Budget,
        Title
    ).

-spec first_payload_within_budget([map()], non_neg_integer(), binary()) -> map().
first_payload_within_budget([], Budget, Title) ->
    title_only_payload(Title, Budget);
first_payload_within_budget([Candidate | Rest], Budget, Title) ->
    case byte_size(encode_payload(Candidate)) =< Budget of
        true -> Candidate;
        false -> first_payload_within_budget(Rest, Budget, Title)
    end.

-spec minimal_envelope(binary(), binary(), binary(), map()) -> map().
minimal_envelope(Title, Tag, Url, Data) ->
    #{
        <<"web_push">> => ?WEB_PUSH_MARKER,
        <<"title">> => Title,
        <<"tag">> => Tag,
        <<"data">> => Data,
        <<"notification">> => #{
            <<"title">> => Title,
            <<"tag">> => Tag,
            <<"navigate">> => Url,
            <<"data">> => Data
        }
    }.

-spec title_only_payload(binary(), non_neg_integer()) -> map().
title_only_payload(Title, Budget) ->
    Candidate = #{<<"web_push">> => ?WEB_PUSH_MARKER, <<"title">> => Title},
    case byte_size(Title) =:= 0 orelse byte_size(encode_payload(Candidate)) =< Budget of
        true ->
            Candidate;
        false ->
            title_only_payload(
                push_notification_format:truncate_bytes(Title, byte_size(Title) - 1), Budget
            )
    end.

-spec minimal_url(map()) -> binary().
minimal_url(Payload) ->
    case first_text(Payload, <<"navigate">>, <<>>) of
        <<>> -> data_text(Payload, <<"url">>);
        Url -> Url
    end.

-spec minimal_data(map()) -> map().
minimal_data(Payload) ->
    case maps:get(<<"data">>, Payload, undefined) of
        Data when is_map(Data) -> maps:with(?MINIMAL_DATA_KEYS, Data);
        _ -> #{}
    end.

-spec data_text(map(), binary()) -> binary().
data_text(Payload, Key) ->
    case maps:get(<<"data">>, Payload, undefined) of
        Data when is_map(Data) -> text_or(maps:get(Key, Data, undefined), <<>>);
        _ -> <<>>
    end.

-spec first_text(map(), binary(), binary()) -> binary().
first_text(Payload, Key, Fallback) ->
    case text_or(maps:get(Key, Payload, undefined), <<>>) of
        <<>> ->
            nested_text(maps:get(<<"notification">>, Payload, undefined), Key, Fallback);
        Value ->
            Value
    end.

-spec nested_text(term(), binary(), binary()) -> binary().
nested_text(Notification, Key, Fallback) when is_map(Notification) ->
    text_or(maps:get(Key, Notification, undefined), Fallback);
nested_text(_Notification, _Key, Fallback) ->
    Fallback.

-spec text_or(term(), binary()) -> binary().
text_or(Value, _Fallback) when is_binary(Value), byte_size(Value) > 0 ->
    Value;
text_or(_Value, Fallback) ->
    Fallback.

-spec encode_payload(map()) -> binary().
encode_payload(Payload) ->
    iolist_to_binary(json:encode(Payload)).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_notification_title_dm_test() ->
    ?assertEqual(
        <<"Alice">>,
        build_notification_title(<<"Alice">>, #{}, 0, undefined, undefined)
    ).

build_notification_title_group_dm_test() ->
    MessageData = #{<<"channel_type">> => 3},
    ?assertEqual(
        <<"Alice (Group DM)">>,
        build_notification_title(<<"Alice">>, MessageData, 0, undefined, undefined)
    ).

build_notification_title_guild_test() ->
    ?assertEqual(
        <<"Alice (#general, My Server)">>,
        build_notification_title(<<"Alice">>, #{}, 123, <<"My Server">>, <<"general">>)
    ).

build_url_dm_test() ->
    ?assertEqual(<<"/channels/@me/456/789">>, push_notification_format:build_url(0, 456, 789)).

build_url_guild_test() ->
    ?assertEqual(
        <<"/channels/123/456/789">>, push_notification_format:build_url(123, 456, 789)
    ).

build_notification_payload_test() ->
    MessageData = #{<<"content">> => <<"Hello world">>, <<"mentions">> => []},
    Result = test_notification_payload(MessageData, 123, <<"Server">>, <<"general">>),
    Data = maps:get(<<"data">>, Result),
    ?assertEqual(<<"Alice (#general, Server)">>, maps:get(<<"title">>, Result)),
    ?assertEqual(<<"Hello world">>, maps:get(<<"body">>, Result)),
    ?assertEqual(5, maps:get(<<"badge_count">>, Data)),
    ?assertEqual(<<"channel:456:789">>, maps:get(<<"tag">>, Result)),
    ?assertEqual(<<"channel:456">>, maps:get(<<"notification_tag">>, Data)).

build_notification_payload_uses_single_sticker_preview_test() ->
    MessageData = #{
        <<"content">> => <<>>,
        <<"mentions">> => [],
        <<"stickers">> => [
            #{<<"id">> => <<"1">>, <<"name">> => <<"Wave">>, <<"animated">> => false}
        ]
    },
    Result = test_notification_payload(MessageData, 0, undefined, undefined),
    ?assertEqual(<<"Sticker: Wave">>, maps:get(<<"body">>, Result)),
    ?assertEqual(
        <<"Sticker: Wave">>, maps:get(<<"body">>, maps:get(<<"notification">>, Result))
    ).

build_notification_payload_uses_multiple_sticker_preview_test() ->
    MessageData = #{
        <<"content">> => <<>>,
        <<"mentions">> => [],
        <<"stickers">> => [
            #{<<"id">> => <<"1">>, <<"name">> => <<"Wave">>, <<"animated">> => false},
            #{<<"id">> => <<"2">>, <<"name">> => <<"Dance">>, <<"animated">> => false}
        ]
    },
    Result = test_notification_payload(MessageData, 0, undefined, undefined),
    ?assertEqual(<<"Stickers: Wave and Dance">>, maps:get(<<"body">>, Result)).

build_notification_payload_uses_attachment_fallback_test() ->
    MessageData = #{
        <<"content">> => <<>>,
        <<"mentions">> => [],
        <<"attachments">> => [
            #{<<"id">> => <<"1">>, <<"filename">> => <<"report.pdf">>}
        ]
    },
    Result = test_notification_payload(MessageData, 0, undefined, undefined),
    ?assertEqual(<<"Attachment: report.pdf">>, maps:get(<<"body">>, Result)).

build_notification_payload_uses_embed_fallback_test() ->
    MessageData = #{
        <<"content">> => <<>>,
        <<"mentions">> => [],
        <<"embeds">> => [
            #{<<"title">> => <<"Build">>, <<"description">> => <<"green">>}
        ]
    },
    Result = test_notification_payload(MessageData, 0, undefined, undefined),
    ?assertEqual(<<"Build: green">>, maps:get(<<"body">>, Result)).

build_notification_payload_uses_markdown_plaintext_context_test() ->
    MessageData = #{
        <<"content">> => <<"**Hi** <@1> <@&2> <#3>">>,
        <<"mentions">> => []
    },
    Context = #{
        <<"preserve_markdown">> => true,
        <<"users">> => #{<<"1">> => <<"Ada">>},
        <<"roles">> => #{<<"2">> => <<"Ops">>},
        <<"channels">> => #{<<"3">> => <<"alerts">>}
    },
    Result = test_notification_payload(
        MessageData, 123, <<"Server">>, <<"general">>, Context
    ),
    ?assertEqual(<<"**Hi** @Ada @Ops #alerts">>, maps:get(<<"body">>, Result)).

build_notification_payload_uses_author_nickname_in_guild_title_test() ->
    MessageData = #{
        <<"content">> => <<"Hello">>,
        <<"author">> => #{<<"id">> => <<"42">>, <<"username">> => <<"Alice">>},
        <<"mentions">> => []
    },
    Context = #{<<"user_nicknames">> => #{<<"42">> => <<"Guild Alice">>}},
    Result = test_notification_payload(
        MessageData, 123, <<"Server">>, <<"general">>, Context
    ),
    ?assertEqual(<<"Guild Alice (#general, Server)">>, maps:get(<<"title">>, Result)).

build_notification_payload_uses_author_nickname_in_group_dm_title_test() ->
    MessageData = #{
        <<"content">> => <<"Hello">>,
        <<"channel_type">> => 3,
        <<"author">> => #{<<"id">> => <<"42">>, <<"username">> => <<"Alice">>},
        <<"nicks">> => #{<<"42">> => <<"Group Alice">>},
        <<"mentions">> => []
    },
    Result = test_notification_payload(MessageData, 0, undefined, undefined),
    ?assertEqual(<<"Group Alice (Group DM)">>, maps:get(<<"title">>, Result)).

build_notification_payload_includes_safe_attachment_image_test() ->
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
    Result = test_notification_payload(MessageData, 123, <<"Server">>, <<"general">>),
    ImgUrl = <<"https://cdn.example/image.png">>,
    DataMap = maps:get(<<"data">>, Result),
    ?assertEqual(ImgUrl, maps:get(<<"image_url">>, Result)),
    ?assertEqual(ImgUrl, maps:get(<<"image_url">>, DataMap)),
    ?assertEqual(true, maps:get(<<"has_media">>, DataMap)).

build_notification_payload_omits_sensitive_attachment_image_test() ->
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
    Result = test_notification_payload(MessageData, 123, <<"Server">>, <<"general">>),
    ?assertEqual(false, maps:get(<<"has_media">>, maps:get(<<"data">>, Result))),
    ?assertEqual(false, maps:is_key(<<"image_url">>, Result)).

build_clear_notification_payload_test() ->
    Result = build_clear_notification_payload(999, 456, 789, 2),
    Data = maps:get(<<"data">>, Result),
    ?assertEqual(<<"notification_clear">>, maps:get(<<"type">>, Result)),
    ?assertEqual(<<"clear_channel">>, maps:get(<<"action">>, Result)),
    ?assertEqual(<<"channel:456">>, maps:get(<<"tag">>, Result)),
    ?assertEqual(<<"channel:456">>, maps:get(<<"notification_tag">>, Data)),
    ?assertEqual(2, maps:get(<<"badge_count">>, Data)).

is_clear_test() ->
    ?assertEqual(true, is_clear(build_clear_notification_payload(999, 456, 789, 2))),
    ?assertEqual(
        false,
        is_clear(
            test_notification_payload(#{<<"content">> => <<"hi">>}, 0, undefined, undefined)
        )
    ).

fit_payload_json_leaves_a_payload_within_budget_untouched_test() ->
    Payload = test_notification_payload(#{<<"content">> => <<"hi">>}, 0, undefined, undefined),
    Encoded = encode_payload(Payload),
    ?assertEqual(Encoded, fit_payload_json(Payload, byte_size(Encoded))).

fit_payload_json_drops_media_first_test() ->
    ImageUrl = <<"https://media.example/external/", (binary:copy(<<"i">>, 400))/binary>>,
    MessageData = #{
        <<"content">> => <<"Photo">>,
        <<"mentions">> => [],
        <<"attachments">> => [
            #{<<"content_type">> => <<"image/png">>, <<"proxy_url">> => ImageUrl}
        ]
    },
    Payload = test_notification_payload(MessageData, 123, <<"Server">>, <<"general">>),
    ?assert(byte_size(encode_payload(Payload)) > 2713),
    Fitted = fit_payload_json(Payload, 2713),
    ?assert(byte_size(Fitted) =< 2713),
    ?assertEqual(nomatch, binary:match(Fitted, ImageUrl)),
    Decoded = json:decode(Fitted),
    ?assertEqual(<<"Photo">>, maps:get(<<"body">>, Decoded)),
    ?assertEqual(<<"http://avatar">>, maps:get(<<"icon">>, Decoded)).

fit_payload_json_falls_back_to_a_minimal_payload_test() ->
    Payload = test_notification_payload(
        #{<<"content">> => <<"hi">>, <<"mentions">> => []},
        123,
        binary:copy(<<"g">>, 4000),
        binary:copy(<<"c">>, 4000)
    ),
    Fitted = fit_payload_json(Payload, 2713),
    ?assert(byte_size(Fitted) =< 2713),
    Decoded = json:decode(Fitted),
    ?assertEqual(<<"channel:456:789">>, maps:get(<<"tag">>, Decoded)),
    ?assertEqual(?MINIMAL_TITLE_MAX_BYTES, byte_size(maps:get(<<"title">>, Decoded))).

fit_payload_json_reaches_a_title_only_payload_test() ->
    Payload = test_notification_payload(
        #{<<"content">> => <<"hi">>, <<"mentions">> => []},
        123,
        binary:copy(<<"g">>, 4000),
        binary:copy(<<"c">>, 4000)
    ),
    Fitted = fit_payload_json(Payload, 60),
    ?assert(byte_size(Fitted) =< 60),
    ?assertEqual([<<"title">>, <<"web_push">>], lists:sort(maps:keys(json:decode(Fitted)))).

fit_payload_json_never_splits_a_utf8_character_test() ->
    Emoji = binary:copy(<<"\xF0\x9F\x98\x80">>, 2000),
    Payload = test_notification_payload(
        #{<<"content">> => <<"hi">>, <<"mentions">> => []},
        123,
        Emoji,
        <<"a", Emoji/binary>>
    ),
    Fitted = fit_payload_json(Payload, 2713),
    ?assert(byte_size(Fitted) =< 2713),
    Title = maps:get(<<"title">>, json:decode(Fitted)),
    ?assertMatch(Bin when is_binary(Bin), unicode:characters_to_binary(Title, utf8, utf8)).

test_notification_payload(MessageData, GuildId, GuildName, ChannelName) ->
    test_notification_payload(MessageData, GuildId, GuildName, ChannelName, #{}).

test_notification_payload(MessageData, GuildId, GuildName, ChannelName, MarkdownContext) ->
    build_notification_payload(#{
        message_data => MessageData,
        guild_id => GuildId,
        channel_id => 456,
        message_id => 789,
        guild_name => GuildName,
        channel_name => ChannelName,
        author_username => <<"Alice">>,
        author_avatar_url => <<"http://avatar">>,
        target_user_id => 999,
        badge_count => 5,
        markdown_context => MarkdownContext
    }).

-endif.
