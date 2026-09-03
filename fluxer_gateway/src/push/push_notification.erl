%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_notification).
-typing([eqwalizer]).

-export([
    build_notification_title/5,
    build_notification_payload/1,
    build_clear_notification_payload/4
]).

-export_type([notification_input/0]).

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
            <<"web_push">> => 8030,
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
        <<"type">> => <<"notification_clear">>,
        <<"action">> => <<"clear_channel">>,
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"message_id">> => integer_to_binary(MessageId),
        <<"target_user_id">> => integer_to_binary(TargetUserId),
        <<"notification_tag">> => Tag,
        <<"tag">> => Tag,
        <<"badge_count">> => BadgeValue
    },
    #{
        <<"type">> => <<"notification_clear">>,
        <<"action">> => <<"clear_channel">>,
        <<"silent">> => true,
        <<"tag">> => Tag,
        <<"notification_tag">> => Tag,
        <<"data">> => Data,
        <<"badge_count">> => BadgeValue,
        <<"web_push">> => 8030,
        <<"notification">> => #{
            <<"tag">> => Tag,
            <<"data">> => Data,
            <<"silent">> => true,
            <<"close">> => true
        }
    }.

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
