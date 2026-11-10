%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_job_fields).
-typing([eqwalizer]).

-export([notification/7, thread/2, caller/1]).

-define(MAX_CALLER_NAME_BYTES, 128).

-spec caller(map()) -> map().
caller(#{caller_id := CallerId, caller_name := Name} = Caller) when
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
caller(_Caller) ->
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

-spec notification(
    map(), map(), integer(), integer(), integer(), binary() | undefined, binary() | undefined
) -> map().
notification(
    MessageData, MarkdownContext, GuildId, ChannelId, MessageId, GuildName, ChannelName
) ->
    ChannelIdBin = integer_to_binary(ChannelId),
    MessageIdBin = integer_to_binary(MessageId),
    Common = #{
        <<"tag">> => <<"channel:", ChannelIdBin/binary, ":", MessageIdBin/binary>>,
        <<"notification_tag">> => <<"channel:", ChannelIdBin/binary>>,
        <<"url">> => push_notification_format:build_url(GuildId, ChannelId, MessageId)
    },
    Fields = message_fields(MessageData, MarkdownContext, GuildId, GuildName, ChannelName),
    with_badge(maps:merge(Common, Fields)).

-spec message_fields(map(), map(), integer(), binary() | undefined, binary() | undefined) ->
    map().
message_fields(MessageData, MarkdownContext, GuildId, GuildName, ChannelName) ->
    AuthorData = maps:get(<<"author">>, MessageData, #{}),
    AuthorUsername = maps:get(<<"username">>, AuthorData, <<"Unknown">>),
    AuthorName = push_notification_format:resolve_author_name(
        MessageData, MarkdownContext, AuthorUsername
    ),
    Fields = #{
        <<"title">> => push_notification:build_notification_title(
            AuthorName, MessageData, GuildId, GuildName, ChannelName
        ),
        <<"body">> => push_notification_format:build_content_preview(
            MessageData, MarkdownContext
        ),
        <<"icon">> => push_notification_format:resolve_author_avatar_url(AuthorData),
        <<"image_url">> => nullable(push_image_url:extract(MessageData))
    },
    maps:merge(
        Fields,
        maps:merge(
            identity(AuthorName, MessageData, GuildId, GuildName, ChannelName),
            push_notification_format:preview_fields(MessageData, MarkdownContext)
        )
    ).

-spec identity(binary(), map(), integer(), binary() | undefined, binary() | undefined) -> map().
identity(AuthorName, MessageData, 0, _GuildName, ChannelName) ->
    Kind =
        case maps:get(<<"channel_type">>, MessageData, 1) of
            3 -> <<"group_dm">>;
            _ -> <<"dm">>
        end,
    binaries(#{
        <<"author_name">> => AuthorName,
        <<"conversation_kind">> => Kind,
        <<"conversation_name">> => group_name(Kind, ChannelName)
    });
identity(AuthorName, MessageData, _GuildId, GuildName, ChannelName) ->
    binaries(#{
        <<"author_name">> => AuthorName,
        <<"conversation_kind">> => <<"guild">>,
        <<"guild_name">> => GuildName,
        <<"conversation_name">> => push_notification:channel_label(MessageData, ChannelName)
    }).

-spec group_name(binary(), term()) -> binary() | undefined.
group_name(<<"group_dm">>, Name) when is_binary(Name), byte_size(Name) > 0 -> Name;
group_name(_Kind, _Name) -> undefined.

-spec binaries(map()) -> map().
binaries(Map) ->
    maps:filter(fun(_Key, Value) -> is_binary(Value) andalso byte_size(Value) > 0 end, Map).

-spec with_badge(map()) -> map().
with_badge(Fields) ->
    case fluxer_gateway_env:get(self_hosted) of
        true ->
            Fields;
        _ ->
            Fields#{
                <<"badge">> => push_utils:construct_static_asset_url(
                    <<"marketing/branding/symbol-white.svg">>
                )
            }
    end.

-spec thread(map(), binary() | undefined) -> map().
thread(MessageData, ChannelName) ->
    case push_notification:thread_fields(MessageData, ChannelName) of
        Fields when map_size(Fields) =:= 0 -> Fields;
        Fields -> Fields#{<<"kind">> => thread_job_kind(MessageData)}
    end.

-spec thread_job_kind(map()) -> binary().
thread_job_kind(#{<<"__thread_push">> := #{<<"forum_thread_created">> := true}}) ->
    <<"forum_thread_created">>;
thread_job_kind(_MessageData) ->
    <<"thread">>.

-spec nullable(binary() | undefined) -> binary() | null.
nullable(undefined) ->
    null;
nullable(Value) ->
    Value.
