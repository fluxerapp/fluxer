%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_notification_format).
-typing([eqwalizer]).

-export([
    build_content_preview/1,
    build_content_preview/2,
    build_markdown_context/4,
    resolve_author_name/3,
    extract_image_url/1,
    maybe_image_fields/1,
    build_url/3
]).

-define(MAX_MENTIONS_FOR_PUSH, 50).
-define(CHANNEL_TYPE_GUILD_TEXT, 0).
-define(CHANNEL_TYPE_GUILD_VOICE, 2).
-define(CHANNEL_TYPE_GUILD_CATEGORY, 4).
-define(CHANNEL_TYPE_GUILD_LINK, 998).

-spec build_content_preview(map()) -> binary().
build_content_preview(MessageData) ->
    build_content_preview(MessageData, #{}).

-spec build_content_preview(map(), map()) -> binary().
build_content_preview(MessageData, MarkdownContext) ->
    Content = push_utils:normalize_binary(maps:get(<<"content">>, MessageData, <<"">>), <<>>),
    Preview = push_markdown_plaintext:render_push_preview(Content, MarkdownContext),
    case Preview of
        <<>> ->
            truncate_preview(build_content_fallback_preview(MessageData));
        _ ->
            truncate_preview(Preview)
    end.

-spec build_markdown_context(map(), non_neg_integer(), map(), map()) -> map().
build_markdown_context(MessageData, GuildId, RoleNames, GuildData) ->
    UserNicknames = user_nicknames(MessageData, GuildId, GuildData),
    #{
        <<"preserve_markdown">> => true,
        <<"include_emoji_names">> => true,
        <<"include_link_urls">> => false,
        <<"users">> => mention_user_names(
            maps:get(<<"mentions">>, MessageData, []), UserNicknames
        ),
        <<"user_nicknames">> => UserNicknames,
        <<"roles">> => role_names_for_context(GuildId, RoleNames),
        <<"channels">> => channel_names_for_context(MessageData, GuildData)
    }.

-spec mention_user_names(term()) -> map().
mention_user_names(Mentions) when is_list(Mentions) ->
    mention_user_names(Mentions, #{});
mention_user_names(_) ->
    #{}.

-spec mention_user_names(term(), map()) -> map().
mention_user_names(Mentions, UserNicknames) when is_list(Mentions), is_map(UserNicknames) ->
    mention_user_names(lists:sublist(Mentions, ?MAX_MENTIONS_FOR_PUSH), UserNicknames, #{});
mention_user_names(Mentions, _UserNicknames) ->
    mention_user_names(Mentions).

-spec mention_user_names([term()], map(), map()) -> map().
mention_user_names([], _UserNicknames, Acc) ->
    Acc;
mention_user_names([Mention | Rest], UserNicknames, Acc) when is_map(Mention) ->
    mention_user_names(
        Rest, UserNicknames, maybe_put_mention_user(Mention, UserNicknames, Acc)
    );
mention_user_names([_Mention | Rest], UserNicknames, Acc) ->
    mention_user_names(Rest, UserNicknames, Acc).

-spec maybe_put_mention_user(map(), map(), map()) -> map().
maybe_put_mention_user(Mention, UserNicknames, Acc) ->
    Id = push_utils:normalize_binary(maps:get(<<"id">>, Mention, undefined)),
    Name = mention_user_name(Mention, UserNicknames),
    maybe_put_name(Id, Name, Acc).

-spec mention_user_name(map(), map()) -> binary() | undefined.
mention_user_name(Mention, UserNicknames) ->
    first_nonempty_binary([
        user_nickname(maps:get(<<"id">>, Mention, undefined), UserNicknames),
        maps:get(<<"global_name">>, Mention, undefined),
        maps:get(<<"username">>, Mention, undefined)
    ]).

-spec resolve_author_name(map(), map(), binary()) -> binary().
resolve_author_name(MessageData, MarkdownContext, FallbackName) ->
    AuthorData = maps:get(<<"author">>, MessageData, #{}),
    UserNicknames = user_nicknames_from_context_or_message(MessageData, MarkdownContext),
    case
        first_nonempty_binary([
            user_nickname(maps:get(<<"id">>, AuthorData, undefined), UserNicknames),
            maps:get(<<"global_name">>, AuthorData, undefined),
            maps:get(<<"username">>, AuthorData, undefined),
            FallbackName
        ])
    of
        Name when is_binary(Name) -> Name;
        undefined -> FallbackName
    end.

-spec user_nicknames_from_context_or_message(map(), map()) -> map().
user_nicknames_from_context_or_message(MessageData, MarkdownContext) when
    is_map(MarkdownContext)
->
    case maps:get(<<"user_nicknames">>, MarkdownContext, undefined) of
        UserNicknames when is_map(UserNicknames), map_size(UserNicknames) > 0 -> UserNicknames;
        _ -> group_dm_user_nicknames(MessageData)
    end;
user_nicknames_from_context_or_message(MessageData, _MarkdownContext) ->
    group_dm_user_nicknames(MessageData).

-spec user_nicknames(map(), non_neg_integer(), map()) -> map().
user_nicknames(MessageData, 0, _GuildData) ->
    group_dm_user_nicknames(MessageData);
user_nicknames(_MessageData, _GuildId, GuildData) ->
    guild_user_nicknames(GuildData).

-spec guild_user_nicknames(map()) -> map().
guild_user_nicknames(GuildData) when is_map(GuildData) ->
    maps:fold(fun put_member_nickname/3, #{}, guild_data_index:member_map(GuildData));
guild_user_nicknames(_GuildData) ->
    #{}.

-spec put_member_nickname(term(), term(), map()) -> map().
put_member_nickname(UserId, Member, Acc) when is_integer(UserId), is_map(Member) ->
    Nickname = push_utils:normalize_binary(maps:get(<<"nick">>, Member, undefined)),
    maybe_put_name(integer_to_binary(UserId), Nickname, Acc);
put_member_nickname(_UserId, _Member, Acc) ->
    Acc.

-spec group_dm_user_nicknames(map()) -> map().
group_dm_user_nicknames(MessageData) when is_map(MessageData) ->
    lists:foldl(
        fun merge_user_nickname_source/2,
        #{},
        [
            nested_channel_nicks(MessageData),
            maps:get(<<"channel_nicks">>, MessageData, undefined),
            maps:get(<<"nicks">>, MessageData, undefined)
        ]
    );
group_dm_user_nicknames(_MessageData) ->
    #{}.

-spec nested_channel_nicks(map()) -> term().
nested_channel_nicks(MessageData) ->
    case maps:get(<<"channel">>, MessageData, undefined) of
        Channel when is_map(Channel) -> maps:get(<<"nicks">>, Channel, undefined);
        _ -> undefined
    end.

-spec merge_user_nickname_source(term(), map()) -> map().
merge_user_nickname_source(Nicknames, Acc) when is_map(Nicknames) ->
    maps:fold(fun put_user_nickname/3, Acc, Nicknames);
merge_user_nickname_source(_Nicknames, Acc) ->
    Acc.

-spec put_user_nickname(term(), term(), map()) -> map().
put_user_nickname(UserId0, Nickname, Acc) ->
    maybe_put_name(user_id_binary(UserId0), push_utils:normalize_binary(Nickname), Acc).

-spec user_nickname(term(), map()) -> binary() | undefined.
user_nickname(UserId0, UserNicknames) when is_map(UserNicknames) ->
    case user_id_binary(UserId0) of
        Id when is_binary(Id) -> maps:get(Id, UserNicknames, undefined);
        undefined -> undefined
    end.

-spec user_id_binary(term()) -> binary() | undefined.
user_id_binary(UserId) when is_integer(UserId), UserId > 0 ->
    integer_to_binary(UserId);
user_id_binary(UserId) ->
    case snowflake_id:parse_maybe(UserId) of
        Id when is_integer(Id), Id > 0 -> integer_to_binary(Id);
        undefined -> undefined
    end.

-spec role_names_for_context(non_neg_integer(), map()) -> map().
role_names_for_context(0, _RoleNames) ->
    #{};
role_names_for_context(_GuildId, RoleNames) when is_map(RoleNames) ->
    maps:fold(fun put_role_name/3, #{}, RoleNames);
role_names_for_context(_GuildId, _RoleNames) ->
    #{}.

-spec put_role_name(term(), term(), map()) -> map().
put_role_name(RoleId, Name, Acc) ->
    Id = role_id_binary(RoleId),
    maybe_put_name(Id, push_utils:normalize_binary(Name), Acc).

-spec role_id_binary(term()) -> binary() | undefined.
role_id_binary(RoleId) when is_integer(RoleId), RoleId > 0 ->
    integer_to_binary(RoleId);
role_id_binary(RoleId) ->
    push_utils:normalize_binary(RoleId).

-spec channel_names_for_context(map(), map()) -> map().
channel_names_for_context(MessageData, GuildData) ->
    FromMentions = mention_channel_names(maps:get(<<"mention_channels">>, MessageData, [])),
    Content = push_utils:normalize_binary(maps:get(<<"content">>, MessageData, <<"">>), <<>>),
    MentionedIds = channel_mention_ids(Content),
    maps:merge(channel_names_from_guild_data(MentionedIds, GuildData), FromMentions).

-spec mention_channel_names(term()) -> map().
mention_channel_names(MentionChannels) when is_list(MentionChannels) ->
    lists:foldl(fun put_mention_channel_name/2, #{}, MentionChannels);
mention_channel_names(_) ->
    #{}.

-spec put_mention_channel_name(term(), map()) -> map().
put_mention_channel_name(Channel, Acc) when is_map(Channel) ->
    case is_copyable_channel_mention(Channel) of
        true ->
            Id = push_utils:normalize_binary(maps:get(<<"id">>, Channel, undefined)),
            Name = push_utils:normalize_binary(maps:get(<<"name">>, Channel, undefined)),
            maybe_put_name(Id, Name, Acc);
        false ->
            Acc
    end;
put_mention_channel_name(_Channel, Acc) ->
    Acc.

-spec channel_mention_ids(binary()) -> [integer()].
channel_mention_ids(Content) ->
    channel_mention_ids(Content, []).

-spec channel_mention_ids(binary(), [integer()]) -> [integer()].
channel_mention_ids(Content, Acc) ->
    case binary:match(Content, <<"<#">>) of
        nomatch ->
            lists:usort(Acc);
        {Start, _Length} ->
            AfterPrefix = binary:part(Content, Start + 2, byte_size(Content) - Start - 2),
            channel_mention_tail(AfterPrefix, Acc)
    end.

-spec channel_mention_tail(binary(), [integer()]) -> [integer()].
channel_mention_tail(AfterPrefix, Acc) ->
    case binary:match(AfterPrefix, <<">">>) of
        nomatch ->
            lists:usort(Acc);
        {End, _Length} ->
            IdBin = binary:part(AfterPrefix, 0, End),
            Rest = binary:part(AfterPrefix, End + 1, byte_size(AfterPrefix) - End - 1),
            channel_mention_ids(Rest, maybe_prepend_snowflake(IdBin, Acc))
    end.

-spec maybe_prepend_snowflake(binary(), [integer()]) -> [integer()].
maybe_prepend_snowflake(IdBin, Acc) ->
    case snowflake_id:parse_optional(IdBin) of
        Id when is_integer(Id), Id > 0 -> [Id | Acc];
        _ -> Acc
    end.

-spec channel_names_from_guild_data([integer()], map()) -> map().
channel_names_from_guild_data([], _GuildData) ->
    #{};
channel_names_from_guild_data(ChannelIds, GuildData) when is_map(GuildData) ->
    ChannelIndex = guild_data_index:channel_index(GuildData),
    lists:foldl(
        fun(ChannelId, Acc) -> put_index_channel_name(ChannelId, ChannelIndex, Acc) end,
        #{},
        ChannelIds
    ).

-spec put_index_channel_name(integer(), map(), map()) -> map().
put_index_channel_name(ChannelId, ChannelIndex, Acc) ->
    Channel = maps:get(ChannelId, ChannelIndex, undefined),
    put_index_channel_name_from_channel(ChannelId, Channel, Acc).

-spec put_index_channel_name_from_channel(integer(), term(), map()) -> map().
put_index_channel_name_from_channel(ChannelId, Channel, Acc) when is_map(Channel) ->
    maybe_put_copyable_index_channel_name(
        is_copyable_channel_mention(Channel), ChannelId, Channel, Acc
    );
put_index_channel_name_from_channel(_ChannelId, _Channel, Acc) ->
    Acc.

-spec maybe_put_copyable_index_channel_name(boolean(), integer(), map(), map()) -> map().
maybe_put_copyable_index_channel_name(true, ChannelId, Channel, Acc) ->
    put_copyable_index_channel_name(ChannelId, Channel, Acc);
maybe_put_copyable_index_channel_name(false, _ChannelId, _Channel, Acc) ->
    Acc.

-spec put_copyable_index_channel_name(integer(), map(), map()) -> map().
put_copyable_index_channel_name(ChannelId, Channel, Acc) ->
    Name = push_utils:normalize_binary(maps:get(<<"name">>, Channel, undefined)),
    maybe_put_name(integer_to_binary(ChannelId), Name, Acc).

-spec is_copyable_channel_mention(map()) -> boolean().
is_copyable_channel_mention(Channel) ->
    case maps:get(<<"type">>, Channel, undefined) of
        undefined -> true;
        Type -> is_copyable_channel_type(Type)
    end.

-spec is_copyable_channel_type(term()) -> boolean().
is_copyable_channel_type(?CHANNEL_TYPE_GUILD_TEXT) ->
    true;
is_copyable_channel_type(?CHANNEL_TYPE_GUILD_VOICE) ->
    true;
is_copyable_channel_type(?CHANNEL_TYPE_GUILD_CATEGORY) ->
    true;
is_copyable_channel_type(?CHANNEL_TYPE_GUILD_LINK) ->
    true;
is_copyable_channel_type(_Type) ->
    false.

-spec maybe_put_name(binary() | undefined, binary() | undefined, map()) -> map().
maybe_put_name(Id, Name, Acc) when is_binary(Id), is_binary(Name), byte_size(Name) > 0 ->
    Acc#{Id => Name};
maybe_put_name(_Id, _Name, Acc) ->
    Acc.

-spec first_nonempty_binary([term()]) -> binary() | undefined.
first_nonempty_binary([]) ->
    undefined;
first_nonempty_binary([Value | Rest]) ->
    case push_utils:normalize_binary(Value) of
        Bin when is_binary(Bin), byte_size(Bin) > 0 -> Bin;
        _ -> first_nonempty_binary(Rest)
    end.

-spec truncate_preview(binary()) -> binary().
truncate_preview(Content) when byte_size(Content) > 100 ->
    binary:part(Content, 0, 100);
truncate_preview(Content) ->
    Content.

-spec build_content_fallback_preview(map()) -> binary().
build_content_fallback_preview(MessageData) ->
    case
        first_nonempty_binary([
            build_sticker_preview(maps:get(<<"stickers">>, MessageData, [])),
            build_attachment_preview(maps:get(<<"attachments">>, MessageData, [])),
            build_embed_preview(maps:get(<<"embeds">>, MessageData, []))
        ])
    of
        Preview when is_binary(Preview) -> Preview;
        undefined -> <<>>
    end.

-spec build_sticker_preview(term()) -> binary().
build_sticker_preview([Sticker]) when is_map(Sticker) ->
    case push_utils:normalize_binary(maps:get(<<"name">>, Sticker, <<>>)) of
        <<>> -> <<"Sticker">>;
        Name when is_binary(Name) -> iolist_to_binary([<<"Sticker: ">>, Name])
    end;
build_sticker_preview([_, _ | _] = Stickers) ->
    case sticker_names(Stickers) of
        [] -> <<"Stickers">>;
        Names -> iolist_to_binary([<<"Stickers: ">>, format_name_list(Names)])
    end;
build_sticker_preview(_) ->
    <<>>.

-spec build_attachment_preview(term()) -> binary().
build_attachment_preview([Attachment | _Rest]) when is_map(Attachment) ->
    case push_utils:normalize_binary(maps:get(<<"filename">>, Attachment, undefined)) of
        Filename when is_binary(Filename), byte_size(Filename) > 0 ->
            iolist_to_binary([<<"Attachment: ">>, Filename]);
        _ ->
            <<"Attachment">>
    end;
build_attachment_preview(_) ->
    <<>>.

-spec build_embed_preview(term()) -> binary().
build_embed_preview([Embed | _Rest]) when is_map(Embed) ->
    Description = push_utils:normalize_binary(maps:get(<<"description">>, Embed, undefined)),
    Title = push_utils:normalize_binary(maps:get(<<"title">>, Embed, undefined)),
    Fields = maps:get(<<"fields">>, Embed, []),
    embed_preview(Title, Description, Fields);
build_embed_preview(_) ->
    <<>>.

-spec embed_preview(binary() | undefined, binary() | undefined, term()) -> binary().
embed_preview(Title, Description, _Fields) when
    is_binary(Title), byte_size(Title) > 0, is_binary(Description), byte_size(Description) > 0
->
    iolist_to_binary([Title, <<": ">>, Description]);
embed_preview(_Title, Description, _Fields) when
    is_binary(Description), byte_size(Description) > 0
->
    Description;
embed_preview(Title, _Description, _Fields) when is_binary(Title), byte_size(Title) > 0 ->
    Title;
embed_preview(_Title, _Description, [Field | _Rest]) when is_map(Field) ->
    embed_field_preview(Field);
embed_preview(_Title, _Description, _Fields) ->
    <<>>.

-spec embed_field_preview(map()) -> binary().
embed_field_preview(Field) ->
    Name = push_utils:normalize_binary(maps:get(<<"name">>, Field, undefined)),
    Value = push_utils:normalize_binary(maps:get(<<"value">>, Field, undefined)),
    embed_field_preview(Name, Value).

-spec embed_field_preview(binary() | undefined, binary() | undefined) -> binary().
embed_field_preview(Name, Value) when
    is_binary(Name), byte_size(Name) > 0, is_binary(Value), byte_size(Value) > 0
->
    iolist_to_binary([Name, <<": ">>, Value]);
embed_field_preview(_Name, _Value) ->
    <<>>.

-spec sticker_names(list()) -> [binary()].
sticker_names(Stickers) ->
    lists:filtermap(
        fun
            (Sticker) when is_map(Sticker) ->
                sticker_name(Sticker);
            (_) ->
                false
        end,
        Stickers
    ).

-spec sticker_name(map()) -> false | {true, binary()}.
sticker_name(Sticker) ->
    case push_utils:normalize_binary(maps:get(<<"name">>, Sticker, <<>>)) of
        Name when is_binary(Name), byte_size(Name) > 0 -> {true, Name};
        _ -> false
    end.

-spec join_binary(nonempty_list(binary()), binary()) -> binary().
join_binary([First | Rest], Separator) ->
    lists:foldl(
        fun(Item, Acc) -> <<Acc/binary, Separator/binary, Item/binary>> end,
        First,
        Rest
    ).

-spec format_name_list(nonempty_list(binary())) -> binary().
format_name_list([Name]) ->
    Name;
format_name_list([First, Second]) ->
    iolist_to_binary([First, <<" and ">>, Second]);
format_name_list(Names) ->
    [Last | ReversedRest] = lists:reverse(Names),
    Prefix = join_binary(lists:reverse(ReversedRest), <<", ">>),
    iolist_to_binary([Prefix, <<", and ">>, Last]).

-spec extract_image_url(map()) -> binary() | undefined.
extract_image_url(MessageData) ->
    case extract_attachment_image_url(maps:get(<<"attachments">>, MessageData, [])) of
        undefined -> extract_embed_image_url(maps:get(<<"embeds">>, MessageData, []));
        ImageUrl -> ImageUrl
    end.

-spec extract_attachment_image_url(term()) -> binary() | undefined.
extract_attachment_image_url([Attachment | Rest]) when is_map(Attachment) ->
    case check_media_safe_url(Attachment) of
        {ok, Url} -> Url;
        unsafe -> extract_attachment_image_url(Rest)
    end;
extract_attachment_image_url([_ | Rest]) ->
    extract_attachment_image_url(Rest);
extract_attachment_image_url(_) ->
    undefined.

-spec extract_embed_image_url(term()) -> binary() | undefined.
extract_embed_image_url([Embed | Rest]) when is_map(Embed) ->
    case first_embed_media_url(Embed) of
        undefined -> extract_embed_image_url(Rest);
        Url -> Url
    end;
extract_embed_image_url([_ | Rest]) ->
    extract_embed_image_url(Rest);
extract_embed_image_url(_) ->
    undefined.

-spec first_embed_media_url(map()) -> binary() | undefined.
first_embed_media_url(Embed) ->
    Candidates = [
        maps:get(<<"image">>, Embed, undefined),
        maps:get(<<"thumbnail">>, Embed, undefined)
    ],
    first_embed_media_url_from_candidates(Candidates).

-spec first_embed_media_url_from_candidates(list()) -> binary() | undefined.
first_embed_media_url_from_candidates([Media | Rest]) when is_map(Media) ->
    case check_media_safe_url(Media) of
        {ok, Url} -> Url;
        unsafe -> first_embed_media_url_from_candidates(Rest)
    end;
first_embed_media_url_from_candidates([_ | Rest]) ->
    first_embed_media_url_from_candidates(Rest);
first_embed_media_url_from_candidates([]) ->
    undefined.

-spec check_media_safe_url(map()) -> {ok, binary()} | unsafe.
check_media_safe_url(Media) ->
    ProxyUrl = maps:get(<<"proxy_url">>, Media, undefined),
    FallbackUrl = maps:get(<<"url">>, Media, undefined),
    Url = push_utils:normalize_binary(
        case ProxyUrl of
            undefined -> FallbackUrl;
            _ -> ProxyUrl
        end
    ),
    DefaultCT = maps:get(<<"content_type">>, Media, <<>>),
    ContentType = push_utils:normalize_binary(DefaultCT),
    Flags = bitset:parse_maybe(
        maps:get(<<"flags">>, Media, 0)
    ),
    Nsfw = maps:get(<<"nsfw">>, Media, false),
    case is_safe_media_url(Url, ContentType, Flags, Nsfw) of
        true when is_binary(Url) -> {ok, Url};
        _ -> unsafe
    end.

-spec is_safe_media_url(
    binary() | undefined,
    binary() | undefined,
    integer() | undefined,
    term()
) ->
    boolean().
is_safe_media_url(undefined, _ContentType, _Flags, _Nsfw) ->
    false;
is_safe_media_url(_Url, undefined, _Flags, _Nsfw) ->
    false;
is_safe_media_url(_Url, _ContentType, undefined, _Nsfw) ->
    false;
is_safe_media_url(Url, ContentType, Flags, Nsfw) when is_binary(Url), is_binary(ContentType) ->
    IsImage = binary:match(lowercase_binary(ContentType), <<"image/">>) =/= nomatch,
    IsSensitive = is_sensitive_media(Nsfw, Flags),
    IsHttps = binary:match(lowercase_binary(Url), <<"https://">>) =:= {0, 8},
    IsImage andalso not IsSensitive andalso IsHttps.

-spec is_sensitive_media(term(), integer()) -> boolean().
is_sensitive_media(true, _Flags) ->
    true;
is_sensitive_media(_Nsfw, Flags) ->
    bitset:any(Flags, 16#18).

-spec maybe_image_fields(binary() | undefined) -> map().
maybe_image_fields(undefined) ->
    #{};
maybe_image_fields(ImageUrl) when is_binary(ImageUrl) ->
    #{<<"image_url">> => ImageUrl, <<"image">> => ImageUrl}.

-spec build_url(integer(), integer(), integer()) -> binary().
build_url(0, ChannelId, MessageId) ->
    build_url_parts([<<"@me">>, integer_to_binary(ChannelId), integer_to_binary(MessageId)]);
build_url(GuildId, ChannelId, MessageId) ->
    build_url_parts([
        integer_to_binary(GuildId),
        integer_to_binary(ChannelId),
        integer_to_binary(MessageId)
    ]).

-spec build_url_parts(nonempty_list(binary())) -> binary().
build_url_parts(Parts) ->
    iolist_to_binary([
        <<"/channels/">>,
        join_binary(Parts, <<"/">>)
    ]).

-spec lowercase_binary(binary()) -> binary().
lowercase_binary(Value) ->
    iolist_to_binary(string:lowercase(Value)).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_url_dm_test() ->
    ?assertEqual(<<"/channels/@me/456/789">>, build_url(0, 456, 789)).

build_url_guild_test() ->
    ?assertEqual(<<"/channels/123/456/789">>, build_url(123, 456, 789)).

extract_image_url_rejects_malformed_flags_test() ->
    MessageData = #{
        <<"attachments">> => [
            #{
                <<"content_type">> => <<"image/png">>,
                <<"proxy_url">> => <<"https://cdn.example/image.png">>,
                <<"flags">> => <<"not-an-int">>
            }
        ]
    },
    ?assertEqual(undefined, extract_image_url(MessageData)).

-endif.
