%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_image_url).
-typing([eqwalizer]).

-export([extract/1]).

-define(MAX_IMAGE_URL_BYTES, 1024).

-type own_link() :: {binary(), binary()} | none.

-spec extract(map()) -> binary() | undefined.
extract(MessageData) ->
    bounded_image_url(resolve_image_url(MessageData)).

-spec resolve_image_url(map()) -> binary() | undefined.
resolve_image_url(MessageData) ->
    Own = own_link(MessageData),
    case extract_attachment_image_url(maps:get(<<"attachments">>, MessageData, []), Own) of
        undefined -> extract_embed_image_url(maps:get(<<"embeds">>, MessageData, []), Own);
        ImageUrl -> ImageUrl
    end.

-spec own_link(map()) -> own_link().
own_link(MessageData) ->
    ChannelId = maps:get(<<"channel_id">>, MessageData, undefined),
    MessageId = maps:get(<<"id">>, MessageData, undefined),
    case is_snowflake(ChannelId) andalso is_snowflake(MessageId) of
        true -> {ChannelId, MessageId};
        false -> none
    end.

-spec is_snowflake(term()) -> boolean().
is_snowflake(Value) when is_binary(Value), byte_size(Value) > 0, byte_size(Value) =< 20 ->
    lists:all(fun(Char) -> Char >= $0 andalso Char =< $9 end, binary_to_list(Value));
is_snowflake(_Value) ->
    false.

-spec bounded_image_url(binary() | undefined) -> binary() | undefined.
bounded_image_url(ImageUrl) when
    is_binary(ImageUrl), byte_size(ImageUrl) =< ?MAX_IMAGE_URL_BYTES
->
    ImageUrl;
bounded_image_url(_ImageUrl) ->
    undefined.

-spec extract_attachment_image_url(term(), own_link()) -> binary() | undefined.
extract_attachment_image_url([Attachment | Rest], Own) when is_map(Attachment) ->
    case check_media_safe_url(Attachment, Own) of
        {ok, Url} -> Url;
        unsafe -> extract_attachment_image_url(Rest, Own)
    end;
extract_attachment_image_url([_ | Rest], Own) ->
    extract_attachment_image_url(Rest, Own);
extract_attachment_image_url(_, _Own) ->
    undefined.

-spec extract_embed_image_url(term(), own_link()) -> binary() | undefined.
extract_embed_image_url([Embed | Rest], Own) when is_map(Embed) ->
    case first_embed_media_url(Embed, Own) of
        undefined -> extract_embed_image_url(Rest, Own);
        Url -> Url
    end;
extract_embed_image_url([_ | Rest], Own) ->
    extract_embed_image_url(Rest, Own);
extract_embed_image_url(_, _Own) ->
    undefined.

-spec first_embed_media_url(map(), own_link()) -> binary() | undefined.
first_embed_media_url(Embed, Own) ->
    Candidates = [
        maps:get(<<"image">>, Embed, undefined),
        maps:get(<<"thumbnail">>, Embed, undefined)
    ],
    first_embed_media_url_from_candidates(Candidates, Own).

-spec first_embed_media_url_from_candidates(list(), own_link()) -> binary() | undefined.
first_embed_media_url_from_candidates([Media | Rest], Own) when is_map(Media) ->
    case check_media_safe_url(Media, Own) of
        {ok, Url} -> Url;
        unsafe -> first_embed_media_url_from_candidates(Rest, Own)
    end;
first_embed_media_url_from_candidates([_ | Rest], Own) ->
    first_embed_media_url_from_candidates(Rest, Own);
first_embed_media_url_from_candidates([], _Own) ->
    undefined.

-spec check_media_safe_url(map(), own_link()) -> {ok, binary()} | unsafe.
check_media_safe_url(Media, Own) ->
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
    case
        is_safe_media_url(Url, ContentType, Flags, Nsfw) andalso not is_own_api_link(Url, Own)
    of
        true when is_binary(Url) -> {ok, Url};
        _ -> unsafe
    end.

-spec is_own_api_link(binary() | undefined, own_link()) -> boolean().
is_own_api_link(Url, {ChannelId, MessageId}) when is_binary(Url) ->
    Pattern = <<
        "^https?://[^/?#]+(?:/[^?#]*)?/channels/",
        ChannelId/binary,
        "/attachments/[0-9]+/[^?#]+\\?(?:[^#]*&)?mid=",
        MessageId/binary,
        "(?:[&#]|$)"
    >>,
    re:run(Url, Pattern, [caseless, {capture, none}]) =:= match;
is_own_api_link(_Url, _Own) ->
    false.

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

-spec lowercase_binary(binary()) -> binary().
lowercase_binary(Value) ->
    iolist_to_binary(string:lowercase(Value)).
