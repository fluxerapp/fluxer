%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_image_url).
-typing([eqwalizer]).

-export([extract/1]).

-define(MAX_IMAGE_URL_BYTES, 1024).

-spec extract(map()) -> binary() | undefined.
extract(MessageData) ->
    bounded_image_url(resolve_image_url(MessageData)).

-spec resolve_image_url(map()) -> binary() | undefined.
resolve_image_url(MessageData) ->
    case extract_attachment_image_url(maps:get(<<"attachments">>, MessageData, [])) of
        undefined -> extract_embed_image_url(maps:get(<<"embeds">>, MessageData, []));
        ImageUrl -> ImageUrl
    end.

-spec bounded_image_url(binary() | undefined) -> binary() | undefined.
bounded_image_url(ImageUrl) when
    is_binary(ImageUrl), byte_size(ImageUrl) =< ?MAX_IMAGE_URL_BYTES
->
    ImageUrl;
bounded_image_url(_ImageUrl) ->
    undefined.

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

-spec lowercase_binary(binary()) -> binary().
lowercase_binary(Value) ->
    iolist_to_binary(string:lowercase(Value)).
