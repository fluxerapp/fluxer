%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_markdown_plaintext).
-typing([eqwalizer]).

-export([
    available/0,
    render_push_preview/2
]).

-define(MAX_CONTENT_BYTES, 16 * 1024).
-define(MAX_CONTEXT_JSON_BYTES, 64 * 1024).
-define(MAX_CONTEXT_NAME_ENTRIES, 512).

-spec available() -> boolean().
available() ->
    try push_markdown_plaintext_nif:available() of
        true -> true;
        _ -> false
    catch
        _:_ -> false
    end.

-spec render_push_preview(binary(), map()) -> binary().
render_push_preview(Content, Context) when
    is_binary(Content), is_map(Context), byte_size(Content) =< ?MAX_CONTENT_BYTES
->
    ContextJson = encode_context(default_context(Context)),
    render_bounded_context(Content, ContextJson);
render_push_preview(Content, _Context) when is_binary(Content) ->
    strip_to_safe(Content).

-spec render_bounded_context(binary(), binary()) -> binary().
render_bounded_context(Content, ContextJson) when
    byte_size(ContextJson) =< ?MAX_CONTEXT_JSON_BYTES
->
    try push_markdown_plaintext_nif:render_push_preview_nif(Content, ContextJson) of
        Preview when is_binary(Preview) -> Preview
    catch
        _:_ -> strip_to_safe(Content)
    end;
render_bounded_context(Content, _ContextJson) ->
    strip_to_safe(Content).

-spec strip_to_safe(binary()) -> binary().
strip_to_safe(Content) when is_binary(Content) ->
    case unicode:characters_to_list(Content) of
        Codepoints when is_list(Codepoints) ->
            safe_codepoints_to_binary(Codepoints, Content);
        _ ->
            Content
    end.

-spec safe_codepoints_to_binary([integer()], binary()) -> binary().
safe_codepoints_to_binary(Codepoints, Fallback) ->
    Filtered = [C || C <- Codepoints, is_safe_codepoint(C)],
    case unicode:characters_to_binary(Filtered) of
        Result when is_binary(Result) -> Result;
        _ -> Fallback
    end.

-spec is_safe_codepoint(integer()) -> boolean().
is_safe_codepoint(C) when C =:= $\n; C =:= $\t -> true;
is_safe_codepoint(C) when C >= 0, C =< 16#1F -> false;
is_safe_codepoint(C) when C >= 16#7F, C =< 16#9F -> false;
is_safe_codepoint(C) when C >= 16#200E, C =< 16#200F -> false;
is_safe_codepoint(C) when C >= 16#202A, C =< 16#202E -> false;
is_safe_codepoint(C) when C >= 16#2066, C =< 16#2069 -> false;
is_safe_codepoint(_) -> true.

-spec default_context(map()) -> map().
default_context(Context) ->
    #{
        <<"preserve_markdown">> => maps:get(<<"preserve_markdown">>, Context, true),
        <<"include_emoji_names">> => maps:get(<<"include_emoji_names">>, Context, true),
        <<"include_link_urls">> => maps:get(<<"include_link_urls">>, Context, false),
        <<"users">> => bounded_name_map(maps:get(<<"users">>, Context, #{})),
        <<"roles">> => bounded_name_map(maps:get(<<"roles">>, Context, #{})),
        <<"channels">> => bounded_name_map(maps:get(<<"channels">>, Context, #{}))
    }.

-spec bounded_name_map(term()) -> map().
bounded_name_map(Map) when is_map(Map), map_size(Map) =< ?MAX_CONTEXT_NAME_ENTRIES ->
    Map;
bounded_name_map(_Map) ->
    #{}.

-spec encode_context(map()) -> binary().
encode_context(Context) ->
    iolist_to_binary(json:encode(Context)).
