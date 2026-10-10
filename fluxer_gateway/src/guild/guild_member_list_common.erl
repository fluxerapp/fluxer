%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_common).
-typing([eqwalizer]).

-export([
    get_member_user_id/1,
    get_member_display_name/1,
    normalize_name/1,
    casefold_binary/1,
    default_presence/0,
    connected_session_user_ids/1,
    unicode_chardata_to_binary/1
]).

-spec get_member_user_id(map()) -> integer() | undefined.
get_member_user_id(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    case snowflake_id:parse_maybe(maps:get(<<"id">>, User, undefined)) of
        UserId when is_integer(UserId), UserId > 0 -> UserId;
        _ -> undefined
    end.

-spec normalize_name(term()) -> binary().
normalize_name(undefined) ->
    <<>>;
normalize_name(null) ->
    <<>>;
normalize_name(<<_/binary>> = B) ->
    B;
normalize_name(L) when is_list(L) ->
    case unicode_list_to_binary(L) of
        {ok, Bin} -> Bin;
        error -> <<>>
    end;
normalize_name(I) when is_integer(I) -> integer_to_binary(I);
normalize_name(_) ->
    <<>>.

-spec get_member_display_name(map()) -> binary().
get_member_display_name(Member) ->
    Nick = normalize_name(maps:get(<<"nick">>, Member, undefined)),
    case Nick =:= <<>> of
        false -> Nick;
        true -> display_name_fallback(Member)
    end.

-spec display_name_fallback(map()) -> binary().
display_name_fallback(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    GlobalName = normalize_name(maps:get(<<"global_name">>, User, undefined)),
    case GlobalName =:= <<>> of
        false -> GlobalName;
        true -> normalize_name(maps:get(<<"username">>, User, undefined))
    end.

-spec casefold_binary(term()) -> binary().
casefold_binary(Value) ->
    Bin = normalize_name(Value),
    case ascii_casefold(Bin) of
        {ok, Folded} ->
            Folded;
        not_ascii ->
            case unicode_chardata_to_binary(string:casefold(Bin)) of
                {ok, Folded} -> Folded;
                error -> Bin
            end
    end.

%% Case folding a pure-ASCII binary is exactly A-Z -> a-z, so a byte pass is
%% equivalent to string:casefold/1 and skips two full unicode traversals per
%% member. Anything with a byte above 127 falls through to the unicode path.
-spec ascii_casefold(binary()) -> {ok, binary()} | not_ascii.
ascii_casefold(Bin) ->
    case scan_ascii(Bin, false) of
        not_ascii -> not_ascii;
        false -> {ok, Bin};
        true -> {ok, lower_ascii(Bin, <<>>)}
    end.

-spec scan_ascii(binary(), boolean()) -> boolean() | not_ascii.
scan_ascii(<<>>, HasUpper) ->
    HasUpper;
scan_ascii(<<C, Rest/binary>>, _HasUpper) when C >= $A, C =< $Z ->
    scan_ascii(Rest, true);
scan_ascii(<<C, Rest/binary>>, HasUpper) when C < 128 ->
    scan_ascii(Rest, HasUpper);
scan_ascii(_Bin, _HasUpper) ->
    not_ascii.

-spec lower_ascii(binary(), binary()) -> binary().
lower_ascii(<<>>, Acc) ->
    Acc;
lower_ascii(<<C, Rest/binary>>, Acc) when C >= $A, C =< $Z ->
    lower_ascii(Rest, <<Acc/binary, (C + 32)>>);
lower_ascii(<<C, Rest/binary>>, Acc) ->
    lower_ascii(Rest, <<Acc/binary, C>>).

-spec unicode_list_to_binary([term()]) -> {ok, binary()} | error.
unicode_list_to_binary(List) ->
    case unicode_charlist(List) of
        {ok, Chars} -> unicode_chardata_to_binary(Chars);
        error -> error
    end.

-spec unicode_charlist([term()]) -> {ok, unicode:charlist()} | error.
unicode_charlist(List) ->
    unicode_charlist(List, []).

-spec unicode_charlist([term()], unicode:charlist()) -> {ok, unicode:charlist()} | error.
unicode_charlist([], Acc) ->
    {ok, lists:reverse(Acc)};
unicode_charlist([Char | Rest], Acc) when
    is_integer(Char), Char >= 0, Char =< 16#10FFFF
->
    unicode_charlist(Rest, [Char | Acc]);
unicode_charlist([Bin | Rest], Acc) when is_binary(Bin) ->
    unicode_charlist(Rest, [Bin | Acc]);
unicode_charlist([Nested | Rest], Acc) when is_list(Nested) ->
    case unicode_charlist(Nested) of
        {ok, NestedChars} -> unicode_charlist(Rest, [NestedChars | Acc]);
        error -> error
    end;
unicode_charlist(_, _) ->
    error.

-spec unicode_chardata_to_binary(unicode:chardata()) -> {ok, binary()} | error.
unicode_chardata_to_binary(Data) ->
    try unicode:characters_to_binary(Data) of
        Bin when is_binary(Bin) -> {ok, Bin};
        _ -> error
    catch
        throw:_Reason -> error;
        error:_Reason -> error;
        exit:_Reason -> error
    end.

-spec default_presence() -> map().
default_presence() -> guild_member_list_connected:default_presence().

-spec connected_session_user_ids(map()) -> sets:set(integer()).
connected_session_user_ids(S) -> guild_member_list_connected:connected_session_user_ids(S).
