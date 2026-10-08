%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(type_conv).
-typing([eqwalizer]).

-export([
    to_integer/1,
    to_binary/1,
    ensure_binary/1,
    ensure_binary/2,
    unicode_to_binary/1,
    extract_id/2
]).

-export_type([
    convertible_to_integer/0,
    convertible_to_binary/0,
    convertible_to_list/0
]).

-type convertible_to_integer() :: integer() | binary() | list() | atom().
-type convertible_to_binary() :: binary() | integer() | list() | atom().
-type convertible_to_list() :: list() | binary() | atom().

-spec to_integer(term()) -> integer() | undefined.
to_integer(undefined) ->
    undefined;
to_integer(Value) when is_integer(Value) ->
    Value;
to_integer(Value) when is_binary(Value) ->
    try
        binary_to_integer(Value)
    catch
        error:badarg ->
            undefined
    end;
to_integer(Value) when is_list(Value) ->
    case byte_list_to_binary(Value) of
        undefined -> undefined;
        Bin -> to_integer(Bin)
    end;
to_integer(Value) when is_atom(Value) ->
    try
        list_to_integer(atom_to_list(Value))
    catch
        error:badarg ->
            undefined
    end;
to_integer(_) ->
    undefined.

-spec ensure_binary(term()) -> binary().
ensure_binary(Value) ->
    ensure_binary(Value, <<>>).

-spec ensure_binary(term(), binary()) -> binary().
ensure_binary(Value, _Default) when is_binary(Value) ->
    Value;
ensure_binary(Value, Default) when is_list(Value) ->
    fold_charlist_to_binary(Value, <<>>, Default);
ensure_binary(_, Default) ->
    Default.

-spec unicode_to_binary(term()) -> binary() | undefined.
unicode_to_binary(Value) when is_binary(Value) ->
    Value;
unicode_to_binary(Value) when is_list(Value) ->
    case fold_charlist_to_binary(Value, <<>>, undefined) of
        undefined -> undefined;
        Bin -> Bin
    end;
unicode_to_binary(_) ->
    undefined.

-spec to_binary(term()) -> binary() | undefined.
to_binary(undefined) ->
    undefined;
to_binary(Value) when is_binary(Value) ->
    Value;
to_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
to_binary(Value) when is_list(Value) ->
    byte_list_to_binary(Value);
to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_binary(_) ->
    undefined.

-spec extract_id(term(), term()) -> pos_integer() | undefined.
extract_id(Map, Field) when is_map(Map), is_atom(Field) ->
    extract_id_value(maps:get(Field, Map, undefined));
extract_id(Map, Field) when is_map(Map), is_binary(Field) ->
    extract_id_value(maps:get(Field, Map, undefined));
extract_id(_, _) ->
    undefined.

-spec extract_id_value(term()) -> pos_integer() | undefined.
extract_id_value(undefined) ->
    undefined;
extract_id_value(Value) ->
    snowflake_id:parse_maybe(Value).

-spec byte_list_to_binary(list()) -> binary() | undefined.
byte_list_to_binary(Value) ->
    case collect_bytes(Value, []) of
        {ok, Bytes} -> bytes_to_binary(lists:reverse(Bytes), <<>>);
        error -> undefined
    end.

-spec collect_bytes(list(), [byte()]) -> {ok, [byte()]} | error.
collect_bytes([], Acc) ->
    {ok, Acc};
collect_bytes([Byte | Rest], Acc) when is_integer(Byte), Byte >= 0, Byte =< 255 ->
    collect_bytes(Rest, [Byte | Acc]);
collect_bytes([Bin | Rest], Acc) when is_binary(Bin) ->
    collect_bytes(Rest, append_binary_bytes(Bin, Acc));
collect_bytes([Nested | Rest], Acc) when is_list(Nested) ->
    case collect_bytes(Nested, Acc) of
        {ok, NestedAcc} -> collect_bytes(Rest, NestedAcc);
        error -> error
    end;
collect_bytes([_ | _], _Acc) ->
    error.

-spec append_binary_bytes(binary(), [byte()]) -> [byte()].
append_binary_bytes(Bin, Acc) ->
    append_binary_bytes(Bin, 0, byte_size(Bin), Acc).

-spec append_binary_bytes(binary(), non_neg_integer(), non_neg_integer(), [byte()]) -> [byte()].
append_binary_bytes(_Bin, Index, Size, Acc) when Index >= Size ->
    Acc;
append_binary_bytes(Bin, Index, Size, Acc) ->
    append_binary_bytes(Bin, Index + 1, Size, [binary:at(Bin, Index) | Acc]).

-spec bytes_to_binary([byte()], binary()) -> binary().
bytes_to_binary([], Acc) ->
    Acc;
bytes_to_binary([Byte | Rest], Acc) ->
    bytes_to_binary(Rest, <<Acc/binary, Byte>>).

-spec fold_charlist_to_binary([term()], binary(), T) -> binary() | T.
fold_charlist_to_binary([], Acc, _Fallback) ->
    Acc;
fold_charlist_to_binary([H | T], Acc, Fallback) when is_integer(H), H >= 0, H =< 255 ->
    fold_charlist_to_binary(T, <<Acc/binary, H>>, Fallback);
fold_charlist_to_binary([H | T], Acc, Fallback) when is_binary(H) ->
    fold_charlist_to_binary(T, <<Acc/binary, H/binary>>, Fallback);
fold_charlist_to_binary(_, _Acc, Fallback) ->
    Fallback.
