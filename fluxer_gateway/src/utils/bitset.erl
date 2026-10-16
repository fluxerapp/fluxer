%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(bitset).
-typing([eqwalizer]).

-export([
    parse/1,
    parse_optional/1,
    parse_maybe/1,
    has/2,
    any/2,
    add/2,
    remove/2,
    apply_allow_deny/3
]).

-export_type([t/0, bit/0]).

-type t() :: non_neg_integer().
-type bit() :: pos_integer().

-spec parse(term()) -> t().
parse(Value) when is_integer(Value), Value >= 0 ->
    Value;
parse(Value) when is_binary(Value) ->
    require_parsed(parse_binary(Value), Value);
parse(Value) when is_list(Value) ->
    require_parsed(parse_list_value(Value), Value);
parse(Value) ->
    erlang:error({invalid_bitset, Value}).

-spec parse_optional(term()) -> t() | undefined.
parse_optional(undefined) ->
    undefined;
parse_optional(null) ->
    undefined;
parse_optional(Value) ->
    parse(Value).

-spec parse_maybe(term()) -> t() | undefined.
parse_maybe(Value) ->
    try parse_optional(Value) of
        Bits -> Bits
    catch
        error:{invalid_bitset, _} -> undefined
    end.

-spec has(t(), bit()) -> boolean().
has(Bits, Bit) when is_integer(Bits), Bits >= 0, is_integer(Bit), Bit > 0 ->
    (Bits band Bit) =:= Bit.

-spec any(t(), t()) -> boolean().
any(Bits, Mask) when is_integer(Bits), Bits >= 0, is_integer(Mask), Mask >= 0 ->
    (Bits band Mask) =/= 0.

-spec add(t(), t()) -> t().
add(Bits, Mask) when is_integer(Bits), Bits >= 0, is_integer(Mask), Mask >= 0 ->
    Bits bor Mask.

-spec remove(t(), t()) -> t().
remove(Bits, Mask) when is_integer(Bits), Bits >= 0, is_integer(Mask), Mask >= 0 ->
    Bits band bnot Mask.

-spec apply_allow_deny(t(), t(), t()) -> t().
apply_allow_deny(Bits, Allow, Deny) ->
    add(remove(Bits, Deny), Allow).

-spec parse_binary(binary()) -> t() | undefined.
parse_binary(<<>>) ->
    undefined;
parse_binary(Value) ->
    parse_digits_binary(Value, 0).

-spec parse_digits_binary(binary(), non_neg_integer()) -> t() | undefined.
parse_digits_binary(<<>>, Acc) ->
    Acc;
parse_digits_binary(<<Digit, Rest/binary>>, Acc) when Digit >= $0, Digit =< $9 ->
    parse_digits_binary(Rest, Acc * 10 + Digit - $0);
parse_digits_binary(_, _Acc) ->
    undefined.

-spec parse_list_value([term()]) -> t() | undefined.
parse_list_value([]) ->
    undefined;
parse_list_value(Value) ->
    parse_digits_list(Value, 0).

-spec parse_digits_list([term()], non_neg_integer()) -> t() | undefined.
parse_digits_list([], Acc) ->
    Acc;
parse_digits_list([Digit | Rest], Acc) when is_integer(Digit), Digit >= $0, Digit =< $9 ->
    parse_digits_list(Rest, Acc * 10 + Digit - $0);
parse_digits_list(_, _Acc) ->
    undefined.

-spec require_parsed(t() | undefined, term()) -> t().
require_parsed(Bits, _Value) when is_integer(Bits) ->
    Bits;
require_parsed(undefined, Value) ->
    erlang:error({invalid_bitset, Value}).
