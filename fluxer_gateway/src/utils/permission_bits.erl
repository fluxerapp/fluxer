%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(permission_bits).
-typing([eqwalizer]).

-export([
    parse/1,
    parse_optional/1,
    parse_maybe/1,
    has/2,
    add/2,
    apply_allow_deny/3
]).

-export_type([t/0, bit/0]).

-type t() :: bitset:t().
-type bit() :: bitset:bit().

-spec parse(term()) -> t().
parse(Value) ->
    bitset:parse(Value).

-spec parse_optional(term()) -> t() | undefined.
parse_optional(Value) ->
    bitset:parse_optional(Value).

-spec parse_maybe(term()) -> t() | undefined.
parse_maybe(Value) ->
    bitset:parse_maybe(Value).

-spec has(t(), bit()) -> boolean().
has(Bits, Bit) ->
    bitset:has(Bits, Bit).

-spec add(t(), t()) -> t().
add(Bits, Mask) ->
    bitset:add(Bits, Mask).

-spec apply_allow_deny(t(), t(), t()) -> t().
apply_allow_deny(Bits, Allow, Deny) ->
    bitset:apply_allow_deny(Bits, Allow, Deny).
