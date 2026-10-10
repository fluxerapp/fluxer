%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(user_flags).
-typing([eqwalizer]).

-export([
    staff/0,
    parse/1,
    has/2,
    is_staff/1
]).

-export_type([t/0, flag/0]).

-type t() :: bitset:t().
-type flag() :: bitset:bit().

-define(STAFF, 16#1).

-spec staff() -> flag().
staff() ->
    ?STAFF.

-spec parse(term()) -> t().
parse(Value) ->
    bitset:parse(Value).

-spec has(t(), flag()) -> boolean().
has(Flags, Flag) ->
    bitset:has(Flags, Flag).

-spec is_staff(term()) -> boolean().
is_staff(Value) ->
    has(parse(Value), staff()).
