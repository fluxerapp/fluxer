%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(user_flags).
-typing([eqwalizer]).

-export([staff/0, is_staff/1]).

-export_type([t/0, flag/0]).

-type t() :: bitset:t().
-type flag() :: bitset:bit().

-define(STAFF, 16#1).

-spec staff() -> flag().
staff() ->
    ?STAFF.

-spec is_staff(term()) -> boolean().
is_staff(Value) ->
    bitset:has(bitset:parse(Value), staff()).
