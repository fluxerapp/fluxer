%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_clock).
-typing([eqwalizer]).

-export([monotonic_ms/0]).

-spec monotonic_ms() -> integer().
monotonic_ms() ->
    erlang:monotonic_time(millisecond).
