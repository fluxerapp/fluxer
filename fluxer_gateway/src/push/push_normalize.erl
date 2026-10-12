%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_normalize).
-typing([eqwalizer]).

-export([notification_level/1]).

-spec notification_level(term()) -> integer() | undefined.
notification_level(undefined) ->
    0;
notification_level(null) ->
    0;
notification_level(Value) when is_integer(Value), Value >= -1, Value =< 3 ->
    Value;
notification_level(Value) ->
    case guild_data_normalize_schema:int(Value) of
        Level when is_integer(Level), Level =< 3 -> Level;
        _ -> undefined
    end.
