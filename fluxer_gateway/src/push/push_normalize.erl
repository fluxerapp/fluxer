%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_normalize).
-typing([eqwalizer]).

-export([
    optional_guild_id/1,
    notification_level/1,
    integer_list/1
]).

-spec optional_guild_id(term()) -> pos_integer() | undefined.
optional_guild_id(Value) -> snowflake_id:parse_maybe(Value).

-spec notification_level(term()) -> integer() | undefined.
notification_level(undefined) ->
    0;
notification_level(null) ->
    0;
notification_level(Value) ->
    case guild_data_normalize_schema:int(Value) of
        Level when Level >= -1, Level =< 3 -> Level;
        _ -> undefined
    end.

-spec integer_list(term()) -> {ok, [integer()]} | error.
integer_list(Value) when is_list(Value) ->
    integer_list(Value, []);
integer_list(_) ->
    error.

-spec integer_list([term()], [integer()]) -> {ok, [integer()]} | error.
integer_list([], Acc) ->
    {ok, lists:reverse(Acc)};
integer_list([Value | Rest], Acc) when is_integer(Value) ->
    integer_list(Rest, [Value | Acc]);
integer_list(_, _) ->
    error.
