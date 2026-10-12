%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_connection_normalize).

-typing([eqwalizer]).

-export([normalize_connection_id/1]).
-export([normalize_user_id/1]).
-export([normalize_channel_id_value/1]).
-export([normalize_boolean/1]).
-export([normalize_coordinate/1]).
-export([normalize_session_id/1]).
-export([normalize_optional_binary/1]).
-export([normalize_guild_id/1]).

-spec normalize_connection_id(term()) -> binary() | undefined.
normalize_connection_id(undefined) -> undefined;
normalize_connection_id(null) -> undefined;
normalize_connection_id(ConnectionId) -> normalize_optional_binary(ConnectionId).

-spec normalize_user_id(term()) -> integer() | undefined.
normalize_user_id(Value) ->
    snowflake_id:parse_maybe(Value).

-spec normalize_channel_id_value(term()) -> integer() | null | undefined.
normalize_channel_id_value(null) -> null;
normalize_channel_id_value(Value) -> snowflake_id:parse_maybe(Value).

-spec normalize_boolean(term()) -> boolean().
normalize_boolean(true) -> true;
normalize_boolean(<<"true">>) -> true;
normalize_boolean(false) -> false;
normalize_boolean(<<"false">>) -> false;
normalize_boolean(_) -> false.

-spec normalize_coordinate(term()) -> binary() | undefined.
normalize_coordinate(undefined) -> undefined;
normalize_coordinate(null) -> undefined;
normalize_coordinate(Value) when is_binary(Value) -> Value;
normalize_coordinate(Value) when is_integer(Value) -> integer_to_binary(Value);
normalize_coordinate(Value) when is_float(Value) -> float_to_binary(Value, [short]);
normalize_coordinate(Value) when is_list(Value) -> safe_iolist_to_binary(Value);
normalize_coordinate(_Value) -> undefined.

-spec normalize_session_id(term()) -> binary() | undefined.
normalize_session_id(undefined) -> undefined;
normalize_session_id(null) -> undefined;
normalize_session_id(SessionId) when is_binary(SessionId) -> SessionId;
normalize_session_id(SessionId) when is_integer(SessionId) -> integer_to_binary(SessionId);
normalize_session_id(SessionId) when is_list(SessionId) -> safe_iolist_to_binary(SessionId);
normalize_session_id(_) -> undefined.

-spec normalize_optional_binary(term()) -> binary() | undefined.
normalize_optional_binary(undefined) -> undefined;
normalize_optional_binary(null) -> undefined;
normalize_optional_binary(Value) when is_binary(Value), byte_size(Value) > 0 -> Value;
normalize_optional_binary(Value) when is_binary(Value) -> undefined;
normalize_optional_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
normalize_optional_binary(Value) when is_list(Value) -> safe_iolist_to_binary(Value);
normalize_optional_binary(_Value) -> undefined.

-spec normalize_guild_id(term()) -> {ok, integer(), binary()} | {error, atom()}.
normalize_guild_id(Value) ->
    case snowflake_id:parse_maybe(Value) of
        undefined -> {error, voice_invalid_guild_id};
        Int -> {ok, Int, guild_id_binary(Value, Int)}
    end.

-spec guild_id_binary(term(), integer()) -> binary().
guild_id_binary(Value, Int) ->
    case type_conv:to_binary(Value) of
        undefined -> integer_to_binary(Int);
        Bin -> Bin
    end.

-spec safe_iolist_to_binary(term()) -> binary() | undefined.
safe_iolist_to_binary(Value) ->
    case safe_iolist_to_binary_parts(Value) of
        {ok, Binary} -> Binary;
        error -> undefined
    end.

-spec safe_iolist_to_binary_parts(term()) -> {ok, binary()} | error.
safe_iolist_to_binary_parts(Value) when is_binary(Value) ->
    {ok, Value};
safe_iolist_to_binary_parts(Value) when is_integer(Value), Value >= 0, Value =< 255 ->
    {ok, <<Value>>};
safe_iolist_to_binary_parts(Value) when is_list(Value) ->
    safe_iolist_list_to_binary(Value);
safe_iolist_to_binary_parts(_) ->
    error.

-spec safe_iolist_list_to_binary(term()) -> {ok, binary()} | error.
safe_iolist_list_to_binary([]) ->
    {ok, <<>>};
safe_iolist_list_to_binary([Head | Tail]) ->
    case {safe_iolist_to_binary_parts(Head), safe_iolist_tail_to_binary(Tail)} of
        {{ok, HeadBinary}, {ok, TailBinary}} -> {ok, <<HeadBinary/binary, TailBinary/binary>>};
        _ -> error
    end;
safe_iolist_list_to_binary(_) ->
    error.

-spec safe_iolist_tail_to_binary(term()) -> {ok, binary()} | error.
safe_iolist_tail_to_binary(Value) when is_binary(Value) ->
    {ok, Value};
safe_iolist_tail_to_binary(Value) when is_list(Value) ->
    safe_iolist_list_to_binary(Value);
safe_iolist_tail_to_binary(_) ->
    error.
