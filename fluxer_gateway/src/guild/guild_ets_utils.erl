%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_ets_utils).
-typing([eqwalizer]).

-export([ensure_table/2, heir_options/0]).

-spec ensure_table(atom(), list()) -> ok.
ensure_table(TableName, Options) ->
    case whereis(guild_ets_owner) of
        Pid when is_pid(Pid), Pid =/= self() ->
            guild_ets_owner:ensure_table(TableName, Options);
        _ ->
            ensure_table_local(TableName, Options)
    end.

-spec heir_options() -> list().
heir_options() ->
    case whereis(guild_ets_owner) of
        Pid when is_pid(Pid), Pid =/= self() -> [{heir, Pid, inherited}];
        _ -> []
    end.

-spec ensure_table_local(atom(), list()) -> ok.
ensure_table_local(TableName, Options) ->
    case ets:whereis(TableName) of
        undefined -> try_create_table(TableName, Options);
        _ -> ok
    end.

-spec try_create_table(atom(), list()) -> ok.
try_create_table(TableName, Options) ->
    try ets:new(TableName, Options) of
        _ -> ok
    catch
        error:badarg -> ok
    end.
