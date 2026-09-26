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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

safe_delete_table(TableName) ->
    try ets:delete(TableName) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

test_table_opts() -> [named_table, public, set].

ensure_table_creates_new_table_test() ->
    TableName = guild_ets_utils_test_table,
    safe_delete_table(TableName),
    ok = ensure_table(TableName, test_table_opts()),
    ?assertNotEqual(undefined, ets:whereis(TableName)),
    ets:delete(TableName).

ensure_table_idempotent_test() ->
    TableName = guild_ets_utils_test_idempotent,
    safe_delete_table(TableName),
    ok = ensure_table(TableName, test_table_opts()),
    ok = ensure_table(TableName, test_table_opts()),
    ?assertNotEqual(undefined, ets:whereis(TableName)),
    ets:delete(TableName).

ensure_table_delegates_to_owner_when_running_test() ->
    TableName = guild_ets_utils_test_owner,
    safe_delete_table(TableName),
    {ok, Pid} = guild_ets_owner:start_link(),
    try
        ok = ensure_table(TableName, test_table_opts()),
        ?assertEqual(Pid, ets:info(TableName, owner))
    after
        gen_server:stop(guild_ets_owner),
        safe_delete_table(TableName)
    end.

-endif.
