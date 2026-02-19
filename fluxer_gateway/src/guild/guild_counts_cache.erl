%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(guild_counts_cache).

-export([
    init/0,
    update/3,
    get/1,
    delete/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(TABLE, guild_counts_cache).

-type guild_id() :: integer().

-spec init() -> ok.
init() ->
    case ets:whereis(?TABLE) of
        undefined ->
            _ = ets:new(?TABLE, [
                named_table,
                public,
                set,
                {read_concurrency, true},
                {write_concurrency, true}
            ]),
            ok;
        _ ->
            ok
    end.

-spec update(guild_id(), non_neg_integer(), non_neg_integer()) -> ok.
update(GuildId, MemberCount, OnlineCount) ->
    ensure_table(),
    ets:insert(?TABLE, {GuildId, MemberCount, OnlineCount}),
    ok.

-spec get(guild_id()) -> {ok, non_neg_integer(), non_neg_integer()} | miss.
get(GuildId) ->
    case catch ets:lookup(?TABLE, GuildId) of
        [{GuildId, MemberCount, OnlineCount}] ->
            {ok, MemberCount, OnlineCount};
        _ ->
            miss
    end.

-spec delete(guild_id()) -> ok.
delete(GuildId) ->
    catch ets:delete(?TABLE, GuildId),
    ok.

-spec ensure_table() -> ok.
ensure_table() ->
    case ets:whereis(?TABLE) of
        undefined -> init();
        _ -> ok
    end.

-ifdef(TEST).

init_creates_table_test() ->
    catch ets:delete(?TABLE),
    ok = init(),
    ?assertNotEqual(undefined, ets:whereis(?TABLE)),
    catch ets:delete(?TABLE).

init_idempotent_test() ->
    catch ets:delete(?TABLE),
    ok = init(),
    ok = init(),
    ?assertNotEqual(undefined, ets:whereis(?TABLE)),
    catch ets:delete(?TABLE).

update_and_get_test() ->
    catch ets:delete(?TABLE),
    ok = init(),
    ok = update(100, 50, 25),
    ?assertEqual({ok, 50, 25}, guild_counts_cache:get(100)),
    catch ets:delete(?TABLE).

get_miss_test() ->
    catch ets:delete(?TABLE),
    ok = init(),
    ?assertEqual(miss, guild_counts_cache:get(999)),
    catch ets:delete(?TABLE).

update_overwrites_test() ->
    catch ets:delete(?TABLE),
    ok = init(),
    ok = update(100, 50, 25),
    ok = update(100, 60, 30),
    ?assertEqual({ok, 60, 30}, guild_counts_cache:get(100)),
    catch ets:delete(?TABLE).

delete_removes_entry_test() ->
    catch ets:delete(?TABLE),
    ok = init(),
    ok = update(100, 50, 25),
    ok = delete(100),
    ?assertEqual(miss, guild_counts_cache:get(100)),
    catch ets:delete(?TABLE).

delete_nonexistent_test() ->
    catch ets:delete(?TABLE),
    ok = init(),
    ok = delete(999),
    catch ets:delete(?TABLE).

-endif.
