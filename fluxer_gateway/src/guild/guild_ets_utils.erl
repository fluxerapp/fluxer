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

-module(guild_ets_utils).

-export([ensure_table/2]).

-spec ensure_table(atom(), list()) -> ok.
ensure_table(TableName, Options) ->
    case ets:whereis(TableName) of
        undefined ->
            try ets:new(TableName, Options) of
                _ -> ok
            catch
                error:badarg -> ok
            end;
        _ ->
            ok
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ensure_table_creates_new_table_test() ->
    TableName = guild_ets_utils_test_table,
    catch ets:delete(TableName),
    ok = ensure_table(TableName, [named_table, public, set]),
    ?assertNotEqual(undefined, ets:whereis(TableName)),
    ets:delete(TableName).

ensure_table_idempotent_test() ->
    TableName = guild_ets_utils_test_idempotent,
    catch ets:delete(TableName),
    ok = ensure_table(TableName, [named_table, public, set]),
    ok = ensure_table(TableName, [named_table, public, set]),
    ?assertNotEqual(undefined, ets:whereis(TableName)),
    ets:delete(TableName).

-endif.
