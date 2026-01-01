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

-module(presence_cache_shard).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([start_link/1, table_name/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type state() :: #{table := atom(), shard_index := non_neg_integer()}.

-define(TABLE_PREFIX, presence_cache).

-spec start_link(non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(ShardIndex) ->
    gen_server:start_link(?MODULE, #{shard_index => ShardIndex}, []).

-spec init(map()) -> {ok, state()}.
init(#{shard_index := ShardIndex}) ->
    process_flag(trap_exit, true),
    TableName = table_name(ShardIndex),
    ensure_table(TableName),
    {ok, #{table => TableName, shard_index => ShardIndex}}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({put, UserId, Presence}, _From, State) ->
    Table = maps:get(table, State),
    {reply, do_put(Table, UserId, Presence), State};
handle_call({delete, UserId}, _From, State) ->
    Table = maps:get(table, State),
    ets:delete(Table, UserId),
    {reply, ok, State};
handle_call({get, UserId}, _From, State) ->
    Table = maps:get(table, State),
    Reply =
        case catch ets:lookup(Table, UserId) of
            [{_, Presence}] -> {ok, Presence};
            _ -> not_found
        end,
    {reply, Reply, State};
handle_call({bulk_get, UserIds}, _From, State) ->
    Table = maps:get(table, State),
    Presences =
        lists:filtermap(
            fun(Uid) ->
                case catch ets:lookup(Table, Uid) of
                    [{_, Presence}] -> {true, Presence};
                    _ -> false
                end
            end,
            lists:usort(UserIds)
        ),
    {reply, Presences, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), term(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) when is_map(State) ->
    {ok, State};
code_change(_OldVsn, {state, Table, ShardIndex}, _Extra) ->
    {ok, #{table => Table, shard_index => ShardIndex}}.

-spec do_put(atom(), integer(), map()) -> ok.
do_put(Table, UserId, Presence) ->
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    case Status of
        <<"invisible">> ->
            ets:delete(Table, UserId),
            ok;
        <<"offline">> ->
            ets:delete(Table, UserId),
            ok;
        _ ->
            ets:insert(Table, {UserId, Presence}),
            ok
    end.

-spec ensure_table(atom()) -> ok.
ensure_table(Table) ->
    case ets:info(Table) of
        undefined ->
            ets:new(Table, [named_table, public, set, {read_concurrency, true}]),
            ok;
        _ ->
            ok
    end.

-spec table_name(non_neg_integer()) -> atom().
table_name(Index) ->
    list_to_atom(atom_to_list(?TABLE_PREFIX) ++ "_" ++ integer_to_list(Index)).
