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

-module(guild_manager).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type guild_id() :: integer().
-type shard_map() :: #{pid => pid(), ref => reference()}.
-type state() :: #{
    shards => #{non_neg_integer() => shard_map()},
    shard_count => pos_integer()
}.

-record(shard, {
    pid :: pid(),
    ref :: reference()
}).

-record(state, {
    shards = #{} :: #{non_neg_integer() => #shard{}},
    shard_count = 1 :: pos_integer()
}).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init(list()) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    {ShardCount, Source} = determine_shard_count(),
    ShardMap = start_shards(ShardCount, #{}),
    maybe_log_shard_source(guild_manager, ShardCount, Source),
    {ok, #{shards => ShardMap, shard_count => ShardCount}}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({start_or_lookup, GuildId} = Request, _From, State) ->
    {Reply, NewState} = forward_call(GuildId, Request, State),
    {reply, Reply, NewState};
handle_call({stop_guild, GuildId} = Request, _From, State) ->
    {Reply, NewState} = forward_call(GuildId, Request, State),
    {reply, Reply, NewState};
handle_call({reload_guild, GuildId} = Request, _From, State) ->
    {Reply, NewState} = forward_call(GuildId, Request, State),
    {reply, Reply, NewState};
handle_call({shutdown_guild, GuildId} = Request, _From, State) ->
    {Reply, NewState} = forward_call(GuildId, Request, State),
    {reply, Reply, NewState};
handle_call({reload_all_guilds, GuildIds}, _From, State) ->
    {Reply, NewState} = handle_reload_all(GuildIds, State),
    {reply, Reply, NewState};
handle_call(get_local_count, _From, State) ->
    {Count, NewState} = aggregate_counts(get_local_count, State),
    {reply, {ok, Count}, NewState};
handle_call(get_global_count, _From, State) ->
    {Count, NewState} = aggregate_counts(get_global_count, State),
    {reply, {ok, Count}, NewState};
handle_call(Request, _From, State) ->
    logger:warning("[guild_manager] unknown request ~p", [Request]),
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, process, _Pid, Reason}, State) ->
    Shards = maps:get(shards, State),
    case find_shard_by_ref(Ref, Shards) of
        {ok, Index} ->
            logger:warning("[guild_manager] shard ~p crashed: ~p", [Index, Reason]),
            {_Shard, NewState} = restart_shard(Index, State),
            {noreply, NewState};
        not_found ->
            {noreply, State}
    end;
handle_info({'EXIT', Pid, Reason}, State) ->
    Shards = maps:get(shards, State),
    case find_shard_by_pid(Pid, Shards) of
        {ok, Index} ->
            logger:warning("[guild_manager] shard ~p exited: ~p", [Index, Reason]),
            {_Shard, NewState} = restart_shard(Index, State),
            {noreply, NewState};
        not_found ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    Shards = maps:get(shards, State),
    lists:foreach(
        fun(ShardMap) ->
            Pid = maps:get(pid, ShardMap),
            catch gen_server:stop(Pid, shutdown, 5000)
        end,
        maps:values(Shards)
    ),
    ok.

-spec code_change(term(), term(), term()) -> {ok, state()}.
code_change(_OldVsn, #state{shards = OldShards, shard_count = ShardCount}, _Extra) ->
    NewShards = maps:map(
        fun(_Index, #shard{pid = Pid, ref = Ref}) ->
            #{pid => Pid, ref => Ref}
        end,
        OldShards
    ),
    {ok, #{shards => NewShards, shard_count => ShardCount}};
code_change(_OldVsn, State, _Extra) when is_map(State) ->
    {ok, State}.

-spec determine_shard_count() -> {pos_integer(), configured | auto}.
determine_shard_count() ->
    case fluxer_gateway_env:get(guild_shards) of
        Value when is_integer(Value), Value > 0 ->
            {Value, configured};
        _ ->
            {default_shard_count(), auto}
    end.

-spec start_shards(pos_integer(), #{}) -> #{non_neg_integer() => shard_map()}.
start_shards(Count, Acc) ->
    lists:foldl(
        fun(Index, MapAcc) ->
            case start_shard(Index) of
                {ok, Shard} ->
                    maps:put(Index, Shard, MapAcc);
                {error, Reason} ->
                    logger:warning("[guild_manager] failed to start shard ~p: ~p", [Index, Reason]),
                    MapAcc
            end
        end,
        Acc,
        lists:seq(0, Count - 1)
    ).

-spec start_shard(non_neg_integer()) -> {ok, shard_map()} | {error, term()}.
start_shard(Index) ->
    case guild_manager_shard:start_link(Index) of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            {ok, #{pid => Pid, ref => Ref}};
        Error ->
            Error
    end.

-spec restart_shard(non_neg_integer(), state()) -> {shard_map(), state()}.
restart_shard(Index, State) ->
    Shards = maps:get(shards, State),
    case start_shard(Index) of
        {ok, Shard} ->
            Updated = State#{shards => maps:put(Index, Shard, Shards)},
            {Shard, Updated};
        {error, Reason} ->
            logger:error("[guild_manager] failed to restart shard ~p: ~p", [Index, Reason]),
            Dummy = #{pid => spawn(fun() -> exit(normal) end), ref => make_ref()},
            {Dummy, State}
    end.

-spec forward_call(guild_id(), term(), state()) -> {term(), state()}.
forward_call(GuildId, Request, State) ->
    {Index, State1} = ensure_shard(GuildId, State),
    Shards = maps:get(shards, State1),
    ShardMap = maps:get(Index, Shards),
    Pid = maps:get(pid, ShardMap),
    case catch gen_server:call(Pid, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        {'EXIT', _} ->
            {_Shard, State2} = restart_shard(Index, State1),
            forward_call(GuildId, Request, State2);
        Reply ->
            {Reply, State1}
    end.

-spec ensure_shard(guild_id(), state()) -> {non_neg_integer(), state()}.
ensure_shard(GuildId, State) ->
    Count = maps:get(shard_count, State),
    Index = select_shard(GuildId, Count),
    ensure_shard_for_index(Index, State).

-spec ensure_shard_for_index(non_neg_integer(), state()) -> {non_neg_integer(), state()}.
ensure_shard_for_index(Index, State) ->
    Shards = maps:get(shards, State),
    case maps:get(Index, Shards, undefined) of
        undefined ->
            {_Shard, NewState} = restart_shard(Index, State),
            {Index, NewState};
        ShardMap when is_map(ShardMap) ->
            Pid = maps:get(pid, ShardMap),
            case erlang:is_process_alive(Pid) of
                true ->
                    {Index, State};
                false ->
                    {_Shard, NewState} = restart_shard(Index, State),
                    {Index, NewState}
            end
    end.

-spec select_shard(guild_id(), pos_integer()) -> non_neg_integer().
select_shard(GuildId, Count) when Count > 0 ->
    rendezvous_router:select(GuildId, Count).

-spec aggregate_counts(term(), state()) -> {non_neg_integer(), state()}.
aggregate_counts(Request, State) ->
    Shards = maps:get(shards, State),
    Counts =
        [
            begin
                Pid = maps:get(pid, ShardMap),
                case catch gen_server:call(Pid, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
                    {ok, Count} -> Count;
                    _ -> 0
                end
            end
         || ShardMap <- maps:values(Shards)
        ],
    {lists:sum(Counts), State}.

-spec handle_reload_all([guild_id()], state()) -> {#{count => non_neg_integer()}, state()}.
handle_reload_all([], State) ->
    Shards = maps:get(shards, State),
    {Replies, FinalState} =
        lists:foldl(
            fun({_Index, ShardMap}, {AccReplies, AccState}) ->
                Pid = maps:get(pid, ShardMap),
                case catch gen_server:call(Pid, {reload_all_guilds, []}, 60000) of
                    Reply ->
                        {AccReplies ++ [Reply], AccState}
                end
            end,
            {[], State},
            maps:to_list(Shards)
        ),
    Count = lists:sum([maps:get(count, Reply, 0) || Reply <- Replies]),
    {#{count => Count}, FinalState};
handle_reload_all(GuildIds, State) ->
    Count = maps:get(shard_count, State),
    Groups = group_ids_by_shard(GuildIds, Count),
    {TotalCount, FinalState} =
        lists:foldl(
            fun({Index, Ids}, {AccCount, AccState}) ->
                {ShardIdx, State1} = ensure_shard_for_index(Index, AccState),
                Shards = maps:get(shards, State1),
                ShardMap = maps:get(ShardIdx, Shards),
                Pid = maps:get(pid, ShardMap),
                case catch gen_server:call(Pid, {reload_all_guilds, Ids}, 60000) of
                    #{count := CountReply} ->
                        {AccCount + CountReply, State1};
                    _ ->
                        {AccCount, State1}
                end
            end,
            {0, State},
            Groups
        ),
    {#{count => TotalCount}, FinalState}.

-spec group_ids_by_shard([guild_id()], pos_integer()) -> [{non_neg_integer(), [guild_id()]}].
group_ids_by_shard(GuildIds, ShardCount) ->
    lists:foldl(
        fun(GuildId, Acc) ->
            Index = select_shard(GuildId, ShardCount),
            case lists:keytake(Index, 1, Acc) of
                {value, {Index, Ids}, Rest} ->
                    [{Index, [GuildId | Ids]} | Rest];
                false ->
                    [{Index, [GuildId]} | Acc]
            end
        end,
        [],
        GuildIds
    ).

-spec find_shard_by_ref(reference(), #{non_neg_integer() => shard_map()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_ref(Ref, Shards) ->
    maps:fold(
        fun
            (Index, ShardMap, _) when is_map(ShardMap) ->
                case maps:get(ref, ShardMap) of
                    R when R =:= Ref -> {ok, Index};
                    _ -> not_found
                end;
            (_, _, Acc) ->
                Acc
        end,
        not_found,
        Shards
    ).

-spec find_shard_by_pid(pid(), #{non_neg_integer() => shard_map()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_pid(Pid, Shards) ->
    maps:fold(
        fun
            (Index, ShardMap, _) when is_map(ShardMap) ->
                case maps:get(pid, ShardMap) of
                    P when P =:= Pid -> {ok, Index};
                    _ -> not_found
                end;
            (_, _, Acc) ->
                Acc
        end,
        not_found,
        Shards
    ).

-spec default_shard_count() -> pos_integer().
default_shard_count() ->
    Candidates = [
        erlang:system_info(logical_processors_available), erlang:system_info(schedulers_online)
    ],
    lists:max([C || C <- Candidates, is_integer(C), C > 0] ++ [1]).

-spec maybe_log_shard_source(atom(), pos_integer(), configured | auto) -> ok.
maybe_log_shard_source(Name, Count, configured) ->
    logger:info("[~p] starting with ~p shards (configured)", [Name, Count]),
    ok;
maybe_log_shard_source(Name, Count, auto) ->
    logger:info("[~p] starting with ~p shards (auto)", [Name, Count]),
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

determine_shard_count_configured_test() ->
    with_runtime_config(guild_shards, 4, fun() ->
        ?assertMatch({4, configured}, determine_shard_count())
    end).

determine_shard_count_auto_test() ->
    with_runtime_config(guild_shards, undefined, fun() ->
        {Count, auto} = determine_shard_count(),
        ?assert(Count > 0)
    end).

with_runtime_config(Key, Value, Fun) ->
    Original = fluxer_gateway_env:get(Key),
    fluxer_gateway_env:patch(#{Key => Value}),
    Result = Fun(),
    fluxer_gateway_env:update(fun(Map) ->
        case Original of
            undefined -> maps:remove(Key, Map);
            Val -> maps:put(Key, Val, Map)
        end
    end),
    Result.
-endif.
