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

-module(presence_cache).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-compile({no_auto_import, [get/1, put/2]}).

-export([start_link/0, put/2, delete/1, get/1, bulk_get/1, get_memory_stats/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type shard() :: #{pid := pid(), ref := reference()}.
-type state() :: #{shards := #{non_neg_integer() => shard()}, shard_count := pos_integer()}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec put(integer(), map()) -> ok.
put(UserId, Presence) when is_integer(UserId), is_map(Presence) ->
    gen_server:call(?MODULE, {put, UserId, Presence}, ?DEFAULT_GEN_SERVER_TIMEOUT).

-spec delete(integer()) -> ok.
delete(UserId) when is_integer(UserId) ->
    gen_server:call(?MODULE, {delete, UserId}, ?DEFAULT_GEN_SERVER_TIMEOUT).

-spec get(integer()) -> {ok, map()} | not_found.
get(UserId) when is_integer(UserId) ->
    gen_server:call(?MODULE, {get, UserId}, ?DEFAULT_GEN_SERVER_TIMEOUT).

-spec bulk_get([term()]) -> [map()].
bulk_get(UserIds) when is_list(UserIds) ->
    gen_server:call(?MODULE, {bulk_get, UserIds}, ?DEFAULT_GEN_SERVER_TIMEOUT).

-spec get_memory_stats() -> {ok, map()} | {error, term()}.
get_memory_stats() ->
    gen_server:call(?MODULE, get_memory_stats, ?DEFAULT_GEN_SERVER_TIMEOUT).

-spec init(list()) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    {ShardCount, Source} = determine_shard_count(presence_cache_shards),
    Shards = start_shards(ShardCount, #{}),
    maybe_log_shard_source(presence_cache, ShardCount, Source),
    {ok, #{shards => Shards, shard_count => ShardCount}}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({put, UserId, Presence}, _From, State) ->
    {Reply, NewState} = forward_call(UserId, {put, UserId, Presence}, State),
    {reply, Reply, NewState};
handle_call({delete, UserId}, _From, State) ->
    {Reply, NewState} = forward_call(UserId, {delete, UserId}, State),
    {reply, Reply, NewState};
handle_call({get, UserId}, _From, State) ->
    {Reply, NewState} = forward_call(UserId, {get, UserId}, State),
    {reply, Reply, NewState};
handle_call({bulk_get, UserIds}, _From, State) ->
    {Reply, NewState} = forward_bulk_get(UserIds, State),
    {reply, Reply, NewState};
handle_call(get_memory_stats, _From, State) ->
    Count = maps:get(shard_count, State),
    WordSize = erlang:system_info(wordsize),
    TotalMemory = lists:foldl(fun(Index, Acc) ->
        TableName = presence_cache_shard:table_name(Index),
        case ets:info(TableName, memory) of
            undefined -> Acc;
            Words -> Acc + (Words * WordSize)
        end
    end, 0, lists:seq(0, Count - 1)),
    TotalEntries = lists:foldl(fun(Index, Acc) ->
        TableName = presence_cache_shard:table_name(Index),
        case ets:info(TableName, size) of
            undefined -> Acc;
            Size -> Acc + Size
        end
    end, 0, lists:seq(0, Count - 1)),
    {reply, {ok, #{memory_bytes => TotalMemory, entry_count => TotalEntries}}, State};
handle_call(Request, _From, State) ->
    logger:warning("[presence_cache] unknown request ~p", [Request]),
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, process, _Pid, Reason}, State) ->
    Shards = maps:get(shards, State),
    case find_shard_by_ref(Ref, Shards) of
        {ok, Index} ->
            logger:warning("[presence_cache] shard ~p crashed: ~p", [Index, Reason]),
            {_Shard, NewState} = restart_shard(Index, State),
            {noreply, NewState};
        not_found ->
            {noreply, State}
    end;
handle_info({'EXIT', Pid, Reason}, State) ->
    Shards = maps:get(shards, State),
    case find_shard_by_pid(Pid, Shards) of
        {ok, Index} ->
            logger:warning("[presence_cache] shard ~p exited: ~p", [Index, Reason]),
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
        fun(Shard) ->
            Pid = maps:get(pid, Shard),
            catch gen_server:stop(Pid, shutdown, 5000)
        end,
        maps:values(Shards)
    ),
    ok.

-spec code_change(term(), term(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) when is_map(State) ->
    {ok, State};
code_change(_OldVsn, {state, Shards, ShardCount}, _Extra) ->
    ConvertedShards = maps:map(
        fun(_Index, {shard, Pid, Ref}) ->
            #{pid => Pid, ref => Ref}
        end,
        Shards
    ),
    {ok, #{shards => ConvertedShards, shard_count => ShardCount}}.

-spec determine_shard_count(atom()) -> {pos_integer(), configured | auto}.
determine_shard_count(ConfigKey) ->
    case fluxer_gateway_env:get(ConfigKey) of
        Value when is_integer(Value), Value > 0 ->
            {Value, configured};
        _ ->
            {default_shard_count(), auto}
    end.

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

-spec start_shards(pos_integer(), #{}) -> #{non_neg_integer() => shard()}.
start_shards(Count, Acc) ->
    lists:foldl(
        fun(Index, MapAcc) ->
            case start_shard(Index) of
                {ok, Shard} ->
                    maps:put(Index, Shard, MapAcc);
                {error, Reason} ->
                    logger:warning("[presence_cache] failed to start shard ~p: ~p", [Index, Reason]),
                    MapAcc
            end
        end,
        Acc,
        lists:seq(0, Count - 1)
    ).

-spec start_shard(non_neg_integer()) -> {ok, shard()} | {error, term()}.
start_shard(Index) ->
    case presence_cache_shard:start_link(Index) of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            {ok, #{pid => Pid, ref => Ref}};
        Error ->
            Error
    end.

-spec restart_shard(non_neg_integer(), state()) -> {shard(), state()}.
restart_shard(Index, State) ->
    case start_shard(Index) of
        {ok, Shard} ->
            Shards = maps:get(shards, State),
            Updated = State#{shards := maps:put(Index, Shard, Shards)},
            {Shard, Updated};
        {error, Reason} ->
            logger:error("[presence_cache] failed to restart shard ~p: ~p", [Index, Reason]),
            Dummy = #{pid => spawn(fun() -> exit(normal) end), ref => make_ref()},
            {Dummy, State}
    end.

-spec forward_call(term(), term(), state()) -> {term(), state()}.
forward_call(Key, Request, State) ->
    {Index, State1} = ensure_shard(Key, State),
    call_shard(Index, Request, State1).

-spec forward_bulk_get([term()], state()) -> {term(), state()}.
forward_bulk_get(UserIds, State) ->
    Count = maps:get(shard_count, State),
    Unique = lists:usort(UserIds),
    Groups = rendezvous_router:group_keys(Unique, Count),
    {Results, FinalState} =
        lists:foldl(
            fun({Index, Ids}, {AccResults, AccState}) ->
                {Reply, State1} = call_shard(Index, {bulk_get, Ids}, AccState),
                case Reply of
                    List when is_list(List) ->
                        {Reply ++ AccResults, State1};
                    _ ->
                        {AccResults, State1}
                end
            end,
            {[], State},
            Groups
        ),
    {lists:reverse(Results), FinalState}.

-spec call_shard(non_neg_integer(), term(), state()) -> {term(), state()}.
call_shard(Index, Request, State) ->
    Shards = maps:get(shards, State),
    Shard = maps:get(Index, Shards),
    Pid = maps:get(pid, Shard),
    case catch gen_server:call(Pid, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        {'EXIT', _} ->
            {_Shard, State1} = restart_shard(Index, State),
            call_shard(Index, Request, State1);
        Reply ->
            {Reply, State}
    end.

-spec ensure_shard(term(), state()) -> {non_neg_integer(), state()}.
ensure_shard(Key, State) ->
    Count = maps:get(shard_count, State),
    Index = select_shard(Key, Count),
    ensure_shard_for_index(Index, State).

-spec ensure_shard_for_index(non_neg_integer(), state()) -> {non_neg_integer(), state()}.
ensure_shard_for_index(Index, State) ->
    Shards = maps:get(shards, State),
    case maps:get(Index, Shards, undefined) of
        undefined ->
            {_Shard, NewState} = restart_shard(Index, State),
            {Index, NewState};
        #{pid := Pid} when is_pid(Pid) ->
            case erlang:is_process_alive(Pid) of
                true ->
                    {Index, State};
                false ->
                    {_Shard, NewState} = restart_shard(Index, State),
                    {Index, NewState}
            end
    end.

-spec select_shard(term(), pos_integer()) -> non_neg_integer().
select_shard(Key, Count) when Count > 0 ->
    rendezvous_router:select(Key, Count).

-spec find_shard_by_ref(reference(), #{non_neg_integer() => shard()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_ref(Ref, Shards) ->
    maps:fold(
        fun
            (Index, #{ref := R}, _) when R =:= Ref -> {ok, Index};
            (_, _, Acc) -> Acc
        end,
        not_found,
        Shards
    ).

-spec find_shard_by_pid(pid(), #{non_neg_integer() => shard()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_pid(Pid, Shards) ->
    maps:fold(
        fun
            (Index, #{pid := P}, _) when P =:= Pid -> {ok, Index};
            (_, _, Acc) -> Acc
        end,
        not_found,
        Shards
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

put_and_get_visible_status_test() ->
    {ok, Pid} = maybe_start_for_test(),
    Presence = #{<<"status">> => <<"online">>},
    ?assertEqual(ok, put(1, Presence)),
    ?assertMatch({ok, _}, get(1)),
    ?assertEqual(ok, gen_server:stop(Pid)).

put_offline_evicted_test() ->
    {ok, Pid} = maybe_start_for_test(),
    Presence = #{<<"status">> => <<"offline">>},
    ?assertEqual(ok, put(2, Presence)),
    ?assertEqual(not_found, get(2)),
    ?assertEqual(ok, gen_server:stop(Pid)).

bulk_get_across_shards_test() ->
    {ok, Pid} = maybe_start_for_test(),
    Visible = #{<<"status">> => <<"online">>, <<"user">> => #{<<"id">> => <<"3">>}},
    put(3, Visible),
    put(4, Visible),
    Results = bulk_get([3, 4, 3]),
    ?assertEqual(2, length(Results)),
    ?assertEqual(ok, gen_server:stop(Pid)).

maybe_start_for_test() ->
    case whereis(?MODULE) of
        undefined -> start_link();
        Existing when is_pid(Existing) -> {ok, Existing}
    end.
-endif.
