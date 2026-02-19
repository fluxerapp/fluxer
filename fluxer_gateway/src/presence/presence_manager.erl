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

-module(presence_manager).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-define(PID_CACHE_TABLE, presence_pid_cache).
-define(SHARD_TABLE, presence_manager_shard_table).
-define(CACHE_TTL_MS, 300000).

-export([
    start_link/0,
    lookup/1,
    lookup_async/2,
    start_or_lookup/1,
    dispatch_to_user/3,
    terminate_all_sessions/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type user_id() :: integer().
-type event_type() :: atom() | binary().
-type shard() :: #{pid := pid(), ref := reference()}.
-type state() :: #{shards := #{non_neg_integer() => shard()}, shard_count := pos_integer()}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec lookup(user_id()) -> {ok, pid()} | {error, not_found}.
lookup(UserId) ->
    case check_cache(UserId) of
        {hit, Pid} ->
            {ok, Pid};
        miss ->
            lookup_and_cache(UserId)
    end.

-spec start_or_lookup(map()) -> {ok, pid()} | {error, term()}.
start_or_lookup(Request) when is_map(Request) ->
    UserId = maps:get(user_id, Request, 0),
    call_shard(UserId, {start_or_lookup, Request}, ?DEFAULT_GEN_SERVER_TIMEOUT).

-spec lookup_async(user_id(), term()) -> ok.
lookup_async(UserId, Message) ->
    case check_cache(UserId) of
        {hit, Pid} ->
            gen_server:cast(Pid, Message);
        miss ->
            spawn(fun() -> lookup_and_cast(UserId, Message) end)
    end,
    ok.

-spec check_cache(user_id()) -> {hit, pid()} | miss.
check_cache(UserId) ->
    case ets:lookup(?PID_CACHE_TABLE, UserId) of
        [{UserId, Pid, Timestamp}] ->
            IsFresh = erlang:monotonic_time(millisecond) - Timestamp < ?CACHE_TTL_MS,
            IsAlive = erlang:is_process_alive(Pid),
            case {IsFresh, IsAlive} of
                {true, true} ->
                    {hit, Pid};
                _ ->
                    ets:delete(?PID_CACHE_TABLE, UserId),
                    miss
            end;
        [] ->
            miss
    end.

-spec lookup_and_cache(user_id()) -> {ok, pid()} | {error, not_found}.
lookup_and_cache(UserId) ->
    case call_shard(UserId, {lookup, UserId}, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        {ok, Pid} ->
            ets:insert(?PID_CACHE_TABLE, {UserId, Pid, erlang:monotonic_time(millisecond)}),
            {ok, Pid};
        _ ->
            {error, not_found}
    end.

-spec lookup_and_cast(user_id(), term()) -> ok.
lookup_and_cast(UserId, Message) ->
    case call_shard(UserId, {lookup, UserId}, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        {ok, Pid} ->
            ets:insert(?PID_CACHE_TABLE, {UserId, Pid, erlang:monotonic_time(millisecond)}),
            gen_server:cast(Pid, Message);
        _ ->
            ok
    end.

-spec terminate_all_sessions(user_id()) -> ok | {error, term()}.
terminate_all_sessions(UserId) ->
    call_shard(UserId, {terminate_all_sessions, UserId}, ?DEFAULT_GEN_SERVER_TIMEOUT).

-spec dispatch_to_user(user_id(), event_type(), term()) -> ok | {error, not_found}.
dispatch_to_user(UserId, Event, Data) ->
    case call_shard(UserId, {dispatch, UserId, Event, Data}, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        ok ->
            ok;
        {error, not_found} ->
            {error, not_found};
        _ ->
            {error, not_found}
    end.

-spec init(list()) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    ensure_shard_table(),
    ets:new(?PID_CACHE_TABLE, [named_table, public, set]),
    {ShardCount, _Source} = determine_shard_count(),
    {ShardMap, _} = lists:foldl(
        fun(Index, {Acc, Counter}) ->
            case start_shard(Index) of
                {ok, Shard} ->
                    {maps:put(Index, Shard, Acc), Counter + 1};
                {error, _Reason} ->
                    {Acc, Counter}
            end
        end,
        {#{}, 0},
        lists:seq(0, ShardCount - 1)
    ),
    State = #{shards => ShardMap, shard_count => ShardCount},
    sync_shard_table(State),
    {ok, State}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({lookup, UserId}, _From, State) ->
    {Reply, NewState} = forward_call(UserId, {lookup, UserId}, State),
    {reply, Reply, NewState};
handle_call({dispatch, UserId, Event, Data}, _From, State) ->
    {Reply, NewState} = forward_call(UserId, {dispatch, UserId, Event, Data}, State),
    {reply, Reply, NewState};
handle_call({terminate_all_sessions, UserId}, _From, State) ->
    {Reply, NewState} = forward_call(UserId, {terminate_all_sessions, UserId}, State),
    {reply, Reply, NewState};
handle_call({start_or_lookup, _} = Request, _From, State) ->
    Key = extract_user_id(Request),
    {Reply, NewState} = forward_call(Key, Request, State),
    {reply, Reply, NewState};
handle_call(get_local_count, _From, State) ->
    {Count, NewState} = aggregate_counts(get_local_count, State),
    {reply, {ok, Count}, NewState};
handle_call(get_global_count, _From, State) ->
    {Count, NewState} = aggregate_counts(get_global_count, State),
    {reply, {ok, Count}, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    clean_cache_by_pid(Pid),
    Shards = maps:get(shards, State),
    case find_shard_by_ref(Ref, Shards) of
        {ok, Index} ->
            {_ShardEntry, UpdatedState} = restart_shard(Index, State),
            {noreply, UpdatedState};
        not_found ->
            {noreply, State}
    end;
handle_info({'EXIT', Pid, _Reason}, State) ->
    clean_cache_by_pid(Pid),
    Shards = maps:get(shards, State),
    case find_shard_by_pid(Pid, Shards) of
        {ok, Index} ->
            {_ShardEntry, UpdatedState} = restart_shard(Index, State),
            {noreply, UpdatedState};
        not_found ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    catch ets:delete(?SHARD_TABLE),
    catch ets:delete(?PID_CACHE_TABLE),
    Shards = maps:get(shards, State),
    lists:foreach(
        fun(#{pid := Pid}) ->
            catch gen_server:stop(Pid, shutdown, 5000)
        end,
        maps:values(Shards)
    ),
    ok.

-spec code_change(term(), term(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) when is_map(State) ->
    sync_shard_table(State),
    {ok, State};
code_change(_OldVsn, {state, Shards, ShardCount}, _Extra) ->
    ConvertedShards = maps:map(
        fun(_Index, {shard, Pid, Ref}) ->
            #{pid => Pid, ref => Ref}
        end,
        Shards
    ),
    ConvertedState = #{shards => ConvertedShards, shard_count => ShardCount},
    sync_shard_table(ConvertedState),
    {ok, ConvertedState}.

-spec determine_shard_count() -> {pos_integer(), configured | auto}.
determine_shard_count() ->
    case fluxer_gateway_env:get(presence_shards) of
        Value when is_integer(Value), Value > 0 ->
            {Value, configured};
        _ ->
            {default_shard_count(), auto}
    end.

-spec start_shard(non_neg_integer()) -> {ok, shard()} | {error, term()}.
start_shard(Index) ->
    case presence_manager_shard:start_link(Index) of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            put_shard_pid(Index, Pid),
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
            sync_shard_table(Updated),
            {Shard, Updated};
        {error, _Reason} ->
            clear_shard_pid(Index),
            Dummy = #{pid => spawn(fun() -> exit(normal) end), ref => make_ref()},
            {Dummy, State}
    end.

-spec call_shard(user_id(), term(), pos_integer()) -> term().
call_shard(Key, Request, Timeout) ->
    case shard_pid_from_table(Key) of
        {ok, Pid} ->
            case catch gen_server:call(Pid, Request, Timeout) of
                {'EXIT', {timeout, _}} ->
                    {error, timeout};
                {'EXIT', _} ->
                    call_via_manager(Request, Timeout);
                Reply ->
                    Reply
            end;
        error ->
            call_via_manager(Request, Timeout)
    end.

-spec call_via_manager(term(), pos_integer()) -> term().
call_via_manager(Request, Timeout) ->
    case catch gen_server:call(?MODULE, Request, Timeout + 1000) of
        {'EXIT', {timeout, _}} ->
            {error, timeout};
        {'EXIT', _} ->
            {error, unavailable};
        Reply ->
            Reply
    end.

-spec forward_call(user_id(), term(), state()) -> {term(), state()}.
forward_call(Key, Request, State) ->
    {ShardIndex, State1} = ensure_shard(Key, State),
    Shards = maps:get(shards, State1),
    #{pid := Pid} = maps:get(ShardIndex, Shards),
    case catch gen_server:call(Pid, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        {'EXIT', _} ->
            {_ShardEntry, State2} = restart_shard(ShardIndex, State1),
            forward_call(Key, Request, State2);
        Reply ->
            {Reply, State1}
    end.

-spec aggregate_counts(term(), state()) -> {non_neg_integer(), state()}.
aggregate_counts(Request, State) ->
    Shards = maps:get(shards, State),
    Results = [
        begin
            #{pid := Pid} = Shard,
            case catch gen_server:call(Pid, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
                {ok, Count} -> Count;
                _ -> 0
            end
        end
     || Shard <- maps:values(Shards)
    ],
    {lists:sum(Results), State}.

-spec ensure_shard(user_id(), state()) -> {non_neg_integer(), state()}.
ensure_shard(Key, State) ->
    Count = maps:get(shard_count, State),
    Shards = maps:get(shards, State),
    Index = select_shard(Key, Count),
    case maps:get(Index, Shards, undefined) of
        undefined ->
            {_ShardEntry, NewState} = restart_shard(Index, State),
            {Index, NewState};
        #{pid := Pid} ->
            case erlang:is_process_alive(Pid) of
                true ->
                    {Index, State};
                false ->
                    {_ShardEntry, NewState} = restart_shard(Index, State),
                    {Index, NewState}
            end
    end.

-spec select_shard(user_id(), pos_integer()) -> non_neg_integer().
select_shard(Key, Count) when Count > 0 ->
    rendezvous_router:select(Key, Count).

-spec extract_user_id(term()) -> user_id().
extract_user_id({start_or_lookup, #{user_id := UserId}}) -> UserId;
extract_user_id(_) -> 0.

-spec ensure_shard_table() -> ok.
ensure_shard_table() ->
    case ets:whereis(?SHARD_TABLE) of
        undefined ->
            _ = ets:new(?SHARD_TABLE, [named_table, public, set, {read_concurrency, true}]),
            ok;
        _ ->
            ok
    end.

-spec sync_shard_table(state()) -> ok.
sync_shard_table(State) ->
    ensure_shard_table(),
    _ = ets:delete_all_objects(?SHARD_TABLE),
    ShardCount = maps:get(shard_count, State),
    ets:insert(?SHARD_TABLE, {shard_count, ShardCount}),
    Shards = maps:get(shards, State),
    lists:foreach(
        fun({Index, #{pid := Pid}}) ->
            put_shard_pid(Index, Pid)
        end,
        maps:to_list(Shards)
    ),
    ok.

-spec put_shard_pid(non_neg_integer(), pid()) -> ok.
put_shard_pid(Index, Pid) ->
    ensure_shard_table(),
    ets:insert(?SHARD_TABLE, {{shard_pid, Index}, Pid}),
    ok.

-spec clear_shard_pid(non_neg_integer()) -> ok.
clear_shard_pid(Index) ->
    try ets:delete(?SHARD_TABLE, {shard_pid, Index}) of
        _ ->
            ok
    catch
        error:badarg ->
            ok
    end.

-spec shard_pid_from_table(user_id()) -> {ok, pid()} | error.
shard_pid_from_table(Key) ->
    try
        case ets:lookup(?SHARD_TABLE, shard_count) of
            [{shard_count, ShardCount}] when is_integer(ShardCount), ShardCount > 0 ->
                Index = select_shard(Key, ShardCount),
                case ets:lookup(?SHARD_TABLE, {shard_pid, Index}) of
                    [{{shard_pid, Index}, Pid}] when is_pid(Pid) ->
                        case erlang:is_process_alive(Pid) of
                            true ->
                                {ok, Pid};
                            false ->
                                error
                        end;
                    _ ->
                        error
                end;
            _ ->
                error
        end
    catch
        error:badarg ->
            error
    end.

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

-spec default_shard_count() -> pos_integer().
default_shard_count() ->
    Candidates = [
        erlang:system_info(logical_processors_available),
        erlang:system_info(schedulers_online)
    ],
    lists:max([C || C <- Candidates, is_integer(C), C > 0] ++ [1]).

-spec clean_cache_by_pid(pid()) -> ok.
clean_cache_by_pid(Pid) ->
    ets:foldl(
        fun
            ({UserId, CachedPid, _}, Acc) when CachedPid =:= Pid ->
                ets:delete(?PID_CACHE_TABLE, UserId),
                Acc;
            (_, Acc) ->
                Acc
        end,
        ok,
        ?PID_CACHE_TABLE
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

determine_shard_count_configured_test() ->
    with_runtime_config(presence_shards, 5, fun() ->
        ?assertMatch({5, configured}, determine_shard_count())
    end).

determine_shard_count_auto_test() ->
    with_runtime_config(presence_shards, undefined, fun() ->
        {Count, auto} = determine_shard_count(),
        ?assert(Count > 0)
    end).

select_shard_test() ->
    ?assert(select_shard(100, 4) >= 0),
    ?assert(select_shard(100, 4) < 4).

extract_user_id_test() ->
    ?assertEqual(123, extract_user_id({start_or_lookup, #{user_id => 123}})),
    ?assertEqual(0, extract_user_id(unknown)).

find_shard_by_ref_test() ->
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Shards = #{0 => #{pid => self(), ref => Ref1}, 1 => #{pid => self(), ref => Ref2}},
    ?assertEqual({ok, 0}, find_shard_by_ref(Ref1, Shards)),
    ?assertEqual({ok, 1}, find_shard_by_ref(Ref2, Shards)),
    ?assertEqual(not_found, find_shard_by_ref(make_ref(), Shards)).

default_shard_count_test() ->
    ?assert(default_shard_count() >= 1).

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
