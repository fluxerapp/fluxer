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

-module(session_manager).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-define(SHARD_TABLE, session_manager_shard_table).
-define(START_TIMEOUT, 10000).
-define(LOOKUP_TIMEOUT, 5000).

-export([start_link/0, start/2, lookup/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type session_id() :: binary().
-type shard() :: #{pid := pid(), ref := reference()}.
-type state() :: #{shards := #{non_neg_integer() => shard()}, shard_count := pos_integer()}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start(map(), pid()) -> term().
start(Request, SocketPid) ->
    SessionId = maps:get(session_id, Request),
    call_shard(SessionId, {start, Request, SocketPid}, ?START_TIMEOUT).

-spec lookup(session_id()) -> {ok, pid()} | {error, not_found}.
lookup(SessionId) ->
    call_shard(SessionId, {lookup, SessionId}, ?LOOKUP_TIMEOUT).

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    fluxer_gateway_env:load(),
    ensure_shard_table(),
    {ShardCount, _Source} = determine_shard_count(),
    Shards = start_shards(ShardCount),
    State = #{shards => Shards, shard_count => ShardCount},
    sync_shard_table(State),
    {ok, State}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({proxy_call, SessionId, Request, Timeout}, _From, State) ->
    {Reply, NewState} = forward_call(SessionId, Request, Timeout, State),
    {reply, Reply, NewState};
handle_call({start, Request, SocketPid}, _From, State) ->
    SessionId = maps:get(session_id, Request),
    {Reply, NewState} = forward_call(SessionId, {start, Request, SocketPid}, ?START_TIMEOUT, State),
    {reply, Reply, NewState};
handle_call({lookup, SessionId}, _From, State) ->
    {Reply, NewState} = forward_call(SessionId, {lookup, SessionId}, ?LOOKUP_TIMEOUT, State),
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
    Shards = maps:get(shards, State),
    case find_shard_by_ref(Ref, Shards) of
        {ok, Index} ->
            {_Shard, NewState} = restart_shard(Index, State),
            {noreply, NewState};
        not_found ->
            case find_shard_by_pid(Pid, Shards) of
                {ok, Index} ->
                    {_Shard, NewState} = restart_shard(Index, State),
                    {noreply, NewState};
                not_found ->
                    {noreply, State}
            end
    end;
handle_info({'EXIT', Pid, _Reason}, State) ->
    Shards = maps:get(shards, State),
    case find_shard_by_pid(Pid, Shards) of
        {ok, Index} ->
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
        fun(#{pid := Pid}) ->
            catch gen_server:stop(Pid, shutdown, 5000)
        end,
        maps:values(Shards)
    ),
    catch ets:delete(?SHARD_TABLE),
    ok.

-spec code_change(term(), term(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) when is_map(State) ->
    case {maps:is_key(shards, State), maps:is_key(shard_count, State)} of
        {true, true} ->
            sync_shard_table(State),
            {ok, State};
        _ ->
            {ok, rebuild_state()}
    end;
code_change(_OldVsn, _State, _Extra) ->
    {ok, rebuild_state()}.

-spec call_shard(session_id(), term(), pos_integer()) -> term().
call_shard(SessionId, Request, Timeout) ->
    case shard_pid_from_table(SessionId) of
        {ok, Pid} ->
            case catch gen_server:call(Pid, Request, Timeout) of
                {'EXIT', {timeout, _}} ->
                    {error, timeout};
                {'EXIT', _} ->
                    call_via_manager(SessionId, Request, Timeout);
                Reply ->
                    Reply
            end;
        error ->
            call_via_manager(SessionId, Request, Timeout)
    end.

-spec call_via_manager(session_id(), term(), pos_integer()) -> term().
call_via_manager(SessionId, Request, Timeout) ->
    case catch gen_server:call(?MODULE, {proxy_call, SessionId, Request, Timeout}, Timeout + 1000) of
        {'EXIT', {timeout, _}} ->
            {error, timeout};
        {'EXIT', _} ->
            {error, unavailable};
        Reply ->
            Reply
    end.

-spec forward_call(session_id(), term(), pos_integer(), state()) -> {term(), state()}.
forward_call(SessionId, Request, Timeout, State) ->
    {Index, State1} = ensure_shard(SessionId, State),
    Shards = maps:get(shards, State1),
    #{pid := Pid} = maps:get(Index, Shards),
    case catch gen_server:call(Pid, Request, Timeout) of
        {'EXIT', _} ->
            {_Shard, State2} = restart_shard(Index, State1),
            Shards2 = maps:get(shards, State2),
            #{pid := RetryPid} = maps:get(Index, Shards2),
            case catch gen_server:call(RetryPid, Request, Timeout) of
                {'EXIT', _} ->
                    {{error, unavailable}, State2};
                Reply ->
                    {Reply, State2}
            end;
        Reply ->
            {Reply, State1}
    end.

-spec rebuild_state() -> state().
rebuild_state() ->
    ensure_shard_table(),
    {ShardCount, _Source} = determine_shard_count(),
    Shards = start_shards(ShardCount),
    State = #{shards => Shards, shard_count => ShardCount},
    sync_shard_table(State),
    State.

-spec determine_shard_count() -> {pos_integer(), configured | auto}.
determine_shard_count() ->
    case fluxer_gateway_env:get(session_shards) of
        Value when is_integer(Value), Value > 0 ->
            {Value, configured};
        _ ->
            {default_shard_count(), auto}
    end.

-spec default_shard_count() -> pos_integer().
default_shard_count() ->
    Candidates = [
        erlang:system_info(logical_processors_available),
        erlang:system_info(schedulers_online)
    ],
    lists:max([C || C <- Candidates, is_integer(C), C > 0] ++ [1]).

-spec start_shards(pos_integer()) -> #{non_neg_integer() => shard()}.
start_shards(Count) ->
    lists:foldl(
        fun(Index, Acc) ->
            case start_shard(Index) of
                {ok, Shard} ->
                    maps:put(Index, Shard, Acc);
                {error, _Reason} ->
                    Acc
            end
        end,
        #{},
        lists:seq(0, Count - 1)
    ).

-spec start_shard(non_neg_integer()) -> {ok, shard()} | {error, term()}.
start_shard(Index) ->
    case session_manager_shard:start_link(Index) of
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
            NewState = State#{shards := maps:put(Index, Shard, Shards)},
            sync_shard_table(NewState),
            {Shard, NewState};
        {error, _Reason} ->
            Dummy = #{pid => spawn(fun() -> exit(normal) end), ref => make_ref()},
            {Dummy, State}
    end.

-spec ensure_shard(session_id(), state()) -> {non_neg_integer(), state()}.
ensure_shard(SessionId, State) ->
    Count = maps:get(shard_count, State),
    Shards = maps:get(shards, State),
    Index = select_shard(SessionId, Count),
    case maps:get(Index, Shards, undefined) of
        undefined ->
            {_Shard, NewState} = restart_shard(Index, State),
            {Index, NewState};
        #{pid := Pid} ->
            case erlang:is_process_alive(Pid) of
                true ->
                    {Index, State};
                false ->
                    {_Shard, NewState} = restart_shard(Index, State),
                    {Index, NewState}
            end
    end.

-spec aggregate_counts(term(), state()) -> {non_neg_integer(), state()}.
aggregate_counts(Request, State) ->
    Shards = maps:get(shards, State),
    Counts =
        lists:map(
            fun(#{pid := Pid}) ->
                case catch gen_server:call(Pid, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
                    {ok, Count} when is_integer(Count) ->
                        Count;
                    Count when is_integer(Count) ->
                        Count;
                    _ ->
                        0
                end
            end,
            maps:values(Shards)
        ),
    {lists:sum(Counts), State}.

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
    ets:insert(?SHARD_TABLE, {{shard_pid, Index}, Pid}),
    ok.

-spec shard_pid_from_table(session_id()) -> {ok, pid()} | error.
shard_pid_from_table(SessionId) ->
    try
        case ets:lookup(?SHARD_TABLE, shard_count) of
            [{shard_count, ShardCount}] when is_integer(ShardCount), ShardCount > 0 ->
                Index = select_shard(SessionId, ShardCount),
                case ets:lookup(?SHARD_TABLE, {shard_pid, Index}) of
                    [{{shard_pid, Index}, Pid}] when is_pid(Pid) ->
                        case erlang:is_process_alive(Pid) of
                            true -> {ok, Pid};
                            false -> error
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

-spec select_shard(session_id(), pos_integer()) -> non_neg_integer().
select_shard(SessionId, Count) when Count > 0 ->
    rendezvous_router:select(SessionId, Count).

-spec find_shard_by_ref(reference(), #{non_neg_integer() => shard()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_ref(Ref, Shards) ->
    maps:fold(
        fun
            (_Index, _Shard, {ok, _} = Found) ->
                Found;
            (Index, #{ref := ExistingRef}, not_found) ->
                case ExistingRef =:= Ref of
                    true -> {ok, Index};
                    false -> not_found
                end
        end,
        not_found,
        Shards
    ).

-spec find_shard_by_pid(pid(), #{non_neg_integer() => shard()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_pid(Pid, Shards) ->
    maps:fold(
        fun
            (_Index, _Shard, {ok, _} = Found) ->
                Found;
            (Index, #{pid := ExistingPid}, not_found) ->
                case ExistingPid =:= Pid of
                    true -> {ok, Index};
                    false -> not_found
                end
        end,
        not_found,
        Shards
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

determine_shard_count_configured_test() ->
    with_runtime_config(session_shards, 3, fun() ->
        ?assertMatch({3, configured}, determine_shard_count())
    end).

determine_shard_count_auto_test() ->
    with_runtime_config(session_shards, undefined, fun() ->
        {Count, auto} = determine_shard_count(),
        ?assert(Count >= 1)
    end).

default_shard_count_positive_test() ->
    Count = default_shard_count(),
    ?assert(Count >= 1).

select_shard_deterministic_test() ->
    SessionId = <<"session-abc">>,
    ShardCount = 8,
    Shard1 = select_shard(SessionId, ShardCount),
    Shard2 = select_shard(SessionId, ShardCount),
    ?assertEqual(Shard1, Shard2).

select_shard_in_range_test() ->
    ShardCount = 8,
    lists:foreach(
        fun(N) ->
            SessionId = list_to_binary(integer_to_list(N)),
            Shard = select_shard(SessionId, ShardCount),
            ?assert(Shard >= 0 andalso Shard < ShardCount)
        end,
        lists:seq(1, 100)
    ).

with_runtime_config(Key, Value, Fun) ->
    Original = fluxer_gateway_env:get(Key),
    fluxer_gateway_env:patch(#{Key => Value}),
    Result = Fun(),
    fluxer_gateway_env:update(fun(Map) ->
        case Original of
            undefined -> maps:remove(Key, Map);
            Existing -> maps:put(Key, Existing, Map)
        end
    end),
    Result.

-endif.
