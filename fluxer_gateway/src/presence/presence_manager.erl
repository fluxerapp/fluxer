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

-export([start_link/0, lookup/1, dispatch_to_user/3, terminate_all_sessions/1]).
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
    gen_server:call(?MODULE, {lookup, UserId}, ?DEFAULT_GEN_SERVER_TIMEOUT).

-spec terminate_all_sessions(user_id()) -> ok | {error, term()}.
terminate_all_sessions(UserId) ->
    gen_server:call(?MODULE, {terminate_all_sessions, UserId}, ?DEFAULT_GEN_SERVER_TIMEOUT).

-spec dispatch_to_user(user_id(), event_type(), term()) -> ok | {error, not_found}.
dispatch_to_user(UserId, Event, Data) ->
    gen_server:call(?MODULE, {dispatch, UserId, Event, Data}, ?DEFAULT_GEN_SERVER_TIMEOUT).

-spec init(list()) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    {ShardCount, Source} = determine_shard_count(),
    {ShardMap, _} = lists:foldl(
        fun(Index, {Acc, Counter}) ->
            case start_shard(Index) of
                {ok, Shard} ->
                    {maps:put(Index, Shard, Acc), Counter + 1};
                {error, Reason} ->
                    logger:warning("[presence_manager] failed to start shard ~p: ~p", [
                        Index, Reason
                    ]),
                    {Acc, Counter}
            end
        end,
        {#{}, 0},
        lists:seq(0, ShardCount - 1)
    ),
    maybe_log_shard_source(presence_manager, ShardCount, Source),
    {ok, #{shards => ShardMap, shard_count => ShardCount}}.

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
handle_call(Request, _From, State) ->
    logger:warning("[presence_manager] unknown request ~p", [Request]),
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, process, _Pid, Reason}, State) ->
    Shards = maps:get(shards, State),
    case find_shard_by_ref(Ref, Shards) of
        {ok, Index} ->
            logger:warning("[presence_manager] shard ~p crashed: ~p", [Index, Reason]),
            {_ShardEntry, UpdatedState} = restart_shard(Index, State),
            {noreply, UpdatedState};
        not_found ->
            {noreply, State}
    end;
handle_info({'EXIT', Pid, Reason}, State) ->
    Shards = maps:get(shards, State),
    case find_shard_by_pid(Pid, Shards) of
        {ok, Index} ->
            logger:warning("[presence_manager] shard ~p exited: ~p", [Index, Reason]),
            {_ShardEntry, UpdatedState} = restart_shard(Index, State),
            {noreply, UpdatedState};
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
            logger:error("[presence_manager] failed to restart shard ~p: ~p", [Index, Reason]),
            Dummy = #{pid => spawn(fun() -> exit(normal) end), ref => make_ref()},
            {Dummy, State}
    end.

-spec forward_call(user_id(), term(), state()) -> {term(), state()}.
forward_call(Key, Request, State) ->
    {ShardIndex, State1} = ensure_shard(Key, State),
    Shards = maps:get(shards, State1),
    Shard = maps:get(ShardIndex, Shards),
    Pid = maps:get(pid, Shard),
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
    Results =
        [
            begin
                Pid = maps:get(pid, Shard),
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
        #{pid := Pid} when is_pid(Pid) ->
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
    with_runtime_config(presence_shards, 5, fun() ->
        ?assertMatch({5, configured}, determine_shard_count())
    end).

determine_shard_count_auto_test() ->
    with_runtime_config(presence_shards, undefined, fun() ->
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
