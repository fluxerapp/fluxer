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

-export([
    start_link/0,
    start_or_lookup/1,
    start_or_lookup/2,
    lookup/1,
    lookup/2,
    ensure_started/1,
    ensure_started/2
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(GUILD_PID_CACHE, guild_pid_cache).
-define(SHARD_TABLE, guild_manager_shard_table).

-type guild_id() :: integer().
-type shard_map() :: #{pid := pid(), ref := reference()}.
-type state() :: #{
    shards := #{non_neg_integer() => shard_map()},
    shard_count := pos_integer()
}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start_or_lookup(guild_id()) -> {ok, pid()} | {error, term()}.
start_or_lookup(GuildId) ->
    start_or_lookup(GuildId, ?DEFAULT_GEN_SERVER_TIMEOUT).

-spec start_or_lookup(guild_id(), pos_integer()) -> {ok, pid()} | {error, term()}.
start_or_lookup(GuildId, Timeout) ->
    call_shard(GuildId, {start_or_lookup, GuildId}, Timeout).

-spec lookup(guild_id()) -> {ok, pid()} | {error, term()}.
lookup(GuildId) ->
    lookup(GuildId, ?DEFAULT_GEN_SERVER_TIMEOUT).

-spec lookup(guild_id(), pos_integer()) -> {ok, pid()} | {error, term()}.
lookup(GuildId, Timeout) ->
    case lookup_cached_guild_pid(GuildId) of
        {ok, GuildPid} ->
            {ok, GuildPid};
        not_found ->
            call_shard(GuildId, {lookup, GuildId}, Timeout)
    end.

-spec ensure_started(guild_id()) -> ok | {error, term()}.
ensure_started(GuildId) ->
    ensure_started(GuildId, ?DEFAULT_GEN_SERVER_TIMEOUT).

-spec ensure_started(guild_id(), pos_integer()) -> ok | {error, term()}.
ensure_started(GuildId, Timeout) ->
    case call_shard(GuildId, {ensure_started, GuildId}, Timeout) of
        ok ->
            ok;
        {ok, GuildPid} when is_pid(GuildPid) ->
            ets:insert(?GUILD_PID_CACHE, {GuildId, GuildPid}),
            ok;
        {error, _} = Error ->
            Error;
        _ ->
            {error, unavailable}
    end.

-spec init(list()) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    ensure_shard_table(),
    ets:new(?GUILD_PID_CACHE, [named_table, public, set, {read_concurrency, true}]),
    {ShardCount, _Source} = determine_shard_count(),
    ShardMap = start_shards(ShardCount),
    State = #{shards => ShardMap, shard_count => ShardCount},
    sync_shard_table(State),
    {ok, State}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({start_or_lookup, GuildId}, _From, State) ->
    {Reply, NewState} = forward_call(GuildId, {start_or_lookup, GuildId}, State),
    {reply, Reply, NewState};
handle_call({lookup, GuildId}, _From, State) ->
    {Reply, NewState} = forward_call(GuildId, {lookup, GuildId}, State),
    {reply, Reply, NewState};
handle_call({ensure_started, GuildId}, _From, State) ->
    {Reply, NewState} = forward_call(GuildId, {ensure_started, GuildId}, State),
    {reply, Reply, NewState};
handle_call({stop_guild, GuildId}, _From, State) ->
    {Reply, NewState} = forward_call(GuildId, {stop_guild, GuildId}, State),
    {reply, Reply, NewState};
handle_call({reload_guild, GuildId}, _From, State) ->
    {Reply, NewState} = forward_call(GuildId, {reload_guild, GuildId}, State),
    {reply, Reply, NewState};
handle_call({shutdown_guild, GuildId}, _From, State) ->
    {Reply, NewState} = forward_call(GuildId, {shutdown_guild, GuildId}, State),
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
            cleanup_guild_from_cache(Pid),
            {noreply, State}
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
        fun(ShardMap) ->
            Pid = maps:get(pid, ShardMap),
            catch gen_server:stop(Pid, shutdown, 5000)
        end,
        maps:values(Shards)
    ),
    catch ets:delete(?SHARD_TABLE),
    catch ets:delete(?GUILD_PID_CACHE),
    ok.

-spec code_change(term(), term(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) when is_map(State) ->
    sync_shard_table(State),
    {ok, State}.

-spec determine_shard_count() -> {pos_integer(), configured | auto}.
determine_shard_count() ->
    case fluxer_gateway_env:get(guild_shards) of
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
    max(1, lists:max([C || C <- Candidates, is_integer(C), C > 0] ++ [1])).

-spec start_shards(pos_integer()) -> #{non_neg_integer() => shard_map()}.
start_shards(Count) ->
    lists:foldl(
        fun(Index, MapAcc) ->
            case start_shard(Index) of
                {ok, Shard} ->
                    maps:put(Index, Shard, MapAcc);
                {error, _Reason} ->
                    MapAcc
            end
        end,
        #{},
        lists:seq(0, Count - 1)
    ).

-spec start_shard(non_neg_integer()) -> {ok, shard_map()} | {error, term()}.
start_shard(Index) ->
    case guild_manager_shard:start_link(Index) of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            put_shard_pid(Index, Pid),
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
            sync_shard_table(Updated),
            {Shard, Updated};
        {error, _Reason} ->
            clear_shard_pid(Index),
            DummyPid = spawn(fun() -> ok end),
            Dummy = #{pid => DummyPid, ref => make_ref()},
            {Dummy, State}
    end.

-spec call_shard(guild_id(), term(), pos_integer()) -> term().
call_shard(GuildId, Request, Timeout) ->
    case shard_pid_from_table(GuildId) of
        {ok, Pid} ->
            case catch gen_server:call(Pid, Request, Timeout) of
                {'EXIT', {timeout, _}} ->
                    {error, timeout};
                {'EXIT', _} ->
                    call_via_manager(Request, Timeout);
                Reply ->
                    maybe_cache_guild_pid(GuildId, Request, Reply)
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

-spec forward_call(guild_id(), term(), state()) -> {term(), state()}.
forward_call(GuildId, {start_or_lookup, _} = Request, State) ->
    case lookup_cached_guild_pid(GuildId) of
        {ok, GuildPid} ->
            {{ok, GuildPid}, State};
        not_found ->
            forward_call_to_shard(GuildId, Request, State)
    end;
forward_call(GuildId, {lookup, _} = Request, State) ->
    case lookup_cached_guild_pid(GuildId) of
        {ok, GuildPid} ->
            {{ok, GuildPid}, State};
        not_found ->
            forward_call_to_shard(GuildId, Request, State)
    end;
forward_call(GuildId, Request, State) ->
    forward_call_to_shard(GuildId, Request, State).

-spec forward_call_to_shard(guild_id(), term(), state()) -> {term(), state()}.
forward_call_to_shard(GuildId, Request, State) ->
    {Index, State1} = ensure_shard(GuildId, State),
    Shards = maps:get(shards, State1),
    ShardMap = maps:get(Index, Shards),
    Pid = maps:get(pid, ShardMap),
    case catch gen_server:call(Pid, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        {'EXIT', _} ->
            case erlang:is_process_alive(Pid) of
                true ->
                    {{error, timeout}, State1};
                false ->
                    {_Shard, State2} = restart_shard(Index, State1),
                    forward_call_to_shard(GuildId, Request, State2)
            end;
        Reply ->
            {maybe_cache_guild_pid(GuildId, Request, Reply), State1}
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
    Counts = lists:map(
        fun(ShardMap) ->
            Pid = maps:get(pid, ShardMap),
            case catch gen_server:call(Pid, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
                {ok, Count} -> Count;
                _ -> 0
            end
        end,
        maps:values(Shards)
    ),
    {lists:sum(Counts), State}.

-spec handle_reload_all([guild_id()], state()) -> {#{count := non_neg_integer()}, state()}.
handle_reload_all([], State) ->
    Shards = maps:get(shards, State),
    {Replies, FinalState} = lists:foldl(
        fun({_Index, ShardMap}, {AccReplies, AccState}) ->
            Pid = maps:get(pid, ShardMap),
            Reply = catch gen_server:call(Pid, {reload_all_guilds, []}, 15000),
            {[Reply | AccReplies], AccState}
        end,
        {[], State},
        maps:to_list(Shards)
    ),
    Count = lists:sum([maps:get(count, Reply, 0) || Reply <- Replies, is_map(Reply)]),
    {#{count => Count}, FinalState};
handle_reload_all(GuildIds, State) ->
    Count = maps:get(shard_count, State),
    Groups = group_ids_by_shard(GuildIds, Count),
    {TotalCount, FinalState} = lists:foldl(
        fun({Index, Ids}, {AccCount, AccState}) ->
            {ShardIdx, State1} = ensure_shard_for_index(Index, AccState),
            Shards = maps:get(shards, State1),
            ShardMap = maps:get(ShardIdx, Shards),
            Pid = maps:get(pid, ShardMap),
            case catch gen_server:call(Pid, {reload_all_guilds, Ids}, 15000) of
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
    rendezvous_router:group_keys(GuildIds, ShardCount).

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

-spec shard_pid_from_table(guild_id()) -> {ok, pid()} | error.
shard_pid_from_table(GuildId) ->
    try
        case ets:lookup(?SHARD_TABLE, shard_count) of
            [{shard_count, ShardCount}] when is_integer(ShardCount), ShardCount > 0 ->
                Index = select_shard(GuildId, ShardCount),
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

-spec lookup_cached_guild_pid(guild_id()) -> {ok, pid()} | not_found.
lookup_cached_guild_pid(GuildId) ->
    case catch ets:lookup(?GUILD_PID_CACHE, GuildId) of
        [{GuildId, GuildPid}] when is_pid(GuildPid) ->
            case erlang:is_process_alive(GuildPid) of
                true ->
                    {ok, GuildPid};
                false ->
                    ets:delete(?GUILD_PID_CACHE, GuildId),
                    not_found
            end;
        _ ->
            not_found
    end.

-spec maybe_cache_guild_pid(guild_id(), term(), term()) -> term().
maybe_cache_guild_pid(GuildId, {start_or_lookup, GuildId}, {ok, GuildPid} = Reply)
    when is_pid(GuildPid)
->
    ets:insert(?GUILD_PID_CACHE, {GuildId, GuildPid}),
    Reply;
maybe_cache_guild_pid(GuildId, {lookup, GuildId}, {ok, GuildPid} = Reply) when is_pid(GuildPid) ->
    ets:insert(?GUILD_PID_CACHE, {GuildId, GuildPid}),
    Reply;
maybe_cache_guild_pid(_GuildId, _Request, Reply) ->
    Reply.

-spec find_shard_by_ref(reference(), #{non_neg_integer() => shard_map()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_ref(Ref, Shards) ->
    find_shard_by(fun(#{ref := R}) -> R =:= Ref end, Shards).

-spec find_shard_by_pid(pid(), #{non_neg_integer() => shard_map()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_pid(Pid, Shards) ->
    find_shard_by(fun(#{pid := P}) -> P =:= Pid end, Shards).

-spec find_shard_by(fun((shard_map()) -> boolean()), #{non_neg_integer() => shard_map()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by(Pred, Shards) ->
    maps:fold(
        fun
            (_, _, {ok, _} = Found) ->
                Found;
            (Index, ShardMap, not_found) ->
                case Pred(ShardMap) of
                    true -> {ok, Index};
                    false -> not_found
                end
        end,
        not_found,
        Shards
    ).

-spec cleanup_guild_from_cache(pid()) -> ok.
cleanup_guild_from_cache(Pid) ->
    case ets:match_object(?GUILD_PID_CACHE, {'$1', Pid}) of
        [{GuildId, _Pid}] ->
            ets:delete(?GUILD_PID_CACHE, GuildId);
        [] ->
            ok
    end,
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

default_shard_count_positive_test() ->
    Count = default_shard_count(),
    ?assert(Count >= 1).

select_shard_deterministic_test() ->
    GuildId = 12345,
    ShardCount = 8,
    Shard1 = select_shard(GuildId, ShardCount),
    Shard2 = select_shard(GuildId, ShardCount),
    ?assertEqual(Shard1, Shard2).

select_shard_in_range_test() ->
    ShardCount = 8,
    lists:foreach(
        fun(GuildId) ->
            Shard = select_shard(GuildId, ShardCount),
            ?assert(Shard >= 0 andalso Shard < ShardCount)
        end,
        lists:seq(1, 100)
    ).

group_ids_by_shard_test() ->
    GuildIds = [1, 2, 3, 4, 5],
    ShardCount = 2,
    Groups = group_ids_by_shard(GuildIds, ShardCount),
    AllIds = lists:flatten([Ids || {_, Ids} <- Groups]),
    ?assertEqual(lists:sort(GuildIds), lists:sort(AllIds)).

find_shard_by_ref_found_test() ->
    Ref = make_ref(),
    Shards = #{0 => #{pid => self(), ref => Ref}},
    ?assertMatch({ok, 0}, find_shard_by_ref(Ref, Shards)).

find_shard_by_ref_not_found_test() ->
    Shards = #{0 => #{pid => self(), ref => make_ref()}},
    ?assertEqual(not_found, find_shard_by_ref(make_ref(), Shards)).

find_shard_by_pid_found_test() ->
    Pid = self(),
    Shards = #{0 => #{pid => Pid, ref => make_ref()}},
    ?assertMatch({ok, 0}, find_shard_by_pid(Pid, Shards)).

forward_call_to_shard_timeout_does_not_restart_shard_test_() ->
    {timeout, 15, fun() ->
        catch ets:delete(guild_pid_cache),
        SlowShardPid = spawn(fun() -> slow_shard_loop() end),
        ShardRef = erlang:monitor(process, SlowShardPid),
        State = #{
            shards => #{0 => #{pid => SlowShardPid, ref => ShardRef}},
            shard_count => 1
        },
        ets:new(guild_pid_cache, [named_table, public, set, {read_concurrency, true}]),
        try
            GuildId = 99999,
            {Reply, NewState} = forward_call_to_shard(GuildId, {start_or_lookup, GuildId}, State),
            ?assertMatch({error, timeout}, Reply),
            ?assert(is_process_alive(SlowShardPid)),
            NewShards = maps:get(shards, NewState),
            #{pid := ShardPidAfter} = maps:get(0, NewShards),
            ?assertEqual(SlowShardPid, ShardPidAfter)
        after
            SlowShardPid ! stop,
            catch ets:delete(guild_pid_cache)
        end
    end}.

slow_shard_loop() ->
    receive
        {'$gen_call', _From, _Msg} ->
            timer:sleep(10000),
            slow_shard_loop();
        stop ->
            ok;
        _ ->
            slow_shard_loop()
    end.

cleanup_guild_from_cache_does_not_remove_new_pid_test() ->
    catch ets:delete(guild_pid_cache),
    ets:new(guild_pid_cache, [named_table, public, set, {read_concurrency, true}]),
    try
        OldPid = spawn(fun() -> ok end),
        timer:sleep(10),
        NewPid = spawn(fun() -> timer:sleep(1000) end),
        ets:insert(guild_pid_cache, {42, NewPid}),
        cleanup_guild_from_cache(OldPid),
        [{42, FoundPid}] = ets:lookup(guild_pid_cache, 42),
        ?assertEqual(NewPid, FoundPid)
    after
        catch ets:delete(guild_pid_cache)
    end.

start_or_lookup_uses_shard_table_without_manager_test_() ->
    {timeout, 10, fun() ->
        catch ets:delete(guild_pid_cache),
        catch ets:delete(guild_manager_shard_table),
        ets:new(guild_pid_cache, [named_table, public, set, {read_concurrency, true}]),
        ets:new(guild_manager_shard_table, [named_table, public, set, {read_concurrency, true}]),
        GuildId = 101,
        GuildPid = spawn(fun() -> timer:sleep(1000) end),
        ShardPid = spawn(fun() -> shard_stub_loop(GuildId, GuildPid) end),
        ets:insert(guild_manager_shard_table, {shard_count, 1}),
        ets:insert(guild_manager_shard_table, {{shard_pid, 0}, ShardPid}),
        try
            ?assertEqual({ok, GuildPid}, start_or_lookup(GuildId))
        after
            ShardPid ! stop,
            catch ets:delete(guild_manager_shard_table),
            catch ets:delete(guild_pid_cache)
        end
    end}.

call_shard_timeout_returns_error_timeout_test_() ->
    {timeout, 10, fun() ->
        catch ets:delete(guild_pid_cache),
        catch ets:delete(guild_manager_shard_table),
        ets:new(guild_pid_cache, [named_table, public, set, {read_concurrency, true}]),
        ets:new(guild_manager_shard_table, [named_table, public, set, {read_concurrency, true}]),
        GuildId = 202,
        SlowShardPid = spawn(fun() -> slow_shard_loop() end),
        ets:insert(guild_manager_shard_table, {shard_count, 1}),
        ets:insert(guild_manager_shard_table, {{shard_pid, 0}, SlowShardPid}),
        try
            ?assertEqual({error, timeout}, call_shard(GuildId, {start_or_lookup, GuildId}, 20))
        after
            SlowShardPid ! stop,
            catch ets:delete(guild_manager_shard_table),
            catch ets:delete(guild_pid_cache)
        end
    end}.

shard_stub_loop(GuildId, GuildPid) ->
    receive
        stop ->
            ok;
        {'$gen_call', From, {start_or_lookup, GuildId}} ->
            gen_server:reply(From, {ok, GuildPid}),
            shard_stub_loop(GuildId, GuildPid);
        {'$gen_call', From, {lookup, GuildId}} ->
            gen_server:reply(From, {ok, GuildPid}),
            shard_stub_loop(GuildId, GuildPid);
        {'$gen_call', From, _Request} ->
            gen_server:reply(From, {error, unsupported}),
            shard_stub_loop(GuildId, GuildPid);
        _ ->
            shard_stub_loop(GuildId, GuildPid)
    end.

-endif.
