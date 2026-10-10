%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_bus).
-typing([eqwalizer]).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([
    start_link/0, subscribe/1, unsubscribe/1, publish/2, diagnostic_info/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([select_shard/2, find_shard_by_ref/2, find_shard_by_pid/2]).

-type shard() :: #{pid := pid(), ref := reference()}.
-type state() :: #{
    shards := #{non_neg_integer() => shard()}, shard_count := pos_integer(), _ => _
}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    normalize_start_link(gen_server:start_link({local, ?MODULE}, ?MODULE, [], [])).

-spec subscribe(integer()) -> ok.
subscribe(UserId) when is_integer(UserId) ->
    safe_call({subscribe, UserId, self()}).

-spec unsubscribe(integer()) -> ok.
unsubscribe(UserId) when is_integer(UserId) ->
    safe_call({unsubscribe, UserId, self()}).

-spec publish(integer(), term()) -> ok.
publish(UserId, Payload) when is_integer(UserId) ->
    safe_call({publish, UserId, Payload}).

-spec diagnostic_info() -> #{atom() => non_neg_integer()}.
diagnostic_info() ->
    case safe_gen_server_call(?MODULE, diagnostic_info, 5000) of
        Info when is_map(Info) -> sanitize_diagnostic_info(Info);
        _ -> #{}
    end.

-spec init(list()) -> {ok, state(), hibernate}.
init([]) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 10),
    {ShardCount, _Source} = determine_shard_count(presence_bus_shards),
    Shards = start_shards(ShardCount, #{}),
    {ok, #{shards => Shards, shard_count => ShardCount}, hibernate}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({subscribe, UserId, Pid}, _From, State) when is_integer(UserId), is_pid(Pid) ->
    reply_forward(UserId, {subscribe, UserId, Pid}, State);
handle_call({unsubscribe, UserId, Pid}, _From, State) when is_integer(UserId), is_pid(Pid) ->
    reply_forward(UserId, {unsubscribe, UserId, Pid}, State);
handle_call({publish, UserId, Payload}, _From, State) when is_integer(UserId) ->
    reply_forward(UserId, {publish, UserId, Payload}, State);
handle_call(diagnostic_info, _From, State) ->
    {reply, collect_diagnostic_info(State), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec reply_forward(term(), term(), state()) -> {reply, term(), state()}.
reply_forward(Key, Request, State) ->
    {Reply, NewState} = forward_call(Key, Request, State),
    {reply, Reply, NewState}.

-spec collect_diagnostic_info(state()) -> map().
collect_diagnostic_info(State) ->
    Shards = maps:get(shards, State),
    ShardCount = maps:get(shard_count, State),
    maps:fold(
        fun(_Index, Shard, Acc) ->
            merge_shard_diagnostic(maps:get(pid, Shard), Acc)
        end,
        #{shard_count => ShardCount},
        Shards
    ).

-spec merge_shard_diagnostic(pid(), map()) -> map().
merge_shard_diagnostic(Pid, Acc) ->
    case safe_gen_server_call(Pid, diagnostic_info, 2000) of
        ShardInfo when is_map(ShardInfo) ->
            maps:merge(Acc, sanitize_diagnostic_info(ShardInfo));
        _ ->
            Acc
    end.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) when is_reference(Ref) ->
    {noreply, maybe_restart_shard(find_shard_by_ref(Ref, maps:get(shards, State)), State)};
handle_info({'EXIT', Pid, _Reason}, State) when is_pid(Pid) ->
    {noreply, maybe_restart_shard(find_shard_by_pid(Pid, maps:get(shards, State)), State)};
handle_info(_Info, State) ->
    {noreply, State}.

-spec maybe_restart_shard({ok, non_neg_integer()} | not_found, state()) -> state().
maybe_restart_shard({ok, Index}, State) ->
    {_Shard, NewState} = restart_shard(Index, State),
    NewState;
maybe_restart_shard(not_found, State) ->
    State.

-spec safe_call(term()) -> ok.
safe_call(Request) ->
    _ = safe_gen_server_call(?MODULE, Request, ?DEFAULT_GEN_SERVER_TIMEOUT),
    ok.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    Shards = maps:get(shards, State),
    lists:foreach(
        fun(Shard) ->
            Pid = maps:get(pid, Shard),
            safe_gen_server_stop(Pid)
        end,
        maps:values(Shards)
    ),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

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
    shard_utils:max_positive([
        erlang:system_info(logical_processors_available),
        erlang:system_info(schedulers_online)
    ]).

-spec start_shards(pos_integer(), #{}) -> #{non_neg_integer() => shard()}.
start_shards(Count, Acc) ->
    lists:foldl(fun start_shard_acc/2, Acc, lists:seq(0, Count - 1)).

-spec start_shard_acc(non_neg_integer(), #{non_neg_integer() => shard()}) ->
    #{non_neg_integer() => shard()}.
start_shard_acc(Index, MapAcc) ->
    case start_shard(Index) of
        {ok, Shard} -> MapAcc#{Index => Shard};
        {error, _Reason} -> MapAcc
    end.

-spec start_shard(non_neg_integer()) -> {ok, shard()} | {error, term()}.
start_shard(Index) ->
    case presence_bus_shard:start_link(Index) of
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
            Updated = State#{shards := Shards#{Index => Shard}},
            {Shard, Updated};
        {error, _Reason} ->
            Dummy = #{pid => spawn(fun dummy_shard/0), ref => make_ref()},
            {Dummy, State}
    end.

-spec dummy_shard() -> ok.
dummy_shard() ->
    receive
        stop -> ok
    after infinity ->
        ok
    end.

-spec forward_call(term(), term(), state()) -> {term(), state()}.
forward_call(Key, Request, State) ->
    {Index, State1} = ensure_shard(Key, State),
    call_shard(Index, Request, State1).

-spec call_shard(non_neg_integer(), term(), state()) -> {term(), state()}.
call_shard(Index, Request, State) ->
    Shards = maps:get(shards, State),
    Shard = maps:get(Index, Shards),
    Pid = maps:get(pid, Shard),
    case safe_gen_server_call(Pid, Request, ?DEFAULT_GEN_SERVER_TIMEOUT) of
        {gen_server_call_failed, _Class, _Reason} ->
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
            restart_shard_index(Index, State);
        #{pid := Pid} when is_pid(Pid) ->
            ensure_shard_alive(Index, Pid, State)
    end.

-spec ensure_shard_alive(non_neg_integer(), pid(), state()) -> {non_neg_integer(), state()}.
ensure_shard_alive(Index, Pid, State) ->
    case process_liveness:is_alive(Pid) of
        true -> {Index, State};
        false -> restart_shard_index(Index, State)
    end.

-spec restart_shard_index(non_neg_integer(), state()) -> {non_neg_integer(), state()}.
restart_shard_index(Index, State) ->
    {_Shard, NewState} = restart_shard(Index, State),
    {Index, NewState}.

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

-spec safe_gen_server_call(pid() | atom(), term(), timeout()) -> term().
safe_gen_server_call(Server, Request, Timeout) ->
    try gen_server:call(Server, Request, Timeout) of
        Reply -> Reply
    catch
        error:Reason -> {gen_server_call_failed, error, Reason};
        exit:Reason -> {gen_server_call_failed, exit, Reason}
    end.

-spec safe_gen_server_stop(pid()) -> ok.
safe_gen_server_stop(Pid) ->
    try gen_server:stop(Pid, shutdown, 5000) of
        _ -> ok
    catch
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec sanitize_diagnostic_info(map()) -> #{atom() => non_neg_integer()}.
sanitize_diagnostic_info(Info) ->
    maps:fold(
        fun
            (Key, Value, Acc) when is_atom(Key), is_integer(Value), Value >= 0 ->
                Acc#{Key => Value};
            (_Key, _Value, Acc) ->
                Acc
        end,
        #{},
        Info
    ).

-spec normalize_start_link(gen_server:start_ret()) -> {ok, pid()} | {error, term()}.
normalize_start_link({ok, Pid}) ->
    {ok, Pid};
normalize_start_link({error, Reason}) ->
    {error, Reason};
normalize_start_link(ignore) ->
    {error, ignore}.
