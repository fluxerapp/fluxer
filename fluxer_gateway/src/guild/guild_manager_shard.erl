%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_manager_shard).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type guild_id() :: integer().
-type guild_ref() :: {pid(), reference()}.
-type fetch_result() :: {ok, map()} | {error, term()}.
-type state() :: #{
    guilds := #{guild_id() => guild_ref() | loading},
    pending_requests := #{guild_id() => [gen_server:from()]},
    fetch_workers := #{reference() => {guild_id(), pid(), reference()}},
    shard_index := non_neg_integer()
}.

-spec start_link(non_neg_integer()) -> gen_server:start_ret().
start_link(ShardIndex) ->
    gen_server:start_link(?MODULE, #{shard_index => ShardIndex}, []).

-spec init(map()) -> {ok, state()}.
init(Args) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 0),
    process_registry:init(),
    _ = fluxer_gateway_env:load(),
    ShardIndex = require_shard_index(maps:get(shard_index, Args, 0)),
    {ok, #{
        guilds => #{},
        pending_requests => #{},
        fetch_workers => #{},
        shard_index => ShardIndex
    }}.

-spec handle_call(term(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({start_or_lookup, GuildId}, From, State) ->
    guild_manager_shard_lookup:do_start_or_lookup(require_guild_id(GuildId), From, State);
handle_call({lookup, GuildId}, _From, State) ->
    guild_manager_shard_lookup:do_lookup(require_guild_id(GuildId), State);
handle_call({start_transferred, GuildId, TransferState}, _From, State) ->
    guild_manager_shard_lifecycle:do_start_transferred(
        require_guild_id(GuildId), require_map(TransferState), State
    );
handle_call({stop_guild, GuildId}, _From, State) ->
    guild_manager_shard_lifecycle:do_stop_guild(require_guild_id(GuildId), State);
handle_call({stop_guild, GuildId, Reason}, _From, State) ->
    guild_manager_shard_lifecycle:do_stop_guild(require_guild_id(GuildId), Reason, State);
handle_call({reload_guild, GuildId}, From, State) ->
    guild_manager_shard_reload:do_reload_guild(require_guild_id(GuildId), From, State);
handle_call({reload_all_guilds, GuildIds}, From, State) ->
    guild_manager_shard_reload:do_reload_all_guilds(require_guild_ids(GuildIds), From, State);
handle_call({shutdown_guild, GuildId}, _From, State) ->
    guild_manager_shard_lifecycle:do_shutdown_guild(require_guild_id(GuildId), State);
handle_call(Request, _From, State) ->
    handle_local_call(Request, State).

-spec handle_local_call(term(), state()) -> {reply, term(), state()}.
handle_local_call(get_local_count, State) ->
    Guilds = maps:get(guilds, State),
    {reply, {ok, process_registry:get_count(Guilds)}, State};
handle_local_call(get_local_guild_ids, State) ->
    Guilds = maps:get(guilds, State),
    {reply, {ok, guild_manager_shard_lookup:collect_active_guild_ids(Guilds)}, State};
handle_local_call(get_global_count, State) ->
    Guilds = maps:get(guilds, State),
    {reply, {ok, process_registry:get_count(Guilds)}, State};
handle_local_call(_Unknown, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({guild_data_fetched, GuildId, FetchToken, Result}, State) when
    is_reference(FetchToken)
->
    guild_manager_shard_lookup:handle_guild_data_fetched(
        require_guild_id(GuildId), FetchToken, require_fetch_result(Result), State
    );
handle_cast({guild_data_fetched, GuildId, Result}, State) ->
    guild_manager_shard_lookup:handle_guild_data_fetched(
        require_guild_id(GuildId), require_fetch_result(Result), State
    );
handle_cast(_Unknown, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, process, Pid, Reason}, State) when is_reference(Ref), is_pid(Pid) ->
    case guild_manager_shard_lookup:handle_fetch_worker_down(Ref, Reason, State) of
        {fetch_worker, NewState} ->
            {noreply, NewState};
        not_fetch_worker ->
            Guilds = maps:get(guilds, State),
            {noreply, State#{guilds => cleanup_guilds_on_down(Pid, Guilds)}}
    end;
handle_info(_Unknown, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    drain_pending_requests(State),
    ok.

-spec drain_pending_requests(state()) -> ok.
drain_pending_requests(State) ->
    Pending = maps:get(pending_requests, State, #{}),
    maps:foreach(
        fun(_GuildId, Requests) ->
            lists:foreach(fun safe_reply_shutdown/1, Requests)
        end,
        Pending
    ).

-spec safe_reply_shutdown(gen_server:from()) -> ok.
safe_reply_shutdown(From) ->
    try gen_server:reply(From, {error, shard_shutdown}) of
        _ -> ok
    catch
        _:_ -> ok
    end.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:process_flag(fullsweep_after, 0),
    erlang:garbage_collect(),
    {ok, State}.

-spec require_shard_index(term()) -> non_neg_integer().
require_shard_index(Index) when is_integer(Index), Index >= 0 ->
    Index;
require_shard_index(Index) ->
    erlang:error({bad_shard_index, Index}).

-spec require_guild_id(term()) -> guild_id().
require_guild_id(GuildId) when is_integer(GuildId) ->
    GuildId;
require_guild_id(GuildId) ->
    erlang:error({bad_guild_id, GuildId}).

-spec require_guild_ids(term()) -> [guild_id()].
require_guild_ids(GuildIds) when is_list(GuildIds) ->
    [require_guild_id(GuildId) || GuildId <- GuildIds];
require_guild_ids(GuildIds) ->
    erlang:error({bad_guild_ids, GuildIds}).

-spec require_map(term()) -> map().
require_map(Value) when is_map(Value) ->
    Value;
require_map(Value) ->
    erlang:error({bad_map, Value}).

-spec require_fetch_result(term()) -> fetch_result().
require_fetch_result({ok, Data}) when is_map(Data) ->
    {ok, Data};
require_fetch_result({error, _Reason} = Error) ->
    Error;
require_fetch_result(Result) ->
    erlang:error({bad_fetch_result, Result}).

-spec cleanup_guilds_on_down(pid(), #{guild_id() => guild_ref() | loading}) ->
    #{guild_id() => guild_ref() | loading}.
cleanup_guilds_on_down(Pid, Guilds) ->
    normalize_guilds(process_registry:cleanup_on_down(guild, Pid, Guilds)).

-spec normalize_guilds(process_registry:process_map()) ->
    #{guild_id() => guild_ref() | loading}.
normalize_guilds(Guilds) ->
    maps:fold(fun normalize_guild_entry/3, #{}, Guilds).

-spec normalize_guild_entry(
    term(), {pid(), reference()} | loading, #{guild_id() => guild_ref() | loading}
) ->
    #{guild_id() => guild_ref() | loading}.
normalize_guild_entry(GuildId, loading, Acc) when is_integer(GuildId) ->
    Acc#{GuildId => loading};
normalize_guild_entry(GuildId, {Pid, Ref}, Acc) when
    is_integer(GuildId), is_pid(Pid), is_reference(Ref)
->
    Acc#{GuildId => {Pid, Ref}};
normalize_guild_entry(_GuildId, _Value, Acc) ->
    Acc.
