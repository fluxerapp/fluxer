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

-module(guild_manager_shard).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-define(BATCH_SIZE, 10).
-define(BATCH_DELAY_MS, 100).
-define(GUILD_COLLECTION_FETCH_TIMEOUT_MS, 120000).
-define(GUILD_MEMBER_COLLECTION_LIMIT, 250).
-define(GUILD_COLLECTIONS, [
    <<"guild">>,
    <<"roles">>,
    <<"channels">>,
    <<"emojis">>,
    <<"stickers">>,
    <<"members">>
]).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type guild_id() :: integer().
-type guild_ref() :: {pid(), reference()}.
-type guild_data() :: #{binary() => term()}.
-type fetch_result() :: {ok, guild_data()} | {error, term()}.
-type state() :: #{
    guilds := #{guild_id() => guild_ref() | loading},
    pending_requests := #{guild_id() => [gen_server:from()]},
    shard_index := non_neg_integer()
}.

-spec start_link(non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(ShardIndex) ->
    gen_server:start_link(?MODULE, #{shard_index => ShardIndex}, []).

-spec init(map()) -> {ok, state()}.
init(Args) ->
    process_flag(trap_exit, true),
    fluxer_gateway_env:load(),
    ShardIndex = maps:get(shard_index, Args, 0),
    {ok, #{
        guilds => #{},
        pending_requests => #{},
        shard_index => ShardIndex
    }}.

-spec handle_call(term(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({start_or_lookup, GuildId}, From, State) ->
    do_start_or_lookup(GuildId, From, State);
handle_call({lookup, GuildId}, _From, State) ->
    do_lookup(GuildId, State);
handle_call({ensure_started, GuildId}, _From, State) ->
    do_ensure_started(GuildId, State);
handle_call({stop_guild, GuildId}, _From, State) ->
    do_stop_guild(GuildId, State);
handle_call({reload_guild, GuildId}, From, State) ->
    do_reload_guild(GuildId, From, State);
handle_call({reload_all_guilds, GuildIds}, From, State) ->
    do_reload_all_guilds(GuildIds, From, State);
handle_call({shutdown_guild, GuildId}, _From, State) ->
    do_shutdown_guild(GuildId, State);
handle_call(get_local_count, _From, State) ->
    Guilds = maps:get(guilds, State),
    Count = process_registry:get_count(Guilds),
    {reply, {ok, Count}, State};
handle_call(get_global_count, _From, State) ->
    Guilds = maps:get(guilds, State),
    Count = process_registry:get_count(Guilds),
    {reply, {ok, Count}, State};
handle_call(_Unknown, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({guild_data_fetched, GuildId, Result}, State) ->
    handle_guild_data_fetched(GuildId, Result, State);
handle_cast({guild_data_reloaded, GuildId, Pid, From, Result}, State) ->
    handle_guild_data_reloaded(GuildId, Pid, From, Result, State);
handle_cast({all_guilds_reloaded, From, Count}, State) ->
    gen_server:reply(From, #{count => Count}),
    {noreply, State};
handle_cast(_Unknown, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    Guilds = maps:get(guilds, State),
    NewGuilds = process_registry:cleanup_on_down(Pid, Guilds),
    {noreply, State#{guilds => NewGuilds}};
handle_info(_Unknown, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), term(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) when is_map(State) ->
    {ok, State}.

-spec do_start_or_lookup(guild_id(), gen_server:from(), state()) ->
    {reply, {ok, pid()} | {error, term()}, state()} | {noreply, state()}.
do_start_or_lookup(GuildId, From, State) ->
    Guilds = maps:get(guilds, State),
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, _Ref} ->
            {reply, {ok, Pid}, State};
        loading ->
            add_pending_request(GuildId, From, State);
        undefined ->
            lookup_or_fetch(GuildId, From, State)
    end.

-spec do_lookup(guild_id(), state()) -> {reply, {ok, pid()} | {error, not_found}, state()}.
do_lookup(GuildId, State) ->
    Guilds = maps:get(guilds, State),
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, _Ref} ->
            {reply, {ok, Pid}, State};
        loading ->
            {reply, {error, not_found}, State};
        undefined ->
            GuildName = process_registry:build_process_name(guild, GuildId),
            case process_registry:lookup_or_monitor(GuildName, GuildId, Guilds) of
                {ok, Pid, _Ref, NewGuilds} ->
                    {reply, {ok, Pid}, State#{guilds => NewGuilds}};
                {error, not_found} ->
                    {reply, {error, not_found}, State}
            end
    end.

-spec do_ensure_started(guild_id(), state()) ->
    {reply, ok | {ok, pid()} | {error, term()}, state()}.
do_ensure_started(GuildId, State) ->
    Guilds = maps:get(guilds, State),
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, _Ref} ->
            {reply, {ok, Pid}, State};
        loading ->
            {reply, ok, State};
        undefined ->
            GuildName = process_registry:build_process_name(guild, GuildId),
            case whereis(GuildName) of
                undefined ->
                    {reply, ok, start_fetch_without_pending(GuildId, State)};
                _ExistingPid ->
                    case process_registry:lookup_or_monitor(GuildName, GuildId, Guilds) of
                        {ok, Pid, _Ref, NewGuilds} ->
                            {reply, {ok, Pid}, State#{guilds => NewGuilds}};
                        {error, not_found} ->
                            {reply, {error, process_died}, State}
                    end
            end
    end.

-spec lookup_or_fetch(guild_id(), gen_server:from(), state()) ->
    {reply, {ok, pid()}, state()} | {noreply, state()}.
lookup_or_fetch(GuildId, From, State) ->
    GuildName = process_registry:build_process_name(guild, GuildId),
    case whereis(GuildName) of
        undefined ->
            start_fetch(GuildId, From, State);
        _ExistingPid ->
            Guilds = maps:get(guilds, State),
            case process_registry:lookup_or_monitor(GuildName, GuildId, Guilds) of
                {ok, Pid, _Ref, NewGuilds} ->
                    {reply, {ok, Pid}, State#{guilds => NewGuilds}};
                {error, not_found} ->
                    {reply, {error, process_died}, State}
            end
    end.

-spec start_fetch(guild_id(), gen_server:from(), state()) -> {noreply, state()}.
start_fetch(GuildId, From, State) ->
    Guilds = maps:get(guilds, State),
    NewGuilds = maps:put(GuildId, loading, Guilds),
    Pending = maps:get(pending_requests, State),
    NewPending = maps:put(GuildId, [From], Pending),
    NewState = State#{guilds => NewGuilds, pending_requests => NewPending},
    spawn_fetch(GuildId, State),
    {noreply, NewState}.

-spec start_fetch_without_pending(guild_id(), state()) -> state().
start_fetch_without_pending(GuildId, State) ->
    Guilds = maps:get(guilds, State),
    NewGuilds = maps:put(GuildId, loading, Guilds),
    NewState = State#{guilds => NewGuilds},
    spawn_fetch(GuildId, State),
    NewState.

-spec spawn_fetch(guild_id(), state()) -> pid().
spawn_fetch(GuildId, _State) ->
    Manager = self(),
    spawn(fun() ->
        try
            Result = fetch_guild_data(GuildId),
            gen_server:cast(Manager, {guild_data_fetched, GuildId, Result})
        catch
            _:_:_ ->
                gen_server:cast(Manager, {guild_data_fetched, GuildId, {error, fetch_failed}})
        end
    end).

-spec add_pending_request(guild_id(), gen_server:from(), state()) -> {noreply, state()}.
add_pending_request(GuildId, From, State) ->
    Pending = maps:get(pending_requests, State),
    Requests = maps:get(GuildId, Pending, []),
    NewPending = maps:put(GuildId, [From | Requests], Pending),
    {noreply, State#{pending_requests => NewPending}}.

-spec handle_guild_data_fetched(guild_id(), fetch_result(), state()) -> {noreply, state()}.
handle_guild_data_fetched(GuildId, Result, State) ->
    Pending = maps:get(pending_requests, State),
    Requests = maps:get(GuildId, Pending, []),
    Guilds = maps:get(guilds, State),
    case Result of
        {ok, Data} ->
            case start_guild(GuildId, Data, State) of
                {ok, Pid, NewState} ->
                    reply_to_all(Requests, {ok, Pid}),
                    NewPending = maps:remove(GuildId, Pending),
                    {noreply, NewState#{pending_requests => NewPending}};
                {error, Reason} ->
                    reply_to_all(Requests, {error, Reason}),
                    NewGuilds = maps:remove(GuildId, Guilds),
                    NewPending = maps:remove(GuildId, Pending),
                    {noreply, State#{guilds => NewGuilds, pending_requests => NewPending}}
            end;
        {error, Reason} ->
            reply_to_all(Requests, {error, Reason}),
            NewGuilds = maps:remove(GuildId, Guilds),
            NewPending = maps:remove(GuildId, Pending),
            {noreply, State#{guilds => NewGuilds, pending_requests => NewPending}}
    end.

-spec handle_guild_data_reloaded(guild_id(), pid(), gen_server:from(), fetch_result(), state()) ->
    {noreply, state()}.
handle_guild_data_reloaded(_GuildId, Pid, From, Result, State) ->
    case Result of
        {ok, Data} ->
            catch gen_server:call(Pid, {reload, Data}, ?GUILD_CALL_TIMEOUT),
            gen_server:reply(From, ok);
        _ ->
            gen_server:reply(From, {error, fetch_failed})
    end,
    {noreply, State}.

-spec reply_to_all([gen_server:from()], term()) -> ok.
reply_to_all(Requests, Reply) ->
    lists:foreach(fun(From) -> gen_server:reply(From, Reply) end, Requests).

-spec do_stop_guild(guild_id(), state()) -> {reply, ok, state()}.
do_stop_guild(GuildId, State) ->
    Guilds = maps:get(guilds, State),
    GuildName = process_registry:build_process_name(guild, GuildId),
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, Ref} ->
            demonitor(Ref, [flush]),
            catch gen_server:stop(Pid, normal, ?SHUTDOWN_TIMEOUT),
            process_registry:safe_unregister(GuildName),
            NewGuilds = maps:remove(GuildId, Guilds),
            {reply, ok, State#{guilds => NewGuilds}};
        _ ->
            case whereis(GuildName) of
                undefined ->
                    {reply, ok, State};
                ExistingPid ->
                    catch gen_server:stop(ExistingPid, normal, ?SHUTDOWN_TIMEOUT),
                    process_registry:safe_unregister(GuildName),
                    {reply, ok, State}
            end
    end.

-spec do_reload_guild(guild_id(), gen_server:from(), state()) ->
    {reply, {error, not_found}, state()} | {noreply, state()}.
do_reload_guild(GuildId, From, State) ->
    Guilds = maps:get(guilds, State),
    GuildName = process_registry:build_process_name(guild, GuildId),
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, _Ref} ->
            spawn_reload(GuildId, Pid, From, State),
            {noreply, State};
        _ ->
            case whereis(GuildName) of
                undefined ->
                    {reply, {error, not_found}, State};
                _ExistingPid ->
                    case process_registry:lookup_or_monitor(GuildName, GuildId, Guilds) of
                        {ok, Pid, _Ref, NewGuilds} ->
                            NewState = State#{guilds => NewGuilds},
                            spawn_reload(GuildId, Pid, From, NewState),
                            {noreply, NewState};
                        {error, not_found} ->
                            {reply, {error, not_found}, State}
                    end
            end
    end.

-spec spawn_reload(guild_id(), pid(), gen_server:from(), state()) -> pid().
spawn_reload(GuildId, Pid, From, _State) ->
    Manager = self(),
    spawn(fun() ->
        try
            Result = fetch_guild_data(GuildId),
            gen_server:cast(Manager, {guild_data_reloaded, GuildId, Pid, From, Result})
        catch
            _:_:_ ->
                gen_server:cast(
                    Manager, {guild_data_reloaded, GuildId, Pid, From, {error, fetch_failed}}
                )
        end
    end).

-spec do_reload_all_guilds([guild_id()], gen_server:from(), state()) -> {noreply, state()}.
do_reload_all_guilds(GuildIds, From, State) ->
    Guilds = maps:get(guilds, State),
    GuildsToReload = select_guilds_to_reload(GuildIds, Guilds),
    Manager = self(),
    spawn(fun() ->
        try
            reload_guilds_in_batches(GuildsToReload, State),
            gen_server:cast(Manager, {all_guilds_reloaded, From, length(GuildsToReload)})
        catch
            _:_:_ ->
                gen_server:cast(Manager, {all_guilds_reloaded, From, 0})
        end
    end),
    {noreply, State}.

-spec select_guilds_to_reload([guild_id()], #{guild_id() => guild_ref() | loading}) ->
    [{guild_id(), pid()}].
select_guilds_to_reload([], Guilds) ->
    [{GuildId, Pid} || {GuildId, {Pid, _Ref}} <- maps:to_list(Guilds)];
select_guilds_to_reload(GuildIds, Guilds) ->
    lists:filtermap(
        fun(GuildId) ->
            case maps:get(GuildId, Guilds, undefined) of
                {Pid, _Ref} -> {true, {GuildId, Pid}};
                _ -> false
            end
        end,
        GuildIds
    ).

-spec reload_guilds_in_batches([{guild_id(), pid()}], state()) -> ok.
reload_guilds_in_batches([], _State) ->
    ok;
reload_guilds_in_batches(Guilds, State) ->
    {Batch, Remaining} = lists:split(min(?BATCH_SIZE, length(Guilds)), Guilds),
    reload_batch(Batch, State),
    case Remaining of
        [] ->
            ok;
        _ ->
            timer:sleep(?BATCH_DELAY_MS),
            reload_guilds_in_batches(Remaining, State)
    end.

-spec reload_batch([{guild_id(), pid()}], state()) -> ok.
reload_batch(Batch, _State) ->
    lists:foreach(
        fun({GuildId, Pid}) ->
            spawn(fun() ->
                try
                    case fetch_guild_data(GuildId) of
                        {ok, Data} ->
                            catch gen_server:call(Pid, {reload, Data}, ?GUILD_CALL_TIMEOUT);
                        {error, _Reason} ->
                            ok
                    end
                catch
                    _:_ -> ok
                end
            end)
        end,
        Batch
    ).

-spec do_shutdown_guild(guild_id(), state()) -> {reply, ok, state()}.
do_shutdown_guild(GuildId, State) ->
    Guilds = maps:get(guilds, State),
    GuildName = process_registry:build_process_name(guild, GuildId),
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, Ref} ->
            demonitor(Ref, [flush]),
            catch gen_server:call(Pid, {terminate}, ?SHUTDOWN_TIMEOUT),
            process_registry:safe_unregister(GuildName),
            NewGuilds = maps:remove(GuildId, Guilds),
            {reply, ok, State#{guilds => NewGuilds}};
        _ ->
            case whereis(GuildName) of
                undefined ->
                    {reply, ok, State};
                ExistingPid ->
                    catch gen_server:call(ExistingPid, {terminate}, ?SHUTDOWN_TIMEOUT),
                    process_registry:safe_unregister(GuildName),
                    {reply, ok, State}
            end
    end.

-spec start_guild(guild_id(), guild_data(), state()) -> {ok, pid(), state()} | {error, term()}.
start_guild(GuildId, Data, State) ->
    GuildName = process_registry:build_process_name(guild, GuildId),
    case whereis(GuildName) of
        undefined ->
            start_new_guild(GuildId, Data, GuildName, State);
        _ExistingPid ->
            lookup_existing_guild(GuildId, GuildName, State)
    end.

-spec start_new_guild(guild_id(), guild_data(), atom(), state()) ->
    {ok, pid(), state()} | {error, term()}.
start_new_guild(GuildId, Data, GuildName, State) ->
    GuildState = #{
        id => GuildId,
        data => Data,
        sessions => #{}
    },
    Guilds = maps:get(guilds, State),
    GuildModule =
        case is_very_large_guild(Data) of
            true -> very_large_guild;
            false -> guild
        end,
    case whereis(GuildName) of
        undefined ->
            case GuildModule:start_link(GuildState) of
                {ok, Pid} ->
                    case process_registry:register_and_monitor(GuildName, Pid, Guilds) of
                        {ok, RegisteredPid, Ref, NewGuilds0} ->
                            CleanGuilds = maps:remove(GuildName, NewGuilds0),
                            NewGuilds = maps:put(GuildId, {RegisteredPid, Ref}, CleanGuilds),
                            {ok, RegisteredPid, State#{guilds => NewGuilds}};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                Error ->
                    Error
            end;
        _AlreadyRegistered ->
            lookup_existing_guild(GuildId, GuildName, State)
    end.

-spec is_very_large_guild(guild_data()) -> boolean().
is_very_large_guild(Data) when is_map(Data) ->
    Guild = maps:get(<<"guild">>, Data, #{}),
    Features = maps:get(<<"features">>, Guild, []),
    lists:member(<<"VERY_LARGE_GUILD">>, Features);
is_very_large_guild(_) ->
    false.

-spec lookup_existing_guild(guild_id(), atom(), state()) -> {ok, pid(), state()} | {error, term()}.
lookup_existing_guild(GuildId, GuildName, State) ->
    Guilds = maps:get(guilds, State),
    case process_registry:lookup_or_monitor(GuildName, GuildId, Guilds) of
        {ok, Pid, _Ref, NewGuilds} ->
            {ok, Pid, State#{guilds => NewGuilds}};
        {error, not_found} ->
            {error, process_died}
    end.

-spec fetch_guild_data(guild_id()) -> fetch_result().
fetch_guild_data(GuildId) ->
    Parent = self(),
    Ref = make_ref(),
    _ = [
        spawn_monitor(fun() ->
            Parent ! {Ref, Collection, fetch_guild_collection(GuildId, Collection)}
        end)
     || Collection <- ?GUILD_COLLECTIONS
    ],
    DeadlineMs = erlang:monotonic_time(millisecond) + ?GUILD_COLLECTION_FETCH_TIMEOUT_MS,
    collect_guild_collection_results(Ref, ?GUILD_COLLECTIONS, #{}, DeadlineMs).

-spec collect_guild_collection_results(reference(), [binary()], guild_data(), integer()) ->
    fetch_result().
collect_guild_collection_results(_Ref, [], Acc, _DeadlineMs) ->
    {ok, Acc};
collect_guild_collection_results(Ref, PendingCollections, Acc, DeadlineMs) ->
    NowMs = erlang:monotonic_time(millisecond),
    RemainingMs = DeadlineMs - NowMs,
    case RemainingMs > 0 of
        false ->
            {error, {guild_collection_fetch_timeout, PendingCollections}};
        true ->
            receive
                {Ref, Collection, {ok, Data}} ->
                    Key = guild_collection_result_key(Collection),
                    NewPending = lists:delete(Collection, PendingCollections),
                    NewAcc = maps:put(Key, Data, Acc),
                    collect_guild_collection_results(Ref, NewPending, NewAcc, DeadlineMs);
                {Ref, Collection, {error, Reason}} ->
                    {error, {guild_collection_fetch_failed, Collection, Reason}};
                {'DOWN', _, process, _, _} ->
                    collect_guild_collection_results(Ref, PendingCollections, Acc, DeadlineMs)
            after RemainingMs ->
                {error, {guild_collection_fetch_timeout, PendingCollections}}
            end
    end.

-spec guild_collection_result_key(binary()) -> binary().
guild_collection_result_key(<<"guild">>) -> <<"guild">>;
guild_collection_result_key(<<"roles">>) -> <<"roles">>;
guild_collection_result_key(<<"channels">>) -> <<"channels">>;
guild_collection_result_key(<<"emojis">>) -> <<"emojis">>;
guild_collection_result_key(<<"stickers">>) -> <<"stickers">>;
guild_collection_result_key(<<"members">>) -> <<"members">>;
guild_collection_result_key(Collection) -> Collection.

-spec fetch_guild_collection(guild_id(), binary()) -> {ok, term()} | {error, term()}.
fetch_guild_collection(GuildId, <<"members">>) ->
    fetch_guild_members_collection_stream(GuildId, undefined, []);
fetch_guild_collection(GuildId, Collection) ->
    RpcRequest = #{
        <<"type">> => <<"guild_collection">>,
        <<"guild_id">> => type_conv:to_binary(GuildId),
        <<"collection">> => Collection
    },
    case rpc_client:call(RpcRequest) of
        {ok, Data} ->
            case maps:get(Collection, Data, undefined) of
                undefined -> {error, {invalid_collection_response, Collection}};
                Value -> {ok, Value}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec fetch_guild_members_collection_stream(guild_id(), binary() | undefined, [[map()]]) ->
    {ok, [map()]} | {error, term()}.
fetch_guild_members_collection_stream(GuildId, AfterUserId, ChunksAcc) ->
    RpcRequest0 = #{
        <<"type">> => <<"guild_collection">>,
        <<"guild_id">> => type_conv:to_binary(GuildId),
        <<"collection">> => <<"members">>,
        <<"limit">> => ?GUILD_MEMBER_COLLECTION_LIMIT
    },
    RpcRequest = maybe_put_after_user_id(AfterUserId, RpcRequest0),
    case rpc_client:call(RpcRequest) of
        {ok, Data} ->
            parse_members_collection_page(GuildId, Data, ChunksAcc);
        {error, Reason} ->
            {error, Reason}
    end.

-spec parse_members_collection_page(guild_id(), map(), [[map()]]) -> {ok, [map()]} | {error, term()}.
parse_members_collection_page(GuildId, Data, ChunksAcc) ->
    Members = maps:get(<<"members">>, Data, undefined),
    HasMore = maps:get(<<"has_more">>, Data, false),
    NextAfterUserId = maps:get(<<"next_after_user_id">>, Data, null),
    case Members of
        MemberList when is_list(MemberList) ->
            parse_members_collection_page_result(
                GuildId,
                MemberList,
                HasMore,
                NextAfterUserId,
                ChunksAcc
            );
        _ ->
            {error, invalid_members_collection_payload}
    end.

-spec parse_members_collection_page_result(
    guild_id(),
    [map()],
    term(),
    term(),
    [[map()]]
) ->
    {ok, [map()]} | {error, term()}.
parse_members_collection_page_result(
    GuildId,
    MemberList,
    true,
    NextAfterUserId,
    ChunksAcc
) when is_binary(NextAfterUserId), MemberList =/= [] ->
    fetch_guild_members_collection_stream(
        GuildId,
        NextAfterUserId,
        [MemberList | ChunksAcc]
    );
parse_members_collection_page_result(
    _GuildId,
    [],
    true,
    _NextAfterUserId,
    _ChunksAcc
) ->
    {error, invalid_members_collection_empty_page};
parse_members_collection_page_result(
    _GuildId,
    _MemberList,
    true,
    _NextAfterUserId,
    _ChunksAcc
) ->
    {error, invalid_members_collection_cursor};
parse_members_collection_page_result(
    _GuildId,
    MemberList,
    false,
    _NextAfterUserId,
    ChunksAcc
) ->
    {ok, lists:append(lists:reverse([MemberList | ChunksAcc]))};
parse_members_collection_page_result(
    _GuildId,
    _MemberList,
    _HasMore,
    _NextAfterUserId,
    _ChunksAcc
) ->
    {error, invalid_members_collection_has_more}.

-spec maybe_put_after_user_id(binary() | undefined, map()) -> map().
maybe_put_after_user_id(undefined, RpcRequest) ->
    RpcRequest;
maybe_put_after_user_id(AfterUserId, RpcRequest) when is_binary(AfterUserId) ->
    maps:put(<<"after_user_id">>, AfterUserId, RpcRequest).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_members_collection_page_result_final_page_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}}
    ],
    ?assertEqual(
        {ok, Members},
        parse_members_collection_page_result(42, Members, false, null, [])
    ).

parse_members_collection_page_result_invalid_cursor_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}}
    ],
    ?assertEqual(
        {error, invalid_members_collection_cursor},
        parse_members_collection_page_result(42, Members, true, null, [])
    ).

maybe_put_after_user_id_test() ->
    BaseRequest = #{
        <<"type">> => <<"guild_collection">>,
        <<"collection">> => <<"members">>
    },
    ?assertEqual(BaseRequest, maybe_put_after_user_id(undefined, BaseRequest)),
    WithCursor = maybe_put_after_user_id(<<"100">>, BaseRequest),
    ?assertEqual(<<"100">>, maps:get(<<"after_user_id">>, WithCursor)).

select_guilds_to_reload_empty_ids_test() ->
    Guilds = #{1 => {self(), make_ref()}, 2 => {self(), make_ref()}},
    Result = select_guilds_to_reload([], Guilds),
    ?assertEqual(2, length(Result)).

select_guilds_to_reload_specific_ids_test() ->
    Pid = self(),
    Ref = make_ref(),
    Guilds = #{1 => {Pid, Ref}, 2 => {Pid, Ref}, 3 => loading},
    Result = select_guilds_to_reload([1, 3], Guilds),
    ?assertEqual(1, length(Result)).

reply_to_all_empty_list_test() ->
    ?assertEqual(ok, reply_to_all([], ok)).

do_start_or_lookup_loading_deduplicates_requests_test() ->
    GuildId = 4444,
    From1 = {self(), make_ref()},
    From2 = {self(), make_ref()},
    State0 = #{
        guilds => #{GuildId => loading},
        pending_requests => #{},
        shard_index => 0
    },
    {noreply, State1} = do_start_or_lookup(GuildId, From1, State0),
    Pending1 = maps:get(pending_requests, State1),
    ?assertEqual([From1], maps:get(GuildId, Pending1)),
    {noreply, State2} = do_start_or_lookup(GuildId, From2, State1),
    Pending2 = maps:get(pending_requests, State2),
    Requests = maps:get(GuildId, Pending2),
    ?assertEqual(2, length(Requests)),
    ?assert(lists:member(From1, Requests)),
    ?assert(lists:member(From2, Requests)).

do_lookup_returns_existing_pid_from_state_test() ->
    GuildId = 5151,
    GuildPid = self(),
    State0 = #{
        guilds => #{GuildId => {GuildPid, make_ref()}},
        pending_requests => #{},
        shard_index => 0
    },
    {reply, {ok, GuildPid}, State1} = do_lookup(GuildId, State0),
    ?assertEqual(State0, State1).

do_lookup_returns_not_found_when_loading_test() ->
    GuildId = 6161,
    State0 = #{
        guilds => #{GuildId => loading},
        pending_requests => #{},
        shard_index => 0
    },
    ?assertEqual({reply, {error, not_found}, State0}, do_lookup(GuildId, State0)).

start_new_guild_skips_start_when_already_registered_test() ->
    GuildId = 77777,
    GuildName = process_registry:build_process_name(guild, GuildId),
    ExistingPid = spawn(fun() -> mock_guild_loop() end),
    register(GuildName, ExistingPid),
    try
        State0 = #{
            guilds => #{},
            pending_requests => #{},
            shard_index => 0
        },
        Data = #{<<"guild">> => #{<<"id">> => <<"77777">>, <<"features">> => []}},
        Result = start_new_guild(GuildId, Data, GuildName, State0),
        ?assertMatch({ok, ExistingPid, _}, Result),
        {ok, RetPid, _NewState} = Result,
        ?assertEqual(ExistingPid, RetPid)
    after
        catch unregister(GuildName),
        ExistingPid ! stop
    end.

start_guild_returns_existing_when_registered_test() ->
    GuildId = 88888,
    GuildName = process_registry:build_process_name(guild, GuildId),
    ExistingPid = spawn(fun() -> mock_guild_loop() end),
    register(GuildName, ExistingPid),
    try
        State0 = #{
            guilds => #{},
            pending_requests => #{},
            shard_index => 0
        },
        Data = #{<<"guild">> => #{<<"id">> => <<"88888">>, <<"features">> => []}},
        Result = start_guild(GuildId, Data, State0),
        ?assertMatch({ok, ExistingPid, _}, Result)
    after
        catch unregister(GuildName),
        ExistingPid ! stop
    end.

register_and_monitor_race_kills_duplicate_test_() ->
    {timeout, 15, fun() ->
        GuildId = 66666,
        GuildName = process_registry:build_process_name(guild, GuildId),
        WinnerPid = spawn(fun() -> mock_guild_loop() end),
        register(GuildName, WinnerPid),
        LoserPid = spawn(fun() -> mock_guild_loop() end),
        try
            Guilds = #{},
            Result = process_registry:register_and_monitor(GuildName, LoserPid, Guilds),
            ?assertMatch({ok, WinnerPid, _, _}, Result),
            timer:sleep(200),
            ?assertEqual(false, is_process_alive(LoserPid)),
            ?assert(is_process_alive(WinnerPid))
        after
            catch unregister(GuildName),
            catch (WinnerPid ! stop),
            catch (LoserPid ! stop)
        end
    end}.

mock_guild_loop() ->
    receive
        {'$gen_call', From, _Msg} ->
            gen_server:reply(From, ok),
            mock_guild_loop();
        stop ->
            ok;
        _ ->
            mock_guild_loop()
    end.

-endif.
