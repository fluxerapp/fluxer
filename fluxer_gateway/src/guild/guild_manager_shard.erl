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

-define(GUILD_API_CANARY_PERCENTAGE, 5).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type guild_id() :: integer().
-type guild_ref() :: {pid(), reference()}.
-type guild_data() :: #{binary() => term()}.
-type fetch_result() :: {ok, guild_data()} | {error, term()}.
-type state() :: #{
    guilds => #{guild_id() => guild_ref() | loading},
    api_host => string(),
    api_canary_host => undefined | string(),
    pending_requests => #{guild_id() => [gen_server:from()]}
}.

-record(state, {
    guilds = #{} :: #{guild_id() => guild_ref() | loading},
    api_host :: string(),
    api_canary_host :: undefined | string(),
    pending_requests = #{} :: #{guild_id() => [gen_server:from()]}
}).

-spec start_link(non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(ShardIndex) ->
    gen_server:start_link(?MODULE, #{shard_index => ShardIndex}, []).

-spec init(map()) -> {ok, state()}.
init(_Args) ->
    process_flag(trap_exit, true),
    fluxer_gateway_env:load(),
    ApiHost = fluxer_gateway_env:get(api_host),
    ApiCanaryHost = fluxer_gateway_env:get(api_canary_host),
    {ok, #{
        guilds => #{},
        api_host => ApiHost,
        api_canary_host => ApiCanaryHost,
        pending_requests => #{}
    }}.

-spec handle_call(Request, From, State) -> Result when
    Request ::
        {start_or_lookup, guild_id()}
        | {stop_guild, guild_id()}
        | {reload_guild, guild_id()}
        | {reload_all_guilds, [guild_id()]}
        | {shutdown_guild, guild_id()}
        | get_local_count
        | get_global_count
        | term(),
    From :: gen_server:from(),
    State :: state(),
    Result ::
        {reply, Reply, state()}
        | {noreply, state()},
    Reply ::
        {ok, pid()}
        | {error, term()}
        | ok
        | {ok, non_neg_integer()}.
handle_call({start_or_lookup, GuildId}, From, State) ->
    do_start_or_lookup(GuildId, From, State);
handle_call({stop_guild, GuildId}, _From, State) ->
    do_stop_guild(GuildId, State);
handle_call({reload_guild, GuildId}, From, State) ->
    do_reload_guild(GuildId, From, State);
handle_call({reload_all_guilds, GuildIds}, From, State) ->
    Guilds = maps:get(guilds, State),
    GuildsToReload =
        case GuildIds of
            [] ->
                [{GuildId, Pid} || {GuildId, {Pid, _Ref}} <- maps:to_list(Guilds)];
            Ids ->
                lists:filtermap(
                    fun(GuildId) ->
                        case maps:get(GuildId, Guilds, undefined) of
                            {Pid, _Ref} -> {true, {GuildId, Pid}};
                            _ -> false
                        end
                    end,
                    Ids
                )
        end,
    Manager = self(),
    spawn(fun() ->
        try
            reload_guilds_in_batches(GuildsToReload, Manager, State, 10, 100),
            gen_server:cast(Manager, {all_guilds_reloaded, From, length(GuildsToReload)})
        catch
            Class:Error:Stacktrace ->
                logger:error(
                    "[guild_manager] Spawned process failed: ~p:~p~n~p",
                    [Class, Error, Stacktrace]
                ),
                gen_server:cast(Manager, {all_guilds_reloaded, From, 0})
        end
    end),
    {noreply, State};
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

-spec handle_cast(Request, State) -> {noreply, state()} when
    Request ::
        {guild_data_fetched, guild_id(), fetch_result()}
        | {guild_data_reloaded, guild_id(), pid(), gen_server:from(), fetch_result()}
        | {all_guilds_reloaded, gen_server:from(), non_neg_integer()}
        | term(),
    State :: state().
handle_cast({guild_data_fetched, GuildId, Result}, State) ->
    Pending = maps:get(pending_requests, State),
    Requests = maps:get(GuildId, Pending, []),
    Guilds = maps:get(guilds, State),
    case Result of
        {ok, Data} ->
            case start_guild(GuildId, Data, State) of
                {ok, Pid, NewState} ->
                    lists:foreach(fun(From) -> gen_server:reply(From, {ok, Pid}) end, Requests),
                    NewPending = maps:remove(GuildId, Pending),
                    NewGuilds = maps:get(guilds, NewState),
                    CleanGuilds = maps:remove(GuildId, NewGuilds),
                    {noreply, NewState#{pending_requests => NewPending, guilds => CleanGuilds}};
                {error, Reason} ->
                    logger:error("[guild_manager] Failed to start guild ~p: ~p", [GuildId, Reason]),
                    lists:foreach(
                        fun(From) -> gen_server:reply(From, {error, Reason}) end, Requests
                    ),
                    NewGuilds = maps:remove(GuildId, Guilds),
                    NewPending = maps:remove(GuildId, Pending),
                    {noreply, State#{guilds => NewGuilds, pending_requests => NewPending}}
            end;
        _ ->
            lists:foreach(fun(From) -> gen_server:reply(From, {error, not_found}) end, Requests),
            NewGuilds = maps:remove(GuildId, Guilds),
            NewPending = maps:remove(GuildId, Pending),
            {noreply, State#{guilds => NewGuilds, pending_requests => NewPending}}
    end;
handle_cast({guild_data_reloaded, _GuildId, Pid, From, Result}, State) ->
    case Result of
        {ok, Data} ->
            gen_server:call(Pid, {reload, Data}, ?GUILD_CALL_TIMEOUT),
            gen_server:reply(From, ok),
            {noreply, State};
        _ ->
            gen_server:reply(From, {error, fetch_failed}),
            {noreply, State}
    end;
handle_cast({all_guilds_reloaded, From, Count}, State) ->
    gen_server:reply(From, #{count => Count}),
    {noreply, State};
handle_cast(_Unknown, State) ->
    {noreply, State}.

-spec handle_info(Info, State) -> {noreply, state()} when
    Info :: {'DOWN', reference(), process, pid(), term()} | term(),
    State :: state().
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    Guilds = maps:get(guilds, State),
    NewGuilds = process_registry:cleanup_on_down(Pid, Guilds),
    {noreply, State#{guilds => NewGuilds}};
handle_info(_Unknown, State) ->
    {noreply, State}.

-spec terminate(Reason, State) -> ok when
    Reason :: term(),
    State :: state().
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), term(), term()) -> {ok, state()}.
code_change(_OldVsn, #state{guilds = Guilds, api_host = ApiHost, api_canary_host = ApiCanaryHost, pending_requests = Pending}, _Extra) ->
    {ok, #{
        guilds => Guilds,
        api_host => ApiHost,
        api_canary_host => ApiCanaryHost,
        pending_requests => Pending
    }};
code_change(_OldVsn, State, _Extra) when is_map(State) ->
    {ok, State}.

-spec fetch_guild_data(guild_id(), string()) -> fetch_result().
fetch_guild_data(GuildId, ApiHost) ->
    RpcRequest = #{
        <<"type">> => <<"guild">>,
        <<"guild_id">> => type_conv:to_binary(GuildId),
        <<"version">> => 1
    },
    Url = rpc_client:get_rpc_url(ApiHost),
    Headers =
        rpc_client:get_rpc_headers() ++ [{<<"content-type">>, <<"application/json">>}],
    Body = jsx:encode(RpcRequest),
    case
        hackney:request(post, Url, Headers, Body, [{recv_timeout, 30000}, {connect_timeout, 5000}])
    of
        {ok, 200, _RespHeaders, ClientRef} ->
            case hackney:body(ClientRef) of
                {ok, RespBody} ->
                    hackney:close(ClientRef),
                    Response = jsx:decode(RespBody, [return_maps]),
                    Data = maps:get(<<"data">>, Response, #{}),
                    {ok, Data};
                {error, BodyReason} ->
                    hackney:close(ClientRef),
                    logger:error("[guild_manager] Failed to read guild response body: ~p", [
                        BodyReason
                    ]),
                    {error, fetch_failed}
            end;
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            ErrorBody =
                case hackney:body(ClientRef) of
                    {ok, Body2} -> Body2;
                    {error, _} -> <<"<unable to read error body>">>
                end,
            hackney:close(ClientRef),
            logger:error(
                "[guild_manager] Guild RPC failed with status ~p: ~s",
                [StatusCode, ErrorBody]
            ),
            {error, fetch_failed};
        {error, Reason} ->
            logger:error("[guild_manager] Guild RPC request failed: ~p", [Reason]),
            {error, fetch_failed}
    end.

-spec select_api_host(state()) -> {string(), boolean()}.
select_api_host(State) ->
    case maps:get(api_canary_host, State) of
        undefined ->
            {maps:get(api_host, State), false};
        _ ->
            case should_use_canary_api() of
                true -> {maps:get(api_canary_host, State), true};
                false -> {maps:get(api_host, State), false}
            end
    end.

-spec should_use_canary_api() -> boolean().
should_use_canary_api() ->
    erlang:unique_integer([positive]) rem 100 < ?GUILD_API_CANARY_PERCENTAGE.

-spec fetch_guild_data_with_fallback(
    guild_id(),
    {string(), boolean()},
    state()
) -> fetch_result().
fetch_guild_data_with_fallback(GuildId, {ApiHost, false}, _) ->
    fetch_guild_data(GuildId, ApiHost);
fetch_guild_data_with_fallback(GuildId, {ApiHost, true}, State) ->
    case fetch_guild_data(GuildId, ApiHost) of
        {ok, Data} ->
            {ok, Data};
        Error ->
            StableHost = maps:get(api_host, State),
            case StableHost == ApiHost of
                true ->
                    Error;
                false ->
                    logger:warning(
                        "[guild_manager] Canary API request failed for ~p, retrying against stable host",
                        [GuildId]
                    ),
                    fetch_guild_data(GuildId, StableHost)
            end
    end.

-spec start_guild(guild_id(), guild_data(), state()) -> {ok, pid(), state()} | {error, term()}.
start_guild(GuildId, Data, State) ->
    GuildName = process_registry:build_process_name(guild, GuildId),
    case whereis(GuildName) of
        undefined ->
            GuildState = #{
                id => GuildId,
                data => Data,
                sessions => #{},
                presences => #{}
            },
            Guilds = maps:get(guilds, State),
            case guild:start_link(GuildState) of
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
        _ExistingPid ->
            Guilds = maps:get(guilds, State),
            case process_registry:lookup_or_monitor(GuildName, GuildId, Guilds) of
                {ok, Pid, _Ref, NewGuilds} ->
                    {ok, Pid, State#{guilds => NewGuilds}};
                {error, not_found} ->
                    {error, process_died}
            end
    end.

-spec reload_guilds_in_batches(
    [{guild_id(), pid()}],
    pid(),
    state(),
    pos_integer(),
    non_neg_integer()
) -> ok.
reload_guilds_in_batches([], _Manager, _State, _BatchSize, _DelayMs) ->
    ok;
reload_guilds_in_batches(Guilds, Manager, State, BatchSize, DelayMs) ->
    {Batch, Remaining} = lists:split(min(BatchSize, length(Guilds)), Guilds),
    lists:foreach(
        fun({GuildId, Pid}) ->
            ApiHostInfo = select_api_host(State),
            spawn(fun() ->
                try
                    case fetch_guild_data_with_fallback(GuildId, ApiHostInfo, State) of
                        {ok, Data} ->
                            gen_server:call(Pid, {reload, Data}, ?GUILD_CALL_TIMEOUT);
                        {error, Reason} ->
                            logger:error("[guild_manager] Failed to reload guild ~p: ~p", [
                                GuildId, Reason
                            ])
                    end
                catch
                    Class:Error:Stacktrace ->
                        logger:error(
                            "[guild_manager] Spawned process failed: ~p:~p~n~p",
                            [Class, Error, Stacktrace]
                        )
                end
            end)
        end,
        Batch
    ),
    case Remaining of
        [] ->
            ok;
        _ ->
            timer:sleep(DelayMs),
            reload_guilds_in_batches(Remaining, Manager, State, BatchSize, DelayMs)
    end.

-spec do_start_or_lookup(guild_id(), gen_server:from(), state()) ->
    {reply, {ok, pid()} | {error, term()}, state()} | {noreply, state()}.
do_start_or_lookup(GuildId, From, State) ->
    Guilds = maps:get(guilds, State),
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, _Ref} ->
            {reply, {ok, Pid}, State};
        loading ->
            Pending = maps:get(pending_requests, State),
            Requests = maps:get(GuildId, Pending, []),
            NewPending = maps:put(GuildId, [From | Requests], Pending),
            {noreply, State#{pending_requests => NewPending}};
        undefined ->
            GuildName = process_registry:build_process_name(guild, GuildId),
            case whereis(GuildName) of
                undefined ->
                    NewGuilds = maps:put(GuildId, loading, Guilds),
                    Pending = maps:get(pending_requests, State),
                    NewPending = maps:put(GuildId, [From], Pending),
                    NewState = State#{guilds => NewGuilds, pending_requests => NewPending},
                    Manager = self(),
                    ApiHostInfo = select_api_host(State),
                    spawn(fun() ->
                        try
                            Result = fetch_guild_data_with_fallback(GuildId, ApiHostInfo, State),
                            gen_server:cast(Manager, {guild_data_fetched, GuildId, Result})
                        catch
                            Class:Error:Stacktrace ->
                                logger:error(
                                    "[guild_manager] Spawned process failed: ~p:~p~n~p",
                                    [Class, Error, Stacktrace]
                                ),
                                gen_server:cast(
                                    Manager, {guild_data_fetched, GuildId, {error, fetch_failed}}
                                )
                        end
                    end),
                    {noreply, NewState};
                _ExistingPid ->
                    case process_registry:lookup_or_monitor(GuildName, GuildId, Guilds) of
                        {ok, Pid, _Ref, NewGuilds} ->
                            {reply, {ok, Pid}, State#{guilds => NewGuilds}};
                        {error, not_found} ->
                            {reply, {error, process_died}, State}
                    end
            end
    end.

-spec do_stop_guild(guild_id(), state()) -> {reply, ok, state()}.
do_stop_guild(GuildId, State) ->
    Guilds = maps:get(guilds, State),
    GuildName = process_registry:build_process_name(guild, GuildId),
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, Ref} ->
            demonitor(Ref, [flush]),
            gen_server:stop(Pid, normal, ?SHUTDOWN_TIMEOUT),
            process_registry:safe_unregister(GuildName),
            NewGuilds = maps:remove(GuildId, Guilds),
            {reply, ok, State#{guilds => NewGuilds}};
        _ ->
            case whereis(GuildName) of
                undefined ->
                    {reply, ok, State};
                ExistingPid ->
                    gen_server:stop(ExistingPid, normal, ?SHUTDOWN_TIMEOUT),
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
            Manager = self(),
            ApiHostInfo = select_api_host(State),
            spawn(fun() ->
                try
                    Result = fetch_guild_data_with_fallback(GuildId, ApiHostInfo, State),
                    gen_server:cast(Manager, {guild_data_reloaded, GuildId, Pid, From, Result})
                catch
                    Class:Error:Stacktrace ->
                        logger:error(
                            "[guild_manager] Spawned process failed: ~p:~p~n~p",
                            [Class, Error, Stacktrace]
                        ),
                        gen_server:cast(
                            Manager,
                            {guild_data_reloaded, GuildId, Pid, From, {error, fetch_failed}}
                        )
                end
            end),
            {noreply, State};
        _ ->
            case whereis(GuildName) of
                undefined ->
                    {reply, {error, not_found}, State};
                _ExistingPid ->
                    case process_registry:lookup_or_monitor(GuildName, GuildId, Guilds) of
                        {ok, Pid, _Ref, NewGuilds} ->
                            NewState = State#{guilds => NewGuilds},
                            Manager = self(),
                            ApiHostInfo = select_api_host(NewState),
                            spawn(fun() ->
                                try
                                    Result = fetch_guild_data_with_fallback(
                                        GuildId, ApiHostInfo, NewState
                                    ),
                                    gen_server:cast(
                                        Manager, {guild_data_reloaded, GuildId, Pid, From, Result}
                                    )
                                catch
                                    Class:Error:Stacktrace ->
                                        logger:error(
                                            "[guild_manager] Spawned process failed: ~p:~p~n~p",
                                            [Class, Error, Stacktrace]
                                        ),
                                        gen_server:cast(
                                            Manager,
                                            {guild_data_reloaded, GuildId, Pid, From,
                                                {error, fetch_failed}}
                                        )
                                end
                            end),
                            {noreply, NewState};
                        {error, not_found} ->
                            {reply, {error, not_found}, State}
                    end
            end
    end.

-spec do_shutdown_guild(guild_id(), state()) -> {reply, ok, state()}.
do_shutdown_guild(GuildId, State) ->
    Guilds = maps:get(guilds, State),
    GuildName = process_registry:build_process_name(guild, GuildId),
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, Ref} ->
            demonitor(Ref, [flush]),
            gen_server:call(Pid, {terminate}, ?SHUTDOWN_TIMEOUT),
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
