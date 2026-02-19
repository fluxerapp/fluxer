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

-module(very_large_guild).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type shard_index() :: non_neg_integer().
-type session_id() :: binary().
-type user_id() :: integer().
-type guild_id() :: integer().
-type channel_id() :: integer().

-define(DEFAULT_MAX_SHARDS, 8).
-define(DEFAULT_MIN_SHARDS, 2).

-type shard_entry() :: #{
    pid := pid(),
    mref := reference()
}.

-type state() :: #{
    id := guild_id(),
    shard_count := pos_integer(),
    shards := #{shard_index() => shard_entry()},
    member_list_pid => pid(),
    session_routes := #{session_id() => shard_index()},
    user_session_counts := #{user_id() => pos_integer()},
    connection_routes := #{binary() => {shard_index(), session_id() | undefined}},
    last_known_data => map()
}.

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(GuildState) when is_map(GuildState) ->
    gen_server:start_link(?MODULE, GuildState, []).

-spec init(map()) -> {ok, state()}.
init(GuildState0) ->
    process_flag(trap_exit, true),
    GuildId = maps:get(id, GuildState0),
    Data0 = maps:get(data, GuildState0, #{}),
    Data = guild_data_index:normalize_data(Data0),
    ShardCount = determine_shard_count(GuildState0),
    {Shards, _Started} = start_shards(GuildId, Data, ShardCount),
    Shard0Pid =
        case maps:get(0, Shards, undefined) of
            #{pid := Pid0} when is_pid(Pid0) -> Pid0;
            _ -> erlang:error(missing_shard0)
        end,
    {ok, MemberListPid} = very_large_guild_member_list:start_link(#{
        id => GuildId,
        coordinator_pid => self(),
        shard0_pid => Shard0Pid
    }),
    {ok, #{
        id => GuildId,
        shard_count => ShardCount,
        shards => Shards,
        member_list_pid => MemberListPid,
        session_routes => #{},
        user_session_counts => #{},
        connection_routes => #{},
        last_known_data => Data
    }}.

handle_call({debug_get_routes}, _From, State) ->
    {reply,
        #{
            session_routes => maps:get(session_routes, State, #{}),
            user_session_counts => maps:get(user_session_counts, State, #{}),
            connection_routes => maps:get(connection_routes, State, #{})
        },
        State};

handle_call({session_connect, Request}, From, State) ->
    UserId = maps:get(user_id, Request, 0),
    ShardIndex = select_shard(UserId, maps:get(shard_count, State, 1)),
    ShardPid = shard_pid_or_throw(ShardIndex, State),
    _ = spawn(fun() ->
        try
            case ShardIndex =/= 0 of
                true ->
                    Shard0 = shard_pid_or_throw(0, State),
                    MemberReply = safe_call(Shard0, {get_guild_member, #{user_id => UserId}}, 5000),
                    case MemberReply of
                        #{success := true, member_data := MemberData} when is_map(MemberData) ->
                            _ = safe_call(ShardPid, {very_large_guild_prime_member, MemberData}, 5000),
                            ok;
                        _ ->
                            ok
                    end;
                false ->
                    ok
            end
        catch
            _:_ -> ok
        end,
        Reply = safe_call(ShardPid, {session_connect, Request}, 10000),
        gen_server:reply(From, Reply)
    end),
    {noreply, State};
handle_call({reload, NewData}, _From, State) ->
    reload_shards(NewData, State),
    prime_connected_members(State),
    _ = maybe_notify_member_list_worker({notify_reload, NewData}, State),
    {reply, ok, maps:put(last_known_data, NewData, State)};
handle_call({dispatch, #{event := Event, data := EventData} = Request}, _From, State) ->
    broadcast_cast({dispatch, Request}, State),
    maybe_trigger_push(Event, EventData, State),
    {reply, ok, State};
handle_call({lazy_subscribe, #{session_id := SessionId} = Request}, _From, State) ->
    MemberListPid = maps:get(member_list_pid, State, undefined),
    case {MemberListPid, maps:get(channel_id, Request, undefined), maps:get(ranges, Request, undefined)} of
        {Pid, ChannelId, Ranges} when is_pid(Pid), is_integer(ChannelId), is_list(Ranges) ->
            gen_server:cast(Pid, {subscribe, SessionId, ChannelId, Ranges}),
            {reply, ok, State};
        _ ->
            {reply, ok, State}
    end;
handle_call({get_counts}, _From, State) ->
    {reply, get_counts_aggregated(State), State};
handle_call({get_sessions}, _From, State) ->
    State1 = try_update_last_known_data(State),
    {reply, get_sessions_aggregated(State1), State1};
handle_call({very_large_guild_get_members, UserIds}, _From, State) when is_list(UserIds) ->
    Shard0 = shard_pid_or_throw(0, State),
    {reply, safe_call(Shard0, {very_large_guild_get_members, UserIds}, 10000), State};
handle_call({voice_state_update, Request}, _From, State) ->
    UserId = maps:get(user_id, Request, 0),
    SessionId = maps:get(session_id, Request, undefined),
    ShardIndex = select_shard(UserId, maps:get(shard_count, State, 1)),
    ShardPid = shard_pid_or_throw(ShardIndex, State),
    Reply = safe_call(ShardPid, {voice_state_update, Request}, 12000),
    {reply, Reply, maybe_track_connection_route(Reply, ShardIndex, SessionId, State)};
handle_call({add_virtual_channel_access, UserId, _ChannelId} = Msg, _From, State) ->
    ShardIndex = select_shard(UserId, maps:get(shard_count, State, 1)),
    ShardPid = shard_pid_or_throw(ShardIndex, State),
    {reply, safe_call(ShardPid, Msg, 10000), State};
handle_call({store_pending_connection, ConnectionId, PendingMetadata} = Request, _From, State) ->
    SessionId = extract_pending_session_id(PendingMetadata),
    case SessionId of
        undefined ->
            ShardIndex0 = extract_shard_from_route(ConnectionId, State, 0),
            ShardPid0 = shard_pid_or_throw(ShardIndex0, State),
            Reply = safe_call(ShardPid0, Request, 10000),
            {reply, Reply, put_connection_route(ConnectionId, ShardIndex0, undefined, State)};
        _ ->
            Reply = safe_call_to_session_shard(SessionId, Request, 10000, State),
            {reply, Reply, maybe_put_connection_route_from_session(SessionId, ConnectionId, State)}
    end;
handle_call({confirm_voice_connection_from_livekit, Request} = Msg, _From, State) ->
    ConnId = maps:get(connection_id, Request, undefined),
    {reply, route_or_fanout_by_connection(ConnId, Msg, State), State};
handle_call({get_pending_joins_for_channel, Request}, _From, State) ->
    {reply, get_pending_joins_for_channel_aggregated(Request, State), State};
handle_call({terminate}, _From, State) ->
    {stop, normal, ok, State};
handle_call(Request, _From, State) ->
    ShardPid = shard_pid_or_throw(0, State),
    {reply, safe_call(ShardPid, Request, 10000), State}.

handle_cast({set_session_active, SessionId}, State) ->
    _ = cast_to_session_shard(SessionId, {set_session_active, SessionId}, State),
    {noreply, State};
handle_cast({session_connect_async, #{guild_id := GuildId, attempt := Attempt, request := Request}}, State) ->
    UserId = maps:get(user_id, Request, 0),
    ShardCount = maps:get(shard_count, State, 1),
    ShardIndex = select_shard(UserId, ShardCount),
    ShardPid = shard_pid_or_throw(ShardIndex, State),
    CoordPid = self(),
    _ = spawn(fun() ->
        try
            case ShardIndex =/= 0 of
                true ->
                    Shard0 = shard_pid_or_throw(0, State),
                    MemberReply = safe_call(Shard0, {get_guild_member, #{user_id => UserId}}, 5000),
                    case MemberReply of
                        #{success := true, member_data := MemberData} when is_map(MemberData) ->
                            _ = safe_call(ShardPid, {very_large_guild_prime_member, MemberData}, 5000),
                            ok;
                        _ ->
                            ok
                    end;
                false ->
                    ok
            end
        catch
            _:_ -> ok
        end,
        gen_server:cast(
            ShardPid,
            {session_connect_async, #{
                guild_id => GuildId,
                attempt => Attempt,
                request => Request,
                reply_via_pid => CoordPid
            }}
        )
    end),
    {noreply, State};
handle_cast({set_session_passive, SessionId}, State) ->
    _ = cast_to_session_shard(SessionId, {set_session_passive, SessionId}, State),
    {noreply, State};
handle_cast({update_member_subscriptions, SessionId, MemberIds}, State) ->
    _ = cast_to_session_shard(SessionId, {update_member_subscriptions, SessionId, MemberIds}, State),
    {noreply, State};
handle_cast({set_session_typing_override, SessionId, TypingFlag}, State) ->
    _ = cast_to_session_shard(
        SessionId, {set_session_typing_override, SessionId, TypingFlag}, State
    ),
    {noreply, State};
handle_cast({send_guild_sync, SessionId}, State) ->
    _ = cast_to_session_shard(SessionId, {send_guild_sync, SessionId}, State),
    {noreply, State};
handle_cast({send_members_chunk, SessionId, ChunkData}, State) ->
    _ = cast_to_session_shard(SessionId, {send_members_chunk, SessionId, ChunkData}, State),
    {noreply, State};
handle_cast({cleanup_virtual_access_for_user, UserId}, State) ->
    ShardIndex = select_shard(UserId, maps:get(shard_count, State, 1)),
    ShardPid = shard_pid_or_throw(ShardIndex, State),
    gen_server:cast(ShardPid, {cleanup_virtual_access_for_user, UserId}),
    _ = maybe_notify_member_list_virtual_access_cleanup(UserId, State),
    {noreply, State};
handle_cast({add_virtual_channel_access, UserId, _ChannelId} = Msg, State) ->
    ShardIndex = select_shard(UserId, maps:get(shard_count, State, 1)),
    ShardPid = shard_pid_or_throw(ShardIndex, State),
    gen_server:cast(ShardPid, Msg),
    _ = maybe_notify_member_list_virtual_access_add(Msg, State),
    {noreply, State};
handle_cast({remove_virtual_channel_access, UserId, _ChannelId} = Msg, State) ->
    ShardIndex = select_shard(UserId, maps:get(shard_count, State, 1)),
    ShardPid = shard_pid_or_throw(ShardIndex, State),
    gen_server:cast(ShardPid, Msg),
    _ = maybe_notify_member_list_virtual_access_remove(Msg, State),
    {noreply, State};
handle_cast({very_large_guild_member_list_notify, NotifyMsg}, State) ->
    _ = maybe_notify_member_list_worker(NotifyMsg, State),
    {noreply, State};
handle_cast({very_large_guild_member_list_deliver, DeliveriesByShard}, State) when is_map(DeliveriesByShard) ->
    maps:foreach(
        fun(ShardIndex, Deliveries) ->
            case catch shard_pid_or_throw(ShardIndex, State) of
                ShardPid when is_pid(ShardPid), is_list(Deliveries) ->
                    gen_server:cast(ShardPid, {very_large_guild_member_list_deliver, Deliveries});
                _ ->
                    ok
            end
        end,
        DeliveriesByShard
    ),
    {noreply, State};
handle_cast({dispatch, #{event := Event, data := EventData} = Request}, State) ->
    broadcast_cast({dispatch, Request}, State),
    maybe_trigger_push(Event, EventData, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({very_large_guild_session_connected, ShardIndex, SessionId, UserId}, State) ->
    State1 = put_session_route(SessionId, ShardIndex, State),
    State2 = inc_user_session_count(UserId, State1),
    _ = maybe_notify_member_list_worker({session_connected, SessionId, ShardIndex, UserId}, State2),
    {noreply, State2};
handle_info({very_large_guild_session_disconnected, ShardIndex, SessionId, UserId}, State) ->
    State1 = remove_session_route(SessionId, State),
    State2 = dec_user_session_count(UserId, State1),
    State3 = remove_connection_routes_for_session(SessionId, State2),
    _ = maybe_notify_member_list_worker({session_disconnected, SessionId, UserId}, State3),
    case maps:is_key(UserId, maps:get(user_session_counts, State3, #{})) of
        true ->
            ok;
        false ->
            case catch shard_pid_or_throw(ShardIndex, State3) of
                ShardPid when is_pid(ShardPid) ->
                    gen_server:cast(ShardPid, {very_large_guild_prune_members});
                _ ->
                    ok
            end
    end,
    case map_size(maps:get(session_routes, State3, #{})) of
        0 ->
            {stop, normal, State3};
        _ ->
            {noreply, State3}
    end;
handle_info({very_large_guild_voice_state_update, SourceShardIndex, VoiceState, OldChannelIdBin}, State) ->
    relay_to_other_shards(SourceShardIndex, {relay_voice_state_update, VoiceState, OldChannelIdBin}, State),
    {noreply, State};
handle_info(
    {very_large_guild_voice_server_update, _SourceShardIndex, GuildId, ChannelId, SessionId, Token,
        Endpoint, ConnectionId},
    State
) ->
    _ = relay_voice_server_update(
        {GuildId, ChannelId, SessionId, Token, Endpoint, ConnectionId},
        State
    ),
    {noreply, State};
handle_info({very_large_guild_session_connect_result, GuildId, Attempt, SessionPid, Result0}, State) ->
    Result =
        case Result0 of
            {ok, GuildState} -> {ok, self(), GuildState};
            {ok_unavailable, UnavailableResponse} -> {ok_unavailable, self(), UnavailableResponse};
            {error, Reason} -> {error, Reason};
            Other -> {error, {unexpected_connect_result, Other}}
        end,
    case is_pid(SessionPid) of
        true -> SessionPid ! {guild_connect_result, GuildId, Attempt, Result};
        false -> ok
    end,
    {noreply, State};
handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    case find_shard_by_monitor_ref(Ref, Pid, State) of
        {ok, ShardIndex} ->
            State1 = restart_shard(ShardIndex, State),
            case ShardIndex of
                0 ->
                    NewPid = shard_pid_or_throw(0, State1),
                    _ = maybe_notify_member_list_worker({set_shard0_pid, NewPid}, State1),
                    ok;
                _ ->
                    ok
            end,
            {noreply, State1};
        not_found ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    maps:foreach(
        fun(_Index, #{pid := Pid}) ->
            catch gen_server:call(Pid, {terminate}, 5000),
            ok
        end,
        maps:get(shards, State, #{})
    ),
    case maps:get(member_list_pid, State, undefined) of
        Pid when is_pid(Pid) ->
            catch gen_server:stop(Pid),
            ok;
        _ ->
            ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec determine_shard_count(map()) -> pos_integer().
determine_shard_count(GuildState) ->
    Override = maps:get(very_large_guild_shard_count, GuildState, undefined),
    case Override of
        N when is_integer(N), N >= 1 ->
            N;
        _ ->
            EnvValue = fluxer_gateway_env:get(very_large_guild_shards),
            case EnvValue of
                N when is_integer(N), N >= 1 ->
                    N;
                _ ->
                    Schedulers = erlang:system_info(schedulers_online),
                    max(
                        ?DEFAULT_MIN_SHARDS,
                        min(?DEFAULT_MAX_SHARDS, max(1, Schedulers))
                    )
            end
    end.

-spec start_shards(guild_id(), map(), pos_integer()) -> {#{shard_index() => shard_entry()}, ok}.
start_shards(GuildId, Data, ShardCount) ->
    Shards = lists:foldl(
        fun(Index, Acc) ->
            ShardState = guild_common:build_shard_state(GuildId, Data, ShardCount, Index),
            case guild:start_link(ShardState) of
                {ok, Pid} ->
                    MRef = monitor(process, Pid),
                    maps:put(Index, #{pid => Pid, mref => MRef}, Acc);
                {error, Reason} ->
                    error({failed_to_start_very_large_guild_shard, Index, Reason})
            end
        end,
        #{},
        lists:seq(0, ShardCount - 1)
    ),
    {Shards, ok}.

-spec restart_shard(shard_index(), state()) -> state().
restart_shard(ShardIndex, State) ->
    Shards0 = maps:get(shards, State, #{}),
    GuildId = maps:get(id, State),
    Fallback = maps:get(last_known_data, State, #{}),
    Data =
        case maps:get(0, Shards0, undefined) of
            #{pid := Pid0} ->
                case guild_common:safe_call(Pid0, {get_push_base_state}, 5000) of
                    #{data := D} -> D;
                    _ -> Fallback
                end;
            _ ->
                Fallback
        end,
    ShardCount = maps:get(shard_count, State, 1),
    ShardState = guild_common:build_shard_state(GuildId, Data, ShardCount, ShardIndex),
    case guild:start_link(ShardState) of
        {ok, NewPid} ->
            MRef = monitor(process, NewPid),
            Shards1 = maps:put(ShardIndex, #{pid => NewPid, mref => MRef}, Shards0),
            State1 = maps:put(shards, Shards1, State),
            maps:put(last_known_data, Data, State1);
        {error, Reason} ->
            error({failed_to_restart_very_large_guild_shard, ShardIndex, Reason})
    end.

-spec find_shard_by_monitor_ref(reference(), pid(), state()) -> {ok, shard_index()} | not_found.
find_shard_by_monitor_ref(Ref, Pid, State) ->
    Shards = maps:get(shards, State, #{}),
    Found =
        maps:fold(
            fun(Index, #{pid := ShardPid, mref := MRef}, Acc) ->
                case Acc of
                    not_found when ShardPid =:= Pid andalso MRef =:= Ref -> {ok, Index};
                    _ -> Acc
                end
            end,
            not_found,
            Shards
        ),
    Found.

-spec shard_pid_or_throw(shard_index(), state()) -> pid().
shard_pid_or_throw(Index, State) ->
    case maps:get(Index, maps:get(shards, State, #{}), undefined) of
        #{pid := Pid} when is_pid(Pid) -> Pid;
        _ -> error({missing_shard_pid, Index})
    end.

-spec select_shard(user_id(), pos_integer()) -> shard_index().
select_shard(UserId, ShardCount) when is_integer(UserId), UserId >= 0 ->
    erlang:phash2(UserId, ShardCount);
select_shard(_, _ShardCount) ->
    0.

-spec put_session_route(session_id(), shard_index(), state()) -> state().
put_session_route(SessionId, ShardIndex, State) ->
    Routes0 = maps:get(session_routes, State, #{}),
    Routes = maps:put(SessionId, ShardIndex, Routes0),
    maps:put(session_routes, Routes, State).

-spec remove_session_route(session_id(), state()) -> state().
remove_session_route(SessionId, State) ->
    Routes0 = maps:get(session_routes, State, #{}),
    Routes = maps:remove(SessionId, Routes0),
    maps:put(session_routes, Routes, State).

-spec inc_user_session_count(user_id(), state()) -> state().
inc_user_session_count(UserId, State) ->
    Counts0 = maps:get(user_session_counts, State, #{}),
    Prev = maps:get(UserId, Counts0, 0),
    Counts = maps:put(UserId, Prev + 1, Counts0),
    maps:put(user_session_counts, Counts, State).

-spec dec_user_session_count(user_id(), state()) -> state().
dec_user_session_count(UserId, State) ->
    Counts0 = maps:get(user_session_counts, State, #{}),
    case maps:get(UserId, Counts0, 0) of
        N when is_integer(N), N > 1 ->
            Counts = maps:put(UserId, N - 1, Counts0),
            maps:put(user_session_counts, Counts, State);
        1 ->
            Counts = maps:remove(UserId, Counts0),
            maps:put(user_session_counts, Counts, State);
        _ ->
            State
    end.

-spec build_connected_sessions_map(state()) -> map().
build_connected_sessions_map(State) ->
    Counts = maps:get(user_session_counts, State, #{}),
    maps:fold(
        fun(UserId, _N, Acc) ->
            maps:put(integer_to_binary(UserId), #{user_id => UserId}, Acc)
        end,
        #{},
        Counts
    ).

-spec maybe_trigger_push(atom(), map(), state()) -> ok.
maybe_trigger_push(message_create, EventData, State) ->
    Shard0 = shard_pid_or_throw(0, State),
    GuildId = maps:get(id, State),
    ConnectedSessions = build_connected_sessions_map(State),
    _ = spawn(fun() ->
        Base0 = safe_call(Shard0, {get_push_base_state}, 5000),
        Base =
            case is_map(Base0) of
                true -> Base0;
                false -> #{id => GuildId, data => #{}, virtual_channel_access => #{}}
            end,
        PushState = maps:merge(Base, #{sessions => ConnectedSessions}),
        guild_dispatch:collect_and_send_push_notifications(EventData, GuildId, PushState)
    end),
    ok;
maybe_trigger_push(_, _EventData, _State) ->
    ok.

-spec broadcast_cast(term(), state()) -> ok.
broadcast_cast(Msg, State) ->
    Shards = maps:get(shards, State, #{}),
    maps:foreach(fun(_Index, #{pid := Pid}) -> gen_server:cast(Pid, Msg) end, Shards),
    ok.

-spec reload_shards(map(), state()) -> ok.
reload_shards(NewData, State) ->
    Shards = maps:get(shards, State, #{}),
    maps:foreach(
        fun(Index, #{pid := Pid}) ->
            Payload =
                case Index of
                    0 -> NewData;
                    _ -> guild_common:strip_members(NewData)
                end,
            _ = guild_common:safe_call(Pid, {reload, Payload}, 20000),
            ok
        end,
        Shards
    ),
    ok.

-spec prime_connected_members(state()) -> ok.
prime_connected_members(State) ->
    _ = spawn(fun() -> do_prime_connected_members(State) end),
    ok.

-spec do_prime_connected_members(state()) -> ok.
do_prime_connected_members(State) ->
    Shard0 = shard_pid_or_throw(0, State),
    ShardCount = maps:get(shard_count, State, 1),
    Counts = maps:get(user_session_counts, State, #{}),
    maps:foreach(
        fun(UserId, _N) ->
            ShardIndex = select_shard(UserId, ShardCount),
            case ShardIndex =/= 0 of
                true ->
                    case catch shard_pid_or_throw(ShardIndex, State) of
                        ShardPid when is_pid(ShardPid) ->
                            MemberReply =
                                safe_call(Shard0, {get_guild_member, #{user_id => UserId}}, 5000),
                            case MemberReply of
                                #{success := true, member_data := MemberData} when is_map(MemberData) ->
                                    _ = safe_call(
                                        ShardPid, {very_large_guild_prime_member, MemberData}, 5000
                                    ),
                                    ok;
                                _ ->
                                    ok
                            end;
                        _ ->
                            ok
                    end;
                false ->
                    ok
            end
        end,
        Counts
    ),
    ok.

-spec safe_call(pid(), term(), timeout()) -> term().
safe_call(Pid, Msg, Timeout) ->
    guild_common:safe_call(Pid, Msg, Timeout).

-spec safe_call_to_session_shard(session_id(), term(), timeout(), state()) -> term().
safe_call_to_session_shard(SessionId, Msg, Timeout, State) ->
    case maps:get(SessionId, maps:get(session_routes, State, #{}), undefined) of
        ShardIndex when is_integer(ShardIndex) ->
            ShardPid = shard_pid_or_throw(ShardIndex, State),
            safe_call(ShardPid, Msg, Timeout);
        _ ->
            fanout_call(Msg, Timeout, State)
    end.

-spec cast_to_session_shard(session_id(), term(), state()) -> ok.
cast_to_session_shard(SessionId, Msg, State) ->
    case maps:get(SessionId, maps:get(session_routes, State, #{}), undefined) of
        ShardIndex when is_integer(ShardIndex) ->
            ShardPid = shard_pid_or_throw(ShardIndex, State),
            gen_server:cast(ShardPid, Msg),
            ok;
        _ ->
            broadcast_cast(Msg, State)
    end.

-spec fanout_call(term(), timeout(), state()) -> term().
fanout_call(Msg, Timeout, State) ->
    Shards = maps:get(shards, State, #{}),
    fanout_call_pids(Msg, Timeout, [maps:get(pid, Entry) || {_I, Entry} <- maps:to_list(Shards)]).

-spec fanout_call_pids(term(), timeout(), [pid()]) -> term().
fanout_call_pids(_Msg, _Timeout, []) ->
    {error, not_found};
fanout_call_pids(Msg, Timeout, [Pid | Rest]) ->
    case safe_call(Pid, Msg, Timeout) of
        {error, _} ->
            fanout_call_pids(Msg, Timeout, Rest);
        Reply ->
            Reply
    end.

-spec try_update_last_known_data(state()) -> state().
try_update_last_known_data(State) ->
    case catch shard_pid_or_throw(0, State) of
        Shard0Pid when is_pid(Shard0Pid) ->
            case safe_call(Shard0Pid, {get_push_base_state}, 5000) of
                #{data := D} when is_map(D) -> maps:put(last_known_data, D, State);
                _ -> State
            end;
        _ ->
            State
    end.

-spec get_counts_aggregated(state()) -> map().
get_counts_aggregated(State) ->
    Shard0 = shard_pid_or_throw(0, State),
    Meta = safe_call(Shard0, {get_large_guild_metadata}, 5000),
    MemberCount = case is_map(Meta) of true -> maps:get(member_count, Meta, 0); false -> 0 end,
    PresenceCount = sum_presence_counts(State),
    #{member_count => MemberCount, presence_count => PresenceCount}.

-spec sum_presence_counts(state()) -> non_neg_integer().
sum_presence_counts(State) ->
    Shards = maps:get(shards, State, #{}),
    maps:fold(
        fun(_Index, #{pid := Pid}, Acc) ->
            Counts = safe_call(Pid, {get_counts}, 5000),
            case is_map(Counts) of true -> Acc + maps:get(presence_count, Counts, 0); false -> Acc end
        end,
        0,
        Shards
    ).

-spec get_sessions_aggregated(state()) -> map().
get_sessions_aggregated(State) ->
    Shards = maps:get(shards, State, #{}),
    Base0 = safe_call(shard_pid_or_throw(0, State), {get_sessions}, 10000),
    Base = case is_map(Base0) of true -> Base0; false -> #{} end,
    Merged = maps:fold(
        fun(_Index, #{pid := Pid}, Acc) ->
            Frag0 = safe_call(Pid, {get_cluster_merge_state}, 5000),
            Frag = case is_map(Frag0) of true -> Frag0; false -> #{} end,
            merge_cluster_state(Acc, Frag)
        end,
        Base#{sessions => #{}},
        Shards
    ),
    maps:without(
        [
            very_large_guild_coordinator_pid,
            very_large_guild_shard_index,
            very_large_guild_shard_count,
            disable_push_notifications,
            disable_auto_stop_on_empty,
            disable_permission_cache_updates
        ],
        Merged
    ).

-spec maybe_notify_member_list_worker(term(), state()) -> ok.
maybe_notify_member_list_worker(Msg, State) ->
    case maps:get(member_list_pid, State, undefined) of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, Msg),
            ok;
        _ ->
            ok
    end.

-spec maybe_notify_member_list_virtual_access_add({add_virtual_channel_access, user_id(), channel_id()}, state()) -> ok.
maybe_notify_member_list_virtual_access_add({add_virtual_channel_access, UserId, ChannelId}, State) ->
    maybe_notify_member_list_worker({virtual_access_added, UserId, ChannelId}, State).

-spec maybe_notify_member_list_virtual_access_remove({remove_virtual_channel_access, user_id(), channel_id()}, state()) -> ok.
maybe_notify_member_list_virtual_access_remove({remove_virtual_channel_access, UserId, ChannelId}, State) ->
    maybe_notify_member_list_worker({virtual_access_removed, UserId, ChannelId}, State).

-spec maybe_notify_member_list_virtual_access_cleanup(user_id(), state()) -> ok.
maybe_notify_member_list_virtual_access_cleanup(UserId, State) ->
    maybe_notify_member_list_worker({virtual_access_cleanup, UserId}, State).

-spec merge_cluster_state(map(), map()) -> map().
merge_cluster_state(Acc, Frag) ->
    guild_common:merge_cluster_state(Acc, Frag).

-spec relay_to_other_shards(shard_index(), term(), state()) -> ok.
relay_to_other_shards(SourceIndex, Msg, State) ->
    Shards = maps:get(shards, State, #{}),
    maps:foreach(
        fun(Index, #{pid := Pid}) ->
            case Index =:= SourceIndex of
                true -> ok;
                false -> gen_server:cast(Pid, Msg)
            end
        end,
        Shards
    ),
    ok.

-spec relay_voice_server_update(
    {integer(), integer(), binary(), binary(), binary(), binary()},
    state()
) -> ok.
relay_voice_server_update({GuildId, ChannelId, SessionId, Token, Endpoint, ConnectionId}, State) ->
    Msg = {relay_voice_server_update, GuildId, ChannelId, SessionId, Token, Endpoint, ConnectionId},
    case maps:get(SessionId, maps:get(session_routes, State, #{}), undefined) of
        ShardIndex when is_integer(ShardIndex) ->
            ShardPid = shard_pid_or_throw(ShardIndex, State),
            gen_server:cast(ShardPid, Msg),
            ok;
        _ ->
            broadcast_cast(Msg, State),
            ok
    end.

-spec extract_pending_session_id(term()) -> binary() | undefined.
extract_pending_session_id(PendingMetadata) when is_map(PendingMetadata) ->
    case maps:get(session_id, PendingMetadata, undefined) of
        SessionId when is_binary(SessionId) -> SessionId;
        _ ->
            case maps:get(<<"session_id">>, PendingMetadata, undefined) of
                SessionId when is_binary(SessionId) -> SessionId;
                _ -> undefined
            end
    end;
extract_pending_session_id(_) ->
    undefined.

-spec maybe_put_connection_route_from_session(session_id(), binary(), state()) -> state().
maybe_put_connection_route_from_session(SessionId, ConnectionId, State) ->
    case maps:get(SessionId, maps:get(session_routes, State, #{}), undefined) of
        ShardIndex when is_integer(ShardIndex) -> put_connection_route(ConnectionId, ShardIndex, SessionId, State);
        _ -> State
    end.

-spec put_connection_route(binary(), shard_index(), session_id() | undefined, state()) -> state().
put_connection_route(ConnectionId, ShardIndex, SessionId, State) ->
    Routes0 = maps:get(connection_routes, State, #{}),
    Routes = maps:put(ConnectionId, {ShardIndex, SessionId}, Routes0),
    maps:put(connection_routes, Routes, State).

-spec extract_shard_from_route(binary(), state(), shard_index()) -> shard_index().
extract_shard_from_route(ConnectionId, State, Default) ->
    case maps:get(ConnectionId, maps:get(connection_routes, State, #{}), undefined) of
        {ShardIndex, _} when is_integer(ShardIndex) -> ShardIndex;
        _ -> Default
    end.

-spec remove_connection_routes_for_session(session_id(), state()) -> state().
remove_connection_routes_for_session(SessionId, State) ->
    Routes0 = maps:get(connection_routes, State, #{}),
    Routes = maps:filter(
        fun(_ConnId, Value) ->
            case Value of
                {_ShardIndex, Sid} -> Sid =/= SessionId;
                _ -> true
            end
        end,
        Routes0
    ),
    maps:put(connection_routes, Routes, State).

-spec maybe_track_connection_route(term(), shard_index(), session_id() | undefined, state()) -> state().
maybe_track_connection_route(Reply, ShardIndex, SessionId, State) when is_map(Reply) ->
    case maps:get(connection_id, Reply, undefined) of
        ConnId when is_binary(ConnId) -> put_connection_route(ConnId, ShardIndex, SessionId, State);
        _ -> State
    end;
maybe_track_connection_route(_Reply, _ShardIndex, _SessionId, State) ->
    State.

-spec route_or_fanout_by_connection(binary() | undefined, term(), state()) -> term().
route_or_fanout_by_connection(undefined, Msg, State) ->
    fanout_call(Msg, 10000, State);
route_or_fanout_by_connection(ConnectionId, Msg, State) ->
    case maps:get(ConnectionId, maps:get(connection_routes, State, #{}), undefined) of
        {ShardIndex, _SessionId} when is_integer(ShardIndex) ->
            ShardPid = shard_pid_or_throw(ShardIndex, State),
            safe_call(ShardPid, Msg, 10000);
        _ ->
            fanout_call(Msg, 10000, State)
    end.

-spec get_pending_joins_for_channel_aggregated(map(), state()) -> term().
get_pending_joins_for_channel_aggregated(Request, State) ->
    Shards = maps:get(shards, State, #{}),
    lists:foldl(
        fun({_Index, #{pid := Pid}}, Acc) ->
            Reply = safe_call(Pid, {get_pending_joins_for_channel, Request}, 5000),
            case {Acc, Reply} of
                {#{pending_joins := A}, #{pending_joins := B}} when is_list(A), is_list(B) ->
                    Acc#{pending_joins := A ++ B};
                {_, #{pending_joins := B}} when is_list(B) ->
                    #{pending_joins => B};
                _ ->
                    Acc
            end
        end,
        #{pending_joins => []},
        maps:to_list(Shards)
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ensure_mock_registered(Name) ->
    case whereis(Name) of
        undefined ->
            Pid = spawn(fun() -> mock_gen_server_loop() end),
            register(Name, Pid),
            Pid;
        Pid ->
            Pid
    end.

mock_gen_server_loop() ->
    receive
        {'$gen_call', From, {get, _}} ->
            gen_server:reply(From, not_found),
            mock_gen_server_loop();
        {'$gen_call', From, _Msg} ->
            gen_server:reply(From, ok),
            mock_gen_server_loop();
        stop ->
            ok;
        _ ->
            mock_gen_server_loop()
    end.

stop_mock_registered(Name) ->
    case whereis(Name) of
        undefined -> ok;
        Pid ->
            catch unregister(Name),
            Pid ! stop,
            ok
    end.

ensure_test_deps() ->
    ensure_mock_registered(presence_bus),
    ensure_mock_registered(presence_cache).

stop_test_deps() ->
    stop_mock_registered(presence_bus),
    stop_mock_registered(presence_cache).

debug_get_routes(Pid) ->
    gen_server:call(Pid, {debug_get_routes}, 2000).

build_test_data(GuildId, Features) ->
    ViewPerm = constants:view_channel_permission(),
    #{
        <<"guild">> => #{
            <<"id">> => integer_to_binary(GuildId),
            <<"owner_id">> => <<"999">>,
            <<"features">> => Features
        },
        <<"roles">> => [
            #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => integer_to_binary(ViewPerm)}
        ],
        <<"members">> => [
            #{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => []},
            #{<<"user">> => #{<<"id">> => <<"2">>}, <<"roles">> => []},
            #{<<"user">> => #{<<"id">> => <<"3">>}, <<"roles">> => []}
        ],
        <<"channels">> => [#{<<"id">> => <<"10">>, <<"permission_overwrites">> => []}]
    }.

start_session_capture(Tag, Parent) ->
    spawn(fun() ->
        receive_loop(Tag, Parent)
    end).

receive_loop(Tag, Parent) ->
    receive
        stop ->
            ok;
        {'$gen_cast', Msg} ->
            Parent ! {Tag, Msg},
            receive_loop(Tag, Parent);
        _ ->
            receive_loop(Tag, Parent)
    end.

push_notification_sent_once_test() ->
    ensure_test_deps(),
    PrevPushEnabled = fluxer_gateway_env:get(push_enabled),
    _ = fluxer_gateway_env:patch(#{push_enabled => true}),
    catch unregister(push),
    Parent = self(),
    PushPid = spawn(fun() -> receive_loop(push, Parent) end),
    true = register(push, PushPid),
    GuildId = 123,
    Data = build_test_data(GuildId, [<<"VERY_LARGE_GUILD">>]),
    {ok, Pid} = start_link(#{
        id => GuildId,
        data => Data,
        very_large_guild_shard_count => 2
    }),
    S1 = start_session_capture(s1, Parent),
    S2 = start_session_capture(s2, Parent),
    Req1 = #{
        session_id => <<"sid-1">>,
        user_id => 1,
        session_pid => S1,
        bot => false,
        is_staff => false,
        initial_guild_id => undefined,
        active_guilds => sets:new()
    },
    Req2 = Req1#{session_id := <<"sid-2">>, user_id := 2, session_pid := S2},
    {ok, _} = gen_server:call(Pid, {session_connect, Req1}, 10000),
    {ok, _} = gen_server:call(Pid, {session_connect, Req2}, 10000),
    EventData = #{
        <<"id">> => <<"500">>,
        <<"channel_id">> => <<"10">>,
        <<"author">> => #{<<"id">> => <<"1">>}
    },
    ok = gen_server:call(
        Pid,
        {dispatch, #{event => message_create, data => EventData}},
        5000
    ),
    PushMsgs = collect_tagged(push, 1000, []),
    ?assertEqual(1, length(PushMsgs)),
    S1Msgs = collect_tagged(s1, 1000, []),
    S2Msgs = collect_tagged(s2, 1000, []),
    ?assert(lists:any(fun({dispatch, message_create, _}) -> true; (_) -> false end, S1Msgs)),
    ?assert(lists:any(fun({dispatch, message_create, _}) -> true; (_) -> false end, S2Msgs)),
    S1 ! stop,
    S2 ! stop,
    PushPid ! stop,
    catch unregister(push),
    _ = fluxer_gateway_env:patch(#{push_enabled => PrevPushEnabled}),
    ok = gen_server:call(Pid, {terminate}, 5000),
    stop_test_deps(),
    ok.

collect_tagged(Tag, TimeoutMs, Acc) ->
    receive
        {Tag, Msg} ->
            collect_tagged(Tag, TimeoutMs, [Msg | Acc])
    after TimeoutMs ->
        lists:reverse(Acc)
    end.

session_routes_track_multiple_sessions_per_user_test() ->
    ensure_test_deps(),
    GuildId = 124,
    Data = build_test_data(GuildId, [<<"VERY_LARGE_GUILD">>]),
    {ok, Pid} = start_link(#{
        id => GuildId,
        data => Data,
        very_large_guild_shard_count => 2
    }),
    Parent = self(),
    S1 = start_session_capture(s1, Parent),
    S2 = start_session_capture(s2, Parent),
    Req1 = #{
        session_id => <<"sid-a">>,
        user_id => 1,
        session_pid => S1,
        bot => false,
        is_staff => false,
        initial_guild_id => undefined,
        active_guilds => sets:new()
    },
    Req2 = Req1#{session_id := <<"sid-b">>, session_pid := S2},
    {ok, _} = gen_server:call(Pid, {session_connect, Req1}, 10000),
    {ok, _} = gen_server:call(Pid, {session_connect, Req2}, 10000),
    Routes = debug_get_routes(Pid),
    SessionRoutes = maps:get(session_routes, Routes),
    UserCounts = maps:get(user_session_counts, Routes),
    ?assert(maps:is_key(<<"sid-a">>, SessionRoutes)),
    ?assert(maps:is_key(<<"sid-b">>, SessionRoutes)),
    ?assertEqual(2, maps:get(1, UserCounts)),
    S1 ! stop,
    S2 ! stop,
    ok = gen_server:call(Pid, {terminate}, 5000),
    stop_test_deps(),
    ok.

vlg_member_list_lazy_subscribe_sends_sync_test() ->
    ensure_test_deps(),
    GuildId = 125,
    Data = build_test_data(GuildId, [<<"VERY_LARGE_GUILD">>]),
    {ok, Pid} = start_link(#{
        id => GuildId,
        data => Data,
        very_large_guild_shard_count => 2
    }),
    Parent = self(),
    S1 = start_session_capture(s1, Parent),
    Req1 = #{
        session_id => <<"sid-ml-1">>,
        user_id => 1,
        session_pid => S1,
        bot => false,
        is_staff => false,
        initial_guild_id => undefined,
        active_guilds => sets:new()
    },
    {ok, _} = gen_server:call(Pid, {session_connect, Req1}, 10000),
    ok = await_session_route(Pid, <<"sid-ml-1">>, 1000),
    ok = gen_server:call(
        Pid,
        {lazy_subscribe, #{session_id => <<"sid-ml-1">>, channel_id => 10, ranges => [{0, 50}]}},
        5000
    ),
    Msgs = collect_tagged(s1, 1500, []),
    ?assert(
        lists:any(
            fun({dispatch, guild_member_list_update, Payload}) ->
                maps:get(<<"channel_id">>, Payload, undefined) =:= <<"10">>;
               (_) ->
                false
            end,
            Msgs
        )
    ),
    S1 ! stop,
    ok = gen_server:call(Pid, {terminate}, 5000),
    stop_test_deps(),
    ok.

vlg_member_list_member_update_triggers_update_test() ->
    ensure_test_deps(),
    GuildId = 126,
    Data = build_test_data(GuildId, [<<"VERY_LARGE_GUILD">>]),
    {ok, Pid} = start_link(#{
        id => GuildId,
        data => Data,
        very_large_guild_shard_count => 2
    }),
    Parent = self(),
    S1 = start_session_capture(s1, Parent),
    Req1 = #{
        session_id => <<"sid-ml-2">>,
        user_id => 1,
        session_pid => S1,
        bot => false,
        is_staff => false,
        initial_guild_id => undefined,
        active_guilds => sets:new()
    },
    {ok, _} = gen_server:call(Pid, {session_connect, Req1}, 10000),
    ok = await_session_route(Pid, <<"sid-ml-2">>, 1000),
    ok = gen_server:call(
        Pid,
        {lazy_subscribe, #{session_id => <<"sid-ml-2">>, channel_id => 10, ranges => [{0, 50}]}},
        5000
    ),
    _ = collect_tagged(s1, 2000, []),
    MemberUpdate = #{
        <<"user">> => #{<<"id">> => <<"2">>},
        <<"roles">> => [<<"999">>]
    },
    ok = gen_server:call(
        Pid,
        {dispatch, #{event => guild_member_update, data => MemberUpdate}},
        5000
    ),
    Msgs2 = collect_tagged(s1, 2000, []),
    ?assert(
        lists:any(
            fun({dispatch, guild_member_list_update, Payload}) ->
                maps:get(<<"id">>, Payload, undefined) =:= <<"10">>;
               (_) ->
                false
            end,
            Msgs2
        )
    ),
    S1 ! stop,
    ok = gen_server:call(Pid, {terminate}, 5000),
    stop_test_deps(),
    ok.

await_session_route(Pid, SessionId, TimeoutMs) ->
    StartMs = erlang:monotonic_time(millisecond),
    await_session_route_loop(Pid, SessionId, TimeoutMs, StartMs).

await_session_route_loop(Pid, SessionId, TimeoutMs, StartMs) ->
    Routes = debug_get_routes(Pid),
    SessionRoutes = maps:get(session_routes, Routes, #{}),
    case maps:is_key(SessionId, SessionRoutes) of
        true ->
            ok;
        false ->
            NowMs = erlang:monotonic_time(millisecond),
            case NowMs - StartMs > TimeoutMs of
                true ->
                    timeout;
                false ->
                    timer:sleep(20),
                    await_session_route_loop(Pid, SessionId, TimeoutMs, StartMs)
            end
    end.

await_process_down(Pid, TimeoutMs) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _} -> ok
    after TimeoutMs ->
        demonitor(Ref, [flush]),
        timeout
    end.

get_shard_pid(CoordPid, ShardIndex) ->
    State = sys:get_state(CoordPid),
    Shards = maps:get(shards, State),
    #{pid := ShardPid} = maps:get(ShardIndex, Shards),
    ShardPid.

await_shard_restart(CoordPid, ShardIndex, OldPid, TimeoutMs) ->
    StartMs = erlang:monotonic_time(millisecond),
    await_shard_restart_loop(CoordPid, ShardIndex, OldPid, TimeoutMs, StartMs).

await_shard_restart_loop(CoordPid, ShardIndex, OldPid, TimeoutMs, StartMs) ->
    case catch get_shard_pid(CoordPid, ShardIndex) of
        NewPid when is_pid(NewPid), NewPid =/= OldPid ->
            NewPid;
        _ ->
            NowMs = erlang:monotonic_time(millisecond),
            case NowMs - StartMs > TimeoutMs of
                true ->
                    timeout;
                false ->
                    timer:sleep(20),
                    await_shard_restart_loop(CoordPid, ShardIndex, OldPid, TimeoutMs, StartMs)
            end
    end.

start_mock_shard() ->
    spawn(fun() -> mock_shard_loop() end).

mock_shard_loop() ->
    receive
        {'$gen_call', From, _Msg} ->
            gen_server:reply(From, {error, timeout}),
            mock_shard_loop();
        stop ->
            ok;
        _ ->
            mock_shard_loop()
    end.

restart_shard_preserves_data_test() ->
    GuildId = 128,
    Data = build_test_data(GuildId, [<<"VERY_LARGE_GUILD">>]),
    {ok, Pid} = start_link(#{
        id => GuildId,
        data => Data,
        very_large_guild_shard_count => 2
    }),
    Counts1 = gen_server:call(Pid, {get_counts}, 5000),
    ?assertEqual(3, maps:get(member_count, Counts1)),
    Shard0Pid = get_shard_pid(Pid, 0),
    exit(Shard0Pid, kill),
    NewShard0 = await_shard_restart(Pid, 0, Shard0Pid, 3000),
    ?assert(is_pid(NewShard0)),
    Counts2 = gen_server:call(Pid, {get_counts}, 5000),
    ?assertEqual(3, maps:get(member_count, Counts2)),
    ok = gen_server:call(Pid, {terminate}, 5000),
    ok.

select_shard_distribution_test() ->
    ShardCount = 4,
    Shards = lists:foldl(
        fun(UserId, Acc) ->
            Shard = select_shard(UserId, ShardCount),
            sets:add_element(Shard, Acc)
        end,
        sets:new(),
        lists:seq(1, 100)
    ),
    ?assert(sets:size(Shards) > 1),
    ok.

connection_route_cleanup_on_disconnect_test() ->
    ensure_test_deps(),
    GuildId = 129,
    Data = build_test_data(GuildId, [<<"VERY_LARGE_GUILD">>]),
    {ok, Pid} = start_link(#{
        id => GuildId,
        data => Data,
        very_large_guild_shard_count => 2
    }),
    Parent = self(),
    S1 = start_session_capture(s1, Parent),
    S2 = start_session_capture(s2, Parent),
    Req1 = #{
        session_id => <<"sid-conn-1">>,
        user_id => 1,
        session_pid => S1,
        bot => false,
        is_staff => false,
        initial_guild_id => undefined,
        active_guilds => sets:new()
    },
    Req2 = Req1#{session_id := <<"sid-conn-2">>, user_id := 2, session_pid := S2},
    {ok, _} = gen_server:call(Pid, {session_connect, Req1}, 10000),
    {ok, _} = gen_server:call(Pid, {session_connect, Req2}, 10000),
    ok = await_session_route(Pid, <<"sid-conn-1">>, 1000),
    ok = await_session_route(Pid, <<"sid-conn-2">>, 1000),
    sys:replace_state(Pid, fun(State) ->
        State1 = put_connection_route(<<"conn-abc">>, 0, <<"sid-conn-1">>, State),
        put_connection_route(<<"conn-xyz">>, 1, <<"sid-conn-2">>, State1)
    end),
    Routes1 = maps:get(connection_routes, debug_get_routes(Pid)),
    ?assert(maps:is_key(<<"conn-abc">>, Routes1)),
    ?assert(maps:is_key(<<"conn-xyz">>, Routes1)),
    Pid ! {very_large_guild_session_disconnected, 0, <<"sid-conn-1">>, 1},
    timer:sleep(100),
    Routes2 = maps:get(connection_routes, debug_get_routes(Pid)),
    ?assertNot(maps:is_key(<<"conn-abc">>, Routes2)),
    ?assert(maps:is_key(<<"conn-xyz">>, Routes2)),
    S1 ! stop,
    S2 ! stop,
    ok = gen_server:call(Pid, {terminate}, 5000),
    stop_test_deps(),
    ok.

get_counts_handles_timeout_test() ->
    GuildId = 130,
    Data = build_test_data(GuildId, [<<"VERY_LARGE_GUILD">>]),
    {ok, Pid} = start_link(#{
        id => GuildId,
        data => Data,
        very_large_guild_shard_count => 2
    }),
    MockPid = start_mock_shard(),
    sys:replace_state(Pid, fun(State) ->
        Shards0 = maps:get(shards, State),
        #{mref := OldMref0} = maps:get(0, Shards0),
        demonitor(OldMref0, [flush]),
        #{mref := OldMref1} = maps:get(1, Shards0),
        demonitor(OldMref1, [flush]),
        MockMref0 = monitor(process, MockPid),
        MockMref1 = monitor(process, MockPid),
        Shards1 = maps:put(0, #{pid => MockPid, mref => MockMref0}, Shards0),
        Shards2 = maps:put(1, #{pid => MockPid, mref => MockMref1}, Shards1),
        maps:put(shards, Shards2, State)
    end),
    Result = gen_server:call(Pid, {get_counts}, 10000),
    ?assertMatch(#{member_count := _, presence_count := _}, Result),
    ?assertEqual(0, maps:get(member_count, Result)),
    ?assertEqual(0, maps:get(presence_count, Result)),
    MockPid ! stop,
    ok.

select_shard_edge_cases_test() ->
    ?assertEqual(0, select_shard(-1, 4)),
    ?assertEqual(0, select_shard(-100, 8)),
    ?assertEqual(0, select_shard(not_an_integer, 4)),
    ?assertEqual(0, select_shard(undefined, 2)),
    Shard0 = select_shard(0, 4),
    ?assert(Shard0 >= 0 andalso Shard0 < 4),
    ok.

strip_members_removes_members_and_role_index_test() ->
    Data = #{
        <<"members">> => [#{<<"user">> => #{<<"id">> => <<"1">>}}],
        <<"member_role_index">> => #{1 => [<<"role1">>]},
        <<"channels">> => [#{<<"id">> => <<"10">>}],
        <<"roles">> => [#{<<"id">> => <<"role1">>}]
    },
    Stripped = guild_common:strip_members(Data),
    ?assertEqual(false, maps:is_key(<<"members">>, Stripped)),
    ?assertEqual(false, maps:is_key(<<"member_role_index">>, Stripped)),
    ?assertEqual([#{<<"id">> => <<"10">>}], maps:get(<<"channels">>, Stripped)),
    ?assertEqual([#{<<"id">> => <<"role1">>}], maps:get(<<"roles">>, Stripped)),
    ?assertEqual(#{}, guild_common:strip_members(#{})),
    ?assertEqual(not_a_map, guild_common:strip_members(not_a_map)),
    ok.

coordinator_stops_on_last_disconnect_test() ->
    GuildId = 131,
    Data = build_test_data(GuildId, [<<"VERY_LARGE_GUILD">>]),
    {ok, Pid} = start_link(#{
        id => GuildId,
        data => Data,
        very_large_guild_shard_count => 2
    }),
    sys:replace_state(Pid, fun(State) ->
        State1 = put_session_route(<<"sid-last">>, 0, State),
        inc_user_session_count(42, State1)
    end),
    Pid ! {very_large_guild_session_disconnected, 0, <<"sid-last">>, 42},
    ?assertEqual(ok, await_process_down(Pid, 2000)),
    ok.

extract_pending_session_id_test() ->
    ?assertEqual(<<"sid-1">>, extract_pending_session_id(#{session_id => <<"sid-1">>})),
    ?assertEqual(<<"sid-2">>, extract_pending_session_id(#{<<"session_id">> => <<"sid-2">>})),
    ?assertEqual(undefined, extract_pending_session_id(#{session_id => 123})),
    ?assertEqual(undefined, extract_pending_session_id(#{})),
    ?assertEqual(undefined, extract_pending_session_id(not_a_map)),
    ?assertEqual(undefined, extract_pending_session_id(undefined)),
    ok.

merge_user_set_maps_test() ->
    SetA = sets:from_list([1, 2]),
    SetB = sets:from_list([2, 3]),
    MapA = #{10 => SetA},
    MapB = #{10 => SetB, 20 => SetB},
    Merged = guild_common:merge_user_set_maps(MapA, MapB),
    ?assert(maps:is_key(10, Merged)),
    ?assert(maps:is_key(20, Merged)),
    MergedSet10 = maps:get(10, Merged),
    ?assert(sets:is_element(1, MergedSet10)),
    ?assert(sets:is_element(2, MergedSet10)),
    ?assert(sets:is_element(3, MergedSet10)),
    ?assertEqual(3, sets:size(MergedSet10)),
    ?assertEqual(SetB, maps:get(20, Merged)),
    EmptyMerge = guild_common:merge_user_set_maps(#{}, #{}),
    ?assertEqual(#{}, EmptyMerge),
    ok.

determine_shard_count_respects_override_test() ->
    ?assertEqual(3, determine_shard_count(#{very_large_guild_shard_count => 3})),
    ?assertEqual(1, determine_shard_count(#{very_large_guild_shard_count => 1})),
    Auto = determine_shard_count(#{}),
    ?assert(Auto >= ?DEFAULT_MIN_SHARDS),
    ?assert(Auto =< ?DEFAULT_MAX_SHARDS),
    ok.

reload_updates_last_known_data_test() ->
    GuildId = 132,
    Data = build_test_data(GuildId, [<<"VERY_LARGE_GUILD">>]),
    {ok, Pid} = start_link(#{
        id => GuildId,
        data => Data,
        very_large_guild_shard_count => 2
    }),
    NewData = #{
        <<"guild">> => #{<<"id">> => integer_to_binary(GuildId), <<"owner_id">> => <<"999">>, <<"features">> => [<<"VERY_LARGE_GUILD">>]},
        <<"roles">> => [#{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"1024">>}],
        <<"members">> => [#{<<"user">> => #{<<"id">> => <<"1">>}, <<"roles">> => []}],
        <<"channels">> => [#{<<"id">> => <<"10">>, <<"permission_overwrites">> => []}, #{<<"id">> => <<"20">>, <<"permission_overwrites">> => []}]
    },
    ok = gen_server:call(Pid, {reload, NewData}, 10000),
    State = sys:get_state(Pid),
    LastData = maps:get(last_known_data, State, #{}),
    Channels = maps:get(<<"channels">>, LastData, []),
    ?assertEqual(2, length(Channels)),
    ok = gen_server:call(Pid, {terminate}, 5000),
    ok.

dec_user_session_count_at_zero_is_noop_test() ->
    State0 = #{user_session_counts => #{}},
    State1 = dec_user_session_count(42, State0),
    ?assertEqual(#{}, maps:get(user_session_counts, State1)),
    State2 = #{user_session_counts => #{42 => 2}},
    State3 = dec_user_session_count(42, State2),
    ?assertEqual(1, maps:get(42, maps:get(user_session_counts, State3))),
    State4 = dec_user_session_count(42, State3),
    ?assertNot(maps:is_key(42, maps:get(user_session_counts, State4))),
    State5 = dec_user_session_count(42, State4),
    ?assertNot(maps:is_key(42, maps:get(user_session_counts, State5))),
    ok.

virtual_access_forwarded_to_member_list_test() ->
    GuildId = 133,
    Data = build_test_data(GuildId, [<<"VERY_LARGE_GUILD">>]),
    {ok, Pid} = start_link(#{
        id => GuildId,
        data => Data,
        very_large_guild_shard_count => 2
    }),
    State0 = sys:get_state(Pid),
    MemberListPid = maps:get(member_list_pid, State0),
    ?assert(is_pid(MemberListPid)),
    ?assert(is_process_alive(MemberListPid)),
    gen_server:cast(Pid, {add_virtual_channel_access, 1, 10}),
    gen_server:cast(Pid, {remove_virtual_channel_access, 1, 10}),
    gen_server:cast(Pid, {cleanup_virtual_access_for_user, 1}),
    timer:sleep(100),
    ?assert(is_process_alive(MemberListPid)),
    ok = gen_server:call(Pid, {terminate}, 5000),
    ok.

cast_to_unknown_session_broadcasts_test() ->
    GuildId = 134,
    Data = build_test_data(GuildId, [<<"VERY_LARGE_GUILD">>]),
    {ok, Pid} = start_link(#{
        id => GuildId,
        data => Data,
        very_large_guild_shard_count => 2
    }),
    gen_server:cast(Pid, {set_session_active, <<"nonexistent-session">>}),
    timer:sleep(50),
    ?assert(is_process_alive(Pid)),
    ok = gen_server:call(Pid, {terminate}, 5000),
    ok.

dispatch_broadcasts_to_all_shards_test() ->
    GuildId = 135,
    Data = build_test_data(GuildId, [<<"VERY_LARGE_GUILD">>]),
    {ok, Pid} = start_link(#{
        id => GuildId,
        data => Data,
        very_large_guild_shard_count => 3
    }),
    State0 = sys:get_state(Pid),
    Shards = maps:get(shards, State0),
    ?assertEqual(3, map_size(Shards)),
    EventData = #{<<"id">> => <<"100">>, <<"channel_id">> => <<"10">>},
    ok = gen_server:call(Pid, {dispatch, #{event => typing_start, data => EventData}}, 5000),
    ?assert(is_process_alive(Pid)),
    ok = gen_server:call(Pid, {terminate}, 5000),
    ok.

lazy_subscribe_with_missing_member_list_pid_test() ->
    GuildId = 136,
    Data = build_test_data(GuildId, [<<"VERY_LARGE_GUILD">>]),
    {ok, Pid} = start_link(#{
        id => GuildId,
        data => Data,
        very_large_guild_shard_count => 2
    }),
    sys:replace_state(Pid, fun(State) ->
        maps:put(member_list_pid, undefined, State)
    end),
    ok = gen_server:call(
        Pid,
        {lazy_subscribe, #{session_id => <<"sid-x">>, channel_id => 10, ranges => [{0, 50}]}},
        5000
    ),
    ?assert(is_process_alive(Pid)),
    ok = gen_server:call(Pid, {terminate}, 5000),
    ok.

-endif.

