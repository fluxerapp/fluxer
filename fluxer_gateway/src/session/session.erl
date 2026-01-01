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

-module(session).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(SessionData) ->
    gen_server:start_link(?MODULE, SessionData, []).

init(SessionData) ->
    process_flag(trap_exit, true),

    Id = maps:get(id, SessionData),
    UserId = maps:get(user_id, SessionData),
    UserData = maps:get(user_data, SessionData),
    Version = maps:get(version, SessionData),
    TokenHash = maps:get(token_hash, SessionData),
    AuthSessionIdHash = maps:get(auth_session_id_hash, SessionData),
    Properties = maps:get(properties, SessionData),
    Status = maps:get(status, SessionData),
    Afk = maps:get(afk, SessionData, false),
    Mobile = maps:get(mobile, SessionData, false),
    SocketPid = maps:get(socket_pid, SessionData),
    GuildIds = maps:get(guilds, SessionData),
    Ready0 = maps:get(ready, SessionData),
    Bot = maps:get(bot, SessionData, false),
    InitialGuildId = maps:get(initial_guild_id, SessionData, undefined),
    Ready =
        case Bot of
            true -> ensure_bot_ready_map(Ready0);
            false -> Ready0
        end,
    IgnoredEvents = build_ignored_events_map(maps:get(ignored_events, SessionData, [])),

    Channels = load_private_channels(Ready),
    logger:debug("[session] Loaded ~p private channels into session state for user ~p", [
        maps:size(Channels),
        UserId
    ]),

    State = #{
        id => Id,
        user_id => UserId,
        user_data => UserData,
        custom_status => maps:get(custom_status, SessionData, null),
        version => Version,
        token_hash => TokenHash,
        auth_session_id_hash => AuthSessionIdHash,
        buffer => [],
        seq => 0,
        ack_seq => 0,
        properties => Properties,
        status => Status,
        afk => Afk,
        mobile => Mobile,
        presence_pid => undefined,
        presence_mref => undefined,
        socket_pid => SocketPid,
        socket_mref => monitor(process, SocketPid),
        guilds => maps:from_list([{Gid, undefined} || Gid <- GuildIds]),
        calls => #{},
        channels => Channels,
        ready => Ready,
        bot => Bot,
        ignored_events => IgnoredEvents,
        initial_guild_id => InitialGuildId,
        collected_guild_states => [],
        collected_sessions => [],
        collected_presences => [],
        relationships => load_relationships(Ready),
        suppress_presence_updates => true,
        pending_presences => [],
        guild_connect_inflight => #{}
    },

    self() ! {presence_connect, 0},
    case Bot of
        true -> self() ! bot_initial_ready;
        false -> ok
    end,
    lists:foreach(fun(Gid) -> self() ! {guild_connect, Gid, 0} end, GuildIds),
    erlang:send_after(3000, self(), premature_readiness),
    erlang:send_after(200, self(), enable_presence_updates),

    {ok, State}.

handle_call({token_verify, Token}, _From, State) ->
    TokenHash = maps:get(token_hash, State),
    HashedInput = utils:hash_token(Token),
    IsValid = HashedInput =:= TokenHash,
    {reply, IsValid, State};
handle_call({heartbeat_ack, Seq}, _From, State) ->
    AckSeq = maps:get(ack_seq, State),
    Buffer = maps:get(buffer, State),

    if
        Seq < AckSeq ->
            {reply, false, State};
        true ->
            NewBuffer = [Event || Event <- Buffer, maps:get(seq, Event) > Seq],
            {reply, true, maps:merge(State, #{ack_seq => Seq, buffer => NewBuffer})}
    end;
handle_call({resume, Seq, SocketPid}, _From, State) ->
    CurrentSeq = maps:get(seq, State),
    Buffer = maps:get(buffer, State),
    PresencePid = maps:get(presence_pid, State, undefined),
    SessionId = maps:get(id, State),
    Status = maps:get(status, State),
    Afk = maps:get(afk, State),
    Mobile = maps:get(mobile, State),

    if
        Seq > CurrentSeq ->
            {reply, invalid_seq, State};
        true ->
            MissedEvents = [Event || Event <- Buffer, maps:get(seq, Event) > Seq],
            NewState = maps:merge(State, #{
                socket_pid => SocketPid,
                socket_mref => monitor(process, SocketPid)
            }),

            case PresencePid of
                undefined ->
                    ok;
                Pid when is_pid(Pid) ->
                    gen_server:call(
                        Pid,
                        {session_connect, #{
                            session_id => SessionId,
                            status => Status,
                            afk => Afk,
                            mobile => Mobile
                        }},
                        10000
                    )
            end,

            {reply, {ok, MissedEvents}, NewState}
    end;
handle_call({get_state}, _From, State) ->
    SerializedState = serialize_state(State),
    {reply, SerializedState, State};
handle_call({voice_state_update, Data}, _From, State) ->
    session_voice:handle_voice_state_update(Data, State);
handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast({presence_update, Update}, State) ->
    PresencePid = maps:get(presence_pid, State, undefined),
    SessionId = maps:get(id, State),
    Status = maps:get(status, State),
    Afk = maps:get(afk, State),
    Mobile = maps:get(mobile, State),

    NewStatus = maps:get(status, Update, Status),
    NewAfk = maps:get(afk, Update, Afk),
    NewMobile = maps:get(mobile, Update, Mobile),

    NewState = maps:merge(State, #{status => NewStatus, afk => NewAfk, mobile => NewMobile}),
    case PresencePid of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            gen_server:cast(
                Pid,
                {presence_update, #{
                    session_id => SessionId, status => NewStatus, afk => NewAfk, mobile => NewMobile
                }}
            )
    end,
    {noreply, NewState};
handle_cast({dispatch, Event, Data}, State) ->
    session_dispatch:handle_dispatch(Event, Data, State);
handle_cast({initial_global_presences, Presences}, State) ->
    NewState =
        lists:foldl(
            fun(Presence, AccState) ->
                {noreply, UpdatedState} = session_dispatch:handle_dispatch(
                    presence_update, Presence, AccState
                ),
                UpdatedState
            end,
            State,
            Presences
        ),
    {noreply, NewState};
handle_cast({guild_join, GuildId}, State) ->
    self() ! {guild_connect, GuildId, 0},
    {noreply, State};
handle_cast({guild_leave, GuildId}, State) ->
    Guilds = maps:get(guilds, State),
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, Ref} when is_pid(Pid) ->
            demonitor(Ref),
            NewGuilds = maps:put(GuildId, undefined, Guilds),
            session_dispatch:handle_dispatch(
                guild_delete, #{<<"id">> => integer_to_binary(GuildId)}, State
            ),
            {noreply, maps:put(guilds, NewGuilds, State)};
        _ ->
            {noreply, State}
    end;
handle_cast({terminate, SessionIdHashes}, State) ->
    AuthHash = maps:get(auth_session_id_hash, State),
    DecodedHashes = [base64url:decode(Hash) || Hash <- SessionIdHashes],
    case lists:member(AuthHash, DecodedHashes) of
        true -> {stop, normal, State};
        false -> {noreply, State}
    end;
handle_cast({terminate_force}, State) ->
    {stop, normal, State};
handle_cast({call_connect, ChannelIdBin}, State) ->
    case validation:validate_snowflake(<<"channel_id">>, ChannelIdBin) of
        {ok, ChannelId} ->
            case gen_server:call(call_manager, {lookup, ChannelId}, 5000) of
                {ok, CallPid} ->
                    case gen_server:call(CallPid, {get_state}, 5000) of
                        {ok, CallData} ->
                            session_dispatch:handle_dispatch(call_create, CallData, State);
                        _ ->
                            {noreply, State}
                    end;
                not_found ->
                    {noreply, State}
            end;
        {error, _, Reason} ->
            logger:warning("[session] Invalid channel_id for call_connect: ~p", [Reason]),
            {noreply, State}
    end;
handle_cast({call_monitor, ChannelId, CallPid}, State) ->
    Calls = maps:get(calls, State, #{}),
    case maps:get(ChannelId, Calls, undefined) of
        undefined ->
            Ref = monitor(process, CallPid),
            NewCalls = maps:put(ChannelId, {CallPid, Ref}, Calls),
            {noreply, maps:put(calls, NewCalls, State)};
        {OldPid, OldRef} when OldPid =/= CallPid ->
            demonitor(OldRef, [flush]),
            Ref = monitor(process, CallPid),
            NewCalls = maps:put(ChannelId, {CallPid, Ref}, Calls),
            {noreply, maps:put(calls, NewCalls, State)};
        _ ->
            {noreply, State}
    end;
handle_cast({call_unmonitor, ChannelId}, State) ->
    Calls = maps:get(calls, State, #{}),
    case maps:get(ChannelId, Calls, undefined) of
        {_Pid, Ref} ->
            demonitor(Ref, [flush]),
            NewCalls = maps:remove(ChannelId, Calls),
            {noreply, maps:put(calls, NewCalls, State)};
        undefined ->
            {noreply, State}
    end;
handle_cast(_, State) ->
    {noreply, State}.

handle_info({presence_connect, Attempt}, State) ->
    PresencePid = maps:get(presence_pid, State, undefined),
    case PresencePid of
        undefined -> session_connection:handle_presence_connect(Attempt, State);
        _ -> {noreply, State}
    end;
handle_info({guild_connect, GuildId, Attempt}, State) ->
    session_connection:handle_guild_connect(GuildId, Attempt, State);
handle_info({guild_connect_result, GuildId, Attempt, Result}, State) ->
    session_connection:handle_guild_connect_result(GuildId, Attempt, Result, State);
handle_info({call_reconnect, ChannelId, Attempt}, State) ->
    session_connection:handle_call_reconnect(ChannelId, Attempt, State);
handle_info(enable_presence_updates, State) ->
    FlushedState = session_dispatch:flush_all_pending_presences(State),
    {noreply, maps:put(suppress_presence_updates, false, FlushedState)};
handle_info(premature_readiness, State) ->
    Ready = maps:get(ready, State),
    case Ready of
        undefined -> {noreply, State};
        _ -> session_ready:dispatch_ready_data(State)
    end;
handle_info(bot_initial_ready, State) ->
    Ready = maps:get(ready, State, undefined),
    case Ready of
        undefined -> {noreply, State};
        _ -> session_ready:dispatch_ready_data(State)
    end;
handle_info(resume_timeout, State) ->
    SocketPid = maps:get(socket_pid, State, undefined),
    case SocketPid of
        undefined -> {stop, normal, State};
        _ -> {noreply, State}
    end;
handle_info({'DOWN', Ref, process, _Pid, Reason}, State) ->
    session_monitor:handle_process_down(Ref, Reason, State);
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

serialize_state(State) ->
    #{
        id => maps:get(id, State),
        session_id => maps:get(id, State),
        user_id => integer_to_binary(maps:get(user_id, State)),
        user_data => maps:get(user_data, State),
        version => maps:get(version, State),
        seq => maps:get(seq, State),
        ack_seq => maps:get(ack_seq, State),
        properties => maps:get(properties, State),
        status => maps:get(status, State),
        afk => maps:get(afk, State),
        mobile => maps:get(mobile, State),
        buffer => maps:get(buffer, State),
        ready => maps:get(ready, State),
        guilds => maps:get(guilds, State, #{}),
        collected_guild_states => maps:get(collected_guild_states, State),
        collected_sessions => maps:get(collected_sessions, State),
        collected_presences => maps:get(collected_presences, State, [])
    }.

build_ignored_events_map(Events) when is_list(Events) ->
    maps:from_list([{Event, true} || Event <- Events]);
build_ignored_events_map(_) ->
    #{}.

load_private_channels(Ready) when is_map(Ready) ->
    PrivateChannels = maps:get(<<"private_channels">>, Ready, []),
    maps:from_list([
        {type_conv:extract_id(Channel, <<"id">>), Channel}
     || Channel <- PrivateChannels
    ]);
load_private_channels(_) ->
    #{}.

load_relationships(Ready) when is_map(Ready) ->
    Relationships = maps:get(<<"relationships">>, Ready, []),
    maps:from_list(
        [
            {type_conv:extract_id(Rel, <<"id">>), maps:get(<<"type">>, Rel, 0)}
         || Rel <- Relationships, type_conv:extract_id(Rel, <<"id">>) =/= undefined
        ]
    );
load_relationships(_) ->
    #{}.

ensure_bot_ready_map(undefined) ->
    #{<<"guilds">> => []};
ensure_bot_ready_map(Ready) when is_map(Ready) ->
    maps:merge(Ready, #{<<"guilds">> => []});
ensure_bot_ready_map(_) ->
    #{<<"guilds">> => []}.
