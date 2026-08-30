%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_ready_dispatch).
-typing([eqwalizer]).

-export([
    dispatch_ready_to_socket/1,
    dispatch_event/3,
    get_private_channels/1,
    dispatch_call_creates_for_channels/3,
    guild_state_event/1
]).

-export_type([session_state/0, channel_id/0]).

-type session_state() :: session:session_state().
-type guild_id() :: session:guild_id().
-type channel_id() :: session:channel_id().

-spec dispatch_ready_to_socket(session_state()) -> {noreply, session_state()}.
dispatch_ready_to_socket(State) ->
    #{
        id := SessionId,
        version := Version,
        bot := IsBot,
        guilds := Guilds,
        collected_guild_states := CollectedGuilds,
        collected_sessions := CollectedSessions
    } = State,
    GwTimings0 = gateway_timings:from_state(State),
    DispatchStartedAt = gateway_timings:start(),
    PresencesStartedAt = gateway_timings:start(),
    CollectedPresences = session_ready_collect:collect_ready_presences(State, CollectedGuilds),
    PresencesSpan = gateway_timings:span(
        <<"session_ready_collect:collect_ready_presences/2">>, PresencesStartedAt
    ),
    UsersStartedAt = gateway_timings:start(),
    Users = session_ready_collect:collect_ready_users(State, CollectedGuilds),
    UsersSpan = gateway_timings:span(
        <<"session_ready_collect:collect_ready_users/2">>, UsersStartedAt
    ),
    BuildStartedAt = gateway_timings:start(),
    FinalReadyData0 = build_final_ready_data(
        State,
        CollectedGuilds,
        CollectedSessions,
        CollectedPresences,
        Users,
        Guilds,
        Version,
        SessionId,
        IsBot
    ),
    BuildSpan = gateway_timings:span(
        <<"session_ready_dispatch:build_final_ready_data/9">>, BuildStartedAt
    ),
    GwTimings = gateway_timings:record_function(
        dispatch_ready_to_socket,
        <<"session_ready_dispatch:dispatch_ready_to_socket/1">>,
        DispatchStartedAt,
        #{children => [PresencesSpan, UsersSpan, BuildSpan]},
        GwTimings0
    ),
    FinalReadyData = apply_timings_visibility(
        is_staff_session(State), GwTimings, FinalReadyData0
    ),
    StateWithTimings = gateway_timings:put_state(GwTimings, State),
    StateAfterReady = dispatch_event(ready, FinalReadyData, StateWithTimings),
    StateAfterGuilds = dispatch_bot_guild_creates(
        IsBot, CollectedGuilds, Guilds, StateAfterReady
    ),
    schedule_call_creates(StateAfterGuilds, SessionId),
    FinalState = StateAfterGuilds#{
        ready => undefined,
        collected_guild_states => [],
        collected_sessions => [],
        collected_presences => []
    },
    erlang:garbage_collect(),
    {noreply, FinalState}.

-spec is_staff_session(session_state()) -> boolean().
is_staff_session(#{is_staff := true}) ->
    true;
is_staff_session(_State) ->
    false.

-spec apply_timings_visibility(boolean(), gateway_timings:recorder(), map()) -> map().
apply_timings_visibility(true, GwTimings, ReadyData) ->
    ReadyData#{<<"_timings_gw">> => gateway_timings_payload:finalize(GwTimings)};
apply_timings_visibility(false, _GwTimings, ReadyData) ->
    maps:without([<<"_timings">>], ReadyData).

-spec build_final_ready_data(
    session_state(),
    [map()],
    [map()],
    [map()],
    [map()],
    map(),
    non_neg_integer(),
    binary(),
    boolean()
) -> map().
build_final_ready_data(
    State,
    CollectedGuilds,
    CollectedSessions,
    CollectedPresences,
    Users,
    Guilds,
    Version,
    SessionId,
    IsBot
) ->
    ReadyData = prepare_ready_base(State, IsBot),
    AllGuildStates = build_all_guild_states(CollectedGuilds, Guilds),
    GuildsForReady = guilds_for_ready_payload(IsBot, AllGuildStates),
    FinalReadyData = ReadyData#{
        <<"guilds">> => GuildsForReady,
        <<"sessions">> => CollectedSessions,
        <<"presences">> => CollectedPresences,
        <<"users">> => Users,
        <<"version">> => Version,
        <<"session_id">> => SessionId
    },
    gateway_sharding:maybe_put_ready_shard(FinalReadyData, maps:get(shard, State, undefined)).

-spec prepare_ready_base(session_state(), boolean()) -> map().
prepare_ready_base(#{ready := undefined}, _IsBot) ->
    #{<<"guilds">> => []};
prepare_ready_base(#{ready := Ready}, IsBot) ->
    Stripped = session_ready_collect:strip_user_from_relationships(Ready),
    case IsBot of
        true -> Stripped#{<<"guilds">> => []};
        false -> Stripped
    end.

-spec build_all_guild_states([map()], map()) -> [map()].
build_all_guild_states(CollectedGuilds, Guilds) ->
    StrippedGuilds = strip_collected_guild_states(CollectedGuilds),
    StrippedGuildIds = collected_guild_id_map(StrippedGuilds),
    UnavailableGuilds = maps:fold(
        fun(GuildId, Value, Acc) ->
            collect_unavailable_placeholder(GuildId, Value, StrippedGuildIds, Acc)
        end,
        [],
        Guilds
    ),
    StrippedGuilds ++ UnavailableGuilds.

-spec strip_collected_guild_states([map()]) -> [map()].
strip_collected_guild_states(CollectedGuilds) ->
    [
        session_ready_collect:strip_users_from_guild_members(G)
     || G <- lists:reverse(CollectedGuilds)
    ].

-spec collect_unavailable_placeholder(guild_id(), term(), #{guild_id() => true}, [map()]) ->
    [map()].
collect_unavailable_placeholder(GuildId, _Value, StrippedGuildIds, Acc) ->
    maybe_add_unavailable_guild(GuildId, StrippedGuildIds, Acc).

-spec maybe_add_unavailable_guild(guild_id(), #{guild_id() => true}, [map()]) -> [map()].
maybe_add_unavailable_guild(GuildId, ExistingGuildIds, Acc) ->
    case maps:is_key(GuildId, ExistingGuildIds) of
        true -> Acc;
        false -> [#{<<"id">> => integer_to_binary(GuildId), <<"unavailable">> => true} | Acc]
    end.

-spec collected_guild_id_map([map()]) -> #{guild_id() => true}.
collected_guild_id_map(GuildStates) ->
    lists:foldl(fun accumulate_guild_id/2, #{}, GuildStates).

-spec accumulate_guild_id(map(), #{guild_id() => true}) -> #{guild_id() => true}.
accumulate_guild_id(GuildState, Acc) ->
    case guild_id_from_state(GuildState) of
        {ok, GuildId} -> Acc#{GuildId => true};
        error -> Acc
    end.

-spec guild_id_from_state(map()) -> {ok, guild_id()} | error.
guild_id_from_state(GuildState) ->
    case maps:get(<<"id">>, GuildState, undefined) of
        GuildId when is_integer(GuildId) -> {ok, GuildId};
        GuildIdBin when is_binary(GuildIdBin) -> parse_guild_id_binary(GuildIdBin);
        _Other -> error
    end.

-spec parse_guild_id_binary(binary()) -> {ok, guild_id()} | error.
parse_guild_id_binary(GuildIdBin) ->
    try binary_to_integer(GuildIdBin) of
        GuildId -> {ok, GuildId}
    catch
        error:badarg -> error
    end.

-spec guilds_for_ready_payload(boolean(), [map()]) -> [map()].
guilds_for_ready_payload(true, _AllGuildStates) -> [];
guilds_for_ready_payload(false, AllGuildStates) -> AllGuildStates.

-spec dispatch_bot_guild_creates(boolean(), [map()], map(), session_state()) -> session_state().
dispatch_bot_guild_creates(false, _CollectedGuilds, _Guilds, State) ->
    State;
dispatch_bot_guild_creates(true, CollectedGuilds, _Guilds, State) ->
    AllGuildStates = strip_collected_guild_states(CollectedGuilds),
    lists:foldl(
        fun(GuildState, AccState) ->
            dispatch_event(guild_state_event(GuildState), GuildState, AccState)
        end,
        State,
        AllGuildStates
    ).

-spec schedule_call_creates(session_state(), binary()) -> ok.
schedule_call_creates(State, SessionId) ->
    PrivateChannels = get_private_channels(State),
    SessionPid = self(),
    spawn(fun() ->
        safe_dispatch_call_creates(PrivateChannels, SessionId, SessionPid)
    end),
    ok.

-spec safe_dispatch_call_creates(#{channel_id() => map()}, binary(), pid()) -> ok.
safe_dispatch_call_creates(PrivateChannels, SessionId, SessionPid) ->
    try
        dispatch_call_creates_for_channels(PrivateChannels, SessionId, SessionPid)
    catch
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec guild_state_event(map()) -> guild_create | guild_delete.
guild_state_event(GuildState) ->
    case maps:get(<<"unavailable">>, GuildState, false) of
        true -> guild_delete;
        _ -> guild_create
    end.

-spec dispatch_event(atom(), map(), session_state()) -> session_state().
dispatch_event(Event, Data, #{seq := Seq, socket_pid := SocketPid} = State) ->
    NewSeq = Seq + 1,
    send_dispatch_to_socket(SocketPid, Event, Data, NewSeq),
    State#{seq => NewSeq}.

-spec send_dispatch_to_socket(pid() | undefined, atom(), map(), non_neg_integer()) -> ok.
send_dispatch_to_socket(undefined, _Event, _Data, _Seq) ->
    ok;
send_dispatch_to_socket(Pid, Event, Data, Seq) when is_pid(Pid) ->
    Pid ! {dispatch, Event, guild_data_wire:payload(Data), Seq},
    ok.

-spec get_private_channels(session_state()) -> #{channel_id() => map()}.
get_private_channels(State) ->
    Channels = maps:get(channels, State, #{}),
    maps:filter(
        fun(_ChannelId, Channel) ->
            Type = maps:get(<<"type">>, Channel, undefined),
            Type =:= 1 orelse Type =:= 3
        end,
        Channels
    ).

-spec dispatch_call_creates_for_channels(#{channel_id() => map()}, binary(), pid()) -> ok.
dispatch_call_creates_for_channels(PrivateChannels, SessionId, SessionPid) ->
    maps:foreach(
        fun(ChannelId, _Channel) ->
            dispatch_call_create_for_channel(ChannelId, SessionId, SessionPid)
        end,
        PrivateChannels
    ).

-spec dispatch_call_create_for_channel(channel_id(), binary(), pid()) -> ok.
dispatch_call_create_for_channel(ChannelId, SessionId, SessionPid) ->
    try
        case call_manager:lookup(ChannelId) of
            {ok, CallPid} ->
                dispatch_call_create_from_pid(CallPid, ChannelId, SessionId, SessionPid);
            _ ->
                ok
        end
    catch
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec dispatch_call_create_from_pid(pid(), channel_id(), binary(), pid()) -> ok.
dispatch_call_create_from_pid(CallPid, ChannelId, _SessionId, SessionPid) ->
    try gen_server:call(CallPid, {get_state}, 5000) of
        {ok, CallData} ->
            gen_server:cast(SessionPid, {call_monitor, ChannelId, CallPid}),
            DispatchData = guild_data_wire:payload(CallData),
            gateway_dispatch_relay:dispatch_direct(SessionPid, call_create, DispatchData),
            ok;
        _ ->
            ok
    catch
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

dispatch_call_create_from_pid_always_casts_to_session_test() ->
    ChannelId = 1234,
    SessionId = <<"session-ready-call-test">>,
    CallData = #{
        channel_id => integer_to_binary(ChannelId),
        message_id => <<"9001">>,
        region => null,
        ringing => [],
        recipients => [],
        voice_states => [],
        created_at => erlang:system_time(millisecond)
    },
    CallPid = spawn(fun() -> call_state_stub_loop(CallData) end),
    ok = dispatch_call_create_from_pid(CallPid, ChannelId, SessionId, self()),
    receive
        {'$gen_cast', {call_monitor, ChannelId, CallPid}} ->
            ok
    after 1000 ->
        ?assert(false, call_monitor_not_cast_to_session)
    end,
    receive
        {'$gen_cast', {dispatch, call_create, DispatchData}} ->
            ?assertEqual(guild_data_wire:payload(CallData), DispatchData)
    after 1000 ->
        ?assert(false, call_create_not_cast_to_session)
    end,
    exit(CallPid, kill),
    ok.

-spec call_state_stub_loop(map()) -> ok.
call_state_stub_loop(CallData) ->
    receive
        {'$gen_call', From, {get_state}} ->
            gen_server:reply(From, {ok, CallData}),
            call_state_stub_loop(CallData);
        _ ->
            call_state_stub_loop(CallData)
    after 30000 -> ok
    end.

-endif.
