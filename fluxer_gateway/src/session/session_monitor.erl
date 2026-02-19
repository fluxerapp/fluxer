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

-module(session_monitor).

-export([
    handle_process_down/3,
    find_guild_by_ref/2,
    find_call_by_ref/2
]).

-type session_state() :: session:session_state().
-type guild_id() :: session:guild_id().
-type channel_id() :: session:channel_id().
-type guild_ref() :: {pid(), reference()} | undefined | cached_unavailable.
-type call_ref() :: {pid(), reference()}.

-spec handle_process_down(reference(), term(), session_state()) ->
    {noreply, session_state()}.
handle_process_down(Ref, Reason, State) ->
    SocketRef = maps:get(socket_mref, State, undefined),
    PresenceRef = maps:get(presence_mref, State, undefined),
    Guilds = maps:get(guilds, State),
    Calls = maps:get(calls, State, #{}),
    case Ref of
        SocketRef when Ref =:= SocketRef ->
            handle_socket_down(State);
        PresenceRef when Ref =:= PresenceRef ->
            handle_presence_down(State);
        _ ->
            case find_guild_by_ref(Ref, Guilds) of
                {ok, GuildId} ->
                    handle_guild_down(GuildId, Reason, State, Guilds);
                not_found ->
                    case find_call_by_ref(Ref, Calls) of
                        {ok, ChannelId} ->
                            handle_call_down(ChannelId, Reason, State, Calls);
                        not_found ->
                            {noreply, State}
                    end
            end
    end.

-spec handle_socket_down(session_state()) -> {noreply, session_state()}.
handle_socket_down(State) ->
    self() ! {presence_update, #{status => offline}},
    erlang:send_after(10000, self(), resume_timeout),
    {noreply, maps:merge(State, #{socket_pid => undefined, socket_mref => undefined})}.

-spec handle_presence_down(session_state()) -> {noreply, session_state()}.
handle_presence_down(State) ->
    self() ! {presence_connect, 0},
    {noreply, maps:put(presence_pid, undefined, State)}.

-spec handle_guild_down(guild_id(), term(), session_state(), #{guild_id() => guild_ref()}) ->
    {noreply, session_state()}.
handle_guild_down(GuildId, killed, State, _Guilds) ->
    gen_server:cast(self(), {guild_leave, GuildId}),
    {noreply, State};
handle_guild_down(GuildId, _Reason, State, Guilds) ->
    GuildDeleteData = #{
        <<"id">> => integer_to_binary(GuildId),
        <<"unavailable">> => true
    },
    {noreply, UpdatedState} = session_dispatch:handle_dispatch(
        guild_delete, GuildDeleteData, State
    ),
    NewGuilds = maps:put(GuildId, undefined, Guilds),
    erlang:send_after(1000, self(), {guild_connect, GuildId, 0}),
    {noreply, maps:put(guilds, NewGuilds, UpdatedState)}.

-spec handle_call_down(channel_id(), term(), session_state(), #{channel_id() => call_ref()}) ->
    {noreply, session_state()}.
handle_call_down(ChannelId, killed, State, Calls) ->
    NewCalls = maps:remove(ChannelId, Calls),
    {noreply, maps:put(calls, NewCalls, State)};
handle_call_down(ChannelId, _Reason, State, Calls) ->
    CallDeleteData = #{
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"unavailable">> => true
    },
    {noreply, UpdatedState} = session_dispatch:handle_dispatch(call_delete, CallDeleteData, State),
    NewCalls = maps:put(ChannelId, undefined, Calls),
    erlang:send_after(1000, self(), {call_reconnect, ChannelId, 0}),
    {noreply, maps:put(calls, NewCalls, UpdatedState)}.

-spec find_guild_by_ref(reference(), #{guild_id() => guild_ref()}) ->
    {ok, guild_id()} | not_found.
find_guild_by_ref(Ref, Guilds) ->
    find_by_ref(Ref, Guilds).

-spec find_call_by_ref(reference(), #{channel_id() => call_ref()}) ->
    {ok, channel_id()} | not_found.
find_call_by_ref(Ref, Calls) ->
    find_by_ref(Ref, Calls).

-spec find_by_ref(reference(), #{integer() => {pid(), reference()} | undefined}) ->
    {ok, integer()} | not_found.
find_by_ref(Ref, Map) ->
    maps:fold(
        fun
            (Id, {_Pid, R}, _) when R =:= Ref -> {ok, Id};
            (_, _, Acc) -> Acc
        end,
        not_found,
        Map
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

find_by_ref_test() ->
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Ref3 = make_ref(),
    Map = #{
        123 => {self(), Ref1},
        456 => {self(), Ref2},
        789 => undefined
    },
    ?assertEqual({ok, 123}, find_by_ref(Ref1, Map)),
    ?assertEqual({ok, 456}, find_by_ref(Ref2, Map)),
    ?assertEqual(not_found, find_by_ref(Ref3, Map)),
    ok.

find_guild_by_ref_test() ->
    Ref = make_ref(),
    Guilds = #{100 => {self(), Ref}, 200 => undefined},
    ?assertEqual({ok, 100}, find_guild_by_ref(Ref, Guilds)),
    ?assertEqual(not_found, find_guild_by_ref(make_ref(), Guilds)),
    ok.

find_call_by_ref_test() ->
    Ref = make_ref(),
    Calls = #{300 => {self(), Ref}},
    ?assertEqual({ok, 300}, find_call_by_ref(Ref, Calls)),
    ?assertEqual(not_found, find_call_by_ref(make_ref(), Calls)),
    ok.

-spec build_test_session_state(guild_id(), #{guild_id() => guild_ref()}) -> session_state().
build_test_session_state(GuildId, Guilds) ->
    #{
        id => <<"session-monitor-test">>,
        user_id => 1,
        user_data => #{},
        custom_status => null,
        version => 1,
        token_hash => <<>>,
        auth_session_id_hash => <<>>,
        buffer => [],
        seq => 0,
        ack_seq => 0,
        properties => #{},
        status => online,
        afk => false,
        mobile => false,
        presence_pid => undefined,
        presence_mref => undefined,
        socket_pid => undefined,
        socket_mref => undefined,
        guilds => Guilds,
        calls => #{},
        channels => #{},
        ready => undefined,
        bot => false,
        ignored_events => #{},
        initial_guild_id => GuildId,
        collected_guild_states => [],
        collected_sessions => [],
        collected_presences => [],
        relationships => #{},
        suppress_presence_updates => false,
        pending_presences => [],
        guild_connect_inflight => #{},
        voice_queue => queue:new(),
        voice_queue_timer => undefined,
        debounce_reactions => false,
        reaction_buffer => [],
        reaction_buffer_timer => undefined
    }.

handle_guild_down_normal_marks_unavailable_and_schedules_reconnect_test() ->
    GuildId = 50001,
    GuildRef = make_ref(),
    GuildPid = spawn(fun() -> receive stop -> ok end end),
    Guilds = #{GuildId => {GuildPid, GuildRef}},
    State0 = build_test_session_state(GuildId, Guilds),
    {noreply, State1} = handle_guild_down(GuildId, normal, State0, Guilds),
    UpdatedGuilds = maps:get(guilds, State1),
    ?assertEqual(undefined, maps:get(GuildId, UpdatedGuilds)),
    Buffer = maps:get(buffer, State1),
    ?assertEqual(1, length(Buffer)),
    [Event] = Buffer,
    ?assertEqual(guild_delete, maps:get(event, Event)),
    EventData = maps:get(data, Event),
    ?assertEqual(integer_to_binary(GuildId), maps:get(<<"id">>, EventData)),
    ?assertEqual(true, maps:get(<<"unavailable">>, EventData)),
    receive
        {guild_connect, GuildId, 0} -> ok
    after 2000 ->
        ?assert(false)
    end,
    GuildPid ! stop.

handle_guild_down_shutdown_marks_unavailable_and_schedules_reconnect_test() ->
    GuildId = 50002,
    GuildRef = make_ref(),
    GuildPid = spawn(fun() -> receive stop -> ok end end),
    Guilds = #{GuildId => {GuildPid, GuildRef}},
    State0 = build_test_session_state(GuildId, Guilds),
    {noreply, State1} = handle_guild_down(GuildId, shutdown, State0, Guilds),
    UpdatedGuilds = maps:get(guilds, State1),
    ?assertEqual(undefined, maps:get(GuildId, UpdatedGuilds)),
    Buffer = maps:get(buffer, State1),
    ?assertEqual(1, length(Buffer)),
    [Event] = Buffer,
    ?assertEqual(guild_delete, maps:get(event, Event)),
    EventData = maps:get(data, Event),
    ?assertEqual(true, maps:get(<<"unavailable">>, EventData)),
    receive
        {guild_connect, GuildId, 0} -> ok
    after 2000 ->
        ?assert(false)
    end,
    GuildPid ! stop.

handle_guild_down_crash_marks_unavailable_and_schedules_reconnect_test() ->
    GuildId = 50003,
    GuildRef = make_ref(),
    GuildPid = spawn(fun() -> receive stop -> ok end end),
    Guilds = #{GuildId => {GuildPid, GuildRef}},
    State0 = build_test_session_state(GuildId, Guilds),
    {noreply, State1} = handle_guild_down(GuildId, {error, something_went_wrong}, State0, Guilds),
    UpdatedGuilds = maps:get(guilds, State1),
    ?assertEqual(undefined, maps:get(GuildId, UpdatedGuilds)),
    Buffer = maps:get(buffer, State1),
    ?assertEqual(1, length(Buffer)),
    [Event] = Buffer,
    ?assertEqual(guild_delete, maps:get(event, Event)),
    EventData = maps:get(data, Event),
    ?assertEqual(true, maps:get(<<"unavailable">>, EventData)),
    receive
        {guild_connect, GuildId, 0} -> ok
    after 2000 ->
        ?assert(false)
    end,
    GuildPid ! stop.

handle_guild_down_killed_sends_permanent_guild_leave_test() ->
    GuildId = 50004,
    GuildRef = make_ref(),
    GuildPid = spawn(fun() -> receive stop -> ok end end),
    Guilds = #{GuildId => {GuildPid, GuildRef}},
    State0 = build_test_session_state(GuildId, Guilds),
    {noreply, State1} = handle_guild_down(GuildId, killed, State0, Guilds),
    ?assertEqual([], maps:get(buffer, State1, [])),
    UpdatedGuilds = maps:get(guilds, State1),
    ?assertEqual({GuildPid, GuildRef}, maps:get(GuildId, UpdatedGuilds)),
    receive
        {guild_connect, GuildId, 0} ->
            ?assert(false)
    after 200 ->
        ok
    end,
    receive
        {'$gen_cast', {guild_leave, GuildId}} -> ok
    after 200 ->
        ?assert(false)
    end,
    GuildPid ! stop.

handle_process_down_guild_normal_exit_dispatches_unavailable_test() ->
    GuildId = 50005,
    GuildPid = spawn(fun() -> receive stop -> ok end end),
    GuildRef = monitor(process, GuildPid),
    Guilds = #{GuildId => {GuildPid, GuildRef}},
    State0 = build_test_session_state(GuildId, Guilds),
    GuildPid ! stop,
    receive
        {'DOWN', GuildRef, process, GuildPid, Reason} ->
            {noreply, State1} = handle_process_down(GuildRef, Reason, State0),
            Buffer = maps:get(buffer, State1),
            ?assertEqual(1, length(Buffer)),
            [Event] = Buffer,
            ?assertEqual(guild_delete, maps:get(event, Event)),
            EventData = maps:get(data, Event),
            ?assertEqual(true, maps:get(<<"unavailable">>, EventData)),
            receive
                {guild_connect, GuildId, 0} -> ok
            after 2000 ->
                ?assert(false)
            end
    after 2000 ->
        ?assert(false)
    end.

-endif.
