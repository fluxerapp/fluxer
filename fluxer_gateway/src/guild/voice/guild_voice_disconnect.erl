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

-module(guild_voice_disconnect).

-export([handle_voice_disconnect/5]).
-export([force_disconnect_participant/4]).
-export([disconnect_voice_user/2]).
-export([disconnect_voice_user_if_in_channel/2]).
-export([disconnect_all_voice_users_in_channel/2]).
-export([cleanup_virtual_channel_access_for_user/2]).
-export([recently_disconnected_voice_states/1]).
-export([clear_recently_disconnected/2]).
-export([clear_recently_disconnected_for_channel/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type guild_state() :: map().
-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.

-spec handle_voice_disconnect(
    binary() | undefined,
    term(),
    integer(),
    voice_state_map() | term(),
    guild_state()
) -> {reply, map(), guild_state()}.
handle_voice_disconnect(undefined, _SessionId, _UserId, _VoiceStates, State) ->
    {reply, gateway_errors:error(voice_missing_connection_id), State};
handle_voice_disconnect(ConnectionId, _SessionId, UserId, VoiceStates0, State) ->
    VoiceStates = voice_state_utils:ensure_voice_states(VoiceStates0),
    case maps:get(ConnectionId, VoiceStates, undefined) of
        undefined ->
            State1 = clear_pending_voice_connection(ConnectionId, State),
            {reply, #{success => true}, State1};
        OldVoiceState ->
            case guild_voice_state:user_matches_voice_state(OldVoiceState, UserId) of
                false ->
                    {reply, gateway_errors:error(voice_user_mismatch), State};
                true ->
                    case
                        {
                            voice_state_utils:voice_state_guild_id(OldVoiceState),
                            voice_state_utils:voice_state_channel_id(OldVoiceState)
                        }
                    of
                        {undefined, _} ->
                            {reply, gateway_errors:error(voice_invalid_state), State};
                        {_, undefined} ->
                            {reply, gateway_errors:error(voice_invalid_state), State};
                        {GuildId, ChannelId} ->
                            maybe_force_disconnect(GuildId, ChannelId, UserId, ConnectionId, State),
                            NewVoiceStates = maps:remove(ConnectionId, VoiceStates),
                            NewState0 = maps:put(voice_states, NewVoiceStates, State),
                            NewState = clear_recently_disconnected(ConnectionId, NewState0),
                            voice_state_utils:broadcast_disconnects(
                                #{ConnectionId => OldVoiceState}, NewState
                            ),
                            FinalState = maybe_cleanup_after_disconnect(
                                UserId, ChannelId, NewState
                            ),
                            {reply, #{success => true}, FinalState}
                    end
            end
    end.

-spec clear_pending_voice_connection(binary(), guild_state()) -> guild_state().
clear_pending_voice_connection(ConnectionId, State) ->
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    case maps:is_key(ConnectionId, PendingConnections) of
        false ->
            State;
        true ->
            NewPendingConnections = maps:remove(ConnectionId, PendingConnections),
            maps:put(pending_voice_connections, NewPendingConnections, State)
    end.

-spec maybe_cleanup_after_disconnect(integer(), integer(), guild_state()) -> guild_state().
maybe_cleanup_after_disconnect(UserId, ChannelId, State) ->
    case
        guild_virtual_channel_access:is_pending_join(UserId, ChannelId, State) orelse
            guild_virtual_channel_access:has_preserve(UserId, ChannelId, State) orelse
            guild_virtual_channel_access:is_move_pending(UserId, ChannelId, State)
    of
        true ->
            State;
        false ->
            cleanup_virtual_channel_access_for_user(UserId, State)
    end.

-spec disconnect_voice_user(map(), guild_state()) -> {reply, map(), guild_state()}.
disconnect_voice_user(#{user_id := UserId} = Request, State) ->
    ConnectionId = maps:get(connection_id, Request, null),
    VoiceStates = voice_state_utils:voice_states(State),
    case ConnectionId of
        null ->
            UserVoiceStates = voice_state_utils:filter_voice_states(VoiceStates, fun(_, V) ->
                voice_state_utils:voice_state_user_id(V) =:= UserId
            end),
            case maps:size(UserVoiceStates) of
                0 ->
                    State1 = clear_pending_voice_connections_for_user(UserId, State),
                    {reply, #{success => true}, State1};
                _ ->
                    maybe_force_disconnect_voice_states(UserVoiceStates, State),
                    NewVoiceStates = voice_state_utils:drop_voice_states(
                        UserVoiceStates, VoiceStates
                    ),
                    NewState0 = maps:put(voice_states, NewVoiceStates, State),
                    NewState = maps:fold(
                        fun(ConnId, _, AccState) ->
                            clear_recently_disconnected(ConnId, AccState)
                        end,
                        NewState0,
                        UserVoiceStates
                    ),
                    voice_state_utils:broadcast_disconnects(UserVoiceStates, NewState),
                    FinalState = cleanup_virtual_channel_access_for_user(UserId, NewState),
                    {reply, #{success => true}, FinalState}
            end;
        SpecificConnection ->
            case maps:get(SpecificConnection, VoiceStates, undefined) of
                undefined ->
                    State1 = clear_pending_voice_connection(SpecificConnection, State),
                    {reply, #{success => true}, State1};
                VoiceState ->
                    case voice_state_utils:voice_state_user_id(VoiceState) of
                        undefined ->
                            {reply, gateway_errors:error(voice_invalid_state), State};
                        VoiceStateUserId when VoiceStateUserId =:= UserId ->
                            maybe_force_disconnect_voice_state(SpecificConnection, VoiceState, State),
                            NewVoiceStates = maps:remove(SpecificConnection, VoiceStates),
                            NewState0 = maps:put(voice_states, NewVoiceStates, State),
                            NewState = clear_recently_disconnected(SpecificConnection, NewState0),
                            voice_state_utils:broadcast_disconnects(
                                #{SpecificConnection => VoiceState}, NewState
                            ),
                            FinalState = cleanup_virtual_channel_access_for_user(UserId, NewState),
                            {reply, #{success => true}, FinalState};
                        _ ->
                            {reply, gateway_errors:error(voice_user_mismatch), State}
                    end
            end
    end.

-spec clear_pending_voice_connections_for_user(integer(), guild_state()) -> guild_state().
clear_pending_voice_connections_for_user(UserId, State) ->
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    FilteredPending = maps:filter(
        fun(_ConnId, PendingData) ->
            PendingUserId = maps:get(user_id, PendingData, undefined),
            PendingUserId =/= UserId
        end,
        PendingConnections
    ),
    maps:put(pending_voice_connections, FilteredPending, State).

-spec disconnect_voice_user_if_in_channel(map(), guild_state()) -> {reply, map(), guild_state()}.
disconnect_voice_user_if_in_channel(
    #{user_id := UserId, expected_channel_id := ExpectedChannelId} = Request,
    State
) ->
    ConnectionId = maps:get(connection_id, Request, undefined),
    VoiceStates = voice_state_utils:voice_states(State),
    case ConnectionId of
        undefined ->
            UserVoiceStates = voice_state_utils:filter_voice_states(VoiceStates, fun(_, V) ->
                voice_state_utils:voice_state_user_id(V) =:= UserId andalso
                    voice_state_utils:voice_state_channel_id(V) =:= ExpectedChannelId
            end),
            case maps:size(UserVoiceStates) of
                0 ->
                    State1 = clear_pending_voice_connections_for_user_channel(
                        UserId, ExpectedChannelId, State
                    ),
                    {reply,
                        #{
                            success => true,
                            ignored => true,
                            reason => <<"not_in_expected_channel">>
                        },
                        State1};
                _ ->
                    NewVoiceStates = voice_state_utils:drop_voice_states(
                        UserVoiceStates, VoiceStates
                    ),
                    NewState0 = maps:put(voice_states, NewVoiceStates, State),
                    NewState = cache_recently_disconnected(UserVoiceStates, NewState0),
                    voice_state_utils:broadcast_disconnects(UserVoiceStates, NewState),
                    {reply, #{success => true}, NewState}
            end;
        ConnId ->
            case maps:get(ConnId, VoiceStates, undefined) of
                undefined ->
                    State1 = clear_pending_voice_connection(ConnId, State),
                    {reply,
                        #{success => true, ignored => true, reason => <<"connection_not_found">>},
                        State1};
                VoiceState ->
                    case
                        {
                            voice_state_utils:voice_state_user_id(VoiceState),
                            voice_state_utils:voice_state_channel_id(VoiceState)
                        }
                    of
                        {UserId, ExpectedChannelId} ->
                            NewVoiceStates = maps:remove(ConnId, VoiceStates),
                            NewState0 = maps:put(voice_states, NewVoiceStates, State),
                            NewState = cache_recently_disconnected(
                                #{ConnId => VoiceState}, NewState0
                            ),
                            voice_state_utils:broadcast_disconnects(
                                #{ConnId => VoiceState}, NewState
                            ),
                            {reply, #{success => true}, NewState};
                        _ ->
                            {reply,
                                #{
                                    success => true,
                                    ignored => true,
                                    reason => <<"user_or_channel_mismatch">>
                                },
                                State}
                    end
            end
    end.

-spec clear_pending_voice_connections_for_user_channel(integer(), integer(), guild_state()) ->
    guild_state().
clear_pending_voice_connections_for_user_channel(UserId, ChannelId, State) ->
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    FilteredPending = maps:filter(
        fun(_ConnId, PendingData) ->
            PendingUserId = maps:get(user_id, PendingData, undefined),
            PendingChannelId = maps:get(channel_id, PendingData, undefined),
            not (PendingUserId =:= UserId andalso PendingChannelId =:= ChannelId)
        end,
        PendingConnections
    ),
    maps:put(pending_voice_connections, FilteredPending, State).

-spec clear_pending_voice_connections_for_channel(integer(), guild_state()) -> guild_state().
clear_pending_voice_connections_for_channel(ChannelId, State) ->
    PendingConnections = maps:get(pending_voice_connections, State, #{}),
    FilteredPending = maps:filter(
        fun(_ConnId, PendingData) ->
            PendingChannelId = maps:get(channel_id, PendingData, undefined),
            PendingChannelId =/= ChannelId
        end,
        PendingConnections
    ),
    maps:put(pending_voice_connections, FilteredPending, State).

-spec disconnect_all_voice_users_in_channel(map(), guild_state()) -> {reply, map(), guild_state()}.
disconnect_all_voice_users_in_channel(#{channel_id := ChannelId}, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    ChannelVoiceStates = voice_state_utils:filter_voice_states(VoiceStates, fun(_, V) ->
        voice_state_utils:voice_state_channel_id(V) =:= ChannelId
    end),
    State1 = clear_pending_voice_connections_for_channel(ChannelId, State),
    case maps:size(ChannelVoiceStates) of
        0 ->
            {reply, #{success => true, disconnected_count => 0}, State1};
        Count ->
            maybe_force_disconnect_voice_states(ChannelVoiceStates, State1),
            NewVoiceStates = voice_state_utils:drop_voice_states(ChannelVoiceStates, VoiceStates),
            NewState0 = maps:put(voice_states, NewVoiceStates, State1),
            NewState = clear_recently_disconnected_for_channel(ChannelId, NewState0),
            voice_state_utils:broadcast_disconnects(ChannelVoiceStates, NewState),
            {reply, #{success => true, disconnected_count => Count}, NewState}
    end.

-spec maybe_force_disconnect_voice_states(voice_state_map(), guild_state()) -> ok.
maybe_force_disconnect_voice_states(VoiceStates, State) ->
    maps:foreach(
        fun(ConnId, VoiceState) ->
            maybe_force_disconnect_voice_state(ConnId, VoiceState, State)
        end,
        VoiceStates
    ),
    ok.

-spec maybe_force_disconnect_voice_state(binary(), voice_state(), guild_state()) -> ok.
maybe_force_disconnect_voice_state(ConnectionId, VoiceState, State) ->
    UserId = voice_state_utils:voice_state_user_id(VoiceState),
    ChannelId = voice_state_utils:voice_state_channel_id(VoiceState),
    GuildId = resolve_guild_id(VoiceState, State),
    case {GuildId, ChannelId, UserId} of
        {GId, CId, UId} when is_integer(GId), is_integer(CId), is_integer(UId) ->
            _ = maybe_force_disconnect(GId, CId, UId, ConnectionId, State),
            ok;
        _ ->
            ok
    end.

-spec resolve_guild_id(voice_state(), guild_state()) -> integer() | undefined.
resolve_guild_id(VoiceState, State) ->
    case voice_state_utils:voice_state_guild_id(VoiceState) of
        undefined ->
            map_utils:get_integer(State, id, undefined);
        GuildId ->
            GuildId
    end.

-spec force_disconnect_participant(integer(), integer(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
force_disconnect_participant(GuildId, ChannelId, UserId, ConnectionId) ->
    Req = voice_utils:build_force_disconnect_rpc_request(GuildId, ChannelId, UserId, ConnectionId),
    case rpc_client:call(Req) of
        {ok, _Data} ->
            {ok, #{success => true}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec cleanup_virtual_channel_access_for_user(integer(), guild_state()) -> guild_state().
cleanup_virtual_channel_access_for_user(UserId, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    VirtualChannels = guild_virtual_channel_access:get_virtual_channels_for_user(UserId, State),
    lists:foldl(
        fun(ChannelId, AccState) ->
            case user_has_voice_connection_in_channel(UserId, ChannelId, VoiceStates) of
                true ->
                    AccState;
                false ->
                    case
                        guild_virtual_channel_access:is_pending_join(UserId, ChannelId, AccState) orelse
                            guild_virtual_channel_access:has_preserve(UserId, ChannelId, AccState) orelse
                            guild_virtual_channel_access:is_move_pending(
                                UserId, ChannelId, AccState
                            )
                    of
                        true ->
                            AccState;
                        false ->
                            ok = maybe_dispatch_visibility_remove(UserId, ChannelId, AccState),
                            guild_virtual_channel_access:remove_virtual_access(
                                UserId, ChannelId, AccState
                            )
                    end
            end
        end,
        State,
        VirtualChannels
    ).

-spec user_has_voice_connection_in_channel(integer(), integer(), voice_state_map()) -> boolean().
user_has_voice_connection_in_channel(UserId, ChannelId, VoiceStates) ->
    ChannelIdBin = integer_to_binary(ChannelId),
    maps:fold(
        fun(_ConnId, VoiceState, Acc) ->
            case Acc of
                true ->
                    true;
                false ->
                    voice_state_utils:voice_state_user_id(VoiceState) =:= UserId andalso
                        maps:get(<<"channel_id">>, VoiceState, null) =:= ChannelIdBin
            end
        end,
        false,
        VoiceStates
    ).

-spec maybe_dispatch_visibility_remove(integer(), integer(), guild_state()) -> ok.
maybe_dispatch_visibility_remove(UserId, ChannelId, State) ->
    case guild_virtual_channel_access:has_virtual_access(UserId, ChannelId, State) of
        true ->
            guild_virtual_channel_access:dispatch_channel_visibility_change(
                UserId, ChannelId, remove, State
            );
        false ->
            ok
    end.

-spec maybe_force_disconnect(integer(), integer(), integer(), binary(), guild_state()) ->
    {ok, map()} | {error, term()}.
maybe_force_disconnect(GuildId, ChannelId, UserId, ConnectionId, State) ->
    case maps:get(test_force_disconnect_fun, State, undefined) of
        Fun when is_function(Fun, 4) ->
            Fun(GuildId, ChannelId, UserId, ConnectionId);
        _ ->
            force_disconnect_participant(GuildId, ChannelId, UserId, ConnectionId)
    end.

-define(RECENTLY_DISCONNECTED_TTL_MS, 60000).

-spec recently_disconnected_voice_states(guild_state()) -> map().
recently_disconnected_voice_states(State) ->
    case maps:get(recently_disconnected_voice_states, State, undefined) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    end.

-spec cache_recently_disconnected(voice_state_map(), guild_state()) -> guild_state().
cache_recently_disconnected(VoiceStatesToCache, State) ->
    Now = erlang:system_time(millisecond),
    Existing = recently_disconnected_voice_states(State),
    Swept = sweep_expired_recently_disconnected(Existing, Now),
    NewEntries = maps:fold(
        fun(ConnId, VoiceState, Acc) ->
            maps:put(ConnId, #{voice_state => VoiceState, disconnected_at => Now}, Acc)
        end,
        Swept,
        VoiceStatesToCache
    ),
    maps:put(recently_disconnected_voice_states, NewEntries, State).

-spec sweep_expired_recently_disconnected(map(), integer()) -> map().
sweep_expired_recently_disconnected(Cache, Now) ->
    maps:filter(
        fun(_ConnId, #{disconnected_at := DisconnectedAt}) ->
            (Now - DisconnectedAt) < ?RECENTLY_DISCONNECTED_TTL_MS;
           (_ConnId, _) ->
            false
        end,
        Cache
    ).

-spec clear_recently_disconnected(binary(), guild_state()) -> guild_state().
clear_recently_disconnected(ConnectionId, State) ->
    Cache = recently_disconnected_voice_states(State),
    NewCache = maps:remove(ConnectionId, Cache),
    maps:put(recently_disconnected_voice_states, NewCache, State).

-spec clear_recently_disconnected_for_channel(integer(), guild_state()) -> guild_state().
clear_recently_disconnected_for_channel(ChannelId, State) ->
    Cache = recently_disconnected_voice_states(State),
    NewCache = maps:filter(
        fun(_ConnId, #{voice_state := VS}) ->
            voice_state_utils:voice_state_channel_id(VS) =/= ChannelId;
           (_ConnId, _) ->
            false
        end,
        Cache
    ),
    maps:put(recently_disconnected_voice_states, NewCache, State).

-ifdef(TEST).

disconnect_voice_user_removes_all_connections_test() ->
    VoiceStates = #{
        <<"a">> => voice_state_fixture(5, 10, 20),
        <<"b">> => voice_state_fixture(5, 10, 21)
    },
    State = #{
        voice_states => VoiceStates,
        test_force_disconnect_fun => fun(_, _, _, _) -> {ok, #{success => true}} end
    },
    {reply, #{success := true}, #{voice_states := #{}}} =
        disconnect_voice_user(#{user_id => 5, connection_id => null}, State).

handle_voice_disconnect_invalid_state_test() ->
    VoiceState = #{<<"user_id">> => <<"5">>},
    VoiceStates = #{<<"conn">> => VoiceState},
    State = #{voice_states => VoiceStates},
    {reply, {error, validation_error, _}, _} =
        handle_voice_disconnect(<<"conn">>, undefined, 5, VoiceStates, State).

disconnect_voice_user_if_in_channel_ignored_test() ->
    VoiceStates = #{},
    State = #{voice_states => VoiceStates},
    {reply, #{ignored := true}, _} =
        disconnect_voice_user_if_in_channel(#{user_id => 5, expected_channel_id => 99}, State).

user_has_voice_connection_in_channel_test() ->
    VoiceStates = #{
        <<"conn">> => #{<<"user_id">> => <<"10">>, <<"channel_id">> => <<"100">>}
    },
    ?assert(user_has_voice_connection_in_channel(10, 100, VoiceStates)),
    ?assertNot(user_has_voice_connection_in_channel(10, 200, VoiceStates)),
    ?assertNot(user_has_voice_connection_in_channel(20, 100, VoiceStates)).

recently_disconnected_voice_states_default_test() ->
    ?assertEqual(#{}, recently_disconnected_voice_states(#{})).

recently_disconnected_voice_states_returns_map_test() ->
    Cache = #{<<"conn">> => #{voice_state => #{}, disconnected_at => 1000}},
    State = #{recently_disconnected_voice_states => Cache},
    ?assertEqual(Cache, recently_disconnected_voice_states(State)).

cache_recently_disconnected_test() ->
    VS = voice_state_fixture(5, 10, 20),
    State = #{},
    NewState = cache_recently_disconnected(#{<<"conn">> => VS}, State),
    Cache = recently_disconnected_voice_states(NewState),
    ?assert(maps:is_key(<<"conn">>, Cache)),
    #{<<"conn">> := #{voice_state := CachedVS}} = Cache,
    ?assertEqual(VS, CachedVS).

clear_recently_disconnected_test() ->
    VS = voice_state_fixture(5, 10, 20),
    State0 = cache_recently_disconnected(#{<<"conn">> => VS}, #{}),
    State1 = clear_recently_disconnected(<<"conn">>, State0),
    ?assertEqual(#{}, recently_disconnected_voice_states(State1)).

clear_recently_disconnected_for_channel_test() ->
    VS1 = voice_state_fixture(5, 10, 20),
    VS2 = voice_state_fixture(6, 10, 30),
    State0 = cache_recently_disconnected(#{<<"a">> => VS1, <<"b">> => VS2}, #{}),
    State1 = clear_recently_disconnected_for_channel(20, State0),
    Cache = recently_disconnected_voice_states(State1),
    ?assertNot(maps:is_key(<<"a">>, Cache)),
    ?assert(maps:is_key(<<"b">>, Cache)).

sweep_expired_recently_disconnected_test() ->
    Now = erlang:system_time(millisecond),
    Cache = #{
        <<"fresh">> => #{voice_state => #{}, disconnected_at => Now - 1000},
        <<"expired">> => #{voice_state => #{}, disconnected_at => Now - 70000}
    },
    Swept = sweep_expired_recently_disconnected(Cache, Now),
    ?assert(maps:is_key(<<"fresh">>, Swept)),
    ?assertNot(maps:is_key(<<"expired">>, Swept)).

disconnect_voice_user_if_in_channel_caches_by_connection_test() ->
    VS = voice_state_fixture(5, 10, 20),
    VoiceStates = #{<<"conn">> => VS},
    State = #{voice_states => VoiceStates},
    {reply, #{success := true}, NewState} =
        disconnect_voice_user_if_in_channel(
            #{user_id => 5, expected_channel_id => 20, connection_id => <<"conn">>},
            State
        ),
    Cache = recently_disconnected_voice_states(NewState),
    ?assert(maps:is_key(<<"conn">>, Cache)).

disconnect_voice_user_calls_force_disconnect_for_all_connections_test() ->
    Self = self(),
    TestFun = fun(GuildId, ChannelId, UserId, ConnectionId) ->
        Self ! {force_disconnect, GuildId, ChannelId, UserId, ConnectionId},
        {ok, #{success => true}}
    end,
    VoiceStates = #{
        <<"a">> => voice_state_fixture(5, 10, 20),
        <<"b">> => voice_state_fixture(5, 10, 21)
    },
    State = #{
        id => 10,
        voice_states => VoiceStates,
        test_force_disconnect_fun => TestFun
    },
    {reply, #{success := true}, #{voice_states := #{}}} =
        disconnect_voice_user(#{user_id => 5, connection_id => null}, State),
    Msgs = collect_force_disconnect_messages(2),
    ?assertEqual(2, length(Msgs)),
    ?assert(lists:member({force_disconnect, 10, 20, 5, <<"a">>}, Msgs)),
    ?assert(lists:member({force_disconnect, 10, 21, 5, <<"b">>}, Msgs)).

disconnect_voice_user_calls_force_disconnect_for_specific_connection_test() ->
    Self = self(),
    TestFun = fun(GuildId, ChannelId, UserId, ConnectionId) ->
        Self ! {force_disconnect, GuildId, ChannelId, UserId, ConnectionId},
        {ok, #{success => true}}
    end,
    VoiceStates = #{
        <<"a">> => voice_state_fixture(5, 10, 20),
        <<"b">> => voice_state_fixture(5, 10, 21)
    },
    State = #{
        id => 10,
        voice_states => VoiceStates,
        test_force_disconnect_fun => TestFun
    },
    {reply, #{success := true}, NewState} =
        disconnect_voice_user(#{user_id => 5, connection_id => <<"a">>}, State),
    Msgs = collect_force_disconnect_messages(1),
    ?assertEqual(1, length(Msgs)),
    ?assert(lists:member({force_disconnect, 10, 20, 5, <<"a">>}, Msgs)),
    Remaining = maps:get(voice_states, NewState),
    ?assert(maps:is_key(<<"b">>, Remaining)),
    ?assertNot(maps:is_key(<<"a">>, Remaining)).

disconnect_all_voice_users_in_channel_calls_force_disconnect_test() ->
    Self = self(),
    TestFun = fun(GuildId, ChannelId, UserId, ConnectionId) ->
        Self ! {force_disconnect, GuildId, ChannelId, UserId, ConnectionId},
        {ok, #{success => true}}
    end,
    VoiceStates = #{
        <<"a">> => voice_state_fixture(5, 10, 20),
        <<"b">> => voice_state_fixture(6, 10, 20),
        <<"c">> => voice_state_fixture(7, 10, 30)
    },
    State = #{
        id => 10,
        voice_states => VoiceStates,
        test_force_disconnect_fun => TestFun
    },
    {reply, #{success := true, disconnected_count := 2}, NewState} =
        disconnect_all_voice_users_in_channel(#{channel_id => 20}, State),
    Msgs = collect_force_disconnect_messages(2),
    ?assertEqual(2, length(Msgs)),
    ?assert(lists:member({force_disconnect, 10, 20, 5, <<"a">>}, Msgs)),
    ?assert(lists:member({force_disconnect, 10, 20, 6, <<"b">>}, Msgs)),
    Remaining = maps:get(voice_states, NewState),
    ?assert(maps:is_key(<<"c">>, Remaining)),
    ?assertNot(maps:is_key(<<"a">>, Remaining)),
    ?assertNot(maps:is_key(<<"b">>, Remaining)).

disconnect_voice_user_if_in_channel_skips_force_disconnect_test() ->
    Self = self(),
    TestFun = fun(_, _, _, _) ->
        Self ! force_disconnect_called,
        {ok, #{success => true}}
    end,
    VoiceStates = #{<<"a">> => voice_state_fixture(5, 10, 20)},
    State = #{
        voice_states => VoiceStates,
        test_force_disconnect_fun => TestFun
    },
    {reply, #{success := true}, _} =
        disconnect_voice_user_if_in_channel(
            #{user_id => 5, expected_channel_id => 20, connection_id => <<"a">>},
            State
        ),
    receive
        force_disconnect_called -> ?assert(false)
    after 0 ->
        ok
    end.

clear_pending_voice_connection_removes_connection_test() ->
    PendingConnections = #{
        <<"conn1">> => #{user_id => 1, channel_id => 100},
        <<"conn2">> => #{user_id => 2, channel_id => 200}
    },
    State = #{pending_voice_connections => PendingConnections},
    NewState = clear_pending_voice_connection(<<"conn1">>, State),
    NewPending = maps:get(pending_voice_connections, NewState),
    ?assertNot(maps:is_key(<<"conn1">>, NewPending)),
    ?assert(maps:is_key(<<"conn2">>, NewPending)).

clear_pending_voice_connection_ignores_missing_test() ->
    PendingConnections = #{<<"conn1">> => #{user_id => 1, channel_id => 100}},
    State = #{pending_voice_connections => PendingConnections},
    NewState = clear_pending_voice_connection(<<"missing">>, State),
    ?assertEqual(State, NewState).

clear_pending_voice_connection_handles_empty_pending_test() ->
    State = #{voice_states => #{}},
    NewState = clear_pending_voice_connection(<<"conn">>, State),
    ?assertEqual(#{}, maps:get(pending_voice_connections, NewState, #{})).

clear_pending_voice_connections_for_user_removes_all_user_connections_test() ->
    PendingConnections = #{
        <<"conn1">> => #{user_id => 5, channel_id => 100},
        <<"conn2">> => #{user_id => 5, channel_id => 200},
        <<"conn3">> => #{user_id => 6, channel_id => 100}
    },
    State = #{pending_voice_connections => PendingConnections},
    NewState = clear_pending_voice_connections_for_user(5, State),
    NewPending = maps:get(pending_voice_connections, NewState),
    ?assertNot(maps:is_key(<<"conn1">>, NewPending)),
    ?assertNot(maps:is_key(<<"conn2">>, NewPending)),
    ?assert(maps:is_key(<<"conn3">>, NewPending)).

clear_pending_voice_connections_for_user_channel_removes_matching_test() ->
    PendingConnections = #{
        <<"conn1">> => #{user_id => 5, channel_id => 100},
        <<"conn2">> => #{user_id => 5, channel_id => 200},
        <<"conn3">> => #{user_id => 6, channel_id => 100}
    },
    State = #{pending_voice_connections => PendingConnections},
    NewState = clear_pending_voice_connections_for_user_channel(5, 100, State),
    NewPending = maps:get(pending_voice_connections, NewState),
    ?assertNot(maps:is_key(<<"conn1">>, NewPending)),
    ?assert(maps:is_key(<<"conn2">>, NewPending)),
    ?assert(maps:is_key(<<"conn3">>, NewPending)).

clear_pending_voice_connections_for_channel_removes_all_channel_connections_test() ->
    PendingConnections = #{
        <<"conn1">> => #{user_id => 5, channel_id => 100},
        <<"conn2">> => #{user_id => 6, channel_id => 100},
        <<"conn3">> => #{user_id => 7, channel_id => 200}
    },
    State = #{pending_voice_connections => PendingConnections},
    NewState = clear_pending_voice_connections_for_channel(100, State),
    NewPending = maps:get(pending_voice_connections, NewState),
    ?assertNot(maps:is_key(<<"conn1">>, NewPending)),
    ?assertNot(maps:is_key(<<"conn2">>, NewPending)),
    ?assert(maps:is_key(<<"conn3">>, NewPending)).

handle_voice_disconnect_cleans_pending_when_not_in_voice_states_test() ->
    PendingConnections = #{<<"conn1">> => #{user_id => 5, channel_id => 100}},
    State = #{
        voice_states => #{},
        pending_voice_connections => PendingConnections
    },
    {reply, #{success := true}, NewState} =
        handle_voice_disconnect(<<"conn1">>, undefined, 5, #{}, State),
    NewPending = maps:get(pending_voice_connections, NewState),
    ?assertNot(maps:is_key(<<"conn1">>, NewPending)).

disconnect_voice_user_cleans_pending_when_no_active_states_test() ->
    PendingConnections = #{
        <<"conn1">> => #{user_id => 5, channel_id => 100},
        <<"conn2">> => #{user_id => 6, channel_id => 100}
    },
    State = #{
        id => 10,
        voice_states => #{},
        pending_voice_connections => PendingConnections
    },
    {reply, #{success := true}, NewState} =
        disconnect_voice_user(#{user_id => 5}, State),
    NewPending = maps:get(pending_voice_connections, NewState),
    ?assertNot(maps:is_key(<<"conn1">>, NewPending)),
    ?assert(maps:is_key(<<"conn2">>, NewPending)).

disconnect_voice_user_cleans_pending_for_specific_connection_test() ->
    PendingConnections = #{
        <<"conn1">> => #{user_id => 5, channel_id => 100},
        <<"conn2">> => #{user_id => 5, channel_id => 200}
    },
    State = #{
        id => 10,
        voice_states => #{},
        pending_voice_connections => PendingConnections
    },
    {reply, #{success := true}, NewState} =
        disconnect_voice_user(#{user_id => 5, connection_id => <<"conn1">>}, State),
    NewPending = maps:get(pending_voice_connections, NewState),
    ?assertNot(maps:is_key(<<"conn1">>, NewPending)),
    ?assert(maps:is_key(<<"conn2">>, NewPending)).

disconnect_voice_user_if_in_channel_cleans_pending_when_not_found_test() ->
    PendingConnections = #{
        <<"conn1">> => #{user_id => 5, channel_id => 100},
        <<"conn2">> => #{user_id => 6, channel_id => 100}
    },
    State = #{
        id => 10,
        voice_states => #{},
        pending_voice_connections => PendingConnections
    },
    {reply, #{success := true, ignored := true}, NewState} =
        disconnect_voice_user_if_in_channel(
            #{user_id => 5, expected_channel_id => 100},
            State
        ),
    NewPending = maps:get(pending_voice_connections, NewState),
    ?assertNot(maps:is_key(<<"conn1">>, NewPending)),
    ?assert(maps:is_key(<<"conn2">>, NewPending)).

disconnect_all_voice_users_in_channel_cleans_pending_test() ->
    Self = self(),
    TestFun = fun(GuildId, ChannelId, UserId, ConnectionId) ->
        Self ! {force_disconnect, GuildId, ChannelId, UserId, ConnectionId},
        {ok, #{success => true}}
    end,
    VoiceStates = #{
        <<"a">> => voice_state_fixture(5, 10, 20)
    },
    PendingConnections = #{
        <<"pending1">> => #{user_id => 6, channel_id => 20},
        <<"pending2">> => #{user_id => 7, channel_id => 30}
    },
    State = #{
        id => 10,
        voice_states => VoiceStates,
        pending_voice_connections => PendingConnections,
        test_force_disconnect_fun => TestFun
    },
    {reply, #{success := true, disconnected_count := 1}, NewState} =
        disconnect_all_voice_users_in_channel(#{channel_id => 20}, State),
    _ = collect_force_disconnect_messages(1),
    NewPending = maps:get(pending_voice_connections, NewState),
    ?assertNot(maps:is_key(<<"pending1">>, NewPending)),
    ?assert(maps:is_key(<<"pending2">>, NewPending)).

disconnect_all_voice_users_in_channel_cleans_pending_when_no_active_states_test() ->
    PendingConnections = #{
        <<"pending1">> => #{user_id => 5, channel_id => 20},
        <<"pending2">> => #{user_id => 6, channel_id => 30}
    },
    State = #{
        id => 10,
        voice_states => #{},
        pending_voice_connections => PendingConnections
    },
    {reply, #{success := true, disconnected_count := 0}, NewState} =
        disconnect_all_voice_users_in_channel(#{channel_id => 20}, State),
    NewPending = maps:get(pending_voice_connections, NewState),
    ?assertNot(maps:is_key(<<"pending1">>, NewPending)),
    ?assert(maps:is_key(<<"pending2">>, NewPending)).

collect_force_disconnect_messages(Count) ->
    collect_force_disconnect_messages(Count, []).

collect_force_disconnect_messages(0, Acc) ->
    lists:reverse(Acc);
collect_force_disconnect_messages(Count, Acc) when Count > 0 ->
    receive
        {force_disconnect, _, _, _, _} = Msg ->
            collect_force_disconnect_messages(Count - 1, [Msg | Acc]);
        _Other ->
            collect_force_disconnect_messages(Count, Acc)
    after 200 ->
        lists:reverse(Acc)
    end.

voice_state_fixture(UserId, GuildId, ChannelId) ->
    #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"channel_id">> => integer_to_binary(ChannelId)
    }.

-endif.
