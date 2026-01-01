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

-type guild_state() :: map().
-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
            {reply, #{success => true}, State};
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
                            NewState = maps:put(voice_states, NewVoiceStates, State),
                            voice_state_utils:broadcast_disconnects(
                                #{ConnectionId => OldVoiceState}, NewState
                            ),
                            FinalState = cleanup_virtual_channel_access_for_user(UserId, NewState),
                            {reply, #{success => true}, FinalState}
                    end
            end
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
                    {reply, #{success => true}, State};
                _ ->
                    NewVoiceStates = voice_state_utils:drop_voice_states(
                        UserVoiceStates, VoiceStates
                    ),
                    NewState = maps:put(voice_states, NewVoiceStates, State),
                    voice_state_utils:broadcast_disconnects(UserVoiceStates, NewState),
                    FinalState = cleanup_virtual_channel_access_for_user(UserId, NewState),
                    {reply, #{success => true}, FinalState}
            end;
        SpecificConnection ->
            case maps:get(SpecificConnection, VoiceStates, undefined) of
                undefined ->
                    {reply, #{success => true}, State};
                VoiceState ->
                    case voice_state_utils:voice_state_user_id(VoiceState) of
                        undefined ->
                            {reply, gateway_errors:error(voice_invalid_state), State};
                        VoiceStateUserId when VoiceStateUserId =:= UserId ->
                            NewVoiceStates = maps:remove(SpecificConnection, VoiceStates),
                            NewState = maps:put(voice_states, NewVoiceStates, State),
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
                    {reply,
                        #{
                            success => true,
                            ignored => true,
                            reason => <<"not_in_expected_channel">>
                        },
                        State};
                _ ->
                    NewVoiceStates = voice_state_utils:drop_voice_states(
                        UserVoiceStates, VoiceStates
                    ),
                    NewState = maps:put(voice_states, NewVoiceStates, State),
                    voice_state_utils:broadcast_disconnects(UserVoiceStates, NewState),
                    {reply, #{success => true}, NewState}
            end;
        ConnId ->
            case maps:get(ConnId, VoiceStates, undefined) of
                undefined ->
                    {reply,
                        #{success => true, ignored => true, reason => <<"connection_not_found">>},
                        State};
                VoiceState ->
                    case
                        {
                            voice_state_utils:voice_state_user_id(VoiceState),
                            voice_state_utils:voice_state_channel_id(VoiceState)
                        }
                    of
                        {UserId, ExpectedChannelId} ->
                            NewVoiceStates = maps:remove(ConnId, VoiceStates),
                            NewState = maps:put(voice_states, NewVoiceStates, State),
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

-spec disconnect_all_voice_users_in_channel(map(), guild_state()) -> {reply, map(), guild_state()}.
disconnect_all_voice_users_in_channel(#{channel_id := ChannelId}, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    ChannelVoiceStates = voice_state_utils:filter_voice_states(VoiceStates, fun(_, V) ->
        voice_state_utils:voice_state_channel_id(V) =:= ChannelId
    end),
    case maps:size(ChannelVoiceStates) of
        0 ->
            {reply, #{success => true, disconnected_count => 0}, State};
        Count ->
            NewVoiceStates = voice_state_utils:drop_voice_states(ChannelVoiceStates, VoiceStates),
            NewState = maps:put(voice_states, NewVoiceStates, State),
            voice_state_utils:broadcast_disconnects(ChannelVoiceStates, NewState),
            {reply, #{success => true, disconnected_count => Count}, NewState}
    end.

-spec force_disconnect_participant(integer(), integer(), integer(), binary()) ->
    {ok, map()} | {error, term()}.
force_disconnect_participant(GuildId, ChannelId, UserId, ConnectionId) ->
    Req = voice_utils:build_force_disconnect_rpc_request(GuildId, ChannelId, UserId, ConnectionId),
    case rpc_client:call(Req) of
        {ok, _Data} ->
            logger:debug(
                "[guild_voice_disconnect] Force disconnected participant via RPC ~p",
                [
                    [
                        {guildId, GuildId},
                        {channelId, ChannelId},
                        {userId, UserId},
                        {connectionId, ConnectionId}
                    ]
                ]
            ),
            {ok, #{success => true}};
        {error, Reason} ->
            logger:error(
                "[guild_voice_disconnect] Failed to force disconnect participant via RPC ~p",
                [
                    [
                        {guildId, GuildId},
                        {channelId, ChannelId},
                        {userId, UserId},
                        {connectionId, ConnectionId},
                        {error, Reason}
                    ]
                ]
            ),
            {error, Reason}
    end.

cleanup_virtual_channel_access_for_user(UserId, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    HasVoiceConnection = maps:fold(
        fun(_ConnId, VoiceState, Acc) ->
            case Acc of
                true -> true;
                false -> voice_state_utils:voice_state_user_id(VoiceState) =:= UserId
            end
        end,
        false,
        VoiceStates
    ),
    case HasVoiceConnection of
        true ->
            State;
        false ->
            VirtualChannels = guild_virtual_channel_access:get_virtual_channels_for_user(
                UserId, State
            ),
            lists:foldl(
                fun(ChannelId, AccState) ->
                    Member = guild_permissions:find_member_by_user_id(UserId, AccState),
                    case Member of
                        undefined ->
                            AccState;
                        _ ->
                            HasViewPermission = guild_permissions:can_view_channel_by_permissions(
                                UserId, ChannelId, Member, AccState
                            ),
                            case HasViewPermission of
                                true ->
                                    guild_virtual_channel_access:remove_virtual_access(
                                        UserId, ChannelId, AccState
                                    );
                                false ->
                                    guild_virtual_channel_access:dispatch_channel_visibility_change(
                                        UserId, ChannelId, remove, AccState
                                    ),
                                    guild_virtual_channel_access:remove_virtual_access(
                                        UserId, ChannelId, AccState
                                    )
                            end
                    end
                end,
                State,
                VirtualChannels
            )
    end.

maybe_force_disconnect(GuildId, ChannelId, UserId, ConnectionId, State) ->
    case maps:get(test_force_disconnect_fun, State, undefined) of
        Fun when is_function(Fun, 4) ->
            Fun(GuildId, ChannelId, UserId, ConnectionId);
        _ ->
            force_disconnect_participant(GuildId, ChannelId, UserId, ConnectionId)
    end.

-ifdef(TEST).

disconnect_voice_user_removes_all_connections_test() ->
    VoiceStates = #{
        <<"a">> => voice_state_fixture(5, 10, 20),
        <<"b">> => voice_state_fixture(5, 10, 21)
    },
    State = #{voice_states => VoiceStates},
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

voice_state_fixture(UserId, GuildId, ChannelId) ->
    #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"channel_id">> => integer_to_binary(ChannelId)
    }.

-endif.
