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

-module(guild_voice_region).

-export([switch_voice_region_handler/2]).
-export([switch_voice_region/3]).

-type guild_state() :: map().
-type guild_reply(T) :: {reply, T, guild_state()}.
-type voice_state() :: map().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec switch_voice_region_handler(map(), guild_state()) -> guild_reply(map()).
switch_voice_region_handler(Request, State) ->
    #{channel_id := ChannelId} = Request,
    Channel = guild_voice_member:find_channel_by_id(ChannelId, State),
    case Channel of
        undefined ->
            {reply, gateway_errors:error(voice_channel_not_found), State};
        _ ->
            ChannelType = maps:get(<<"type">>, Channel, 0),
            case ChannelType of
                2 ->
                    {reply, #{success => true}, State};
                _ ->
                    {reply, gateway_errors:error(voice_channel_not_voice), State}
            end
    end.

-spec switch_voice_region(integer(), integer(), pid()) -> ok.
switch_voice_region(GuildId, ChannelId, GuildPid) ->
    case gen_server:call(GuildPid, {get_sessions}, 10000) of
        State when is_map(State) ->
            VoiceStates = voice_state_utils:voice_states(State),
            UsersInChannel = collect_users_in_channel(VoiceStates, ChannelId),
            lists:foreach(
                fun({UserId, SessionId, ExistingConnectionId, VoiceState}) ->
                    case SessionId of
                        undefined ->
                            ok;
                        _ ->
                            send_voice_server_update_for_region_switch(
                                GuildId, ChannelId, UserId, SessionId, ExistingConnectionId,
                                VoiceState, GuildPid
                            )
                    end
                end,
                UsersInChannel
            );
        _ ->
            ok
    end.

-spec collect_users_in_channel(map(), integer()) ->
    [{integer(), binary() | undefined, binary(), voice_state()}].
collect_users_in_channel(VoiceStates, ChannelId) ->
    maps:fold(
        fun(ConnectionId, VoiceState, Acc) ->
            case voice_state_utils:voice_state_channel_id(VoiceState) of
                ChannelId ->
                    case voice_state_utils:voice_state_user_id(VoiceState) of
                        undefined ->
                            Acc;
                        UserId ->
                            SessionId = maps:get(<<"session_id">>, VoiceState, undefined),
                            [{UserId, SessionId, ConnectionId, VoiceState} | Acc]
                    end;
                _ ->
                    Acc
            end
        end,
        [],
        VoiceStates
    ).

-spec send_voice_server_update_for_region_switch(
    integer(), integer(), integer(), binary(), binary(), voice_state(), pid()
) -> ok.
send_voice_server_update_for_region_switch(
    GuildId, ChannelId, UserId, SessionId, ExistingConnectionId, ExistingVoiceState, GuildPid
) ->
    case gen_server:call(GuildPid, {get_sessions}, 10000) of
        State when is_map(State) ->
            VoicePermissions = voice_utils:compute_voice_permissions(UserId, ChannelId, State),
            TokenNonce = voice_utils:generate_token_nonce(),
            case
                guild_voice_connection:request_voice_token(
                    GuildId, ChannelId, UserId, ExistingConnectionId, VoicePermissions, TokenNonce
                )
            of
                {ok, TokenData} ->
                    Token = maps:get(token, TokenData),
                    Endpoint = maps:get(endpoint, TokenData),
                    ConnectionId = maps:get(connection_id, TokenData),
                    PendingMetadata = build_pending_metadata(
                        UserId, GuildId, ChannelId, SessionId, ExistingVoiceState, TokenNonce
                    ),
                    _ = store_pending_connection(
                        GuildId, GuildPid, ConnectionId, PendingMetadata
                    ),
                    guild_voice_broadcast:broadcast_voice_server_update_to_session(
                        GuildId,
                        ChannelId,
                        SessionId,
                        Token,
                        Endpoint,
                        ConnectionId,
                        State
                    );
                {error, _Reason} ->
                    ok
            end;
        _ ->
            ok
    end.

-spec build_pending_metadata(integer(), integer(), integer(), binary(), voice_state(), binary()) -> map().
build_pending_metadata(UserId, GuildId, ChannelId, SessionId, ExistingVoiceState, TokenNonce) ->
    Now = erlang:system_time(millisecond),
    #{
        user_id => UserId,
        guild_id => GuildId,
        channel_id => ChannelId,
        session_id => SessionId,
        self_mute => maps:get(<<"self_mute">>, ExistingVoiceState, false),
        self_deaf => maps:get(<<"self_deaf">>, ExistingVoiceState, false),
        self_video => maps:get(<<"self_video">>, ExistingVoiceState, false),
        self_stream => maps:get(<<"self_stream">>, ExistingVoiceState, false),
        is_mobile => maps:get(<<"is_mobile">>, ExistingVoiceState, false),
        server_mute => maps:get(<<"mute">>, ExistingVoiceState, false),
        server_deaf => maps:get(<<"deaf">>, ExistingVoiceState, false),
        member => maps:get(<<"member">>, ExistingVoiceState, #{}),
        viewer_stream_keys => [],
        token_nonce => TokenNonce,
        created_at => Now,
        expires_at => Now + 30000
    }.

-spec store_pending_connection(integer(), pid(), binary(), map()) -> ok.
store_pending_connection(GuildId, GuildPid, ConnectionId, Metadata) ->
    TargetPid = resolve_voice_server(GuildId, GuildPid),
    gen_server:call(
        TargetPid,
        {store_pending_connection, ConnectionId, Metadata},
        10000
    ).

-spec resolve_voice_server(integer(), pid()) -> pid().
resolve_voice_server(GuildId, FallbackPid) ->
    case guild_voice_server:lookup(GuildId) of
        {ok, VoicePid} -> VoicePid;
        {error, not_found} -> FallbackPid
    end.

-ifdef(TEST).

switch_voice_region_handler_not_found_test() ->
    State = #{data => #{<<"channels">> => []}},
    Request = #{channel_id => 999},
    {reply, Error, _} = switch_voice_region_handler(Request, State),
    ?assertEqual({error, not_found, voice_channel_not_found}, Error).

switch_voice_region_handler_not_voice_test() ->
    State = #{
        data => #{
            <<"channels">> => [
                #{<<"id">> => <<"100">>, <<"type">> => 0}
            ]
        }
    },
    Request = #{channel_id => 100},
    {reply, Error, _} = switch_voice_region_handler(Request, State),
    ?assertEqual({error, validation_error, voice_channel_not_voice}, Error).

switch_voice_region_handler_success_test() ->
    State = #{
        data => #{
            <<"channels">> => [
                #{<<"id">> => <<"100">>, <<"type">> => 2}
            ]
        }
    },
    Request = #{channel_id => 100},
    {reply, Reply, _} = switch_voice_region_handler(Request, State),
    ?assertEqual(true, maps:get(success, Reply)).

collect_users_in_channel_test() ->
    VoiceState = #{
        <<"channel_id">> => <<"100">>,
        <<"user_id">> => <<"10">>,
        <<"session_id">> => <<"sess1">>
    },
    VoiceStates = #{<<"conn1">> => VoiceState},
    Result = collect_users_in_channel(VoiceStates, 100),
    ?assertEqual(1, length(Result)),
    [{UserId, SessionId, ConnectionId, _}] = Result,
    ?assertEqual(10, UserId),
    ?assertEqual(<<"sess1">>, SessionId),
    ?assertEqual(<<"conn1">>, ConnectionId).

-endif.
