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

-module(guild_voice_broadcast).

-export([broadcast_voice_state_update/3]).
-export([broadcast_voice_server_update_to_session/6]).

-ifdef(TEST).
-define(WARN_MISSING_CONN(_VoiceState), ok).
-else.
-define(WARN_MISSING_CONN(VoiceState),
    logger:warning(
        "[guild_voice_broadcast] Skipping VOICE_STATE_UPDATE broadcast - missing connection_id: ~p",
        [VoiceState]
    )
).
-endif.

broadcast_voice_state_update(VoiceState, State, OldChannelIdBin) ->
    case maps:get(<<"connection_id">>, VoiceState, undefined) of
        undefined ->
            ?WARN_MISSING_CONN(VoiceState),
            ok;
        ConnectionId ->
            Sessions = maps:get(sessions, State, #{}),
            ChannelIdBin = maps:get(<<"channel_id">>, VoiceState, null),

            FilterChannelIdBin =
                case ChannelIdBin of
                    null ->
                        OldChannelIdBin;
                    _ ->
                        ChannelIdBin
                end,

            FilterChannelId = utils:binary_to_integer_safe(FilterChannelIdBin),

            UserId = maps:get(<<"user_id">>, VoiceState, <<"unknown">>),
            GuildId = maps:get(id, State, 0),
            AllSessionDetails = [{Sid, maps:get(user_id, S)} || {Sid, S} <- maps:to_list(Sessions)],
            logger:info(
                "[guild_voice_broadcast] Broadcasting voice state update: "
                "guild_id=~p user_id=~p channel_id=~p connection_id=~p "
                "total_sessions=~p all_sessions=~p filter_channel_id=~p",
                [
                    GuildId,
                    UserId,
                    ChannelIdBin,
                    ConnectionId,
                    maps:size(Sessions),
                    AllSessionDetails,
                    FilterChannelId
                ]
            ),

            FilteredSessions = guild_sessions:filter_sessions_for_channel(
                Sessions, FilterChannelId, undefined, State
            ),

            SessionDetails = [{Sid, maps:get(user_id, S)} || {Sid, S} <- FilteredSessions],
            Pids = [maps:get(pid, S) || {_Sid, S} <- FilteredSessions],

            logger:info(
                "[guild_voice_broadcast] Filtered sessions: "
                "guild_id=~p user_id=~p filtered_count=~p session_details=~p pids=~p",
                [GuildId, UserId, length(FilteredSessions), SessionDetails, Pids]
            ),

            lists:foreach(
                fun(Pid) when is_pid(Pid) ->
                    logger:info(
                        "[guild_voice_broadcast] Sending voice_state_update to session pid ~p",
                        [Pid]
                    ),
                    gen_server:cast(Pid, {dispatch, voice_state_update, VoiceState})
                end,
                Pids
            )
    end.

broadcast_voice_server_update_to_session(GuildId, SessionId, Token, Endpoint, ConnectionId, State) ->
    VoiceServerUpdate = #{
        <<"token">> => Token,
        <<"endpoint">> => Endpoint,
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"connection_id">> => ConnectionId
    },

    Sessions = maps:get(sessions, State, #{}),

    case maps:get(SessionId, Sessions, undefined) of
        undefined ->
            ok;
        SessionData ->
            SessionPid = maps:get(pid, SessionData, null),
            case SessionPid of
                Pid when is_pid(Pid) ->
                    gen_server:cast(Pid, {dispatch, voice_server_update, VoiceServerUpdate});
                _ ->
                    ok
            end
    end.
