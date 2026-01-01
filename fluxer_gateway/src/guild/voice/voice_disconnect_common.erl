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

-module(voice_disconnect_common).

-export([
    find_session_by_user_id/2,
    disconnect_user/4,
    disconnect_user_if_in_channel/5,
    channel_has_capacity/3
]).

-type user_id() :: integer().
-type session_id() :: binary().
-type session_pid() :: pid().
-type monitor_ref() :: reference().
-type session_tuple() :: {user_id(), session_pid(), monitor_ref()}.
-type sessions_map() :: #{session_id() => session_tuple()}.
-type voice_states_map() :: #{user_id() => map()}.
-type cleanup_fun() :: fun((user_id(), session_id()) -> ok).

-spec find_session_by_user_id(user_id(), sessions_map()) ->
    {ok, session_id(), session_pid(), monitor_ref()} | not_found.
find_session_by_user_id(UserId, Sessions) ->
    maps:fold(
        fun
            (SessionId, {U, Pid, Ref}, _) when U =:= UserId ->
                {ok, SessionId, Pid, Ref};
            (_, _, Acc) ->
                Acc
        end,
        not_found,
        Sessions
    ).

-spec disconnect_user(user_id(), voice_states_map(), sessions_map(), cleanup_fun()) ->
    {ok, voice_states_map(), sessions_map()} | {not_found, voice_states_map(), sessions_map()}.
disconnect_user(UserId, VoiceStates, Sessions, CleanupFun) ->
    case find_session_by_user_id(UserId, Sessions) of
        not_found ->
            {not_found, VoiceStates, Sessions};
        {ok, SessionId, _Pid, Ref} ->
            demonitor(Ref, [flush]),
            CleanupFun(UserId, SessionId),
            NewVoiceStates = maps:remove(UserId, VoiceStates),
            NewSessions = maps:remove(SessionId, Sessions),
            {ok, NewVoiceStates, NewSessions}
    end.

-spec disconnect_user_if_in_channel(
    user_id(), integer(), voice_states_map(), sessions_map(), cleanup_fun()
) ->
    {ok, voice_states_map(), sessions_map()}
    | {not_found, voice_states_map(), sessions_map()}
    | {channel_mismatch, voice_states_map(), sessions_map()}.
disconnect_user_if_in_channel(UserId, ExpectedChannelId, VoiceStates, Sessions, CleanupFun) ->
    case maps:get(UserId, VoiceStates, undefined) of
        undefined ->
            {not_found, VoiceStates, Sessions};
        VoiceState ->
            ChannelIdBin = maps:get(<<"channel_id">>, VoiceState, undefined),
            ExpectedBin = integer_to_binary(ExpectedChannelId),
            case ChannelIdBin =:= ExpectedBin of
                false ->
                    {channel_mismatch, VoiceStates, Sessions};
                true ->
                    disconnect_user(UserId, VoiceStates, Sessions, CleanupFun)
            end
    end.

-spec channel_has_capacity(binary() | integer(), non_neg_integer(), voice_states_map()) ->
    boolean().
channel_has_capacity(_ChannelId, 0, _VoiceStates) ->
    true;
channel_has_capacity(ChannelId, UserLimit, VoiceStates) ->
    ChannelIdBin = ensure_binary(ChannelId),
    UsersInChannel = maps:fold(
        fun(_UserId, VoiceState, Count) ->
            case maps:get(<<"channel_id">>, VoiceState, undefined) of
                ChannelIdBin -> Count + 1;
                _ -> Count
            end
        end,
        0,
        VoiceStates
    ),
    UsersInChannel < UserLimit.

-spec ensure_binary(binary() | integer()) -> binary().
ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_integer(Value) -> integer_to_binary(Value).
