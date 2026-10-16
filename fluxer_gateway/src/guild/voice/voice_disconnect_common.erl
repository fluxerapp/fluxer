%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(voice_disconnect_common).
-typing([eqwalizer]).

-export([
    disconnect_user/4,
    disconnect_user_if_in_channel/6
]).

-export_type([
    user_id/0,
    session_id/0,
    session_pid/0,
    monitor_ref/0,
    session_tuple/0,
    sessions_map/0,
    voice_states_map/0,
    cleanup_fun/0
]).

-type user_id() :: integer().
-type session_id() :: binary().
-type session_pid() :: pid().
-type monitor_ref() :: reference().
-type session_tuple() :: {user_id(), session_pid(), monitor_ref()}.
-type sessions_map() :: #{session_id() => session_tuple()}.
-type voice_states_map() :: #{user_id() => map()}.
-type cleanup_fun() :: fun((user_id(), session_id()) -> ok).

-spec disconnect_user(user_id(), voice_states_map(), sessions_map(), cleanup_fun()) ->
    {ok, voice_states_map(), sessions_map()} | {not_found, voice_states_map(), sessions_map()}.
disconnect_user(UserId, VoiceStates, Sessions, CleanupFun) ->
    case sessions_for_user(UserId, Sessions) of
        [] ->
            {not_found, VoiceStates, Sessions};
        UserSessions ->
            NewSessions = remove_user_sessions(UserId, UserSessions, Sessions, CleanupFun),
            {ok, maps:remove(UserId, VoiceStates), NewSessions}
    end.

-spec remove_user_sessions(
    user_id(), [{session_id(), monitor_ref()}], sessions_map(), cleanup_fun()
) -> sessions_map().
remove_user_sessions(UserId, UserSessions, Sessions, CleanupFun) ->
    lists:foldl(
        fun({SessionId, Ref}, Acc) ->
            demonitor(Ref, [flush]),
            CleanupFun(UserId, SessionId),
            maps:remove(SessionId, Acc)
        end,
        Sessions,
        UserSessions
    ).

-spec sessions_for_user(user_id(), sessions_map()) -> [{session_id(), monitor_ref()}].
sessions_for_user(UserId, Sessions) ->
    maps:fold(
        fun
            (SessionId, {U, _Pid, Ref}, Acc) when U =:= UserId ->
                [{SessionId, Ref} | Acc];
            (_, _, Acc) ->
                Acc
        end,
        [],
        Sessions
    ).

-spec disconnect_user_if_in_channel(
    user_id(),
    integer(),
    binary() | undefined,
    voice_states_map(),
    sessions_map(),
    cleanup_fun()
) ->
    {ok, voice_states_map(), sessions_map()}
    | {not_found, voice_states_map(), sessions_map()}
    | {channel_mismatch, voice_states_map(), sessions_map()}.
disconnect_user_if_in_channel(
    UserId, ExpectedChannelId, ExpectedConnectionId, VoiceStates, Sessions, CleanupFun
) ->
    case maps:get(UserId, VoiceStates, undefined) of
        undefined ->
            {not_found, VoiceStates, Sessions};
        VoiceState ->
            case connection_matches(VoiceState, ExpectedConnectionId) of
                false ->
                    {not_found, VoiceStates, Sessions};
                true ->
                    disconnect_matching_channel(
                        UserId,
                        ExpectedChannelId,
                        VoiceState,
                        VoiceStates,
                        Sessions,
                        CleanupFun
                    )
            end
    end.

-spec connection_matches(map(), binary() | undefined) -> boolean().
connection_matches(_VoiceState, undefined) ->
    true;
connection_matches(VoiceState, ExpectedConnectionId) ->
    case maps:get(<<"connection_id">>, VoiceState, undefined) of
        undefined -> true;
        ExpectedConnectionId -> true;
        _ -> false
    end.

-spec disconnect_matching_channel(
    user_id(), integer(), map(), voice_states_map(), sessions_map(), cleanup_fun()
) ->
    {ok, voice_states_map(), sessions_map()}
    | {not_found, voice_states_map(), sessions_map()}
    | {channel_mismatch, voice_states_map(), sessions_map()}.
disconnect_matching_channel(
    UserId, ExpectedChannelId, VoiceState, VoiceStates, Sessions, CleanupFun
) ->
    ChannelIdBin = maps:get(<<"channel_id">>, VoiceState, undefined),
    case snowflake_id:equal(ExpectedChannelId, ChannelIdBin) of
        false -> {channel_mismatch, VoiceStates, Sessions};
        true -> disconnect_user(UserId, VoiceStates, Sessions, CleanupFun)
    end.
