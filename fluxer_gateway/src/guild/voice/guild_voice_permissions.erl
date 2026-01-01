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

-module(guild_voice_permissions).

-export([check_voice_permissions_and_limits/6]).

-type guild_state() :: map().
-type voice_state_map() :: #{binary() => map()}.
-type channel() :: map().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-import(utils, [parse_iso8601_to_unix_ms/1]).

-spec check_voice_permissions_and_limits(
    integer(), integer(), channel(), voice_state_map(), guild_state(), boolean()
) ->
    {ok, allowed} | {error, atom(), atom()}.
check_voice_permissions_and_limits(UserId, ChannelIdValue, Channel, VoiceStates, State, IsUpdate) ->
    case is_member_timed_out(UserId, State) of
        true ->
            gateway_errors:error(voice_member_timed_out);
        false ->
            case has_view_and_connect_perms(UserId, ChannelIdValue, State) of
                false ->
                    gateway_errors:error(voice_permission_denied);
                true ->
                    case
                        channel_has_capacity(UserId, ChannelIdValue, Channel, VoiceStates, IsUpdate)
                    of
                        true -> {ok, allowed};
                        false -> gateway_errors:error(voice_channel_full)
                    end
            end
    end.

-spec has_view_and_connect_perms(integer(), integer(), guild_state()) -> boolean().
has_view_and_connect_perms(UserId, ChannelIdValue, State) ->
    case guild_virtual_channel_access:has_virtual_access(UserId, ChannelIdValue, State) of
        true ->
            true;
        false ->
            Permissions = resolve_permissions(UserId, ChannelIdValue, State),
            ViewPerm = constants:view_channel_permission(),
            ConnectPerm = constants:connect_permission(),
            (Permissions band ViewPerm) =:= ViewPerm andalso
                (Permissions band ConnectPerm) =:= ConnectPerm
    end.

-spec channel_has_capacity(integer(), integer(), channel(), voice_state_map(), boolean()) ->
    boolean().
channel_has_capacity(UserId, ChannelIdValue, Channel, VoiceStates, IsUpdate) ->
    UserLimit = maps:get(<<"user_limit">>, Channel, 0),
    case UserLimit of
        0 ->
            true;
        Limit when Limit > 0 ->
            UsersInChannel = users_in_channel(ChannelIdValue, VoiceStates),
            CurrentCount = sets:size(UsersInChannel),
            AlreadyPresent = sets:is_element(UserId, UsersInChannel),
            AdjustedCount =
                case AlreadyPresent orelse IsUpdate of
                    true -> CurrentCount - 1;
                    false -> CurrentCount
                end,
            AdjustedCount < Limit;
        _ ->
            true
    end.

-spec is_member_timed_out(integer(), guild_state()) -> boolean().
is_member_timed_out(UserId, State) ->
    case guild_permissions:find_member_by_user_id(UserId, State) of
        undefined ->
            false;
        Member ->
            TimeoutMs = parse_iso8601_to_unix_ms(
                maps:get(<<"communication_disabled_until">>, Member, undefined)
            ),
            case TimeoutMs of
                undefined ->
                    false;
                Value when is_integer(Value) ->
                    Value > erlang:system_time(millisecond);
                _ ->
                    false
            end
    end.

-spec users_in_channel(integer(), voice_state_map()) -> sets:set().
users_in_channel(ChannelIdValue, VoiceStates0) ->
    VoiceStates = voice_state_utils:ensure_voice_states(VoiceStates0),
    maps:fold(
        fun(_ConnId, VState, Acc) ->
            case voice_state_utils:voice_state_channel_id(VState) of
                ChannelIdValue ->
                    case voice_state_utils:voice_state_user_id(VState) of
                        undefined -> Acc;
                        UserId -> sets:add_element(UserId, Acc)
                    end;
                _ ->
                    Acc
            end
        end,
        sets:new(),
        VoiceStates
    ).

-spec resolve_permissions(integer(), integer(), guild_state()) -> integer().
resolve_permissions(UserId, ChannelIdValue, State) ->
    case State of
        #{test_perm_fun := Fun} when is_function(Fun, 1) ->
            Fun(UserId);
        _ ->
            guild_permissions:get_member_permissions(UserId, ChannelIdValue, State)
    end.

-ifdef(TEST).

voice_permissions_missing_view_test() ->
    State = permission_test_state(0, fun(_) -> constants:view_channel_permission() end),
    Result = check_voice_permissions_and_limits(1, 10, #{<<"user_limit">> => 0}, #{}, State, false),
    ?assertMatch({error, permission_denied, voice_permission_denied}, Result).

voice_permissions_full_channel_test() ->
    State = permission_test_state(2, fun(_) -> required_voice_perms() end),
    VoiceStates = #{
        <<"conn1">> => #{<<"channel_id">> => <<"10">>, <<"user_id">> => <<"1">>},
        <<"conn2">> => #{<<"channel_id">> => <<"10">>, <<"user_id">> => <<"2">>}
    },
    Result = check_voice_permissions_and_limits(
        3, 10, #{<<"user_limit">> => 2}, VoiceStates, State, false
    ),
    ?assertMatch({error, permission_denied, voice_channel_full}, Result).

voice_permissions_existing_user_update_test() ->
    State = permission_test_state(2, fun(_) -> required_voice_perms() end),
    VoiceStates = #{
        <<"conn1">> => #{<<"channel_id">> => <<"10">>, <<"user_id">> => <<"1">>},
        <<"conn2">> => #{<<"channel_id">> => <<"10">>, <<"user_id">> => <<"2">>}
    },
    Result = check_voice_permissions_and_limits(
        1, 10, #{<<"user_limit">> => 2}, VoiceStates, State, true
    ),
    ?assertEqual({ok, allowed}, Result).

required_voice_perms() ->
    constants:view_channel_permission() bor constants:connect_permission().

permission_test_state(GuildId, PermFun) ->
    #{id => GuildId, test_perm_fun => PermFun}.

-endif.
