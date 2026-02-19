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

-module(guild_availability).

-export([
    is_guild_unavailable_for_user/2,
    is_user_staff/2,
    check_unavailability_transition/2,
    handle_unavailability_transition/2,
    get_cached_unavailability_mode/1,
    is_guild_unavailable_for_user_from_cache/2,
    update_unavailability_cache_for_state/1
]).

-type guild_state() :: map().
-type user_id() :: integer().
-type guild_id() :: integer().
-type unavailability_mode() ::
    available
    | unavailable_for_everyone
    | unavailable_for_everyone_but_staff.
-type transition_result() :: {unavailable_enabled, boolean()} | unavailable_disabled | no_change.

-define(GUILD_UNAVAILABILITY_CACHE, guild_unavailability_cache).
-define(STAFF_USER_FLAG, 16#1).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec is_guild_unavailable_for_user(user_id(), guild_state()) -> boolean().
is_guild_unavailable_for_user(UserId, State) ->
    case get_unavailability_mode_from_state(State) of
        unavailable_for_everyone ->
            true;
        unavailable_for_everyone_but_staff ->
            not is_user_staff(UserId, State);
        available ->
            false
    end.

-spec is_user_staff(user_id(), guild_state()) -> boolean().
is_user_staff(UserId, State) ->
    case is_user_staff_from_sessions(UserId, State) of
        true ->
            true;
        false ->
            false;
        undefined ->
            case guild_permissions:find_member_by_user_id(UserId, State) of
                undefined ->
                    false;
                Member ->
                    User = maps:get(<<"user">>, Member, #{}),
                    is_user_staff_from_user_data(User)
            end
    end.

-spec is_user_staff_from_sessions(user_id(), guild_state()) -> boolean() | undefined.
is_user_staff_from_sessions(UserId, State) ->
    Sessions = maps:get(sessions, State, #{}),
    maps:fold(
        fun(_SessionId, SessionData, Acc) ->
            case Acc of
                undefined ->
                    SessionUserId = maps:get(user_id, SessionData, undefined),
                    SessionIsStaff = maps:get(is_staff, SessionData, undefined),
                    case {SessionUserId =:= UserId, SessionIsStaff} of
                        {true, true} ->
                            true;
                        {true, false} ->
                            false;
                        _ ->
                            undefined
                    end;
                _ ->
                    Acc
            end
        end,
        undefined,
        Sessions
    ).

-spec get_cached_unavailability_mode(guild_id()) -> unavailability_mode().
get_cached_unavailability_mode(GuildId) ->
    ensure_unavailability_cache_table(),
    case ets:lookup(?GUILD_UNAVAILABILITY_CACHE, GuildId) of
        [{GuildId, Mode}] ->
            normalize_unavailability_mode(Mode);
        [] ->
            available
    end.

-spec is_guild_unavailable_for_user_from_cache(guild_id(), map()) -> boolean().
is_guild_unavailable_for_user_from_cache(GuildId, UserData) ->
    case get_cached_unavailability_mode(GuildId) of
        unavailable_for_everyone ->
            true;
        unavailable_for_everyone_but_staff ->
            not is_user_staff_from_user_data(UserData);
        available ->
            false
    end.

-spec update_unavailability_cache_for_state(guild_state()) -> unavailability_mode().
update_unavailability_cache_for_state(State) ->
    GuildId = maps:get(id, State),
    Mode = get_unavailability_mode_from_state(State),
    set_cached_unavailability_mode(GuildId, Mode),
    Mode.

-spec check_unavailability_transition(guild_state(), guild_state()) -> transition_result().
check_unavailability_transition(OldState, NewState) ->
    OldMode = get_unavailability_mode_from_state(OldState),
    NewMode = get_unavailability_mode_from_state(NewState),
    case {OldMode, NewMode} of
        {available, unavailable_for_everyone} ->
            {unavailable_enabled, false};
        {available, unavailable_for_everyone_but_staff} ->
            {unavailable_enabled, true};
        {unavailable_for_everyone, available} ->
            unavailable_disabled;
        {unavailable_for_everyone_but_staff, available} ->
            unavailable_disabled;
        {unavailable_for_everyone_but_staff, unavailable_for_everyone} ->
            {unavailable_enabled, false};
        {unavailable_for_everyone, unavailable_for_everyone_but_staff} ->
            {unavailable_enabled, true};
        _ ->
            no_change
    end.

-spec handle_unavailability_transition(guild_state(), guild_state()) -> guild_state().
handle_unavailability_transition(OldState, NewState) ->
    _ = update_unavailability_cache_for_state(NewState),
    GuildId = maps:get(id, NewState),
    case check_unavailability_transition(OldState, NewState) of
        {unavailable_enabled, StaffOnly} ->
            disconnect_ineligible_sessions(StaffOnly, NewState, GuildId);
        unavailable_disabled ->
            Sessions = maps:get(sessions, NewState, #{}),
            BulkPresences = presence_utils:collect_guild_member_presences(NewState),
            lists:foreach(
                fun({_SessionId, SessionData}) ->
                    case maps:get(pending_connect, SessionData, false) of
                        true ->
                            ok;
                        false ->
                            UserId = maps:get(user_id, SessionData),
                            Pid = maps:get(pid, SessionData),
                            GuildState = guild_data:get_guild_state(UserId, NewState),
                            gen_server:cast(Pid, {dispatch, guild_create, GuildState}),
                            presence_utils:send_presence_bulk(Pid, GuildId, UserId, BulkPresences)
                    end
                end,
                maps:to_list(Sessions)
            ),
            NewState;
        no_change ->
            NewState
    end.

-spec disconnect_ineligible_sessions(boolean(), guild_state(), guild_id()) -> guild_state().
disconnect_ineligible_sessions(StaffOnly, State, GuildId) ->
    Sessions = maps:get(sessions, State, #{}),
    {FinalState, _DisconnectedUsers} = lists:foldl(
        fun({SessionId, SessionData}, {AccState, ProcessedUsers}) ->
            UserId = maps:get(user_id, SessionData),
            case should_disconnect_user(UserId, StaffOnly, AccState) of
                true ->
                    Pid = maps:get(pid, SessionData, undefined),
                    maybe_send_guild_leave(Pid, GuildId),
                    {VoiceState, UpdatedUsers} =
                        maybe_disconnect_voice_for_user(UserId, ProcessedUsers, AccState),
                    NewState = guild_sessions:remove_session(SessionId, VoiceState),
                    {NewState, UpdatedUsers};
                false ->
                    {AccState, ProcessedUsers}
            end
        end,
        {State, sets:new()},
        maps:to_list(Sessions)
    ),
    FinalState.

-spec should_disconnect_user(user_id(), boolean(), guild_state()) -> boolean().
should_disconnect_user(UserId, true, State) ->
    not is_user_staff(UserId, State);
should_disconnect_user(_UserId, false, _State) ->
    true.

-spec maybe_send_guild_leave(pid() | undefined, guild_id()) -> ok.
maybe_send_guild_leave(Pid, GuildId) when is_pid(Pid) ->
    gen_server:cast(Pid, {guild_leave, GuildId, forced_unavailable}),
    ok;
maybe_send_guild_leave(_Pid, _GuildId) ->
    ok.

-spec maybe_disconnect_voice_for_user(user_id(), sets:set(user_id()), guild_state()) ->
    {guild_state(), sets:set(user_id())}.
maybe_disconnect_voice_for_user(UserId, ProcessedUsers, State) ->
    case sets:is_element(UserId, ProcessedUsers) of
        true ->
            {State, ProcessedUsers};
        false ->
            {reply, _Result, VoiceState} = guild_voice_disconnect:disconnect_voice_user(
                #{user_id => UserId, connection_id => null},
                State
            ),
            {VoiceState, sets:add_element(UserId, ProcessedUsers)}
    end.

-spec ensure_unavailability_cache_table() -> ok.
ensure_unavailability_cache_table() ->
    guild_ets_utils:ensure_table(?GUILD_UNAVAILABILITY_CACHE, [named_table, public, set, {read_concurrency, true}]).

-spec set_cached_unavailability_mode(guild_id(), unavailability_mode()) -> ok.
set_cached_unavailability_mode(GuildId, available) ->
    ensure_unavailability_cache_table(),
    ets:delete(?GUILD_UNAVAILABILITY_CACHE, GuildId),
    ok;
set_cached_unavailability_mode(GuildId, Mode) ->
    ensure_unavailability_cache_table(),
    ets:insert(?GUILD_UNAVAILABILITY_CACHE, {GuildId, Mode}),
    ok.

-spec normalize_unavailability_mode(term()) -> unavailability_mode().
normalize_unavailability_mode(unavailable_for_everyone) ->
    unavailable_for_everyone;
normalize_unavailability_mode(unavailable_for_everyone_but_staff) ->
    unavailable_for_everyone_but_staff;
normalize_unavailability_mode(_) ->
    available.

-spec get_unavailability_mode_from_state(guild_state()) -> unavailability_mode().
get_unavailability_mode_from_state(State) ->
    Data = maps:get(data, State, #{}),
    Guild = maps:get(<<"guild">>, Data, #{}),
    Features = maps:get(<<"features">>, Guild, []),
    get_unavailability_mode_from_features(Features).

-spec get_unavailability_mode_from_features(term()) -> unavailability_mode().
get_unavailability_mode_from_features(Features) when is_list(Features) ->
    HasUnavailableForEveryone = lists:member(<<"UNAVAILABLE_FOR_EVERYONE">>, Features),
    HasUnavailableForEveryoneButStaff =
        lists:member(<<"UNAVAILABLE_FOR_EVERYONE_BUT_STAFF">>, Features),
    case {HasUnavailableForEveryone, HasUnavailableForEveryoneButStaff} of
        {true, _} ->
            unavailable_for_everyone;
        {false, true} ->
            unavailable_for_everyone_but_staff;
        {false, false} ->
            available
    end;
get_unavailability_mode_from_features(_) ->
    available.

-spec is_user_staff_from_user_data(map()) -> boolean().
is_user_staff_from_user_data(UserData) when is_map(UserData) ->
    case parse_is_staff_value(maps:get(<<"is_staff">>, UserData, undefined)) of
        undefined ->
            is_user_staff_from_flags(UserData);
        IsStaff ->
            IsStaff
    end;
is_user_staff_from_user_data(_) ->
    false.

-spec parse_is_staff_value(term()) -> boolean() | undefined.
parse_is_staff_value(true) ->
    true;
parse_is_staff_value(false) ->
    false;
parse_is_staff_value(<<"true">>) ->
    true;
parse_is_staff_value(<<"false">>) ->
    false;
parse_is_staff_value(_) ->
    undefined.

-spec is_user_staff_from_flags(map()) -> boolean().
is_user_staff_from_flags(UserData) ->
    FlagsValue = maps:get(<<"flags">>, UserData, 0),
    Flags = type_conv:to_integer(FlagsValue),
    case Flags of
        undefined ->
            false;
        Value when is_integer(Value) ->
            (Value band ?STAFF_USER_FLAG) =:= ?STAFF_USER_FLAG
    end.

-ifdef(TEST).

-spec cleanup_unavailability_cache(guild_id()) -> ok.
cleanup_unavailability_cache(GuildId) ->
    set_cached_unavailability_mode(GuildId, available).

disconnect_ineligible_sessions_staff_only_test() ->
    Parent = self(),
    GuildId = 99001,
    NonStaffPid = start_session_capture(non_staff, Parent),
    StaffPid = start_session_capture(staff, Parent),
    try
        BaseState = state_for_unavailability_transition_test(GuildId, NonStaffPid, StaffPid),
        OldState = BaseState,
        NewState = BaseState#{
            data => #{
                <<"guild">> => #{
                    <<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE_BUT_STAFF">>]
                },
                <<"members">> => [
                    #{<<"user">> => #{<<"id">> => <<"1001">>, <<"flags">> => <<"0">>}},
                    #{<<"user">> => #{<<"id">> => <<"1002">>, <<"flags">> => <<"1">>}}
                ]
            }
        },
        UpdatedState = handle_unavailability_transition(OldState, NewState),
        Sessions = maps:get(sessions, UpdatedState, #{}),
        ?assertEqual(1, map_size(Sessions)),
        ?assert(maps:is_key(<<"staff">>, Sessions)),
        ?assertEqual(unavailable_for_everyone_but_staff, get_cached_unavailability_mode(GuildId)),
        receive
            {non_staff, {'$gen_cast', {guild_leave, GuildId, forced_unavailable}}} -> ok
        after 1000 ->
            ?assert(false)
        end,
        receive
            {staff, {'$gen_cast', {guild_leave, GuildId, forced_unavailable}}} ->
                ?assert(false)
        after 200 ->
            ok
        end
    after
        cleanup_unavailability_cache(GuildId),
        NonStaffPid ! stop,
        StaffPid ! stop
    end.

disconnect_ineligible_sessions_everyone_test() ->
    Parent = self(),
    GuildId = 99002,
    UserOnePid = start_session_capture(user_one, Parent),
    UserTwoPid = start_session_capture(user_two, Parent),
    try
        BaseState = state_for_unavailability_transition_test(GuildId, UserOnePid, UserTwoPid),
        OldState = BaseState,
        NewState = BaseState#{
            data => #{
                <<"guild">> => #{
                    <<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]
                },
                <<"members">> => [
                    #{<<"user">> => #{<<"id">> => <<"1001">>, <<"flags">> => <<"0">>}},
                    #{<<"user">> => #{<<"id">> => <<"1002">>, <<"flags">> => <<"1">>}}
                ]
            }
        },
        UpdatedState = handle_unavailability_transition(OldState, NewState),
        ?assertEqual(#{}, maps:get(sessions, UpdatedState, #{})),
        ?assertEqual(unavailable_for_everyone, get_cached_unavailability_mode(GuildId)),
        receive
            {user_one, {'$gen_cast', {guild_leave, GuildId, forced_unavailable}}} -> ok
        after 1000 ->
            ?assert(false)
        end,
        receive
            {user_two, {'$gen_cast', {guild_leave, GuildId, forced_unavailable}}} -> ok
        after 1000 ->
            ?assert(false)
        end
    after
        cleanup_unavailability_cache(GuildId),
        UserOnePid ! stop,
        UserTwoPid ! stop
    end.

is_guild_unavailable_for_user_from_cache_is_staff_test() ->
    GuildId = 99003,
    try
        set_cached_unavailability_mode(GuildId, unavailable_for_everyone_but_staff),
        ?assertEqual(
            true,
            is_guild_unavailable_for_user_from_cache(
                GuildId,
                #{<<"is_staff">> => false}
            )
        ),
        ?assertEqual(
            false,
            is_guild_unavailable_for_user_from_cache(
                GuildId,
                #{<<"is_staff">> => true}
            )
        )
    after
        cleanup_unavailability_cache(GuildId)
    end.

-spec start_session_capture(atom(), pid()) -> pid().
start_session_capture(Tag, Parent) ->
    spawn(fun() -> session_capture_loop(Tag, Parent) end).

-spec session_capture_loop(atom(), pid()) -> ok.
session_capture_loop(Tag, Parent) ->
    receive
        stop ->
            ok;
        {'$gen_cast', Msg} ->
            Parent ! {Tag, {'$gen_cast', Msg}},
            session_capture_loop(Tag, Parent);
        _Other ->
            session_capture_loop(Tag, Parent)
    end.

-spec state_for_unavailability_transition_test(guild_id(), pid(), pid()) -> guild_state().
state_for_unavailability_transition_test(GuildId, NonStaffPid, StaffPid) ->
    #{
        id => GuildId,
        sessions => #{
            <<"non_staff">> => #{
                session_id => <<"non_staff">>,
                user_id => 1001,
                pid => NonStaffPid,
                mref => make_ref(),
                active_guilds => sets:new(),
                user_roles => [],
                bot => false,
                is_staff => false
            },
            <<"staff">> => #{
                session_id => <<"staff">>,
                user_id => 1002,
                pid => StaffPid,
                mref => make_ref(),
                active_guilds => sets:new(),
                user_roles => [],
                bot => false,
                is_staff => true
            }
        },
        presence_subscriptions => #{},
        member_list_subscriptions => #{},
        member_subscriptions => guild_subscriptions:init_state(),
        data => #{
            <<"guild">> => #{
                <<"features">> => []
            },
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => <<"1001">>, <<"flags">> => <<"0">>}},
                #{<<"user">> => #{<<"id">> => <<"1002">>, <<"flags">> => <<"1">>}}
            ]
        },
        voice_states => #{},
        pending_voice_connections => #{}
    }.

is_guild_unavailable_for_user_unavailable_for_everyone_test() ->
    State = #{
        data => #{
            <<"guild">> => #{
                <<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]
            },
            <<"members">> => []
        }
    },
    ?assertEqual(true, is_guild_unavailable_for_user(123, State)).

is_guild_unavailable_for_user_available_test() ->
    State = #{
        data => #{
            <<"guild">> => #{
                <<"features">> => []
            },
            <<"members">> => []
        }
    },
    ?assertEqual(false, is_guild_unavailable_for_user(123, State)).

check_unavailability_transition_no_change_test() ->
    State = #{
        data => #{
            <<"guild">> => #{
                <<"features">> => []
            }
        }
    },
    ?assertEqual(no_change, check_unavailability_transition(State, State)).

check_unavailability_transition_enabled_test() ->
    OldState = #{
        data => #{
            <<"guild">> => #{
                <<"features">> => []
            }
        }
    },
    NewState = #{
        data => #{
            <<"guild">> => #{
                <<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]
            }
        }
    },
    ?assertEqual({unavailable_enabled, false}, check_unavailability_transition(OldState, NewState)).

check_unavailability_transition_disabled_test() ->
    OldState = #{
        data => #{
            <<"guild">> => #{
                <<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]
            }
        }
    },
    NewState = #{
        data => #{
            <<"guild">> => #{
                <<"features">> => []
            }
        }
    },
    ?assertEqual(unavailable_disabled, check_unavailability_transition(OldState, NewState)).

-endif.
