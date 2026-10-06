%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_availability).
-typing([eqwalizer]).

-export([
    is_guild_unavailable_for_user/2,
    is_user_staff/2,
    check_unavailability_transition/2,
    handle_unavailability_transition/2,
    get_cached_unavailability_mode/1,
    is_unavailable_hidden_enabled/1,
    is_unavailable_hidden_enabled_from_cache/1,
    is_guild_unavailable_for_user_from_cache/2,
    update_unavailability_cache_for_state/1,
    schedule_availability_recheck/1,
    handle_availability_recheck/1
]).

-type guild_state() :: map().
-type user_id() :: integer().
-type guild_id() :: integer().
-type unavailability_mode() ::
    available
    | unavailable_for_everyone
    | unavailable_for_everyone_but_staff.

-export_type([guild_state/0, user_id/0, guild_id/0, unavailability_mode/0]).

-define(AVAILABILITY_RECHECK_INTERVAL, 30000).

-spec is_guild_unavailable_for_user(user_id(), guild_state()) -> boolean().
is_guild_unavailable_for_user(UserId, State) ->
    case guild_availability_check:get_unavailability_mode_from_state(State) of
        unavailable_for_everyone ->
            true;
        unavailable_for_everyone_but_staff ->
            not guild_availability_check:is_user_staff(UserId, State);
        available ->
            false
    end.

-spec is_user_staff(user_id(), guild_state()) -> boolean().
is_user_staff(UserId, State) ->
    guild_availability_check:is_user_staff(UserId, State).

-spec check_unavailability_transition(guild_state(), guild_state()) ->
    {unavailable_enabled, boolean()} | unavailable_disabled | no_change.
check_unavailability_transition(OldState, NewState) ->
    guild_availability_check:check_unavailability_transition(OldState, NewState).

-spec get_cached_unavailability_mode(guild_id()) -> unavailability_mode().
get_cached_unavailability_mode(GuildId) ->
    guild_availability_cache:get_cached_unavailability_mode(GuildId).

-spec is_unavailable_hidden_enabled(guild_state()) -> boolean().
is_unavailable_hidden_enabled(State) ->
    guild_availability_check:is_unavailable_hidden_enabled(State).

-spec is_unavailable_hidden_enabled_from_cache(guild_id()) -> boolean().
is_unavailable_hidden_enabled_from_cache(GuildId) ->
    {Mode, UnavailableHidden} = guild_availability_cache:get_cached_unavailability_entry(
        GuildId
    ),
    Mode =/= available andalso UnavailableHidden.

-spec is_guild_unavailable_for_user_from_cache(guild_id(), map()) -> boolean().
is_guild_unavailable_for_user_from_cache(GuildId, UserData) ->
    case guild_availability_cache:get_cached_unavailability_mode(GuildId) of
        unavailable_for_everyone ->
            true;
        unavailable_for_everyone_but_staff ->
            not guild_availability_check:is_user_staff_from_user_data(UserData);
        available ->
            false
    end.

-spec update_unavailability_cache_for_state(guild_state()) -> unavailability_mode().
update_unavailability_cache_for_state(State) ->
    GuildId = maps:get(id, State),
    Mode = guild_availability_check:get_unavailability_mode_from_state(State),
    UnavailableHidden = guild_availability_check:is_unavailable_hidden_enabled(State),
    case UnavailableHidden of
        true ->
            guild_availability_cache:set_cached_unavailability_mode(GuildId, Mode, true);
        false ->
            guild_availability_cache:set_cached_unavailability_mode(GuildId, Mode)
    end,
    Mode.

-spec handle_unavailability_transition(guild_state(), guild_state()) -> guild_state().
handle_unavailability_transition(OldState, NewState) ->
    _ = update_unavailability_cache_for_state(NewState),
    GuildId = maps:get(id, NewState),
    case guild_availability_check:check_unavailability_transition(OldState, NewState) of
        {unavailable_enabled, StaffOnly} ->
            UnavailableHidden = guild_availability_check:is_unavailable_hidden_enabled(
                NewState
            ),
            disconnect_ineligible_sessions(StaffOnly, UnavailableHidden, NewState, GuildId);
        unavailable_disabled ->
            send_guild_create_to_sessions(NewState, GuildId);
        no_change ->
            NewState
    end.

-spec send_guild_create_to_sessions(guild_state(), guild_id()) -> guild_state().
send_guild_create_to_sessions(State, GuildId) ->
    Sessions = maps:get(sessions, State, #{}),
    BulkPresences = presence_utils:collect_guild_member_presences(State),
    maps:foreach(
        fun(_SessionId, SessionData) ->
            send_guild_create_to_session(SessionData, GuildId, State, BulkPresences)
        end,
        Sessions
    ),
    State.

-spec send_guild_create_to_session(map(), guild_id(), guild_state(), list()) -> ok.
send_guild_create_to_session(SessionData, GuildId, State, BulkPresences) ->
    case maps:get(pending_connect, SessionData, false) of
        true ->
            ok;
        false ->
            UserId = maps:get(user_id, SessionData),
            Pid = maps:get(pid, SessionData),
            GuildState = guild_data:get_guild_state(UserId, State),
            gateway_dispatch_relay:dispatch(Pid, guild_create, GuildState, GuildId),
            presence_utils:send_presence_bulk(Pid, GuildId, UserId, BulkPresences)
    end.

-spec disconnect_ineligible_sessions(boolean(), boolean(), guild_state(), guild_id()) ->
    guild_state().
disconnect_ineligible_sessions(StaffOnly, UnavailableHidden, State, GuildId) ->
    Sessions = maps:get(sessions, State, #{}),
    Ctx = {StaffOnly, UnavailableHidden, GuildId},
    {FinalState, _} = maps:fold(
        fun(SessionId, SessionData, Acc) ->
            maybe_disconnect_session(SessionId, SessionData, Ctx, Acc)
        end,
        {State, sets:new()},
        Sessions
    ),
    FinalState.

-spec maybe_disconnect_session(
    binary(),
    map(),
    {boolean(), boolean(), guild_id()},
    {guild_state(), sets:set(user_id())}
) -> {guild_state(), sets:set(user_id())}.
maybe_disconnect_session(
    SessionId,
    SessionData,
    {StaffOnly, UnavailableHidden, GuildId},
    {AccState, ProcessedUsers}
) ->
    UserId = maps:get(user_id, SessionData),
    case should_disconnect_user(UserId, StaffOnly, AccState) of
        true ->
            do_disconnect(
                SessionId,
                SessionData,
                UserId,
                UnavailableHidden,
                GuildId,
                AccState,
                ProcessedUsers
            );
        false ->
            {AccState, ProcessedUsers}
    end.

-spec do_disconnect(
    binary(),
    map(),
    user_id(),
    boolean(),
    guild_id(),
    guild_state(),
    sets:set(user_id())
) -> {guild_state(), sets:set(user_id())}.
do_disconnect(
    SessionId, SessionData, UserId, UnavailableHidden, GuildId, State, ProcessedUsers
) ->
    Pid = maps:get(pid, SessionData, undefined),
    maybe_send_guild_leave(Pid, GuildId, UnavailableHidden),
    {VoiceState, UpdatedUsers} = maybe_disconnect_voice(UserId, ProcessedUsers, State),
    NewState = guild_sessions:remove_session(SessionId, VoiceState),
    {NewState, UpdatedUsers}.

-spec should_disconnect_user(user_id(), boolean(), guild_state()) -> boolean().
should_disconnect_user(UserId, true, State) ->
    not guild_availability_check:is_user_staff(UserId, State);
should_disconnect_user(_UserId, false, _State) ->
    true.

-spec maybe_send_guild_leave(pid() | undefined, guild_id(), boolean()) -> ok.
maybe_send_guild_leave(Pid, GuildId, true) when is_pid(Pid) ->
    gen_server:cast(Pid, {guild_leave, GuildId, forced_unavailable, true}),
    ok;
maybe_send_guild_leave(Pid, GuildId, false) when is_pid(Pid) ->
    gen_server:cast(Pid, {guild_leave, GuildId, forced_unavailable}),
    ok;
maybe_send_guild_leave(_Pid, _GuildId, _UnavailableHidden) ->
    ok.

-spec maybe_disconnect_voice(user_id(), sets:set(user_id()), guild_state()) ->
    {guild_state(), sets:set(user_id())}.
maybe_disconnect_voice(UserId, ProcessedUsers, State) ->
    case sets:is_element(UserId, ProcessedUsers) of
        true ->
            {State, ProcessedUsers};
        false ->
            ok = guild_voice_lifecycle:cast_disconnect_voice_user(UserId, State),
            {State, sets:add_element(UserId, ProcessedUsers)}
    end.

-spec schedule_availability_recheck(guild_state()) -> guild_state().
schedule_availability_recheck(State) ->
    case guild_availability_check:get_unavailability_mode_from_state(State) of
        available ->
            State;
        _Unavailable ->
            erlang:send_after(?AVAILABILITY_RECHECK_INTERVAL, self(), availability_recheck),
            State
    end.

-spec handle_availability_recheck(guild_state()) -> guild_state().
handle_availability_recheck(State) ->
    GuildId = maps:get(id, State),
    OldMode = guild_availability_cache:get_cached_unavailability_mode(GuildId),
    CurrentMode = guild_availability_check:get_unavailability_mode_from_state(State),
    NewState = handle_recheck_transition(OldMode, CurrentMode, GuildId, State),
    _ = update_unavailability_cache_for_state(NewState),
    _ = schedule_availability_recheck(NewState),
    NewState.

-spec handle_recheck_transition(
    unavailability_mode(), unavailability_mode(), guild_id(), guild_state()
) -> guild_state().
handle_recheck_transition(OldMode, available, GuildId, State) when OldMode =/= available ->
    guild_availability_cache:set_cached_unavailability_mode(GuildId, available),
    send_guild_create_to_sessions(State, GuildId);
handle_recheck_transition(_OldMode, _CurrentMode, _GuildId, State) ->
    State.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

schedule_availability_recheck_available_no_timer_test() ->
    State = #{data => #{<<"guild">> => #{<<"features">> => []}}},
    ?assertEqual(State, schedule_availability_recheck(State)),
    receive
        availability_recheck -> ?assert(false, unexpected_timer)
    after 50 ->
        ok
    end.

schedule_availability_recheck_unavailable_sets_timer_test() ->
    State = #{data => #{<<"guild">> => #{<<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]}}},
    Result = schedule_availability_recheck(State),
    ?assertEqual(State, Result).

handle_recheck_transition_clears_cache_on_recovery_test() ->
    GuildId = 99001,
    State = #{
        id => GuildId,
        sessions => #{},
        data => #{<<"guild">> => #{<<"features">> => []}}
    },
    guild_availability_cache:set_cached_unavailability_mode(GuildId, unavailable_for_everyone),
    try
        _ = handle_recheck_transition(unavailable_for_everyone, available, GuildId, State),
        ?assertEqual(
            available, guild_availability_cache:get_cached_unavailability_mode(GuildId)
        )
    after
        guild_availability_cache:set_cached_unavailability_mode(GuildId, available)
    end.

handle_recheck_transition_no_change_when_still_unavailable_test() ->
    GuildId = 99002,
    State = #{
        id => GuildId,
        sessions => #{},
        data => #{<<"guild">> => #{<<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]}}
    },
    guild_availability_cache:set_cached_unavailability_mode(GuildId, unavailable_for_everyone),
    try
        Result = handle_recheck_transition(
            unavailable_for_everyone, unavailable_for_everyone, GuildId, State
        ),
        ?assertEqual(State, Result)
    after
        guild_availability_cache:set_cached_unavailability_mode(GuildId, available)
    end.

-endif.
