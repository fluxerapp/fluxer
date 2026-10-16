%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_availability_check).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-typing([eqwalizer]).

-export([
    get_unavailability_mode_from_state/1,
    is_unavailable_hidden_enabled/1,
    is_unavailable_hidden_enabled_from_features/1,
    is_user_staff/2,
    is_user_staff_from_user_data/1,
    check_unavailability_transition/2
]).

-type guild_state() :: map().
-type user_id() :: integer().
-type unavailability_mode() ::
    available
    | unavailable_for_everyone
    | unavailable_for_everyone_but_staff.
-type transition_result() ::
    {unavailable_enabled, boolean()} | unavailable_disabled | no_change.

-export_type([guild_state/0, user_id/0, unavailability_mode/0, transition_result/0]).

-spec get_unavailability_mode_from_state(guild_state()) -> unavailability_mode().
get_unavailability_mode_from_state(State) ->
    Data = maps:get(data, State, #{}),
    Guild = maps:get(<<"guild">>, Data, #{}),
    Features = maps:get(<<"features">>, Guild, []),
    get_unavailability_mode_from_features(Features).

-spec get_unavailability_mode_from_features(term()) -> unavailability_mode().
get_unavailability_mode_from_features(Features) when is_list(Features) ->
    HasForEveryone = lists:member(<<"UNAVAILABLE_FOR_EVERYONE">>, Features),
    HasForEveryoneButStaff =
        lists:member(<<"UNAVAILABLE_FOR_EVERYONE_BUT_STAFF">>, Features),
    case {HasForEveryone, HasForEveryoneButStaff} of
        {true, _} -> unavailable_for_everyone;
        {false, true} -> unavailable_for_everyone_but_staff;
        {false, false} -> available
    end;
get_unavailability_mode_from_features(_) ->
    available.

-spec is_unavailable_hidden_enabled(guild_state()) -> boolean().
is_unavailable_hidden_enabled(State) ->
    Data = maps:get(data, State, #{}),
    Guild = maps:get(<<"guild">>, Data, #{}),
    Features = maps:get(<<"features">>, Guild, []),
    is_unavailable_hidden_enabled_from_features(Features).

-spec is_unavailable_hidden_enabled_from_features(term()) -> boolean().
is_unavailable_hidden_enabled_from_features(Features) when is_list(Features) ->
    HasUnavailableHidden = lists:member(<<"UNAVAILABLE_HIDDEN">>, Features),
    IsUnavailable = get_unavailability_mode_from_features(Features) =/= available,
    HasUnavailableHidden andalso IsUnavailable;
is_unavailable_hidden_enabled_from_features(_) ->
    false.

-spec is_user_staff(user_id(), guild_state()) -> boolean().
is_user_staff(UserId, State) ->
    case is_user_staff_from_sessions(UserId, State) of
        true -> true;
        false -> false;
        undefined -> is_user_staff_from_member(UserId, State)
    end.

-spec is_user_staff_from_sessions(user_id(), guild_state()) -> boolean() | undefined.
is_user_staff_from_sessions(UserId, State) ->
    Sessions = maps:get(sessions, State, #{}),
    maps:fold(
        fun(_SessionId, SessionData, Acc) ->
            fold_check_staff(UserId, SessionData, Acc)
        end,
        undefined,
        Sessions
    ).

-spec fold_check_staff(user_id(), map(), boolean() | undefined) -> boolean() | undefined.
fold_check_staff(_UserId, _SessionData, Acc) when Acc =/= undefined ->
    Acc;
fold_check_staff(UserId, SessionData, undefined) ->
    check_session_staff(UserId, SessionData).

-spec check_session_staff(user_id(), map()) -> boolean() | undefined.
check_session_staff(UserId, SessionData) ->
    SessionUserId = maps:get(user_id, SessionData, undefined),
    SessionIsStaff = maps:get(is_staff, SessionData, undefined),
    case {SessionUserId =:= UserId, SessionIsStaff} of
        {true, true} -> true;
        {true, false} -> false;
        _ -> undefined
    end.

-spec is_user_staff_from_member(user_id(), guild_state()) -> boolean().
is_user_staff_from_member(UserId, State) ->
    case guild_permissions_check:find_member_by_user_id(UserId, State) of
        undefined ->
            false;
        Member ->
            User = maps:get(<<"user">>, Member, #{}),
            is_user_staff_from_user_data(User)
    end.

-spec is_user_staff_from_user_data(map()) -> boolean().
is_user_staff_from_user_data(UserData) when is_map(UserData) ->
    case parse_is_staff_value(maps:get(<<"is_staff">>, UserData, undefined)) of
        undefined -> is_user_staff_from_flags(UserData);
        IsStaff -> IsStaff
    end;
is_user_staff_from_user_data(_) ->
    false.

-spec parse_is_staff_value(term()) -> boolean() | undefined.
parse_is_staff_value(true) -> true;
parse_is_staff_value(false) -> false;
parse_is_staff_value(<<"true">>) -> true;
parse_is_staff_value(<<"false">>) -> false;
parse_is_staff_value(_) -> undefined.

-spec is_user_staff_from_flags(map()) -> boolean().
is_user_staff_from_flags(UserData) ->
    user_flags:is_staff(maps:get(<<"flags">>, UserData, 0)).

-spec check_unavailability_transition(guild_state(), guild_state()) -> transition_result().
check_unavailability_transition(OldState, NewState) ->
    OldMode = get_unavailability_mode_from_state(OldState),
    NewMode = get_unavailability_mode_from_state(NewState),
    transition_result(OldMode, NewMode).

-spec transition_result(unavailability_mode(), unavailability_mode()) -> transition_result().
transition_result(available, unavailable_for_everyone) ->
    {unavailable_enabled, false};
transition_result(available, unavailable_for_everyone_but_staff) ->
    {unavailable_enabled, true};
transition_result(unavailable_for_everyone, available) ->
    unavailable_disabled;
transition_result(unavailable_for_everyone_but_staff, available) ->
    unavailable_disabled;
transition_result(unavailable_for_everyone_but_staff, unavailable_for_everyone) ->
    {unavailable_enabled, false};
transition_result(unavailable_for_everyone, unavailable_for_everyone_but_staff) ->
    {unavailable_enabled, true};
transition_result(_, _) ->
    no_change.

-ifdef(TEST).

get_unavailability_mode_available_test() ->
    State = #{data => #{<<"guild">> => #{<<"features">> => []}}},
    ?assertEqual(available, get_unavailability_mode_from_state(State)).

get_unavailability_mode_for_everyone_test() ->
    State = #{data => #{<<"guild">> => #{<<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]}}},
    ?assertEqual(unavailable_for_everyone, get_unavailability_mode_from_state(State)).

get_unavailability_mode_for_everyone_but_staff_test() ->
    Features = [<<"UNAVAILABLE_FOR_EVERYONE_BUT_STAFF">>],
    State = #{data => #{<<"guild">> => #{<<"features">> => Features}}},
    ?assertEqual(
        unavailable_for_everyone_but_staff,
        get_unavailability_mode_from_state(State)
    ).

is_unavailable_hidden_enabled_test() ->
    HiddenFeatures = [<<"UNAVAILABLE_FOR_EVERYONE">>, <<"UNAVAILABLE_HIDDEN">>],
    StateHidden = #{data => #{<<"guild">> => #{<<"features">> => HiddenFeatures}}},
    NotHiddenFeatures = [<<"UNAVAILABLE_HIDDEN">>],
    StateNotHidden = #{data => #{<<"guild">> => #{<<"features">> => NotHiddenFeatures}}},
    ?assertEqual(true, is_unavailable_hidden_enabled(StateHidden)),
    ?assertEqual(false, is_unavailable_hidden_enabled(StateNotHidden)).

check_unavailability_transition_no_change_test() ->
    State = #{data => #{<<"guild">> => #{<<"features">> => []}}},
    ?assertEqual(no_change, check_unavailability_transition(State, State)).

check_unavailability_transition_enabled_test() ->
    Old = #{data => #{<<"guild">> => #{<<"features">> => []}}},
    New = #{data => #{<<"guild">> => #{<<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]}}},
    ?assertEqual({unavailable_enabled, false}, check_unavailability_transition(Old, New)).

check_unavailability_transition_disabled_test() ->
    Old = #{data => #{<<"guild">> => #{<<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]}}},
    New = #{data => #{<<"guild">> => #{<<"features">> => []}}},
    ?assertEqual(unavailable_disabled, check_unavailability_transition(Old, New)).

is_user_staff_from_user_data_test() ->
    ?assertEqual(true, is_user_staff_from_user_data(#{<<"is_staff">> => true})),
    ?assertEqual(false, is_user_staff_from_user_data(#{<<"is_staff">> => false})),
    ?assertEqual(true, is_user_staff_from_user_data(#{<<"flags">> => 1})),
    ?assertEqual(false, is_user_staff_from_user_data(#{<<"flags">> => 0})),
    ?assertEqual(false, is_user_staff_from_user_data(#{})).

parse_is_staff_value_test() ->
    ?assertEqual(true, parse_is_staff_value(true)),
    ?assertEqual(false, parse_is_staff_value(false)),
    ?assertEqual(true, parse_is_staff_value(<<"true">>)),
    ?assertEqual(false, parse_is_staff_value(<<"false">>)),
    ?assertEqual(undefined, parse_is_staff_value(something)).

is_user_staff_ignores_injected_member_data_test() ->
    UserId = 42,
    Sessions = #{
        <<"sess1">> => #{
            session_id => <<"sess1">>,
            user_id => UserId,
            is_staff => false
        }
    },
    InjectedMember = #{
        <<"user">> => #{<<"id">> => <<"42">>, <<"is_staff">> => true, <<"flags">> => <<"0">>}
    },
    State = #{
        sessions => Sessions,
        data => #{
            <<"members">> => [InjectedMember],
            <<"guild">> => #{<<"features">> => []}
        }
    },
    ?assertEqual(false, is_user_staff(UserId, State)).

is_user_staff_trusts_session_identity_test() ->
    UserId = 99,
    Sessions = #{
        <<"sess2">> => #{
            session_id => <<"sess2">>,
            user_id => UserId,
            is_staff => true
        }
    },
    State = #{
        sessions => Sessions,
        data => #{
            <<"members">> => [],
            <<"guild">> => #{<<"features">> => []}
        }
    },
    ?assertEqual(true, is_user_staff(UserId, State)).

-endif.
