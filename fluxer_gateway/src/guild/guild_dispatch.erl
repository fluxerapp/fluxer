%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_dispatch).
-typing([eqwalizer]).

-include_lib("kernel/include/logger.hrl").

-export([
    handle_dispatch/3,
    extract_and_remove_session_id/1,
    decorate_member_data/3,
    extract_member_for_event/3,
    collect_and_send_push_notifications/3,
    normalize_event/1,
    is_member_list_updates_enabled/1
]).
-export_type([event/0, event_data/0, guild_state/0]).

-type guild_state() :: map().
-type event() :: atom().
-type event_data() :: map().

-spec normalize_event(term()) -> event().
normalize_event(Event) when is_atom(Event) ->
    Event;
normalize_event(Event) when is_binary(Event) ->
    normalized_event_atom(event_atoms:normalize(Event)).

-spec normalized_event_atom(atom() | binary()) -> event().
normalized_event_atom(Event) when is_atom(Event) ->
    Event.

-spec handle_dispatch(term(), event_data(), guild_state()) -> {noreply, guild_state()}.
handle_dispatch(Event, EventData, State) ->
    case should_skip_dispatch(Event, State) of
        true ->
            {noreply, State};
        false ->
            NormalizedEvent = normalize_event(Event),
            process_dispatch(NormalizedEvent, EventData, State)
    end.

-spec should_skip_dispatch(term(), guild_state()) -> boolean().
should_skip_dispatch(guild_update, _State) ->
    false;
should_skip_dispatch(_Event, State) ->
    Data = maps:get(data, State, #{}),
    Guild = maps:get(<<"guild">>, Data, #{}),
    Features = maps:get(<<"features">>, Guild, []),
    lists:member(<<"UNAVAILABLE_FOR_EVERYONE">>, Features) orelse
        lists:member(<<"UNAVAILABLE_FOR_EVERYONE_BUT_STAFF">>, Features).

-spec process_dispatch(event(), event_data(), guild_state()) -> {noreply, guild_state()}.
process_dispatch(Event, EventData, State) ->
    GuildId = maps:get(id, State),
    {SessionIdOpt, CleanData} = extract_session_id_if_needed(Event, EventData),
    DecoratedData = CleanData#{<<"guild_id">> => integer_to_binary(GuildId)},
    FinalData = guild_dispatch_decorate:decorate_member_data(Event, DecoratedData, State),
    UpdatedState = guild_state:update_state(Event, FinalData, State),
    FilterState = filter_state_for_event(Event, State, UpdatedState),
    Sessions = maps:get(sessions, UpdatedState, #{}),
    FilteredSessions = guild_dispatch_filter:filter_sessions_for_event(
        Event, FinalData, SessionIdOpt, Sessions, FilterState
    ),
    ?LOG_DEBUG(
        "process_dispatch: event=~p guild_id=~p total_sessions=~p filtered_sessions=~p",
        [Event, GuildId, map_size(Sessions), length(FilteredSessions)]
    ),
    guild_dispatch_send:dispatch_to_sessions(FilteredSessions, Event, FinalData, UpdatedState),
    guild_dispatch_push:maybe_send_push_notifications(Event, FinalData, GuildId, UpdatedState),
    FinalState = guild_dispatch_member_list:maybe_broadcast_member_list_update(
        Event, FinalData, State, UpdatedState
    ),
    {noreply, FinalState}.

-spec filter_state_for_event(event(), guild_state(), guild_state()) -> guild_state().
filter_state_for_event(channel_delete, PreviousState, _UpdatedState) ->
    PreviousState;
filter_state_for_event(_Event, _PreviousState, UpdatedState) ->
    UpdatedState.

-spec extract_session_id_if_needed(event(), event_data()) ->
    {binary() | undefined, event_data()}.
extract_session_id_if_needed(message_reaction_add, EventData) ->
    guild_dispatch_decorate:extract_and_remove_session_id(EventData);
extract_session_id_if_needed(message_reaction_remove, EventData) ->
    guild_dispatch_decorate:extract_and_remove_session_id(EventData);
extract_session_id_if_needed(_, EventData) ->
    {undefined, EventData}.

-spec extract_and_remove_session_id(event_data()) -> {binary() | undefined, event_data()}.
extract_and_remove_session_id(Data) ->
    guild_dispatch_decorate:extract_and_remove_session_id(Data).

-spec decorate_member_data(event(), event_data(), guild_state()) -> event_data().
decorate_member_data(Event, Data, State) ->
    guild_dispatch_decorate:decorate_member_data(Event, Data, State).

-spec extract_member_for_event(event(), event_data(), guild_state()) -> map() | undefined.
extract_member_for_event(Event, Data, State) ->
    guild_dispatch_decorate:extract_member_for_event(Event, Data, State).

-spec collect_and_send_push_notifications(event_data(), integer(), guild_state()) -> ok.
collect_and_send_push_notifications(MessageData, GuildId, State) ->
    guild_dispatch_push:collect_and_send_push_notifications(MessageData, GuildId, State).

-spec is_member_list_updates_enabled(guild_state()) -> boolean().
is_member_list_updates_enabled(State) ->
    guild_dispatch_config:is_member_list_updates_enabled(State).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

extract_session_id_if_needed_reaction_test() ->
    Data = #{<<"session_id">> => <<"sid">>, <<"emoji">> => #{}},
    {SessionId, CleanData} = extract_session_id_if_needed(message_reaction_add, Data),
    ?assertEqual(<<"sid">>, SessionId),
    ?assertNot(maps:is_key(<<"session_id">>, CleanData)).

extract_session_id_if_needed_other_test() ->
    Data = #{<<"session_id">> => <<"sid">>, <<"content">> => <<"hi">>},
    {SessionId, CleanData} = extract_session_id_if_needed(message_create, Data),
    ?assertEqual(undefined, SessionId),
    ?assertEqual(Data, CleanData).

extract_session_id_if_needed_reaction_remove_test() ->
    Data = #{<<"session_id">> => <<"sid">>, <<"emoji">> => #{}},
    {SessionId, CleanData} = extract_session_id_if_needed(message_reaction_remove, Data),
    ?assertEqual(<<"sid">>, SessionId),
    ?assertNot(maps:is_key(<<"session_id">>, CleanData)).

should_skip_dispatch_guild_update_never_skipped_test() ->
    State = #{
        data => #{
            <<"guild">> => #{<<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]}
        }
    },
    ?assertEqual(false, should_skip_dispatch(guild_update, State)).

should_skip_dispatch_unavailable_for_everyone_test() ->
    State = #{
        data => #{
            <<"guild">> => #{<<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE">>]}
        }
    },
    ?assertEqual(true, should_skip_dispatch(message_create, State)).

should_skip_dispatch_unavailable_for_everyone_but_staff_test() ->
    State = #{
        data => #{
            <<"guild">> => #{<<"features">> => [<<"UNAVAILABLE_FOR_EVERYONE_BUT_STAFF">>]}
        }
    },
    ?assertEqual(true, should_skip_dispatch(message_create, State)).

should_skip_dispatch_normal_guild_test() ->
    State = #{
        data => #{
            <<"guild">> => #{<<"features">> => []}
        }
    },
    ?assertEqual(false, should_skip_dispatch(message_create, State)).

should_skip_dispatch_no_features_test() ->
    State = #{data => #{<<"guild">> => #{}}},
    ?assertEqual(false, should_skip_dispatch(message_create, State)).

channel_delete_dispatch_filters_by_pre_delete_visibility_test() ->
    Parent = self(),
    VisiblePid = start_dispatch_capture(visible, Parent),
    HiddenPid = start_dispatch_capture(hidden, Parent),
    try
        State = build_channel_delete_dispatch_state(VisiblePid, HiddenPid),
        HiddenMember = guild_permissions:find_member_by_user_id(1001, State),
        VisibleMember = guild_permissions:find_member_by_user_id(1002, State),
        ?assertEqual(false, guild_permissions:can_view_channel(1001, 10, HiddenMember, State)),
        ?assertEqual(true, guild_permissions:can_view_channel(1002, 10, VisibleMember, State)),
        {noreply, _} = handle_dispatch(channel_delete, #{<<"id">> => <<"10">>}, State),
        assert_visible_channel_delete_dispatched(),
        assert_hidden_channel_delete_not_dispatched()
    after
        VisiblePid ! stop,
        HiddenPid ! stop
    end.

-spec start_dispatch_capture(atom(), pid()) -> pid().
start_dispatch_capture(Tag, Parent) ->
    spawn(fun() -> dispatch_capture_loop(Tag, Parent) end).

-spec dispatch_capture_loop(atom(), pid()) -> ok.
dispatch_capture_loop(Tag, Parent) ->
    receive
        stop ->
            ok;
        {'$gen_cast', Msg} ->
            Parent ! {Tag, Msg},
            dispatch_capture_loop(Tag, Parent);
        _Other ->
            dispatch_capture_loop(Tag, Parent)
    after 5000 ->
        dispatch_capture_loop(Tag, Parent)
    end.

-spec assert_visible_channel_delete_dispatched() -> ok.
assert_visible_channel_delete_dispatched() ->
    receive
        {visible, {dispatch, channel_delete, Payload0}} ->
            Payload = decode_dispatch_payload(Payload0),
            ?assertEqual(<<"10">>, maps:get(<<"id">>, Payload));
        {visible, Other} ->
            ?assert(false, {unexpected_visible_message, Other})
    after 1000 ->
        ?assert(false, visible_channel_delete_not_dispatched)
    end.

-spec assert_hidden_channel_delete_not_dispatched() -> ok.
assert_hidden_channel_delete_not_dispatched() ->
    receive
        {hidden, {dispatch, channel_delete, _Payload}} ->
            ?assert(false, hidden_user_received_channel_delete)
    after 200 ->
        ok
    end.

-spec decode_dispatch_payload(term()) -> map().
decode_dispatch_payload({pre_encoded, Bin}) when is_binary(Bin) ->
    decoded_dispatch_payload(json:decode(Bin));
decode_dispatch_payload(Map) when is_map(Map) ->
    Map.

-spec decoded_dispatch_payload(term()) -> map().
decoded_dispatch_payload(Payload) when is_map(Payload) ->
    Payload.

-spec build_channel_delete_dispatch_state(pid(), pid()) -> guild_state().
build_channel_delete_dispatch_state(VisiblePid, HiddenPid) ->
    GuildId = 1,
    GuildIdBin = integer_to_binary(GuildId),
    ViewPermissionBin = integer_to_binary(constants:view_channel_permission()),
    #{
        id => GuildId,
        member_count => 100,
        sessions => build_test_sessions(GuildId, VisiblePid, HiddenPid),
        data => build_test_data(GuildIdBin, ViewPermissionBin),
        member_list_subscriptions => guild_member_list_subs:new()
    }.

-spec build_test_sessions(integer(), pid(), pid()) -> map().
build_test_sessions(GuildId, VisiblePid, HiddenPid) ->
    #{
        <<"visible">> => #{
            user_id => 1002,
            pid => VisiblePid,
            active_guilds => sets:from_list([GuildId])
        },
        <<"hidden">> => #{
            user_id => 1001,
            pid => HiddenPid,
            active_guilds => sets:from_list([GuildId])
        }
    }.

-spec build_test_data(binary(), binary()) -> map().
build_test_data(GuildIdBin, ViewPermissionBin) ->
    #{
        <<"guild">> => #{
            <<"id">> => GuildIdBin,
            <<"owner_id">> => <<"999">>,
            <<"features">> => []
        },
        <<"roles">> => [
            #{<<"id">> => GuildIdBin, <<"permissions">> => ViewPermissionBin},
            #{<<"id">> => <<"200">>, <<"permissions">> => <<"0">>}
        ],
        <<"members">> => [
            #{<<"user">> => #{<<"id">> => <<"1001">>}, <<"roles">> => [<<"200">>]},
            #{<<"user">> => #{<<"id">> => <<"1002">>}, <<"roles">> => []}
        ],
        <<"channels">> => [
            #{
                <<"id">> => <<"10">>,
                <<"permission_overwrites">> => [
                    #{
                        <<"id">> => <<"200">>,
                        <<"type">> => 0,
                        <<"allow">> => <<"0">>,
                        <<"deny">> => ViewPermissionBin
                    }
                ]
            }
        ]
    }.

-endif.
