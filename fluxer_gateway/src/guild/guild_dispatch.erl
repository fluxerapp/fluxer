%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_dispatch).
-typing([eqwalizer]).

-include_lib("kernel/include/logger.hrl").

-export([
    handle_dispatch/3,
    normalize_event/1,
    is_member_list_updates_enabled/1
]).

-type guild_state() :: map().
-type event() :: atom().
-type event_data() :: map().

-spec normalize_event(atom() | binary()) -> atom() | binary().
normalize_event(Event) when is_atom(Event) ->
    Event;
normalize_event(Event) when is_binary(Event) ->
    event_atoms:normalize(Event).

-spec handle_dispatch(atom() | binary(), map(), map()) -> {noreply, map()}.
handle_dispatch(Event, EventData, State) ->
    case should_skip_dispatch(Event, State) of
        true ->
            {noreply, State};
        false ->
            normalized_event_atom(normalize_event(Event), EventData, State)
    end.

-spec normalized_event_atom(atom() | binary(), event_data(), guild_state()) ->
    {noreply, guild_state()}.
normalized_event_atom(Event, EventData, State) when is_atom(Event) ->
    process_dispatch(Event, EventData, State);
normalized_event_atom(Event, _EventData, State) when is_binary(Event) ->
    ?LOG_WARNING("guild dispatch dropped unknown event: event=~ts guild_id=~p", [
        Event, maps:get(id, State, undefined)
    ]),
    {noreply, State}.

-spec should_skip_dispatch(term(), guild_state()) -> boolean().
should_skip_dispatch(guild_update, _State) ->
    false;
should_skip_dispatch(_Event, State) ->
    Data = maps:get(data, State),
    Guild = maps:get(<<"guild">>, Data, #{}),
    Features = maps:get(<<"features">>, Guild, []),
    lists:member(<<"UNAVAILABLE_FOR_EVERYONE">>, Features) orelse
        lists:member(<<"UNAVAILABLE_FOR_EVERYONE_BUT_STAFF">>, Features).

-spec process_dispatch(event(), event_data(), guild_state()) -> {noreply, guild_state()}.
process_dispatch(Event, EventData, State) ->
    case
        {
            guild_thread_gate:pre_update_filter(Event, EventData, State),
            guild_thread_dispatch:handles(Event)
        }
    of
        {drop, _HandlesThread} ->
            ok = maybe_self_heal(State),
            {noreply, guild_thread_flip:hold(Event, EventData, State)};
        {{filtered, FilteredData}, _HandlesThread} ->
            ok = maybe_self_heal(State),
            process_regular_dispatch(
                Event, FilteredData, guild_thread_flip:hold(Event, EventData, State)
            );
        {pass, true} ->
            State1 = guild_thread_flip:hold(Event, EventData, State),
            guild_thread_dispatch:handle(Event, EventData, State1);
        {pass, false} ->
            State1 = guild_thread_flip:hold(Event, EventData, State),
            process_regular_dispatch(Event, EventData, State1)
    end.

-spec maybe_self_heal(guild_state()) -> ok.
maybe_self_heal(#{id := GuildId}) when is_integer(GuildId) ->
    case channel_threads_config:guild_active(GuildId) of
        true -> channel_threads_flip:self_heal(GuildId);
        false -> ok
    end;
maybe_self_heal(_State) ->
    ok.

-spec process_regular_dispatch(event(), event_data(), guild_state()) ->
    {noreply, guild_state()}.
process_regular_dispatch(Event, EventData, State) ->
    GuildId = maps:get(id, State),
    {SessionIdOpt, CleanData} = extract_session_id_if_needed(Event, EventData),
    DecoratedData = CleanData#{<<"guild_id">> => integer_to_binary(GuildId)},
    FinalData0 = guild_dispatch_decorate:decorate_member_data(Event, DecoratedData, State),
    UpdatedState0 = guild_state:update_state(Event, FinalData0, State),
    UpdatedState = guild_state_threads:after_regular_event(Event, FinalData0, UpdatedState0),
    FilterState = filter_state_for_event(Event, State, UpdatedState),
    Sessions = maps:get(sessions, UpdatedState),
    FilteredSessions = filter_dispatch_sessions(
        Event, FinalData0, SessionIdOpt, Sessions, FilterState
    ),
    FinalData = strip_thread_internal(FinalData0, UpdatedState),
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

-spec filter_dispatch_sessions(
    event(), event_data(), binary() | undefined, map(), guild_state()
) -> [{binary(), map()}].
filter_dispatch_sessions(Event, Data, SessionIdOpt, Sessions, State) ->
    case guild_thread_gate:event_scope(Event, Data, State) of
        open ->
            guild_dispatch_filter:filter_sessions_for_event(
                Event, Data, SessionIdOpt, Sessions, State
            );
        viewers ->
            guild_dispatch_filter:filter_sessions_for_event(
                Event, Data, SessionIdOpt, guild_thread_gate:viewer_sessions(Sessions), State
            );
        {thread, ThreadId, Thread} ->
            guild_thread_gate:thread_sessions(
                Event, Data, ThreadId, Thread, SessionIdOpt, State
            )
    end.

-spec strip_thread_internal(event_data(), guild_state()) -> event_data().
strip_thread_internal(Data, State) ->
    case guild_thread_gate:needs_variant(State) of
        true -> guild_thread_payload:strip_internal(Data);
        false -> Data
    end.

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

-spec is_member_list_updates_enabled(map()) -> boolean().
is_member_list_updates_enabled(State) ->
    guild_dispatch_config:is_member_list_updates_enabled(State).
