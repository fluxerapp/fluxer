%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_dispatch_send).
-typing([eqwalizer]).

-include_lib("kernel/include/logger.hrl").

-export([dispatch_to_sessions/4]).

-define(MAX_BULK_ENCODE_GROUPS, 1024).
-define(BROADCASTER_CLAIM_DEADLINE_MS, 250).
-define(BROADCASTER_CLAIM_BACKOFF_MS, 1).

-type event() :: atom().
-type event_data() :: map().
-type guild_state() :: map().
-type session_id() :: binary().
-type guild_id() :: integer().
-type session_pair() :: {session_id(), map()}.
-type bulk_group_key() :: [term()].
-type bulk_groups() :: #{bulk_group_key() => {[map()], [pid()]}}.
-type bulk_acc() :: {bulk_groups(), non_neg_integer()}.

-spec dispatch_to_sessions([{binary(), map()}], atom(), map(), map()) ->
    non_neg_integer().
dispatch_to_sessions(FilteredSessions, Event, FinalData, UpdatedState) ->
    case guild_thread_gate:needs_variant(UpdatedState) of
        false ->
            dispatch_variant(FilteredSessions, Event, FinalData, UpdatedState);
        true ->
            dispatch_masked_variants(FilteredSessions, Event, FinalData, UpdatedState)
    end.

-spec dispatch_masked_variants([session_pair()], event(), event_data(), guild_state()) ->
    non_neg_integer().
dispatch_masked_variants(FilteredSessions, Event, FinalData, UpdatedState) ->
    Masked = guild_thread_payload:mask_payload(Event, FinalData),
    {Viewers, Others} = lists:partition(
        fun({_Sid, Session}) -> guild_thread_gate:session_viewer(Session) end, FilteredSessions
    ),
    case {Masked =:= FinalData, Viewers, Others} of
        {true, _, _} ->
            dispatch_variant(FilteredSessions, Event, FinalData, UpdatedState);
        {false, _, []} ->
            dispatch_variant(Viewers, Event, FinalData, UpdatedState);
        {false, [], _} ->
            dispatch_masked(Others, Event, Masked, UpdatedState);
        {false, _, _} ->
            max(
                dispatch_variant(Viewers, Event, FinalData, UpdatedState),
                dispatch_masked(Others, Event, Masked, UpdatedState)
            )
    end.

-spec dispatch_masked([session_pair()], event(), event_data(), guild_state()) ->
    non_neg_integer().
dispatch_masked(Sessions, Event, Masked, UpdatedState) ->
    Count = dispatch_variant(Sessions, Event, Masked, UpdatedState),
    ok = guild_thread_dispatch:count_masked(Event, Count),
    Count.

-spec dispatch_variant([session_pair()], event(), event_data(), guild_state()) ->
    non_neg_integer().
dispatch_variant(FilteredSessions, Event, FinalData, UpdatedState) ->
    GuildId = maps:get(id, UpdatedState),
    case guild_dispatch_filter:is_bulk_update_event(Event) of
        true ->
            dispatch_bulk_update(FilteredSessions, Event, FinalData, UpdatedState);
        false ->
            dispatch_standard(FilteredSessions, Event, FinalData, GuildId, UpdatedState)
    end.

-spec dispatch_bulk_update([session_pair()], event(), event_data(), guild_state()) ->
    non_neg_integer().
dispatch_bulk_update(FilteredSessions, Event, FinalData, UpdatedState) ->
    GuildId = maps:get(id, UpdatedState),
    BulkChannels = maps:get(<<"channels">>, FinalData, []),
    IndexedChannels = [
        {
            guild_dispatch_decorate:parse_snowflake(
                <<"id">>,
                maps:get(<<"id">>, Ch, undefined)
            ),
            Ch
        }
     || Ch <- BulkChannels
    ],
    {Groups, Dispatched} = lists:foldl(
        fun({_Sid, SessionData}, Acc) ->
            collect_bulk_recipient(
                SessionData, Event, FinalData, IndexedChannels, GuildId, UpdatedState, Acc
            )
        end,
        {#{}, 0},
        FilteredSessions
    ),
    SuccessCount = dispatch_bulk_groups(Groups, Event, FinalData, GuildId, Dispatched),
    normalize_success(SuccessCount).

-spec collect_bulk_recipient(
    map(),
    event(),
    event_data(),
    [{integer() | undefined, map()}],
    guild_id(),
    guild_state(),
    bulk_acc()
) -> bulk_acc().
collect_bulk_recipient(SessionData, Event, FinalData, IndexedChannels, GuildId, State, Acc) ->
    Pid = maps:get(pid, SessionData),
    Eligible =
        is_pid(Pid) andalso
            session_passive:should_receive_event(Event, FinalData, GuildId, SessionData, State),
    case Eligible of
        false ->
            Acc;
        true ->
            Filtered = filter_indexed_for_session(SessionData, IndexedChannels, State),
            add_bulk_recipient(Pid, Filtered, Event, FinalData, GuildId, Acc)
    end.

-spec add_bulk_recipient(pid(), [map()], event(), event_data(), guild_id(), bulk_acc()) ->
    bulk_acc().
add_bulk_recipient(_Pid, [], _Event, _FinalData, _GuildId, Acc) ->
    Acc;
add_bulk_recipient(Pid, FilteredChannels, Event, FinalData, GuildId, {Groups, Dispatched}) ->
    Key = bulk_group_key(FilteredChannels),
    case Groups of
        #{Key := {Channels, Pids}} ->
            {Groups#{Key := {Channels, [Pid | Pids]}}, Dispatched};
        _ when map_size(Groups) >= ?MAX_BULK_ENCODE_GROUPS ->
            Sent = dispatch_bulk_to_pid(
                Pid, Event, FinalData, FilteredChannels, GuildId, Dispatched
            ),
            {Groups, Sent};
        _ ->
            {Groups#{Key => {FilteredChannels, [Pid]}}, Dispatched}
    end.

-spec bulk_group_key([map()]) -> bulk_group_key().
bulk_group_key(FilteredChannels) ->
    [maps:get(<<"id">>, Ch, undefined) || Ch <- FilteredChannels].

-spec dispatch_bulk_groups(
    bulk_groups(), event(), event_data(), guild_id(), non_neg_integer()
) -> non_neg_integer().
dispatch_bulk_groups(Groups, Event, FinalData, GuildId, Dispatched) ->
    maps:fold(
        fun(_Key, {FilteredChannels, Pids}, Acc) ->
            dispatch_bulk_to_pids(
                lists:reverse(Pids), Event, FinalData, FilteredChannels, GuildId, Acc
            )
        end,
        Dispatched,
        Groups
    ).

-spec filter_indexed_for_session(map(), [{integer() | undefined, map()}], guild_state()) ->
    [map()].
filter_indexed_for_session(SessionData, IndexedChannels, UpdatedState) ->
    Visible = filter_permitted_for_session(SessionData, IndexedChannels, UpdatedState),
    case guild_thread_gate:needs_variant(UpdatedState) of
        false ->
            Visible;
        true ->
            [
                Ch
             || Ch <- Visible,
                guild_thread_gate:channel_visible(
                    SessionData,
                    guild_dispatch_decorate:parse_snowflake(
                        <<"id">>, maps:get(<<"id">>, Ch, undefined)
                    ),
                    UpdatedState
                )
            ]
    end.

-spec filter_permitted_for_session(map(), [{integer() | undefined, map()}], guild_state()) ->
    [map()].
filter_permitted_for_session(SessionData, IndexedChannels, UpdatedState) ->
    case maps:get(viewable_channels, SessionData, undefined) of
        ViewableMap when is_map(ViewableMap) ->
            [
                Ch
             || {ChId, Ch} <- IndexedChannels,
                is_integer(ChId),
                maps:is_key(ChId, ViewableMap)
            ];
        _ ->
            UserId = maps:get(user_id, SessionData),
            Member = guild_permissions_check:find_member_by_user_id(UserId, UpdatedState),
            [
                Ch
             || {ChId, Ch} <- IndexedChannels,
                is_integer(ChId),
                guild_permissions_check:can_view_channel(UserId, ChId, Member, UpdatedState)
            ]
    end.

-spec dispatch_bulk_to_pid(
    pid(), event(), event_data(), [map()], guild_id(), non_neg_integer()
) -> non_neg_integer().
dispatch_bulk_to_pid(Pid, Event, FinalData, FilteredChannels, GuildId, Acc) when is_pid(Pid) ->
    EncodedData = encode_bulk_payload(FinalData, FilteredChannels),
    try
        gateway_dispatch_relay:dispatch(Pid, Event, EncodedData, GuildId),
        Acc + 1
    catch
        _:_ -> Acc
    end;
dispatch_bulk_to_pid(_, _, _, _, _GuildId, Acc) ->
    Acc.

-spec dispatch_bulk_to_pids(
    [pid()], event(), event_data(), [map()], guild_id(), non_neg_integer()
) -> non_neg_integer().
dispatch_bulk_to_pids([], _Event, _FinalData, _FilteredChannels, _GuildId, Acc) ->
    Acc;
dispatch_bulk_to_pids(Pids, Event, FinalData, FilteredChannels, GuildId, Acc) ->
    EncodedData = encode_bulk_payload(FinalData, FilteredChannels),
    try
        gateway_dispatch_relay:dispatch_many(Pids, Event, EncodedData, GuildId),
        Acc + length(Pids)
    catch
        _:_ -> Acc
    end.

-spec encode_bulk_payload(event_data(), [map()]) -> {pre_encoded, binary()}.
encode_bulk_payload(FinalData, FilteredChannels) ->
    CustomData = FinalData#{<<"channels">> => FilteredChannels},
    {pre_encoded,
        iolist_to_binary(
            json:encode(guild_data_wire:payload(CustomData), fun json:encode_value/2)
        )}.

-spec dispatch_standard([session_pair()], event(), event_data(), guild_id(), guild_state()) ->
    non_neg_integer().
dispatch_standard(FilteredSessions, Event, FinalData, GuildId, State) ->
    ?LOG_DEBUG(
        "dispatch_standard: event=~p guild_id=~p filtered_sessions=~p member_count=~p",
        [Event, GuildId, length(FilteredSessions), maps:get(member_count, State, undefined)]
    ),
    EncodedData =
        {pre_encoded,
            iolist_to_binary(
                json:encode(guild_data_wire:payload(FinalData), fun json:encode_value/2)
            )},
    Pids = collect_eligible_pids(FilteredSessions, Event, FinalData, GuildId, State),
    dispatch_to_pids(Pids, Event, EncodedData, GuildId, State),
    normalize_dispatched(Pids).

-spec collect_eligible_pids(
    [session_pair()], event(), event_data(), guild_id(), guild_state()
) -> [pid()].
collect_eligible_pids(FilteredSessions, Event, FinalData, GuildId, State) ->
    lists:filtermap(
        fun({_Sid, SessionData}) ->
            check_eligible_pid(SessionData, Event, FinalData, GuildId, State)
        end,
        FilteredSessions
    ).

-spec check_eligible_pid(map(), event(), event_data(), guild_id(), guild_state()) ->
    {true, pid()} | false.
check_eligible_pid(SessionData, Event, FinalData, GuildId, State) ->
    Pid = maps:get(pid, SessionData),
    Eligible =
        is_pid(Pid) andalso
            session_passive:should_receive_event(Event, FinalData, GuildId, SessionData, State),
    case Eligible of
        true ->
            {true, Pid};
        false ->
            false
    end.

-spec dispatch_to_pids([pid()], event(), term(), guild_id(), guild_state()) -> ok.
dispatch_to_pids([], _Event, _EncodedData, _GuildId, _State) ->
    ok;
dispatch_to_pids(Pids, Event, EncodedData, GuildId, State) ->
    case maps:get(broadcaster_pid, State, undefined) of
        BroadcasterPid when is_pid(BroadcasterPid) ->
            claim_broadcaster(BroadcasterPid, Pids, Event, EncodedData, claim_deadline());
        _ ->
            gateway_dispatch_relay:dispatch_many(Pids, Event, EncodedData, GuildId)
    end.

-spec claim_broadcaster(pid(), [pid()], event(), term(), integer()) -> ok.
claim_broadcaster(BroadcasterPid, Pids, Event, EncodedData, Deadline) ->
    case guild_broadcaster:cast_event(BroadcasterPid, Event, EncodedData, Pids) of
        true ->
            ok;
        false ->
            claim_broadcaster_after_wait(
                BroadcasterPid,
                Pids,
                Event,
                EncodedData,
                Deadline,
                gateway_retry_timer:wait_until(?BROADCASTER_CLAIM_BACKOFF_MS, Deadline)
            )
    end.

-spec claim_broadcaster_after_wait(
    pid(), [pid()], event(), term(), integer(), term()
) -> ok.
claim_broadcaster_after_wait(BroadcasterPid, Pids, Event, EncodedData, Deadline, ok) ->
    claim_broadcaster(BroadcasterPid, Pids, Event, EncodedData, Deadline);
claim_broadcaster_after_wait(BroadcasterPid, Pids, Event, EncodedData, _Deadline, _Expired) ->
    gen_server:cast(BroadcasterPid, {event_broadcast, Event, EncodedData, Pids}).

-spec claim_deadline() -> integer().
claim_deadline() ->
    erlang:monotonic_time(millisecond) + ?BROADCASTER_CLAIM_DEADLINE_MS.

-spec normalize_success(non_neg_integer()) -> non_neg_integer().
normalize_success(Count) when Count > 0 -> 1;
normalize_success(_) -> 0.

-spec normalize_dispatched([pid()]) -> non_neg_integer().
normalize_dispatched([]) -> 0;
normalize_dispatched([_ | _]) -> 1.
