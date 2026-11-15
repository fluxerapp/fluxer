%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_sync_batch).
-typing([eqwalizer]).

-export([
    queue_list_sync/2,
    queue_subscribed_list_syncs/2,
    flush_pending_syncs/1,
    queue_connection_sync/3,
    invalidate_synced/2
]).

-type guild_state() :: map().
-type list_id() :: binary().
-type channel_id() :: integer().
-type user_id() :: integer().
-type pending_mark() :: true | synced.

-export_type([guild_state/0, list_id/0, pending_mark/0, user_id/0]).

-define(SYNC_BATCH_STATE_KEY, pending_member_list_sync_batch).
-define(FLUSH_SYNC_BATCH_MSG, flush_member_list_sync_batch).

-spec queue_list_sync(list_id(), guild_state()) -> guild_state().
queue_list_sync(ListId, State) ->
    queue_list_syncs([ListId], State).

-spec queue_subscribed_list_syncs(guild_state(), ets:table()) -> guild_state().
queue_subscribed_list_syncs(State, SubsTab) ->
    queue_existing_list_syncs(guild_member_list_subs:list_ids(SubsTab), State).

-spec flush_pending_syncs(guild_state()) -> guild_state().
flush_pending_syncs(State) ->
    {ListIds, State1} = take_pending_list_ids([], State),
    dispatch_pending_syncs(ListIds, State1).

-spec queue_list_syncs([list_id()], guild_state()) -> guild_state().
queue_list_syncs(ListIds, State) ->
    ValidListIds = lists:usort([ListId || ListId <- ListIds, is_binary(ListId)]),
    case ValidListIds of
        [] ->
            State;
        _ ->
            queue_existing_list_syncs(ValidListIds, State)
    end.

-spec queue_existing_list_syncs([list_id()], guild_state()) -> guild_state().
queue_existing_list_syncs([], State) ->
    State;
queue_existing_list_syncs(ListIds, State) ->
    {DispatchListIds, State1} = take_pending_list_ids(ListIds, State),
    dispatch_pending_syncs(DispatchListIds, State1).

-spec take_pending_list_ids([list_id()], guild_state()) -> {[list_id()], guild_state()}.
take_pending_list_ids(ListIds, State) ->
    case maps:get(?SYNC_BATCH_STATE_KEY, State, undefined) of
        #{pending_list_ids := PendingListIds} = Batch when is_map(PendingListIds) ->
            cancel_timer(Batch),
            {
                normalize_list_ids(ListIds ++ unsynced_list_ids(PendingListIds)),
                maps:remove(?SYNC_BATCH_STATE_KEY, State)
            };
        _ ->
            {normalize_list_ids(ListIds), State}
    end.

-spec unsynced_list_ids(map()) -> [term()].
unsynced_list_ids(PendingListIds) ->
    maps:keys(maps:filter(fun(_ListId, Mark) -> Mark =/= synced end, PendingListIds)).

-spec normalize_list_ids([term()]) -> [list_id()].
normalize_list_ids(ListIds) ->
    lists:usort([ListId || ListId <- ListIds, is_binary(ListId)]).

-spec cancel_timer(map()) -> ok.
cancel_timer(Batch) ->
    case maps:get(timer_ref, Batch, undefined) of
        TimerRef when is_reference(TimerRef) ->
            _ = erlang:cancel_timer(TimerRef, [{async, true}, {info, false}]),
            ok;
        _ ->
            ok
    end.

-spec dispatch_pending_syncs([list_id()], guild_state()) -> guild_state().
dispatch_pending_syncs([], State) ->
    State;
dispatch_pending_syncs(ListIds, State) ->
    case maps:get(member_list_subscriptions, State, undefined) of
        undefined ->
            State;
        SubsTab ->
            dispatch_pending_syncs(ListIds, State, SubsTab)
    end.

-spec dispatch_pending_syncs([list_id()], guild_state(), ets:table()) -> guild_state().
dispatch_pending_syncs(ListIds, State, SubsTab) ->
    _ = guild_member_list_write_context:with_guild_id(State, fun(GuildId) ->
        dispatch_memoized_syncs(GuildId, ListIds, State, SubsTab),
        {ok, State}
    end),
    State.

-spec dispatch_memoized_syncs(integer(), [list_id()], guild_state(), ets:table()) -> ok.
dispatch_memoized_syncs(GuildId, ListIds, State, SubsTab) ->
    Sessions = maps:get(sessions, State, #{}),
    guild_member_list_read:with_member_item_memo(fun() ->
        dispatch_pending_syncs_for_guild(GuildId, lists:sort(ListIds), Sessions, State, SubsTab)
    end).

-spec dispatch_pending_syncs_for_guild(
    integer(), [list_id()], map(), guild_state(), ets:table()
) -> ok.
dispatch_pending_syncs_for_guild(GuildId, ListIds, Sessions, State, SubsTab) ->
    lists:foreach(
        fun(ListId) ->
            dispatch_pending_list_sync(GuildId, ListId, Sessions, State, SubsTab)
        end,
        ListIds
    ).

-spec dispatch_pending_list_sync(integer(), list_id(), map(), guild_state(), ets:table()) ->
    ok.
dispatch_pending_list_sync(GuildId, ListId, Sessions, State, SubsTab) ->
    dispatch_list_sync(GuildId, ListId, SubsTab, Sessions, State).

-spec dispatch_list_sync(
    integer(), list_id(), ets:table(), map(), guild_state()
) -> ok.
dispatch_list_sync(GuildId, ListId, SubsTab, Sessions, State) ->
    case guild_member_list_write_context:list_dispatch_channel_id(ListId) of
        error ->
            ok;
        {ok, ChannelId} ->
            SyncFun = build_sync_response_fun(GuildId, ListId, ChannelId, State),
            guild_member_list_subscribe:dispatch_sync_to_subscribed_list(
                ListId,
                SubsTab,
                Sessions,
                ChannelId,
                GuildId,
                State,
                SyncFun
            )
    end.

-spec build_sync_response_fun(integer(), list_id(), channel_id() | undefined, guild_state()) ->
    fun(([guild_member_list_subs:range()]) -> map()).
build_sync_response_fun(GuildId, ListId, undefined, State) ->
    guild_member_list_read:build_normalized_sync_response_builder(GuildId, ListId, State);
build_sync_response_fun(GuildId, ListId, ChannelId, State) ->
    ChannelIdBin = integer_to_binary(ChannelId),
    SyncBuilder = guild_member_list_read:build_normalized_sync_response_builder(
        GuildId, ListId, State
    ),
    fun(Ranges) ->
        Response = SyncBuilder(Ranges),
        Response#{<<"channel_id">> => ChannelIdBin}
    end.

-spec queue_connection_sync(list_id(), pending_mark(), guild_state()) -> guild_state().
queue_connection_sync(ListId, Mark, State) ->
    case maps:get(?SYNC_BATCH_STATE_KEY, State, undefined) of
        #{pending_list_ids := PendingListIds} = Batch when is_map(PendingListIds) ->
            State#{
                ?SYNC_BATCH_STATE_KEY => Batch#{
                    pending_list_ids => maps:update_with(
                        ListId,
                        fun(Old) -> merge_pending_mark(Old, Mark) end,
                        Mark,
                        PendingListIds
                    )
                }
            };
        _ ->
            TimerRef = erlang:send_after(
                connection_sync_delay_ms(State), self(), ?FLUSH_SYNC_BATCH_MSG
            ),
            State#{
                ?SYNC_BATCH_STATE_KEY => #{
                    pending_list_ids => #{ListId => Mark},
                    timer_ref => TimerRef
                }
            }
    end.

-spec merge_pending_mark(term(), pending_mark()) -> pending_mark().
merge_pending_mark(true, _Mark) ->
    true;
merge_pending_mark(_Old, Mark) ->
    Mark.

-spec invalidate_synced(user_id(), guild_state()) -> guild_state().
invalidate_synced(UserId, State) ->
    case maps:get(?SYNC_BATCH_STATE_KEY, State, undefined) of
        #{pending_list_ids := PendingListIds} = Batch when is_map(PendingListIds) ->
            Pending = invalidate_pending(UserId, PendingListIds, State),
            State#{?SYNC_BATCH_STATE_KEY => Batch#{pending_list_ids => Pending}};
        _ ->
            State
    end.

-spec invalidate_pending(user_id(), map(), guild_state()) -> map().
invalidate_pending(UserId, PendingListIds, State) ->
    maps:map(
        fun(ListId, Mark) -> invalidate_synced_mark(UserId, ListId, Mark, State) end,
        PendingListIds
    ).

-spec invalidate_synced_mark(user_id(), list_id(), term(), guild_state()) -> term().
invalidate_synced_mark(UserId, ListId, synced, State) ->
    case guild_member_list_absence:of_list(UserId, ListId, State) of
        present -> true;
        {absent, _Ref} -> synced
    end;
invalidate_synced_mark(_UserId, _ListId, Mark, _State) ->
    Mark.

-spec connection_sync_delay_ms(guild_state()) -> pos_integer().
connection_sync_delay_ms(State) ->
    case State of
        #{member_count := MemberCount} when is_integer(MemberCount), MemberCount >= 5000 ->
            positive_env(member_list_large_connection_sync_delay_ms, 1000);
        _ ->
            positive_env(member_list_connection_sync_delay_ms, 250)
    end.

-spec positive_env(atom(), pos_integer()) -> pos_integer().
positive_env(Key, Default) ->
    case application:get_env(fluxer_gateway, Key, Default) of
        Value when is_integer(Value), Value >= 1 -> Value;
        _ -> Default
    end.
