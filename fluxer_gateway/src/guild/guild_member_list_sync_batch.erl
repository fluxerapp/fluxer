%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_sync_batch).
-typing([eqwalizer]).

-export([
    queue_list_sync/2,
    queue_subscribed_list_syncs/2,
    flush_pending_syncs/1
]).

-type guild_state() :: map().
-type list_id() :: binary().
-type channel_id() :: integer().

-export_type([guild_state/0, list_id/0]).

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
                normalize_list_ids(ListIds ++ maps:keys(PendingListIds)),
                maps:remove(?SYNC_BATCH_STATE_KEY, State)
            };
        _ ->
            {normalize_list_ids(ListIds), State}
    end.

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
        Sessions = maps:get(sessions, State, #{}),
        dispatch_pending_syncs_for_guild(
            GuildId, lists:sort(ListIds), Sessions, State, SubsTab
        ),
        {ok, State}
    end),
    State.

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

queue_list_sync_dispatches_without_pending_timer_test() ->
    State = #{},
    ?assertEqual(State, queue_list_sync(<<"500">>, State)),
    ?assertEqual(State, queue_list_sync(<<"600">>, State)).

flush_pending_syncs_clears_empty_batch_test() ->
    State = #{?SYNC_BATCH_STATE_KEY => #{pending_list_ids => #{}}},
    ?assertEqual(#{}, flush_pending_syncs(State)).

queue_list_sync_drains_preexisting_pending_batch_test() ->
    Ref = erlang:send_after(60000, self(), ?FLUSH_SYNC_BATCH_MSG),
    State = #{
        ?SYNC_BATCH_STATE_KEY => #{
            timer_ref => Ref,
            pending_list_ids => #{<<"500">> => true, <<"600">> => true}
        }
    },
    ?assertEqual(#{}, queue_list_sync(<<"500">>, State)).

-endif.
