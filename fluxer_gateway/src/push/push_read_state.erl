%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_read_state).
-typing([eqwalizer]).

-export([split_stale_buffered/1, check_stale/1, counters/0]).
-export_type([read_key/0]).

-define(CNT_READ_STATE_SUPPRESSED, push_read_state_suppressed).
-define(CNT_READ_STATE_FAILURES, push_read_state_fetch_failures).
-define(CNT_READ_STATE_SKIPPED, push_read_state_fetch_skipped).
-define(READ_STATE_FETCH_TIMEOUT_MS, 2000).
-define(READ_STATE_TABLE, push_read_state_counters).
-define(READ_STATE_FETCH_SLOTS, read_state_fetches_in_flight).
-define(MAX_READ_STATE_FETCHES, 64).

-type read_key() :: {integer(), integer(), integer()}.
-type read_marks() :: #{{integer(), integer()} => non_neg_integer()}.

-spec counters() -> map().
counters() ->
    #{
        read_state_suppressed => read_state_counter(?CNT_READ_STATE_SUPPRESSED),
        read_state_fetch_failures => read_state_counter(?CNT_READ_STATE_FAILURES),
        read_state_fetch_skipped => read_state_counter(?CNT_READ_STATE_SKIPPED),
        read_state_fetches_in_flight => read_state_counter(?READ_STATE_FETCH_SLOTS)
    }.

-spec split_stale_buffered([map()]) -> {[{read_key(), map()}], [map()]}.
split_stale_buffered(ParamsList) ->
    lists:foldr(fun put_buffered/2, {[], []}, ParamsList).

-spec put_buffered(map(), {[{read_key(), map()}], [map()]}) -> {[{read_key(), map()}], [map()]}.
put_buffered(Params, {Stale, Fresh}) ->
    Unbuffered = maps:remove(buffered_at, Params),
    case stale_read_key(Params) of
        {ok, Key} -> {[{Key, Unbuffered} | Stale], Fresh};
        none -> {Stale, [Unbuffered | Fresh]}
    end.

-spec stale_read_key(map()) -> {ok, read_key()} | none.
stale_read_key(#{buffered_at := BufferedAt} = Params) when is_integer(BufferedAt) ->
    case
        erlang:system_time(millisecond) - BufferedAt >= push_outbox:max_age_ms() andalso
            push_message_params:context(Params)
    of
        {ok, #{user_ids := [UserId], channel_id := ChannelId, message_id := MessageId}} ->
            {ok, {UserId, ChannelId, MessageId}};
        _ ->
            none
    end;
stale_read_key(_Params) ->
    none.

-spec check_stale([{read_key(), map()}]) -> ok.
check_stale([]) ->
    ok;
check_stale(Stale) ->
    Channels = lists:usort([{UserId, ChannelId} || {{UserId, ChannelId, _}, _Params} <- Stale]),
    check_channels(read_state_fetch_allowed(length(Channels)), Channels, Stale).

-spec check_channels(boolean(), [{integer(), integer()}], [{read_key(), map()}]) -> ok.
check_channels(true, Channels, Stale) ->
    _ = spawn(fun() -> publish_unread(Channels, Stale) end),
    ok;
check_channels(false, _Channels, Stale) ->
    lists:foreach(fun(Entry) -> publish_unless_read(#{}, Entry) end, Stale).

-spec publish_unread([{integer(), integer()}], [{read_key(), map()}]) -> ok.
publish_unread(Channels, Stale) ->
    try
        ReadMarks = fetch_read_marks(Channels),
        lists:foreach(fun(Entry) -> publish_unless_read(ReadMarks, Entry) end, Stale)
    after
        release_read_state_fetch_slot()
    end.

-spec fetch_read_marks([{integer(), integer()}]) -> read_marks().
fetch_read_marks(Channels) ->
    try
        fetch_each_read_mark(Channels, #{})
    catch
        _:_ -> #{}
    end.

-spec publish_unless_read(read_marks(), {read_key(), map()}) -> ok.
publish_unless_read(ReadMarks, {{UserId, ChannelId, MessageId}, Params}) ->
    case MessageId =< maps:get({UserId, ChannelId}, ReadMarks, 0) of
        true -> bump_read_state_counter(?CNT_READ_STATE_SUPPRESSED, 1);
        false -> push:handle_message_create(Params)
    end.

-spec read_state_fetch_allowed(non_neg_integer()) -> boolean().
read_state_fetch_allowed(Fetches) ->
    case acquire_read_state_fetch_slot() of
        true ->
            true;
        false ->
            bump_read_state_counter(?CNT_READ_STATE_SKIPPED, Fetches),
            false
    end.

-spec acquire_read_state_fetch_slot() -> boolean().
acquire_read_state_fetch_slot() ->
    ok = ensure_read_state_table(),
    try
        ets:update_counter(
            ?READ_STATE_TABLE, ?READ_STATE_FETCH_SLOTS, {2, 1}, {?READ_STATE_FETCH_SLOTS, 0}
        )
    of
        InFlight when InFlight > ?MAX_READ_STATE_FETCHES ->
            release_read_state_fetch_slot(),
            false;
        _InFlight ->
            true
    catch
        error:badarg -> false
    end.

-spec release_read_state_fetch_slot() -> ok.
release_read_state_fetch_slot() ->
    try ets:update_counter(?READ_STATE_TABLE, ?READ_STATE_FETCH_SLOTS, {2, -1, 0, 0}) of
        _InFlight -> ok
    catch
        error:badarg -> ok
    end.

-spec bump_read_state_counter(atom(), non_neg_integer()) -> ok.
bump_read_state_counter(Key, Increment) ->
    ok = ensure_read_state_table(),
    try ets:update_counter(?READ_STATE_TABLE, Key, {2, Increment}, {Key, 0}) of
        _Value -> ok
    catch
        error:badarg -> ok
    end.

-spec ensure_read_state_table() -> ok.
ensure_read_state_table() ->
    case ets:whereis(?READ_STATE_TABLE) of
        undefined ->
            guild_ets_utils:ensure_table(?READ_STATE_TABLE, [
                named_table, public, set, {write_concurrency, true}
            ]);
        _Table ->
            ok
    end.

-spec read_state_counter(atom()) -> non_neg_integer() | unavailable.
read_state_counter(Key) ->
    try ets:lookup(?READ_STATE_TABLE, Key) of
        [{Key, Value}] when is_integer(Value), Value >= 0 -> Value;
        _ -> unavailable
    catch
        error:badarg -> unavailable
    end.

-spec fetch_each_read_mark([{integer(), integer()}], read_marks()) -> read_marks().
fetch_each_read_mark([], ReadMarks) ->
    ReadMarks;
fetch_each_read_mark([{UserId, ChannelId} = Key | Rest], ReadMarks) ->
    case fetch_read_message_id(UserId, ChannelId) of
        {ok, ReadMessageId} ->
            fetch_each_read_mark(Rest, ReadMarks#{Key => ReadMessageId});
        error ->
            bump_read_state_counter(?CNT_READ_STATE_SKIPPED, length(Rest)),
            ReadMarks
    end.

-spec fetch_read_message_id(integer(), integer()) -> {ok, non_neg_integer()} | error.
fetch_read_message_id(UserId, ChannelId) ->
    Request = #{
        <<"type">> => <<"get_read_state">>,
        <<"user_id">> => integer_to_binary(UserId),
        <<"channel_id">> => integer_to_binary(ChannelId)
    },
    case read_state_call(Request) of
        {ok, #{<<"last_message_id">> := LastMessageId}} ->
            {ok, read_message_id(snowflake_id:parse_maybe(LastMessageId))};
        {ok, _Data} ->
            {ok, 0};
        {error, Reason} ->
            bump_read_state_counter(?CNT_READ_STATE_FAILURES, 1),
            logger:debug("Push: read state fetch failed", #{
                reason => Reason, user_id => UserId, channel_id => ChannelId
            }),
            error
    end.

-spec read_state_call(map()) -> {ok, term()} | {error, term()}.
read_state_call(Request) ->
    try
        rpc_client:call(Request, ?READ_STATE_FETCH_TIMEOUT_MS)
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

-spec read_message_id(integer() | undefined) -> non_neg_integer().
read_message_id(MessageId) when is_integer(MessageId), MessageId > 0 -> MessageId;
read_message_id(_MessageId) -> 0.
