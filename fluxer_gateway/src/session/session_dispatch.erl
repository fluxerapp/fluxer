%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_dispatch).
-typing([eqwalizer]).

-export([
    handle_dispatch/3,
    flush_all_pending_presences/1,
    flush_reaction_buffer/1,
    replay_floor_after_eviction/2
]).

-export_type([session_state/0, event/0]).

-define(MAX_EVENT_BUFFER_SIZE, 4096).
-define(MAX_SINGLE_EVENT_BUFFER_BYTES, 2097152).
-define(MAX_TOTAL_BUFFER_BYTES, 16777216).

-type session_state() :: session:session_state().
-type event() :: atom() | binary().

-spec handle_dispatch(event(), map() | list() | {pre_encoded, binary()}, session_state()) ->
    {noreply, session_state()}.
handle_dispatch(Event, {pre_encoded, _} = Data, State) ->
    case
        should_skip_for_shard(Event, Data, State) orelse should_ignore_event(Event, Data, State) orelse
            thread_backstop(Event, #{}, State) =:= drop
    of
        true -> {noreply, State};
        false -> do_handle_dispatch_pre_encoded(Event, Data, State)
    end;
handle_dispatch(Event, Data, State) ->
    case thread_backstop(Event, Data, State) of
        drop -> {noreply, State};
        {ok, Data1} -> handle_plain_dispatch(Event, Data1, State)
    end.

-spec handle_plain_dispatch(event(), map() | list(), session_state()) ->
    {noreply, session_state()}.
handle_plain_dispatch(Event, Data, State) ->
    case
        should_skip_for_shard(Event, Data, State) orelse should_ignore_event(Event, Data, State)
    of
        true -> {noreply, State};
        false -> route_bot_guild_event(Event, Data, State)
    end.

-spec thread_backstop(event(), map() | list(), session_state()) -> drop | {ok, map() | list()}.
thread_backstop(Event, Data, State) ->
    Capable =
        maps:get(bot, State, false) =:= true orelse
            maps:get(thread_channels_capable, State, false) =:= true,
    case {Capable, is_thread_event(Event), Data} of
        {false, true, _} ->
            drop;
        {_, _, #{<<"__thread_scoped">> := GuildId}} ->
            thread_scoped(Capable, GuildId, Data, State);
        {_, _, #{<<"__thread_unscoped">> := GuildId}} ->
            thread_unscoped(thread_viewer(Capable, GuildId, State), Data);
        _ ->
            {ok, Data}
    end.

-spec thread_scoped(boolean(), term(), map(), session_state()) -> drop | {ok, map()}.
thread_scoped(false, _GuildId, _Data, _State) ->
    drop;
thread_scoped(true, GuildId, Data, State) ->
    case thread_viewer(true, GuildId, State) of
        true -> {ok, maps:remove(<<"__thread_scoped">>, Data)};
        false -> drop
    end.

-spec thread_unscoped(boolean(), map()) -> drop | {ok, map()}.
thread_unscoped(true, _Data) ->
    drop;
thread_unscoped(false, Data) ->
    {ok, maps:remove(<<"__thread_unscoped">>, Data)}.

-spec thread_viewer(boolean(), term(), session_state()) -> boolean().
thread_viewer(false, _GuildId, _State) ->
    false;
thread_viewer(true, GuildId, State) ->
    UserId = maps:get(user_id, State, undefined),
    case {channel_threads_config:loaded(), snowflake_id:parse_maybe(GuildId)} of
        {false, _} ->
            true;
        {true, G} when is_integer(G), is_integer(UserId) ->
            guild_thread_gate:compute_viewer(
                channel_threads_config:guild_active(G),
                maps:get(bot, State, false) =:= true,
                true,
                UserId
            );
        _ ->
            false
    end.

-spec is_thread_event(event()) -> boolean().
is_thread_event(thread_create) -> true;
is_thread_event(thread_update) -> true;
is_thread_event(thread_delete) -> true;
is_thread_event(thread_list_sync) -> true;
is_thread_event(thread_member_update) -> true;
is_thread_event(thread_members_update) -> true;
is_thread_event(thread_member_list_update) -> true;
is_thread_event(forum_unreads) -> true;
is_thread_event(<<"THREAD_", _/binary>>) -> true;
is_thread_event(<<"FORUM_UNREADS">>) -> true;
is_thread_event(_) -> false.

-spec route_bot_guild_event(event(), map() | list(), session_state()) ->
    {noreply, session_state()}.
route_bot_guild_event(Event, Data, State) when is_map(Data) ->
    {BotData, BotState} = session_bot_guilds:guild_event(Event, Data, State),
    route_dispatch(Event, BotData, BotState);
route_bot_guild_event(Event, Data, State) ->
    route_dispatch(Event, Data, State).

-spec should_skip_for_shard(
    event(), map() | list() | {pre_encoded, binary()}, session_state()
) ->
    boolean().
should_skip_for_shard(Event, {pre_encoded, EncodedData}, State) ->
    case shard_filter_active(State) of
        true -> should_skip_pre_encoded_for_shard(Event, EncodedData);
        false -> false
    end;
should_skip_for_shard(Event, Data, State) when is_map(Data) ->
    case shard_filter_active(State) of
        true -> not has_guild_context(Event, Data);
        false -> false
    end;
should_skip_for_shard(_Event, Data, State) when is_list(Data) ->
    shard_filter_active(State).

-spec should_skip_pre_encoded_for_shard(event(), binary()) -> boolean().
should_skip_pre_encoded_for_shard(Event, EncodedData) ->
    case decode_pre_encoded_data(EncodedData) of
        {ok, Data} -> not has_guild_context(Event, Data);
        error -> false
    end.

-spec shard_filter_active(session_state()) -> boolean().
shard_filter_active(State) ->
    case maps:get(shard, State, undefined) of
        {ShardId, _NumShards} when ShardId =/= 0 -> true;
        _Other -> false
    end.

-spec decode_pre_encoded_data(binary()) -> {ok, map()} | error.
decode_pre_encoded_data(EncodedData) ->
    try json:decode(EncodedData) of
        Data when is_map(Data) -> {ok, Data};
        _Other -> error
    catch
        error:_Reason -> error;
        exit:_Reason -> error
    end.

-spec has_guild_context(event(), map()) -> boolean().
has_guild_context(Event, Data) ->
    case session_reply_event(Event) orelse has_nonempty_field(<<"guild_id">>, Data) of
        true -> true;
        false -> guild_id_event(Event) andalso has_nonempty_field(<<"id">>, Data)
    end.

-spec session_reply_event(event()) -> boolean().
session_reply_event(Event) ->
    case event_name(Event) of
        <<"RATE_LIMITED">> -> true;
        <<"GUILD_COUNTS_UPDATE">> -> true;
        <<"CHANNEL_MEMBER_COUNTS_UPDATE">> -> true;
        _Other -> false
    end.

-spec has_nonempty_field(binary(), map()) -> boolean().
has_nonempty_field(Key, Data) ->
    case maps:get(Key, Data, undefined) of
        Value when is_integer(Value), Value > 0 -> true;
        Value when is_binary(Value), byte_size(Value) > 0 -> true;
        _Other -> false
    end.

-spec guild_id_event(event()) -> boolean().
guild_id_event(Event) ->
    case event_name(Event) of
        <<"GUILD_CREATE">> -> true;
        <<"GUILD_UPDATE">> -> true;
        <<"GUILD_DELETE">> -> true;
        <<"GUILD_SYNC">> -> true;
        _Other -> false
    end.

-spec route_dispatch(event(), map() | list(), session_state()) -> {noreply, session_state()}.
route_dispatch(Event, Data, State) ->
    case session_dispatch_voice:should_buffer_reaction(Event, State) of
        true ->
            {noreply, session_dispatch_voice:buffer_reaction(Data, State)};
        false ->
            route_after_reaction(Event, Data, State)
    end.

-spec route_after_reaction(event(), map() | list(), session_state()) ->
    {noreply, session_state()}.
route_after_reaction(Event, Data, State) ->
    case session_dispatch_voice:maybe_cancel_buffered_reaction(Event, Data, State) of
        {cancelled, NewState} ->
            {noreply, NewState};
        not_applicable ->
            route_after_cancel(Event, Data, State)
    end.

-spec route_after_cancel(event(), map() | list(), session_state()) ->
    {noreply, session_state()}.
route_after_cancel(Event, Data, State) ->
    case session_dispatch_presence:should_buffer_presence(Event, Data, State) of
        true ->
            {noreply, session_dispatch_presence:buffer_presence(Event, Data, State)};
        false ->
            do_handle_dispatch(Event, Data, State)
    end.

-spec do_handle_dispatch(event(), map() | list(), session_state()) ->
    {noreply, session_state()}.
do_handle_dispatch(Event, Data, State) ->
    Seq = maps:get(seq, State),
    NewSeq = Seq + 1,
    case should_skip_replay_buffer(Event) of
        true ->
            dispatch_without_replay(Event, Data, NewSeq, State);
        false ->
            dispatch_replayable_event(Event, Data, NewSeq, State)
    end.

-spec dispatch_replayable_event(event(), map() | list(), non_neg_integer(), session_state()) ->
    {noreply, session_state()}.
dispatch_replayable_event(Event, Data, NewSeq, State) ->
    Request = #{event => Event, data => Data, seq => NewSeq},
    RequestBytes = buffer_entry_bytes(Request),
    case is_oversized_event(RequestBytes) of
        true ->
            dispatch_without_replay(Event, Data, NewSeq, State#{replay_floor => NewSeq});
        false ->
            dispatch_with_replay(Event, Data, NewSeq, Request, RequestBytes, State)
    end.

-spec dispatch_with_replay(
    event(), map() | list(), non_neg_integer(), map(), non_neg_integer(), session_state()
) ->
    {noreply, session_state()}.
dispatch_with_replay(Event, Data, NewSeq, Request, RequestBytes, State) ->
    Buffer = maps:get(buffer, State),
    Deque =
        case is_list(Buffer) of
            true ->
                limited_deque:from_list(
                    Buffer, ?MAX_EVENT_BUFFER_SIZE, ?MAX_TOTAL_BUFFER_BYTES
                );
            false ->
                Buffer
        end,
    {NewBuffer, Dropped} = limited_deque:push_trimmed(Request, RequestBytes, Deque),
    send_to_socket(maps:get(socket_pid, State, undefined), Event, Data, NewSeq),
    StateAfterMain = apply_state_updates(Event, Data, State, #{
        seq => NewSeq,
        buffer => NewBuffer,
        buffer_bytes => limited_deque:bytes(NewBuffer),
        replay_floor => replay_floor_after_eviction(Dropped, maps:get(replay_floor, State, 0))
    }),
    finalize_dispatch(Event, Data, StateAfterMain).

-spec dispatch_without_replay(event(), map() | list(), non_neg_integer(), session_state()) ->
    {noreply, session_state()}.
dispatch_without_replay(Event, Data, NewSeq, State) ->
    send_to_socket(maps:get(socket_pid, State, undefined), Event, Data, NewSeq),
    StateAfterMain = apply_state_updates(Event, Data, State, #{seq => NewSeq}),
    finalize_dispatch(Event, Data, StateAfterMain).

-spec apply_state_updates(event(), map() | list(), session_state(), map()) -> session_state().
apply_state_updates(Event, Data, State, Extra) ->
    S1 = session_dispatch_guild:update_channels_map(Event, Data, State),
    S2 = session_dispatch_guild:update_dm_voice_states_map(Event, Data, S1),
    S3 = session_dispatch_guild:update_relationships_map(Event, Data, S2),
    maps:merge(S3, Extra).

-spec finalize_dispatch(event(), map() | list(), session_state()) ->
    {noreply, session_state()}.
finalize_dispatch(Event, Data, State) ->
    {S1, FlushedIds} = session_dispatch_presence:maybe_flush_pending_presences(
        Event, Data, State
    ),
    {noreply, session_dispatch_presence:maybe_sync_presence_targets(Event, FlushedIds, S1)}.

-spec do_handle_dispatch_pre_encoded(event(), {pre_encoded, binary()}, session_state()) ->
    {noreply, session_state()}.
do_handle_dispatch_pre_encoded(Event, {pre_encoded, EncodedData} = Data, State) ->
    Seq = maps:get(seq, State),
    NewSeq = Seq + 1,
    BufferedState = buffer_pre_encoded_event(Event, Data, NewSeq, State),
    send_to_socket(maps:get(socket_pid, State, undefined), Event, Data, NewSeq),
    StateAfterMain =
        case needs_state_update(Event) of
            true -> apply_pre_encoded_state_update(Event, EncodedData, BufferedState, NewSeq);
            false -> BufferedState#{seq => NewSeq}
        end,
    {noreply, StateAfterMain}.

-spec buffer_pre_encoded_event(
    event(), {pre_encoded, binary()}, non_neg_integer(), session_state()
) ->
    session_state().
buffer_pre_encoded_event(Event, Data, NewSeq, State) ->
    case should_buffer_pre_encoded(Event) of
        false ->
            State;
        true ->
            Request = #{event => Event, data => Data, seq => NewSeq},
            RequestBytes = buffer_entry_bytes(Request),
            case is_oversized_event(RequestBytes) of
                true ->
                    State#{replay_floor => NewSeq};
                false ->
                    Buffer = maps:get(buffer, State),
                    Deque =
                        case is_list(Buffer) of
                            true ->
                                limited_deque:from_list(
                                    Buffer, ?MAX_EVENT_BUFFER_SIZE, ?MAX_TOTAL_BUFFER_BYTES
                                );
                            false ->
                                Buffer
                        end,
                    {NewBuffer, Dropped} = limited_deque:push_trimmed(
                        Request, RequestBytes, Deque
                    ),
                    State#{
                        buffer => NewBuffer,
                        buffer_bytes => limited_deque:bytes(NewBuffer),
                        replay_floor => replay_floor_after_eviction(
                            Dropped, maps:get(replay_floor, State, 0)
                        )
                    }
            end
    end.

-spec apply_pre_encoded_state_update(event(), binary(), session_state(), non_neg_integer()) ->
    session_state().
apply_pre_encoded_state_update(Event, EncodedData, State, NewSeq) ->
    case json:decode(EncodedData) of
        DecodedData when is_map(DecodedData) ->
            S1 = apply_state_updates(Event, DecodedData, State, #{seq => NewSeq}),
            {S2, FlushedIds} = session_dispatch_presence:maybe_flush_pending_presences(
                Event, DecodedData, S1
            ),
            session_dispatch_presence:maybe_sync_presence_targets(Event, FlushedIds, S2);
        _ ->
            State#{seq => NewSeq}
    end.

-spec needs_state_update(event()) -> boolean().
needs_state_update(channel_create) -> true;
needs_state_update(channel_update) -> true;
needs_state_update(channel_delete) -> true;
needs_state_update(channel_recipient_add) -> true;
needs_state_update(channel_recipient_remove) -> true;
needs_state_update(relationship_add) -> true;
needs_state_update(relationship_update) -> true;
needs_state_update(relationship_remove) -> true;
needs_state_update(_) -> false.

-spec is_oversized_event(non_neg_integer()) -> boolean().
is_oversized_event(RequestBytes) ->
    RequestBytes > ?MAX_SINGLE_EVENT_BUFFER_BYTES.

-spec should_buffer_pre_encoded(event()) -> boolean().
should_buffer_pre_encoded(Event) ->
    not should_skip_replay_buffer(Event).

-spec should_skip_replay_buffer(event()) -> boolean().
should_skip_replay_buffer(Event) ->
    case event_name(Event) of
        <<"GUILD_MEMBERS_CHUNK">> -> true;
        <<"GUILD_MEMBER_LIST_UPDATE">> -> true;
        <<"GUILD_SYNC">> -> true;
        <<"THREAD_LIST_SYNC">> -> true;
        <<"THREAD_MEMBER_LIST_UPDATE">> -> true;
        _Other -> false
    end.

-spec buffer_entry_bytes(term()) -> non_neg_integer().
buffer_entry_bytes(Request) ->
    limited_deque:entry_bytes(Request).

-spec replay_floor_after_eviction([term()], non_neg_integer()) -> non_neg_integer().
replay_floor_after_eviction(Dropped, Floor) ->
    lists:foldl(fun evicted_seq_max/2, Floor, Dropped).

-spec evicted_seq_max(term(), non_neg_integer()) -> non_neg_integer().
evicted_seq_max(Event, Floor) when is_map(Event) ->
    case maps:get(seq, Event, undefined) of
        Seq when is_integer(Seq), Seq > Floor -> Seq;
        _Other -> Floor
    end;
evicted_seq_max(_Event, Floor) ->
    Floor.

-spec send_to_socket(pid() | undefined, event(), term(), non_neg_integer()) -> ok.
send_to_socket(undefined, _Event, _Data, _Seq) ->
    ok;
send_to_socket(Pid, Event, Data, Seq) when is_pid(Pid) ->
    Pid ! {dispatch, Event, guild_data_wire:payload(Data), Seq},
    ok.

-spec should_ignore_event(event(), map() | list() | {pre_encoded, binary()}, session_state()) ->
    boolean().
should_ignore_event(Event, Data, State) ->
    IgnoredEvents = maps:get(ignored_events, State, #{}),
    case event_name(Event) of
        undefined ->
            false;
        EventName ->
            maps:is_key(EventName, IgnoredEvents) andalso
                not ignored_event_must_dispatch(Event, Data, State)
    end.

-spec event_name(event()) -> binary() | undefined.
event_name(Event) when is_binary(Event) -> Event;
event_name(Event) when is_atom(Event) ->
    try constants:dispatch_event_atom(Event) of
        Name when is_binary(Name) -> Name
    catch
        error:_Reason -> undefined;
        exit:_Reason -> undefined
    end;
event_name(_) ->
    undefined.

-spec ignored_event_must_dispatch(
    event(), map() | list() | {pre_encoded, binary()}, session_state()
) ->
    boolean().
ignored_event_must_dispatch(message_create, {pre_encoded, EncodedData}, State) ->
    case decode_pre_encoded_data(EncodedData) of
        {ok, Data} -> ignored_event_must_dispatch(message_create, Data, State);
        error -> false
    end;
ignored_event_must_dispatch(message_create, Data, State) when is_map(Data) ->
    session_passive:is_user_mentioned(Data, State);
ignored_event_must_dispatch(_, _Data, _State) ->
    false.

-spec flush_all_pending_presences(session_state()) -> session_state().
flush_all_pending_presences(State) ->
    session_dispatch_presence:flush_all_pending_presences(State).

-spec flush_reaction_buffer(session_state()) -> session_state().
flush_reaction_buffer(State) ->
    session_dispatch_voice:flush_reaction_buffer(fun do_handle_dispatch/3, State).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

base_state(Opts) ->
    maps:merge(
        #{
            seq => 0,
            user_id => 1,
            buffer => [],
            buffer_bytes => 0,
            socket_pid => undefined,
            channels => #{},
            relationships => #{},
            suppress_presence_updates => false,
            pending_presences => [],
            presence_pid => undefined,
            ignored_events => #{},
            debounce_reactions => false,
            thread_channels_capable => false,
            reaction_buffer => [],
            reaction_buffer_timer => undefined
        },
        Opts
    ).

is_oversized_event_small_event_test() ->
    Req = #{event => message_create, data => #{<<"content">> => <<"hello">>}, seq => 1},
    ?assertEqual(false, is_oversized_event(buffer_entry_bytes(Req))).

is_oversized_event_large_event_test() ->
    LargeData = make_large_data(),
    Req = #{event => guild_create, data => LargeData, seq => 1},
    ?assertEqual(true, is_oversized_event(buffer_entry_bytes(Req))).

ignored_message_create_dispatches_when_mentioned_test() ->
    State = base_state(#{
        ignored_events => #{<<"MESSAGE_CREATE">> => true},
        user_id => 123,
        user_roles => [456]
    }),
    Direct = #{<<"mentions">> => [#{<<"id">> => <<"123">>}]},
    Role = #{<<"mention_roles">> => [<<"456">>]},
    Everyone = #{<<"mention_everyone">> => true},
    Unmentioned = #{<<"mentions">> => [], <<"mention_roles">> => []},
    ?assertEqual(false, should_ignore_event(message_create, Direct, State)),
    ?assertEqual(false, should_ignore_event(message_create, Role, State)),
    ?assertEqual(false, should_ignore_event(message_create, Everyone, State)),
    ?assertEqual(true, should_ignore_event(message_create, Unmentioned, State)).

oversized_event_sent_but_not_buffered_test() ->
    LargeData = make_large_data(),
    {noreply, S1} = do_handle_dispatch(guild_create, LargeData, base_state(#{})),
    ?assertEqual([], maps:get(buffer, S1, [])),
    ?assertEqual(1, maps:get(seq, S1)),
    ?assertEqual(1, maps:get(replay_floor, S1)).

guild_members_chunk_sent_but_not_buffered_test() ->
    ChunkData = #{
        <<"guild_id">> => <<"123">>,
        <<"chunk_index">> => 0,
        <<"chunk_count">> => 2,
        <<"members">> => []
    },
    BaseState = base_state(#{socket_pid => self()}),
    {noreply, S1} = do_handle_dispatch(guild_members_chunk, ChunkData, BaseState),
    ?assertEqual([], maps:get(buffer, S1, [])),
    ?assertEqual(0, maps:get(buffer_bytes, S1, 0)),
    ?assertEqual(1, maps:get(seq, S1)),
    receive
        {dispatch, guild_members_chunk, ReceivedData, 1} ->
            ?assertEqual(ChunkData, ReceivedData)
    after 100 -> ?assert(false, dispatch_not_received)
    end.

standard_dispatch_sends_wire_payload_but_buffers_internal_data_test() ->
    Data = #{<<"id">> => 123, <<"roles">> => [456], <<"permissions">> => 8},
    {noreply, S1} = do_handle_dispatch(guild_create, Data, base_state(#{socket_pid => self()})),
    [Buffered] = limited_deque:to_list(maps:get(buffer, S1)),
    case Buffered of
        BufferedMap when is_map(BufferedMap) ->
            ?assertEqual(Data, maps:get(data, BufferedMap));
        _ ->
            ?assert(false)
    end,
    receive
        {dispatch, guild_create, ReceivedData, 1} ->
            ?assertEqual(
                #{
                    <<"id">> => <<"123">>,
                    <<"roles">> => [<<"456">>],
                    <<"permissions">> => <<"8">>
                },
                ReceivedData
            )
    after 100 ->
        ?assert(false, dispatch_not_received)
    end.

make_large_data() ->
    Pairs = [{integer_to_binary(I), lists:duplicate(1000, $x)} || I <- lists:seq(1, 500)],
    maps:from_list(Pairs).

normal_event_buffered_test() ->
    Data = #{<<"content">> => <<"hello">>},
    {noreply, S1} = do_handle_dispatch(message_create, Data, base_state(#{})),
    ?assertEqual(1, limited_deque:size(maps:get(buffer, S1))),
    ?assertEqual(1, maps:get(seq, S1)).

should_skip_replay_buffer_test() ->
    ?assertEqual(true, should_skip_replay_buffer(guild_members_chunk)),
    ?assertEqual(true, should_skip_replay_buffer(<<"GUILD_MEMBERS_CHUNK">>)),
    ?assertEqual(true, should_skip_replay_buffer(guild_member_list_update)),
    ?assertEqual(true, should_skip_replay_buffer(guild_sync)),
    ?assertEqual(false, should_skip_replay_buffer(message_create)).

should_buffer_pre_encoded_test() ->
    ?assertEqual(false, should_buffer_pre_encoded(guild_members_chunk)),
    ?assertEqual(false, should_buffer_pre_encoded(guild_member_list_update)),
    ?assertEqual(false, should_buffer_pre_encoded(guild_sync)),
    ?assertEqual(true, should_buffer_pre_encoded(voice_state_update)),
    ?assertEqual(true, should_buffer_pre_encoded(message_create)),
    ?assertEqual(true, should_buffer_pre_encoded(channel_update)).

guild_member_list_update_map_form_not_buffered_test() ->
    Data = #{<<"guild_id">> => <<"123">>, <<"ops">> => []},
    {noreply, S1} = do_handle_dispatch(
        guild_member_list_update, Data, base_state(#{replay_floor => 0})
    ),
    ?assertEqual([], maps:get(buffer, S1, [])),
    ?assertEqual(1, maps:get(seq, S1)),
    ?assertEqual(0, maps:get(replay_floor, S1)).

replay_floor_stays_zero_without_eviction_test() ->
    {noreply, S1} = do_handle_dispatch(
        message_create, #{<<"content">> => <<"hello">>}, base_state(#{})
    ),
    ?assertEqual(0, maps:get(replay_floor, S1)).

replay_floor_tracks_evicted_seq_test() ->
    State0 = base_state(#{buffer => limited_deque:new(2, 0)}),
    State3 = lists:foldl(
        fun(_N, S) ->
            {noreply, Next} = do_handle_dispatch(
                message_create, #{<<"content">> => <<"hello">>}, S
            ),
            Next
        end,
        State0,
        lists:seq(1, 3)
    ),
    ?assertEqual(1, maps:get(replay_floor, State3)),
    ?assertEqual(
        [2, 3],
        [maps:get(seq, E) || E <- limited_deque:to_list(maps:get(buffer, State3))]
    ).

needs_state_update_test() ->
    lists:foreach(
        fun(E) -> ?assertEqual(true, needs_state_update(E)) end,
        [
            channel_create,
            channel_update,
            channel_delete,
            channel_recipient_add,
            channel_recipient_remove,
            relationship_add,
            relationship_update,
            relationship_remove
        ]
    ),
    lists:foreach(
        fun(E) -> ?assertEqual(false, needs_state_update(E)) end,
        [
            message_create,
            guild_member_list_update,
            guild_sync,
            presence_update,
            typing_start,
            guild_member_update
        ]
    ).

guildless_dispatch_skipped_for_nonzero_shard_test() ->
    drain_mailbox(),
    BaseState = base_state(#{socket_pid => self(), shard => {1, 2}}),
    {noreply, S1} = handle_dispatch(message_create, #{<<"content">> => <<"hello">>}, BaseState),
    ?assertEqual(0, maps:get(seq, S1)),
    assert_no_dispatch().

guild_dispatch_allowed_for_nonzero_shard_test() ->
    drain_mailbox(),
    Data = #{<<"guild_id">> => <<"123">>, <<"content">> => <<"hello">>},
    BaseState = base_state(#{socket_pid => self(), shard => {1, 2}}),
    {noreply, S1} = handle_dispatch(message_create, Data, BaseState),
    ?assertEqual(1, maps:get(seq, S1)),
    receive
        {dispatch, message_create, Data, 1} -> ok
    after 100 ->
        ?assert(false, dispatch_not_received)
    end.

guildless_dispatch_allowed_for_shard_zero_test() ->
    drain_mailbox(),
    Data = #{<<"content">> => <<"hello">>},
    BaseState = base_state(#{socket_pid => self(), shard => {0, 2}}),
    {noreply, S1} = handle_dispatch(message_create, Data, BaseState),
    ?assertEqual(1, maps:get(seq, S1)),
    receive
        {dispatch, message_create, Data, 1} -> ok
    after 100 ->
        ?assert(false, dispatch_not_received)
    end.

list_payload_dispatch_reaches_socket_test() ->
    drain_mailbox(),
    Data = [<<"1">>, <<"2">>],
    BaseState = base_state(#{socket_pid => self()}),
    {noreply, S1} = handle_dispatch(user_pinned_dms_update, Data, BaseState),
    ?assertEqual(1, maps:get(seq, S1)),
    receive
        {dispatch, user_pinned_dms_update, ReceivedData, 1} ->
            ?assertEqual(Data, ReceivedData)
    after 100 ->
        ?assert(false, dispatch_not_received)
    end.

list_payload_dispatch_skipped_for_nonzero_shard_test() ->
    drain_mailbox(),
    BaseState = base_state(#{socket_pid => self(), shard => {1, 2}}),
    {noreply, S1} = handle_dispatch(user_pinned_dms_update, [<<"1">>], BaseState),
    ?assertEqual(0, maps:get(seq, S1)),
    assert_no_dispatch().

pre_encoded_guildless_dispatch_skipped_for_nonzero_shard_test() ->
    drain_mailbox(),
    Encoded = iolist_to_binary(
        json:encode(
            #{<<"content">> => <<"hello">>}, fun json:encode_value/2
        )
    ),
    BaseState = base_state(#{socket_pid => self(), shard => {1, 2}}),
    {noreply, S1} = handle_dispatch(message_create, {pre_encoded, Encoded}, BaseState),
    ?assertEqual(0, maps:get(seq, S1)),
    assert_no_dispatch().

drain_mailbox() ->
    receive
        _Message -> drain_mailbox()
    after 0 ->
        ok
    end.

assert_no_dispatch() ->
    receive
        {dispatch, _Event, _Data, _Seq} -> ?assert(false, unexpected_dispatch)
    after 100 ->
        ok
    end.

thread_backstop_drops_thread_events_for_incapable_sessions_test() ->
    State = base_state(#{}),
    ?assertEqual(drop, thread_backstop(thread_create, #{}, State)),
    ?assertEqual(drop, thread_backstop(<<"THREAD_LIST_SYNC">>, #{}, State)),
    ?assertEqual(
        drop, thread_backstop(message_ack, #{<<"__thread_scoped">> => <<"5">>}, State)
    ),
    ?assertEqual({ok, #{}}, thread_backstop(message_create, #{}, State)),
    ?assertEqual({ok, #{}}, thread_backstop(thread_create, #{}, State#{bot => true})),
    ?assertEqual(
        {ok, #{}}, thread_backstop(thread_create, #{}, State#{thread_channels_capable => true})
    ).

thread_backstop_passes_scoped_events_before_the_config_loads_test() ->
    with_default_config(false, fun() ->
        Data = #{<<"__thread_scoped">> => <<"5">>, <<"id">> => <<"9">>},
        Unscoped = #{<<"__thread_unscoped">> => <<"5">>, <<"id">> => <<"9">>},
        Capable = base_state(#{thread_channels_capable => true}),
        ?assertEqual({ok, #{<<"id">> => <<"9">>}}, thread_backstop(message_ack, Data, Capable)),
        ?assertEqual(drop, thread_backstop(user_guild_settings_update, Unscoped, Capable)),
        ?assertEqual(drop, thread_backstop(message_ack, Data, base_state(#{}))),
        ?assertEqual(
            {ok, #{<<"id">> => <<"9">>}},
            thread_backstop(user_guild_settings_update, Unscoped, base_state(#{}))
        )
    end).

thread_backstop_fails_closed_once_a_default_config_is_pulled_test() ->
    with_default_config(true, fun() ->
        Data = #{<<"__thread_scoped">> => <<"5">>, <<"id">> => <<"9">>},
        Unscoped = #{<<"__thread_unscoped">> => <<"5">>, <<"id">> => <<"9">>},
        Capable = base_state(#{thread_channels_capable => true, user_id => 7}),
        Bot = base_state(#{thread_channels_capable => true, bot => true, user_id => 7}),
        ?assertEqual(drop, thread_backstop(message_ack, Data, Capable)),
        ?assertEqual(drop, thread_backstop(message_ack, Data, Bot)),
        ?assertEqual(
            {ok, #{<<"id">> => <<"9">>}},
            thread_backstop(user_guild_settings_update, Unscoped, Capable)
        )
    end).

with_default_config(Pulled, Fun) ->
    Key = channel_threads_config,
    PulledKey = {channel_threads_config, pulled},
    Previous = persistent_term:get(Key, undefined),
    PreviousPulled = persistent_term:get(PulledKey, undefined),
    persistent_term:put(Key, channel_threads_config:default_config()),
    _ = persistent_term:erase(PulledKey),
    Pulled andalso persistent_term:put(PulledKey, true),
    try
        Fun()
    after
        case Previous of
            undefined -> persistent_term:erase(Key);
            _ -> persistent_term:put(Key, Previous)
        end,
        case PreviousPulled of
            undefined -> persistent_term:erase(PulledKey);
            _ -> persistent_term:put(PulledKey, PreviousPulled)
        end
    end.

thread_backstop_strips_the_scope_key_for_viewers_test() ->
    Key = channel_threads_config,
    Previous = persistent_term:get(Key, undefined),
    persistent_term:put(Key, (channel_threads_config:default_config())#{
        enabled => true,
        enabled_guilds => #{<<"5">> => true},
        included_users => #{<<"1">> => true}
    }),
    try
        Data = #{<<"__thread_scoped">> => <<"5">>, <<"id">> => <<"9">>},
        Capable = base_state(#{thread_channels_capable => true}),
        ?assertEqual({ok, #{<<"id">> => <<"9">>}}, thread_backstop(message_ack, Data, Capable)),
        ?assertEqual(
            drop,
            thread_backstop(message_ack, Data#{<<"__thread_scoped">> => <<"6">>}, Capable)
        ),
        ?assertEqual(drop, thread_backstop(message_ack, Data, Capable#{user_id => 2})),
        Unscoped = #{<<"__thread_unscoped">> => <<"5">>, <<"id">> => <<"9">>},
        ?assertEqual(drop, thread_backstop(user_guild_settings_update, Unscoped, Capable)),
        ?assertEqual(
            {ok, #{<<"id">> => <<"9">>}},
            thread_backstop(user_guild_settings_update, Unscoped, Capable#{user_id => 2})
        ),
        ?assertEqual(
            {ok, #{<<"id">> => <<"9">>}},
            thread_backstop(
                user_guild_settings_update,
                Unscoped#{<<"__thread_unscoped">> => <<"6">>},
                Capable
            )
        ),
        ?assertEqual(
            {ok, #{<<"id">> => <<"9">>}},
            thread_backstop(user_guild_settings_update, Unscoped, base_state(#{}))
        )
    after
        case Previous of
            undefined -> persistent_term:erase(Key);
            _ -> persistent_term:put(Key, Previous)
        end
    end.

thread_subscription_events_skip_the_replay_buffer_test() ->
    ?assert(should_skip_replay_buffer(thread_list_sync)),
    ?assert(should_skip_replay_buffer(thread_member_list_update)).

-endif.
