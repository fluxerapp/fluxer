%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_dispatch_stress_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(EVENT_COUNT, 6000).
-define(MAX_EVENT_BUFFER_SIZE, 4096).
-define(MAX_TOTAL_BUFFER_BYTES, 16777216).

message_create_flood_keeps_replay_buffer_bounded_test_() ->
    {timeout, 30, fun message_create_flood_keeps_replay_buffer_bounded/0}.

pre_encoded_flood_keeps_replay_buffer_bounded_test_() ->
    {timeout, 30, fun pre_encoded_flood_keeps_replay_buffer_bounded/0}.

guild_members_chunk_flood_does_not_fill_replay_buffer_test_() ->
    {timeout, 30, fun guild_members_chunk_flood_does_not_fill_replay_buffer/0}.

guild_member_list_update_flood_does_not_fill_replay_buffer_test_() ->
    {timeout, 30, fun guild_member_list_update_flood_does_not_fill_replay_buffer/0}.

message_create_flood_keeps_replay_buffer_bounded() ->
    State0 = base_state(),
    State1 = lists:foldl(
        fun(Seq, State) ->
            Data = #{
                <<"id">> => integer_to_binary(Seq),
                <<"content">> => <<"stress">>
            },
            {noreply, NextState} = session_dispatch:handle_dispatch(
                message_create, Data, State
            ),
            NextState
        end,
        State0,
        lists:seq(1, ?EVENT_COUNT)
    ),
    Buffer = maps:get(buffer, State1),
    BufferedEvents = limited_deque:to_list(Buffer),
    ?assertEqual(?EVENT_COUNT, maps:get(seq, State1)),
    ?assertEqual(?MAX_EVENT_BUFFER_SIZE, limited_deque:size(Buffer)),
    ?assert(limited_deque:bytes(Buffer) =< ?MAX_TOTAL_BUFFER_BYTES),
    ?assertEqual(?EVENT_COUNT - ?MAX_EVENT_BUFFER_SIZE + 1, first_buffered_seq(BufferedEvents)),
    ?assertEqual(?EVENT_COUNT, last_buffered_seq(BufferedEvents)),
    ?assertEqual(?EVENT_COUNT - ?MAX_EVENT_BUFFER_SIZE, maps:get(replay_floor, State1)).

pre_encoded_flood_keeps_replay_buffer_bounded() ->
    Encoded = iolist_to_binary(json:encode(#{<<"content">> => <<"preencoded stress">>})),
    State1 = lists:foldl(
        fun(_Seq, State) ->
            {noreply, NextState} = session_dispatch:handle_dispatch(
                message_create, {pre_encoded, Encoded}, State
            ),
            NextState
        end,
        base_state(),
        lists:seq(1, ?EVENT_COUNT)
    ),
    Buffer = maps:get(buffer, State1),
    BufferedEvents = limited_deque:to_list(Buffer),
    ?assertEqual(?EVENT_COUNT, maps:get(seq, State1)),
    ?assertEqual(?MAX_EVENT_BUFFER_SIZE, limited_deque:size(Buffer)),
    ?assert(limited_deque:bytes(Buffer) =< ?MAX_TOTAL_BUFFER_BYTES),
    ?assertEqual(?EVENT_COUNT - ?MAX_EVENT_BUFFER_SIZE + 1, first_buffered_seq(BufferedEvents)),
    ?assertEqual(?EVENT_COUNT, last_buffered_seq(BufferedEvents)),
    ?assertEqual(?EVENT_COUNT - ?MAX_EVENT_BUFFER_SIZE, maps:get(replay_floor, State1)).

guild_member_list_update_flood_does_not_fill_replay_buffer() ->
    Encoded = iolist_to_binary(json:encode(#{<<"ops">> => []})),
    State1 = lists:foldl(
        fun(_Seq, State) ->
            {noreply, NextState} = session_dispatch:handle_dispatch(
                guild_member_list_update, {pre_encoded, Encoded}, State
            ),
            NextState
        end,
        base_state(),
        lists:seq(1, ?EVENT_COUNT)
    ),
    ?assertEqual(?EVENT_COUNT, maps:get(seq, State1)),
    ?assertEqual(0, limited_deque:size(maps:get(buffer, State1))),
    ?assertEqual(0, maps:get(buffer_bytes, State1)).

guild_members_chunk_flood_does_not_fill_replay_buffer() ->
    State1 = lists:foldl(
        fun(Seq, State) ->
            Data = #{
                <<"guild_id">> => <<"123">>,
                <<"chunk_index">> => Seq,
                <<"chunk_count">> => ?EVENT_COUNT,
                <<"members">> => []
            },
            {noreply, NextState} = session_dispatch:handle_dispatch(
                guild_members_chunk, Data, State
            ),
            NextState
        end,
        base_state(),
        lists:seq(1, ?EVENT_COUNT)
    ),
    ?assertEqual(?EVENT_COUNT, maps:get(seq, State1)),
    ?assertEqual(0, limited_deque:size(maps:get(buffer, State1))),
    ?assertEqual(0, maps:get(buffer_bytes, State1)).

base_state() ->
    #{
        seq => 0,
        user_id => 1,
        buffer => limited_deque:new(?MAX_EVENT_BUFFER_SIZE, ?MAX_TOTAL_BUFFER_BYTES),
        buffer_bytes => 0,
        socket_pid => undefined,
        channels => #{},
        relationships => #{},
        suppress_presence_updates => false,
        pending_presences => [],
        presence_pid => undefined,
        ignored_events => #{},
        debounce_reactions => false,
        reaction_buffer => [],
        reaction_buffer_timer => undefined
    }.

first_buffered_seq([First | _]) ->
    maps:get(seq, First).

last_buffered_seq(Events) ->
    maps:get(seq, lists:last(Events)).
