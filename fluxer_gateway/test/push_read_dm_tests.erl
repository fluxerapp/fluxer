%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_read_dm_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(USER, 910001).
-define(PARTNER, 910002).
-define(DM, 910005).
-define(PARTNER_MSG, 910100).
-define(REPLY_MSG, 910101).
-define(DESKTOP, <<"desktop">>).

a_dm_read_by_replying_on_desktop_is_not_pushed_when_desktop_goes_afk_test() ->
    Pushed = with_desktop_presence(fun(PresencePid, _SessionPid) ->
        dispatch(PresencePid, message_create, dm_message(?PARTNER_MSG, ?PARTNER)),
        ?assertEqual([?PARTNER_MSG], buffered_message_ids(PresencePid)),
        dispatch(PresencePid, message_create, dm_message(?REPLY_MSG, ?USER)),
        ?assertEqual([], push_buffer(PresencePid)),
        gen_server:cast(PresencePid, {presence_update, desktop_request(true)}),
        sync(PresencePid),
        pushed_message_ids([])
    end),
    ?assertEqual([], Pushed).

a_dm_read_by_replying_on_desktop_is_not_pushed_when_desktop_disconnects_test() ->
    Pushed = with_desktop_presence(fun(PresencePid, SessionPid) ->
        dispatch(PresencePid, message_create, dm_message(?PARTNER_MSG, ?PARTNER)),
        dispatch(PresencePid, message_create, dm_message(?REPLY_MSG, ?USER)),
        ?assertEqual([], push_buffer(PresencePid)),
        MRef = monitor(process, PresencePid),
        SessionPid ! stop,
        receive
            {'DOWN', MRef, process, PresencePid, normal} -> ok
        after 2000 ->
            ?assert(false)
        end,
        pushed_message_ids([])
    end),
    ?assertEqual([], Pushed).

a_reply_keeps_buffered_dms_newer_than_the_reply_and_in_other_channels_test() ->
    Buffered = with_desktop_presence(fun(PresencePid, _SessionPid) ->
        dispatch(PresencePid, message_create, dm_message(?PARTNER_MSG, ?PARTNER)),
        dispatch(PresencePid, message_create, (dm_message(?PARTNER_MSG + 5, ?PARTNER))#{
            <<"channel_id">> => integer_to_binary(?DM + 1)
        }),
        dispatch(PresencePid, message_create, dm_message(?REPLY_MSG + 5, ?PARTNER)),
        dispatch(PresencePid, message_create, dm_message(?REPLY_MSG, ?USER)),
        lists:sort(buffered_message_ids(PresencePid))
    end),
    ?assertEqual([?PARTNER_MSG + 5, ?REPLY_MSG + 5], Buffered).

an_unread_dm_is_pushed_with_its_buffer_time_when_desktop_goes_afk_test() ->
    Self = self(),
    Before = erlang:system_time(millisecond),
    with_desktop_presence(fun(PresencePid, _SessionPid) ->
        ok = meck:expect(push, handle_buffered_message_creates, fun(ParamsList) ->
            lists:foreach(fun(Params) -> Self ! {pushed_params, Params} end, ParamsList)
        end),
        dispatch(PresencePid, message_create, dm_message(?PARTNER_MSG, ?PARTNER)),
        gen_server:cast(PresencePid, {presence_update, desktop_request(true)}),
        sync(PresencePid)
    end),
    receive
        {pushed_params, #{message_data := MessageData, buffered_at := BufferedAt}} ->
            ?assertEqual(integer_to_binary(?PARTNER_MSG), maps:get(<<"id">>, MessageData)),
            ?assert(BufferedAt >= Before),
            ?assert(BufferedAt =< erlang:system_time(millisecond))
    after 0 ->
        erlang:error(nothing_pushed)
    end.

a_dm_older_than_the_reply_arriving_after_it_is_not_pushed_test() ->
    Pushed = with_desktop_presence(fun(PresencePid, _SessionPid) ->
        dispatch(PresencePid, message_create, dm_message(?REPLY_MSG, ?USER)),
        dispatch(PresencePid, message_create, dm_message(?PARTNER_MSG, ?PARTNER)),
        ?assertEqual([], push_buffer(PresencePid)),
        gen_server:cast(PresencePid, {presence_update, desktop_request(true)}),
        sync(PresencePid),
        dispatch(PresencePid, message_create, dm_message(?PARTNER_MSG - 1, ?PARTNER)),
        dispatch(PresencePid, message_create, dm_message(?REPLY_MSG + 1, ?PARTNER)),
        pushed_message_ids([])
    end),
    ?assertEqual([?REPLY_MSG + 1], Pushed).

a_buffered_dm_without_a_buffer_time_is_still_pushed_test() ->
    Pushed = with_desktop_presence(fun(PresencePid, _SessionPid) ->
        sys:replace_state(PresencePid, fun(State) ->
            State#{
                push_buffer := [
                    #{
                        channel_id => ?DM,
                        message_id => ?PARTNER_MSG,
                        params => #{message_data => dm_message(?PARTNER_MSG, ?PARTNER)}
                    }
                ]
            }
        end),
        gen_server:cast(PresencePid, {presence_update, desktop_request(true)}),
        sync(PresencePid),
        pushed_message_ids([])
    end),
    ?assertEqual([?PARTNER_MSG], Pushed).

a_message_ack_drops_the_buffered_dm_push_test() ->
    Pushed = with_desktop_presence(fun(PresencePid, _SessionPid) ->
        dispatch(PresencePid, message_create, dm_message(?PARTNER_MSG, ?PARTNER)),
        dispatch(PresencePid, message_ack, #{
            <<"channel_id">> => integer_to_binary(?DM),
            <<"message_id">> => integer_to_binary(?PARTNER_MSG)
        }),
        ?assertEqual([], push_buffer(PresencePid)),
        gen_server:cast(PresencePid, {presence_update, desktop_request(true)}),
        sync(PresencePid),
        pushed_message_ids([])
    end),
    ?assertEqual([], Pushed).

a_flush_hands_every_buffered_dm_to_push_in_one_batch_test() ->
    Self = self(),
    with_desktop_presence(fun(PresencePid, _SessionPid) ->
        ok = meck:expect(push, handle_buffered_message_creates, fun(ParamsList) ->
            Self ! {batch, [maps:get(<<"id">>, maps:get(message_data, P)) || P <- ParamsList]},
            ok
        end),
        dispatch(PresencePid, message_create, dm_message(?PARTNER_MSG, ?PARTNER)),
        dispatch(PresencePid, message_create, dm_message(?PARTNER_MSG + 1, ?PARTNER)),
        gen_server:cast(PresencePid, {presence_update, desktop_request(true)}),
        sync(PresencePid)
    end),
    receive
        {batch, Ids} ->
            ?assertEqual(
                [integer_to_binary(?PARTNER_MSG), integer_to_binary(?PARTNER_MSG + 1)], Ids
            )
    after 0 ->
        erlang:error(no_batch)
    end,
    receive
        {batch, _} -> erlang:error(second_batch)
    after 0 -> ok
    end.

read_marks_are_capped_at_32_channels_dropping_the_oldest_test() ->
    Marks = with_desktop_presence(fun(PresencePid, _SessionPid) ->
        lists:foreach(
            fun(N) -> ack(PresencePid, ?DM + N, ?PARTNER_MSG + N) end,
            lists:seq(1, 33)
        ),
        maps:get(push_read_marks, sys:get_state(PresencePid))
    end),
    ?assertEqual(32, map_size(Marks)),
    ?assertNot(maps:is_key(?DM + 1, Marks)),
    ?assertEqual(?PARTNER_MSG + 2, maps:get(?DM + 2, Marks)),
    ?assertEqual(?PARTNER_MSG + 33, maps:get(?DM + 33, Marks)).

a_lower_ack_after_a_reply_does_not_lower_the_read_mark_test() ->
    Pushed = with_desktop_presence(fun(PresencePid, _SessionPid) ->
        dispatch(PresencePid, message_create, dm_message(?REPLY_MSG, ?USER)),
        ack(PresencePid, ?DM, ?PARTNER_MSG - 10),
        dispatch(PresencePid, message_create, dm_message(?PARTNER_MSG, ?PARTNER)),
        gen_server:cast(PresencePid, {presence_update, desktop_request(true)}),
        sync(PresencePid),
        pushed_message_ids([])
    end),
    ?assertEqual([], Pushed).

a_create_arriving_after_its_own_ack_is_not_pushed_test() ->
    Pushed = with_desktop_presence(fun(PresencePid, _SessionPid) ->
        ack(PresencePid, ?DM, ?PARTNER_MSG),
        dispatch(PresencePid, message_create, dm_message(?PARTNER_MSG, ?PARTNER)),
        gen_server:cast(PresencePid, {presence_update, desktop_request(true)}),
        sync(PresencePid),
        pushed_message_ids([])
    end),
    ?assertEqual([], Pushed).

stale_read_state_fetches_are_capped_per_node_and_fail_open_past_the_cap_test() ->
    Flushes = 100,
    {Fetchers, Published, InFlight, Late} = with_stale_read_state_mocks(fun() ->
        lists:foreach(
            fun(N) ->
                spawn(fun() ->
                    push:handle_buffered_message_creates([stale_params(?USER + N)])
                end)
            end,
            lists:seq(1, Flushes)
        ),
        {Blocked, Immediate} = collect_read_state_traffic(Flushes, [], 0),
        Held = read_state_fetches_in_flight(),
        lists:foreach(fun(Pid) -> Pid ! release end, Blocked),
        wait_for_released_fetch_slots(50),
        {Blocked, Immediate, Held, collect_published(0)}
    end),
    ?assertEqual(64, length(Fetchers)),
    ?assertEqual(64, InFlight),
    ?assertEqual(Flushes - 64, Published),
    ?assertEqual(0, Late),
    ?assertEqual(0, read_state_fetches_in_flight()).

with_stale_read_state_mocks(Fun) ->
    Self = self(),
    ok = push_worker_pool:init_counter(),
    true = ets:delete(push_worker_counter, read_state_fetches_in_flight),
    ok = meck:new(fluxer_gateway_env, [passthrough, no_link]),
    ok = meck:new(gateway_node_router, [passthrough, no_link]),
    ok = meck:new(rpc_client, [passthrough, no_link]),
    try
        ok = meck:expect(fluxer_gateway_env, get, fun
            (push_enabled) -> true;
            (Key) -> meck:passthrough([Key])
        end),
        ok = meck:expect(gateway_node_router, owner_node_result, fun(_Key, push) ->
            Self ! published,
            {error, test}
        end),
        ok = meck:expect(rpc_client, call, fun(_Request, _Timeout) ->
            Self ! {fetching, self()},
            receive
                release -> {ok, #{<<"last_message_id">> => integer_to_binary(?PARTNER_MSG)}}
            after 5000 ->
                {error, timeout}
            end
        end),
        Fun()
    after
        meck:unload(rpc_client),
        meck:unload(gateway_node_router),
        meck:unload(fluxer_gateway_env)
    end.

stale_params(UserId) ->
    #{
        user_ids => [UserId],
        guild_id => 0,
        author_id => ?PARTNER,
        message_data => dm_message(?PARTNER_MSG, ?PARTNER),
        buffered_at => erlang:system_time(millisecond) - push_outbox:max_age_ms() - 1000
    }.

collect_read_state_traffic(Expected, Fetchers, Published) when
    length(Fetchers) + Published >= Expected
->
    {Fetchers, Published};
collect_read_state_traffic(Expected, Fetchers, Published) ->
    receive
        {fetching, Pid} -> collect_read_state_traffic(Expected, [Pid | Fetchers], Published);
        published -> collect_read_state_traffic(Expected, Fetchers, Published + 1)
    after 2000 ->
        {Fetchers, Published}
    end.

collect_published(Count) ->
    receive
        published -> collect_published(Count + 1)
    after 200 ->
        Count
    end.

wait_for_released_fetch_slots(0) ->
    ok;
wait_for_released_fetch_slots(Attempts) ->
    case read_state_fetches_in_flight() of
        0 ->
            ok;
        _ ->
            timer:sleep(20),
            wait_for_released_fetch_slots(Attempts - 1)
    end.

read_state_fetches_in_flight() ->
    case ets:lookup(push_worker_counter, read_state_fetches_in_flight) of
        [{_, InFlight}] -> InFlight;
        [] -> 0
    end.

an_outbox_read_watermark_filters_a_later_job_for_the_read_message_test() ->
    {Sent, Stats} = with_outbox(60000, fun() ->
        ok = push_outbox:truncate_read(?USER, ?DM, ?PARTNER_MSG),
        sync(whereis(push_outbox)),
        ok = push_outbox:enqueue(outbox_job())
    end),
    ?assertEqual([], Sent),
    ?assertEqual(1, maps:get(truncations, Stats)).

an_outbox_forgets_a_read_once_the_watermark_is_pruned_test() ->
    {Sent, Stats} = with_outbox(50, fun() ->
        ok = push_outbox:truncate_read(?USER, ?DM, ?PARTNER_MSG),
        Outbox = whereis(push_outbox),
        sync(Outbox),
        timer:sleep(120),
        Outbox ! prune,
        sync(Outbox),
        ok = push_outbox:enqueue(outbox_job())
    end),
    ?assertEqual([<<"rpc.push.message">>], Sent),
    ?assertEqual(0, maps:get(truncations, Stats)).

with_desktop_presence(Fun) ->
    maybe_start(presence_bus),
    maybe_start(presence_cache),
    Self = self(),
    ok = meck:new(push, [passthrough, no_link]),
    try
        ok = meck:expect(push, handle_message_create, fun(Params) ->
            Self ! {pushed, maps:get(message_data, Params)},
            ok
        end),
        ok = meck:expect(push, handle_buffered_message_creates, fun(ParamsList) ->
            lists:foreach(
                fun(Params) -> Self ! {pushed, maps:get(message_data, Params)} end, ParamsList
            )
        end),
        {ok, PresencePid} = presence:start_link(presence_data()),
        unlink(PresencePid),
        SessionPid = start_session(PresencePid),
        try
            Fun(PresencePid, SessionPid)
        after
            SessionPid ! stop,
            stop_quietly(PresencePid),
            _ = pushed_message_ids([])
        end
    after
        meck:unload(push)
    end.

with_outbox(MaxAgeMs, Fun) ->
    Self = self(),
    undefined = whereis(push_outbox),
    ok = meck:new(fluxer_gateway_env, [passthrough, no_link]),
    ok = meck:new(gateway_node_router, [passthrough, no_link]),
    ok = meck:new(push_job_publisher, [passthrough, no_link]),
    try
        ok = meck:expect(fluxer_gateway_env, get_optional, fun
            (push_outbox_max_age_ms) -> MaxAgeMs;
            (Key) -> meck:passthrough([Key])
        end),
        ok = meck:expect(gateway_node_router, active_nodes, fun(push) -> [node()] end),
        ok = meck:expect(push_job_publisher, request, fun(Subject, _Body, _Timeout) ->
            Self ! {sent, Subject},
            ok
        end),
        {ok, Outbox} = push_outbox:start_link(),
        unlink(Outbox),
        try
            Fun(),
            timer:sleep(100),
            Stats = push_outbox:stats(),
            {sent_subjects([]), Stats}
        after
            stop_quietly(Outbox)
        end
    after
        meck:unload(push_job_publisher),
        meck:unload(gateway_node_router),
        meck:unload(fluxer_gateway_env)
    end.

dispatch(PresencePid, Event, Data) ->
    gen_server:cast(PresencePid, {dispatch, Event, Data}),
    sync(PresencePid).

ack(PresencePid, ChannelId, MessageId) ->
    dispatch(PresencePid, message_ack, #{
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"message_id">> => integer_to_binary(MessageId)
    }).

push_buffer(PresencePid) ->
    maps:get(push_buffer, sys:get_state(PresencePid)).

buffered_message_ids(PresencePid) ->
    [maps:get(message_id, Entry) || Entry <- push_buffer(PresencePid)].

sync(Pid) ->
    _ = sys:get_state(Pid),
    ok.

pushed_message_ids(Acc) ->
    receive
        {pushed, MessageData} ->
            Id = binary_to_integer(maps:get(<<"id">>, MessageData)),
            pushed_message_ids([Id | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

sent_subjects(Acc) ->
    receive
        {sent, Subject} -> sent_subjects([Subject | Acc])
    after 0 ->
        lists:reverse(Acc)
    end.

dm_message(MessageId, AuthorId) ->
    #{
        <<"id">> => integer_to_binary(MessageId),
        <<"channel_id">> => integer_to_binary(?DM),
        <<"channel_type">> => 1,
        <<"author">> => #{<<"id">> => integer_to_binary(AuthorId)},
        <<"content">> => <<"hi">>
    }.

outbox_job() ->
    Job = #{<<"user_ids">> => [integer_to_binary(?USER)]},
    #{
        kind => message,
        subject => <<"rpc.push.message">>,
        job => Job,
        body => iolist_to_binary(json:encode(Job)),
        user_ids => [?USER],
        channel_id => ?DM,
        message_id => ?PARTNER_MSG
    }.

desktop_request(Afk) ->
    #{session_id => ?DESKTOP, status => online, afk => Afk, mobile => false}.

start_session(PresencePid) ->
    Parent = self(),
    Pid = spawn(fun() ->
        Reply = gen_server:call(
            PresencePid,
            {session_connect, #{
                session_id => ?DESKTOP,
                status => online,
                afk => false,
                mobile => false,
                socket_pid => undefined
            }},
            5000
        ),
        Parent ! {session_connected, self(), Reply},
        session_loop()
    end),
    receive
        {session_connected, Pid, {ok, _Sessions}} -> Pid
    after 2000 ->
        erlang:error(session_connect_timeout)
    end.

session_loop() ->
    receive
        stop -> ok;
        _ -> session_loop()
    end.

stop_quietly(Pid) ->
    try gen_server:stop(Pid) of
        ok -> ok
    catch
        exit:_ -> ok
    end.

maybe_start(Name) ->
    case whereis(Name) of
        undefined ->
            case Name:start_link() of
                {ok, Pid} ->
                    unlink(Pid),
                    ok;
                {error, {already_started, _Pid}} ->
                    ok
            end;
        _ ->
            ok
    end.

presence_data() ->
    #{
        user_id => ?USER,
        user_data => #{
            <<"id">> => integer_to_binary(?USER),
            <<"username">> => <<"test">>,
            <<"discriminator">> => <<"0001">>,
            <<"avatar">> => null,
            <<"flags">> => 0
        },
        guild_ids => [],
        friend_ids => [],
        group_dm_recipients => #{},
        status => online,
        custom_status => null
    }.
