%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_outbox_read_tests).
-include_lib("eunit/include/eunit.hrl").

-define(USER, 10).
-define(OTHER, 11).
-define(CHANNEL, 5).
-define(MESSAGE, 100).

a_read_during_an_in_flight_message_job_clears_again_after_it_completes_test() ->
    with_outbox(#{}, fun() ->
        ok = push_outbox:enqueue(message_job([?USER], ?CHANNEL, ?MESSAGE)),
        {MessageWorker, _MessageBody} = await_request(<<"push.job.message">>),
        ok = push_outbox:truncate_read(?USER, ?CHANNEL, ?MESSAGE),
        ok = push_job_publisher:publish_clear(?USER, ?CHANNEL, ?MESSAGE),
        {ClearWorker, ClearBody} = await_request(<<"push.job.clear">>),
        ?assertNot(maps:is_key(<<"after_message_id">>, json:decode(ClearBody))),
        ClearWorker ! release,
        _ = await_delivered(1),
        ?assertEqual(nothing, next_request(100)),
        MessageWorker ! release,
        {FollowupWorker, FollowupBody} = await_request(<<"push.job.clear">>),
        ?assertEqual(
            followup_fields(?USER, ?CHANNEL, ?MESSAGE, ?MESSAGE),
            clear_body_fields(FollowupBody)
        ),
        FollowupWorker ! release,
        Stats = await_delivered(3),
        ?assertEqual(1, maps:get(followup_clears, Stats)),
        ?assertEqual(nothing, next_request(100))
    end).

reads_during_an_in_flight_job_coalesce_into_one_clear_at_the_highest_read_test() ->
    with_outbox(#{}, fun() ->
        ok = push_outbox:enqueue(message_job([?USER, ?OTHER], ?CHANNEL, ?MESSAGE)),
        {MessageWorker, _} = await_request(<<"push.job.message">>),
        ok = push_outbox:truncate_read(?USER, ?CHANNEL, ?MESSAGE),
        ok = push_outbox:truncate_read(?USER, ?CHANNEL, ?MESSAGE + 5),
        ok = push_outbox:truncate_read(?USER, ?CHANNEL + 1, ?MESSAGE + 5),
        ok = push_outbox:truncate_read(?OTHER, ?CHANNEL, ?MESSAGE - 1),
        MessageWorker ! release,
        {FollowupWorker, FollowupBody} = await_request(<<"push.job.clear">>),
        ?assertEqual(
            followup_fields(?USER, ?CHANNEL, ?MESSAGE + 5, ?MESSAGE),
            clear_body_fields(FollowupBody)
        ),
        FollowupWorker ! release,
        Stats = await_delivered(2),
        ?assertEqual(1, maps:get(followup_clears, Stats)),
        ?assertEqual(nothing, next_request(100))
    end).

a_failed_in_flight_job_clears_the_reader_and_retries_without_them_test() ->
    with_outbox(#{}, fun() ->
        ok = push_outbox:enqueue(message_job([?USER, ?OTHER], ?CHANNEL, ?MESSAGE)),
        {MessageWorker, _} = await_request(<<"push.job.message">>),
        ok = push_outbox:truncate_read(?USER, ?CHANNEL, ?MESSAGE),
        MessageWorker ! fail,
        {FollowupWorker, FollowupBody} = await_request(<<"push.job.clear">>),
        ?assertEqual(
            followup_fields(?USER, ?CHANNEL, ?MESSAGE, ?MESSAGE),
            clear_body_fields(FollowupBody)
        ),
        FollowupWorker ! release,
        {RetryWorker, RetryBody} = await_request(<<"push.job.message">>),
        ?assertEqual([integer_to_binary(?OTHER)], body_user_ids(RetryBody)),
        RetryWorker ! release,
        Stats = await_delivered(2),
        ?assertEqual(1, maps:get(retries, Stats)),
        ?assertEqual(1, maps:get(truncations, Stats)),
        ?assertEqual(nothing, next_request(100))
    end).

a_read_across_in_flight_jobs_of_one_channel_clears_once_after_the_last_completes_test() ->
    with_outbox(#{}, fun() ->
        ok = push_outbox:enqueue(message_job([?USER], ?CHANNEL, ?MESSAGE)),
        {FirstWorker, _} = await_request(<<"push.job.message">>),
        ok = push_outbox:enqueue(message_job([?USER, ?OTHER], ?CHANNEL, ?MESSAGE + 1)),
        {SecondWorker, _} = await_request(<<"push.job.message">>),
        ok = push_outbox:truncate_read(?USER, ?CHANNEL, ?MESSAGE + 1),
        FirstWorker ! release,
        _ = await_delivered(1),
        ?assertEqual(nothing, next_request(100)),
        SecondWorker ! release,
        {FollowupWorker, FollowupBody} = await_request(<<"push.job.clear">>),
        ?assertEqual(
            followup_fields(?USER, ?CHANNEL, ?MESSAGE + 1, ?MESSAGE + 1),
            clear_body_fields(FollowupBody)
        ),
        FollowupWorker ! release,
        Stats = await_delivered(3),
        ?assertEqual(1, maps:get(followup_clears, Stats)),
        ?assertEqual(nothing, next_request(100))
    end).

an_outbox_state_from_before_follow_ups_still_follows_up_test() ->
    with_outbox(#{}, fun() ->
        ok = push_outbox:enqueue(message_job([?USER], ?CHANNEL, ?MESSAGE)),
        {MessageWorker, _} = await_request(<<"push.job.message">>),
        _ = sys:replace_state(push_outbox, fun(State) -> maps:remove(followups, State) end),
        ok = push_outbox:truncate_read(?USER, ?CHANNEL, ?MESSAGE),
        MessageWorker ! release,
        {FollowupWorker, _} = await_request(<<"push.job.clear">>),
        FollowupWorker ! release,
        Stats = await_delivered(2),
        ?assertEqual(1, maps:get(followup_clears, Stats))
    end).

a_hot_loaded_outbox_finishes_an_in_flight_job_that_nobody_read_test() ->
    with_outbox(#{}, fun() ->
        ok = push_outbox:enqueue(message_job([?USER], ?CHANNEL, ?MESSAGE)),
        {MessageWorker, _} = await_request(<<"push.job.message">>),
        _ = sys:replace_state(push_outbox, fun(State) -> maps:remove(followups, State) end),
        Outbox = whereis(push_outbox),
        MessageWorker ! release,
        Stats = await_delivered(1),
        ?assertEqual(Outbox, whereis(push_outbox)),
        ?assertEqual(0, maps:get(followup_clears, Stats)),
        ?assertEqual(nothing, next_request(100))
    end).

a_read_in_another_channel_does_not_follow_up_an_in_flight_job_test() ->
    with_outbox(#{}, fun() ->
        ok = push_outbox:enqueue(message_job([?USER], ?CHANNEL, ?MESSAGE)),
        {MessageWorker, _} = await_request(<<"push.job.message">>),
        ok = push_outbox:truncate_read(?USER, ?CHANNEL + 1, ?MESSAGE + 5),
        MessageWorker ! release,
        Stats = await_delivered(1),
        ?assertEqual(0, maps:get(followup_clears, Stats)),
        ?assertEqual(nothing, next_request(100))
    end).

a_read_by_someone_the_job_is_not_for_does_not_follow_up_test() ->
    with_outbox(#{}, fun() ->
        ok = push_outbox:enqueue(message_job([?USER], ?CHANNEL, ?MESSAGE)),
        {MessageWorker, _} = await_request(<<"push.job.message">>),
        ok = push_outbox:truncate_read(?OTHER, ?CHANNEL, ?MESSAGE),
        MessageWorker ! release,
        Stats = await_delivered(1),
        ?assertEqual(0, maps:get(followup_clears, Stats)),
        ?assertEqual(nothing, next_request(100))
    end).

reads_during_in_flight_jobs_in_two_channels_clear_each_channel_test() ->
    with_outbox(#{}, fun() ->
        ok = push_outbox:enqueue(message_job([?USER], ?CHANNEL, ?MESSAGE)),
        {FirstWorker, _} = await_request(<<"push.job.message">>),
        ok = push_outbox:enqueue(message_job([?USER], ?CHANNEL + 1, ?MESSAGE + 50)),
        {SecondWorker, _} = await_request(<<"push.job.message">>),
        ok = push_outbox:truncate_read(?USER, ?CHANNEL, ?MESSAGE),
        ok = push_outbox:truncate_read(?USER, ?CHANNEL + 1, ?MESSAGE + 50),
        FirstWorker ! release,
        {FirstFollowup, FirstBody} = await_request(<<"push.job.clear">>),
        ?assertEqual(
            followup_fields(?USER, ?CHANNEL, ?MESSAGE, ?MESSAGE),
            clear_body_fields(FirstBody)
        ),
        FirstFollowup ! release,
        SecondWorker ! release,
        {SecondFollowup, SecondBody} = await_request(<<"push.job.clear">>),
        ?assertEqual(
            followup_fields(?USER, ?CHANNEL + 1, ?MESSAGE + 50, ?MESSAGE + 50),
            clear_body_fields(SecondBody)
        ),
        SecondFollowup ! release,
        Stats = await_delivered(4),
        ?assertEqual(2, maps:get(followup_clears, Stats))
    end).

reads_arriving_out_of_order_still_clear_at_the_highest_read_test() ->
    with_outbox(#{}, fun() ->
        ok = push_outbox:enqueue(message_job([?USER], ?CHANNEL, ?MESSAGE)),
        {MessageWorker, _} = await_request(<<"push.job.message">>),
        ok = push_outbox:truncate_read(?USER, ?CHANNEL, ?MESSAGE + 5),
        ok = push_outbox:truncate_read(?USER, ?CHANNEL, ?MESSAGE),
        MessageWorker ! release,
        {FollowupWorker, FollowupBody} = await_request(<<"push.job.clear">>),
        ?assertEqual(
            followup_fields(?USER, ?CHANNEL, ?MESSAGE + 5, ?MESSAGE),
            clear_body_fields(FollowupBody)
        ),
        FollowupWorker ! release,
        _ = await_delivered(2)
    end).

no_follow_up_clear_is_sent_while_clears_are_disabled_test() ->
    with_outbox(#{push_enrolled_clear_notifications_enabled => false}, fun() ->
        ok = push_outbox:enqueue(message_job([?USER], ?CHANNEL, ?MESSAGE)),
        {MessageWorker, _} = await_request(<<"push.job.message">>),
        ok = push_outbox:truncate_read(?USER, ?CHANNEL, ?MESSAGE),
        MessageWorker ! release,
        Stats = await_delivered(1),
        ?assertEqual(0, maps:get(followup_clears, Stats)),
        ?assertEqual(nothing, next_request(100))
    end).

a_read_removes_the_reader_from_a_message_job_still_queued_test() ->
    with_outbox(#{push_outbox_max_inflight => 1}, fun() ->
        ok = push_outbox:enqueue(message_job([?OTHER], ?CHANNEL + 1, ?MESSAGE)),
        {Blocker, _} = await_request(<<"push.job.message">>),
        ok = push_outbox:enqueue(message_job([?USER, ?OTHER], ?CHANNEL, ?MESSAGE)),
        ok = push_outbox:truncate_read(?USER, ?CHANNEL, ?MESSAGE),
        Queued = push_outbox:stats(),
        ?assertEqual(1, maps:get(truncations, Queued)),
        ?assertEqual(1, maps:get(depth, Queued)),
        Blocker ! release,
        {Worker, Body} = await_request(<<"push.job.message">>),
        ?assertEqual([integer_to_binary(?OTHER)], body_user_ids(Body)),
        Worker ! release,
        Stats = await_delivered(2),
        ?assertEqual(1, maps:get(truncations, Stats))
    end).

a_read_drops_a_queued_message_job_it_was_the_only_reader_of_test() ->
    with_outbox(#{push_outbox_max_inflight => 1}, fun() ->
        ok = push_outbox:enqueue(message_job([?OTHER], ?CHANNEL + 1, ?MESSAGE)),
        {Blocker, _} = await_request(<<"push.job.message">>),
        ok = push_outbox:enqueue(message_job([?USER], ?CHANNEL, ?MESSAGE)),
        ok = push_outbox:truncate_read(?USER, ?CHANNEL, ?MESSAGE),
        ?assertEqual(0, maps:get(depth, push_outbox:stats())),
        Blocker ! release,
        _ = await_delivered(1),
        ?assertEqual(nothing, next_request(200)),
        ?assertEqual(1, maps:get(truncations, push_outbox:stats()))
    end).

with_outbox(Env, Fun) ->
    Self = self(),
    Modules = [fluxer_gateway_env, gateway_node_router, push_job_publisher],
    lists:foreach(fun(Module) -> ok = meck:new(Module, [passthrough, no_link]) end, Modules),
    try
        ok = meck:expect(fluxer_gateway_env, get_optional, fun(Key) ->
            maps:get(Key, Env, undefined)
        end),
        ok = meck:expect(fluxer_gateway_env, get, fun(Key) -> maps:get(Key, Env, undefined) end),
        ok = meck:expect(gateway_node_router, active_nodes, fun(push) -> [node()] end),
        ok = meck:expect(push_job_publisher, request, fun(Subject, Body, _Timeout) ->
            Self ! {request, self(), Subject, Body},
            receive
                release -> ok;
                fail -> {error, rejected}
            end
        end),
        ok = application:set_env(fluxer_gateway, push_outbox_retry_base_ms, 10),
        {ok, Outbox} = push_outbox:start_link(),
        unlink(Outbox),
        try
            Fun()
        after
            gen_server:stop(Outbox)
        end
    after
        ok = application:unset_env(fluxer_gateway, push_outbox_retry_base_ms),
        lists:foreach(fun meck:unload/1, Modules),
        drain_requests()
    end.

message_job(UserIds, ChannelId, MessageId) ->
    Job = #{
        <<"v">> => 1,
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"message_id">> => integer_to_binary(MessageId),
        <<"user_ids">> => [integer_to_binary(UserId) || UserId <- UserIds]
    },
    #{
        kind => message,
        subject => <<"push.job.message">>,
        job => Job,
        body => iolist_to_binary(json:encode(Job)),
        user_ids => UserIds,
        channel_id => ChannelId,
        message_id => MessageId
    }.

await_request(Subject) ->
    receive
        {request, Worker, Subject, Body} -> {Worker, Body}
    after 2000 -> error({no_request, Subject})
    end.

next_request(Timeout) ->
    receive
        {request, _Worker, Subject, Body} -> {Subject, Body}
    after Timeout -> nothing
    end.

await_delivered(Count) ->
    await_delivered(Count, 100).

await_delivered(Count, 0) ->
    error({not_delivered, Count, push_outbox:stats()});
await_delivered(Count, Attempts) ->
    Stats = push_outbox:stats(),
    case maps:get(delivered, Stats) of
        Count ->
            Stats;
        _ ->
            receive
            after 20 -> await_delivered(Count, Attempts - 1)
            end
    end.

followup_fields(UserId, ChannelId, MessageId, AfterMessageId) ->
    #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"message_id">> => integer_to_binary(MessageId),
        <<"after_message_id">> => integer_to_binary(AfterMessageId)
    }.

clear_body_fields(Body) ->
    maps:with(
        [<<"user_id">>, <<"channel_id">>, <<"message_id">>, <<"after_message_id">>],
        json:decode(Body)
    ).

body_user_ids(Body) ->
    maps:get(<<"user_ids">>, json:decode(Body)).

drain_requests() ->
    receive
        {request, Worker, _Subject, _Body} ->
            Worker ! release,
            drain_requests()
    after 0 -> ok
    end.
