%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_mailbox_age).
-typing([eqwalizer]).

-export([note/0, handle_mark/1, min_age_ms/0]).

-define(KEY, {?MODULE, marks}).
-define(MARK_INTERVAL_MS, 50).

-type seq() :: non_neg_integer().
-type marks() :: {seq(), integer() | undefined, queue:queue({seq(), integer()})}.

-spec note() -> ok.
note() ->
    Now = erlang:monotonic_time(millisecond),
    {Seq, Checked, Pending} = marks(),
    case is_integer(Checked) andalso Now - Checked < ?MARK_INTERVAL_MS of
        true -> ok;
        false -> maybe_mark(Now, Seq, Pending)
    end.

-spec maybe_mark(integer(), seq(), queue:queue({seq(), integer()})) -> ok.
maybe_mark(Now, Seq, Pending) ->
    case erlang:process_info(self(), message_queue_len) of
        {message_queue_len, Len} when Len > 0 ->
            self() ! {?MODULE, Seq},
            SentBy = erlang:monotonic_time(millisecond) + 1,
            put_marks({Seq + 1, Now, queue:in({Seq, SentBy}, Pending)});
        _ ->
            put_marks({Seq, Now, Pending})
    end.

-spec handle_mark(seq()) -> ok.
handle_mark(Seq) ->
    {Next, Checked, Pending} = marks(),
    put_marks({Next, Checked, drop_through(Seq, Pending)}).

-spec min_age_ms() -> non_neg_integer() | undefined.
min_age_ms() ->
    {_, _, Pending} = marks(),
    case queue:peek(Pending) of
        {value, {_, SentBy}} when is_integer(SentBy) ->
            max(0, erlang:monotonic_time(millisecond) - SentBy);
        _ ->
            undefined
    end.

-spec drop_through(seq(), queue:queue({seq(), integer()})) -> queue:queue({seq(), integer()}).
drop_through(Seq, Pending) ->
    case queue:peek(Pending) of
        {value, {S, _}} when S =< Seq -> drop_through(Seq, queue:drop(Pending));
        _ -> Pending
    end.

-spec marks() -> marks().
marks() ->
    case erlang:get(?KEY) of
        {Seq, _, _} = Marks when is_integer(Seq) ->
            Marks;
        _ ->
            {0, undefined, queue:new()}
    end.

-spec put_marks(marks()) -> ok.
put_marks(Marks) ->
    _ = erlang:put(?KEY, Marks),
    ok.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

in_fresh_process(Fun) ->
    Self = self(),
    Ref = make_ref(),
    {Pid, MRef} = spawn_monitor(fun() -> Self ! {Ref, Fun()} end),
    receive
        {Ref, Result} ->
            erlang:demonitor(MRef, [flush]),
            Result;
        {'DOWN', MRef, process, Pid, Reason} ->
            erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.

next_mark() ->
    receive
        {?MODULE, Seq} -> Seq
    after 0 -> none
    end.

no_age_without_a_backlog_test() ->
    ?assertEqual(
        {undefined, none},
        in_fresh_process(fun() ->
            ok = note(),
            {min_age_ms(), next_mark()}
        end)
    ).

a_message_queued_before_a_mark_is_at_least_as_old_as_the_mark_test() ->
    {Age, Seq} = in_fresh_process(fun() ->
        self() ! queued_request,
        ok = note(),
        timer:sleep(120),
        receive
            queued_request -> ok
        end,
        {min_age_ms(), next_mark()}
    end),
    ?assertEqual(0, Seq),
    ?assert(Age >= 119),
    ?assert(Age =< 1000).

handled_marks_stop_bounding_later_messages_test() ->
    ?assertEqual(
        undefined,
        in_fresh_process(fun() ->
            self() ! queued_request,
            ok = note(),
            receive
                queued_request -> ok
            end,
            ok = handle_mark(next_mark()),
            min_age_ms()
        end)
    ).

marks_are_rate_limited_while_busy_test() ->
    Seqs = in_fresh_process(fun() ->
        self() ! queued_request,
        ok = note(),
        ok = note(),
        timer:sleep(60),
        ok = note(),
        [next_mark(), next_mark(), next_mark()]
    end),
    ?assertEqual([0, 1, none], Seqs).

the_oldest_unhandled_mark_bounds_the_age_test() ->
    {Age, Seqs} = in_fresh_process(fun() ->
        self() ! queued_request,
        ok = note(),
        timer:sleep(200),
        ok = note(),
        receive
            queued_request -> ok
        end,
        First = next_mark(),
        Second = next_mark(),
        {min_age_ms(), [First, Second]}
    end),
    ?assertEqual([0, 1], Seqs),
    ?assert(Age >= 199).

a_forged_mark_only_loosens_the_bound_test() ->
    ?assertEqual(
        undefined,
        in_fresh_process(fun() ->
            self() ! queued_request,
            ok = note(),
            ok = handle_mark(1000),
            min_age_ms()
        end)
    ).

-endif.
