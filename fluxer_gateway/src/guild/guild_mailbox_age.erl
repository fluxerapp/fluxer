%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_mailbox_age).
-typing([eqwalizer]).

-export([note/0, handle_mark/1, min_age_ms/0]).
-export_type([seq/0]).

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
