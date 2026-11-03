%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_outbox).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([
    start_link/0,
    enqueue/1,
    truncate_read/3,
    note_session_active/1,
    record_dropped/2,
    stats/0,
    request_timeout_ms/0,
    max_age_ms/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export_type([job/0, kind/0]).

-define(DEFAULT_MAX_QUEUE, 10000).
-define(DEFAULT_MAX_INFLIGHT, 64).
-define(DEFAULT_REQUEST_TIMEOUT_MS, 100000).
-define(DEFAULT_MAX_AGE_MS, 300000).
-define(DEFAULT_RETRY_BASE_MS, 1000).
-define(RETRY_MAX_MS, 30000).
-define(ENQUEUE_TIMEOUT_MS, 1000).
-define(STATS_TIMEOUT_MS, 1000).
-define(PRUNE_INTERVAL_MS, 30000).

-type kind() :: message | clear | ring.
-type job() :: #{
    kind := kind(),
    subject := binary(),
    job := map(),
    body := binary(),
    user_ids := [integer()],
    channel_id := integer(),
    message_id := integer()
}.
-type entry() :: #{
    kind := kind(),
    subject := binary(),
    job := map(),
    body := binary(),
    user_ids := [integer()],
    channel_id := integer(),
    message_id := integer(),
    seq := non_neg_integer(),
    enqueued_at := integer(),
    attempts := non_neg_integer()
}.
-type counter() ::
    delivered
    | retries
    | sheds
    | truncations
    | skipped_active
    | followup_clears
    | enqueued.
-type reads() :: #{{integer(), integer()} => {integer(), integer()}}.
-type active() :: #{integer() => {non_neg_integer(), integer()}}.
-type state() :: #{
    jobs := gb_trees:tree(non_neg_integer(), entry()),
    ready := queue:queue(non_neg_integer()),
    inflight := #{pid() => {reference(), reference(), entry()}},
    followups => #{pid() => #{integer() => integer()}},
    next_seq := non_neg_integer(),
    reads := reads(),
    active := active(),
    counters := #{counter() => non_neg_integer()},
    dropped := #{{kind(), atom()} => pos_integer()},
    max_queue := pos_integer(),
    max_inflight := pos_integer(),
    request_timeout_ms := pos_integer(),
    max_age_ms := pos_integer(),
    retry_base_ms := pos_integer()
}.

-spec start_link() -> {ok, pid()} | {error, term()} | ignore.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec enqueue(job()) -> ok | {error, term()}.
enqueue(Job) ->
    try gen_server:call(?MODULE, {enqueue, Job}, ?ENQUEUE_TIMEOUT_MS) of
        ok -> ok
    catch
        exit:{noproc, _} -> {error, outbox_unavailable};
        exit:{Reason, _} -> {error, {outbox_unavailable, Reason}}
    end.

-spec truncate_read(integer(), integer(), integer()) -> ok.
truncate_read(UserId, ChannelId, MessageId) ->
    broadcast({truncate_read, UserId, ChannelId, MessageId}).

-spec note_session_active(integer()) -> ok.
note_session_active(UserId) ->
    broadcast({session_active, UserId}).

-spec record_dropped(kind(), atom()) -> ok.
record_dropped(Kind, Reason) ->
    gen_server:cast(?MODULE, {dropped, Kind, Reason}).

-spec stats() -> map().
stats() ->
    try gen_server:call(?MODULE, stats, ?STATS_TIMEOUT_MS) of
        Stats when is_map(Stats) -> Stats
    catch
        exit:_ -> #{}
    end.

-spec request_timeout_ms() -> pos_integer().
request_timeout_ms() ->
    env_pos_integer(push_outbox_request_timeout_ms, ?DEFAULT_REQUEST_TIMEOUT_MS).

-spec max_age_ms() -> pos_integer().
max_age_ms() ->
    env_pos_integer(push_outbox_max_age_ms, ?DEFAULT_MAX_AGE_MS).

-spec init([]) -> {ok, state()}.
init([]) ->
    erlang:process_flag(fullsweep_after, 10),
    schedule_prune(),
    {ok, #{
        jobs => gb_trees:empty(),
        ready => queue:new(),
        inflight => #{},
        followups => #{},
        next_seq => 0,
        reads => #{},
        active => #{},
        counters => #{},
        dropped => #{},
        max_queue => env_pos_integer(push_outbox_max_queue, ?DEFAULT_MAX_QUEUE),
        max_inflight => env_pos_integer(push_outbox_max_inflight, ?DEFAULT_MAX_INFLIGHT),
        request_timeout_ms => request_timeout_ms(),
        max_age_ms => max_age_ms(),
        retry_base_ms => push_utils:app_pos_integer(
            push_outbox_retry_base_ms, ?DEFAULT_RETRY_BASE_MS
        )
    }}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({enqueue, Job}, _From, State) when
    is_map_key(kind, Job),
    is_map_key(subject, Job),
    is_map_key(body, Job),
    is_map_key(user_ids, Job),
    is_map_key(channel_id, Job),
    is_map_key(message_id, Job)
->
    {reply, ok, pump(admit(eqwalizer:dynamic_cast(Job), State))};
handle_call(stats, _From, State) ->
    {reply, build_stats(State), State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({truncate_read, UserId, ChannelId, MessageId}, State) when
    is_integer(UserId), is_integer(ChannelId), is_integer(MessageId)
->
    {noreply, apply_read(UserId, ChannelId, MessageId, State)};
handle_cast({session_active, UserId}, State) when is_integer(UserId) ->
    {noreply, record_active(UserId, State)};
handle_cast({dropped, Kind, Reason}, State) when is_atom(Kind), is_atom(Reason) ->
    {noreply, count_dropped(eqwalizer:dynamic_cast(Kind), Reason, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({push_outbox_reply, Pid, Result}, State) when is_pid(Pid) ->
    {noreply, pump(finish_worker(Pid, push_outbox_worker:reply_result(Result), State))};
handle_info({'DOWN', _MRef, process, Pid, Reason}, State) when is_pid(Pid) ->
    {noreply, pump(finish_worker(Pid, push_outbox_worker:down_result(Reason), State))};
handle_info({request_deadline, Pid}, #{inflight := Inflight} = State) when is_pid(Pid) ->
    push_outbox_worker:kill_expired(Pid, Inflight),
    {noreply, State};
handle_info({retry, Seq}, State) when is_integer(Seq), Seq >= 0 ->
    {noreply, pump(make_ready(Seq, State))};
handle_info(prune, State) ->
    schedule_prune(),
    {noreply, prune(State)};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec admit(job(), state()) -> state().
admit(Job, #{next_seq := Seq} = State) ->
    Entry = maps:merge(Job, #{seq => Seq, enqueued_at => now_ms(), attempts => 0}),
    State1 = bump(enqueued, 1, State#{next_seq := Seq + 1}),
    shed_to_capacity(insert(Entry, State1)).

-spec insert(entry(), state()) -> state().
insert(#{seq := Seq} = Entry, #{jobs := Jobs, ready := Ready} = State) ->
    State#{jobs := gb_trees:enter(Seq, Entry, Jobs), ready := queue:in(Seq, Ready)}.

-spec shed_to_capacity(state()) -> state().
shed_to_capacity(#{jobs := Jobs, max_queue := MaxQueue} = State) ->
    case gb_trees:size(Jobs) > MaxQueue of
        true ->
            {_Seq, Shed, Rest} = gb_trees:take_smallest(Jobs),
            shed_to_capacity(shed(Shed, State#{jobs := Rest}));
        false ->
            State
    end.

-spec shed(entry(), state()) -> state().
shed(Entry, State) ->
    log_shed(Entry),
    bump(sheds, 1, State).

-spec log_shed(entry()) -> ok.
log_shed(#{kind := Kind, channel_id := ChannelId, message_id := MessageId}) ->
    logger:warning(
        "Push outbox at capacity, dropping the earliest queued job",
        #{kind => Kind, channel_id => ChannelId, message_id => MessageId}
    ).

-spec pump(state()) -> state().
pump(#{inflight := Inflight, max_inflight := MaxInflight} = State) when
    map_size(Inflight) >= MaxInflight
->
    State;
pump(#{ready := Ready, jobs := Jobs} = State) ->
    case queue:out(Ready) of
        {empty, _} ->
            State;
        {{value, Seq}, Rest} ->
            pump(take_ready(gb_trees:lookup(Seq, Jobs), Seq, State#{ready := Rest}))
    end.

-spec take_ready(none | {value, entry()}, non_neg_integer(), state()) -> state().
take_ready(none, _Seq, State) ->
    State;
take_ready({value, Entry}, Seq, #{jobs := Jobs} = State) ->
    dispatch(Entry, State#{jobs := gb_trees:delete(Seq, Jobs)}).

-spec dispatch(entry(), state()) -> state().
dispatch(Entry, State) ->
    case prepare(Entry, State) of
        {skip, State1} -> State1;
        {send, Prepared, State1} -> send_or_drop(Prepared, State1)
    end.

-spec send_or_drop(entry(), state()) -> state().
send_or_drop(Entry, State) ->
    case is_expired(Entry, State) of
        true -> drop_expired(Entry, State);
        false -> start_worker(Entry, State)
    end.

-spec prepare(entry(), state()) -> {skip, state()} | {send, entry(), state()}.
prepare(#{kind := Kind} = Entry, State) when Kind =:= clear; Kind =:= ring ->
    {send, Entry, State};
prepare(#{user_ids := UserIds} = Entry, #{reads := Reads, active := Active} = State) ->
    #{channel_id := ChannelId, message_id := MessageId, seq := Seq} = Entry,
    {Read, Unread} = lists:partition(
        fun(UserId) -> is_read(UserId, ChannelId, MessageId, Reads) end, UserIds
    ),
    {Activated, Kept} = lists:partition(
        fun(UserId) -> became_active(UserId, Seq, Active) end, Unread
    ),
    State1 = bump(skipped_active, length(Activated), bump(truncations, length(Read), State)),
    case Kept of
        [] -> {skip, State1};
        UserIds -> {send, Entry, State1};
        _ -> {send, with_user_ids(Kept, Entry), State1}
    end.

-spec is_read(integer(), integer(), integer(), reads()) -> boolean().
is_read(UserId, ChannelId, MessageId, Reads) ->
    case maps:find({UserId, ChannelId}, Reads) of
        {ok, {ReadMessageId, _At}} -> MessageId =< ReadMessageId;
        error -> false
    end.

-spec became_active(integer(), non_neg_integer(), active()) -> boolean().
became_active(UserId, Seq, Active) ->
    case maps:find(UserId, Active) of
        {ok, {ActiveSeq, _At}} -> ActiveSeq > Seq;
        error -> false
    end.

-spec with_user_ids([integer()], entry()) -> entry().
with_user_ids(UserIds, #{job := Job} = Entry) ->
    Rewritten = Job#{<<"user_ids">> => [integer_to_binary(UserId) || UserId <- UserIds]},
    Entry#{
        user_ids := UserIds,
        job := Rewritten,
        body := iolist_to_binary(json:encode(Rewritten))
    }.

-spec apply_read(integer(), integer(), integer(), state()) -> state().
apply_read(UserId, ChannelId, MessageId, #{reads := Reads, jobs := Jobs} = State) ->
    Key = {UserId, ChannelId},
    Watermark =
        case maps:find(Key, Reads) of
            {ok, {Previous, _At}} -> max(Previous, MessageId);
            error -> MessageId
        end,
    State1 = State#{reads := Reads#{Key => {Watermark, now_ms()}}},
    State2 = lists:foldl(
        fun({Seq, Entry}, Acc) ->
            truncate_entry(Seq, Entry, UserId, ChannelId, MessageId, Acc)
        end,
        State1,
        gb_trees:to_list(Jobs)
    ),
    note_inflight_read(UserId, ChannelId, MessageId, State2).

-spec note_inflight_read(integer(), integer(), integer(), state()) -> state().
note_inflight_read(UserId, ChannelId, MessageId, #{inflight := Inflight} = State) ->
    maps:fold(
        fun(Pid, {_MRef, _TRef, Entry}, Acc) ->
            note_followup(Pid, Entry, UserId, ChannelId, MessageId, Acc)
        end,
        State,
        Inflight
    ).

-spec note_followup(pid(), entry(), integer(), integer(), integer(), state()) -> state().
note_followup(
    Pid,
    #{
        kind := message,
        channel_id := ChannelId,
        message_id := JobMessageId,
        user_ids := UserIds
    },
    UserId,
    ChannelId,
    MessageId,
    State
) when JobMessageId =< MessageId ->
    case lists:member(UserId, UserIds) of
        false -> State;
        true -> record_followup(Pid, UserId, MessageId, State)
    end;
note_followup(_Pid, _Entry, _UserId, _ChannelId, _MessageId, State) ->
    State.

-spec record_followup(pid(), integer(), integer(), state()) -> state().
record_followup(Pid, UserId, MessageId, State) ->
    Followups = maps:get(followups, State, #{}),
    Reads = maps:get(Pid, Followups, #{}),
    Watermark = max(MessageId, maps:get(UserId, Reads, MessageId)),
    State#{followups => Followups#{Pid => Reads#{UserId => Watermark}}}.

-spec truncate_entry(non_neg_integer(), entry(), integer(), integer(), integer(), state()) ->
    state().
truncate_entry(
    Seq,
    #{kind := message, channel_id := ChannelId, message_id := JobMessageId} = Entry,
    UserId,
    ChannelId,
    MessageId,
    State
) when JobMessageId =< MessageId ->
    remove_reader(Seq, Entry, UserId, State);
truncate_entry(_Seq, _Entry, _UserId, _ChannelId, _MessageId, State) ->
    State.

-spec remove_reader(non_neg_integer(), entry(), integer(), state()) -> state().
remove_reader(Seq, #{user_ids := UserIds} = Entry, UserId, State) ->
    case lists:member(UserId, UserIds) of
        false ->
            State;
        true ->
            drop_reader(Seq, lists:delete(UserId, UserIds), Entry, bump(truncations, 1, State))
    end.

-spec drop_reader(non_neg_integer(), [integer()], entry(), state()) -> state().
drop_reader(Seq, [], _Entry, #{jobs := Jobs} = State) ->
    State#{jobs := gb_trees:delete(Seq, Jobs)};
drop_reader(Seq, Remaining, Entry, #{jobs := Jobs} = State) ->
    State#{jobs := gb_trees:update(Seq, with_user_ids(Remaining, Entry), Jobs)}.

-spec record_active(integer(), state()) -> state().
record_active(UserId, #{active := Active, next_seq := Seq} = State) ->
    State#{active := Active#{UserId => {Seq, now_ms()}}, next_seq := Seq + 1}.

-spec start_worker(entry(), state()) -> state().
start_worker(Entry, #{inflight := Inflight, request_timeout_ms := Timeout} = State) ->
    #{subject := Subject, body := Body} = Entry,
    Outbox = self(),
    {Pid, MRef} = spawn_monitor(fun() ->
        Outbox ! {push_outbox_reply, self(), push_outbox_worker:send(Subject, Body, Timeout)}
    end),
    TRef = erlang:send_after(Timeout, Outbox, {request_deadline, Pid}),
    State#{inflight := Inflight#{Pid => {MRef, TRef, Entry}}}.

-spec finish_worker(pid(), ok | {error, term()}, state()) -> state().
finish_worker(Pid, Result, #{inflight := Inflight} = State) ->
    case maps:take(Pid, Inflight) of
        {{MRef, TRef, Entry}, Rest} ->
            erlang:demonitor(MRef, [flush]),
            _ = erlang:cancel_timer(TRef, [{async, true}, {info, false}]),
            Followups = maps:get(followups, State, #{}),
            Reads = maps:get(Pid, Followups, #{}),
            State1 = State#{inflight := Rest, followups => maps:remove(Pid, Followups)},
            follow_up_clears(Entry, Reads, handle_result(Result, Entry, State1));
        error ->
            State
    end.

-spec follow_up_clears(entry(), #{integer() => integer()}, state()) -> state().
follow_up_clears(_Entry, Reads, State) when map_size(Reads) =:= 0 ->
    State;
follow_up_clears(#{channel_id := ChannelId, message_id := AfterMessageId}, Reads, State) ->
    case push:clear_notifications_enabled() of
        true -> follow_up_each_clear(ChannelId, AfterMessageId, Reads, State);
        false -> State
    end.

-spec follow_up_each_clear(integer(), integer(), #{integer() => integer()}, state()) -> state().
follow_up_each_clear(ChannelId, AfterMessageId, Reads, State) ->
    maps:fold(
        fun(UserId, MessageId, Acc) ->
            follow_up_clear(UserId, ChannelId, MessageId, AfterMessageId, Acc)
        end,
        State,
        Reads
    ).

-spec follow_up_clear(integer(), integer(), integer(), integer(), state()) -> state().
follow_up_clear(UserId, ChannelId, MessageId, AfterMessageId, State) ->
    case pending_followup(UserId, ChannelId, State) of
        {ok, Pid} ->
            record_followup(Pid, UserId, MessageId, State);
        none ->
            Job = push_job_publisher:clear_job(UserId, ChannelId, MessageId, AfterMessageId),
            bump(followup_clears, 1, admit(Job, State))
    end.

-spec pending_followup(integer(), integer(), state()) -> {ok, pid()} | none.
pending_followup(UserId, ChannelId, #{inflight := Inflight} = State) ->
    Pending = maps:filter(
        fun(Pid, Reads) ->
            is_map_key(UserId, Reads) andalso
                inflight_channel(Pid, Inflight) =:= {ok, ChannelId}
        end,
        maps:get(followups, State, #{})
    ),
    case maps:keys(Pending) of
        [Pid | _] -> {ok, Pid};
        [] -> none
    end.

-spec inflight_channel(pid(), #{pid() => {reference(), reference(), entry()}}) ->
    {ok, integer()} | error.
inflight_channel(Pid, Inflight) ->
    case maps:find(Pid, Inflight) of
        {ok, {_MRef, _TRef, #{channel_id := ChannelId}}} -> {ok, ChannelId};
        error -> error
    end.

-spec handle_result(ok | {error, term()}, entry(), state()) -> state().
handle_result(ok, _Entry, State) ->
    bump(delivered, 1, State);
handle_result({error, Reason}, Entry, State) ->
    logger:debug("Push outbox request not delivered", #{
        reason => Reason,
        kind => maps:get(kind, Entry),
        message_id => maps:get(message_id, Entry),
        attempts => maps:get(attempts, Entry)
    }),
    case is_expired(Entry, State) of
        true -> drop_prepared(Entry, State);
        false -> schedule_retry(Entry, State)
    end.

-spec drop_prepared(entry(), state()) -> state().
drop_prepared(Entry, State) ->
    case prepare(Entry, State) of
        {skip, State1} -> State1;
        {send, Prepared, State1} -> drop_expired(Prepared, State1)
    end.

-spec schedule_retry(entry(), state()) -> state().
schedule_retry(#{seq := Seq, attempts := Attempts} = Entry, #{jobs := Jobs} = State) ->
    NextAttempts = Attempts + 1,
    _ = erlang:send_after(retry_delay(NextAttempts, State), self(), {retry, Seq}),
    State1 = State#{jobs := gb_trees:enter(Seq, Entry#{attempts := NextAttempts}, Jobs)},
    shed_to_capacity(bump(retries, 1, State1)).

-spec retry_delay(pos_integer(), state()) -> pos_integer().
retry_delay(Attempts, #{retry_base_ms := Base}) ->
    Delay = min(?RETRY_MAX_MS, Base bsl min(Attempts - 1, 16)),
    Delay + rand:uniform(max(1, Delay div 4)).

-spec make_ready(non_neg_integer(), state()) -> state().
make_ready(Seq, #{jobs := Jobs, ready := Ready} = State) ->
    case gb_trees:is_defined(Seq, Jobs) of
        true -> State#{ready := queue:in(Seq, Ready)};
        false -> State
    end.

-spec is_expired(entry(), state()) -> boolean().
is_expired(#{enqueued_at := EnqueuedAt}, #{max_age_ms := MaxAge}) ->
    now_ms() - EnqueuedAt >= MaxAge.

-spec drop_expired(entry(), state()) -> state().
drop_expired(#{kind := Kind, user_ids := UserIds} = Entry, State) ->
    logger:warning("Push outbox job expired undelivered, dropping it", #{
        kind => Kind,
        channel_id => maps:get(channel_id, Entry),
        message_id => maps:get(message_id, Entry),
        attempts => maps:get(attempts, Entry),
        user_count => length(UserIds)
    }),
    count_dropped(Kind, expired, State).

-spec count_dropped(kind(), atom(), state()) -> state().
count_dropped(Kind, Reason, #{dropped := Dropped} = State) ->
    Key = {Kind, Reason},
    State#{dropped := Dropped#{Key => maps:get(Key, Dropped, 0) + 1}}.

-spec prune(state()) -> state().
prune(#{reads := Reads, active := Active, max_age_ms := MaxAge} = State) ->
    Cutoff = now_ms() - MaxAge,
    State#{
        reads := maps:filter(fun(_Key, {_MessageId, At}) -> At >= Cutoff end, Reads),
        active := maps:filter(fun(_UserId, {_Seq, At}) -> At >= Cutoff end, Active)
    }.

-spec build_stats(state()) -> map().
build_stats(#{jobs := Jobs, inflight := Inflight, counters := Counters, dropped := Dropped}) ->
    maps:merge(
        #{
            delivered => 0,
            retries => 0,
            sheds => 0,
            truncations => 0,
            skipped_active => 0,
            followup_clears => 0,
            enqueued => 0
        },
        Counters#{
            depth => gb_trees:size(Jobs),
            inflight => map_size(Inflight),
            dropped => Dropped
        }
    ).

-spec bump(counter(), non_neg_integer(), state()) -> state().
bump(_Counter, 0, State) ->
    State;
bump(Counter, Increment, #{counters := Counters} = State) ->
    State#{counters := Counters#{Counter => maps:get(Counter, Counters, 0) + Increment}}.

-spec broadcast(term()) -> ok.
broadcast(Msg) ->
    abcast = gen_server:abcast(push_nodes(), ?MODULE, Msg),
    ok.

-spec push_nodes() -> [node()].
push_nodes() ->
    try gateway_node_router:active_nodes(push) of
        Nodes -> Nodes
    catch
        _:_ -> [node()]
    end.

-spec schedule_prune() -> reference().
schedule_prune() ->
    erlang:send_after(?PRUNE_INTERVAL_MS, self(), prune).

-spec now_ms() -> integer().
now_ms() ->
    erlang:monotonic_time(millisecond).

-spec env_pos_integer(atom(), pos_integer()) -> pos_integer().
env_pos_integer(Key, Default) ->
    case fluxer_gateway_env:get_optional(Key) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> Default
    end.
