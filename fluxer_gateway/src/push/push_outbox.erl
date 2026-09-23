%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_outbox).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([
    start_link/0,
    enqueue/1,
    truncate_read/3,
    note_session_active/1,
    delivery_config_changed/0,
    stats/0,
    request_timeout_ms/0
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export_type([job/0, fallback/0]).

-define(DEFAULT_MAX_QUEUE, 10000).
-define(DEFAULT_MAX_INFLIGHT, 64).
-define(DEFAULT_REQUEST_TIMEOUT_MS, 100000).
-define(DEFAULT_MAX_AGE_MS, 300000).
-define(DEFAULT_RETRY_BASE_MS, 1000).
-define(DEFAULT_MAX_FALLBACK_RUNNERS, 16).
-define(RETRY_MAX_MS, 30000).
-define(ENQUEUE_TIMEOUT_MS, 1000).
-define(STATS_TIMEOUT_MS, 1000).
-define(PRUNE_INTERVAL_MS, 30000).

-type kind() :: message | clear | ring.
-type fallback() :: fun(([integer()]) -> term()).
-type job() :: #{
    kind := kind(),
    subject := binary(),
    job := map(),
    body := binary(),
    user_ids := [integer()],
    channel_id := integer(),
    message_id := integer(),
    fallback := fallback()
}.
-type entry() :: #{
    kind := kind(),
    subject := binary(),
    job := map(),
    body := binary(),
    user_ids := [integer()],
    channel_id := integer(),
    message_id := integer(),
    fallback := fallback(),
    seq := non_neg_integer(),
    enqueued_at := integer(),
    attempts := non_neg_integer(),
    config_version := non_neg_integer() | undefined
}.
-type settled() :: {keep, entry(), state()} | {none, state()}.
-type counter() ::
    delivered
    | retries
    | sheds
    | truncations
    | skipped_active
    | fallbacks
    | lost
    | enqueued.
-type state() :: #{
    jobs := gb_trees:tree(non_neg_integer(), entry()),
    ready := queue:queue(non_neg_integer()),
    inflight := #{pid() => {reference(), reference(), entry()}},
    fallback_backlog := queue:queue({fallback(), [integer()]}),
    fallback_runners := #{pid() => reference()},
    next_seq := non_neg_integer(),
    reads := #{{integer(), integer()} => {integer(), integer()}},
    active := #{integer() => {non_neg_integer(), integer()}},
    counters := #{counter() => non_neg_integer()},
    max_queue := pos_integer(),
    max_inflight := pos_integer(),
    max_fallback_runners := pos_integer(),
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

-spec delivery_config_changed() -> ok.
delivery_config_changed() ->
    gen_server:cast(?MODULE, delivery_config_changed).

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

-spec init([]) -> {ok, state()}.
init([]) ->
    erlang:process_flag(fullsweep_after, 10),
    schedule_prune(),
    {ok, #{
        jobs => gb_trees:empty(),
        ready => queue:new(),
        inflight => #{},
        fallback_backlog => queue:new(),
        fallback_runners => #{},
        next_seq => 0,
        reads => #{},
        active => #{},
        counters => #{},
        max_queue => env_pos_integer(push_outbox_max_queue, ?DEFAULT_MAX_QUEUE),
        max_inflight => env_pos_integer(push_outbox_max_inflight, ?DEFAULT_MAX_INFLIGHT),
        max_fallback_runners => app_pos_integer(
            push_outbox_max_fallback_runners, ?DEFAULT_MAX_FALLBACK_RUNNERS
        ),
        request_timeout_ms => request_timeout_ms(),
        max_age_ms => env_pos_integer(push_outbox_max_age_ms, ?DEFAULT_MAX_AGE_MS),
        retry_base_ms => app_pos_integer(push_outbox_retry_base_ms, ?DEFAULT_RETRY_BASE_MS)
    }}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({enqueue, Job}, _From, State) when
    is_map_key(kind, Job),
    is_map_key(subject, Job),
    is_map_key(body, Job),
    is_map_key(user_ids, Job),
    is_map_key(channel_id, Job),
    is_map_key(message_id, Job),
    is_map_key(fallback, Job)
->
    {reply, ok, pump(admit(Job, State))};
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
handle_cast(delivery_config_changed, State) ->
    {noreply, drain(State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({push_outbox_reply, Pid, Result}, State) when is_pid(Pid) ->
    {noreply, pump(finish_worker(Pid, reply_result(Result), State))};
handle_info({'DOWN', _MRef, process, Pid, Reason}, #{fallback_runners := Runners} = State) when
    is_map_key(Pid, Runners)
->
    {noreply, finish_fallback(Pid, Reason, State)};
handle_info({'DOWN', _MRef, process, Pid, Reason}, State) when is_pid(Pid) ->
    {noreply, pump(finish_worker(Pid, down_result(Reason), State))};
handle_info({request_deadline, Pid}, State) when is_pid(Pid) ->
    kill_expired_worker(Pid, State),
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
    Entry = maps:merge(Job, #{
        seq => Seq,
        enqueued_at => now_ms(),
        attempts => 0,
        config_version => job_config_version(Job)
    }),
    State1 = bump(enqueued, 1, State#{next_seq := Seq + 1}),
    queue_settled(settle_if_stale(Entry, State1)).

-spec job_config_version(job()) -> non_neg_integer() | undefined.
job_config_version(#{job := #{<<"config_version">> := Version}}) when
    is_integer(Version), Version >= 0
->
    Version;
job_config_version(_Job) ->
    undefined.

-spec queue_settled(settled()) -> state().
queue_settled({keep, Entry, State}) ->
    shed_to_capacity(insert(Entry, State));
queue_settled({none, State}) ->
    State.

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
        {send, Prepared, State1} -> send_or_fall_back(Prepared, State1)
    end.

-spec send_or_fall_back(entry(), state()) -> state().
send_or_fall_back(Entry, State) ->
    case is_expired(Entry, State) of
        true -> fall_back(Entry, State);
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

-spec is_read(integer(), integer(), integer(), #{
    {integer(), integer()} => {integer(), integer()}
}) ->
    boolean().
is_read(UserId, ChannelId, MessageId, Reads) ->
    case maps:find({UserId, ChannelId}, Reads) of
        {ok, {ReadMessageId, _At}} -> MessageId =< ReadMessageId;
        error -> false
    end.

-spec became_active(integer(), non_neg_integer(), #{integer() => {non_neg_integer(), integer()}}) ->
    boolean().
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
    lists:foldl(
        fun({Seq, Entry}, Acc) ->
            truncate_entry(Seq, Entry, UserId, ChannelId, MessageId, Acc)
        end,
        State1,
        gb_trees:to_list(Jobs)
    ).

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
remove_reader(Seq, #{user_ids := UserIds} = Entry, UserId, #{jobs := Jobs} = State) ->
    case lists:member(UserId, UserIds) of
        false ->
            State;
        true ->
            State1 = bump(truncations, 1, State),
            case lists:delete(UserId, UserIds) of
                [] ->
                    State1#{jobs := gb_trees:delete(Seq, Jobs)};
                Remaining ->
                    State1#{jobs := gb_trees:update(Seq, with_user_ids(Remaining, Entry), Jobs)}
            end
    end.

-spec record_active(integer(), state()) -> state().
record_active(UserId, #{active := Active, next_seq := Seq} = State) ->
    State#{active := Active#{UserId => {Seq, now_ms()}}, next_seq := Seq + 1}.

-spec start_worker(entry(), state()) -> state().
start_worker(Entry, #{inflight := Inflight, request_timeout_ms := Timeout} = State) ->
    #{subject := Subject, body := Body} = Entry,
    Outbox = self(),
    {Pid, MRef} = spawn_monitor(fun() ->
        Outbox ! {push_outbox_reply, self(), send(Subject, Body, Timeout)}
    end),
    TRef = erlang:send_after(Timeout, Outbox, {request_deadline, Pid}),
    State#{inflight := Inflight#{Pid => {MRef, TRef, Entry}}}.

-spec send(binary(), binary(), pos_integer()) -> ok | {error, term()}.
send(Subject, Body, Timeout) ->
    try push_job_publisher:request(Subject, Body, Timeout) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

-spec kill_expired_worker(pid(), state()) -> ok.
kill_expired_worker(Pid, #{inflight := Inflight}) ->
    case maps:is_key(Pid, Inflight) of
        true ->
            exit(Pid, kill),
            ok;
        false ->
            ok
    end.

-spec finish_worker(pid(), ok | {error, term()}, state()) -> state().
finish_worker(Pid, Result, #{inflight := Inflight} = State) ->
    case maps:take(Pid, Inflight) of
        {{MRef, TRef, Entry}, Rest} ->
            erlang:demonitor(MRef, [flush]),
            _ = erlang:cancel_timer(TRef, [{async, true}, {info, false}]),
            handle_result(Result, Entry, State#{inflight := Rest});
        error ->
            State
    end.

-spec reply_result(term()) -> ok | {error, term()}.
reply_result(ok) -> ok;
reply_result({error, Reason}) -> {error, Reason};
reply_result(Other) -> {error, {invalid_worker_reply, Other}}.

-spec down_result(term()) -> {error, term()}.
down_result(killed) -> {error, timeout};
down_result(Reason) -> {error, {worker_down, Reason}}.

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
        true -> fall_back(Entry, State);
        false -> retry_settled(settle_if_stale(Entry, State))
    end.

-spec retry_settled(settled()) -> state().
retry_settled({keep, Entry, State}) ->
    schedule_retry(Entry, State);
retry_settled({none, State}) ->
    State.

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

-spec fall_back(entry(), state()) -> state().
fall_back(#{user_ids := UserIds} = Entry, State) ->
    logger:warning("Push outbox job expired undelivered, falling back to the gateway path", #{
        kind => maps:get(kind, Entry),
        channel_id => maps:get(channel_id, Entry),
        message_id => maps:get(message_id, Entry),
        attempts => maps:get(attempts, Entry),
        user_count => length(UserIds)
    }),
    hand_back(UserIds, Entry, State).

-spec hand_back([integer()], entry(), state()) -> state().
hand_back(UserIds, #{fallback := Fallback}, #{fallback_backlog := Backlog} = State) ->
    Queued = State#{fallback_backlog := queue:in({Fallback, UserIds}, Backlog)},
    run_fallbacks(bump(fallbacks, 1, Queued)).

-spec run_fallbacks(state()) -> state().
run_fallbacks(#{fallback_runners := Runners, max_fallback_runners := MaxRunners} = State) when
    map_size(Runners) >= MaxRunners
->
    State;
run_fallbacks(#{fallback_backlog := Backlog, fallback_runners := Runners} = State) ->
    case queue:out(Backlog) of
        {empty, _} ->
            State;
        {{value, {Fallback, UserIds}}, Rest} ->
            {Pid, MRef} = spawn_monitor(fun() -> run_fallback(Fallback, UserIds) end),
            run_fallbacks(State#{
                fallback_backlog := Rest, fallback_runners := Runners#{Pid => MRef}
            })
    end.

-spec finish_fallback(pid(), term(), state()) -> state().
finish_fallback(Pid, Reason, #{fallback_runners := Runners} = State) ->
    run_fallbacks(
        count_fallback_exit(Reason, State#{fallback_runners := maps:remove(Pid, Runners)})
    ).

-spec count_fallback_exit(term(), state()) -> state().
count_fallback_exit(normal, State) ->
    State;
count_fallback_exit(_Reason, State) ->
    bump(lost, 1, State).

-spec drain(state()) -> state().
drain(#{jobs := Jobs} = State) ->
    Drained = lists:foldl(fun resettle/2, State, gb_trees:to_list(Jobs)),
    log_drain(count(fallbacks, Drained) - count(fallbacks, State), Drained),
    Drained.

-spec resettle({non_neg_integer(), entry()}, state()) -> state().
resettle({Seq, Entry}, State) ->
    case settle(Entry, State) of
        {keep, Kept, #{jobs := Jobs} = State1} ->
            State1#{jobs := gb_trees:update(Seq, Kept, Jobs)};
        {none, #{jobs := Jobs} = State1} ->
            State1#{jobs := gb_trees:delete(Seq, Jobs)}
    end.

-spec settle_if_stale(entry(), state()) -> settled().
settle_if_stale(#{config_version := Version} = Entry, State) ->
    case push_delivery_config:config_version() of
        Version -> {keep, Entry, State};
        _Changed -> settle(Entry, State)
    end.

-spec settle(entry(), state()) -> settled().
settle(#{kind := ring} = Entry, State) ->
    {keep, Entry, State};
settle(Entry, State) ->
    case prepare(Entry, State) of
        {skip, State1} ->
            {none, State1};
        {send, Prepared, State1} ->
            hand_back_unenrolled(push_delivery_config:config(), Prepared, State1)
    end.

-spec hand_back_unenrolled(push_delivery_config:config(), entry(), state()) -> settled().
hand_back_unenrolled(Config, #{user_ids := UserIds} = Entry, State) ->
    Settled = Entry#{config_version := maps:get(config_version, Config)},
    case push_delivery_config:partition_users(Config, UserIds) of
        {UserIds, []} ->
            {keep, Settled, State};
        {[], Unenrolled} ->
            {none, hand_back(Unenrolled, Entry, State)};
        {Enrolled, Unenrolled} ->
            {keep, with_user_ids(Enrolled, Settled), hand_back(Unenrolled, Entry, State)}
    end.

-spec log_drain(integer(), state()) -> ok.
log_drain(HandedBack, _State) when HandedBack =< 0 ->
    ok;
log_drain(HandedBack, #{jobs := Jobs}) ->
    logger:notice(
        "Push outbox handed queued jobs back to the gateway path after a delivery config change",
        #{
            handed_back => HandedBack,
            config_version => push_delivery_config:config_version(),
            depth => gb_trees:size(Jobs)
        }
    ).

-spec run_fallback(fallback(), [integer()]) -> ok.
run_fallback(Fallback, UserIds) ->
    try Fallback(UserIds) of
        _ -> ok
    catch
        Class:Reason ->
            logger:error("Push outbox fallback crashed", #{class => Class, reason => Reason}),
            exit(fallback_crashed)
    end.

-spec prune(state()) -> state().
prune(#{reads := Reads, active := Active, max_age_ms := MaxAge} = State) ->
    Cutoff = now_ms() - MaxAge,
    State#{
        reads := maps:filter(fun(_Key, {_MessageId, At}) -> At >= Cutoff end, Reads),
        active := maps:filter(fun(_UserId, {_Seq, At}) -> At >= Cutoff end, Active)
    }.

-spec build_stats(state()) -> map().
build_stats(#{
    jobs := Jobs,
    inflight := Inflight,
    fallback_backlog := Backlog,
    fallback_runners := Runners,
    counters := Counters
}) ->
    maps:merge(
        #{
            delivered => 0,
            retries => 0,
            sheds => 0,
            truncations => 0,
            skipped_active => 0,
            fallbacks => 0,
            lost => 0,
            enqueued => 0
        },
        Counters#{
            depth => gb_trees:size(Jobs),
            inflight => map_size(Inflight),
            fallback_backlog => queue:len(Backlog),
            fallback_runners => map_size(Runners)
        }
    ).

-spec count(counter(), state()) -> non_neg_integer().
count(Counter, #{counters := Counters}) ->
    maps:get(Counter, Counters, 0).

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

-spec app_pos_integer(atom(), pos_integer()) -> pos_integer().
app_pos_integer(Key, Default) ->
    case application:get_env(fluxer_gateway, Key, undefined) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> Default
    end.
