%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(channel_threads_flip).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([
    start_link/0,
    config_changed/2,
    self_heal/1,
    flip_counts/0,
    affected_targets/3
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-export([schedule/4, take_due/3, heal/4]).
-endif.

-define(REGISTRY_TABLE, process_registry_table).
-define(FLIP_COUNTS_KEY, {channel_threads_flip, flip_counts}).
-define(JITTER_WINDOW_MS, 60000).
-define(GUILD_BATCH_SIZE, 10).
-define(VIEWERS_BATCH_SIZE, 10).
-define(USER_BATCH_SIZE, 1000).
-define(BATCH_DELAY_MS, 100).
-define(HEAL_COOLDOWN_MS, 30000).

-type kind() :: guild | viewers | user.
-type target() :: {kind(), integer(), pid()}.
-type pending_key() :: {kind(), integer()}.
-type due() :: gb_sets:set({integer(), pending_key(), pid()}).
-type pending() :: #{
    due := #{kind() => due()},
    keys := #{pending_key() => {integer(), pid()}}
}.
-type state() :: #{
    pending := pending(),
    timer := reference() | undefined,
    healed := #{integer() => integer()}
}.
-type counted() :: guild | viewers | user | heal.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec config_changed(channel_threads_config:config(), channel_threads_config:config()) -> ok.
config_changed(Previous, Current) ->
    gen_server:cast(?MODULE, {config_changed, Previous, Current}).

-spec self_heal(integer()) -> ok.
self_heal(GuildId) when is_integer(GuildId) ->
    gen_server:cast(?MODULE, {self_heal, GuildId, self()}).

-spec flip_counts() -> #{counted() => non_neg_integer()}.
flip_counts() ->
    case persistent_term:get(?FLIP_COUNTS_KEY, undefined) of
        undefined ->
            #{};
        Counters ->
            maps:from_list([
                {Kind, counters:get(Counters, count_index(Kind))}
             || Kind <- counted()
            ])
    end.

-spec affected_targets(
    channel_threads_config:config(), channel_threads_config:config(), [target()]
) -> [target()].
affected_targets(Previous, Current, Candidates) ->
    Changed = changed_fun(Previous, Current),
    lists:foldr(
        fun({Kind, Id, Pid}, Acc) -> local_target(Kind, Id, Pid, Changed, Acc) end,
        [],
        Candidates
    ).

-spec changed_fun(channel_threads_config:config(), channel_threads_config:config()) ->
    fun((kind(), integer()) -> boolean()).
changed_fun(Previous, Current) ->
    GuildsChanged = channel_threads_config:guild_fields_changed(Previous, Current),
    UsersChanged = channel_threads_config:user_fields_changed(Previous, Current),
    Unloaded = unloaded(Previous),
    fun
        (user, _Id) when Unloaded ->
            false;
        (Kind, Id) ->
            target_changed(Kind, Id, GuildsChanged, UsersChanged, Previous, Current)
    end.

-spec unloaded(channel_threads_config:config()) -> boolean().
unloaded(#{enabled := false, config_version := 0}) -> true;
unloaded(_Config) -> false.

-spec target_changed(
    kind(),
    integer(),
    boolean(),
    boolean(),
    channel_threads_config:config(),
    channel_threads_config:config()
) -> boolean().
target_changed(guild, GuildId, true, _UsersChanged, Previous, Current) ->
    channel_threads_config:guild_active(Previous, GuildId) =/=
        channel_threads_config:guild_active(Current, GuildId);
target_changed(viewers, GuildId, _GuildsChanged, true, _Previous, Current) ->
    channel_threads_config:guild_active(Current, GuildId);
target_changed(user, UserId, _GuildsChanged, true, Previous, Current) ->
    channel_threads_config:user_active(Previous, UserId) =/=
        channel_threads_config:user_active(Current, UserId) orelse
        (maps:get(enabled, Previous) orelse maps:get(enabled, Current)) andalso
            channel_threads_config:user_excluded(Previous, UserId) =/=
                channel_threads_config:user_excluded(Current, UserId);
target_changed(_Kind, _Id, _GuildsChanged, _UsersChanged, _Previous, _Current) ->
    false.

-spec init([]) -> {ok, state()}.
init([]) ->
    erlang:process_flag(fullsweep_after, 50),
    ensure_counters(),
    {ok, #{pending => empty_pending(), timer => undefined, healed => #{}}}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({config_changed, Previous, Current}, State) ->
    case
        (maps:get(enabled, Previous) orelse maps:get(enabled, Current)) andalso
            (channel_threads_config:guild_fields_changed(Previous, Current) orelse
                channel_threads_config:user_fields_changed(Previous, Current))
    of
        true -> {noreply, schedule_changed(Previous, Current, State)};
        false -> {noreply, State}
    end;
handle_cast({self_heal, GuildId, Pid}, State) when is_integer(GuildId), is_pid(Pid) ->
    {Healed, Pending} = heal(GuildId, Pid, now_ms(), State),
    {noreply, State#{healed := Healed, pending := Pending}};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(flip_tick, State) ->
    {Due, Pending} = take_due(
        maps:get(pending, State),
        now_ms(),
        #{guild => ?GUILD_BATCH_SIZE, viewers => ?VIEWERS_BATCH_SIZE, user => ?USER_BATCH_SIZE}
    ),
    lists:foreach(fun send_flip/1, Due),
    {noreply, arm_timer(State#{pending := Pending, timer := undefined})};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec schedule_changed(
    channel_threads_config:config(), channel_threads_config:config(), state()
) -> state().
schedule_changed(Previous, Current, State) ->
    Targets = local_targets(changed_fun(Previous, Current)),
    Pending = schedule(Targets, now_ms(), maps:get(pending, State), fun jitter/0),
    arm_timer(State#{pending := Pending}).

-spec empty_pending() -> pending().
empty_pending() ->
    #{
        due => #{guild => gb_sets:new(), viewers => gb_sets:new(), user => gb_sets:new()},
        keys => #{}
    }.

-spec schedule([target()], integer(), pending(), fun(() -> non_neg_integer())) -> pending().
schedule(Targets, Now, Pending, Jitter) ->
    lists:foldl(
        fun(Target, Acc) -> schedule_target(Target, Now, Acc, Jitter) end, Pending, Targets
    ).

-spec schedule_target(target(), integer(), pending(), fun(() -> non_neg_integer())) ->
    pending().
schedule_target({Kind, Id, Pid}, Now, #{due := Due, keys := Keys} = Pending, Jitter) ->
    Key = {Kind, Id},
    KindDue = maps:get(Kind, Due),
    case maps:find(Key, Keys) of
        {ok, {_At, Pid}} ->
            Pending;
        {ok, {At, StalePid}} ->
            #{
                due => Due#{
                    Kind => gb_sets:add(
                        {At, Key, Pid}, gb_sets:del_element({At, Key, StalePid}, KindDue)
                    )
                },
                keys => Keys#{Key => {At, Pid}}
            };
        error ->
            At = Now + Jitter(),
            count(Kind),
            #{
                due => Due#{Kind => gb_sets:add({At, Key, Pid}, KindDue)},
                keys => Keys#{Key => {At, Pid}}
            }
    end.

-spec take_due(pending(), integer(), #{kind() => non_neg_integer()}) ->
    {[target()], pending()}.
take_due(Pending, Now, Limits) ->
    maps:fold(
        fun(Kind, Limit, {Acc, P}) ->
            {Taken, Rest} = take_due_kind(Kind, P, Now, Limit, []),
            {Acc ++ Taken, Rest}
        end,
        {[], Pending},
        Limits
    ).

-spec take_due_kind(kind(), pending(), integer(), non_neg_integer(), [target()]) ->
    {[target()], pending()}.
take_due_kind(_Kind, Pending, _Now, 0, Acc) ->
    {lists:reverse(Acc), Pending};
take_due_kind(Kind, #{due := Due, keys := Keys} = Pending, Now, Limit, Acc) ->
    KindDue = maps:get(Kind, Due),
    case gb_sets:is_empty(KindDue) of
        true ->
            {lists:reverse(Acc), Pending};
        false ->
            case gb_sets:take_smallest(KindDue) of
                {{At, {_Kind, Id} = Key, Pid}, Rest} when At =< Now ->
                    take_due_kind(
                        Kind,
                        #{due => Due#{Kind => Rest}, keys => maps:remove(Key, Keys)},
                        Now,
                        Limit - 1,
                        [{Kind, Id, Pid} | Acc]
                    );
                _Later ->
                    {lists:reverse(Acc), Pending}
            end
    end.

-spec heal(integer(), pid(), integer(), state()) -> {#{integer() => integer()}, pending()}.
heal(GuildId, Pid, Now, #{healed := Healed0, pending := Pending}) ->
    Healed = maps:filter(fun(_Id, At) -> Now - At < ?HEAL_COOLDOWN_MS end, Healed0),
    case maps:is_key(GuildId, Healed) of
        true ->
            {Healed, Pending};
        false ->
            count(heal),
            send_flip({guild, GuildId, Pid}),
            {Healed#{GuildId => Now}, drop_pending({guild, GuildId}, Pending)}
    end.

-spec drop_pending(pending_key(), pending()) -> pending().
drop_pending({Kind, _Id} = Key, #{due := Due, keys := Keys} = Pending) ->
    case maps:take(Key, Keys) of
        {{At, Pid}, RestKeys} ->
            KindDue = gb_sets:del_element({At, Key, Pid}, maps:get(Kind, Due)),
            #{due => Due#{Kind => KindDue}, keys => RestKeys};
        error ->
            Pending
    end.

-spec arm_timer(state()) -> state().
arm_timer(#{timer := Timer} = State) when is_reference(Timer) ->
    State;
arm_timer(#{pending := #{due := Due}} = State) ->
    case [At || KindDue <- maps:values(Due), {At, _Key, _Pid} <- smallest(KindDue)] of
        [] ->
            State;
        Ats ->
            Delay = max(?BATCH_DELAY_MS, lists:min(Ats) - now_ms()),
            State#{timer := erlang:send_after(Delay, self(), flip_tick)}
    end.

-spec smallest(due()) -> [{integer(), pending_key(), pid()}].
smallest(KindDue) ->
    case gb_sets:is_empty(KindDue) of
        true -> [];
        false -> [gb_sets:smallest(KindDue)]
    end.

-spec send_flip(target()) -> ok.
send_flip({Kind, _GuildId, Pid}) when Kind =:= guild; Kind =:= viewers ->
    Pid ! {thread_gate_flip, channel_threads_config:version()},
    ok;
send_flip({user, _UserId, Pid}) ->
    Pid ! {thread_user_flip, channel_threads_config:version()},
    ok.

-spec local_targets(fun((kind(), integer()) -> boolean())) -> [target()].
local_targets(Changed) ->
    try
        ets:foldl(
            fun(Entry, Acc) -> collect_local_target(Entry, Changed, Acc) end,
            [],
            ?REGISTRY_TABLE
        )
    catch
        error:badarg -> []
    end.

-spec collect_local_target(term(), fun((kind(), integer()) -> boolean()), [target()]) ->
    [target()].
collect_local_target({{guild, GuildId}, Pid}, Changed, Acc) when
    is_integer(GuildId), is_pid(Pid)
->
    local_target(guild, GuildId, Pid, Changed, Acc);
collect_local_target({{presence, UserId}, Pid}, Changed, Acc) when
    is_integer(UserId), is_pid(Pid)
->
    local_target(user, UserId, Pid, Changed, Acc);
collect_local_target(_Entry, _Changed, Acc) ->
    Acc.

-spec local_target(kind(), integer(), pid(), fun((kind(), integer()) -> boolean()), [target()]) ->
    [target()].
local_target(guild, Id, Pid, Changed, Acc) when node(Pid) =:= node() ->
    case Changed(guild, Id) of
        true -> [{guild, Id, Pid} | Acc];
        false -> local_target(viewers, Id, Pid, Changed, Acc)
    end;
local_target(Kind, Id, Pid, Changed, Acc) when node(Pid) =:= node() ->
    case Changed(Kind, Id) of
        true -> [{Kind, Id, Pid} | Acc];
        false -> Acc
    end;
local_target(_Kind, _Id, _Pid, _Changed, Acc) ->
    Acc.

-spec jitter() -> non_neg_integer().
jitter() ->
    rand:uniform(?JITTER_WINDOW_MS) - 1.

-spec now_ms() -> integer().
now_ms() ->
    erlang:monotonic_time(millisecond).

-spec ensure_counters() -> ok.
ensure_counters() ->
    case persistent_term:get(?FLIP_COUNTS_KEY, undefined) of
        undefined ->
            persistent_term:put(?FLIP_COUNTS_KEY, counters:new(length(counted()), [atomics]));
        _Counters ->
            ok
    end.

-spec count(counted()) -> ok.
count(Kind) ->
    case persistent_term:get(?FLIP_COUNTS_KEY, undefined) of
        undefined -> ok;
        Counters -> counters:add(Counters, count_index(Kind), 1)
    end.

-spec counted() -> [counted()].
counted() ->
    [guild, viewers, user, heal].

-spec count_index(counted()) -> pos_integer().
count_index(guild) -> 1;
count_index(user) -> 2;
count_index(heal) -> 3;
count_index(viewers) -> 4.
