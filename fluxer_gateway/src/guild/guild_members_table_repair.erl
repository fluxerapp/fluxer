%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_members_table_repair).
-typing([eqwalizer]).

-export([
    maybe_repair/1,
    members_table_healthy/1
]).

-define(REPAIR_KEY, members_ets_repair).
-define(ATTEMPTS_KEY, members_ets_repair_attempts).
-define(MAX_MEMBER_ETS_REPAIR_CHUNK, 5000).
-define(MAX_MEMBER_ETS_REPAIR_MEMBERS, 250000).
-define(MAX_MEMBER_ETS_REPAIR_TICKS, 200).
-define(MAX_MEMBER_ETS_REPAIR_ATTEMPTS, 3).
-define(MAX_MEMBER_ETS_REPAIR_MAILBOX, 1000).

-type guild_state() :: map().
-type user_id() :: integer().
-type member() :: map().
-type member_map() :: #{user_id() => member()}.
-type pass_result() :: continue | complete | {error, table_unavailable}.

-export_type([guild_state/0]).

-spec maybe_repair(guild_state()) -> guild_state().
maybe_repair(State) ->
    case self_heal_enabled() of
        true -> repair_enabled(State);
        false -> release_pending_repair(State)
    end.

-spec repair_enabled(guild_state()) -> guild_state().
repair_enabled(State) ->
    case members_table_healthy(members_table(State)) of
        true -> release_pending_repair(State);
        false -> repair_unhealthy(State)
    end.

-spec self_heal_enabled() -> boolean().
self_heal_enabled() ->
    type_conv:is_true(application:get_env(fluxer_gateway, guild_members_table_self_heal, true)).

-spec members_table_healthy(term()) -> boolean().
members_table_healthy(Tab) when is_reference(Tab) ->
    table_owner(Tab) =:= self();
members_table_healthy(_Tab) ->
    false.

-spec repair_unhealthy(guild_state()) -> guild_state().
repair_unhealthy(State) ->
    case mailbox_within_bound(message_queue_len()) of
        true -> repair_pass(State);
        false -> State
    end.

-spec repair_pass(guild_state()) -> guild_state().
repair_pass(State) ->
    case maps:get(?REPAIR_KEY, State, undefined) of
        #{tab := Tab, ticks := Ticks} when is_reference(Tab), is_integer(Ticks) ->
            continue_repair(eqwalizer:dynamic_cast(Tab), Ticks, State);
        _ ->
            start_repair(maps:remove(?REPAIR_KEY, State))
    end.

-spec start_repair(guild_state()) -> guild_state().
start_repair(State) ->
    case attempts(State) < ?MAX_MEMBER_ETS_REPAIR_ATTEMPTS of
        true -> start_repair_within_bounds(State);
        false -> State
    end.

-spec start_repair_within_bounds(guild_state()) -> guild_state().
start_repair_within_bounds(State) ->
    Size = map_size(member_map(State)),
    case Size =< ?MAX_MEMBER_ETS_REPAIR_MEMBERS of
        true -> create_staging_table(State);
        false -> refuse_oversized(Size, State)
    end.

-spec refuse_oversized(non_neg_integer(), guild_state()) -> guild_state().
refuse_oversized(Size, State) ->
    logger:warning(
        "guild_members_table_repair_refused: guild_id=~p members=~p max=~p",
        [guild_id(State), Size, ?MAX_MEMBER_ETS_REPAIR_MEMBERS]
    ),
    State#{?ATTEMPTS_KEY => ?MAX_MEMBER_ETS_REPAIR_ATTEMPTS}.

-spec create_staging_table(guild_state()) -> guild_state().
create_staging_table(State) ->
    case new_staging_table() of
        Tab when is_reference(Tab) -> continue_repair(eqwalizer:dynamic_cast(Tab), 0, State);
        undefined -> record_start_failure(State)
    end.

-spec continue_repair(ets:tid(), non_neg_integer(), guild_state()) -> guild_state().
continue_repair(Tab, Ticks, State) when Ticks >= ?MAX_MEMBER_ETS_REPAIR_TICKS ->
    abandon_repair(Tab, ticks_exhausted, State);
continue_repair(Tab, Ticks, State) ->
    case run_pass(Tab, member_map(State)) of
        continue -> State#{?REPAIR_KEY => #{tab => Tab, ticks => Ticks + 1}};
        complete -> publish_repair(Tab, State);
        {error, table_unavailable} -> abandon_repair(Tab, table_unavailable, State)
    end.

-spec publish_repair(ets:tid(), guild_state()) -> guild_state().
publish_repair(Tab, State) ->
    case maps:get(data, State, #{}) of
        Data when is_map(Data) -> publish_into_data(Tab, Data, State);
        _ -> abandon_repair(Tab, invalid_guild_data, State)
    end.

-spec publish_into_data(ets:tid(), map(), guild_state()) -> guild_state().
publish_into_data(Tab, Data, State) ->
    logger:info(
        "guild_members_table_repair_completed: guild_id=~p members=~p",
        [guild_id(State), map_size(member_map(State))]
    ),
    Published = State#{data => Data#{members_ets => Tab}},
    maps:remove(?ATTEMPTS_KEY, maps:remove(?REPAIR_KEY, Published)).

-spec abandon_repair(ets:tid(), atom(), guild_state()) -> guild_state().
abandon_repair(Tab, Reason, State) ->
    ok = delete_staging_table(Tab),
    logger:warning(
        "guild_members_table_repair_abandoned: guild_id=~p reason=~p attempts=~p",
        [guild_id(State), Reason, attempts(State) + 1]
    ),
    bump_attempts(maps:remove(?REPAIR_KEY, State)).

-spec record_start_failure(guild_state()) -> guild_state().
record_start_failure(State) ->
    logger:warning(
        "guild_members_table_repair_start_failed: guild_id=~p attempts=~p",
        [guild_id(State), attempts(State) + 1]
    ),
    bump_attempts(State).

-spec run_pass(ets:table(), member_map()) -> pass_result().
run_pass(Tab, MemberMap) ->
    run_pass(Tab, MemberMap, ?MAX_MEMBER_ETS_REPAIR_CHUNK).

-spec run_pass(ets:table(), member_map(), pos_integer()) -> pass_result().
run_pass(Tab, MemberMap, Budget) ->
    try
        apply_pass(Tab, MemberMap, Budget)
    catch
        error:badarg -> {error, table_unavailable}
    end.

-spec apply_pass(ets:table(), member_map(), pos_integer()) -> continue | complete.
apply_pass(Tab, MemberMap, Budget) ->
    case collect_missing_rows(Tab, MemberMap, Budget) of
        [] -> drop_extra_rows(Tab, MemberMap, Budget);
        Rows -> insert_missing_rows(Tab, MemberMap, Rows, Budget)
    end.

-spec insert_missing_rows(ets:table(), member_map(), [{user_id(), member()}], pos_integer()) ->
    continue | complete.
insert_missing_rows(Tab, MemberMap, Rows, Budget) ->
    true = ets:insert(Tab, Rows),
    case length(Rows) < Budget of
        true -> drop_extra_rows(Tab, MemberMap, Budget);
        false -> continue
    end.

-spec drop_extra_rows(ets:table(), member_map(), pos_integer()) -> continue | complete.
drop_extra_rows(Tab, MemberMap, Budget) ->
    case collect_extra_ids(Tab, MemberMap, Budget) of
        [] -> complete;
        Ids -> delete_extra_ids(Tab, Ids, Budget)
    end.

-spec delete_extra_ids(ets:table(), [user_id()], pos_integer()) -> continue | complete.
delete_extra_ids(Tab, Ids, Budget) ->
    lists:foreach(fun(UserId) -> ets:delete(Tab, UserId) end, Ids),
    case length(Ids) < Budget of
        true -> complete;
        false -> continue
    end.

-spec collect_missing_rows(ets:table(), member_map(), pos_integer()) ->
    [{user_id(), member()}].
collect_missing_rows(Tab, MemberMap, Budget) ->
    collect_missing_iter(maps:next(maps:iterator(MemberMap)), Tab, Budget, []).

-spec collect_missing_iter(
    none | {user_id(), member(), maps:iterator(user_id(), member())},
    ets:table(),
    non_neg_integer(),
    [{user_id(), member()}]
) ->
    [{user_id(), member()}].
collect_missing_iter(_Next, _Tab, 0, Acc) ->
    Acc;
collect_missing_iter(none, _Tab, _Budget, Acc) ->
    Acc;
collect_missing_iter({UserId, Member, Iter}, Tab, Budget, Acc) ->
    case ets:member(Tab, UserId) of
        true ->
            collect_missing_iter(maps:next(Iter), Tab, Budget, Acc);
        false ->
            collect_missing_iter(maps:next(Iter), Tab, Budget - 1, [{UserId, Member} | Acc])
    end.

-spec collect_extra_ids(ets:table(), member_map(), pos_integer()) -> [user_id()].
collect_extra_ids(Tab, MemberMap, Budget) ->
    collect_extra_iter(eqwalizer:dynamic_cast(ets:first(Tab)), Tab, MemberMap, Budget, []).

-spec collect_extra_iter(
    user_id() | '$end_of_table', ets:table(), member_map(), non_neg_integer(), [user_id()]
) ->
    [user_id()].
collect_extra_iter(_Key, _Tab, _MemberMap, 0, Acc) ->
    Acc;
collect_extra_iter('$end_of_table', _Tab, _MemberMap, _Budget, Acc) ->
    Acc;
collect_extra_iter(Key, Tab, MemberMap, Budget, Acc) ->
    Next = eqwalizer:dynamic_cast(ets:next(Tab, Key)),
    case maps:is_key(Key, MemberMap) of
        true ->
            collect_extra_iter(Next, Tab, MemberMap, Budget, Acc);
        false ->
            collect_extra_iter(Next, Tab, MemberMap, Budget - 1, [Key | Acc])
    end.

-spec release_pending_repair(guild_state()) -> guild_state().
release_pending_repair(State) ->
    case maps:get(?REPAIR_KEY, State, undefined) of
        undefined ->
            State;
        #{tab := Tab} when is_reference(Tab) ->
            discard_staging_table(eqwalizer:dynamic_cast(Tab), State);
        _ ->
            maps:remove(?REPAIR_KEY, State)
    end.

-spec discard_staging_table(ets:tid(), guild_state()) -> guild_state().
discard_staging_table(Tab, State) ->
    ok = delete_staging_table(Tab),
    maps:remove(?REPAIR_KEY, State).

-spec new_staging_table() -> ets:tid() | undefined.
new_staging_table() ->
    try ets:new(guild_members_data, [set, public, {read_concurrency, true}]) of
        Tab when is_reference(Tab) -> Tab;
        _Named -> undefined
    catch
        error:badarg -> undefined;
        error:system_limit -> undefined
    end.

-spec delete_staging_table(ets:tid()) -> ok.
delete_staging_table(Tab) ->
    try ets:delete(Tab) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec table_owner(term()) -> pid() | undefined.
table_owner(Tab) ->
    try ets:info(eqwalizer:dynamic_cast(Tab), owner) of
        Owner when is_pid(Owner) -> Owner;
        _ -> undefined
    catch
        error:badarg -> undefined
    end.

-spec members_table(guild_state()) -> ets:tid() | undefined.
members_table(State) ->
    members_ets_table(data_map(State)).

-spec members_ets_table(map()) -> ets:tid() | undefined.
members_ets_table(#{members_ets := Tab}) ->
    Tab;
members_ets_table(_Data) ->
    undefined.

-spec member_map(guild_state()) -> member_map().
member_map(State) ->
    guild_data_index_members:member_map(data_map(State)).

-spec data_map(guild_state()) -> map().
data_map(State) ->
    case maps:get(data, State, #{}) of
        Data when is_map(Data) -> Data;
        _ -> #{}
    end.

-spec guild_id(guild_state()) -> term().
guild_id(State) ->
    maps:get(id, State, undefined).

-spec attempts(guild_state()) -> non_neg_integer().
attempts(State) ->
    case maps:get(?ATTEMPTS_KEY, State, 0) of
        N when is_integer(N), N >= 0 -> N;
        _ -> 0
    end.

-spec bump_attempts(guild_state()) -> guild_state().
bump_attempts(State) ->
    State#{?ATTEMPTS_KEY => attempts(State) + 1}.

-spec message_queue_len() -> non_neg_integer().
message_queue_len() ->
    case erlang:process_info(self(), message_queue_len) of
        {message_queue_len, Len} when is_integer(Len), Len >= 0 -> Len;
        _ -> 0
    end.

-spec mailbox_within_bound(non_neg_integer()) -> boolean().
mailbox_within_bound(Len) ->
    Len < ?MAX_MEMBER_ETS_REPAIR_MAILBOX.
