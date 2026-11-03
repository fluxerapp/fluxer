%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_thread_load).
-typing([eqwalizer]).

-export([
    channels_collection/1,
    take_extras/1,
    install/2,
    export_handoff/1,
    handle_retry/1,
    handle_load_result/3,
    queue_event/3,
    loading/1,
    load_payload/1,
    parse_gate/1,
    fetch_flip_data/2,
    deactivate/1,
    fetch_counts/0,
    init_counters/0
]).

-define(HANDOFF_ROW_BUDGET, 50000).
-define(HANDOFF_MAX_AGE_MS, 60000).
-define(LOAD_QUEUE_CAP, 10000).
-define(BACKOFF_MS, [30000, 120000, 600000]).
-define(FLIP_RETRIES, 5).
-define(FLIP_RETRY_DELAY_MS, 1000).
-define(MEMBER_PAGE_LIMIT, 1000).
-define(MEMBER_PAGES_PER_THREAD, 2).
-define(MEMBER_THREAD_BATCH, 100).
-define(FETCH_COUNTS_KEY, {?MODULE, fetch_counts}).
-define(EXTRA_KEYS, [thread_gate, thread_tainted, thread_load, thread_rows, thread_rows_at]).

-type gate() :: #{active := boolean(), version := non_neg_integer()}.
-type payload() :: #{threads := [map()], members := [map()], parent_settings := [map()]}.
-type fetch_result() :: ok | error | refetch.

-spec channels_collection(map()) -> {[map()], map()}.
channels_collection(Response) ->
    Channels = list_field(<<"channels">>, Response),
    Gate = parse_gate(maps:get(<<"thread_gate">>, Response, undefined)),
    Tainted = type_conv:is_true(maps:get(<<"thread_tainted">>, Response, false)),
    Base = #{thread_gate => Gate, thread_tainted => Tainted},
    case Gate of
        #{active := true} ->
            {merge_channels(Channels, list_field(<<"thread_only_channels">>, Response)), Base};
        _ ->
            {Channels, Base}
    end.

-spec fetch_payload(integer()) -> {ok, payload()} | {error, term()}.
fetch_payload(GuildId) ->
    case fetch_flip_data(GuildId, channel_threads_config:version()) of
        {ok, Data} ->
            count_fetch(ok),
            {ok, load_payload(Data)};
        {error, Reason} ->
            count_fetch(error),
            {error, Reason}
    end.

-spec fetch_flip_data(integer(), non_neg_integer()) -> {ok, map()} | {error, term()}.
fetch_flip_data(GuildId, Version) ->
    fetch_flip_data(GuildId, Version, ?FLIP_RETRIES).

-spec fetch_flip_data(integer(), non_neg_integer(), non_neg_integer()) ->
    {ok, map()} | {error, term()}.
fetch_flip_data(GuildId, Version, Retries) ->
    Request = #{
        <<"type">> => <<"guild_thread_flip_data">>,
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"config_version">> => Version,
        <<"paged_members">> => true
    },
    try rpc_client:call(Request) of
        {ok, #{<<"config_version">> := ApiVersion}} when
            is_integer(ApiVersion), ApiVersion < Version, Retries > 0
        ->
            timer:sleep(?FLIP_RETRY_DELAY_MS),
            fetch_flip_data(GuildId, Version, Retries - 1);
        {ok, #{<<"threads">> := Threads} = Data} when
            is_list(Threads), not is_map_key(<<"thread_members">>, Data)
        ->
            with_paged_members(GuildId, Threads, Data);
        {ok, Data} ->
            {ok, Data};
        {error, Reason} ->
            {error, Reason}
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

-spec with_paged_members(integer(), list(), map()) -> {ok, map()} | {error, term()}.
with_paged_members(GuildId, Threads, Data) ->
    ThreadIds = [
        Id
     || T <- Threads, is_map(T), Id <- [maps:get(<<"id">>, T, undefined)], is_binary(Id)
    ],
    try fetch_thread_members(GuildId, ThreadIds, []) of
        {ok, Members} -> {ok, Data#{<<"thread_members">> => Members}};
        {error, Reason} -> {error, Reason}
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

-spec fetch_thread_members(integer(), [binary()], [[map()]]) ->
    {ok, [map()]} | {error, term()}.
fetch_thread_members(_GuildId, [], Acc) ->
    {ok, lists:append(lists:reverse(Acc))};
fetch_thread_members(GuildId, ThreadIds, Acc) ->
    {Batch, Rest} = lists:split(min(?MEMBER_THREAD_BATCH, length(ThreadIds)), ThreadIds),
    case
        fetch_member_pages(
            GuildId, Batch, undefined, Acc, ?MEMBER_PAGES_PER_THREAD * length(Batch)
        )
    of
        {ok, Acc1} -> fetch_thread_members(GuildId, Rest, Acc1);
        {error, Reason} -> {error, Reason}
    end.

-spec fetch_member_pages(
    integer(), [binary()], binary() | undefined, [[map()]], non_neg_integer()
) ->
    {ok, [[map()]]} | {error, term()}.
fetch_member_pages(_GuildId, [], _After, Acc, _PagesLeft) ->
    {ok, Acc};
fetch_member_pages(_GuildId, _ThreadIds, _After, Acc, 0) ->
    {ok, Acc};
fetch_member_pages(GuildId, ThreadIds, After, Acc, PagesLeft) ->
    Request0 = #{
        <<"type">> => <<"list_thread_members">>,
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"thread_ids">> => ThreadIds,
        <<"limit">> => ?MEMBER_PAGE_LIMIT
    },
    Request =
        case After of
            undefined -> Request0;
            _ -> Request0#{<<"after_user_id">> => After}
        end,
    Result =
        case rpc_client:call(Request) of
            {ok, #{<<"members">> := Members} = Page} when is_list(Members) ->
                Acc1 = [[M || M <- Members, is_map(M)] | Acc],
                {next, next_member_cursor(Page, ThreadIds), Acc1};
            {ok, _Invalid} ->
                {error, invalid_thread_members_page};
            {error, Reason} ->
                {error, Reason}
        end,
    case Result of
        {next, done, CursorAcc} ->
            {ok, CursorAcc};
        {next, {Remaining, Next}, CursorAcc} ->
            fetch_member_pages(GuildId, Remaining, Next, CursorAcc, PagesLeft - 1);
        {next, invalid, _CursorAcc} ->
            {error, invalid_thread_members_page};
        {error, _Reason} = Error ->
            Error
    end.

-spec next_member_cursor(map(), [binary()]) ->
    done | invalid | {[binary()], binary() | undefined}.
next_member_cursor(
    #{<<"has_more">> := true, <<"next_thread_id">> := ThreadId} = Page, ThreadIds
) when
    is_binary(ThreadId)
->
    Remaining = lists:dropwhile(fun(Id) -> Id =/= ThreadId end, ThreadIds),
    Cursor =
        case Remaining of
            [] -> invalid;
            _ -> maps:get(<<"next_after_user_id">>, Page, null)
        end,
    case Cursor of
        Next when is_binary(Next) -> {Remaining, Next};
        null -> {Remaining, undefined};
        _ -> invalid
    end;
next_member_cursor(#{<<"has_more">> := true}, _ThreadIds) ->
    invalid;
next_member_cursor(_Page, _ThreadIds) ->
    done.

-spec load_payload(map()) ->
    #{threads := [map()], members := [map()], parent_settings := [map()]}.
load_payload(Response) ->
    #{
        threads => [guild_data_normalize_thread(T) || T <- list_field(<<"threads">>, Response)],
        members => [
            guild_data_normalize:thread_member(M)
         || M <- list_field(<<"thread_members">>, Response)
        ],
        parent_settings => list_field(<<"thread_parent_settings">>, Response)
    }.

-spec guild_data_normalize_thread(map()) -> map().
guild_data_normalize_thread(Thread) ->
    case guild_data_normalize:thread(Thread) of
        Normalized when is_map(Normalized) -> Normalized;
        _ -> Thread
    end.

-spec parse_gate(term()) -> #{active := boolean(), version := non_neg_integer()}.
parse_gate(#{<<"active">> := Active} = Gate) when is_boolean(Active) ->
    Version =
        case maps:get(<<"config_version">>, Gate, 0) of
            V when is_integer(V), V >= 0 -> V;
            _ -> 0
        end,
    #{active => Active, version => Version};
parse_gate(_) ->
    #{active => false, version => 0}.

-spec merge_channels([map()], [map()]) -> [map()].
merge_channels(Channels, []) ->
    Channels;
merge_channels(Channels, Forums) ->
    Ids = sets:from_list([maps:get(<<"id">>, C, undefined) || C <- Channels]),
    Channels ++ [F || F <- Forums, not sets:is_element(maps:get(<<"id">>, F, undefined), Ids)].

-spec take_extras(map()) -> {map(), map()}.
take_extras(Data) ->
    Extras0 = maps:with(?EXTRA_KEYS, Data),
    Data1 = maps:without(?EXTRA_KEYS, Data),
    case maps:get(<<"guild">>, Data1, undefined) of
        Guild when is_map(Guild) ->
            Extras = maps:merge(guild_extras(Guild), Extras0),
            {Extras, Data1#{
                <<"guild">> => maps:without([<<"thread_gate">>, <<"thread_tainted">>], Guild)
            }};
        _ ->
            {Extras0, Data1}
    end.

-spec guild_extras(map()) -> map().
guild_extras(Guild) ->
    Gate =
        case maps:find(<<"thread_gate">>, Guild) of
            {ok, G} -> #{thread_gate => parse_gate(G)};
            error -> #{}
        end,
    case maps:find(<<"thread_tainted">>, Guild) of
        {ok, true} -> Gate#{thread_tainted => true};
        _ -> Gate
    end.

-spec install(map(), map()) -> map().
install(Extras, State) ->
    Data = maps:get(data, State),
    Gate = maps:get(thread_gate, Extras, #{active => false, version => 0}),
    Tainted = type_conv:is_true(maps:get(thread_tainted, Extras, false)),
    Data1 = put_flag(thread_tainted, Tainted, put_gate(Gate, Data)),
    State1 = State#{data => Data1},
    State2 =
        case Gate of
            #{active := true} -> install_active(Extras, State1);
            _ -> State1
        end,
    case guild_thread_gate:active(State2) orelse any_viewer(State2) of
        true ->
            guild_thread_subscriptions:refresh_presence_users(
                guild_thread_gate:recompute_session_viewers(State2)
            );
        false ->
            State2
    end.

-spec any_viewer(map()) -> boolean().
any_viewer(State) ->
    lists:any(
        fun guild_thread_gate:session_viewer/1,
        [S || S <- maps:values(maps:get(sessions, State, #{})), is_map(S)]
    ).

-spec put_gate(gate(), map()) -> map().
put_gate(#{active := true} = Gate, Data) -> Data#{thread_gate => Gate};
put_gate(_Gate, Data) -> maps:remove(thread_gate, Data).

-spec put_flag(atom(), boolean(), map()) -> map().
put_flag(Key, true, Map) -> Map#{Key => true};
put_flag(Key, false, Map) -> maps:remove(Key, Map).

-spec install_active(map(), map()) -> map().
install_active(#{thread_rows := Rows} = Extras, State) when is_list(Rows) ->
    case fresh_rows(Extras) of
        true ->
            put_store(guild_thread_store:import(Rows), ready, State);
        false ->
            count_fetch(refetch),
            start_retry(put_store(guild_thread_store:new(), loading, State))
    end;
install_active(#{thread_kept := Tab}, State) ->
    start_retry(put_store(Tab, loading, State));
install_active(#{thread_load := {ok, Payload}}, State) when is_map(Payload) ->
    State1 = put_store(guild_thread_store:new(), ready, State),
    apply_payload(Payload, State1);
install_active(_Extras, State) ->
    start_retry(put_store(guild_thread_store:new(), loading, State)).

-spec fresh_rows(map()) -> boolean().
fresh_rows(#{thread_rows_at := At}) when is_integer(At) ->
    erlang:system_time(millisecond) - At =< ?HANDOFF_MAX_AGE_MS;
fresh_rows(_) ->
    false.

-spec put_store(ets:table(), atom(), map()) -> map().
put_store(Tab, Status, State) ->
    Data = maps:get(data, State),
    State#{
        data => Data#{thread_store => Tab},
        thread_load => #{status => Status, queue => [], queued => 0, attempt => 0}
    }.

-spec apply_payload(payload() | map(), map()) -> map().
apply_payload(Payload, State) ->
    case guild_thread_gate:store(State) of
        undefined ->
            State;
        Tab ->
            ok = guild_thread_store:load(
                Tab, maps:get(threads, Payload, []), maps:get(members, Payload, [])
            ),
            merge_parent_settings(maps:get(parent_settings, Payload, []), State)
    end.

-spec merge_parent_settings([map()], map()) -> map().
merge_parent_settings([], State) ->
    State;
merge_parent_settings(Settings, State) ->
    Data = maps:get(data, State),
    ById = maps:from_list([
        {
            snowflake_id:parse_maybe(maps:get(<<"channel_id">>, S, undefined)),
            maps:remove(<<"channel_id">>, S)
        }
     || S <- Settings, is_map(S)
    ]),
    Channels = [
        merge_parent_setting(C, ById)
     || C <- guild_data_index_channels:channel_list(Data)
    ],
    State#{data => guild_data_index:put_channels(Channels, Data)}.

-spec merge_parent_setting(map(), map()) -> map().
merge_parent_setting(Channel, ById) ->
    case
        maps:get(
            snowflake_id:parse_maybe(maps:get(<<"id">>, Channel, undefined)), ById, undefined
        )
    of
        Settings when is_map(Settings) -> maps:merge(Channel, Settings);
        _ -> Channel
    end.

-spec export_handoff(map()) -> map().
export_handoff(State) ->
    export_handoff_data(maps:get(data, State, #{}), loading(State)).

-spec export_handoff_data(map(), boolean()) -> map().
export_handoff_data(#{thread_store := Tab} = Data, false) ->
    Base = maps:remove(thread_store, Data),
    try guild_thread_store:row_count(Tab) =< ?HANDOFF_ROW_BUDGET of
        true ->
            Base#{
                thread_rows => guild_thread_store:export(Tab),
                thread_rows_at => erlang:system_time(millisecond)
            };
        false ->
            count_fetch(refetch),
            Base
    catch
        error:badarg -> Base
    end;
export_handoff_data(Data, _Loading) ->
    maps:remove(thread_store, Data).

-spec loading(map()) -> boolean().
loading(#{thread_load := #{status := Status}}) -> Status =/= ready;
loading(_) -> false.

-spec queue_event(atom(), map(), map()) -> map().
queue_event(
    Event,
    EventData,
    #{thread_load := #{queue := Queue, queued := Queued, ref := _} = Load} = State
) ->
    case Queued >= ?LOAD_QUEUE_CAP of
        true ->
            count_fetch(refetch),
            start_retry(State#{
                thread_load => Load#{
                    queue => [], queued => 0, attempt => maps:get(attempt, Load, 0) + 1
                }
            });
        false ->
            State#{
                thread_load => Load#{
                    queue => [{Event, EventData} | Queue], queued => Queued + 1
                }
            }
    end;
queue_event(_Event, _EventData, State) ->
    State.

-spec start_retry(map()) -> map().
start_retry(#{thread_load := Load0} = State) ->
    _ =
        case maps:get(timer, Load0, undefined) of
            Old when is_reference(Old) -> erlang:cancel_timer(Old);
            _ -> false
        end,
    Load = maps:remove(ref, Load0),
    Timer = erlang:send_after(
        retry_delay(maps:get(attempt, Load, 0)), self(), thread_load_retry
    ),
    State#{thread_load => Load#{status => loading, timer => Timer}}.

-spec retry_delay(non_neg_integer()) -> non_neg_integer().
retry_delay(0) -> 0;
retry_delay(Attempt) -> lists:nth(min(Attempt, length(?BACKOFF_MS)), ?BACKOFF_MS).

-spec handle_retry(map()) -> map().
handle_retry(State) ->
    Request =
        case {maps:get(id, State, undefined), maps:get(thread_load, State, undefined)} of
            {GuildId, Load} when is_integer(GuildId), is_map(Load) ->
                {guild_thread_gate:active(State), GuildId, Load};
            _ ->
                skip
        end,
    case Request of
        {true, CandidateGuildId, CandidateLoad} ->
            retry_load(CandidateGuildId, CandidateLoad, State);
        {false, _GuildId, _Load} ->
            maps:remove(thread_load, State);
        skip ->
            State
    end.

-spec retry_load(integer(), map(), map()) -> map().
retry_load(GuildId, Load, State) ->
    Self = self(),
    Ref = make_ref(),
    _ = spawn(fun() ->
        Self ! {thread_load_result, Ref, fetch_payload(GuildId)}
    end),
    State#{thread_load => Load#{ref => Ref}}.

-spec handle_load_result(reference(), term(), map()) -> map().
handle_load_result(Ref, Result, #{thread_load := #{ref := Ref} = Load} = State) ->
    Snapshot =
        case {Result, guild_thread_gate:store(State)} of
            {{ok, Payload}, Tab} when Tab =/= undefined ->
                Fresh = guild_thread_store:new(),
                State1 = replace_store(Tab, Fresh, State),
                {loaded, Payload, State1};
            {{ok, _Payload}, undefined} ->
                {done, maps:remove(thread_load, State)};
            {_Error, _Tab} ->
                {done,
                    start_retry(State#{
                        thread_load => Load#{attempt => maps:get(attempt, Load, 0) + 1}
                    })}
        end,
    case Snapshot of
        {loaded, LoadedPayload, LoadedState} when is_map(LoadedPayload) ->
            State2 = apply_payload(LoadedPayload, LoadedState#{
                thread_load => #{status => ready, queue => [], queued => 0, attempt => 0}
            }),
            State3 = replay(lists:reverse(maps:get(queue, Load, [])), State2),
            guild_thread_subscriptions:resync_after_load(State3);
        {loaded, InvalidPayload, _LoadedState} ->
            error({badmap, InvalidPayload});
        {done, FinalState} ->
            FinalState
    end;
handle_load_result(_Ref, _Result, State) ->
    State.

-spec replace_store(ets:table(), ets:table(), map()) -> map().
replace_store(Old, New, State) ->
    Data = maps:get(data, State),
    ok = guild_thread_store:retire(Old),
    State1 = State#{data => Data#{thread_store => New}},
    ok = guild_maintenance:maybe_put_permission_cache(State1),
    State1.

-spec replay([{atom(), map()}], map()) -> map().
replay(Events, State) ->
    lists:foldl(
        fun
            ({Event, _EventData}, Acc) when
                Event =:= message_delete; Event =:= message_delete_bulk
            ->
                Acc;
            ({Event, EventData}, Acc) ->
                guild_state_threads:after_replayed_event(
                    Event, EventData, guild_state_threads:apply_event(Event, EventData, Acc)
                )
        end,
        State,
        Events
    ).

-spec deactivate(map()) -> map().
deactivate(State) ->
    Data = maps:get(data, State),
    ok = guild_thread_store:retire(maps:get(thread_store, Data, undefined)),
    _ =
        case maps:get(thread_load, State, undefined) of
            #{timer := Timer} when is_reference(Timer) -> erlang:cancel_timer(Timer);
            _ -> false
        end,
    maps:remove(thread_load, State#{data => maps:remove(thread_store, Data)}).

-spec list_field(binary(), map()) -> [map()].
list_field(Key, Map) ->
    case maps:get(Key, Map, []) of
        List when is_list(List) -> [Item || Item <- List, is_map(Item)];
        _ -> []
    end.

-spec count_fetch(ok | error | refetch) -> ok.
count_fetch(Result) ->
    guild_thread_store:add_counter(?FETCH_COUNTS_KEY, fetch_index(Result), 1).

-spec init_counters() -> ok.
init_counters() ->
    guild_thread_store:ensure_counter(?FETCH_COUNTS_KEY, 3).

-spec fetch_index(fetch_result()) -> 1..3.
fetch_index(ok) -> 1;
fetch_index(error) -> 2;
fetch_index(refetch) -> 3.

-spec fetch_counts() -> #{(ok | error | refetch) => non_neg_integer()}.
fetch_counts() ->
    case persistent_term:get(?FETCH_COUNTS_KEY, undefined) of
        undefined ->
            #{};
        Counters ->
            maps:from_list([
                {R, counters:get(Counters, fetch_index(R))}
             || R <- [ok, error, refetch]
            ])
    end.
