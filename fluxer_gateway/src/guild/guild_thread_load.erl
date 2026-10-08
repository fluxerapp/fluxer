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

-type guild_state() :: map().
-type gate() :: #{active := boolean(), version := non_neg_integer()}.
-type payload() :: #{threads := [map()], members := [map()], parent_settings := [map()]}.
-type fetch_result() :: ok | error | refetch.

-export_type([payload/0, gate/0]).

-spec channels_collection(map()) -> {[map()], map()}.
channels_collection(Response) ->
    Channels = list_field(<<"channels">>, Response),
    Gate = parse_gate(maps:get(<<"thread_gate">>, Response, undefined)),
    Tainted = maps:get(<<"thread_tainted">>, Response, false) =:= true,
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
    case rpc_client:call(Request) of
        {ok, #{<<"members">> := Members} = Page} when is_list(Members) ->
            Acc1 = [[M || M <- Members, is_map(M)] | Acc],
            case next_member_cursor(Page, ThreadIds) of
                done ->
                    {ok, Acc1};
                {Remaining, Next} ->
                    fetch_member_pages(GuildId, Remaining, Next, Acc1, PagesLeft - 1);
                invalid ->
                    {error, invalid_thread_members_page}
            end;
        {ok, _Invalid} ->
            {error, invalid_thread_members_page};
        {error, Reason} ->
            {error, Reason}
    end.

-spec next_member_cursor(map(), [binary()]) ->
    done | invalid | {[binary()], binary() | undefined}.
next_member_cursor(
    #{<<"has_more">> := true, <<"next_thread_id">> := ThreadId} = Page, ThreadIds
) when
    is_binary(ThreadId)
->
    case lists:dropwhile(fun(Id) -> Id =/= ThreadId end, ThreadIds) of
        [] ->
            invalid;
        Remaining ->
            case maps:get(<<"next_after_user_id">>, Page, null) of
                Next when is_binary(Next) -> {Remaining, Next};
                null -> {Remaining, undefined};
                _ -> invalid
            end
    end;
next_member_cursor(#{<<"has_more">> := true}, _ThreadIds) ->
    invalid;
next_member_cursor(_Page, _ThreadIds) ->
    done.

-spec load_payload(map()) -> payload().
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

-spec parse_gate(term()) -> gate().
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

-spec install(map(), guild_state()) -> guild_state().
install(Extras, State) ->
    Data = maps:get(data, State),
    Gate = maps:get(thread_gate, Extras, #{active => false, version => 0}),
    Tainted = maps:get(thread_tainted, Extras, false) =:= true,
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

-spec any_viewer(guild_state()) -> boolean().
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

-spec install_active(map(), guild_state()) -> guild_state().
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

-spec put_store(ets:table(), atom(), guild_state()) -> guild_state().
put_store(Tab, Status, State) ->
    Data = maps:get(data, State),
    State#{
        data => Data#{thread_store => Tab},
        thread_load => #{status => Status, queue => [], queued => 0, attempt => 0}
    }.

-spec apply_payload(payload() | map(), guild_state()) -> guild_state().
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

-spec merge_parent_settings([map()], guild_state()) -> guild_state().
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
    Channels = [merge_parent_setting(C, ById) || C <- guild_data_index:channel_list(Data)],
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

-spec export_handoff(guild_state()) -> map().
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

-spec loading(guild_state()) -> boolean().
loading(#{thread_load := #{status := Status}}) -> Status =/= ready;
loading(_) -> false.

-spec queue_event(atom(), map(), guild_state()) -> guild_state().
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

-spec start_retry(guild_state()) -> guild_state().
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

-spec handle_retry(guild_state()) -> guild_state().
handle_retry(State) ->
    case {maps:get(id, State, undefined), maps:get(thread_load, State, undefined)} of
        {GuildId, Load} when is_integer(GuildId), is_map(Load) ->
            retry_load(GuildId, Load, State);
        _ ->
            State
    end.

-spec retry_load(integer(), map(), guild_state()) -> guild_state().
retry_load(GuildId, Load, State) ->
    case guild_thread_gate:active(State) of
        false ->
            maps:remove(thread_load, State);
        true ->
            Self = self(),
            Ref = make_ref(),
            _ = spawn(fun() ->
                Self ! {thread_load_result, Ref, fetch_payload(GuildId)}
            end),
            State#{thread_load => Load#{ref => Ref}}
    end.

-spec handle_load_result(reference(), term(), guild_state()) -> guild_state().
handle_load_result(Ref, Result, #{thread_load := #{ref := Ref} = Load} = State) ->
    case {Result, guild_thread_gate:store(State)} of
        {{ok, Payload}, Tab} when Tab =/= undefined ->
            Fresh = guild_thread_store:new(),
            State1 = replace_store(Tab, Fresh, State),
            State2 = apply_payload(Payload, State1#{
                thread_load => #{status => ready, queue => [], queued => 0, attempt => 0}
            }),
            State3 = replay(lists:reverse(maps:get(queue, Load, [])), State2),
            guild_thread_subscriptions:resync_after_load(State3);
        {{ok, _Payload}, undefined} ->
            maps:remove(thread_load, State);
        {_Error, _Tab} ->
            start_retry(State#{
                thread_load => Load#{attempt => maps:get(attempt, Load, 0) + 1}
            })
    end;
handle_load_result(_Ref, _Result, State) ->
    State.

-spec replace_store(ets:table(), ets:table(), guild_state()) -> guild_state().
replace_store(Old, New, State) ->
    Data = maps:get(data, State),
    ok = guild_thread_store:retire(Old),
    State1 = State#{data => Data#{thread_store => New}},
    ok = guild_maintenance:maybe_put_permission_cache(State1),
    State1.

-spec replay([{atom(), map()}], guild_state()) -> guild_state().
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

-spec deactivate(guild_state()) -> guild_state().
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

-spec fetch_counts() -> #{fetch_result() => non_neg_integer()}.
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

channels_collection_inactive_keeps_channels_test() ->
    Response = #{<<"channels">> => [#{<<"id">> => <<"1">>}]},
    ?assertEqual(
        {[#{<<"id">> => <<"1">>}], #{
            thread_gate => #{active => false, version => 0}, thread_tainted => false
        }},
        channels_collection(Response)
    ).

channels_collection_active_merges_forums_and_defers_threads_test() ->
    Response = #{
        <<"channels">> => [#{<<"id">> => <<"1">>}],
        <<"thread_only_channels">> => [#{<<"id">> => <<"2">>, <<"type">> => 15}],
        <<"thread_gate">> => #{<<"active">> => true, <<"config_version">> => 4},
        <<"thread_tainted">> => true,
        <<"threads">> => [
            #{
                <<"id">> => <<"10">>,
                <<"parent_id">> => <<"1">>,
                <<"type">> => 11,
                <<"member_ids_preview">> => [<<"7">>]
            }
        ],
        <<"thread_members">> => [
            #{<<"id">> => <<"10">>, <<"user_id">> => <<"7">>, <<"flags">> => 1}
        ]
    },
    {Channels, Extras} = channels_collection(Response),
    ?assertEqual([<<"1">>, <<"2">>], [maps:get(<<"id">>, C) || C <- Channels]),
    ?assertEqual(
        #{thread_gate => #{active => true, version => 4}, thread_tainted => true}, Extras
    ).

load_payload_normalizes_threads_and_members_test() ->
    Payload = load_payload(#{
        <<"threads">> => [
            #{
                <<"id">> => <<"10">>,
                <<"parent_id">> => <<"1">>,
                <<"type">> => 11,
                <<"member_ids_preview">> => [<<"7">>]
            }
        ],
        <<"thread_members">> => [
            #{<<"id">> => <<"10">>, <<"user_id">> => <<"7">>, <<"flags">> => 1}
        ]
    }),
    #{threads := [Thread], members := [Member]} = Payload,
    ?assertEqual(10, maps:get(<<"id">>, Thread)),
    ?assertEqual([7], maps:get(<<"member_ids_preview">>, Thread)),
    ?assertEqual(7, maps:get(<<"user_id">>, Member)).

take_extras_strips_the_guild_map_test() ->
    Data = #{
        <<"guild">> => #{<<"id">> => <<"5">>, <<"thread_gate">> => #{<<"active">> => true}},
        thread_tainted => true
    },
    {Extras, Rest} = take_extras(Data),
    ?assertEqual(#{<<"guild">> => #{<<"id">> => <<"5">>}}, Rest),
    ?assertEqual(
        #{thread_gate => #{active => true, version => 0}, thread_tainted => true}, Extras
    ).

handoff_export_respects_the_row_budget_test() ->
    Tab = guild_thread_store:new(),
    ok = guild_thread_store:put_thread(Tab, #{<<"id">> => 10, <<"parent_id">> => 1}),
    Data = #{thread_store => Tab, thread_gate => #{active => true}},
    Exported = export_handoff(#{
        data => Data, thread_load => #{status => ready, queue => [], queued => 0}
    }),
    ?assertNot(maps:is_key(thread_store, Exported)),
    ?assertEqual(2, length(maps:get(thread_rows, Exported))),
    guild_thread_store:destroy(Tab).

handoff_export_while_loading_forces_a_refetch_test() ->
    Tab = guild_thread_store:new(),
    ok = guild_thread_store:put_thread(Tab, #{<<"id">> => 10, <<"parent_id">> => 1}),
    Gate = #{active => true, version => 1},
    Exported = export_handoff(#{
        data => #{thread_store => Tab, thread_gate => Gate},
        thread_load => #{status => loading, queue => [], queued => 0}
    }),
    guild_thread_store:destroy(Tab),
    ?assertNot(maps:is_key(thread_store, Exported)),
    ?assertNot(maps:is_key(thread_rows, Exported)),
    {Extras, _} = take_extras(Exported),
    Installed = install(Extras, #{id => 5, data => #{}, sessions => #{}}),
    ?assert(loading(Installed)),
    #{thread_load := #{timer := Timer}} = Installed,
    erlang:cancel_timer(Timer),
    deactivate(Installed).

install_imports_fresh_rows_and_refetches_stale_ones_test() ->
    Tab = guild_thread_store:new(),
    ok = guild_thread_store:put_thread(Tab, #{<<"id">> => 10, <<"parent_id">> => 1}),
    Rows = guild_thread_store:export(Tab),
    guild_thread_store:destroy(Tab),
    Base = #{id => 5, data => #{<<"channels">> => []}, sessions => #{}},
    Gate = #{active => true, version => 1},
    Fresh = install(
        #{
            thread_gate => Gate,
            thread_rows => Rows,
            thread_rows_at => erlang:system_time(millisecond)
        },
        Base
    ),
    ?assertNotEqual(undefined, guild_thread_gate:thread(10, Fresh)),
    ?assertNot(loading(Fresh)),
    Stale = install(#{thread_gate => Gate, thread_rows => Rows, thread_rows_at => 0}, Base),
    ?assert(loading(Stale)),
    ?assertEqual(undefined, guild_thread_gate:thread(10, Stale)),
    #{thread_load := #{timer := Timer}} = Stale,
    erlang:cancel_timer(Timer),
    deactivate(Fresh),
    deactivate(Stale).

fetch_flip_data_pages_thread_members_test() ->
    meck:new(rpc_client, [no_link]),
    meck:expect(rpc_client, call, fun
        (#{<<"type">> := <<"guild_thread_flip_data">>, <<"paged_members">> := true}) ->
            {ok, #{
                <<"config_version">> => 1,
                <<"threads">> => [#{<<"id">> => <<"10">>}, #{<<"id">> => <<"11">>}]
            }};
        (
            #{<<"type">> := <<"list_thread_members">>, <<"thread_ids">> := [<<"10">>, <<"11">>]} =
                R
        ) ->
            case maps:get(<<"after_user_id">>, R, undefined) of
                undefined ->
                    {ok, #{
                        <<"members">> => [#{<<"id">> => <<"10">>, <<"user_id">> => <<"7">>}],
                        <<"has_more">> => true,
                        <<"next_thread_id">> => <<"10">>,
                        <<"next_after_user_id">> => <<"7">>
                    }};
                <<"7">> ->
                    {ok, #{
                        <<"members">> => [#{<<"id">> => <<"10">>, <<"user_id">> => <<"8">>}],
                        <<"has_more">> => true,
                        <<"next_thread_id">> => <<"11">>,
                        <<"next_after_user_id">> => null
                    }}
            end;
        (#{<<"type">> := <<"list_thread_members">>, <<"thread_ids">> := [<<"11">>]} = R) ->
            ?assertNot(maps:is_key(<<"after_user_id">>, R)),
            {ok, #{
                <<"members">> => [#{<<"id">> => <<"11">>, <<"user_id">> => <<"9">>}],
                <<"has_more">> => false
            }}
    end),
    try
        {ok, Data} = fetch_flip_data(5, 1),
        ?assertEqual(
            [<<"7">>, <<"8">>, <<"9">>],
            [maps:get(<<"user_id">>, M) || M <- maps:get(<<"thread_members">>, Data)]
        ),
        ?assertEqual(4, meck:num_calls(rpc_client, call, '_'))
    after
        meck:unload(rpc_client)
    end.

fetch_flip_data_batches_many_threads_per_rpc_test() ->
    Ids = [integer_to_binary(N) || N <- lists:seq(1, 250)],
    meck:new(rpc_client, [no_link]),
    meck:expect(rpc_client, call, fun
        (#{<<"type">> := <<"guild_thread_flip_data">>}) ->
            {ok, #{
                <<"config_version">> => 1, <<"threads">> => [#{<<"id">> => Id} || Id <- Ids]
            }};
        (#{<<"type">> := <<"list_thread_members">>, <<"thread_ids">> := Batch}) ->
            {ok, #{
                <<"members">> => [#{<<"id">> => Id, <<"user_id">> => <<"7">>} || Id <- Batch],
                <<"has_more">> => false
            }}
    end),
    try
        {ok, Data} = fetch_flip_data(5, 1),
        ?assertEqual(Ids, [maps:get(<<"id">>, M) || M <- maps:get(<<"thread_members">>, Data)]),
        ?assertEqual(4, meck:num_calls(rpc_client, call, '_'))
    after
        meck:unload(rpc_client)
    end.

fetch_flip_data_rejects_a_cursor_outside_the_batch_test() ->
    meck:new(rpc_client, [no_link]),
    meck:expect(rpc_client, call, fun
        (#{<<"type">> := <<"guild_thread_flip_data">>}) ->
            {ok, #{<<"config_version">> => 1, <<"threads">> => [#{<<"id">> => <<"10">>}]}};
        (#{<<"type">> := <<"list_thread_members">>}) ->
            {ok, #{
                <<"members">> => [], <<"has_more">> => true, <<"next_thread_id">> => <<"99">>
            }}
    end),
    try
        ?assertEqual({error, invalid_thread_members_page}, fetch_flip_data(5, 1))
    after
        meck:unload(rpc_client)
    end.

loaded_forum_without_a_config_row_gets_default_surface_fields_test() ->
    Forum = #{<<"id">> => <<"2">>, <<"type">> => 15, <<"name">> => <<"forum">>},
    Data = guild_data_index:put_channels([Forum], #{}),
    Settings = #{
        <<"channel_id">> => <<"2">>,
        <<"available_tags">> => [],
        <<"default_tag_setting">> => <<"match_some">>,
        <<"default_forum_layout">> => 0,
        <<"flags">> => 0
    },
    State = install(
        #{
            thread_gate => #{active => true, version => 1},
            thread_load =>
                {ok, #{threads => [], members => [], parent_settings => [Settings]}}
        },
        #{id => 5, data => Data, sessions => #{}}
    ),
    [Loaded] = guild_data_index:channel_list(maps:get(data, State)),
    ?assertEqual([], maps:get(<<"available_tags">>, Loaded)),
    ?assertEqual(<<"match_some">>, maps:get(<<"default_tag_setting">>, Loaded)),
    ?assertEqual(0, maps:get(<<"default_forum_layout">>, Loaded)),
    deactivate(State).

fetch_flip_data_fails_on_a_failed_member_page_test() ->
    meck:new(rpc_client, [no_link]),
    meck:expect(rpc_client, call, fun
        (#{<<"type">> := <<"guild_thread_flip_data">>}) ->
            {ok, #{<<"config_version">> => 1, <<"threads">> => [#{<<"id">> => <<"10">>}]}};
        (#{<<"type">> := <<"list_thread_members">>}) ->
            {error, timeout}
    end),
    try
        ?assertEqual({error, timeout}, fetch_flip_data(5, 1))
    after
        meck:unload(rpc_client)
    end.

fetch_flip_data_fails_when_a_member_page_throws_test() ->
    meck:new(rpc_client, [no_link]),
    meck:expect(rpc_client, call, fun
        (#{<<"type">> := <<"guild_thread_flip_data">>}) ->
            {ok, #{<<"config_version">> => 1, <<"threads">> => [#{<<"id">> => <<"10">>}]}};
        (#{<<"type">> := <<"list_thread_members">>}) ->
            error(circuit_open)
    end),
    try
        ?assertEqual({error, {error, circuit_open}}, fetch_flip_data(5, 1))
    after
        meck:unload(rpc_client)
    end.

fetch_flip_data_keeps_inline_members_from_an_older_api_test() ->
    Inline = #{
        <<"config_version">> => 1,
        <<"threads">> => [#{<<"id">> => <<"10">>}],
        <<"thread_members">> => [#{<<"id">> => <<"10">>, <<"user_id">> => <<"7">>}]
    },
    meck:new(rpc_client, [no_link]),
    meck:expect(rpc_client, call, fun(_) -> {ok, Inline} end),
    try
        ?assertEqual({ok, Inline}, fetch_flip_data(5, 1)),
        ?assertEqual(1, meck:num_calls(rpc_client, call, '_'))
    after
        meck:unload(rpc_client)
    end.

install_inactive_creates_no_store_test() ->
    State = install(#{}, #{id => 5, data => #{}, sessions => #{}}),
    ?assertEqual(#{}, maps:get(data, State)),
    ?assertNot(maps:is_key(thread_load, State)).

queue_overflow_triggers_refetch_test() ->
    Stale = make_ref(),
    State0 = #{
        id => 5,
        data => #{thread_gate => #{active => true, version => 1}},
        thread_load => #{
            status => loading,
            queue => [],
            queued => ?LOAD_QUEUE_CAP,
            attempt => 0,
            ref => Stale
        }
    },
    State = queue_event(thread_update, #{}, State0),
    #{thread_load := #{queued := 0, queue := [], attempt := 1, timer := Timer} = Load} = State,
    ?assertNot(maps:is_key(ref, Load)),
    ?assertEqual(State, handle_load_result(Stale, {ok, #{}}, State)),
    Remaining = erlang:cancel_timer(Timer),
    ?assert(is_integer(Remaining) andalso Remaining > 25000).

deactivate_orphans_an_inflight_fetch_test() ->
    Stale = make_ref(),
    State = deactivate(#{
        id => 5,
        data => #{},
        thread_load => #{status => loading, ref => Stale}
    }),
    ?assertNot(maps:is_key(thread_load, State)),
    ?assertEqual(State, handle_load_result(Stale, {ok, #{}}, State)).

events_during_a_backoff_are_not_queued_test() ->
    ok = guild_thread_store:init(),
    Tab = guild_thread_store:new(),
    Timer = erlang:send_after(600000, self(), thread_load_retry),
    State0 = #{
        id => 5,
        data => #{thread_store => Tab, thread_gate => #{active => true, version => 1}},
        thread_load => #{
            status => loading, queue => [], queued => 0, attempt => 3, timer => Timer
        }
    },
    Message = #{<<"id">> => <<"400">>, <<"channel_id">> => <<"300">>, <<"type">> => 0},
    try
        State = lists:foldl(
            fun(_, Acc) ->
                guild_state_threads:after_regular_event(message_create, Message, Acc)
            end,
            State0,
            lists:seq(1, ?LOAD_QUEUE_CAP + 1)
        ),
        ?assertMatch(
            #{thread_load := #{queued := 0, queue := [], attempt := 3, timer := Timer}}, State
        ),
        ?assert(is_integer(erlang:read_timer(Timer)))
    after
        erlang:cancel_timer(Timer),
        guild_thread_store:destroy(Tab)
    end.

queued_deletes_do_not_replay_onto_the_snapshot_test() ->
    ok = guild_thread_store:init(),
    Loading = guild_thread_store:new(),
    Ref = make_ref(),
    State0 = #{
        id => 5,
        disable_permission_cache_updates => true,
        sessions => #{},
        data => #{thread_store => Loading, thread_gate => #{active => true, version => 1}},
        thread_load => #{status => loading, queue => [], queued => 0, attempt => 0, ref => Ref}
    },
    State1 = guild_state_threads:after_regular_event(
        message_delete, #{<<"id">> => <<"401">>, <<"channel_id">> => <<"300">>}, State0
    ),
    State2 = guild_state_threads:after_regular_event(
        message_delete_bulk,
        #{<<"ids">> => [<<"402">>, <<"403">>], <<"channel_id">> => <<"300">>},
        State1
    ),
    ?assertMatch(#{thread_load := #{queued := 2}}, State2),
    Payload = load_payload(#{
        <<"threads">> => [
            #{
                <<"id">> => <<"300">>,
                <<"parent_id">> => <<"100">>,
                <<"type">> => 11,
                <<"last_message_id">> => <<"400">>,
                <<"message_count">> => 5,
                <<"total_message_sent">> => 8,
                <<"thread_metadata">> => #{<<"archived">> => false}
            }
        ]
    }),
    State3 = handle_load_result(Ref, {ok, Payload}, State2),
    Fresh = guild_thread_gate:store(State3),
    try
        ?assertEqual(
            5, maps:get(<<"message_count">>, guild_thread_store:get_thread(Fresh, 300))
        )
    after
        guild_thread_store:destroy(Fresh),
        guild_thread_store:destroy(Loading)
    end.

regular_channel_messages_during_a_load_are_not_queued_test() ->
    ok = guild_thread_store:init(),
    Tab = guild_thread_store:new(),
    Ref = make_ref(),
    State0 = #{
        id => 5,
        disable_permission_cache_updates => true,
        sessions => #{},
        data => #{
            thread_store => Tab,
            thread_gate => #{active => true, version => 1},
            <<"channels">> => [#{<<"id">> => <<"100">>, <<"type">> => 0}]
        },
        thread_load => #{status => loading, queue => [], queued => 0, attempt => 1, ref => Ref}
    },
    try
        State1 = lists:foldl(
            fun(N, Acc) ->
                guild_state_threads:after_regular_event(
                    message_create,
                    #{<<"id">> => integer_to_binary(1000 + N), <<"channel_id">> => <<"100">>},
                    Acc
                )
            end,
            State0,
            lists:seq(1, ?LOAD_QUEUE_CAP + 1)
        ),
        ?assertMatch(#{thread_load := #{queued := 0, ref := Ref, attempt := 1}}, State1),
        ?assertNot(maps:is_key(timer, maps:get(thread_load, State1))),
        State2 = guild_state_threads:after_regular_event(
            message_create, #{<<"id">> => <<"5000">>, <<"channel_id">> => <<"300">>}, State1
        ),
        ?assertMatch(#{thread_load := #{queued := 1}}, State2)
    after
        guild_thread_store:retire(Tab)
    end.

store_events_during_a_load_replay_onto_the_fresh_store_test() ->
    ok = guild_thread_store:init(),
    Loading = guild_thread_store:new(),
    Ref = make_ref(),
    State0 = #{
        id => 5,
        disable_permission_cache_updates => true,
        sessions => #{},
        data => #{thread_store => Loading, thread_gate => #{active => true, version => 1}},
        thread_load => #{status => loading, queue => [], queued => 0, attempt => 0, ref => Ref}
    },
    Message = #{<<"id">> => <<"400">>, <<"channel_id">> => <<"300">>, <<"type">> => 0},
    State1 = guild_state_threads:after_regular_event(message_create, Message, State0),
    State2 = guild_state_threads:after_regular_event(
        channel_delete, #{<<"id">> => <<"200">>, <<"type">> => 0}, State1
    ),
    State3 = guild_state_threads:after_regular_event(
        guild_member_remove, #{<<"user">> => #{<<"id">> => <<"7">>}}, State2
    ),
    Thread = fun(Id, Parent) ->
        #{
            <<"id">> => integer_to_binary(Id),
            <<"parent_id">> => integer_to_binary(Parent),
            <<"type">> => 11,
            <<"message_count">> => 0,
            <<"total_message_sent">> => 0,
            <<"thread_metadata">> => #{<<"archived">> => false}
        }
    end,
    Payload = load_payload(#{
        <<"threads">> => [Thread(300, 100), Thread(301, 200)],
        <<"thread_members">> => [#{<<"id">> => <<"300">>, <<"user_id">> => <<"7">>}]
    }),
    State4 = handle_load_result(Ref, {ok, Payload}, State3),
    Fresh = guild_thread_gate:store(State4),
    try
        ?assertNotEqual(Loading, Fresh),
        ?assertNot(loading(State4)),
        Loaded = guild_thread_store:get_thread(Fresh, 300),
        ?assertEqual(400, snowflake_id:parse_maybe(maps:get(<<"last_message_id">>, Loaded))),
        ?assertEqual(1, maps:get(<<"message_count">>, Loaded)),
        ?assertEqual(undefined, guild_thread_store:get_thread(Fresh, 301)),
        ?assertEqual([], guild_thread_store:member_ids(Fresh, 300)),
        State5 = replay([{message_create, Message}], State4),
        ?assertEqual(
            1, maps:get(<<"message_count">>, guild_thread_store:get_thread(Fresh, 300))
        ),
        _ = State5
    after
        guild_thread_store:destroy(Fresh),
        guild_thread_store:destroy(Loading)
    end.

-endif.
