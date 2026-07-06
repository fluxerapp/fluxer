%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(process_registry).
-typing([eqwalizer]).

-export([
    init/0,
    build_process_key/2,
    build_process_name/2,
    registry_whereis/1,
    registry_unregister/1,
    register_and_monitor/3,
    lookup_or_monitor/3,
    safe_unregister/1,
    safe_unregister/2,
    cleanup_on_down/2,
    cleanup_on_down/3,
    get_count/1,
    force_stop_process/1
]).

-export_type([
    process_id/0,
    process_prefix/0,
    process_key/0,
    process_map/0,
    register_result/0,
    lookup_result/0
]).

-define(REGISTRY_TABLE, process_registry_table).

-type process_id() :: integer() | binary() | string().
-type process_prefix() :: atom() | string().
-type process_key() :: {atom(), integer() | binary()}.
-type process_map() :: #{term() => {pid(), reference()} | loading}.
-type register_result() :: {ok, pid(), reference(), process_map()} | {error, term()}.
-type lookup_result() :: {ok, pid(), reference(), process_map()} | {error, not_found}.

-spec init() -> ok.
init() ->
    _ =
        case ets:info(?REGISTRY_TABLE) of
            undefined ->
                _ = ets:new(?REGISTRY_TABLE, [
                    named_table, public, set, {read_concurrency, true}
                ]);
            _ ->
                ok
        end,
    ok.

-spec build_process_key(process_prefix(), process_id()) -> process_key().
build_process_key(Prefix, Id) when is_atom(Prefix), is_integer(Id) ->
    {Prefix, Id};
build_process_key(Prefix, Id) when is_atom(Prefix), is_binary(Id) ->
    {Prefix, Id};
build_process_key(Prefix, Id) when is_atom(Prefix), is_list(Id) ->
    {Prefix, list_to_binary(Id)};
build_process_key(Prefix, Id) when is_list(Prefix), is_integer(Id) ->
    {prefix_to_atom(Prefix), Id};
build_process_key(Prefix, Id) when is_list(Prefix), is_binary(Id) ->
    {prefix_to_atom(Prefix), Id};
build_process_key(Prefix, Id) when is_list(Prefix), is_list(Id) ->
    {prefix_to_atom(Prefix), list_to_binary(Id)}.

-spec prefix_to_atom(string()) -> atom().
prefix_to_atom("call") -> call;
prefix_to_atom("channel") -> channel;
prefix_to_atom("guild") -> guild;
prefix_to_atom("presence") -> presence;
prefix_to_atom("session") -> session;
prefix_to_atom("session_group") -> session_group;
prefix_to_atom("voice") -> voice.

-spec build_process_name(process_prefix(), process_id()) -> process_key().
build_process_name(Prefix, Id) ->
    build_process_key(Prefix, Id).

-spec registry_whereis(process_key()) -> pid() | undefined.
registry_whereis(Key) ->
    case ets:lookup(?REGISTRY_TABLE, Key) of
        [{Key, Pid}] -> live_registered_pid(Key, Pid);
        [] -> undefined
    end.

-spec live_registered_pid(process_key(), term()) -> pid() | undefined.
live_registered_pid(_Key, Pid) when not is_pid(Pid) ->
    undefined;
live_registered_pid(Key, Pid) ->
    case process_liveness:is_alive(Pid) of
        true ->
            Pid;
        false ->
            registry_unregister_if_pid(Key, Pid),
            undefined
    end.

-spec registry_unregister(process_key()) -> ok.
registry_unregister(Key) ->
    try
        ets:delete(?REGISTRY_TABLE, Key),
        ok
    catch
        error:badarg ->
            ok;
        _:_ ->
            ok
    end.

-spec register_and_monitor(process_key(), pid(), process_map()) -> register_result().
register_and_monitor(Key, Pid, ProcessMap) ->
    try
        case ets:insert_new(?REGISTRY_TABLE, {Key, Pid}) of
            true ->
                Ref = monitor(process, Pid),
                NewMap = ProcessMap#{Key => {Pid, Ref}},
                {ok, Pid, Ref, NewMap};
            false ->
                force_stop_process(Pid),
                register_existing_process(Key, ProcessMap)
        end
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

-spec register_existing_process(process_key(), process_map()) -> register_result().
register_existing_process(Key, ProcessMap) ->
    case registry_whereis(Key) of
        undefined ->
            {error, registration_race_condition};
        ExistingPid ->
            monitor_into_map(Key, ExistingPid, ProcessMap)
    end.

-spec force_stop_process(pid()) -> ok.
force_stop_process(Pid) ->
    MRef = monitor(process, Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', MRef, process, Pid, _} -> ok
    after 3000 ->
        exit(Pid, kill),
        receive
            {'DOWN', MRef, process, Pid, _} -> ok
        after 2000 ->
            demonitor(MRef, [flush]),
            ok
        end
    end.

-spec lookup_or_monitor(process_key(), term(), process_map()) -> lookup_result().
lookup_or_monitor(Key, MapKey, ProcessMap) ->
    case registry_whereis(Key) of
        undefined ->
            {error, not_found};
        Pid ->
            monitor_into_map(MapKey, Pid, ProcessMap)
    end.

-spec monitor_into_map(term(), pid(), process_map()) -> lookup_result().
monitor_into_map(MapKey, Pid, ProcessMap) ->
    case maps:get(MapKey, ProcessMap, undefined) of
        {Pid, ExistingRef} when is_reference(ExistingRef) ->
            {ok, Pid, ExistingRef, ProcessMap};
        Existing ->
            demonitor_existing(Existing),
            Ref = monitor(process, Pid),
            {ok, Pid, Ref, ProcessMap#{MapKey => {Pid, Ref}}}
    end.

-spec demonitor_existing(term()) -> ok.
demonitor_existing({_OldPid, OldRef}) when is_reference(OldRef) ->
    demonitor(OldRef, [flush]),
    ok;
demonitor_existing(_) ->
    ok.

-spec safe_unregister(process_key()) -> ok.
safe_unregister(Key) ->
    registry_unregister(Key).

-spec safe_unregister(process_key(), pid()) -> ok.
safe_unregister(Key, Pid) when is_pid(Pid) ->
    registry_unregister_if_pid(Key, Pid);
safe_unregister(Key, _Pid) ->
    registry_unregister(Key).

-spec cleanup_on_down(pid(), process_map()) -> process_map().
cleanup_on_down(DeadPid, ProcessMap) ->
    maps:filter(
        fun
            (_Key, loading) ->
                true;
            (_Key, {Pid, _Ref}) ->
                Pid =/= DeadPid
        end,
        ProcessMap
    ).

-spec cleanup_on_down(process_prefix(), pid(), process_map()) -> process_map().
cleanup_on_down(Prefix, DeadPid, ProcessMap) ->
    maps:fold(
        fun
            (Id, {Pid, _Ref}, Acc) when Pid =:= DeadPid ->
                maybe_unregister_dead_entry(Prefix, Id, DeadPid),
                Acc;
            (Id, Value, Acc) ->
                Acc#{Id => Value}
        end,
        #{},
        ProcessMap
    ).

-spec maybe_unregister_dead_entry(process_prefix(), term(), pid()) -> ok.
maybe_unregister_dead_entry(Prefix, Id, DeadPid) ->
    case safe_process_key(Prefix, Id) of
        {ok, Key} -> registry_unregister_if_pid(Key, DeadPid);
        error -> ok
    end.

-spec safe_process_key(process_prefix(), term()) -> {ok, process_key()} | error.
safe_process_key(Prefix, Id) when is_integer(Id); is_binary(Id) ->
    try build_process_key(Prefix, Id) of
        Key -> {ok, Key}
    catch
        _:_ -> error
    end;
safe_process_key(_Prefix, _Id) ->
    error.

-spec registry_unregister_if_pid(process_key(), pid()) -> ok.
registry_unregister_if_pid(Key, Pid) ->
    try
        ets:delete_object(?REGISTRY_TABLE, {Key, Pid}),
        ok
    catch
        error:badarg ->
            ok;
        _:_ ->
            ok
    end.

-spec get_count(process_map()) -> non_neg_integer().
get_count(ProcessMap) ->
    Pids = [Pid || {Pid, _Ref} <- maps:values(ProcessMap), is_pid(Pid)],
    Liveness = process_liveness:are_alive(Pids),
    lists:foldl(
        fun(Item, Acc) -> count_live_process(Item, Liveness, Acc) end,
        0,
        maps:values(ProcessMap)
    ).

-spec count_live_process(
    loading | {pid(), reference()}, #{term() => boolean()}, non_neg_integer()
) ->
    non_neg_integer().
count_live_process(loading, _Liveness, Acc) ->
    Acc;
count_live_process({Pid, _Ref}, Liveness, Acc) ->
    case maps:get(Pid, Liveness, false) of
        true -> Acc + 1;
        false -> Acc
    end.
