%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(process_registry).

-export([
    build_process_name/2,
    register_and_monitor/3,
    lookup_or_monitor/3,
    safe_unregister/1,
    cleanup_on_down/2,
    get_count/1
]).

-type process_id() :: integer() | binary() | string().
-type process_prefix() :: atom() | string().
-type process_map() :: #{term() => {pid(), reference()} | loading}.
-type register_result() :: {ok, pid(), reference(), process_map()} | {error, term()}.
-type lookup_result() :: {ok, pid(), reference(), process_map()} | {error, not_found}.

-export_type([process_id/0, process_prefix/0, process_map/0]).

-spec build_process_name(process_prefix(), process_id()) -> atom().
build_process_name(Prefix, Id) when is_atom(Prefix), is_integer(Id) ->
    list_to_atom(atom_to_list(Prefix) ++ "_" ++ integer_to_list(Id));
build_process_name(Prefix, Id) when is_atom(Prefix), is_binary(Id) ->
    list_to_atom(atom_to_list(Prefix) ++ "_" ++ binary_to_list(Id));
build_process_name(Prefix, Id) when is_atom(Prefix), is_list(Id) ->
    list_to_atom(atom_to_list(Prefix) ++ "_" ++ Id);
build_process_name(Prefix, Id) when is_list(Prefix), is_integer(Id) ->
    list_to_atom(Prefix ++ "_" ++ integer_to_list(Id));
build_process_name(Prefix, Id) when is_list(Prefix), is_binary(Id) ->
    list_to_atom(Prefix ++ "_" ++ binary_to_list(Id));
build_process_name(Prefix, Id) when is_list(Prefix), is_list(Id) ->
    list_to_atom(Prefix ++ "_" ++ Id).

-spec register_and_monitor(atom(), pid(), process_map()) -> register_result().
register_and_monitor(Name, Pid, ProcessMap) ->
    try
        register(Name, Pid),
        Ref = monitor(process, Pid),
        NewMap = maps:put(Name, {Pid, Ref}, ProcessMap),
        {ok, Pid, Ref, NewMap}
    catch
        error:badarg ->
            force_stop_process(Pid),
            case whereis(Name) of
                undefined ->
                    {error, registration_race_condition};
                ExistingPid ->
                    ExistingRef = monitor(process, ExistingPid),
                    ExistingMap = maps:put(Name, {ExistingPid, ExistingRef}, ProcessMap),
                    {ok, ExistingPid, ExistingRef, ExistingMap}
            end;
        Error:Reason ->
            {error, {Error, Reason}}
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

-spec lookup_or_monitor(atom(), term(), process_map()) -> lookup_result().
lookup_or_monitor(Name, Key, ProcessMap) ->
    case whereis(Name) of
        undefined ->
            {error, not_found};
        Pid ->
            Ref = monitor(process, Pid),
            NewMap = maps:put(Key, {Pid, Ref}, ProcessMap),
            {ok, Pid, Ref, NewMap}
    end.

-spec safe_unregister(atom()) -> ok.
safe_unregister(Name) ->
    try
        unregister(Name),
        ok
    catch
        error:badarg ->
            ok;
        _:_ ->
            ok
    end.

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

-spec get_count(process_map()) -> non_neg_integer().
get_count(ProcessMap) ->
    maps:size(
        maps:filter(
            fun
                (_Key, loading) -> false;
                (_Key, {_Pid, _Ref}) -> true
            end,
            ProcessMap
        )
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_process_name_integer_atom_test() ->
    ?assertEqual('guild_123456', build_process_name(guild, 123456)),
    ?assertEqual('channel_0', build_process_name(channel, 0)),
    ?assertEqual('voice_999', build_process_name(voice, 999)).

build_process_name_integer_string_test() ->
    ?assertEqual('channel_999', build_process_name("channel", 999)),
    ?assertEqual('guild_12345', build_process_name("guild", 12345)),
    ?assertEqual('voice_0', build_process_name("voice", 0)).

build_process_name_binary_atom_test() ->
    ?assertEqual('guild_123456', build_process_name(guild, <<"123456">>)),
    ?assertEqual('voice_789', build_process_name(voice, <<"789">>)),
    ?assertEqual('channel_abc', build_process_name(channel, <<"abc">>)).

build_process_name_binary_string_test() ->
    ?assertEqual('voice_789', build_process_name("voice", <<"789">>)),
    ?assertEqual('guild_test', build_process_name("guild", <<"test">>)),
    ?assertEqual('channel_123', build_process_name("channel", <<"123">>)).

build_process_name_string_atom_test() ->
    ?assertEqual('guild_123456', build_process_name(guild, "123456")),
    ?assertEqual('channel_abc', build_process_name(channel, "abc")),
    ?assertEqual('voice_xyz', build_process_name(voice, "xyz")).

build_process_name_string_string_test() ->
    ?assertEqual('channel_abc', build_process_name("channel", "abc")),
    ?assertEqual('guild_test', build_process_name("guild", "test")),
    ?assertEqual('voice_123', build_process_name("voice", "123")).

build_process_name_special_chars_test() ->
    ?assertEqual('guild_123_456', build_process_name(guild, "123_456")),
    ?assertEqual('channel_test-channel', build_process_name(channel, "test-channel")).

register_and_monitor_success_test() ->
    Name = test_process_reg_success,
    ProcessMap = #{},

    Pid = spawn(fun() -> timer:sleep(100) end),

    Result = register_and_monitor(Name, Pid, ProcessMap),

    ?assertMatch({ok, Pid, _Ref, _NewMap}, Result),
    {ok, ReturnedPid, Ref, NewMap} = Result,

    ?assertEqual(Pid, ReturnedPid),
    ?assertEqual(Pid, whereis(Name)),

    ?assertEqual(1, maps:size(NewMap)),
    ?assertEqual({Pid, Ref}, maps:get(Name, NewMap)),

    ?assert(is_reference(Ref)),

    unregister(Name).

register_and_monitor_existing_map_test() ->
    Name = test_process_reg_existing,
    ExistingPid = list_to_pid("<0.100.0>"),
    ExistingRef = make_ref(),
    ProcessMap = #{other_process => {ExistingPid, ExistingRef}},

    Pid = spawn(fun() -> timer:sleep(100) end),

    {ok, _ReturnedPid, _Ref, NewMap} = register_and_monitor(Name, Pid, ProcessMap),

    ?assertEqual(2, maps:size(NewMap)),
    ?assert(maps:is_key(other_process, NewMap)),
    ?assert(maps:is_key(Name, NewMap)),

    unregister(Name).

register_and_monitor_race_condition_test() ->
    Name = test_process_race,
    ProcessMap = #{},

    WinnerPid = spawn(fun() -> timer:sleep(200) end),
    register(Name, WinnerPid),

    LoserPid = spawn(fun() -> timer:sleep(100) end),

    Result = register_and_monitor(Name, LoserPid, ProcessMap),

    ?assertMatch({ok, WinnerPid, _Ref, _NewMap}, Result),
    {ok, ReturnedPid, Ref, NewMap} = Result,

    ?assertEqual(WinnerPid, ReturnedPid),
    ?assertEqual(WinnerPid, whereis(Name)),

    timer:sleep(50),
    ?assertEqual(false, is_process_alive(LoserPid)),

    ?assertEqual({WinnerPid, Ref}, maps:get(Name, NewMap)),

    unregister(Name).

register_and_monitor_race_dead_test() ->
    Name = test_process_race_dead,
    ProcessMap = #{},

    DeadPid = spawn(fun() -> ok end),
    timer:sleep(10),
    ?assertEqual(false, is_process_alive(DeadPid)),

    NewPid = spawn(fun() -> timer:sleep(100) end),

    Result = register_and_monitor(Name, NewPid, ProcessMap),
    ?assertMatch({ok, NewPid, _Ref, _NewMap}, Result),

    catch unregister(Name).

register_and_monitor_dead_process_test() ->
    Name = test_process_dead,
    ProcessMap = #{},

    DeadPid = spawn(fun() -> exit(normal) end),
    timer:sleep(10),
    ?assertEqual(false, is_process_alive(DeadPid)),

    Result = register_and_monitor(Name, DeadPid, ProcessMap),

    case Result of
        {ok, DeadPid, _Ref, _NewMap} ->
            ?assertEqual(DeadPid, whereis(Name)),
            catch unregister(Name);
        {error, _} ->
            ok
    end.

register_and_monitor_concurrent_test_() ->
    {timeout, 10, fun() ->
        Name = test_process_concurrent,

        Parent = self(),
        Pids = [
            spawn(fun() ->
                Pid = spawn(fun() -> timer:sleep(200) end),
                Result = register_and_monitor(Name, Pid, #{}),
                Parent ! {self(), Result}
            end)
         || _ <- lists:seq(1, 5)
        ],

        Results = [
            receive
                {P, R} -> R
            after 2000 -> timeout
            end
         || P <- Pids
        ],

        ?assertEqual(5, length(Results)),

        SuccessResults = [R || R <- Results, element(1, R) =:= ok],
        RaceErrors = [R || R <- Results, R =:= {error, registration_race_condition}],
        Timeouts = [R || R <- Results, R =:= timeout],

        ?assertEqual(0, length(Timeouts)),

        ?assert(length(SuccessResults) >= 1),

        ?assertEqual(5, length(SuccessResults) + length(RaceErrors)),

        case SuccessResults of
            [] ->
                ?assert(false);
            [{ok, FirstPid, _, _} | RestResults] ->
                AllSamePid = lists:all(
                    fun
                        ({ok, P, _, _}) -> P =:= FirstPid;
                        (_) -> false
                    end,
                    RestResults
                ),
                ?assert(AllSamePid)
        end,

        catch unregister(Name)
    end}.

lookup_or_monitor_success_test() ->
    Name = test_lookup_success,
    Key = test_key,
    ProcessMap = #{},

    Pid = spawn(fun() -> timer:sleep(200) end),
    register(Name, Pid),

    Result = lookup_or_monitor(Name, Key, ProcessMap),

    ?assertMatch({ok, Pid, _Ref, _NewMap}, Result),
    {ok, ReturnedPid, Ref, NewMap} = Result,

    ?assertEqual(Pid, ReturnedPid),
    ?assert(is_reference(Ref)),
    ?assertEqual({Pid, Ref}, maps:get(Key, NewMap)),

    unregister(Name).

lookup_or_monitor_not_found_test() ->
    Name = test_lookup_not_found_99999,
    Key = test_key,
    ProcessMap = #{},

    Result = lookup_or_monitor(Name, Key, ProcessMap),
    ?assertEqual({error, not_found}, Result).

lookup_or_monitor_existing_map_test() ->
    Name = test_lookup_existing,
    Key = new_key,
    ExistingPid = list_to_pid("<0.100.0>"),
    ExistingRef = make_ref(),
    ProcessMap = #{existing_key => {ExistingPid, ExistingRef}},

    Pid = spawn(fun() -> timer:sleep(200) end),
    register(Name, Pid),

    {ok, _ReturnedPid, _Ref, NewMap} = lookup_or_monitor(Name, Key, ProcessMap),

    ?assertEqual(2, maps:size(NewMap)),
    ?assert(maps:is_key(existing_key, NewMap)),
    ?assert(maps:is_key(Key, NewMap)),

    unregister(Name).

lookup_or_monitor_different_key_test() ->
    Name = test_lookup_diff_key,
    Key = different_key_name,
    ProcessMap = #{},

    Pid = spawn(fun() -> timer:sleep(200) end),
    register(Name, Pid),

    {ok, _ReturnedPid, Ref, NewMap} = lookup_or_monitor(Name, Key, ProcessMap),

    ?assertEqual({Pid, Ref}, maps:get(Key, NewMap)),
    ?assertEqual(false, maps:is_key(Name, NewMap)),

    unregister(Name).

lookup_or_monitor_dead_process_test() ->
    Name = test_lookup_dead,
    Key = test_key,
    ProcessMap = #{},

    Pid = spawn(fun() -> ok end),
    register(Name, Pid),
    timer:sleep(10),

    Result = lookup_or_monitor(Name, Key, ProcessMap),
    ?assertEqual({error, not_found}, Result).

safe_unregister_registered_test() ->
    Name = test_safe_unreg_registered,

    Pid = spawn(fun() -> timer:sleep(100) end),
    register(Name, Pid),

    ?assertEqual(Pid, whereis(Name)),
    ?assertEqual(ok, safe_unregister(Name)),
    ?assertEqual(undefined, whereis(Name)).

safe_unregister_unregistered_test() ->
    ?assertEqual(ok, safe_unregister(nonexistent_process_name_12345)).

safe_unregister_multiple_test() ->
    Name = test_safe_unreg_multiple,

    Pid = spawn(fun() -> timer:sleep(100) end),
    register(Name, Pid),

    ?assertEqual(ok, safe_unregister(Name)),
    ?assertEqual(ok, safe_unregister(Name)),
    ?assertEqual(ok, safe_unregister(Name)).

safe_unregister_edge_cases_test() ->
    ?assertEqual(ok, safe_unregister(undefined_name_xyz)),
    ?assertEqual(ok, safe_unregister('some_random_name')),
    ?assertEqual(ok, safe_unregister('')).

cleanup_on_down_preserves_loading_test() ->
    DeadPid = list_to_pid("<0.100.0>"),
    AlivePid = list_to_pid("<0.101.0>"),
    Ref1 = make_ref(),
    Ref2 = make_ref(),

    Map = #{
        guild_1 => {DeadPid, Ref1},
        guild_2 => loading,
        guild_3 => {AlivePid, Ref2}
    },

    Result = cleanup_on_down(DeadPid, Map),

    ?assertEqual(2, maps:size(Result)),
    ?assertEqual(loading, maps:get(guild_2, Result)),
    ?assertEqual({AlivePid, Ref2}, maps:get(guild_3, Result)),
    ?assertEqual(false, maps:is_key(guild_1, Result)).

cleanup_on_down_multiple_loading_test() ->
    DeadPid = list_to_pid("<0.100.0>"),
    AlivePid = list_to_pid("<0.101.0>"),
    Ref1 = make_ref(),
    Ref2 = make_ref(),

    Map = #{
        guild_1 => {DeadPid, Ref1},
        guild_2 => loading,
        guild_3 => {AlivePid, Ref2},
        guild_4 => loading,
        guild_5 => loading
    },

    Result = cleanup_on_down(DeadPid, Map),

    ?assertEqual(4, maps:size(Result)),
    ?assertEqual(loading, maps:get(guild_2, Result)),
    ?assertEqual(loading, maps:get(guild_4, Result)),
    ?assertEqual(loading, maps:get(guild_5, Result)),
    ?assertEqual({AlivePid, Ref2}, maps:get(guild_3, Result)),
    ?assertEqual(false, maps:is_key(guild_1, Result)).

cleanup_on_down_single_removal_test() ->
    DeadPid = list_to_pid("<0.100.0>"),
    AlivePid1 = list_to_pid("<0.101.0>"),
    AlivePid2 = list_to_pid("<0.102.0>"),
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Ref3 = make_ref(),

    Map = #{
        guild_1 => {AlivePid1, Ref1},
        guild_2 => {DeadPid, Ref2},
        guild_3 => {AlivePid2, Ref3}
    },

    Result = cleanup_on_down(DeadPid, Map),

    ?assertEqual(2, maps:size(Result)),
    ?assertEqual({AlivePid1, Ref1}, maps:get(guild_1, Result)),
    ?assertEqual({AlivePid2, Ref3}, maps:get(guild_3, Result)),
    ?assertEqual(false, maps:is_key(guild_2, Result)).

cleanup_on_down_empty_test() ->
    DeadPid = list_to_pid("<0.100.0>"),
    Result = cleanup_on_down(DeadPid, #{}),
    ?assertEqual(#{}, Result).

cleanup_on_down_only_loading_test() ->
    DeadPid = list_to_pid("<0.100.0>"),
    Map = #{
        guild_1 => loading,
        guild_2 => loading
    },
    Result = cleanup_on_down(DeadPid, Map),
    ?assertEqual(Map, Result).

cleanup_on_down_pid_not_found_test() ->
    DeadPid = list_to_pid("<0.100.0>"),
    AlivePid = list_to_pid("<0.101.0>"),
    Ref = make_ref(),

    Map = #{
        guild_1 => {AlivePid, Ref},
        guild_2 => loading
    },

    Result = cleanup_on_down(DeadPid, Map),
    ?assertEqual(Map, Result).

cleanup_on_down_duplicate_pids_test() ->
    DeadPid = list_to_pid("<0.100.0>"),
    Ref1 = make_ref(),
    Ref2 = make_ref(),

    Map = #{
        guild_1 => {DeadPid, Ref1},
        guild_2 => {DeadPid, Ref2}
    },

    Result = cleanup_on_down(DeadPid, Map),
    ?assertEqual(0, maps:size(Result)).

get_count_mixed_test() ->
    Pid1 = list_to_pid("<0.100.0>"),
    Pid2 = list_to_pid("<0.101.0>"),
    Ref1 = make_ref(),
    Ref2 = make_ref(),

    Map = #{
        guild_1 => {Pid1, Ref1},
        guild_2 => loading,
        guild_3 => {Pid2, Ref2},
        guild_4 => loading
    },

    ?assertEqual(2, get_count(Map)).

get_count_empty_test() ->
    ?assertEqual(0, get_count(#{})).

get_count_only_loading_test() ->
    Map = #{
        guild_1 => loading,
        guild_2 => loading
    },
    ?assertEqual(0, get_count(Map)).

get_count_only_processes_test() ->
    Pid1 = list_to_pid("<0.100.0>"),
    Pid2 = list_to_pid("<0.101.0>"),
    Pid3 = list_to_pid("<0.102.0>"),
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Ref3 = make_ref(),

    Map = #{
        guild_1 => {Pid1, Ref1},
        guild_2 => {Pid2, Ref2},
        guild_3 => {Pid3, Ref3}
    },

    ?assertEqual(3, get_count(Map)).

get_count_single_test() ->
    Pid = list_to_pid("<0.100.0>"),
    Ref = make_ref(),
    Map = #{guild_1 => {Pid, Ref}},
    ?assertEqual(1, get_count(Map)).

get_count_single_loading_test() ->
    Map = #{guild_1 => loading},
    ?assertEqual(0, get_count(Map)).

integration_full_lifecycle_test() ->
    Id = 12345,
    Name = build_process_name(guild, Id),
    ?assertEqual('guild_12345', Name),

    Pid = spawn(fun() -> timer:sleep(200) end),
    {ok, Pid, _Ref, Map1} = register_and_monitor(Name, Pid, #{}),
    ?assertEqual(1, get_count(Map1)),

    Map2 = maps:put(guild_67890, loading, Map1),
    ?assertEqual(1, get_count(Map2)),

    OtherName = build_process_name(channel, 67890),
    OtherPid = spawn(fun() -> timer:sleep(200) end),
    register(OtherName, OtherPid),
    {ok, OtherPid, _OtherRef, Map3} = lookup_or_monitor(OtherName, channel_67890, Map2),
    ?assertEqual(2, get_count(Map3)),

    Map4 = cleanup_on_down(Pid, Map3),
    ?assertEqual(1, get_count(Map4)),
    ?assertEqual(loading, maps:get(guild_67890, Map4)),

    safe_unregister(Name),
    safe_unregister(OtherName),

    ?assertEqual(undefined, whereis(Name)),
    ?assertEqual(undefined, whereis(OtherName)).

integration_process_death_test_() ->
    {timeout, 10, fun() ->
        Name = test_integration_death,

        Pid = spawn(fun() ->
            receive
                die -> exit(normal)
            after 100 -> exit(normal)
            end
        end),

        {ok, Pid, Ref, Map} = register_and_monitor(Name, Pid, #{}),
        ?assertEqual(1, get_count(Map)),

        Pid ! die,

        receive
            {'DOWN', Ref, process, Pid, _Reason} ->
                Map2 = cleanup_on_down(Pid, Map),
                ?assertEqual(0, get_count(Map2)),
                safe_unregister(Name)
        after 500 ->
            ?assert(false)
        end
    end}.

integration_race_conditions_test_() ->
    {timeout, 10, fun() ->
        Name = test_integration_race,

        Parent = self(),

        FirstPid = spawn(fun() -> timer:sleep(300) end),
        {ok, FirstPid, _FirstRef, _Map1} = register_and_monitor(Name, FirstPid, #{}),

        Workers = [
            spawn(fun() ->
                NewPid = spawn(fun() -> timer:sleep(100) end),
                Result = register_and_monitor(Name, NewPid, #{}),
                Parent ! {register_result, Result}
            end)
         || _ <- lists:seq(1, 3)
        ],

        Results = [
            receive
                {register_result, R} -> R
            after 1000 -> timeout
            end
         || _ <- Workers
        ],
        AllGotFirstPid = lists:all(
            fun
                ({ok, P, _, _}) -> P =:= FirstPid;
                (_) -> false
            end,
            Results
        ),
        ?assert(AllGotFirstPid),

        safe_unregister(Name)
    end}.

integration_rapid_cycles_test_() ->
    {timeout, 10, fun() ->
        lists:foreach(
            fun(N) ->
                Name = list_to_atom("test_rapid_" ++ integer_to_list(N)),
                Pid = spawn(fun() -> timer:sleep(50) end),

                {ok, Pid, _Ref, Map} = register_and_monitor(Name, Pid, #{}),
                ?assertEqual(1, get_count(Map)),

                safe_unregister(Name),
                ?assertEqual(undefined, whereis(Name))
            end,
            lists:seq(1, 10)
        )
    end}.

force_stop_process_normal_test_() ->
    {timeout, 15, fun() ->
        Pid = spawn(fun() ->
            receive stop -> ok end
        end),
        ?assert(is_process_alive(Pid)),
        force_stop_process(Pid),
        timer:sleep(50),
        ?assertEqual(false, is_process_alive(Pid))
    end}.

force_stop_process_already_dead_test() ->
    Pid = spawn(fun() -> ok end),
    timer:sleep(10),
    ?assertEqual(false, is_process_alive(Pid)),
    force_stop_process(Pid).

force_stop_process_kills_unresponsive_test_() ->
    {timeout, 15, fun() ->
        Pid = spawn(fun() ->
            process_flag(trap_exit, true),
            receive
                never_arrives -> ok
            end
        end),
        ?assert(is_process_alive(Pid)),
        force_stop_process(Pid),
        timer:sleep(100),
        ?assertEqual(false, is_process_alive(Pid))
    end}.

register_and_monitor_duplicate_stops_loser_test_() ->
    {timeout, 15, fun() ->
        Name = test_reg_dup_stops_loser,
        WinnerPid = spawn(fun() -> timer:sleep(5000) end),
        register(Name, WinnerPid),
        LoserPid = spawn(fun() ->
            process_flag(trap_exit, true),
            receive
                never_arrives -> ok
            end
        end),
        ?assert(is_process_alive(LoserPid)),
        Result = register_and_monitor(Name, LoserPid, #{}),
        ?assertMatch({ok, WinnerPid, _, _}, Result),
        timer:sleep(100),
        ?assertEqual(false, is_process_alive(LoserPid)),
        unregister(Name)
    end}.

-endif.
