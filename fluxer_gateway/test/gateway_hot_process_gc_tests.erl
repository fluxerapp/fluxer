%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_hot_process_gc_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

-define(GENERATIONAL_FULLSWEEP_AFTER, 10).

session_init_keeps_generational_gc_test() ->
    ?assertEqual(
        ?GENERATIONAL_FULLSWEEP_AFTER,
        fullsweep_after_in(fun() -> session:init(session_data()) end)
    ).

session_code_change_keeps_generational_gc_test() ->
    ?assertEqual(
        ?GENERATIONAL_FULLSWEEP_AFTER,
        fullsweep_after_in(fun() -> session:code_change(0, #{}, []) end)
    ).

guild_init_keeps_generational_gc_test() ->
    ?assertEqual(
        ?GENERATIONAL_FULLSWEEP_AFTER,
        fullsweep_after_in(fun() -> guild:init(guild_data()) end)
    ).

guild_code_change_keeps_generational_gc_test() ->
    ?assertEqual(
        ?GENERATIONAL_FULLSWEEP_AFTER,
        fullsweep_after_in(fun() -> guild:code_change(0, #{}, []) end)
    ).

guild_broadcaster_init_keeps_generational_gc_test() ->
    Parent = self(),
    ?assertEqual(
        ?GENERATIONAL_FULLSWEEP_AFTER,
        fullsweep_after_in(fun() -> guild_broadcaster:init([900301, Parent]) end)
    ).

session_data() ->
    #{
        id => <<"gc-session">>,
        user_id => 900401,
        user_data => #{<<"username">> => <<"gc">>},
        version => 9,
        token_hash => <<"token_hash">>,
        auth_session_id_hash => <<"auth_hash">>,
        properties => #{},
        status => online,
        ready => undefined,
        socket_pid => self(),
        guilds => []
    }.

guild_data() ->
    #{
        id => 900300,
        member_count => 0,
        sessions => #{},
        data => #{
            <<"guild">> => #{
                <<"id">> => 900300,
                <<"owner_id">> => 900401,
                <<"features">> => [],
                <<"member_count">> => 0
            },
            <<"roles">> => [],
            <<"channels">> => [],
            <<"members">> => []
        }
    }.

fullsweep_after_in(Fun) ->
    Ref = make_ref(),
    Parent = self(),
    {Pid, MonRef} = spawn_monitor(fun() ->
        erlang:process_flag(fullsweep_after, 0),
        _ = Fun(),
        Parent ! {Ref, fullsweep_after(self())}
    end),
    Result =
        receive
            {Ref, Value} -> Value;
            {'DOWN', MonRef, process, Pid, Reason} -> {crashed, Reason}
        after 10000 -> timeout
        end,
    erlang:demonitor(MonRef, [flush]),
    exit(Pid, kill),
    Result.

fullsweep_after(Pid) ->
    case erlang:process_info(Pid, garbage_collection) of
        {garbage_collection, Info} -> proplists:get_value(fullsweep_after, Info);
        _ -> undefined
    end.
