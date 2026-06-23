%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_common).
-typing([eqwalizer]).

-export([
    safe_call/3,
    parse_event_data/1,
    relay_upsert_voice_state/2,
    strip_members/1
]).

-spec safe_call(pid(), term(), timeout()) -> term().
safe_call(Pid, Msg, Timeout) when is_pid(Pid) ->
    try gen_server:call(Pid, Msg, Timeout) of
        Reply -> Reply
    catch
        exit:{timeout, _} -> {error, timeout};
        exit:{noproc, _} -> {error, noproc};
        exit:{normal, _} -> {error, noproc};
        _:Reason -> {error, Reason}
    end.

-spec parse_event_data
    (binary()) -> term();
    (map()) -> map().
parse_event_data(EventData) when is_binary(EventData) ->
    json:decode(EventData);
parse_event_data(EventData) when is_map(EventData) ->
    EventData.

-spec relay_upsert_voice_state(term(), map()) -> map().
relay_upsert_voice_state(VoiceState, State) when is_map(VoiceState) ->
    ConnectionId = maps:get(<<"connection_id">>, VoiceState, undefined),
    case ConnectionId of
        undefined ->
            State;
        _ ->
            upsert_voice_state(ConnectionId, VoiceState, State)
    end;
relay_upsert_voice_state(_, State) ->
    State.

-spec upsert_voice_state(term(), map(), map()) -> map().
upsert_voice_state(ConnectionId, VoiceState, State) ->
    VoiceStates0 = maps:get(voice_states, State, #{}),
    ChannelId = maps:get(<<"channel_id">>, VoiceState, null),
    VoiceStates =
        case ChannelId of
            null -> maps:remove(ConnectionId, VoiceStates0);
            _ -> VoiceStates0#{ConnectionId => VoiceState}
        end,
    State#{voice_states => VoiceStates}.

-spec strip_members(term()) -> term().
strip_members(Data) when is_map(Data) ->
    Data1 = maps:remove(<<"members">>, Data),
    maps:remove(<<"member_role_index">>, Data1);
strip_members(Data) ->
    Data.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

slow_gen_call_handler() ->
    receive
        {'$gen_call', _From, _Msg} ->
            ok = gateway_retry_timer:wait(5000)
    after 30000 ->
        ok
    end.

safe_call_timeout_test() ->
    Pid = spawn(fun slow_gen_call_handler/0),
    Result = safe_call(Pid, ping, 50),
    ?assertEqual({error, timeout}, Result),
    exit(Pid, kill),
    ok.

safe_call_noproc_test() ->
    Pid = spawn(fun() -> ok end),
    ok = gateway_retry_timer:wait(50),
    Result = safe_call(Pid, ping, 100),
    ?assertMatch({error, _}, Result),
    ok.

parse_event_data_binary_test() ->
    Binary = <<"{\"key\":\"value\"}">>,
    Result = parse_event_data(Binary),
    ?assertEqual(#{<<"key">> => <<"value">>}, Result).

parse_event_data_map_test() ->
    Map = #{<<"key">> => <<"value">>},
    Result = parse_event_data(Map),
    ?assertEqual(Map, Result).

relay_upsert_voice_state_adds_state_test() ->
    VoiceState = #{
        <<"connection_id">> => <<"conn-1">>,
        <<"channel_id">> => <<"100">>,
        <<"user_id">> => <<"42">>
    },
    State0 = #{voice_states => #{}},
    State1 = relay_upsert_voice_state(VoiceState, State0),
    VoiceStates = maps:get(voice_states, State1),
    ?assertEqual(VoiceState, maps:get(<<"conn-1">>, VoiceStates)).

relay_upsert_voice_state_removes_on_null_channel_test() ->
    Existing = #{<<"connection_id">> => <<"conn-1">>, <<"channel_id">> => <<"100">>},
    State0 = #{voice_states => #{<<"conn-1">> => Existing}},
    RemoveState = #{<<"connection_id">> => <<"conn-1">>, <<"channel_id">> => null},
    State1 = relay_upsert_voice_state(RemoveState, State0),
    VoiceStates = maps:get(voice_states, State1),
    ?assertEqual(false, maps:is_key(<<"conn-1">>, VoiceStates)).

relay_upsert_voice_state_no_connection_id_test() ->
    State0 = #{voice_states => #{}},
    State1 = relay_upsert_voice_state(#{<<"channel_id">> => <<"100">>}, State0),
    ?assertEqual(State0, State1).

relay_upsert_voice_state_non_map_test() ->
    State0 = #{voice_states => #{}},
    State1 = relay_upsert_voice_state(not_a_map, State0),
    ?assertEqual(State0, State1).

strip_members_test() ->
    Data = #{
        <<"members">> => [#{<<"user">> => #{<<"id">> => <<"1">>}}],
        <<"member_role_index">> => #{1 => [<<"role1">>]},
        <<"channels">> => [#{<<"id">> => <<"10">>}],
        <<"roles">> => [#{<<"id">> => <<"role1">>}]
    },
    Stripped = strip_members(Data),
    ?assertEqual(false, maps:is_key(<<"members">>, Stripped)),
    ?assertEqual(false, maps:is_key(<<"member_role_index">>, Stripped)),
    ?assertEqual([#{<<"id">> => <<"10">>}], maps:get(<<"channels">>, Stripped)),
    ?assertEqual([#{<<"id">> => <<"role1">>}], maps:get(<<"roles">>, Stripped)).

strip_members_empty_test() ->
    ?assertEqual(#{}, strip_members(#{})).

strip_members_non_map_test() ->
    ?assertEqual(not_a_map, strip_members(not_a_map)).

-endif.
