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

-module(guild_common).

-export([
    safe_call/3,
    parse_event_data/1,
    relay_upsert_voice_state/2,
    strip_members/1,
    build_shard_state/4,
    merge_cluster_state/2,
    merge_user_set_maps/2
]).

-type guild_id() :: integer().
-type shard_index() :: non_neg_integer().

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

-spec parse_event_data(binary() | map()) -> map().
parse_event_data(EventData) when is_binary(EventData) ->
    json:decode(EventData);
parse_event_data(EventData) when is_map(EventData) ->
    EventData.

-spec relay_upsert_voice_state(map(), map()) -> map().
relay_upsert_voice_state(VoiceState, State) when is_map(VoiceState) ->
    ConnectionId = maps:get(<<"connection_id">>, VoiceState, undefined),
    case ConnectionId of
        undefined ->
            State;
        _ ->
            VoiceStates0 = maps:get(voice_states, State, #{}),
            ChannelId = maps:get(<<"channel_id">>, VoiceState, null),
            VoiceStates =
                case ChannelId of
                    null -> maps:remove(ConnectionId, VoiceStates0);
                    _ -> maps:put(ConnectionId, VoiceState, VoiceStates0)
                end,
            maps:put(voice_states, VoiceStates, State)
    end;
relay_upsert_voice_state(_, State) ->
    State.

-spec strip_members(map()) -> map().
strip_members(Data) when is_map(Data) ->
    Data1 = maps:remove(<<"members">>, Data),
    maps:remove(<<"member_role_index">>, Data1);
strip_members(Data) ->
    Data.

-spec build_shard_state(guild_id(), map(), pos_integer(), shard_index()) -> map().
build_shard_state(GuildId, Data, ShardCount, ShardIndex) ->
    DisableCache = ShardIndex =/= 0,
    MemberCount = guild_data_index:member_count(Data),
    ShardData =
        case DisableCache of
            true -> strip_members(Data);
            false -> Data
        end,
    ShardState0 = #{
        id => GuildId,
        data => ShardData,
        sessions => #{},
        member_count => MemberCount,
        disable_push_notifications => true,
        disable_member_list_updates => DisableCache,
        disable_auto_stop_on_empty => true,
        very_large_guild_coordinator_pid => self(),
        very_large_guild_shard_count => ShardCount,
        very_large_guild_shard_index => ShardIndex
    },
    case DisableCache of
        true -> maps:put(disable_permission_cache_updates, true, ShardState0);
        false -> ShardState0
    end.

-spec merge_cluster_state(map(), map()) -> map().
merge_cluster_state(Acc, Frag) ->
    SessionsAcc = maps:get(sessions, Acc, #{}),
    SessionsFrag = maps:get(sessions, Frag, #{}),
    VoiceAcc = maps:get(voice_states, Acc, #{}),
    VoiceFrag = maps:get(voice_states, Frag, #{}),
    VAAcc = maps:get(virtual_channel_access, Acc, #{}),
    VAFrag = maps:get(virtual_channel_access, Frag, #{}),
    PendingAcc = maps:get(virtual_channel_access_pending, Acc, #{}),
    PendingFrag = maps:get(virtual_channel_access_pending, Frag, #{}),
    PreserveAcc = maps:get(virtual_channel_access_preserve, Acc, #{}),
    PreserveFrag = maps:get(virtual_channel_access_preserve, Frag, #{}),
    MoveAcc = maps:get(virtual_channel_access_move_pending, Acc, #{}),
    MoveFrag = maps:get(virtual_channel_access_move_pending, Frag, #{}),
    Acc#{
        sessions => maps:merge(SessionsAcc, SessionsFrag),
        voice_states => maps:merge(VoiceAcc, VoiceFrag),
        virtual_channel_access => merge_user_set_maps(VAAcc, VAFrag),
        virtual_channel_access_pending => merge_user_set_maps(PendingAcc, PendingFrag),
        virtual_channel_access_preserve => merge_user_set_maps(PreserveAcc, PreserveFrag),
        virtual_channel_access_move_pending => merge_user_set_maps(MoveAcc, MoveFrag)
    }.

-spec merge_user_set_maps(map(), map()) -> map().
merge_user_set_maps(A, B) ->
    maps:fold(
        fun(UserId, SetB, Acc) ->
            case maps:get(UserId, Acc, undefined) of
                undefined ->
                    maps:put(UserId, SetB, Acc);
                SetA ->
                    maps:put(UserId, sets:union(SetA, SetB), Acc)
            end
        end,
        A,
        B
    ).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

safe_call_timeout_test() ->
    Pid = spawn(fun() ->
        receive
            {'$gen_call', _From, _Msg} ->
                timer:sleep(5000)
        end
    end),
    Result = safe_call(Pid, ping, 50),
    ?assertEqual({error, timeout}, Result),
    exit(Pid, kill),
    ok.

safe_call_noproc_test() ->
    Pid = spawn(fun() -> ok end),
    timer:sleep(50),
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

merge_user_set_maps_test() ->
    SetA = sets:from_list([1, 2]),
    SetB = sets:from_list([2, 3]),
    MapA = #{10 => SetA},
    MapB = #{10 => SetB, 20 => SetB},
    Merged = merge_user_set_maps(MapA, MapB),
    ?assert(maps:is_key(10, Merged)),
    ?assert(maps:is_key(20, Merged)),
    MergedSet10 = maps:get(10, Merged),
    ?assert(sets:is_element(1, MergedSet10)),
    ?assert(sets:is_element(2, MergedSet10)),
    ?assert(sets:is_element(3, MergedSet10)),
    ?assertEqual(3, sets:size(MergedSet10)),
    ?assertEqual(SetB, maps:get(20, Merged)).

merge_user_set_maps_empty_test() ->
    ?assertEqual(#{}, merge_user_set_maps(#{}, #{})).

merge_cluster_state_test() ->
    Acc = #{
        sessions => #{<<"s1">> => #{user_id => 1}},
        voice_states => #{<<"c1">> => #{channel_id => 10}},
        virtual_channel_access => #{},
        virtual_channel_access_pending => #{},
        virtual_channel_access_preserve => #{},
        virtual_channel_access_move_pending => #{}
    },
    Frag = #{
        sessions => #{<<"s2">> => #{user_id => 2}},
        voice_states => #{<<"c2">> => #{channel_id => 20}},
        virtual_channel_access => #{},
        virtual_channel_access_pending => #{},
        virtual_channel_access_preserve => #{},
        virtual_channel_access_move_pending => #{}
    },
    Merged = merge_cluster_state(Acc, Frag),
    ?assert(maps:is_key(<<"s1">>, maps:get(sessions, Merged))),
    ?assert(maps:is_key(<<"s2">>, maps:get(sessions, Merged))),
    ?assert(maps:is_key(<<"c1">>, maps:get(voice_states, Merged))),
    ?assert(maps:is_key(<<"c2">>, maps:get(voice_states, Merged))).

build_shard_state_primary_test() ->
    Data = #{<<"members">> => [#{<<"user">> => #{<<"id">> => <<"1">>}}]},
    ShardState = build_shard_state(100, Data, 4, 0),
    ?assertEqual(100, maps:get(id, ShardState)),
    ?assertEqual(Data, maps:get(data, ShardState)),
    ?assertEqual(false, maps:get(disable_member_list_updates, ShardState)),
    ?assertEqual(false, maps:is_key(disable_permission_cache_updates, ShardState)).

build_shard_state_secondary_test() ->
    Data = #{<<"members">> => [#{<<"user">> => #{<<"id">> => <<"1">>}}]},
    ShardState = build_shard_state(100, Data, 4, 2),
    ?assertEqual(100, maps:get(id, ShardState)),
    ShardData = maps:get(data, ShardState),
    ?assertEqual(false, maps:is_key(<<"members">>, ShardData)),
    ?assertEqual(true, maps:get(disable_member_list_updates, ShardState)),
    ?assertEqual(true, maps:get(disable_permission_cache_updates, ShardState)).

-endif.
