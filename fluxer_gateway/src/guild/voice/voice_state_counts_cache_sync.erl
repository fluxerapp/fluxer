%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(voice_state_counts_cache_sync).
-typing([eqwalizer]).

-export([
    collect_live_voice_states/1,
    merge_live_voice_state_entries/3
]).

-export_type([
    voice_state/0
]).

-type voice_state() :: map().

-spec collect_live_voice_states(timeout()) -> #{binary() => voice_state()}.
collect_live_voice_states(CallTimeout) ->
    GuildStates = collect_live_guild_voice_states(CallTimeout),
    DmStates = collect_live_dm_voice_states(CallTimeout),
    maps:merge(GuildStates, DmStates).

-spec collect_live_guild_voice_states(timeout()) -> #{binary() => voice_state()}.
collect_live_guild_voice_states(CallTimeout) ->
    try
        ets:foldl(
            fun
                ({_GuildId, Pid}, Acc) ->
                    collect_guild_pid_voice_states(Pid, CallTimeout, Acc);
                (_Row, Acc) ->
                    Acc
            end,
            #{},
            guild_voice_registry
        )
    catch
        error:badarg ->
            #{}
    end.

-spec collect_guild_pid_voice_states(pid(), timeout(), #{binary() => voice_state()}) ->
    #{binary() => voice_state()}.
collect_guild_pid_voice_states(Pid, CallTimeout, Acc) ->
    case is_pid(Pid) andalso process_liveness:is_alive(Pid) of
        true -> fetch_guild_pid_voice_states(Pid, CallTimeout, Acc);
        false -> Acc
    end.

-spec fetch_guild_pid_voice_states(pid(), timeout(), #{binary() => voice_state()}) ->
    #{binary() => voice_state()}.
fetch_guild_pid_voice_states(Pid, CallTimeout, Acc) ->
    try gen_server:call(Pid, {get_voice_states_map}, CallTimeout) of
        VS when is_map(VS) -> maps:merge(Acc, voice_state_utils:ensure_voice_states(VS));
        _ -> Acc
    catch
        exit:_Reason -> Acc;
        error:_Reason -> Acc
    end.

-spec collect_live_dm_voice_states(timeout()) -> #{binary() => voice_state()}.
collect_live_dm_voice_states(CallTimeout) ->
    CallIds =
        try
            call_manager:local_call_ids()
        catch
            exit:_Reason -> [];
            error:_Reason -> []
        end,
    lists:foldl(
        fun(ChannelId, Acc) ->
            collect_live_dm_call_voice_states(ChannelId, CallTimeout, Acc)
        end,
        #{},
        CallIds
    ).

-spec collect_live_dm_call_voice_states(integer(), timeout(), #{binary() => voice_state()}) ->
    #{binary() => voice_state()}.
collect_live_dm_call_voice_states(ChannelId, CallTimeout, Acc) ->
    case lookup_call_pid(ChannelId) of
        {ok, Pid} ->
            fetch_call_voice_states(Pid, ChannelId, CallTimeout, Acc);
        error ->
            Acc
    end.

-spec lookup_call_pid(integer()) -> {ok, pid()} | error.
lookup_call_pid(ChannelId) ->
    try call_manager:lookup(ChannelId) of
        {ok, Pid} when is_pid(Pid) -> {ok, Pid};
        _ -> error
    catch
        exit:_Reason -> error;
        error:_Reason -> error
    end.

-spec fetch_call_voice_states(pid(), integer(), timeout(), #{binary() => voice_state()}) ->
    #{binary() => voice_state()}.
fetch_call_voice_states(Pid, ChannelId, CallTimeout, Acc) ->
    case process_liveness:is_alive(Pid) of
        false ->
            Acc;
        true ->
            fetch_live_call_voice_states(Pid, ChannelId, CallTimeout, Acc)
    end.

-spec fetch_live_call_voice_states(pid(), integer(), timeout(), #{binary() => voice_state()}) ->
    #{binary() => voice_state()}.
fetch_live_call_voice_states(Pid, ChannelId, CallTimeout, Acc) ->
    try gen_server:call(Pid, {get_state}, CallTimeout) of
        {ok, CallData} when is_map(CallData) ->
            VoiceStates = extract_voice_states(CallData),
            merge_live_voice_state_entries(VoiceStates, ChannelId, Acc);
        _ ->
            Acc
    catch
        exit:_Reason -> Acc;
        error:_Reason -> Acc
    end.

-spec extract_voice_states(map()) -> term().
extract_voice_states(CallData) ->
    maps:get(
        voice_states,
        CallData,
        maps:get(<<"voice_states">>, CallData, [])
    ).

-spec merge_live_voice_state_entries(term(), term(), #{binary() => voice_state()}) ->
    #{binary() => voice_state()}.
merge_live_voice_state_entries(VoiceStates, DefaultChannelId, Acc) when is_list(VoiceStates) ->
    lists:foldl(
        fun(VoiceState, InnerAcc) ->
            maybe_put_live_voice_state(undefined, DefaultChannelId, VoiceState, InnerAcc)
        end,
        Acc,
        VoiceStates
    );
merge_live_voice_state_entries(VoiceStates, DefaultChannelId, Acc) when is_map(VoiceStates) ->
    maps:fold(
        fun(ConnectionId, VoiceState, InnerAcc) ->
            maybe_put_live_voice_state(ConnectionId, DefaultChannelId, VoiceState, InnerAcc)
        end,
        Acc,
        VoiceStates
    );
merge_live_voice_state_entries(_VoiceStates, _DefaultChannelId, Acc) ->
    Acc.

-spec maybe_put_live_voice_state(term(), term(), term(), #{binary() => voice_state()}) ->
    #{binary() => voice_state()}.
maybe_put_live_voice_state(_FallbackConnId, _DefaultChanId, VoiceState, Acc) when
    not is_map(VoiceState)
->
    Acc;
maybe_put_live_voice_state(FallbackConnId, DefaultChanId, VoiceState, Acc) ->
    RawConnId = maps:get(
        <<"connection_id">>,
        VoiceState,
        maps:get(connection_id, VoiceState, FallbackConnId)
    ),
    ConnectionId = guild_voice_connection_normalize:normalize_optional_binary(RawConnId),
    case ConnectionId of
        undefined ->
            Acc;
        _ ->
            VsWithConn = VoiceState#{<<"connection_id">> => ConnectionId},
            Acc#{ConnectionId => maybe_put_default_channel_id(VsWithConn, DefaultChanId)}
    end.

-spec maybe_put_default_channel_id(voice_state(), term()) -> voice_state().
maybe_put_default_channel_id(VoiceState, DefaultChannelId) ->
    ExistingId = maps:get(
        <<"channel_id">>,
        VoiceState,
        maps:get(channel_id, VoiceState, undefined)
    ),
    case guild_voice_connection_normalize:normalize_optional_binary(ExistingId) of
        undefined ->
            apply_default_channel_id(VoiceState, DefaultChannelId);
        ChannelId ->
            VoiceState#{<<"channel_id">> => ChannelId}
    end.

-spec apply_default_channel_id(voice_state(), term()) -> voice_state().
apply_default_channel_id(VoiceState, DefaultChannelId) ->
    case guild_voice_connection_normalize:normalize_optional_binary(DefaultChannelId) of
        undefined -> VoiceState;
        ChannelId -> VoiceState#{<<"channel_id">> => ChannelId}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

reset_count_tables() ->
    try
        ets:delete(voice_state_count_connections)
    catch
        error:badarg -> ok
    end,
    try
        ets:delete(voice_state_count_regions)
    catch
        error:badarg -> ok
    end,
    try
        ets:delete(voice_state_count_servers)
    catch
        error:badarg -> ok
    end,
    ok.

merge_live_voice_state_entries_counts_dm_call_voice_states_test() ->
    ok = reset_count_tables(),
    LiveStates = merge_live_voice_state_entries(
        [
            #{
                <<"connection_id">> => <<"dm-conn-1">>,
                <<"user_id">> => <<"100">>,
                <<"region_id">> => <<"us-east">>,
                <<"server_id">> => <<"6f47e7f-ewr">>
            }
        ],
        123,
        #{}
    ),
    VoiceState = maps:get(<<"dm-conn-1">>, LiveStates),
    ?assertEqual(<<"123">>, maps:get(<<"channel_id">>, VoiceState)),
    ok = voice_state_counts_cache:upsert_voice_state(VoiceState),
    Counts = voice_state_counts_cache:get_local_counts(),
    ?assertEqual(1, maps:get(<<"total_voice_states">>, Counts)),
    ?assertEqual(
        [#{<<"region_id">> => <<"us-east">>, <<"voice_state_count">> => 1}],
        maps:get(<<"regions">>, Counts)
    ),
    ?assertEqual(
        [#{<<"server_id">> => <<"6f47e7f-ewr">>, <<"voice_state_count">> => 1}],
        maps:get(<<"servers">>, Counts)
    ).

merge_live_voice_state_entries_uses_map_key_connection_id_test() ->
    LiveStates = merge_live_voice_state_entries(
        #{
            <<"dm-conn-2">> => #{
                <<"user_id">> => <<"200">>,
                <<"channel_id">> => 456
            }
        },
        undefined,
        #{}
    ),
    VoiceState = maps:get(<<"dm-conn-2">>, LiveStates),
    ?assertEqual(<<"dm-conn-2">>, maps:get(<<"connection_id">>, VoiceState)),
    ?assertEqual(<<"456">>, maps:get(<<"channel_id">>, VoiceState)).

-endif.
