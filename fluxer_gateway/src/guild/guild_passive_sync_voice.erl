%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_passive_sync_voice).
-typing([eqwalizer]).

-export([build_current_voice_state_map/2, compute_voice_state_updates/3]).

-export_type([voice_state/0]).

-type voice_state() :: map().

-spec build_current_voice_state_map(sets:set(), map()) -> #{binary() => voice_state()}.
build_current_voice_state_map(ViewableChannels, VoiceStates) ->
    maps:fold(
        fun(ConnectionId, VoiceState, Acc) ->
            maybe_include_voice_state(ConnectionId, VoiceState, ViewableChannels, Acc)
        end,
        #{},
        VoiceStates
    ).

-spec maybe_include_voice_state(binary(), voice_state(), sets:set(), map()) -> map().
maybe_include_voice_state(ConnectionId, VoiceState, ViewableChannels, Acc) ->
    case maps:get(<<"channel_id">>, VoiceState, null) of
        null ->
            Acc;
        ChannelIdBin ->
            maybe_include_if_viewable(
                ConnectionId, VoiceState, ChannelIdBin, ViewableChannels, Acc
            )
    end.

-spec maybe_include_if_viewable(binary(), voice_state(), binary(), sets:set(), map()) -> map().
maybe_include_if_viewable(ConnectionId, VoiceState, ChannelIdBin, ViewableChannels, Acc) ->
    case snowflake_id:parse_maybe(ChannelIdBin) of
        undefined ->
            Acc;
        ChannelId ->
            include_viewable_voice_state(
                ConnectionId, VoiceState, ChannelId, ViewableChannels, Acc
            )
    end.

-spec include_viewable_voice_state(binary(), voice_state(), integer(), sets:set(), map()) ->
    map().
include_viewable_voice_state(ConnectionId, VoiceState, ChannelId, ViewableChannels, Acc) ->
    case sets:is_element(ChannelId, ViewableChannels) of
        true -> Acc#{ConnectionId => VoiceState};
        false -> Acc
    end.

-spec compute_voice_state_updates(
    #{binary() => voice_state()}, #{binary() => voice_state()}, integer()
) -> [voice_state()].
compute_voice_state_updates(Current, Previous, GuildId) ->
    GuildIdBin = integer_to_binary(GuildId),
    Updated = collect_updated(Current, Previous, GuildIdBin),
    Removed = collect_removed(Current, Previous, GuildIdBin),
    lists:reverse(Updated) ++ lists:reverse(Removed).

-spec collect_updated(map(), map(), binary()) -> [voice_state()].
collect_updated(Current, Previous, GuildIdBin) ->
    maps:fold(
        fun(ConnId, VoiceState, Acc) ->
            collect_updated_state(ConnId, VoiceState, Previous, GuildIdBin, Acc)
        end,
        [],
        Current
    ).

-spec collect_updated_state(binary(), voice_state(), map(), binary(), [voice_state()]) ->
    [voice_state()].
collect_updated_state(ConnId, VoiceState, Previous, GuildIdBin, Acc) ->
    PrevState = maps:get(ConnId, Previous, undefined),
    case is_changed(VoiceState, PrevState) of
        true ->
            [
                voice_state_utils:sanitize_voice_state_for_broadcast(
                    ensure_guild(VoiceState, GuildIdBin)
                )
                | Acc
            ];
        false ->
            Acc
    end.

-spec collect_removed(map(), map(), binary()) -> [voice_state()].
collect_removed(Current, Previous, GuildIdBin) ->
    maps:fold(
        fun(ConnId, PrevState, Acc) ->
            collect_removed_state(ConnId, PrevState, Current, GuildIdBin, Acc)
        end,
        [],
        Previous
    ).

-spec collect_removed_state(binary(), voice_state(), map(), binary(), [voice_state()]) ->
    [voice_state()].
collect_removed_state(ConnId, PrevState, Current, GuildIdBin, Acc) ->
    case maps:is_key(ConnId, Current) of
        true -> Acc;
        false -> [build_removed(PrevState, GuildIdBin) | Acc]
    end.

-spec is_changed(voice_state(), voice_state() | undefined) -> boolean().
is_changed(_Current, undefined) -> true;
is_changed(Current, Previous) -> version(Current) =/= version(Previous).

-spec version(voice_state()) -> integer().
version(VoiceState) ->
    case map_utils:get_integer(VoiceState, <<"version">>, 0) of
        V when is_integer(V) -> V;
        _ -> 0
    end.

-spec ensure_guild(voice_state(), binary()) -> voice_state().
ensure_guild(VoiceState, GuildIdBin) ->
    case maps:get(<<"guild_id">>, VoiceState, undefined) of
        undefined -> VoiceState#{<<"guild_id">> => GuildIdBin};
        _ -> VoiceState
    end.

-spec build_removed(voice_state(), binary()) -> voice_state().
build_removed(PrevState, GuildIdBin) ->
    voice_state_utils:sanitize_voice_state_for_broadcast(
        (ensure_guild(PrevState, GuildIdBin))#{<<"channel_id">> => null}
    ).
