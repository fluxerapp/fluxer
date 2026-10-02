%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_engine_inputs).
-typing([eqwalizer]).

-export([
    is_stale/2,
    current_twin/3,
    record/4,
    forget/2,
    forget_all/1,
    mark_stale/2,
    latch_stale/1
]).

-type guild_state() :: map().
-type list_id() :: binary().
-type channel_id() :: pos_integer().
-type inputs() :: {term(), [term()]}.

-export_type([guild_state/0, list_id/0]).

-define(INPUTS_KEY, channel_member_list_engine_inputs).

-spec is_stale(list_id(), guild_state()) -> boolean().
is_stale(ListId, State) ->
    case {channel_id(ListId), maps:find(ListId, recorded(State))} of
        {ChannelId, {ok, Recorded}} when is_integer(ChannelId) ->
            Recorded =/= inputs(ChannelId, State);
        _ ->
            true
    end.

-spec current_twin(channel_id(), [list_id()], guild_state()) -> {ok, list_id()} | none.
current_twin(ChannelId, ListIds, State) ->
    AccessByChannel = virtual_access_by_channel(State),
    find_current_twin(
        inputs(ChannelId, State, AccessByChannel),
        ListIds,
        recorded(State),
        AccessByChannel,
        State
    ).

-spec find_current_twin(inputs(), [list_id()], map(), #{term() => [term()]}, guild_state()) ->
    {ok, list_id()} | none.
find_current_twin(_Inputs, [], _Recorded, _AccessByChannel, _State) ->
    none;
find_current_twin(Inputs, [ListId | Rest], Recorded, AccessByChannel, State) ->
    case {maps:find(ListId, Recorded), channel_id(ListId)} of
        {{ok, Inputs}, TwinChannelId} when is_integer(TwinChannelId) ->
            case inputs(TwinChannelId, State, AccessByChannel) of
                Inputs -> {ok, ListId};
                _ -> find_current_twin(Inputs, Rest, Recorded, AccessByChannel, State)
            end;
        _ ->
            find_current_twin(Inputs, Rest, Recorded, AccessByChannel, State)
    end.

-spec record(list_id(), channel_id(), guild_state(), guild_state()) -> guild_state().
record(ListId, ChannelId, BuiltFrom, State) ->
    put_recorded(maps:put(ListId, inputs(ChannelId, BuiltFrom), recorded(State)), State).

-spec forget(list_id(), guild_state()) -> guild_state().
forget(ListId, State) ->
    put_recorded(maps:remove(ListId, recorded(State)), State).

-spec forget_all(guild_state()) -> guild_state().
forget_all(State) ->
    put_recorded(#{}, State).

-spec mark_stale(channel_id(), guild_state()) -> guild_state().
mark_stale(ChannelId, State) ->
    forget(integer_to_binary(ChannelId), State).

-spec latch_stale(guild_state()) -> guild_state().
latch_stale(State) ->
    AccessByChannel = virtual_access_by_channel(State),
    put_recorded(
        maps:filter(
            fun(ListId, Recorded) ->
                case channel_id(ListId) of
                    undefined -> false;
                    ChannelId -> Recorded =:= inputs(ChannelId, State, AccessByChannel)
                end
            end,
            recorded(State)
        ),
        State
    ).

-spec inputs(channel_id(), guild_state()) -> inputs().
inputs(ChannelId, State) ->
    inputs(ChannelId, State, virtual_access_by_channel(State)).

-spec inputs(channel_id(), guild_state(), #{term() => [term()]}) -> inputs().
inputs(ChannelId, State, AccessByChannel) ->
    {guild_permissions:view_inputs(ChannelId, State), maps:get(ChannelId, AccessByChannel, [])}.

-spec virtual_access_by_channel(guild_state()) -> #{term() => [term()]}.
virtual_access_by_channel(State) ->
    ByChannel = maps:fold(
        fun(UserId, Channels, Acc) ->
            sets:fold(
                fun(ChannelId, Inner) ->
                    maps:update_with(
                        ChannelId, fun(Users) -> [UserId | Users] end, [UserId], Inner
                    )
                end,
                Acc,
                Channels
            )
        end,
        #{},
        map_utils:ensure_map(maps:get(virtual_channel_access, State, #{}))
    ),
    maps:map(fun(_ChannelId, Users) -> lists:sort(Users) end, ByChannel).

-spec recorded(guild_state()) -> #{list_id() => inputs()}.
recorded(State) ->
    case maps:get(?INPUTS_KEY, State, #{}) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    end.

-spec put_recorded(#{list_id() => inputs()}, guild_state()) -> guild_state().
put_recorded(Map, State) when map_size(Map) =:= 0 ->
    maps:remove(?INPUTS_KEY, State);
put_recorded(Map, State) ->
    State#{?INPUTS_KEY => Map}.

-spec channel_id(list_id()) -> channel_id() | undefined.
channel_id(ListId) ->
    case snowflake_id:parse_maybe(ListId) of
        Id when is_integer(Id), Id > 0 -> Id;
        _ -> undefined
    end.
