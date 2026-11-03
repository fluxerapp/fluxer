%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_guilds).
-typing([eqwalizer]).

-export([
    handle_guild_leave/2,
    handle_forced_unavailable_guild_leave/3,
    store_guild_subscriptions/2,
    remove_guild_subscription_state/2
]).

-export_type([session_state/0, guild_id/0]).

-type session_state() :: session:session_state().
-type guild_id() :: session:guild_id().

-spec handle_guild_leave(guild_id(), session_state()) ->
    {noreply, session_state()}.
handle_guild_leave(GuildId, #{guilds := Guilds} = State) ->
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, Ref} when is_pid(Pid) ->
            demonitor(Ref, [flush]),
            DeleteData = #{<<"id">> => integer_to_binary(GuildId)},
            {noreply, DispatchedState} =
                session_dispatch:handle_dispatch(guild_delete, DeleteData, State),
            State1 = session_guild_health:forget(GuildId, DispatchedState#{
                guilds => maps:remove(GuildId, Guilds)
            }),
            {noreply, remove_guild_subscription_state(GuildId, State1)};
        _ ->
            {noreply, State}
    end.

-spec handle_forced_unavailable_guild_leave(
    guild_id(), boolean(), session_state()
) -> {noreply, session_state()}.
handle_forced_unavailable_guild_leave(GuildId, UnavailableHidden, #{guilds := Guilds} = State) ->
    demonitor_guild_if_connected(maps:get(GuildId, Guilds, undefined)),
    GuildDeleteData = build_unavailable_guild_data(GuildId, UnavailableHidden),
    {noreply, State1} =
        session_dispatch:handle_dispatch(guild_delete, GuildDeleteData, State),
    self() ! {guild_connect, GuildId, 0},
    {noreply,
        session_guild_health:forget(GuildId, State1#{
            guilds => Guilds#{GuildId => cached_unavailable}
        })}.

-spec demonitor_guild_if_connected(term()) -> ok.
demonitor_guild_if_connected({Pid, Ref}) when is_pid(Pid), is_reference(Ref) ->
    demonitor(Ref, [flush]),
    ok;
demonitor_guild_if_connected(_) ->
    ok.

-spec build_unavailable_guild_data(guild_id(), boolean()) -> map().
build_unavailable_guild_data(GuildId, true) ->
    #{
        <<"id">> => integer_to_binary(GuildId),
        <<"unavailable">> => true,
        <<"unavailable_hidden">> => true
    };
build_unavailable_guild_data(GuildId, false) ->
    #{<<"id">> => integer_to_binary(GuildId), <<"unavailable">> => true}.

-spec store_guild_subscriptions(map(), session_state()) -> session_state().
store_guild_subscriptions(Data, State) when is_map(Data) ->
    case maps:get(<<"subscriptions">>, Data, undefined) of
        Subscriptions when is_map(Subscriptions) ->
            State1 = merge_subscriptions(Subscriptions, State),
            State2 = merge_active_guilds(Subscriptions, State1),
            reconcile_active_member_list(Subscriptions, State2);
        _ ->
            State
    end;
store_guild_subscriptions(_Data, State) ->
    State.

-spec reconcile_active_member_list(map(), session_state()) -> session_state().
reconcile_active_member_list(Subscriptions, State) ->
    case find_active_member_list_guild(Subscriptions) of
        undefined ->
            State;
        GuildId ->
            switch_active_member_list_guild(GuildId, State)
    end.

-spec find_active_member_list_guild(map()) -> guild_id() | undefined.
find_active_member_list_guild(Subscriptions) ->
    maps:fold(fun find_active_member_list_guild_fold/3, undefined, Subscriptions).

-spec find_active_member_list_guild_fold(term(), term(), guild_id() | undefined) ->
    guild_id() | undefined.
find_active_member_list_guild_fold(_GuildIdBin, _GuildSubData, Found) when
    Found =/= undefined
->
    Found;
find_active_member_list_guild_fold(GuildIdBin, GuildSubData, undefined) when
    is_binary(GuildIdBin), is_map(GuildSubData)
->
    case has_non_empty_member_list_subscribe(GuildSubData) of
        true ->
            valid_guild_id_or_undefined(GuildIdBin);
        false ->
            undefined
    end;
find_active_member_list_guild_fold(_, _, Found) ->
    Found.

-spec valid_guild_id_or_undefined(binary()) -> guild_id() | undefined.
valid_guild_id_or_undefined(GuildIdBin) ->
    case validation:validate_snowflake(<<"guild_id">>, GuildIdBin) of
        {ok, GuildId} -> GuildId;
        _ -> undefined
    end.

-spec has_non_empty_member_list_subscribe(map()) -> boolean().
has_non_empty_member_list_subscribe(GuildSubData) ->
    case maps:get(<<"member_list_channels">>, GuildSubData, undefined) of
        Channels when is_map(Channels) ->
            member_list_channels_has_ranges(Channels);
        _ ->
            false
    end.

-spec member_list_channels_has_ranges(map()) -> boolean().
member_list_channels_has_ranges(Channels) ->
    maps:fold(
        fun(_ChannelId, Ranges, Acc) -> Acc orelse is_non_empty_ranges(Ranges) end,
        false,
        Channels
    ).

-spec is_non_empty_ranges(term()) -> boolean().
is_non_empty_ranges(Ranges) when is_list(Ranges) ->
    Ranges =/= [];
is_non_empty_ranges(_) ->
    false.

-spec switch_active_member_list_guild(guild_id(), session_state()) -> session_state().
switch_active_member_list_guild(GuildId, State) ->
    case maps:get(active_member_list_guild, State, undefined) of
        GuildId ->
            State;
        undefined ->
            State#{active_member_list_guild => GuildId};
        PrevGuildId when is_integer(PrevGuildId) ->
            drop_member_lists_in_guild(PrevGuildId, State),
            State#{active_member_list_guild => GuildId};
        _ ->
            State#{active_member_list_guild => GuildId}
    end.

-spec drop_member_lists_in_guild(guild_id(), session_state()) -> ok.
drop_member_lists_in_guild(GuildId, State) ->
    Guilds = maps:get(guilds, State, #{}),
    SessionId = maps:get(id, State, undefined),
    case {maps:get(GuildId, Guilds, undefined), SessionId} of
        {{Pid, _Ref}, SId} when is_pid(Pid), is_binary(SId) ->
            _ = shard_utils:safe_cast(Pid, {drop_session_member_lists, SId}),
            ok;
        _ ->
            ok
    end.

-spec merge_subscriptions(map(), session_state()) -> session_state().
merge_subscriptions(Subscriptions, State) ->
    Current = maps:get(guild_subscription_state, State, #{}),
    Updated = maps:fold(
        fun merge_single/3,
        Current,
        Subscriptions
    ),
    State#{guild_subscription_state => Updated}.

-spec merge_single(binary(), term(), map()) -> map().
merge_single(GuildIdBin, GuildSubData, Acc) when is_map(GuildSubData) ->
    case validation:validate_snowflake(<<"guild_id">>, GuildIdBin) of
        {ok, GuildId} ->
            Existing = maps:get(GuildId, Acc, #{}),
            Merged = merge_guild_subscription_data(Existing, GuildSubData),
            Acc#{GuildId => Merged};
        _ ->
            Acc
    end;
merge_single(_, _, Acc) ->
    Acc.

-spec merge_guild_subscription_data(map(), map()) -> map().
merge_guild_subscription_data(Existing, Incoming) ->
    maps:fold(fun merge_subscription_key/3, Existing, Incoming).

-spec merge_subscription_key(binary(), term(), map()) -> map().
merge_subscription_key(<<"member_list_channels">>, Channels, Acc) when is_map(Channels) ->
    ExistingChannels = maps:get(<<"member_list_channels">>, Acc, #{}),
    Acc#{<<"member_list_channels">> => maps:merge(ExistingChannels, Channels)};
merge_subscription_key(Key, Value, Acc) ->
    Acc#{Key => Value}.

-spec merge_active_guilds(map(), session_state()) -> session_state().
merge_active_guilds(Subscriptions, State) ->
    ActiveGuilds = maps:get(active_guilds, State, sets:new()),
    UpdatedActiveGuilds = maps:fold(
        fun merge_active_guild/3,
        ActiveGuilds,
        Subscriptions
    ),
    State#{active_guilds => UpdatedActiveGuilds}.

-spec merge_active_guild(binary(), term(), sets:set(guild_id())) -> sets:set(guild_id()).
merge_active_guild(GuildIdBin, GuildSubData, ActiveGuilds) when is_map(GuildSubData) ->
    case validation:validate_snowflake(<<"guild_id">>, GuildIdBin) of
        {ok, GuildId} ->
            apply_active_flag(
                GuildId, maps:get(<<"active">>, GuildSubData, undefined), ActiveGuilds
            );
        _ ->
            ActiveGuilds
    end;
merge_active_guild(_, _, ActiveGuilds) ->
    ActiveGuilds.

-spec apply_active_flag(guild_id(), term(), sets:set(guild_id())) -> sets:set(guild_id()).
apply_active_flag(GuildId, true, ActiveGuilds) ->
    sets:add_element(GuildId, ActiveGuilds);
apply_active_flag(GuildId, false, ActiveGuilds) ->
    sets:del_element(GuildId, ActiveGuilds);
apply_active_flag(_GuildId, _Value, ActiveGuilds) ->
    ActiveGuilds.

-spec remove_guild_subscription_state(guild_id(), session_state()) -> session_state().
remove_guild_subscription_state(GuildId, State) ->
    Current = maps:get(guild_subscription_state, State, #{}),
    ActiveGuilds = maps:get(active_guilds, State, sets:new()),
    State1 = State#{
        guild_subscription_state => maps:remove(GuildId, Current),
        active_guilds => sets:del_element(GuildId, ActiveGuilds)
    },
    clear_active_member_list_guild_if(GuildId, State1).

-spec clear_active_member_list_guild_if(guild_id(), session_state()) -> session_state().
clear_active_member_list_guild_if(GuildId, State) ->
    case maps:get(active_member_list_guild, State, undefined) of
        GuildId -> maps:remove(active_member_list_guild, State);
        _ -> State
    end.
