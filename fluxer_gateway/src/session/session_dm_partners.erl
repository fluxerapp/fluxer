%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_dm_partners).
-typing([eqwalizer]).

-export([register_all/1, register_guild/3, handle_mutual/3, forget_guild/2]).

-type session_state() :: map().
-type guild_id() :: integer().
-type user_id() :: integer().

-define(MAX_PARTNERS, 1000).

-spec register_all(session_state()) -> session_state().
register_all(State) ->
    case enabled(State) of
        true ->
            Partners = partner_ids(State),
            maps:foreach(
                fun(_GuildId, GuildRef) -> cast_partners(GuildRef, Partners, State) end,
                maps:get(guilds, State, #{})
            ),
            State;
        false ->
            State
    end.

-spec register_guild(guild_id(), pid(), session_state()) -> session_state().
register_guild(_GuildId, GuildPid, State) ->
    case enabled(State) of
        true ->
            maybe_cast_partners({GuildPid, undefined}, partner_ids(State), State);
        false ->
            State
    end.

-spec maybe_cast_partners(term(), [user_id()], session_state()) -> session_state().
maybe_cast_partners(_GuildRef, [], State) ->
    State;
maybe_cast_partners(GuildRef, Partners, State) ->
    ok = cast_partners(GuildRef, Partners, State),
    State.

-spec handle_mutual(guild_id(), [user_id()], session_state()) -> {noreply, session_state()}.
handle_mutual(GuildId, PartnerIds, State) ->
    case maps:get(GuildId, maps:get(guilds, State, #{}), undefined) of
        {Pid, _Ref} when is_pid(Pid) ->
            {noreply, put_guild_partners(GuildId, PartnerIds, State)};
        _ ->
            {noreply, State}
    end.

-spec forget_guild(guild_id(), session_state()) -> session_state().
forget_guild(GuildId, State) ->
    Current = maps:get(dm_mutual_by_guild, State, #{}),
    State#{dm_mutual_by_guild => maps:remove(GuildId, Current)}.

-spec put_guild_partners(guild_id(), [user_id()], session_state()) -> session_state().
put_guild_partners(GuildId, PartnerIds, State) ->
    Current = maps:get(dm_mutual_by_guild, State, #{}),
    Next =
        case presence_targets:map_from_ids(PartnerIds) of
            Empty when map_size(Empty) =:= 0 -> maps:remove(GuildId, Current);
            Ids -> Current#{GuildId => Ids}
        end,
    case Next =:= Current of
        true ->
            State;
        false ->
            session_dispatch_presence:sync_presence_targets(State#{dm_mutual_by_guild => Next})
    end.

-spec cast_partners(term(), [user_id()], session_state()) -> ok.
cast_partners({Pid, _Ref}, Partners, State) when is_pid(Pid) ->
    gen_server:cast(Pid, {update_dm_partners, maps:get(id, State), Partners});
cast_partners(_GuildRef, _Partners, _State) ->
    ok.

-spec partner_ids(session_state()) -> [user_id()].
partner_ids(State) ->
    lists:sublist(presence_targets:direct_dm_partner_ids(State), ?MAX_PARTNERS).

-spec enabled(session_state()) -> boolean().
enabled(State) ->
    maps:get(bot, State, false) =/= true andalso
        presence_targets:dm_partner_presence_enabled(maps:get(user_id, State, undefined)).
