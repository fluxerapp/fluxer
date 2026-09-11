%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_dm_partners).
-typing([eqwalizer]).

-export([handle_cast/2, maybe_reevaluate/4]).

-type guild_state() :: map().
-type session_id() :: binary().
-type user_id() :: integer().
-type registration() :: #{
    user_id := user_id(),
    pid := pid(),
    partners := #{user_id() => true},
    eligible := #{user_id() => true}
}.
-type registrations() :: #{session_id() => registration()}.
-type scope() :: none | all | {user, user_id()}.

-export_type([guild_state/0]).

-define(MAX_PARTNERS, 1000).
-define(PRUNE_SLACK, 64).

-spec handle_cast(term(), guild_state()) -> {noreply, guild_state()}.
handle_cast({update_dm_partners, SessionId, PartnerIds}, State) when
    is_binary(SessionId), is_list(PartnerIds)
->
    {noreply, update(SessionId, PartnerIds, State)};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec maybe_reevaluate(term(), term(), guild_state(), guild_state()) -> guild_state().
maybe_reevaluate(Event, Data, PreviousState, State) ->
    Registrations = registrations(State),
    case map_size(Registrations) of
        0 ->
            State;
        _ ->
            reevaluate(
                effective_scope(scope(Event, Data), PreviousState, State), Registrations, State
            )
    end.

-spec effective_scope(scope(), guild_state(), guild_state()) -> scope().
effective_scope(all, PreviousState, State) ->
    case visibility_inputs(PreviousState) =:= visibility_inputs(State) of
        true -> none;
        false -> all
    end;
effective_scope(Scope, _PreviousState, _State) ->
    Scope.

-spec visibility_inputs(guild_state()) -> term().
visibility_inputs(State) ->
    Data = map_utils:ensure_map(maps:get(data, State, #{})),
    Guild = map_utils:ensure_map(maps:get(<<"guild">>, Data, #{})),
    {
        maps:get(<<"owner_id">>, Guild, undefined),
        role_inputs(guild_data_index:role_index(Data)),
        channel_inputs(guild_data_index:channel_index(Data)),
        maps:get(virtual_channel_access, State, #{})
    }.

-spec role_inputs(term()) -> term().
role_inputs(Roles) when is_map(Roles) ->
    lists:sort([
        {Id, maps:get(<<"permissions">>, Role, undefined)}
     || {Id, Role} <- maps:to_list(Roles), is_map(Role)
    ]);
role_inputs(Roles) ->
    Roles.

-spec channel_inputs(term()) -> term().
channel_inputs(Channels) when is_map(Channels) ->
    lists:sort([
        {
            Id,
            maps:get(<<"type">>, Channel, undefined),
            maps:get(<<"parent_id">>, Channel, undefined),
            maps:get(<<"permission_overwrites">>, Channel, [])
        }
     || {Id, Channel} <- maps:to_list(Channels), is_map(Channel)
    ]);
channel_inputs(Channels) ->
    Channels.

-spec update(session_id(), [term()], guild_state()) -> guild_state().
update(SessionId, PartnerIds, State) ->
    Registrations = registrations(State),
    Next =
        case session_owner(SessionId, State) of
            {ok, UserId, Pid} ->
                update_owned(SessionId, UserId, Pid, PartnerIds, Registrations, State);
            error ->
                maps:remove(SessionId, Registrations)
        end,
    State#{dm_partners => maybe_prune(Next, State)}.

-spec update_owned(session_id(), user_id(), pid(), [term()], registrations(), guild_state()) ->
    registrations().
update_owned(SessionId, UserId, Pid, PartnerIds, Registrations, State) ->
    case presence_targets:dm_partner_presence_enabled(UserId) of
        false ->
            maps:remove(SessionId, Registrations);
        true ->
            Entry = #{
                user_id => UserId,
                pid => Pid,
                partners => partner_set(PartnerIds, UserId),
                eligible => previous_eligible(
                    maps:get(SessionId, Registrations, undefined), Pid
                )
            },
            put_all(evaluate(#{SessionId => Entry}, State), Registrations)
    end.

-spec reevaluate(scope(), registrations(), guild_state()) -> guild_state().
reevaluate(none, _Registrations, State) ->
    State;
reevaluate(all, Registrations, State) ->
    State#{dm_partners => evaluate(live_registrations(Registrations, State), State)};
reevaluate({user, UserId}, Registrations, State) ->
    Affected = maps:fold(
        fun(SessionId, Entry, Acc) ->
            case involves_user(UserId, Entry) of
                true -> Acc#{SessionId => Entry};
                false -> Acc
            end
        end,
        #{},
        Registrations
    ),
    case map_size(Affected) of
        0 ->
            State;
        _ ->
            Live = live_registrations(Affected, State),
            Kept = maps:without(
                maps:keys(maps:without(maps:keys(Live), Affected)), Registrations
            ),
            State#{dm_partners => maybe_prune(put_all(evaluate(Live, State), Kept), State)}
    end.

-spec put_all(registrations(), registrations()) -> registrations().
put_all(Entries, Registrations) ->
    maps:fold(
        fun(SessionId, Entry, Acc) -> Acc#{SessionId => Entry} end, Registrations, Entries
    ).

-spec maybe_prune(registrations(), guild_state()) -> registrations().
maybe_prune(Registrations, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case map_size(Registrations) > map_size(Sessions) + ?PRUNE_SLACK of
        true ->
            maps:filter(
                fun(SessionId, _Entry) -> maps:is_key(SessionId, Sessions) end, Registrations
            );
        false ->
            Registrations
    end.

-spec evaluate(registrations(), guild_state()) -> registrations().
evaluate(Entries, _State) when map_size(Entries) =:= 0 ->
    Entries;
evaluate(Entries, State) ->
    Requests = [
        {SessionId, UserId, maps:keys(Partners)}
     || {SessionId, #{user_id := UserId, partners := Partners}} <- maps:to_list(Entries)
    ],
    Results = guild_subscription_mutual_channels:filter_session_member_ids(Requests, State),
    GuildId = maps:get(id, State),
    maps:map(
        fun(SessionId, Entry) ->
            apply_result(GuildId, maps:get(SessionId, Results, []), Entry)
        end,
        Entries
    ).

-spec apply_result(integer(), [user_id()], registration()) -> registration().
apply_result(GuildId, EligibleIds, #{pid := Pid, eligible := Previous} = Entry) ->
    Eligible = presence_targets:map_from_ids(EligibleIds),
    ok = notify_if_changed(Pid, GuildId, Previous, Eligible),
    Entry#{eligible := Eligible}.

-spec notify_if_changed(pid(), integer(), #{user_id() => true}, #{user_id() => true}) -> ok.
notify_if_changed(_Pid, _GuildId, Same, Same) ->
    ok;
notify_if_changed(Pid, GuildId, _Previous, Eligible) ->
    Pid ! {dm_partner_mutual, GuildId, maps:keys(Eligible)},
    ok.

-spec live_registrations(registrations(), guild_state()) -> registrations().
live_registrations(Registrations, State) ->
    Sessions = maps:get(sessions, State, #{}),
    maps:filter(
        fun(SessionId, #{user_id := UserId}) ->
            maps:is_key(SessionId, Sessions) andalso
                presence_targets:dm_partner_presence_enabled(UserId)
        end,
        Registrations
    ).

-spec involves_user(user_id(), registration()) -> boolean().
involves_user(UserId, #{user_id := UserId}) ->
    true;
involves_user(UserId, #{partners := Partners}) ->
    maps:is_key(UserId, Partners).

-spec scope(term(), term()) -> scope().
scope(guild_member_add, Data) -> member_scope(Data);
scope(guild_member_remove, Data) -> member_scope(Data);
scope(guild_member_update, Data) -> member_scope(Data);
scope(guild_update, _Data) -> all;
scope(guild_role_update, _Data) -> all;
scope(guild_role_update_bulk, _Data) -> all;
scope(guild_role_delete, _Data) -> all;
scope(channel_create, _Data) -> all;
scope(channel_update, _Data) -> all;
scope(channel_update_bulk, _Data) -> all;
scope(channel_delete, _Data) -> all;
scope(_Event, _Data) -> none.

-spec member_scope(term()) -> scope().
member_scope(#{<<"user">> := #{<<"id">> := RawUserId}}) ->
    case snowflake_id:parse_maybe(RawUserId) of
        UserId when is_integer(UserId) -> {user, UserId};
        _ -> all
    end;
member_scope(_Data) ->
    all.

-spec session_owner(session_id(), guild_state()) -> {ok, user_id(), pid()} | error.
session_owner(SessionId, State) ->
    case maps:get(SessionId, maps:get(sessions, State, #{}), undefined) of
        #{user_id := UserId, pid := Pid} when is_integer(UserId), is_pid(Pid) ->
            {ok, UserId, Pid};
        _ ->
            error
    end.

-spec previous_eligible(registration() | undefined, pid()) -> #{user_id() => true}.
previous_eligible(#{pid := Pid, eligible := Eligible}, Pid) ->
    Eligible;
previous_eligible(_Previous, _Pid) ->
    #{}.

-spec partner_set([term()], user_id()) -> #{user_id() => true}.
partner_set(PartnerIds, UserId) ->
    presence_targets:map_from_ids(
        lists:sublist([Id || Id <- PartnerIds, is_integer(Id), Id =/= UserId], ?MAX_PARTNERS)
    ).

-spec registrations(guild_state()) -> registrations().
registrations(State) ->
    case maps:get(dm_partners, State, #{}) of
        Registrations when is_map(Registrations) -> Registrations;
        _ -> #{}
    end.
