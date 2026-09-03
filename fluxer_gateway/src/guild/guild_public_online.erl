%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_public_online).
-typing([eqwalizer]).

-export([compute_count/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export_type([guild_state/0]).

-type guild_state() :: map().
-type channel() :: map().
-type user_id() :: integer().

-define(ALL_PERMS_VIEWER_KEY, public_online_all_perms_viewer).

-spec compute_count(guild_state()) -> non_neg_integer().
compute_count(State) ->
    case resolve_data(State) of
        undefined ->
            0;
        Data ->
            compute_count_for_data(Data, State)
    end.

-spec compute_count_for_data(map(), guild_state()) -> non_neg_integer().
compute_count_for_data(Data, State) ->
    Channels = [C || C <- ensure_list(maps:get(<<"channels">>, Data, [])), is_map(C)],
    case classify_everyone_viewable(Channels, State) of
        {open, _ChannelIds} -> guild_member_list:get_online_count(State);
        {restricted, ChannelIdSet} -> count_online_with_access(ChannelIdSet, State);
        empty -> 0
    end.

-spec resolve_data(guild_state()) -> map() | undefined.
resolve_data(State) ->
    case maps:get(data, State, undefined) of
        D when is_map(D) -> D;
        _ -> undefined
    end.

-spec classify_everyone_viewable([channel()], guild_state()) ->
    {open, [integer()]} | {restricted, sets:set(integer())} | empty.
classify_everyone_viewable(Channels, State) ->
    EveryoneMember = #{<<"user">> => #{<<"id">> => <<"0">>}, <<"roles">> => []},
    ViewBit = constants:view_channel_permission(),
    {OpenAcc, RestrictedAcc} = lists:foldl(
        fun(Channel, {OpenIds, RestrictedIds}) ->
            classify_channel_visibility(Channel, EveryoneMember, ViewBit, State, {
                OpenIds, RestrictedIds
            })
        end,
        {[], []},
        Channels
    ),
    case {OpenAcc, RestrictedAcc} of
        {[], []} -> empty;
        {[_ | _], _} -> {open, OpenAcc};
        {[], _} -> classify_restricted(RestrictedAcc)
    end.

-spec classify_restricted([integer()]) ->
    {open, [integer()]} | {restricted, sets:set(integer())}.
classify_restricted(RestrictedIds) ->
    case application:get_env(fluxer_gateway, ?ALL_PERMS_VIEWER_KEY, false) of
        true -> {open, RestrictedIds};
        _ -> {restricted, sets:from_list(RestrictedIds)}
    end.

-spec classify_channel_visibility(
    channel(), map(), integer(), guild_state(), {[integer()], [integer()]}
) -> {[integer()], [integer()]}.
classify_channel_visibility(Channel, EveryoneMember, ViewBit, State, {OpenIds, RestrictedIds}) ->
    case channel_id(Channel) of
        ChannelId when is_integer(ChannelId), ChannelId > 0 ->
            classify_valid_channel(ChannelId, Channel, EveryoneMember, ViewBit, State, {
                OpenIds, RestrictedIds
            });
        _ ->
            {OpenIds, RestrictedIds}
    end.

-spec classify_valid_channel(
    integer(), channel(), map(), integer(), guild_state(), {[integer()], [integer()]}
) -> {[integer()], [integer()]}.
classify_valid_channel(ChannelId, Channel, EveryoneMember, ViewBit, State, Acc) ->
    case
        guild_permissions:can_view_channel_by_permissions(0, ChannelId, EveryoneMember, State)
    of
        true -> classify_visible_channel(ChannelId, Channel, ViewBit, Acc);
        false -> Acc
    end.

-spec classify_visible_channel(integer(), channel(), integer(), {[integer()], [integer()]}) ->
    {[integer()], [integer()]}.
classify_visible_channel(ChannelId, Channel, ViewBit, {OpenIds, RestrictedIds}) ->
    case channel_has_view_restricting_overrides(Channel, ViewBit) of
        false -> {[ChannelId | OpenIds], RestrictedIds};
        true -> {OpenIds, [ChannelId | RestrictedIds]}
    end.

-spec channel_has_view_restricting_overrides(channel(), integer()) -> boolean().
channel_has_view_restricting_overrides(Channel, ViewBit) ->
    Overwrites =
        case maps:get(<<"permission_overwrites">>, Channel, []) of
            L when is_list(L) -> L;
            _ -> []
        end,
    lists:any(
        fun
            (OW) when is_map(OW) ->
                Deny = permission_bits:parse(maps:get(<<"deny">>, OW, undefined)),
                Type = map_utils:get_integer(OW, <<"type">>, undefined),
                is_integer(Deny) andalso permission_bits:has(Deny, ViewBit) andalso
                    (Type =:= 0 orelse Type =:= 1);
            (_) ->
                false
        end,
        Overwrites
    ).

-spec count_online_with_access(sets:set(integer()), guild_state()) -> non_neg_integer().
count_online_with_access(ChannelIdSet, State) ->
    TargetMap = maps:from_list([{Ch, true} || Ch <- sets:to_list(ChannelIdSet)]),
    Tab = maps:get(member_presence, State),
    ets:foldl(
        fun({UserId, Presence}, Acc) ->
            maybe_count_online_user(UserId, Presence, TargetMap, State, Acc)
        end,
        0,
        Tab
    ).

-spec maybe_count_online_user(term(), term(), map(), guild_state(), non_neg_integer()) ->
    non_neg_integer().
maybe_count_online_user(UserId, Presence, TargetMap, State, Acc) when
    is_map(Presence), is_integer(UserId), UserId > 0
->
    case is_online(Presence) of
        true -> count_user_with_access(UserId, TargetMap, State, Acc);
        false -> Acc
    end;
maybe_count_online_user(_UserId, _Presence, _TargetMap, _State, Acc) ->
    Acc.

-spec count_user_with_access(user_id(), map(), guild_state(), non_neg_integer()) ->
    non_neg_integer().
count_user_with_access(UserId, TargetMap, State, Acc) ->
    case user_has_access(UserId, TargetMap, State) of
        true -> Acc + 1;
        false -> Acc
    end.

-spec user_has_access(user_id(), map(), guild_state()) -> boolean().
user_has_access(UserId, TargetMap, State) ->
    UserChannels = guild_visibility:get_user_viewable_channels(UserId, State),
    lists:any(fun(Ch) -> maps:is_key(Ch, TargetMap) end, UserChannels).

-spec is_online(map()) -> boolean().
is_online(Presence) ->
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    Status =/= <<"offline">> andalso Status =/= <<"invisible">>.

-spec channel_id(map()) -> integer() | undefined.
channel_id(Channel) when is_map(Channel) ->
    snowflake_id:parse_optional(maps:get(<<"id">>, Channel, undefined));
channel_id(_) ->
    undefined.

-spec ensure_list(term()) -> [term()].
ensure_list(L) when is_list(L) -> L;
ensure_list(M) when is_map(M) -> maps:values(M);
ensure_list(_) -> [].

-ifdef(TEST).

guild_id() -> 1.
view_perm() -> constants:view_channel_permission().

base_role(GuildId, Perms) ->
    #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => integer_to_binary(Perms)}.

member(UserId, RoleIds) ->
    #{
        <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
        <<"roles">> => [integer_to_binary(R) || R <- RoleIds]
    }.

presence(Status) ->
    #{<<"status">> => Status}.

build_state(Channels, Members, Roles, Presences) ->
    GuildId = guild_id(),
    Tab = ets:new(test_member_presence, [set, public]),
    lists:foreach(
        fun({UserId, Status}) -> ets:insert(Tab, {UserId, presence(Status)}) end,
        Presences
    ),
    #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [base_role(GuildId, view_perm()) | Roles],
            <<"members">> => maps:from_list(
                [{UserId, member(UserId, RoleIds)} || {UserId, RoleIds} <- Members]
            ),
            <<"channels">> => Channels
        },
        member_presence => Tab,
        member_list_engine => build_engine(Presences),
        sessions => #{}
    }.

build_engine(Presences) ->
    Online = length([S || {_UserId, S} <- Presences, S =/= <<"offline">>, S =/= <<"invisible">>]),
    Ref = ets:new(test_member_list_engine, [set, public]),
    ets:insert(Ref, [{total_count, length(Presences)}, {online_count, Online}]),
    Ref.

deny_view_overwrite(Id, Type) ->
    #{
        <<"id">> => integer_to_binary(Id),
        <<"type">> => Type,
        <<"allow">> => <<"0">>,
        <<"deny">> => integer_to_binary(view_perm())
    }.

channel(ChannelId, Overwrites) ->
    #{
        <<"id">> => integer_to_binary(ChannelId),
        <<"type">> => 0,
        <<"permission_overwrites">> => Overwrites
    }.

role(RoleId) ->
    #{<<"id">> => integer_to_binary(RoleId), <<"permissions">> => <<"0">>}.

with_all_perms_viewer(Fun) ->
    Previous = application:get_env(fluxer_gateway, ?ALL_PERMS_VIEWER_KEY),
    application:set_env(fluxer_gateway, ?ALL_PERMS_VIEWER_KEY, true),
    try
        Fun()
    after
        restore_all_perms_viewer(Previous)
    end.

restore_all_perms_viewer(undefined) ->
    application:unset_env(fluxer_gateway, ?ALL_PERMS_VIEWER_KEY);
restore_all_perms_viewer({ok, Value}) ->
    application:set_env(fluxer_gateway, ?ALL_PERMS_VIEWER_KEY, Value).

verify_gate_state() ->
    VerifiedRoleId = 4242,
    Channels = [channel(400, [deny_view_overwrite(VerifiedRoleId, 0)])],
    Members = [{1, [VerifiedRoleId]}, {2, [VerifiedRoleId]}, {3, [VerifiedRoleId]}, {4, []}],
    Presences = [
        {1, <<"online">>},
        {2, <<"online">>},
        {3, <<"idle">>},
        {4, <<"online">>}
    ],
    build_state(Channels, Members, [role(VerifiedRoleId)], Presences).

user_deny_state() ->
    Channels = [channel(700, [deny_view_overwrite(42, 1)])],
    Members = [{41, []}, {42, []}, {43, []}],
    Presences = [{41, <<"online">>}, {42, <<"online">>}, {43, <<"online">>}],
    build_state(Channels, Members, [], Presences).

returns_zero_when_no_channels_test() ->
    State = build_state([], [], [], []),
    ?assertEqual(0, compute_count(State)).

returns_zero_when_data_missing_test() ->
    ?assertEqual(0, compute_count(#{})).

slow_path_with_role_view_deny_test() ->
    GuildId = guild_id(),
    BotRoleId = 555,
    Channels = [
        #{
            <<"id">> => <<"100">>,
            <<"type">> => 0,
            <<"permission_overwrites">> => [
                #{
                    <<"id">> => integer_to_binary(BotRoleId),
                    <<"type">> => 0,
                    <<"allow">> => <<"0">>,
                    <<"deny">> => integer_to_binary(view_perm())
                }
            ]
        }
    ],
    Roles = [#{<<"id">> => integer_to_binary(BotRoleId), <<"permissions">> => <<"0">>}],
    Members = [{10, []}, {11, [BotRoleId]}, {12, []}],
    Presences = [{10, <<"online">>}, {11, <<"online">>}, {12, <<"offline">>}],
    State0 = build_state(Channels, Members, Roles, Presences),
    State = State0,
    _ = GuildId,
    ?assertEqual(1, compute_count(State)).

slow_path_skips_invisible_and_offline_test() ->
    BotRoleId = 777,
    Channels = [
        #{
            <<"id">> => <<"200">>,
            <<"type">> => 0,
            <<"permission_overwrites">> => [
                #{
                    <<"id">> => integer_to_binary(BotRoleId),
                    <<"type">> => 0,
                    <<"allow">> => <<"0">>,
                    <<"deny">> => integer_to_binary(view_perm())
                }
            ]
        }
    ],
    Members = [{1, []}, {2, []}, {3, []}, {4, []}],
    Presences = [
        {1, <<"online">>},
        {2, <<"idle">>},
        {3, <<"invisible">>},
        {4, <<"offline">>}
    ],
    Roles = [#{<<"id">> => integer_to_binary(BotRoleId), <<"permissions">> => <<"0">>}],
    State = build_state(Channels, Members, Roles, Presences),
    ?assertEqual(2, compute_count(State)).

returns_zero_when_no_everyone_viewable_test() ->
    GuildId = guild_id(),
    Channels = [#{<<"id">> => <<"300">>, <<"type">> => 0, <<"permission_overwrites">> => []}],
    Members = [{1, []}],
    Presences = [{1, <<"online">>}],
    State0 = build_state(Channels, Members, [], Presences),
    OldData = maps:get(data, State0),
    Roles = [#{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>}],
    State = State0#{data => OldData#{<<"roles">> => Roles}},
    ?assertEqual(0, compute_count(State)).

channel_has_view_restricting_overrides_detects_user_deny_test() ->
    Channel = #{
        <<"permission_overwrites">> => [
            #{
                <<"id">> => <<"42">>,
                <<"type">> => 1,
                <<"allow">> => <<"0">>,
                <<"deny">> => integer_to_binary(view_perm())
            }
        ]
    },
    ?assert(channel_has_view_restricting_overrides(Channel, view_perm())).

channel_has_view_restricting_overrides_ignores_allow_only_test() ->
    Channel = #{
        <<"permission_overwrites">> => [
            #{
                <<"id">> => <<"42">>,
                <<"type">> => 0,
                <<"allow">> => integer_to_binary(view_perm()),
                <<"deny">> => <<"0">>
            }
        ]
    },
    ?assertNot(channel_has_view_restricting_overrides(Channel, view_perm())).

channel_has_view_restricting_overrides_no_overwrites_test() ->
    Channel = #{<<"permission_overwrites">> => []},
    ?assertNot(channel_has_view_restricting_overrides(Channel, view_perm())).

verify_gate_collapses_count_without_flag_test() ->
    ?assertEqual(1, compute_count(verify_gate_state())).

verify_gate_reports_full_count_with_flag_test() ->
    State = verify_gate_state(),
    ?assertEqual(4, with_all_perms_viewer(fun() -> compute_count(State) end)).

everyone_deny_channel_stays_excluded_with_flag_test() ->
    GuildId = guild_id(),
    Channels = [channel(500, [deny_view_overwrite(GuildId, 0)])],
    State = build_state(Channels, [{1, []}, {2, []}], [], [{1, <<"online">>}, {2, <<"idle">>}]),
    ?assertEqual(0, compute_count(State)),
    ?assertEqual(0, with_all_perms_viewer(fun() -> compute_count(State) end)).

no_channels_stays_zero_with_flag_test() ->
    State = build_state([], [{1, []}], [], [{1, <<"online">>}]),
    ?assertEqual(0, with_all_perms_viewer(fun() -> compute_count(State) end)).

plain_open_channel_unchanged_by_flag_test() ->
    Members = [{1, []}, {2, []}, {3, []}],
    Presences = [{1, <<"online">>}, {2, <<"idle">>}, {3, <<"offline">>}],
    State = build_state([channel(600, [])], Members, [], Presences),
    ?assertEqual(2, compute_count(State)),
    ?assertEqual(2, with_all_perms_viewer(fun() -> compute_count(State) end)).

user_deny_collapses_count_without_flag_test() ->
    ?assertEqual(2, compute_count(user_deny_state())).

user_deny_reports_full_count_with_flag_test() ->
    State = user_deny_state(),
    ?assertEqual(3, with_all_perms_viewer(fun() -> compute_count(State) end)).

-endif.
