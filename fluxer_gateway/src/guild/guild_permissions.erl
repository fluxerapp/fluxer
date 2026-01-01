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

-module(guild_permissions).

-define(ALL_PERMISSIONS, 16#FFFFFFFFFFFFFFFF).

-export([get_member_permissions/3]).
-export([can_view_channel/4]).
-export([can_view_channel_by_permissions/4]).
-export([can_manage_channel/3]).
-export([apply_channel_overwrites/5]).
-export([get_max_role_position/2]).
-export([find_member_by_user_id/2]).
-export([find_role_by_id/2]).
-export([find_channel_by_id/2]).

-export_type([permission/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type permission() :: non_neg_integer().
-type user_id() :: integer().
-type role_id() :: integer().
-type channel_id() :: integer().
-type maybe_channel_id() :: channel_id() | undefined.
-type guild_state() :: map().
-type guild_data() :: map().
-type member() :: map().
-type role() :: map().
-type channel() :: map().
-type overwrite() :: map().
-type member_roles() :: [role_id()].
-type maybe_member() :: member() | undefined.

-spec get_member_permissions(user_id(), maybe_channel_id(), guild_state()) -> permission().
get_member_permissions(UserId, ChannelId, State) ->
    compute_member_permissions(UserId, ChannelId, undefined, State).

-spec can_view_channel(user_id(), channel_id(), maybe_member(), guild_state()) -> boolean().
can_view_channel(UserId, ChannelId, Member, State) ->
    guild_virtual_channel_access:has_virtual_access(UserId, ChannelId, State) orelse
        can_view_channel_by_permissions(UserId, ChannelId, Member, State).

-spec can_view_channel_by_permissions(user_id(), channel_id(), maybe_member(), guild_state()) ->
    boolean().
can_view_channel_by_permissions(UserId, ChannelId, Member, State) ->
    (compute_member_permissions(UserId, ChannelId, Member, State) band
        constants:view_channel_permission()) =/= 0.

-spec can_manage_channel(user_id(), maybe_channel_id(), guild_state()) -> boolean().
can_manage_channel(UserId, ChannelId, State) ->
    (get_member_permissions(UserId, ChannelId, State) band
        constants:manage_channels_permission()) =/= 0.

-spec apply_channel_overwrites(permission(), user_id(), member_roles(), channel(), role_id()) ->
    permission().
apply_channel_overwrites(BasePerms, UserId, MemberRoles, Channel, EveryoneRoleId) ->
    Overwrites = channel_overwrites(Channel),
    EveryonePerms = apply_everyone_overwrites(BasePerms, Overwrites, EveryoneRoleId),
    {RoleAllow, RoleDeny} = accumulate_role_overwrites(MemberRoles, Overwrites),
    RolePerms = (EveryonePerms band bnot RoleDeny) bor RoleAllow,
    apply_user_overwrites(RolePerms, Overwrites, UserId).

-spec get_max_role_position(user_id(), guild_state()) -> integer().
get_max_role_position(UserId, State) ->
    case {find_member_by_user_id(UserId, State), resolve_data_map(State)} of
        {undefined, _} ->
            -1;
        {_, undefined} ->
            -1;
        {Member, Data} ->
            Roles = ensure_list(maps:get(<<"roles">>, Data, [])),
            lists:foldl(
                fun(RoleId, MaxPos) ->
                    case find_role_by_id(RoleId, Roles) of
                        undefined ->
                            MaxPos;
                        Role ->
                            Position = maps:get(<<"position">>, Role, 0),
                            max(Position, MaxPos)
                    end
                end,
                -1,
                member_role_ids(Member)
            )
    end.

-spec find_member_by_user_id(user_id(), guild_state()) -> member() | undefined.
find_member_by_user_id(UserId, State) when is_integer(UserId) ->
    case resolve_data_map(State) of
        undefined ->
            undefined;
        Data ->
            Members = ensure_list(maps:get(<<"members">>, Data, [])),
            lists:foldl(
                fun(Member, Acc) ->
                    case Acc of
                        undefined ->
                            MUser = maps:get(<<"user">>, Member, #{}),
                            MemberId = to_int(maps:get(<<"id">>, MUser, <<"0">>)),
                            case MemberId =:= UserId of
                                true -> Member;
                                false -> undefined
                            end;
                        Found ->
                            Found
                    end
                end,
                undefined,
                Members
            )
    end;
find_member_by_user_id(_, _) ->
    undefined.

-spec find_role_by_id(role_id(), list()) -> role() | undefined.
find_role_by_id(RoleId, Roles) ->
    TargetId = to_int(RoleId),
    lists:foldl(
        fun(Role, Acc) ->
            case Acc of
                undefined ->
                    case role_id(Role) =:= TargetId of
                        true -> Role;
                        false -> undefined
                    end;
                Found ->
                    Found
            end
        end,
        undefined,
        ensure_list(Roles)
    ).

-spec find_channel_by_id(channel_id(), guild_state()) -> channel() | undefined.
find_channel_by_id(ChannelId, State) when is_integer(ChannelId) ->
    case resolve_data_map(State) of
        undefined ->
            undefined;
        Data ->
            Channels = ensure_list(maps:get(<<"channels">>, Data, [])),
            lists:foldl(
                fun(Channel, Acc) ->
                    case Acc of
                        undefined ->
                            ChanId = to_int(maps:get(<<"id">>, Channel, <<"0">>)),
                            case ChanId =:= ChannelId of
                                true -> Channel;
                                false -> undefined
                            end;
                        Found ->
                            Found
                    end
                end,
                undefined,
                Channels
            )
    end;
find_channel_by_id(_, _) ->
    undefined.

-spec compute_member_permissions(user_id(), maybe_channel_id(), maybe_member(), guild_state()) ->
    permission().
compute_member_permissions(UserId, ChannelId, ProvidedMember, State) when is_integer(UserId) ->
    case resolve_data_map(State) of
        undefined ->
            0;
        Data ->
            OwnerId = guild_owner_id(Data),
            case UserId =:= OwnerId of
                true ->
                    ?ALL_PERMISSIONS;
                false ->
                    case resolve_member(UserId, ProvidedMember, State) of
                        undefined ->
                            0;
                        Member ->
                            GuildId = guild_id(State),
                            Roles = ensure_list(maps:get(<<"roles">>, Data, [])),
                            BasePermissions = base_role_permissions(GuildId, Roles),
                            MemberRoles = member_role_ids(Member),
                            Permissions = aggregate_role_permissions(
                                MemberRoles, Roles, BasePermissions
                            ),
                            case (Permissions band constants:administrator_permission()) =/= 0 of
                                true ->
                                    ?ALL_PERMISSIONS;
                                false ->
                                    maybe_apply_channel_overwrites(
                                        Permissions,
                                        UserId,
                                        MemberRoles,
                                        ChannelId,
                                        GuildId,
                                        State
                                    )
                            end
                    end
            end
    end;
compute_member_permissions(_, _, _, _) ->
    0.

-spec resolve_member(user_id(), maybe_member(), guild_state()) -> maybe_member().
resolve_member(_UserId, Member, _State) when is_map(Member) ->
    Member;
resolve_member(UserId, _Member, State) ->
    find_member_by_user_id(UserId, State).

-spec guild_owner_id(guild_data()) -> user_id().
guild_owner_id(Data) ->
    Guild = maps:get(<<"guild">>, Data, #{}),
    to_int(maps:get(<<"owner_id">>, Guild, <<"0">>)).

-spec guild_id(guild_state()) -> integer().
guild_id(State) ->
    case maps:get(id, State, undefined) of
        undefined ->
            to_int(maps:get(<<"id">>, State, 0));
        GuildId when is_integer(GuildId) ->
            GuildId;
        GuildId ->
            to_int(GuildId)
    end.

-spec base_role_permissions(role_id(), list()) -> permission().
base_role_permissions(GuildId, Roles) ->
    lists:foldl(
        fun(Role, Acc) ->
            case role_id(Role) =:= GuildId of
                true -> role_permissions(Role);
                false -> Acc
            end
        end,
        0,
        ensure_list(Roles)
    ).

-spec aggregate_role_permissions(member_roles(), list(), permission()) -> permission().
aggregate_role_permissions(MemberRoles, Roles, BasePermissions) ->
    lists:foldl(
        fun(RoleId, Acc) ->
            case find_role_by_id(RoleId, Roles) of
                undefined ->
                    Acc;
                Role ->
                    Acc bor role_permissions(Role)
            end
        end,
        BasePermissions,
        MemberRoles
    ).

-spec maybe_apply_channel_overwrites(
    permission(), user_id(), member_roles(), maybe_channel_id(), role_id(), guild_state()
) -> permission().
maybe_apply_channel_overwrites(Permissions, _UserId, _MemberRoles, undefined, _GuildId, _State) ->
    Permissions;
maybe_apply_channel_overwrites(Permissions, UserId, MemberRoles, ChannelId, GuildId, State) when
    is_integer(ChannelId)
->
    case find_channel_by_id(ChannelId, State) of
        undefined ->
            Permissions;
        Channel ->
            apply_channel_overwrites(Permissions, UserId, MemberRoles, Channel, GuildId)
    end;
maybe_apply_channel_overwrites(Permissions, _UserId, _MemberRoles, _ChannelId, _GuildId, _State) ->
    Permissions.

-spec member_role_ids(member()) -> member_roles().
member_role_ids(Member) ->
    RoleIds = maps:get(<<"roles">>, Member, []),
    extract_integer_list(RoleIds).

-spec role_permissions(role()) -> permission().
role_permissions(Role) ->
    to_int(maps:get(<<"permissions">>, Role, <<"0">>)).

-spec role_id(role()) -> role_id().
role_id(Role) ->
    to_int(maps:get(<<"id">>, Role, <<"0">>)).

-spec channel_overwrites(channel()) -> [overwrite()].
channel_overwrites(Channel) ->
    case maps:get(<<"permission_overwrites">>, Channel, []) of
        Overwrites when is_list(Overwrites) -> Overwrites;
        _ -> []
    end.

-spec apply_everyone_overwrites(permission(), [overwrite()], role_id()) -> permission().
apply_everyone_overwrites(BasePerms, Overwrites, EveryoneRoleId) ->
    lists:foldl(
        fun(Overwrite, Acc) ->
            case overwrite_matches_role(Overwrite, EveryoneRoleId) of
                true ->
                    apply_allow_deny(Acc, overwrite_allow(Overwrite), overwrite_deny(Overwrite));
                false ->
                    Acc
            end
        end,
        BasePerms,
        Overwrites
    ).

-spec accumulate_role_overwrites(member_roles(), [overwrite()]) -> {permission(), permission()}.
accumulate_role_overwrites(MemberRoles, Overwrites) ->
    lists:foldl(
        fun(RoleId, {AllowAcc, DenyAcc}) ->
            lists:foldl(
                fun(Overwrite, {A, D}) ->
                    case overwrite_matches_role(Overwrite, RoleId) of
                        true ->
                            {A bor overwrite_allow(Overwrite), D bor overwrite_deny(Overwrite)};
                        false ->
                            {A, D}
                    end
                end,
                {AllowAcc, DenyAcc},
                Overwrites
            )
        end,
        {0, 0},
        MemberRoles
    ).

-spec apply_user_overwrites(permission(), [overwrite()], user_id()) -> permission().
apply_user_overwrites(Perms, Overwrites, UserId) ->
    lists:foldl(
        fun(Overwrite, Acc) ->
            case overwrite_matches_user(Overwrite, UserId) of
                true ->
                    apply_allow_deny(Acc, overwrite_allow(Overwrite), overwrite_deny(Overwrite));
                false ->
                    Acc
            end
        end,
        Perms,
        Overwrites
    ).

-spec overwrite_matches_role(overwrite(), role_id()) -> boolean().
overwrite_matches_role(Overwrite, RoleId) when is_map(Overwrite), is_integer(RoleId) ->
    overwrite_type(Overwrite) =:= 0 andalso overwrite_id(Overwrite) =:= RoleId;
overwrite_matches_role(_, _) ->
    false.

-spec overwrite_matches_user(overwrite(), user_id()) -> boolean().
overwrite_matches_user(Overwrite, UserId) when is_map(Overwrite), is_integer(UserId) ->
    overwrite_type(Overwrite) =:= 1 andalso overwrite_id(Overwrite) =:= UserId;
overwrite_matches_user(_, _) ->
    false.

-spec overwrite_id(overwrite()) -> integer().
overwrite_id(Overwrite) ->
    to_int(maps:get(<<"id">>, Overwrite, <<"0">>)).

-spec overwrite_type(overwrite()) -> integer().
overwrite_type(Overwrite) ->
    maps:get(<<"type">>, Overwrite, 0).

-spec overwrite_allow(overwrite()) -> permission().
overwrite_allow(Overwrite) ->
    to_int(maps:get(<<"allow">>, Overwrite, <<"0">>)).

-spec overwrite_deny(overwrite()) -> permission().
overwrite_deny(Overwrite) ->
    to_int(maps:get(<<"deny">>, Overwrite, <<"0">>)).

-spec apply_allow_deny(permission(), permission(), permission()) -> permission().
apply_allow_deny(Acc, Allow, Deny) ->
    (Acc band bnot Deny) bor Allow.

-spec extract_integer_list(list()) -> [integer()].
extract_integer_list(List) when is_list(List) ->
    lists:reverse(
        lists:foldl(
            fun(Value, Acc) ->
                case type_conv:to_integer(Value) of
                    undefined -> Acc;
                    Int -> [Int | Acc]
                end
            end,
            [],
            List
        )
    );
extract_integer_list(_) ->
    [].

-spec ensure_list(term()) -> list().
ensure_list(List) when is_list(List) ->
    List;
ensure_list(_) ->
    [].

-spec to_int(term()) -> integer().
to_int(Value) ->
    case type_conv:to_integer(Value) of
        undefined -> 0;
        Int -> Int
    end.

-spec resolve_data_map(guild_state() | map()) -> guild_data() | undefined.
resolve_data_map(State) when is_map(State) ->
    case maps:find(data, State) of
        {ok, Data} when is_map(Data) ->
            Data;
        {ok, Data} when is_map(Data) =:= false ->
            Data;
        error ->
            case State of
                #{<<"members">> := _} ->
                    State;
                _ ->
                    undefined
            end
    end;
resolve_data_map(_) ->
    undefined.

-ifdef(TEST).

owner_receives_full_permissions_test() ->
    OwnerId = 1,
    GuildId = 100,
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => integer_to_binary(OwnerId)},
            <<"roles">> => [#{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>}]
        }
    },
    ?assertEqual(?ALL_PERMISSIONS, get_member_permissions(OwnerId, undefined, State)).

channel_scope_permissions_test() ->
    GuildId = 42,
    UserId = 600,
    ChannelId = 700,
    RoleId = 800,
    View = constants:view_channel_permission(),
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => integer_to_binary(GuildId + 1)},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => <<"0">>},
                #{<<"id">> => integer_to_binary(RoleId), <<"permissions">> => <<"0">>}
            ],
            <<"members">> => [
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                    <<"roles">> => [integer_to_binary(RoleId)]
                }
            ],
            <<"channels">> => [
                #{
                    <<"id">> => integer_to_binary(ChannelId),
                    <<"permission_overwrites">> => [
                        #{
                            <<"id">> => integer_to_binary(GuildId),
                            <<"type">> => 0,
                            <<"allow">> => <<"0">>,
                            <<"deny">> => <<"0">>
                        },
                        #{
                            <<"id">> => integer_to_binary(RoleId),
                            <<"type">> => 0,
                            <<"allow">> => integer_to_binary(View),
                            <<"deny">> => <<"0">>
                        }
                    ]
                }
            ]
        }
    },
    ?assertEqual(0, get_member_permissions(UserId, undefined, State)),
    ChannelPerms = get_member_permissions(UserId, ChannelId, State),
    ?assert((ChannelPerms band View) =/= 0).

apply_channel_overwrites_e2e_test() ->
    View = constants:view_channel_permission(),
    GuildId = 5,
    RoleId = 9,
    UserId = 11,
    Channel = #{
        <<"permission_overwrites">> => [
            #{
                <<"id">> => integer_to_binary(GuildId),
                <<"type">> => 0,
                <<"allow">> => <<"0">>,
                <<"deny">> => integer_to_binary(View)
            },
            #{
                <<"id">> => integer_to_binary(RoleId),
                <<"type">> => 0,
                <<"allow">> => integer_to_binary(View),
                <<"deny">> => <<"0">>
            },
            #{
                <<"id">> => integer_to_binary(UserId),
                <<"type">> => 1,
                <<"allow">> => <<"0">>,
                <<"deny">> => integer_to_binary(View)
            }
        ]
    },
    Base = View,
    Result = apply_channel_overwrites(Base, UserId, [RoleId], Channel, GuildId),
    ?assertEqual(0, Result).

administrator_role_grants_all_permissions_test() ->
    Admin = constants:administrator_permission(),
    GuildId = 100,
    UserId = 200,
    ChannelId = 300,
    OwnerId = 999,
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => integer_to_binary(OwnerId)},
            <<"roles">> => [
                #{<<"id">> => integer_to_binary(GuildId), <<"permissions">> => integer_to_binary(Admin)}
            ],
            <<"members">> => [
                #{
                    <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
                    <<"roles">> => []
                }
            ],
            <<"channels">> => [
                #{
                    <<"id">> => integer_to_binary(ChannelId),
                    <<"permission_overwrites">> => []
                }
            ]
        }
    },
    ?assertEqual(?ALL_PERMISSIONS, get_member_permissions(UserId, undefined, State)),
    ?assertEqual(?ALL_PERMISSIONS, get_member_permissions(UserId, ChannelId, State)),
    ?assert(can_view_channel(UserId, ChannelId, undefined, State)).

-endif.
