%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_permission_cache).
-typing([eqwalizer]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([
    put_state/1,
    put_data/2,
    put_normalized_data/2,
    delete/1,
    get_permissions/3,
    get_snapshot/1,
    has_member/2,
    get_member/2,
    strip_data/1,
    member_projection_changed/3,
    migrate_existing_entries/0
]).

-type guild_id() :: integer().
-type user_id() :: integer().
-type channel_id() :: integer().
-type guild_state() :: map().
-type guild_data() :: map().

-export_type([guild_id/0, user_id/0, channel_id/0, guild_state/0, guild_data/0]).

-define(TABLE, guild_permission_cache).

-spec put_state(guild_state()) -> ok.
put_state(State) when is_map(State) ->
    GuildId = maps:get(id, State, undefined),
    Data = maps:get(data, State, #{}),
    case is_integer(GuildId) of
        true ->
            put_normalized_data(GuildId, Data);
        false ->
            ok
    end;
put_state(_) ->
    ok.

-spec put_data(guild_id(), guild_data()) -> ok.
put_data(GuildId, Data) when is_integer(GuildId), is_map(Data) ->
    NormalizedData = guild_data_index:normalize_map(Data),
    put_normalized_data(GuildId, NormalizedData);
put_data(_, _) ->
    ok.

-spec put_normalized_data(guild_id(), guild_data()) -> ok.
put_normalized_data(GuildId, NormalizedData) when is_integer(GuildId), is_map(NormalizedData) ->
    ensure_table(),
    StrippedData = strip_data(NormalizedData),
    Snapshot = #{id => GuildId, data => StrippedData},
    true = ets:insert(?TABLE, {GuildId, Snapshot}),
    ok;
put_normalized_data(_, _) ->
    ok.

-spec delete(guild_id()) -> ok.
delete(GuildId) when is_integer(GuildId) ->
    case ets:whereis(?TABLE) of
        undefined -> ok;
        _ -> safe_ets_delete(GuildId)
    end,
    ok;
delete(_) ->
    ok.

-spec safe_ets_delete(guild_id()) -> ok.
safe_ets_delete(GuildId) ->
    try ets:delete(?TABLE, GuildId) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec get_permissions(guild_id(), user_id(), channel_id() | undefined) ->
    {ok, integer()} | {error, not_found}.
get_permissions(GuildId, UserId, ChannelId) when is_integer(GuildId), is_integer(UserId) ->
    case get_snapshot(GuildId) of
        {ok, Snapshot} ->
            Permissions = guild_permissions:get_member_permissions(UserId, ChannelId, Snapshot),
            {ok, Permissions};
        {error, not_found} ->
            {error, not_found}
    end;
get_permissions(_, _, _) ->
    {error, not_found}.

-spec has_member(guild_id(), user_id()) -> {ok, boolean()} | {error, not_found}.
has_member(GuildId, UserId) when is_integer(GuildId), is_integer(UserId) ->
    case get_snapshot(GuildId) of
        {ok, Snapshot} ->
            Member = guild_permissions:find_member_by_user_id(UserId, Snapshot),
            {ok, Member =/= undefined};
        {error, not_found} ->
            {error, not_found}
    end;
has_member(_, _) ->
    {error, not_found}.

-spec get_member(guild_id(), user_id()) -> {ok, map() | undefined} | {error, not_found}.
get_member(GuildId, UserId) when is_integer(GuildId), is_integer(UserId) ->
    case get_snapshot(GuildId) of
        {ok, Snapshot} ->
            {ok, guild_permissions:find_member_by_user_id(UserId, Snapshot)};
        {error, not_found} ->
            {error, not_found}
    end;
get_member(_, _) ->
    {error, not_found}.

-spec get_snapshot(guild_id()) -> {ok, guild_state()} | {error, not_found}.
get_snapshot(GuildId) when is_integer(GuildId) ->
    ensure_table(),
    case ets:lookup(?TABLE, GuildId) of
        [{GuildId, Snapshot}] ->
            {ok, Snapshot};
        [] ->
            {error, not_found}
    end;
get_snapshot(_) ->
    {error, not_found}.

-spec ensure_table() -> ok.
ensure_table() ->
    guild_ets_utils:ensure_table(?TABLE, [named_table, public, set, {read_concurrency, true}]).

-spec strip_data(guild_data()) -> guild_data().
strip_data(Data) when is_map(Data) ->
    Guild = strip_guild(maps:get(<<"guild">>, Data, #{})),
    Members = strip_members(maps:get(<<"members">>, Data, #{})),
    Roles = strip_roles(maps:get(<<"roles">>, Data, [])),
    Channels = strip_channels(maps:get(<<"channels">>, Data, [])),
    ChannelIndex = strip_channel_index(maps:get(<<"channel_index">>, Data, #{})),
    MemberRoleIndex = maps:get(<<"member_role_index">>, Data, #{}),
    RolePermsCache = maps:get(role_perms_cache, Data, #{}),
    OverwritePermsCache = maps:get(overwrite_perms_cache, Data, #{}),
    #{
        <<"guild">> => Guild,
        <<"members">> => Members,
        <<"roles">> => Roles,
        <<"channels">> => Channels,
        <<"channel_index">> => ChannelIndex,
        <<"member_role_index">> => MemberRoleIndex,
        role_perms_cache => RolePermsCache,
        overwrite_perms_cache => OverwritePermsCache
    };
strip_data(Data) ->
    Data.

-spec member_projection_changed(user_id() | undefined, guild_data(), guild_data()) -> boolean().
member_projection_changed(UserId, OldData, NewData) when is_integer(UserId) ->
    strip_member(guild_data_index:get_member(UserId, OldData)) =/=
        strip_member(guild_data_index:get_member(UserId, NewData));
member_projection_changed(_UserId, _OldData, _NewData) ->
    true.

-spec strip_guild(map() | term()) -> map().
strip_guild(Guild) when is_map(Guild) ->
    case snowflake_id:parse_optional(maps:get(<<"owner_id">>, Guild, undefined)) of
        undefined -> #{};
        OwnerId -> #{<<"owner_id">> => OwnerId}
    end;
strip_guild(_) ->
    #{}.

-spec strip_members(map() | list() | term()) -> map().
strip_members(Members) when is_map(Members) ->
    maps:map(fun(_UserId, Member) -> strip_member(Member) end, Members);
strip_members(Members) when is_list(Members) ->
    lists:foldl(fun strip_member_entry/2, #{}, Members);
strip_members(_) ->
    #{}.

-spec strip_member_entry(term(), map()) -> map().
strip_member_entry(Member, Acc) when is_map(Member) ->
    case get_member_user_id(Member) of
        undefined -> Acc;
        UserId -> Acc#{UserId => strip_member(Member)}
    end;
strip_member_entry(_, Acc) ->
    Acc.

-spec strip_member(map() | term()) -> map().
strip_member(Member) when is_map(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    StrippedUser = strip_user(User),
    Roles = snowflake_id:parse_list(maps:get(<<"roles">>, Member, [])),
    Base = #{
        <<"user">> => StrippedUser,
        <<"roles">> => Roles
    },
    copy_optional_member_fields([<<"communication_disabled_until">>], Member, Base);
strip_member(_) ->
    #{}.

-spec copy_optional_member_fields([binary()], map(), map()) -> map().
copy_optional_member_fields(Keys, Source, Acc) ->
    lists:foldl(
        fun(Key, Current) -> copy_if_present(Key, Source, Current) end,
        Acc,
        Keys
    ).

-spec copy_if_present(binary(), map(), map()) -> map().
copy_if_present(Key, Source, Current) ->
    case maps:find(Key, Source) of
        {ok, Value} -> Current#{Key => Value};
        error -> Current
    end.

-spec strip_user(map() | term()) -> map().
strip_user(User) when is_map(User) ->
    case snowflake_id:parse_optional(maps:get(<<"id">>, User, undefined)) of
        undefined -> #{};
        Id -> #{<<"id">> => Id}
    end;
strip_user(_) ->
    #{}.

-spec strip_roles(list() | term()) -> list().
strip_roles(Roles) when is_list(Roles) ->
    [strip_role(Role) || Role <- Roles, is_map(Role)];
strip_roles(_) ->
    [].

-spec strip_role(map()) -> map().
strip_role(Role) ->
    Keep = [<<"id">>, <<"permissions">>, <<"position">>],
    maps:with(Keep, normalize_role(Role)).

-spec strip_channels(list() | term()) -> list().
strip_channels(Channels) when is_list(Channels) ->
    [strip_channel(Channel) || Channel <- Channels, is_map(Channel)];
strip_channels(_) ->
    [].

-spec strip_channel(map()) -> map().
strip_channel(Channel) ->
    Keep = [<<"id">>, <<"name">>, <<"type">>, <<"parent_id">>, <<"permission_overwrites">>],
    maps:with(Keep, normalize_channel(Channel)).

-spec strip_channel_index(map() | term()) -> map().
strip_channel_index(ChannelIndex) when is_map(ChannelIndex) ->
    maps:map(
        fun(_Id, Channel) when is_map(Channel) -> strip_channel(Channel) end, ChannelIndex
    );
strip_channel_index(_) ->
    #{}.

-spec normalize_role(map()) -> map().
normalize_role(Role) ->
    case guild_data_normalize:role(Role) of
        Normalized when is_map(Normalized) -> Normalized;
        _ -> Role
    end.

-spec normalize_channel(map()) -> map().
normalize_channel(Channel) ->
    case guild_data_normalize:channel(Channel) of
        Normalized when is_map(Normalized) -> Normalized;
        _ -> Channel
    end.

-spec get_member_user_id(map()) -> integer() | undefined.
get_member_user_id(Member) when is_map(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    parse_user_id(maps:get(<<"id">>, User, undefined)).

-spec parse_user_id(term()) -> integer() | undefined.
parse_user_id(Id) ->
    snowflake_id:parse_optional(Id).

-spec migrate_existing_entries() -> {ok, non_neg_integer()}.
migrate_existing_entries() ->
    ensure_table(),
    Count = ets:foldl(
        fun
            ({GuildId, #{data := Data} = _Snapshot}, Acc) ->
                Stripped = strip_data(Data),
                NewSnapshot = #{id => GuildId, data => Stripped},
                true = ets:insert(?TABLE, {GuildId, NewSnapshot}),
                Acc + 1;
            (_, Acc) ->
                Acc
        end,
        0,
        ?TABLE
    ),
    {ok, Count}.

-ifdef(TEST).

strip_member_preserves_communication_disabled_until_test() ->
    GuildId = 901,
    UserId = 902,
    TimeoutUntil = <<"2026-05-09T22:00:00.000Z">>,
    Data = #{
        <<"guild">> => #{<<"owner_id">> => <<"1">>},
        <<"roles">> => [],
        <<"members">> => #{
            UserId => #{
                <<"user">> => #{
                    <<"id">> => integer_to_binary(UserId),
                    <<"username">> => <<"ignored">>
                },
                <<"roles">> => [<<"42">>],
                <<"communication_disabled_until">> => TimeoutUntil,
                <<"nick">> => <<"not needed for permission cache">>
            }
        },
        <<"channels">> => []
    },
    ok = put_data(GuildId, Data),
    try
        {ok, #{} = MemberData} = get_member(GuildId, UserId),
        ?assertEqual(TimeoutUntil, maps:get(<<"communication_disabled_until">>, MemberData)),
        ?assertEqual([42], maps:get(<<"roles">>, MemberData)),
        ?assertEqual(false, maps:is_key(<<"nick">>, MemberData))
    after
        ok = delete(GuildId)
    end.

-endif.
