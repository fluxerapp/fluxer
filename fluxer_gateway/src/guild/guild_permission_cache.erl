%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_permission_cache).

-typing([eqwalizer]).

-export([
    put_state/1,
    delete/1,
    get_permissions/3,
    get_snapshot/1,
    get_role_members/2,
    has_member/2,
    strip_data/1,
    member_projection_changed/3
]).

-type guild_id() :: integer().
-type user_id() :: integer().
-type channel_id() :: integer().
-type role_id() :: integer().
-type guild_state() :: map().
-type guild_data() :: map().

-export_type([guild_id/0, user_id/0, channel_id/0, role_id/0, guild_state/0, guild_data/0]).

-define(TABLE, guild_permission_cache).
-define(STRIPPED_MEMBERS_MEMO, guild_permission_cache_stripped_members).

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

-spec put_normalized_data(guild_id(), guild_data()) -> ok.
put_normalized_data(GuildId, NormalizedData) when is_integer(GuildId), is_map(NormalizedData) ->
    ensure_table(),
    StrippedData = strip_data(NormalizedData),
    Snapshot = #{id => GuildId, data => StrippedData},
    RoleIndex = maps:get(<<"member_role_index">>, NormalizedData, #{}),
    MemberSource = maps:with([members_ets], StrippedData),
    true = ets:insert(?TABLE, [
        {GuildId, Snapshot},
        {role_index_key(GuildId), RoleIndex, MemberSource}
    ]),
    ok;
put_normalized_data(_, _) ->
    ok.

-spec role_index_key(guild_id()) -> {member_role_index, guild_id()}.
role_index_key(GuildId) ->
    {member_role_index, GuildId}.

-spec delete(guild_id()) -> ok.
delete(GuildId) when is_integer(GuildId) ->
    case ets:whereis(?TABLE) of
        undefined -> ok;
        _ -> safe_ets_delete([GuildId, role_index_key(GuildId)])
    end,
    ok;
delete(_) ->
    ok.

-spec safe_ets_delete([term()]) -> ok.
safe_ets_delete(Keys) ->
    try lists:foreach(fun(Key) -> true = ets:delete(?TABLE, Key) end, Keys) of
        _ -> ok
    catch
        error:badarg -> ok
    end.

-spec get_permissions(guild_id(), user_id(), channel_id() | undefined) ->
    {ok, integer()} | {error, not_found}.
get_permissions(GuildId, UserId, ChannelId) when is_integer(GuildId), is_integer(UserId) ->
    case get_snapshot(GuildId) of
        {ok, Snapshot} ->
            read_permissions(UserId, ChannelId, Snapshot);
        {error, not_found} ->
            {error, not_found}
    end;
get_permissions(_, _, _) ->
    {error, not_found}.

-spec has_member(guild_id(), user_id()) -> {ok, boolean()} | {error, not_found}.
has_member(GuildId, UserId) when is_integer(GuildId), is_integer(UserId) ->
    case get_snapshot(GuildId) of
        {ok, Snapshot} ->
            read_has_member(UserId, Snapshot);
        {error, not_found} ->
            {error, not_found}
    end;
has_member(_, _) ->
    {error, not_found}.

-spec read_permissions(user_id(), channel_id() | undefined, guild_state()) ->
    {ok, integer()} | {error, not_found}.
read_permissions(UserId, ChannelId, Snapshot) ->
    safe_member_read(fun() ->
        {ok, guild_permissions:get_member_permissions(UserId, ChannelId, Snapshot)}
    end).

-spec read_has_member(user_id(), guild_state()) -> {ok, boolean()} | {error, not_found}.
read_has_member(UserId, Snapshot) ->
    safe_member_read(fun() ->
        {ok, guild_permissions_check:find_member_by_user_id(UserId, Snapshot) =/= undefined}
    end).

-spec get_snapshot(guild_id()) -> {ok, guild_state()} | {error, not_found}.
get_snapshot(GuildId) when is_integer(GuildId) ->
    ensure_table(),
    case ets:lookup(?TABLE, GuildId) of
        [{GuildId, Snapshot}] ->
            snapshot_with_live_members(Snapshot);
        [] ->
            {error, not_found}
    end;
get_snapshot(_) ->
    {error, not_found}.

-spec get_role_members(guild_id(), role_id()) -> {ok, [user_id()]} | {error, not_found}.
get_role_members(GuildId, RoleId) when is_integer(GuildId), is_integer(RoleId) ->
    ensure_table(),
    case ets:lookup(?TABLE, role_index_key(GuildId)) of
        [{_Key, RoleIndex, MemberSource}] ->
            live_role_members(maps:get(RoleId, RoleIndex, #{}), MemberSource);
        [] ->
            {error, not_found}
    end;
get_role_members(_, _) ->
    {error, not_found}.

-spec live_role_members(map(), guild_data()) -> {ok, [user_id()]} | {error, not_found}.
live_role_members(RoleMembers, MemberSource) ->
    case live_members(data_member_table(MemberSource)) of
        true -> {ok, lists:sort(maps:keys(RoleMembers))};
        false -> {error, not_found}
    end.

-spec snapshot_with_live_members(guild_state()) -> {ok, guild_state()} | {error, not_found}.
snapshot_with_live_members(Snapshot) ->
    case live_members(snapshot_member_table(Snapshot)) of
        true -> {ok, Snapshot};
        false -> {error, not_found}
    end.

-spec live_members(ets:tid() | undefined) -> boolean().
live_members(undefined) ->
    true;
live_members(Tab) ->
    ets:info(Tab, owner) =/= undefined.

-spec snapshot_member_table(guild_state()) -> ets:tid() | undefined.
snapshot_member_table(#{data := Data}) when is_map(Data) ->
    data_member_table(Data);
snapshot_member_table(_Snapshot) ->
    undefined.

-spec safe_member_read(fun(() -> {ok, T})) -> {ok, T} | {error, not_found}.
safe_member_read(Read) ->
    try
        Read()
    catch
        error:badarg -> {error, not_found}
    end.

-spec ensure_table() -> ok.
ensure_table() ->
    guild_ets_utils:ensure_table(?TABLE, [named_table, public, set, {read_concurrency, true}]).

-spec strip_data(guild_data()) -> guild_data().
strip_data(Data) when is_map(Data) ->
    Guild = guild_permission_strip:guild(maps:get(<<"guild">>, Data, #{})),
    Roles = guild_permission_strip:roles(maps:get(<<"roles">>, Data, [])),
    Channels = guild_permission_strip:channels(maps:get(<<"channels">>, Data, [])),
    ChannelIndex = guild_permission_strip:channel_index(
        maps:get(<<"channel_index">>, Data, #{})
    ),
    RolePermsCache = maps:get(role_perms_cache, Data, #{}),
    OverwritePermsCache = maps:get(overwrite_perms_cache, Data, #{}),
    Base = maps:merge(maps:with([thread_gate, thread_store, thread_forum_categories], Data), #{
        <<"guild">> => Guild,
        <<"roles">> => Roles,
        <<"channels">> => Channels,
        <<"channel_index">> => ChannelIndex,
        role_perms_cache => RolePermsCache,
        overwrite_perms_cache => OverwritePermsCache
    }),
    with_member_source(Data, Base);
strip_data(Data) ->
    Data.

-spec with_member_source(guild_data(), guild_data()) -> guild_data().
with_member_source(Data, Base) ->
    case shared_member_table(Data) of
        {ok, Tab} ->
            _ = erlang:erase(?STRIPPED_MEMBERS_MEMO),
            Base#{<<"members">> => #{}, members_ets => Tab};
        none ->
            Base#{<<"members">> => memoised_strip_members(maps:get(<<"members">>, Data, #{}))}
    end.

-spec shared_member_table(guild_data()) -> {ok, ets:tid()} | none.
shared_member_table(Data) ->
    case {members_ets_enabled(), data_member_table(Data)} of
        {true, Tab} when is_reference(Tab) -> readable_member_table(Tab);
        _ -> none
    end.

-spec readable_member_table(ets:tid()) -> {ok, ets:tid()} | none.
readable_member_table(Tab) ->
    case {ets:info(Tab, type), ets:info(Tab, protection)} of
        {set, public} -> {ok, Tab};
        {set, protected} -> {ok, Tab};
        _ -> none
    end.

-spec data_member_table(guild_data()) -> ets:tid() | undefined.
data_member_table(#{members_ets := Tab}) ->
    Tab;
data_member_table(_Data) ->
    undefined.

-spec members_ets_enabled() -> boolean().
members_ets_enabled() ->
    case application:get_env(fluxer_gateway, permission_cache_members_ets, true) of
        false -> false;
        _ -> true
    end.

-spec member_projection_changed(user_id() | undefined, guild_data(), guild_data()) -> boolean().
member_projection_changed(UserId, OldData, NewData) when is_integer(UserId) ->
    strip_member(guild_data_index_members:get_member(UserId, OldData)) =/=
        strip_member(guild_data_index_members:get_member(UserId, NewData));
member_projection_changed(_UserId, _OldData, _NewData) ->
    true.

%% Keyed on the IDENTITY of the source term, never on its value: a hit means this process
%% already stripped that exact term, and strip_members/1 is pure, so the stored result is
%% that term's result. Every write to a member map yields a new term and so cannot hit, and
%% only a completed strip is stored, so a term whose strip raises still raises.
-spec memoised_strip_members(term()) -> map().
memoised_strip_members(Members) ->
    case strip_members_memo_enabled() of
        true -> strip_members_memoised(Members);
        false -> strip_members_unmemoised(Members)
    end.

-spec strip_members_unmemoised(term()) -> map().
strip_members_unmemoised(Members) ->
    _ = erlang:erase(?STRIPPED_MEMBERS_MEMO),
    strip_members(Members).

-spec strip_members_memoised(term()) -> map().
strip_members_memoised(Members) ->
    case erlang:get(?STRIPPED_MEMBERS_MEMO) of
        {Source, Stripped} when is_map(Stripped) ->
            reuse_stripped_members(Members, Source, Stripped);
        _ ->
            store_stripped_members(Members)
    end.

-spec reuse_stripped_members(term(), term(), map()) -> map().
reuse_stripped_members(Members, Source, Stripped) ->
    case erts_debug:same(Members, Source) of
        true -> Stripped;
        false -> store_stripped_members(Members)
    end.

%% Dropped before the new one is built so a member map that shares nothing with the
%% previous one, a wholesale rebuild, is never held twice on the guild heap at once.
-spec store_stripped_members(term()) -> map().
store_stripped_members(Members) ->
    _ = erlang:erase(?STRIPPED_MEMBERS_MEMO),
    Stripped = strip_members(Members),
    _ = erlang:put(?STRIPPED_MEMBERS_MEMO, {Members, Stripped}),
    Stripped.

-spec strip_members_memo_enabled() -> boolean().
strip_members_memo_enabled() ->
    case application:get_env(fluxer_gateway, permission_cache_strip_memo_enabled, true) of
        false -> false;
        _ -> true
    end.

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
    Roles = strip_role_ids(maps:get(<<"roles">>, Member, [])),
    Base = #{
        <<"user">> => StrippedUser,
        <<"roles">> => Roles
    },
    copy_if_present(<<"communication_disabled_until">>, Member, Base);
strip_member(_) ->
    #{}.

%% snowflake_id:parse/1 returns a positive integer unchanged, so a roles list that already
%% holds only those is its own parse_list/1 result and does not need to be rebuilt.
-spec strip_role_ids(term()) -> list().
strip_role_ids(Roles) when is_list(Roles) ->
    case all_role_ids_parsed(Roles) of
        true -> Roles;
        false -> snowflake_id:parse_list(Roles)
    end;
strip_role_ids(Roles) ->
    snowflake_id:parse_list(Roles).

-spec all_role_ids_parsed(term()) -> boolean().
all_role_ids_parsed([Id | Rest]) when is_integer(Id), Id > 0 -> all_role_ids_parsed(Rest);
all_role_ids_parsed([]) -> true;
all_role_ids_parsed(_) -> false.

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

-spec get_member_user_id(map()) -> integer() | undefined.
get_member_user_id(Member) when is_map(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    snowflake_id:parse_maybe(maps:get(<<"id">>, User, undefined)).
