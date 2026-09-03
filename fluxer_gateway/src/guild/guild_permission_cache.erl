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
    Members = memoised_strip_members(maps:get(<<"members">>, Data, #{})),
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

strip_member_matches_reference_implementation_test() ->
    lists:foreach(
        fun(Member) ->
            ?assertEqual(reference_strip_member(Member), strip_member(Member))
        end,
        strip_member_cases()
    ).

strip_member_cases() ->
    Until = <<"2026-05-09T22:00:00.000Z">>,
    User = #{<<"id">> => 5, <<"username">> => <<"dropped">>},
    [
        #{<<"user">> => User, <<"roles">> => [7, 9]},
        #{<<"user">> => User, <<"roles">> => [<<"7">>, 9]},
        #{<<"user">> => User, <<"roles">> => [<<"7">>, <<"9">>]},
        #{<<"user">> => User, <<"roles">> => []},
        #{<<"user">> => User},
        #{<<"roles">> => [7]},
        #{<<"user">> => User, <<"roles">> => [7], <<"communication_disabled_until">> => Until},
        #{<<"user">> => User, <<"roles">> => [7], <<"nick">> => <<"dropped">>},
        #{<<"user">> => #{}, <<"roles">> => undefined},
        #{<<"user">> => not_a_map, <<"roles">> => null},
        not_a_member
    ].

reference_strip_member(Member) when is_map(Member) ->
    StrippedUser = strip_user(maps:get(<<"user">>, Member, #{})),
    Roles = snowflake_id:parse_list(maps:get(<<"roles">>, Member, [])),
    Base = #{<<"user">> => StrippedUser, <<"roles">> => Roles},
    lists:foldl(
        fun(Key, Current) -> copy_if_present(Key, Member, Current) end,
        Base,
        [<<"communication_disabled_until">>]
    );
reference_strip_member(_) ->
    #{}.

strip_member_rejects_unparsed_role_ids_test() ->
    ?assertError({invalid_snowflake, 0}, strip_member(#{<<"roles">> => [0]})),
    ?assertError({invalid_snowflake, -3}, strip_member(#{<<"roles">> => [-3]})),
    ?assertError({invalid_snowflake, <<"0">>}, strip_member(#{<<"roles">> => [<<"0">>]})),
    ?assertError({invalid_snowflake, bad}, strip_member(#{<<"roles">> => [7, bad]})),
    ?assertError({invalid_snowflake_list, oops}, strip_member(#{<<"roles">> => oops})).

strip_members_keeps_already_parsed_role_ids_test() ->
    GuildId = 905,
    UserId = 906,
    Member = #{<<"user">> => #{<<"id">> => UserId}, <<"roles">> => [42, 43]},
    Data = #{
        <<"guild">> => #{<<"owner_id">> => <<"1">>},
        <<"roles">> => [],
        <<"members">> => #{UserId => Member},
        <<"channels">> => []
    },
    ok = put_data(GuildId, Data),
    try
        {ok, #{} = MemberData} = get_member(GuildId, UserId),
        ?assertEqual([42, 43], maps:get(<<"roles">>, MemberData)),
        ?assertEqual(#{<<"id">> => UserId}, maps:get(<<"user">>, MemberData))
    after
        ok = delete(GuildId)
    end.

memo_reset() ->
    erlang:erase(?STRIPPED_MEMBERS_MEMO).

memo_member(Id, Roles) ->
    #{
        <<"user">> => #{<<"id">> => integer_to_binary(Id), <<"username">> => <<"dropped">>},
        <<"roles">> => Roles,
        <<"nick">> => <<"dropped">>
    }.

memo_members(Ids) ->
    maps:from_list([{Id, memo_member(Id, [<<"42">>])} || Id <- Ids]).

%% strip_members/1 as it read before the memo, over the pre-shortcut strip_member/1.
reference_strip_members(Members) when is_map(Members) ->
    maps:map(fun(_UserId, Member) -> reference_strip_member(Member) end, Members);
reference_strip_members(Members) when is_list(Members) ->
    lists:foldl(fun reference_strip_member_entry/2, #{}, Members);
reference_strip_members(_) ->
    #{}.

reference_strip_member_entry(Member, Acc) when is_map(Member) ->
    case get_member_user_id(Member) of
        undefined -> Acc;
        UserId -> Acc#{UserId => reference_strip_member(Member)}
    end;
reference_strip_member_entry(_, Acc) ->
    Acc.

assert_memo_matches_reference(Members) ->
    Expected = reference_strip_members(Members),
    ?assertEqual(Expected, memoised_strip_members(Members)),
    ?assertEqual(Expected, memoised_strip_members(Members)).

memo_members_cases() ->
    [
        #{},
        memo_members([1]),
        memo_members([1, 2, 3]),
        #{7 => memo_member(7, [])},
        [memo_member(4, [<<"42">>]), memo_member(5, [7, 9])],
        [memo_member(6, []), not_a_member],
        [],
        not_a_member_map,
        undefined
    ].

memoised_strip_members_matches_reference_test() ->
    memo_reset(),
    try
        lists:foreach(fun assert_memo_matches_reference/1, memo_members_cases())
    after
        memo_reset()
    end.

memo_reuses_the_stripped_term_for_an_identical_members_term_test() ->
    memo_reset(),
    try
        Members = memo_members([1, 2]),
        First = memoised_strip_members(Members),
        Second = memoised_strip_members(Members),
        ?assertEqual(reference_strip_members(Members), First),
        ?assert(erts_debug:same(First, Second)),
        ?assertEqual({Members, First}, erlang:get(?STRIPPED_MEMBERS_MEMO))
    after
        memo_reset()
    end.

memo_recomputes_for_an_equal_but_distinct_members_term_test() ->
    memo_reset(),
    try
        Members = memo_members([1, 2]),
        Copy = binary_to_term(term_to_binary(Members)),
        Stripped = memoised_strip_members(Members),
        ?assertNot(erts_debug:same(Members, Copy)),
        ?assertEqual(Stripped, memoised_strip_members(Copy)),
        {Source, _Stripped} = erlang:get(?STRIPPED_MEMBERS_MEMO),
        ?assert(erts_debug:same(Copy, Source))
    after
        memo_reset()
    end.

memo_changed_member_maps(Members) ->
    [
        Members#{2 => memo_member(2, [<<"7">>])},
        Members#{3 => memo_member(3, [<<"42">>])},
        maps:remove(1, Members),
        maps:map(fun(Id, _Member) -> memo_member(Id, []) end, Members)
    ].

memo_follows_every_change_to_the_member_map_test() ->
    memo_reset(),
    try
        Members = memo_members([1, 2]),
        _Stripped = memoised_strip_members(Members),
        lists:foreach(
            fun assert_memo_matches_reference/1,
            memo_changed_member_maps(Members)
        )
    after
        memo_reset()
    end.

memo_never_suppresses_an_invalid_role_id_test() ->
    memo_reset(),
    try
        Good = memo_members([1]),
        Stripped = memoised_strip_members(Good),
        Bad = #{2 => memo_member(2, [bad])},
        ?assertError({invalid_snowflake, bad}, memoised_strip_members(Bad)),
        ?assertEqual(undefined, erlang:get(?STRIPPED_MEMBERS_MEMO)),
        ?assertError({invalid_snowflake, bad}, memoised_strip_members(Bad)),
        ?assertEqual(Stripped, memoised_strip_members(Good)),
        ?assertEqual({Good, Stripped}, erlang:get(?STRIPPED_MEMBERS_MEMO))
    after
        memo_reset()
    end.

memo_ignores_an_unrecognised_entry_test() ->
    memo_reset(),
    try
        Members = memo_members([1]),
        Expected = reference_strip_members(Members),
        erlang:put(?STRIPPED_MEMBERS_MEMO, {Members, junk}),
        ?assertEqual(Expected, memoised_strip_members(Members)),
        erlang:put(?STRIPPED_MEMBERS_MEMO, stale_shape),
        ?assertEqual(Expected, memoised_strip_members(Members)),
        ?assertEqual({Members, Expected}, erlang:get(?STRIPPED_MEMBERS_MEMO))
    after
        memo_reset()
    end.

memo_disabled_matches_reference_and_releases_the_memo_test() ->
    memo_reset(),
    Members = memo_members([1, 2]),
    Expected = reference_strip_members(Members),
    ?assertEqual(Expected, memoised_strip_members(Members)),
    application:set_env(fluxer_gateway, permission_cache_strip_memo_enabled, false),
    try
        ?assertEqual(Expected, memoised_strip_members(Members)),
        ?assertEqual(undefined, erlang:get(?STRIPPED_MEMBERS_MEMO))
    after
        application:unset_env(fluxer_gateway, permission_cache_strip_memo_enabled),
        memo_reset()
    end.

%% strip_data/1 with the memo off is the computation this module ran before the memo.
unmemoised_strip_data(Data) ->
    application:set_env(fluxer_gateway, permission_cache_strip_memo_enabled, false),
    try
        strip_data(Data)
    after
        application:unset_env(fluxer_gateway, permission_cache_strip_memo_enabled)
    end.

memo_test_data(OwnerId) ->
    guild_data_index:normalize_data(#{
        <<"guild">> => #{<<"owner_id">> => OwnerId},
        <<"roles">> => [#{<<"id">> => <<"42">>, <<"permissions">> => <<"0">>}],
        <<"members">> => [memo_member(1, [<<"42">>]), memo_member(2, [])],
        <<"channels">> => [#{<<"id">> => <<"500">>, <<"permission_overwrites">> => []}]
    }).

strip_data_reuses_the_stripped_members_across_a_non_member_change_test() ->
    memo_reset(),
    Data = memo_test_data(<<"1">>),
    Updated = Data#{<<"guild">> => #{<<"owner_id">> => <<"9">>}},
    Expected = unmemoised_strip_data(Data),
    ExpectedUpdated = unmemoised_strip_data(Updated),
    memo_reset(),
    try
        ?assertEqual(Expected, strip_data(Data)),
        ?assertEqual(ExpectedUpdated, strip_data(Updated)),
        MembersA = maps:get(<<"members">>, strip_data(Data)),
        MembersB = maps:get(<<"members">>, strip_data(Updated)),
        ?assert(erts_debug:same(MembersA, MembersB))
    after
        memo_reset()
    end.

put_normalized_data_writes_the_same_snapshot_with_a_warm_memo_test() ->
    GuildId = 907,
    memo_reset(),
    Data = memo_test_data(<<"1">>),
    try
        ok = put_normalized_data(GuildId, Data),
        {ok, First} = get_snapshot(GuildId),
        ok = put_normalized_data(GuildId, Data),
        {ok, Second} = get_snapshot(GuildId),
        ?assertEqual(First, Second),
        ?assertEqual(#{id => GuildId, data => unmemoised_strip_data(Data)}, Second)
    after
        memo_reset(),
        ok = delete(GuildId)
    end.

-endif.
