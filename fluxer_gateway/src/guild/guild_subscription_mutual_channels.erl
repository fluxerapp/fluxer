%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_subscription_mutual_channels).
-typing([eqwalizer]).

-export([filter_member_ids/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type guild_state() :: map().
-type user_id() :: integer().
-type memo() :: #{exceptions := sets:set(user_id()), cache := #{term() => boolean()}}.
-export_type([guild_state/0, user_id/0]).

-spec filter_member_ids(user_id(), [user_id()], guild_state()) -> [user_id()].
filter_member_ids(_SessionUserId, [], _State) ->
    [];
filter_member_ids(SessionUserId, MemberIds, State) ->
    SessionMap = session_channel_map(SessionUserId, State),
    {Kept, _Memo} = lists:foldl(
        fun(MemberId, Acc) ->
            keep_member_with_mutual_channel(MemberId, SessionUserId, SessionMap, State, Acc)
        end,
        {[], new_memo(State)},
        MemberIds
    ),
    lists:reverse(Kept).

-spec session_channel_map(user_id(), guild_state()) -> map().
session_channel_map(SessionUserId, State) ->
    case guild_visibility_channels:get_cached_viewable_channel_map(SessionUserId, State) of
        undefined ->
            guild_sessions:build_viewable_channel_map(
                guild_visibility:get_user_viewable_channels(SessionUserId, State)
            );
        Map ->
            Map
    end.

-spec keep_member_with_mutual_channel(
    term(), user_id(), map(), guild_state(), {[user_id()], memo()}
) -> {[user_id()], memo()}.
keep_member_with_mutual_channel(MemberId, SessionUserId, _SessionMap, _State, Acc) when
    MemberId =:= SessionUserId; not is_integer(MemberId)
->
    Acc;
keep_member_with_mutual_channel(MemberId, _SessionUserId, SessionMap, State, {Kept, Memo}) ->
    case memoised_has_mutual_channel(MemberId, SessionMap, State, Memo) of
        {true, Memo1} -> {[MemberId | Kept], Memo1};
        {false, Memo1} -> {Kept, Memo1}
    end.

%% get_user_viewable_channels/2 reads nothing from a member except its raw roles
%% term, so two members holding the same term see the same channels and give the same
%% answer against a fixed session map. The user-specific inputs are the guild owner,
%% virtual channel access and user-type permission overwrites, and every user named
%% by one of those bypasses the memo.
-spec new_memo(guild_state()) -> memo().
new_memo(State) ->
    #{exceptions => exceptions(State), cache => #{}}.

-spec memoised_has_mutual_channel(user_id(), map(), guild_state(), memo()) ->
    {boolean(), memo()}.
memoised_has_mutual_channel(MemberId, SessionMap, State, Memo) ->
    #{exceptions := Exceptions, cache := Cache} = Memo,
    case memo_key(MemberId, Exceptions, State) of
        bypass ->
            {has_mutual_channel(MemberId, SessionMap, State), Memo};
        {ok, RawRoles} ->
            cached_has_mutual_channel(MemberId, RawRoles, SessionMap, State, Cache, Memo)
    end.

-spec cached_has_mutual_channel(
    user_id(), term(), map(), guild_state(), #{term() => boolean()}, memo()
) -> {boolean(), memo()}.
cached_has_mutual_channel(MemberId, RawRoles, SessionMap, State, Cache, Memo) ->
    case maps:find(RawRoles, Cache) of
        {ok, HasMutual} ->
            {HasMutual, Memo};
        error ->
            HasMutual = has_mutual_channel(MemberId, SessionMap, State),
            {HasMutual, Memo#{cache := Cache#{RawRoles => HasMutual}}}
    end.

-spec memo_key(user_id(), sets:set(user_id()), guild_state()) -> {ok, term()} | bypass.
memo_key(MemberId, Exceptions, State) ->
    case sets:is_element(MemberId, Exceptions) of
        true -> bypass;
        false -> member_roles_key(guild_permissions:find_member_by_user_id(MemberId, State))
    end.

-spec member_roles_key(map() | undefined) -> {ok, term()} | bypass.
member_roles_key(Member) when is_map(Member) ->
    {ok, maps:get(<<"roles">>, Member, [])};
member_roles_key(_Member) ->
    bypass.

-spec exceptions(guild_state()) -> sets:set(user_id()).
exceptions(State) ->
    Data = map_utils:ensure_map(map_utils:get_safe(State, data, #{})),
    Guild = map_utils:ensure_map(maps:get(<<"guild">>, Data, #{})),
    Owner = owner_ids(maps:get(<<"owner_id">>, Guild, undefined)),
    Virtual = maps:keys(map_utils:ensure_map(maps:get(virtual_channel_access, State, #{}))),
    sets:from_list(Owner ++ Virtual ++ overwrite_target_ids(Data)).

%% parse_maybe/1, not parse_optional/1: an id this rejects can never match a user in
%% a permission check either, so leaving it out of the exception set keeps the memo
%% exact while keeping the set itself total.
-spec owner_ids(term()) -> [user_id()].
owner_ids(OwnerIdRaw) ->
    case snowflake_id:parse_maybe(OwnerIdRaw) of
        OwnerId when is_integer(OwnerId) -> [OwnerId];
        _ -> []
    end.

%% Every overwrite target on every channel, read from both places a permission check
%% can take them from, and role targets alongside user targets. Role ids can never
%% collide with user ids, so the superset only costs a few members their memo entry.
-spec overwrite_target_ids(map()) -> [user_id()].
overwrite_target_ids(Data) ->
    Cached = maps:fold(
        fun(_ChannelId, Entries, Acc) -> cached_target_ids(Entries, Acc) end,
        [],
        map_utils:ensure_map(maps:get(overwrite_perms_cache, Data, #{}))
    ),
    maps:fold(
        fun(_ChannelId, Channel, Acc) -> channel_target_ids(Channel, Acc) end,
        Cached,
        guild_data_index:channel_index(Data)
    ).

-spec cached_target_ids(term(), [user_id()]) -> [user_id()].
cached_target_ids(Entries, Acc) when is_list(Entries) ->
    [Id || {Id, _Type, _Allow, _Deny} <- Entries, is_integer(Id)] ++ Acc;
cached_target_ids(_Entries, Acc) ->
    Acc.

-spec channel_target_ids(term(), [user_id()]) -> [user_id()].
channel_target_ids(Channel, Acc) when is_map(Channel) ->
    lists:foldl(
        fun overwrite_target_id/2,
        Acc,
        map_utils:ensure_list(maps:get(<<"permission_overwrites">>, Channel, []))
    );
channel_target_ids(_Channel, Acc) ->
    Acc.

-spec overwrite_target_id(term(), [user_id()]) -> [user_id()].
overwrite_target_id(Overwrite, Acc) when is_map(Overwrite) ->
    case snowflake_id:parse_maybe(maps:get(<<"id">>, Overwrite, undefined)) of
        Id when is_integer(Id) -> [Id | Acc];
        _ -> Acc
    end;
overwrite_target_id(_Overwrite, Acc) ->
    Acc.

-spec has_mutual_channel(user_id(), map(), guild_state()) -> boolean().
has_mutual_channel(MemberId, SessionMap, State) ->
    MemberChannels = guild_visibility:get_user_viewable_channels(MemberId, State),
    has_shared_channel(MemberChannels, SessionMap).

-spec has_shared_channel([integer()], map()) -> boolean().
has_shared_channel(MemberChannels, SessionMap) ->
    lists:any(fun(Ch) -> maps:is_key(Ch, SessionMap) end, MemberChannels).

-ifdef(TEST).

-define(GUILD_ID, 42).
-define(VIEWER_ROLE, 1000).
-define(OTHER_ROLE, 2000).

test_role(RoleId) ->
    #{<<"id">> => integer_to_binary(RoleId), <<"permissions">> => <<"0">>}.

test_member(UserId, RoleIds) ->
    #{
        <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
        <<"roles">> => [integer_to_binary(RoleId) || RoleId <- RoleIds]
    }.

test_overwrite(TargetId, Type) ->
    #{
        <<"id">> => integer_to_binary(TargetId),
        <<"type">> => Type,
        <<"allow">> => integer_to_binary(constants:view_channel_permission()),
        <<"deny">> => <<"0">>
    }.

test_channel(ChannelId, Overwrites) ->
    #{
        <<"id">> => integer_to_binary(ChannelId),
        <<"type">> => 0,
        <<"permission_overwrites">> => Overwrites
    }.

%% Session user 10 and members 20 and 21 reach channel 500 through the viewer role.
%% Members 30, 31, 99 and 4242 all carry the same roles term and reach only channel
%% 600 by role, but 99 holds virtual access to 500 and 4242 is a user-overwrite
%% target on 500, so both must still come out true. Member 7 owns the guild.
test_state() ->
    Channels = [
        test_channel(500, [test_overwrite(?VIEWER_ROLE, 0), test_overwrite(4242, 1)]),
        test_channel(600, [test_overwrite(?OTHER_ROLE, 0)])
    ],
    Roles = [test_role(?GUILD_ID), test_role(?VIEWER_ROLE), test_role(?OTHER_ROLE)],
    Members = maps:from_list(
        [{7, test_member(7, [])}] ++
            [{Id, test_member(Id, [?VIEWER_ROLE])} || Id <- [10, 20, 21]] ++
            [{Id, test_member(Id, [?OTHER_ROLE])} || Id <- [30, 31, 99, 4242]]
    ),
    #{
        id => ?GUILD_ID,
        virtual_channel_access => #{99 => sets:from_list([500])},
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"7">>},
            <<"roles">> => Roles,
            <<"members">> => Members,
            <<"channels">> => Channels,
            <<"channel_index">> => guild_data_index:build_id_index(Channels)
        }
    }.

%% filter_member_ids/3 as it read before the memo: every candidate materialises its
%% own complete viewable channel list.
reference_filter_member_ids(SessionUserId, MemberIds, State) ->
    SessionMap = session_channel_map(SessionUserId, State),
    lists:filtermap(
        fun(MemberId) -> reference_keep(MemberId, SessionUserId, SessionMap, State) end,
        MemberIds
    ).

reference_keep(MemberId, SessionUserId, _SessionMap, _State) when
    MemberId =:= SessionUserId; not is_integer(MemberId)
->
    false;
reference_keep(MemberId, _SessionUserId, SessionMap, State) ->
    case has_mutual_channel(MemberId, SessionMap, State) of
        true -> {true, MemberId};
        false -> false
    end.

candidate_ids() ->
    [20, 30, 21, 31, 10, 7, 99, 4242, 777, 20].

exceptions_cover_owner_virtual_and_overwrite_targets_test() ->
    Exceptions = exceptions(test_state()),
    ?assertEqual([true, true, true], [
        sets:is_element(Id, Exceptions)
     || Id <- [7, 99, 4242]
    ]),
    ?assertEqual([false, false], [sets:is_element(Id, Exceptions) || Id <- [20, 30]]).

filter_member_ids_matches_unmemoised_test() ->
    State = test_state(),
    Ids = candidate_ids(),
    ?assertEqual([20, 21, 7, 99, 4242, 20], filter_member_ids(10, Ids, State)),
    ?assertEqual(
        [reference_filter_member_ids(Viewer, Ids, State) || Viewer <- [10, 30, 7, 99]],
        [filter_member_ids(Viewer, Ids, State) || Viewer <- [10, 30, 7, 99]]
    ).

memo_holds_one_entry_per_roles_term_test() ->
    State = test_state(),
    SessionMap = session_channel_map(10, State),
    {First, Memo1} = memoised_has_mutual_channel(20, SessionMap, State, new_memo(State)),
    {Second, Memo2} = memoised_has_mutual_channel(21, SessionMap, State, Memo1),
    ?assert(First),
    ?assertEqual(First, Second),
    ?assertEqual([1, 1], [map_size(maps:get(cache, M)) || M <- [Memo1, Memo2]]).

exception_user_is_not_answered_from_its_role_set_test() ->
    State = test_state(),
    SessionMap = session_channel_map(10, State),
    {false, Memo} = memoised_has_mutual_channel(30, SessionMap, State, new_memo(State)),
    ?assertEqual(
        [{true, Memo}, {true, Memo}, {true, Memo}],
        [memoised_has_mutual_channel(Id, SessionMap, State, Memo) || Id <- [7, 99, 4242]]
    ).

non_member_candidate_is_dropped_test() ->
    State = test_state(),
    ?assertEqual([], filter_member_ids(10, [777], State)),
    ?assertEqual([], reference_filter_member_ids(10, [777], State)).

-endif.
