%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_channel_engine).
-typing([eqwalizer]).

-export([
    ref/2,
    is_engine_list/2,
    ensure/2,
    rebuild/2,
    rebuild_channels/2,
    drop/2,
    destroy_all/1,
    sync_online/3,
    update_user/3,
    update_user_all/2,
    remove_user/3,
    remove_user_all/2,
    set_hoisted_roles_all/2
]).

-type guild_state() :: map().
-type list_id() :: binary().
-type visibility_memo() :: disabled | #{exceptions := sets:set(integer()), cache := map()}.
-type role_cache() :: #{term() => {[integer()], [integer()]}}.
-type engine_ref() :: ets:table().

-export_type([guild_state/0, list_id/0, engine_ref/0]).

-define(ENGINES_KEY, channel_member_list_engines).

-spec ref(list_id(), guild_state()) -> engine_ref() | undefined.
ref(ListId, State) ->
    maps:get(ListId, engines(State), undefined).

-spec is_engine_list(list_id(), guild_state()) -> boolean().
is_engine_list(<<"0">>, _State) ->
    false;
is_engine_list(ListId, _State) ->
    channel_id(ListId) =/= undefined.

-spec ensure(list_id(), guild_state()) -> guild_state().
ensure(ListId, State) ->
    case is_engine_list(ListId, State) andalso not maps:is_key(ListId, engines(State)) of
        true -> build(ListId, State);
        false -> State
    end.

-spec rebuild(list_id(), guild_state()) -> guild_state().
rebuild(ListId, State) ->
    case maps:get(ListId, engines(State), undefined) of
        undefined ->
            ensure(ListId, State);
        OldRef ->
            rebuild_existing(ListId, OldRef, State)
    end.

-spec rebuild_existing(list_id(), engine_ref(), guild_state()) -> guild_state().
rebuild_existing(ListId, OldRef, State) ->
    case channel_id(ListId) of
        undefined ->
            drop(ListId, State);
        ChannelId ->
            replace_engine(ListId, ChannelId, OldRef, State)
    end.

-spec replace_engine(list_id(), pos_integer(), engine_ref(), guild_state()) -> guild_state().
replace_engine(ListId, ChannelId, OldRef, State) ->
    NewRef = load_engine(ChannelId, State),
    State1 = put_engines(maps:put(ListId, NewRef, engines(State)), State),
    guild_member_list_engine:destroy(OldRef),
    guild_member_list_engine_inputs:record(ListId, ChannelId, State, State1).

-spec rebuild_channels([pos_integer()], guild_state()) -> guild_state().
rebuild_channels(ChannelIds, State) ->
    lists:foldl(
        fun rebuild_channel_if_loaded/2,
        State,
        ChannelIds
    ).

-spec rebuild_channel_if_loaded(pos_integer(), guild_state()) -> guild_state().
rebuild_channel_if_loaded(ChannelId, State) ->
    ListId = integer_to_binary(ChannelId),
    case maps:is_key(ListId, engines(State)) of
        true -> rebuild(ListId, State);
        false -> State
    end.

-spec drop(list_id(), guild_state()) -> guild_state().
drop(ListId, State) ->
    Engines = engines(State),
    case maps:get(ListId, Engines, undefined) of
        undefined ->
            State;
        Ref ->
            guild_member_list_engine:destroy(Ref),
            guild_member_list_engine_inputs:forget(
                ListId, put_engines(maps:remove(ListId, Engines), State)
            )
    end.

-spec destroy_all(guild_state()) -> guild_state().
destroy_all(State) ->
    maps:foreach(
        fun(_ListId, Ref) -> guild_member_list_engine:destroy(Ref) end,
        engines(State)
    ),
    guild_member_list_engine_inputs:forget_all(put_engines(#{}, State)).

-spec sync_online(integer(), boolean(), guild_state()) -> ok.
sync_online(UserId, IsOnline, State) ->
    maps:foreach(
        fun(_ListId, Ref) ->
            guild_member_list_engine:set_online(Ref, UserId, IsOnline)
        end,
        engines(State)
    ),
    ok.

-spec update_user(integer(), list_id(), guild_state()) -> ok.
update_user(UserId, ListId, State) ->
    case ref(ListId, State) of
        undefined ->
            ok;
        Ref ->
            update_user_in_engine(UserId, ListId, Ref, State)
    end.

-spec update_user_all(integer(), guild_state()) -> ok.
update_user_all(UserId, State) ->
    maps:foreach(
        fun(ListId, _Ref) ->
            ok = update_user(UserId, ListId, State)
        end,
        engines(State)
    ),
    ok.

-spec update_user_in_engine(integer(), list_id(), engine_ref(), guild_state()) -> ok.
update_user_in_engine(UserId, ListId, Ref, State) ->
    case channel_id(ListId) of
        undefined ->
            ok;
        ChannelId ->
            update_user_in_channel(UserId, ChannelId, Ref, State)
    end.

-spec update_user_in_channel(integer(), pos_integer(), engine_ref(), guild_state()) -> ok.
update_user_in_channel(UserId, ChannelId, Ref, State) ->
    Data = maps:get(data, State, #{}),
    case guild_data_index_members:get_member(UserId, Data) of
        Member when is_map(Member) ->
            upsert_visible_user(UserId, ChannelId, Member, Ref, State);
        _ ->
            guild_member_list_engine:remove_member(Ref, UserId)
    end.

-spec upsert_visible_user(integer(), pos_integer(), map(), engine_ref(), guild_state()) -> ok.
upsert_visible_user(UserId, ChannelId, Member, Ref, State) ->
    case can_view(UserId, ChannelId, Member, State) of
        true ->
            DisplayName = guild_member_list_common:get_member_display_name(Member),
            SortKey = guild_member_list_common:casefold_binary(DisplayName),
            RoleIds = guild_member_list_store:extract_role_ids(Member),
            IsOnline = guild_member_list_connected:user_is_online(UserId, State),
            guild_member_list_engine:update_member(Ref, UserId, SortKey, RoleIds, IsOnline);
        false ->
            guild_member_list_engine:remove_member(Ref, UserId)
    end.

-spec remove_user(integer(), list_id(), guild_state()) -> ok.
remove_user(UserId, ListId, State) ->
    case ref(ListId, State) of
        undefined -> ok;
        Ref -> guild_member_list_engine:remove_member(Ref, UserId)
    end.

-spec remove_user_all(integer(), guild_state()) -> ok.
remove_user_all(UserId, State) ->
    maps:foreach(
        fun(ListId, _Ref) ->
            ok = remove_user(UserId, ListId, State)
        end,
        engines(State)
    ),
    ok.

-spec set_hoisted_roles_all([integer()], guild_state()) -> boolean().
set_hoisted_roles_all(HoistedRoleIds, State) ->
    {_Roles, Changed} = maps:fold(
        fun fold_hoisted_role_change/3,
        {HoistedRoleIds, false},
        engines(State)
    ),
    Changed.

-spec fold_hoisted_role_change(list_id(), engine_ref(), {[integer()], boolean()}) ->
    {[integer()], boolean()}.
fold_hoisted_role_change(_ListId, Ref, {HoistedRoleIds, AnyChanged}) ->
    Result = guild_member_list_engine:set_hoisted_roles(Ref, HoistedRoleIds),
    {HoistedRoleIds, merge_hoisted_role_result(Result, AnyChanged)}.

-spec merge_hoisted_role_result(changed | unchanged, boolean()) -> boolean().
merge_hoisted_role_result(changed, _AnyChanged) ->
    true;
merge_hoisted_role_result(unchanged, AnyChanged) ->
    AnyChanged.

-spec build(list_id(), guild_state()) -> guild_state().
build(ListId, State) ->
    case channel_id(ListId) of
        undefined ->
            State;
        ChannelId ->
            Ref = load_or_clone_engine(ListId, ChannelId, State),
            guild_member_list_engine_inputs:record(
                ListId,
                ChannelId,
                State,
                put_engines(maps:put(ListId, Ref, engines(State)), State)
            )
    end.

-spec load_or_clone_engine(list_id(), pos_integer(), guild_state()) -> engine_ref().
load_or_clone_engine(ListId, ChannelId, State) ->
    Engines = maps:remove(ListId, engines(State)),
    case guild_member_list_engine_inputs:current_twin(ChannelId, maps:keys(Engines), State) of
        {ok, TwinListId} -> guild_member_list_engine:clone(maps:get(TwinListId, Engines));
        none -> load_engine(ChannelId, State)
    end.

-spec load_engine(pos_integer(), guild_state()) -> engine_ref().
load_engine(ChannelId, State) ->
    Ref = guild_member_list_engine:new(),
    {Tuples, HoistedRoleIds} = build_inputs(ChannelId, State),
    ok = guild_member_list_engine:bulk_load(Ref, Tuples, HoistedRoleIds),
    Ref.

-spec build_inputs(pos_integer(), guild_state()) ->
    {[guild_member_list_store:member_tuple()], [integer()]}.
build_inputs(ChannelId, State) ->
    Data = maps:get(data, State, #{}),
    MemberMap = guild_data_index_members:member_map(Data),
    ConnectedUserIds = guild_member_list_common:connected_session_user_ids(State),
    PresenceTab = maps:get(member_presence, State),
    {Tuples, _Memo, _RoleCache} = maps:fold(
        fun(UserId, Member, {Acc, Memo, RoleCache}) ->
            maybe_prepare_visible_member_tuple(
                UserId,
                Member,
                ChannelId,
                State,
                PresenceTab,
                ConnectedUserIds,
                Acc,
                Memo,
                RoleCache
            )
        end,
        {[], new_visibility_memo(ChannelId, Data, State), #{}},
        MemberMap
    ),
    Roles = map_utils:ensure_list(maps:get(<<"roles">>, Data, [])),
    HoistedRoleIds =
        case guild_id(State) of
            GuildId when is_integer(GuildId), GuildId > 0 ->
                guild_member_list_store:prepare_hoisted_role_ids(Roles, GuildId);
            _ ->
                []
        end,
    {Tuples, HoistedRoleIds}.

-spec maybe_prepare_visible_member_tuple(
    integer(),
    map(),
    pos_integer(),
    guild_state(),
    ets:tid() | map(),
    sets:set(integer()),
    [guild_member_list_store:member_tuple()],
    visibility_memo(),
    role_cache()
) -> {[guild_member_list_store:member_tuple()], visibility_memo(), role_cache()}.
maybe_prepare_visible_member_tuple(
    UserId, Member, ChannelId, State, PresenceTab, ConnectedUserIds, Acc, Memo, RoleCache
) ->
    {RoleIds, SortedRoleIds, RoleCache1} = member_role_ids(Member, RoleCache),
    {Visible, Memo1} = memoised_can_view(UserId, ChannelId, Member, SortedRoleIds, State, Memo),
    case Visible of
        true ->
            {
                prepare_member_tuple(
                    UserId, Member, PresenceTab, ConnectedUserIds, RoleIds, Acc
                ),
                Memo1,
                RoleCache1
            };
        false ->
            {Acc, Memo1, RoleCache1}
    end.

%% extract_role_ids/1 and the sort that forms the visibility memo key are both
%% pure functions of the member's raw roles term, so caching on that term
%% collapses them to the number of distinct role combinations.
-spec member_role_ids(map(), role_cache()) -> {[integer()], [integer()], role_cache()}.
member_role_ids(Member, RoleCache) ->
    RawRoles = maps:get(<<"roles">>, Member, []),
    case maps:find(RawRoles, RoleCache) of
        {ok, {RoleIds, SortedRoleIds}} ->
            {RoleIds, SortedRoleIds, RoleCache};
        error ->
            RoleIds = guild_member_list_store:extract_role_ids(Member),
            SortedRoleIds = lists:sort(RoleIds),
            {RoleIds, SortedRoleIds, RoleCache#{RawRoles => {RoleIds, SortedRoleIds}}}
    end.

%% can_view_channel/4 reads nothing from the member except its role ids, so for a
%% non-category channel its result is a function of the role set. The only
%% user-specific inputs are the guild owner, virtual channel access, and the
%% channel's own permission overwrites, and those users bypass the memo.
-spec new_visibility_memo(pos_integer(), map(), guild_state()) -> visibility_memo().
new_visibility_memo(ChannelId, Data, State) ->
    case memoisable_channel(ChannelId, Data) of
        true ->
            #{
                exceptions => visibility_exceptions(ChannelId, Data, State),
                cache => #{}
            };
        false ->
            disabled
    end.

-spec memoised_can_view(
    integer(), pos_integer(), map(), [integer()], guild_state(), visibility_memo()
) -> {boolean(), visibility_memo()}.
memoised_can_view(UserId, ChannelId, Member, _SortedRoleIds, State, disabled) ->
    {can_view(UserId, ChannelId, Member, State), disabled};
memoised_can_view(UserId, ChannelId, Member, SortedRoleIds, State, Memo) ->
    #{exceptions := Exceptions} = Memo,
    case sets:is_element(UserId, Exceptions) of
        true ->
            {can_view(UserId, ChannelId, Member, State), Memo};
        false ->
            cached_can_view(UserId, ChannelId, Member, SortedRoleIds, State, Memo)
    end.

-spec cached_can_view(
    integer(), pos_integer(), map(), [integer()], guild_state(), visibility_memo()
) -> {boolean(), visibility_memo()}.
cached_can_view(UserId, ChannelId, Member, SortedRoleIds, State, #{cache := Cache} = Memo) ->
    case maps:find(SortedRoleIds, Cache) of
        {ok, Visible} ->
            {Visible, Memo};
        error ->
            Visible = can_view(UserId, ChannelId, Member, State),
            {Visible, Memo#{cache := Cache#{SortedRoleIds => Visible}}}
    end.

%% Category visibility recurses into child channels, each with its own
%% overwrites, so the exception set is not local to this channel. Do not memoise.
-spec memoisable_channel(pos_integer(), map()) -> boolean().
memoisable_channel(ChannelId, Data) ->
    case maps:get(ChannelId, guild_data_index_channels:channel_index(Data), undefined) of
        Channel when is_map(Channel) -> maps:get(<<"type">>, Channel, undefined) =/= 4;
        _ -> false
    end.

-spec visibility_exceptions(pos_integer(), map(), guild_state()) -> sets:set(integer()).
visibility_exceptions(ChannelId, Data, State) ->
    Guild = map_utils:ensure_map(maps:get(<<"guild">>, Data, #{})),
    Owner =
        case snowflake_id:parse_optional(maps:get(<<"owner_id">>, Guild, undefined)) of
            OwnerId when is_integer(OwnerId) -> [OwnerId];
            _ -> []
        end,
    Virtual = maps:keys(map_utils:ensure_map(maps:get(virtual_channel_access, State, #{}))),
    sets:from_list(Owner ++ Virtual ++ overwrite_target_ids(ChannelId, Data)).

%% Every overwrite target on the channel, role and user alike. Role ids can never
%% collide with user ids, so the superset costs only a few direct computations.
-spec overwrite_target_ids(pos_integer(), map()) -> [integer()].
overwrite_target_ids(ChannelId, Data) ->
    Cache = map_utils:ensure_map(maps:get(overwrite_perms_cache, Data, #{})),
    case maps:get(ChannelId, Cache, undefined) of
        Cached when is_list(Cached) ->
            [Id || {Id, _Type, _Allow, _Deny} <- Cached, is_integer(Id)];
        _ ->
            channel_overwrite_target_ids(ChannelId, Data)
    end.

-spec channel_overwrite_target_ids(pos_integer(), map()) -> [integer()].
channel_overwrite_target_ids(ChannelId, Data) ->
    case maps:get(ChannelId, guild_data_index_channels:channel_index(Data), undefined) of
        Channel when is_map(Channel) ->
            lists:filtermap(
                fun overwrite_target_id/1,
                map_utils:ensure_list(maps:get(<<"permission_overwrites">>, Channel, []))
            );
        _ ->
            []
    end.

-spec overwrite_target_id(term()) -> {true, integer()} | false.
overwrite_target_id(Overwrite) when is_map(Overwrite) ->
    case snowflake_id:parse_optional(maps:get(<<"id">>, Overwrite, undefined)) of
        Id when is_integer(Id) -> {true, Id};
        _ -> false
    end;
overwrite_target_id(_Overwrite) ->
    false.

-spec prepare_member_tuple(
    integer(),
    map(),
    ets:tid() | map(),
    sets:set(integer()),
    [integer()],
    [guild_member_list_store:member_tuple()]
) -> [guild_member_list_store:member_tuple()].
prepare_member_tuple(UserId, Member, PresenceTab, ConnectedUserIds, RoleIds, Acc) ->
    DisplayName = guild_member_list_common:get_member_display_name(Member),
    SortKey = guild_member_list_common:casefold_binary(DisplayName),
    IsOnline = member_is_online(UserId, PresenceTab, ConnectedUserIds),
    [{UserId, SortKey, RoleIds, IsOnline} | Acc].

%% Presence is consulted only to decide IsOnline, and IsOnline is false for anyone
%% without a connected session, so the ETS read and its default-presence
%% allocation are skipped for every member who is not connected.
-spec member_is_online(integer(), ets:tid() | map(), sets:set(integer())) -> boolean().
member_is_online(UserId, PresenceTab, ConnectedUserIds) ->
    sets:is_element(UserId, ConnectedUserIds) andalso
        presence_status_is_online(UserId, PresenceTab).

-spec presence_status_is_online(integer(), ets:tid() | map()) -> boolean().
presence_status_is_online(UserId, PresenceTab) ->
    Presence = guild_state_member:lookup_presence(PresenceTab, UserId),
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    Status =/= <<"offline">> andalso Status =/= <<"invisible">>.

-spec can_view(integer(), pos_integer(), map(), guild_state()) -> boolean().
can_view(UserId, ChannelId, Member, State) when is_integer(UserId), UserId > 0 ->
    try
        guild_permissions_check:can_view_channel(UserId, ChannelId, Member, State)
    catch
        _:_ -> false
    end;
can_view(_UserId, _ChannelId, _Member, _State) ->
    false.

-spec engines(guild_state()) -> #{list_id() => engine_ref()}.
engines(State) ->
    case maps:get(?ENGINES_KEY, State, #{}) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    end.

-spec put_engines(#{list_id() => engine_ref()}, guild_state()) -> guild_state().
put_engines(Map, State) when map_size(Map) =:= 0 ->
    maps:remove(?ENGINES_KEY, State);
put_engines(Map, State) ->
    State#{?ENGINES_KEY => Map}.

-spec channel_id(list_id()) -> pos_integer() | undefined.
channel_id(ListId) ->
    case snowflake_id:parse_maybe(ListId) of
        Id when is_integer(Id), Id > 0 -> Id;
        _ -> undefined
    end.

-spec guild_id(guild_state()) -> integer() | undefined.
guild_id(State) ->
    snowflake_id:parse_maybe(maps:get(id, State, undefined)).
