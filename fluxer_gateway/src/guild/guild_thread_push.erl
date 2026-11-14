%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_thread_push).
-typing([eqwalizer]).

-export([
    scope/2,
    is_candidate/4,
    role_visible/2,
    role_adds/4,
    push_message_data/4,
    forum_starter/3,
    mark_forum_thread_created/1,
    mention_universe/2,
    passive_threads/3
]).

-export_type([scope/0]).

-define(THREAD_PUSH_KEY, <<"__thread_push">>).
-define(FORUM_THREAD_CREATED_KEY, <<"__forum_thread_created">>).
-define(MEMBER_SETTING_KEYS, [<<"flags">>, <<"muted">>, <<"mute_config">>]).

-define(MAX_ROLE_MENTION_THREAD_ADDS, 250).
-define(MAX_THREAD_MEMBERS, 1000).

-type user_id() :: integer().
-type member_source() :: {ets, ets:tid()} | {map, map()}.

-type scope() :: #{thread := map(), members := #{integer() => map()}} | undefined.

-spec scope(integer(), map()) -> scope().
scope(ChannelId, State) ->
    case guild_thread_gate:store(State) of
        undefined -> undefined;
        Tab -> safe_scope(Tab, ChannelId)
    end.

-spec safe_scope(ets:table(), integer()) -> scope().
safe_scope(Tab, ChannelId) ->
    try guild_thread_store:get_thread(Tab, ChannelId) of
        undefined ->
            undefined;
        Thread ->
            #{
                thread => Thread,
                members => maps:from_list(guild_thread_store:members(Tab, ChannelId))
            }
    catch
        error:badarg -> undefined
    end.

-spec push_message_data(map(), integer(), [integer()], map()) -> {map(), binary()} | undefined.
push_message_data(MessageData, ChannelId, UserIds, Data) ->
    case scope(ChannelId, Data) of
        undefined ->
            undefined;
        #{thread := Thread, members := Members} ->
            ParentId = snowflake_id:parse_optional(
                maps:get(<<"parent_id">>, Thread, undefined)
            ),
            Parent = parent_channel(ParentId, Data),
            {ForumFields, MessageData1} = take_forum_thread_created(MessageData),
            ThreadPush = ForumFields#{
                <<"parent_id">> => ParentId,
                <<"parent_name">> => binary_or_undefined(
                    maps:get(<<"name">>, Parent, undefined)
                ),
                <<"category_id">> => snowflake_id:parse_optional(
                    maps:get(<<"parent_id">>, Parent, undefined)
                ),
                <<"members">> => member_settings(UserIds, Members)
            },
            {
                MessageData1#{?THREAD_PUSH_KEY => ThreadPush},
                thread_name(Thread)
            }
    end.

-spec take_forum_thread_created(map()) -> {#{binary() => true}, map()}.
take_forum_thread_created(MessageData) ->
    case maps:take(?FORUM_THREAD_CREATED_KEY, MessageData) of
        {true, Rest} -> {#{<<"forum_thread_created">> => true}, Rest};
        {_Other, Rest} -> {#{}, Rest};
        error -> {#{}, MessageData}
    end.

-spec mark_forum_thread_created(map()) -> map().
mark_forum_thread_created(MessageData) ->
    MessageData#{?FORUM_THREAD_CREATED_KEY => true}.

-spec forum_starter(map(), integer(), map()) -> {integer(), #{integer() => map()}} | undefined.
forum_starter(MessageData, ChannelId, Data) ->
    case snowflake_id:parse_maybe(maps:get(<<"id">>, MessageData, undefined)) of
        ChannelId -> forum_starter_scope(scope(ChannelId, Data), Data);
        _ -> undefined
    end.

-spec forum_starter_scope(scope(), map()) -> {integer(), #{integer() => map()}} | undefined.
forum_starter_scope(#{thread := Thread, members := Members}, Data) ->
    ParentId = snowflake_id:parse_maybe(maps:get(<<"parent_id">>, Thread, undefined)),
    case
        channel_types:is_thread_only(
            maps:get(<<"type">>, parent_channel(ParentId, Data), undefined)
        )
    of
        true when is_integer(ParentId) -> {ParentId, Members};
        _ -> undefined
    end;
forum_starter_scope(undefined, _Data) ->
    undefined.

-spec parent_channel(integer() | undefined, map()) -> map().
parent_channel(ParentId, Data) when is_integer(ParentId) ->
    case maps:get(ParentId, guild_data_index_channels:channel_index(Data), undefined) of
        Parent when is_map(Parent) -> Parent;
        _ -> #{}
    end;
parent_channel(_ParentId, _Data) ->
    #{}.

-spec member_settings([integer()], #{integer() => map()}) -> #{integer() => map()}.
member_settings(UserIds, Members) ->
    maps:map(
        fun(_UserId, Member) -> maps:with(?MEMBER_SETTING_KEYS, Member) end,
        maps:with(UserIds, Members)
    ).

-spec thread_name(map()) -> binary().
thread_name(Thread) ->
    case binary_or_undefined(maps:get(<<"name">>, Thread, undefined)) of
        undefined -> <<"unknown">>;
        Name -> Name
    end.

-spec binary_or_undefined(term()) -> binary() | undefined.
binary_or_undefined(Value) when is_binary(Value), byte_size(Value) > 0 -> Value;
binary_or_undefined(_Value) -> undefined.

-spec mention_universe(integer(), map()) -> #{integer() => true} | undefined.
mention_universe(ChannelId, State) ->
    case scope(ChannelId, State) of
        undefined ->
            undefined;
        #{members := Members} ->
            Recipient = guild_thread_gate:recipient_active(ChannelId, State),
            maps:from_keys([UserId || UserId <- maps:keys(Members), Recipient(UserId)], true)
    end.

-spec passive_threads(map(), integer(), map()) -> [map()].
passive_threads(SessionData, UserId, State) ->
    case {guild_thread_gate:session_viewer(SessionData), guild_thread_gate:store(State)} of
        {true, Tab} when Tab =/= undefined -> joined_threads(Tab, UserId);
        _ -> []
    end.

-spec joined_threads(ets:table(), integer()) -> [map()].
joined_threads(Tab, UserId) ->
    try
        [
            Thread
         || ThreadId <- guild_thread_store:user_thread_ids(Tab, UserId),
            Thread <- [guild_thread_store:get_thread(Tab, ThreadId)],
            is_map(Thread)
        ]
    catch
        error:badarg -> []
    end.

-spec is_candidate(integer(), map(), scope(), map()) -> boolean().
is_candidate(_UserId, _Member, undefined, _Mentions) ->
    true;
is_candidate(UserId, _Member, #{members := Members}, Mentions) ->
    maps:is_key(UserId, Members) orelse
        maps:is_key(UserId, maps:get(direct_mentions, Mentions)) orelse
        maps:is_key(UserId, maps:get(role_adds, Mentions)).

-spec role_visible(integer(), map()) -> fun((integer(), map()) -> boolean()).
role_visible(ChannelId, State) ->
    Recipient = guild_thread_gate:recipient_active(ChannelId, State),
    fun(UserId, Member) ->
        Recipient(UserId) andalso
            guild_permissions_check:can_view_channel(UserId, ChannelId, Member, State)
    end.

-type role_add_request() :: #{
    members := map(),
    mentions := map(),
    mention_roles := map(),
    source := member_source(),
    visible := fun((user_id(), map()) -> boolean())
}.

-spec role_adds(
    map(), map(), {ets, ets:tid()} | {map, map()}, fun((integer(), map()) -> boolean())
) -> #{integer() => true}.
role_adds(#{thread := Thread, members := Members}, Mentions, Source, Visible) ->
    MentionRoles = maps:get(mention_roles, Mentions),
    case map_size(MentionRoles) of
        0 ->
            #{};
        _ ->
            select_role_adds(Thread, #{
                members => Members,
                mentions => Mentions,
                mention_roles => MentionRoles,
                source => Source,
                visible => Visible
            })
    end.

-spec select_role_adds(map(), role_add_request()) -> #{user_id() => true}.
select_role_adds(#{<<"type">> := 12}, _Request) ->
    #{};
select_role_adds(#{<<"thread_metadata">> := #{<<"archived">> := true}}, _Request) ->
    #{};
select_role_adds(_Thread, #{
    members := Members,
    mentions := Mentions,
    mention_roles := MentionRoles,
    source := Source,
    visible := Visible
}) ->
    Direct = maps:get(direct_mentions, Mentions),
    AuthorId = maps:get(author_id, Mentions),
    Candidates = guild_push_candidates:filtermap_members(
        fun(UserId, Member) ->
            UserId =/= AuthorId andalso
                not maps:is_key(UserId, Members) andalso
                not maps:is_key(UserId, Direct) andalso
                guild_push_candidates:has_mentioned_role(Member, MentionRoles) andalso
                not guild_members_common:is_member_bot(Member) andalso
                {true, UserId}
        end,
        Source
    ),
    Room = max(0, ?MAX_THREAD_MEMBERS - map_size(Members)),
    take_role_adds(
        lists:sort(Candidates),
        Source,
        Visible,
        min(?MAX_ROLE_MENTION_THREAD_ADDS, Room),
        #{}
    ).

-spec take_role_adds(
    [user_id()],
    member_source(),
    fun((user_id(), map()) -> boolean()),
    non_neg_integer(),
    map()
) -> #{user_id() => true}.
take_role_adds([], _Source, _Visible, _Left, Acc) ->
    Acc;
take_role_adds(_UserIds, _Source, _Visible, 0, Acc) ->
    Acc;
take_role_adds([UserId | Rest], Source, Visible, Left, Acc) ->
    Member = guild_push_candidates:lookup_member(UserId, Source),
    case is_map(Member) andalso Visible(UserId, Member) of
        true -> take_role_adds(Rest, Source, Visible, Left - 1, Acc#{UserId => true});
        false -> take_role_adds(Rest, Source, Visible, Left, Acc)
    end.
