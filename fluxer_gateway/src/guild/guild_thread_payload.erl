%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_thread_payload).
-typing([eqwalizer]).

-export([
    mask_payload/2,
    strip_channel_surface/1,
    mask_role/1,
    mask_overwrites/1,
    strip_internal/1,
    starter_variants/4,
    hide_unreadable_source/4
]).

-define(THREAD_MESSAGE_FLAGS, 16#120).
-define(THREAD_CREATED_MESSAGE, 18).
-define(THREAD_STARTER_MESSAGE, 21).
-define(THREAD_PERMISSION_BITS,
    (16#400000000 bor 16#800000000 bor 16#1000000000 bor 16#4000000000)
).
-define(SURFACE_KEYS, [
    <<"thread_metadata">>,
    <<"applied_tags">>,
    <<"available_tags">>,
    <<"default_auto_archive_duration">>,
    <<"default_thread_rate_limit_per_user">>,
    <<"default_reaction_emoji">>,
    <<"default_sort_order">>,
    <<"default_forum_layout">>,
    <<"default_tag_setting">>,
    <<"message_count">>,
    <<"total_message_sent">>,
    <<"member_ids_preview">>,
    <<"member_count">>,
    <<"flags">>
]).

-spec mask_payload(atom(), map()) -> map().
mask_payload(Event, Data) when
    Event =:= message_create; Event =:= message_update
->
    mask_message(Data);
mask_payload(Event, Data) when
    Event =:= channel_create; Event =:= channel_update; Event =:= channel_delete
->
    mask_overwrites(strip_channel_surface(Data));
mask_payload(channel_update_bulk, #{<<"channels">> := Channels} = Data) when
    is_list(Channels)
->
    Data#{
        <<"channels">> => [
            mask_overwrites(strip_channel_surface(Channel))
         || Channel <- Channels, is_map(Channel)
        ]
    };
mask_payload(Event, #{<<"role">> := Role} = Data) when
    is_map(Role), Event =:= guild_role_create; is_map(Role), Event =:= guild_role_update
->
    Data#{<<"role">> => mask_role(Role)};
mask_payload(guild_role_update_bulk, #{<<"roles">> := Roles} = Data) when is_list(Roles) ->
    Data#{<<"roles">> => [mask_role(Role) || Role <- Roles, is_map(Role)]};
mask_payload(_Event, Data) ->
    Data.

-spec mask_message(map()) -> map().
mask_message(Message) ->
    Message1 = maps:remove(<<"thread">>, Message),
    Message2 =
        case maps:get(<<"flags">>, Message1, undefined) of
            Flags when is_integer(Flags) ->
                Message1#{<<"flags">> => Flags band bnot ?THREAD_MESSAGE_FLAGS};
            _ ->
                Message1
        end,
    Message3 =
        case maps:get(<<"referenced_message">>, Message2, undefined) of
            #{<<"type">> := ?THREAD_CREATED_MESSAGE} ->
                Message2#{<<"referenced_message">> => null};
            Referenced when is_map(Referenced) ->
                Message2#{<<"referenced_message">> => mask_message(Referenced)};
            _ ->
                Message2
        end,
    case maps:get(<<"message_snapshots">>, Message3, undefined) of
        Snapshots when is_list(Snapshots) ->
            Message3#{<<"message_snapshots">> => [mask_snapshot(S) || S <- Snapshots]};
        _ ->
            Message3
    end.

-spec mask_snapshot(term()) -> term().
mask_snapshot(#{<<"message">> := Message} = Snapshot) when is_map(Message) ->
    Snapshot#{<<"message">> => mask_message(Message)};
mask_snapshot(Snapshot) when is_map(Snapshot) ->
    mask_message(Snapshot);
mask_snapshot(Snapshot) ->
    Snapshot.

-spec strip_channel_surface(map()) -> map().
strip_channel_surface(Channel) ->
    maps:without(?SURFACE_KEYS, Channel).

-spec mask_role(map()) -> map().
mask_role(#{<<"permissions">> := Permissions} = Role) ->
    Role#{<<"permissions">> => mask_bits(Permissions)};
mask_role(Role) ->
    Role.

-spec mask_overwrites(map()) -> map().
mask_overwrites(#{<<"permission_overwrites">> := Overwrites} = Channel) when
    is_list(Overwrites)
->
    Channel#{<<"permission_overwrites">> => [mask_overwrite(O) || O <- Overwrites]};
mask_overwrites(Channel) ->
    Channel.

-spec mask_overwrite(term()) -> term().
mask_overwrite(Overwrite) when is_map(Overwrite) ->
    maps:map(
        fun
            (<<"allow">>, Value) -> mask_bits(Value);
            (<<"deny">>, Value) -> mask_bits(Value);
            (_Key, Value) -> Value
        end,
        Overwrite
    );
mask_overwrite(Overwrite) ->
    Overwrite.

-spec mask_bits(term()) -> term().
mask_bits(Value) when is_integer(Value) ->
    Value band bnot ?THREAD_PERMISSION_BITS;
mask_bits(Value) when is_binary(Value) ->
    case bitset:parse_optional(Value) of
        Bits when is_integer(Bits) -> integer_to_binary(Bits band bnot ?THREAD_PERMISSION_BITS);
        _ -> Value
    end;
mask_bits(Value) ->
    Value.

-spec starter_variants(atom(), map(), [{binary(), map()}], map()) ->
    [{[{binary(), map()}], map()}].
starter_variants(Event, Data, Sessions, State) ->
    case starter_source(Event, Data) of
        none ->
            [{Sessions, Data}];
        Source ->
            split_starter_sessions(Source, Data, Sessions, State)
    end.

-spec split_starter_sessions(
    unknown | {integer(), binary()}, map(), [{binary(), map()}], map()
) ->
    [{[{binary(), map()}], map()}].
split_starter_sessions(Source, Data, Sessions, State) ->
    {Readable, Hidden, _Memo} = lists:foldl(
        fun(Pair, Acc) -> split_starter_session(Pair, Acc, Source, State) end,
        {[], [], #{}},
        Sessions
    ),
    [V || {[_ | _], _} = V <- [{Readable, Data}, {Hidden, without_source(Data)}]].

-spec split_starter_session(
    {binary(), map()},
    {[{binary(), map()}], [{binary(), map()}], map()},
    unknown | {integer(), binary()},
    map()
) -> {[{binary(), map()}], [{binary(), map()}], map()}.
split_starter_session({_Sid, Session} = Pair, {ReadableAcc, HiddenAcc, Memo}, Source, State) ->
    UserId = maps:get(user_id, Session, undefined),
    case memo_readable(UserId, Memo, Source, State) of
        {true, Memo1} -> {[Pair | ReadableAcc], HiddenAcc, Memo1};
        {false, Memo1} -> {ReadableAcc, [Pair | HiddenAcc], Memo1}
    end.

-spec memo_readable(term(), map(), unknown | {integer(), binary()}, map()) ->
    {boolean(), map()}.
memo_readable(UserId, Memo, Source, State) ->
    PermissionsFor = fun(ParentId) -> member_permissions(UserId, ParentId, State) end,
    case Memo of
        #{UserId := Known} ->
            {Known, Memo};
        _ ->
            Known = source_readable(Source, PermissionsFor, State),
            {Known, Memo#{UserId => Known}}
    end.

-spec hide_unreadable_source(atom(), map(), fun((integer()) -> integer()), map()) -> map().
hide_unreadable_source(Event, Data, PermissionsFor, State) ->
    case starter_source(Event, Data) of
        none ->
            Data;
        Source ->
            readable_or_stripped(source_readable(Source, PermissionsFor, State), Data)
    end.

-spec readable_or_stripped(boolean(), map()) -> map().
readable_or_stripped(true, Data) ->
    Data;
readable_or_stripped(false, Data) ->
    without_source(Data).

-spec starter_source(atom(), map()) -> none | unknown | {integer(), binary()}.
starter_source(
    message_create,
    #{<<"type">> := ?THREAD_STARTER_MESSAGE, <<"referenced_message">> := Referenced} = Data
) when is_map(Referenced) ->
    case maps:get(<<"message_reference">>, Data, undefined) of
        #{<<"channel_id">> := ChannelId, <<"message_id">> := MessageId} when
            is_binary(MessageId)
        ->
            parent_source(snowflake_id:parse_optional(ChannelId), MessageId);
        _ ->
            unknown
    end;
starter_source(_Event, _Data) ->
    none.

-spec parent_source(term(), binary()) -> unknown | {integer(), binary()}.
parent_source(ParentId, MessageId) when is_integer(ParentId) ->
    {ParentId, MessageId};
parent_source(_ParentId, _MessageId) ->
    unknown.

-spec source_readable(unknown | {integer(), binary()}, fun((integer()) -> integer()), map()) ->
    boolean().
source_readable(unknown, _PermissionsFor, _State) ->
    false;
source_readable({ParentId, MessageId}, PermissionsFor, State) ->
    guild_permissions_check:can_access_message_by_permissions(
        PermissionsFor(ParentId), MessageId, State
    ).

-spec member_permissions(term(), integer(), map()) -> integer().
member_permissions(UserId, ParentId, State) when is_integer(UserId) ->
    guild_permissions:get_member_permissions(UserId, ParentId, State);
member_permissions(_UserId, _ParentId, _State) ->
    0.

-spec without_source(map()) -> map().
without_source(Data) ->
    Data#{<<"referenced_message">> => null}.

-spec strip_internal(map()) -> map().
strip_internal(Data) ->
    maps:filter(fun(Key, _Value) -> not internal_key(Key) end, Data).

-spec internal_key(term()) -> boolean().
internal_key(<<"_fluxer_", _/binary>>) -> true;
internal_key(<<"__thread_", _/binary>>) -> true;
internal_key(_) -> false.
