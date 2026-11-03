%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_thread_payload).
-typing([eqwalizer]).

-export([
    mask_payload/2,
    strip_channel_surface/1,
    mask_role/1,
    mask_overwrites/1,
    strip_internal/1
]).

-define(THREAD_MESSAGE_FLAGS, 16#120).
-define(THREAD_CREATED_MESSAGE, 18).
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
    Event =:= channel_create; Event =:= channel_update
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

-spec strip_internal(map()) -> map().
strip_internal(Data) ->
    maps:filter(fun(Key, _Value) -> not internal_key(Key) end, Data).

-spec internal_key(term()) -> boolean().
internal_key(<<"_fluxer_", _/binary>>) -> true;
internal_key(<<"__thread_", _/binary>>) -> true;
internal_key(_) -> false.
