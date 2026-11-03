%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_visitor_permissions).
-typing([eqwalizer]).

-export([
    viewable_channel_map/2,
    strip_overwrites/2,
    guild_nsfw/1
]).

-define(AGE_RESTRICTED_LEVEL, 3).

-type context() :: #{
    guild_id := integer(),
    everyone := non_neg_integer(),
    guild_nsfw := boolean(),
    adult := boolean()
}.

-spec mask() -> non_neg_integer().
mask() ->
    constants:view_channel_permission() bor constants:read_message_history_permission().

-spec context(map(), boolean()) -> context().
context(#{id := GuildId} = State, Adult) when is_integer(GuildId) ->
    Data = guild_data_index:ensure_data_map(State),
    #{
        guild_id => GuildId,
        everyone => everyone_permissions(Data, GuildId),
        guild_nsfw => guild_nsfw(Data),
        adult => Adult
    }.

-spec can_view(context(), map(), map() | undefined) -> boolean().
can_view(Context, Channel, Parent) ->
    not hidden_nsfw(Context, Channel, Parent) andalso
        bitset:has(visible_permissions(Context, Channel), constants:view_channel_permission()).

-spec viewable_channel_map(map(), boolean()) -> #{integer() => true}.
viewable_channel_map(State, Adult) ->
    Data = guild_data_index:ensure_data_map(State),
    Context = context(State, Adult),
    Index = guild_data_index_channels:channel_index(Data),
    lists:foldl(
        fun(Channel, Acc) -> add_viewable(Channel, Index, Context, Acc) end,
        #{},
        map_utils:ensure_list(guild_data_index_channels:channel_list(Data))
    ).

-spec strip_overwrites(map(), integer()) -> map().
strip_overwrites(Channel, GuildId) ->
    case maps:get(<<"permission_overwrites">>, Channel, undefined) of
        Overwrites when is_list(Overwrites) ->
            Channel#{
                <<"permission_overwrites">> => [
                    Overwrite
                 || Overwrite <- Overwrites, overwrite_id(Overwrite) =:= GuildId
                ]
            };
        _ ->
            Channel
    end.

-spec guild_nsfw(map()) -> boolean().
guild_nsfw(Data) ->
    Guild = map_utils:ensure_map(maps:get(<<"guild">>, Data, #{})),
    case maps:get(<<"nsfw">>, Guild, undefined) of
        true -> true;
        false -> false;
        _ -> maps:get(<<"nsfw_level">>, Guild, 0) =:= ?AGE_RESTRICTED_LEVEL
    end.

-spec everyone_permissions(map(), integer()) -> non_neg_integer().
everyone_permissions(Data, GuildId) ->
    lists:foldl(
        fun(Role, Acc) -> everyone_role_permissions(Role, GuildId, Acc) end,
        0,
        map_utils:ensure_list(maps:get(<<"roles">>, Data, []))
    ).

-spec everyone_role_permissions(term(), integer(), non_neg_integer()) -> non_neg_integer().
everyone_role_permissions(Role, GuildId, Acc) when is_map(Role) ->
    case snowflake_id:parse_optional(maps:get(<<"id">>, Role, undefined)) of
        GuildId -> parse_bits(maps:get(<<"permissions">>, Role, 0));
        _ -> Acc
    end;
everyone_role_permissions(_Role, _GuildId, Acc) ->
    Acc.

-spec add_viewable(term(), map(), context(), #{integer() => true}) -> #{integer() => true}.
add_viewable(Channel, Index, Context, Acc) when is_map(Channel) ->
    ChannelId = snowflake_id:parse_optional(maps:get(<<"id">>, Channel, undefined)),
    case
        is_integer(ChannelId) andalso can_view(Context, Channel, parent_channel(Channel, Index))
    of
        true when is_integer(ChannelId) -> Acc#{ChannelId => true};
        false -> Acc
    end;
add_viewable(_Channel, _Index, _Context, Acc) ->
    Acc.

-spec parent_channel(map(), map()) -> map() | undefined.
parent_channel(Channel, Index) ->
    ParentId = snowflake_id:parse_optional(maps:get(<<"parent_id">>, Channel, undefined)),
    case Index of
        #{ParentId := Parent} when is_integer(ParentId), is_map(Parent) ->
            Parent;
        _ ->
            undefined
    end.

-spec hidden_nsfw(context(), map(), map() | undefined) -> boolean().
hidden_nsfw(#{adult := true}, _Channel, _Parent) ->
    false;
hidden_nsfw(#{guild_nsfw := GuildNsfw}, Channel, Parent) ->
    effective_nsfw(Channel, Parent, GuildNsfw).

-spec effective_nsfw(map(), map() | undefined, boolean()) -> boolean().
effective_nsfw(Channel, Parent, GuildNsfw) ->
    case nsfw_override(Channel) of
        Value when is_boolean(Value) -> Value;
        undefined -> inherited_nsfw(Channel, Parent, GuildNsfw)
    end.

-spec inherited_nsfw(map(), map() | undefined, boolean()) -> boolean().
inherited_nsfw(Channel, Parent, GuildNsfw) ->
    IsCategory = channel_types:is_category(
        map_utils:get_integer(Channel, <<"type">>, undefined)
    ),
    ParentOverride =
        case {IsCategory, Parent} of
            {false, ParentMap} when is_map(ParentMap) -> nsfw_override(ParentMap);
            _ -> undefined
        end,
    case ParentOverride of
        undefined -> GuildNsfw;
        Value -> Value
    end.

-spec nsfw_override(map()) -> boolean() | undefined.
nsfw_override(Channel) ->
    case maps:find(<<"nsfw_override">>, Channel) of
        {ok, Value} when is_boolean(Value) -> Value;
        {ok, _} -> undefined;
        error -> legacy_nsfw(Channel)
    end.

-spec legacy_nsfw(map()) -> boolean() | undefined.
legacy_nsfw(Channel) ->
    case maps:get(<<"nsfw">>, Channel, undefined) of
        Value when is_boolean(Value) -> Value;
        _ -> undefined
    end.

-spec visible_permissions(context(), map()) -> non_neg_integer().
visible_permissions(#{guild_id := GuildId, everyone := Everyone}, Channel) ->
    Base = Everyone band (bnot constants:administrator_permission()),
    Overwrites = map_utils:ensure_list(maps:get(<<"permission_overwrites">>, Channel, [])),
    Applied = lists:foldl(
        fun(Overwrite, Acc) -> apply_everyone_overwrite(Overwrite, GuildId, Acc) end,
        Base,
        Overwrites
    ),
    Applied band mask().

-spec apply_everyone_overwrite(term(), integer(), non_neg_integer()) -> non_neg_integer().
apply_everyone_overwrite(Overwrite, GuildId, Acc) when is_map(Overwrite) ->
    case {overwrite_type(Overwrite), overwrite_id(Overwrite)} of
        {0, GuildId} ->
            Allow = parse_bits(maps:get(<<"allow">>, Overwrite, 0)),
            Deny = parse_bits(maps:get(<<"deny">>, Overwrite, 0)),
            (Acc band (bnot Deny)) bor Allow;
        _ ->
            Acc
    end;
apply_everyone_overwrite(_Overwrite, _GuildId, Acc) ->
    Acc.

-spec overwrite_type(map()) -> integer() | undefined.
overwrite_type(Overwrite) ->
    case map_utils:get_integer(Overwrite, <<"type">>, undefined) of
        Type when is_integer(Type) -> Type;
        _ -> undefined
    end.

-spec overwrite_id(term()) -> integer() | undefined.
overwrite_id(Overwrite) when is_map(Overwrite) ->
    snowflake_id:parse_optional(maps:get(<<"id">>, Overwrite, undefined));
overwrite_id(_) ->
    undefined.

-spec parse_bits(term()) -> non_neg_integer().
parse_bits(Value) ->
    case bitset:parse_maybe(Value) of
        Bits when is_integer(Bits), Bits >= 0 -> Bits;
        _ -> 0
    end.
