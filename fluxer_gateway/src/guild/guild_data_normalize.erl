%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data_normalize).
-typing([eqwalizer]).

-export([
    guild_data/1,
    member/1,
    role/1,
    channel/1
]).

-spec guild_data(term()) -> term().
guild_data(Data) when is_map(Data) ->
    normalize_nested_map(<<"guild">>, fun guild/1, Data);
guild_data(Data) ->
    Data.

-spec member(term()) -> term().
member(Member) when is_map(Member) ->
    Member1 = normalize_nested_map(<<"user">>, fun user/1, Member),
    Member2 = normalize_snowflake_list_field(<<"roles">>, Member1),
    guild_data_normalize_schema:normalize_nullable_int_fields(
        [<<"accent_color">>, <<"profile_flags">>, <<"mention_flags">>],
        Member2
    );
member(Member) ->
    Member.

-spec role(term()) -> term().
role(Role) when is_map(Role) ->
    Role1 = normalize_required_snowflake_field(<<"id">>, Role),
    Role2 = normalize_permission_field(<<"permissions">>, Role1),
    Role3 = guild_data_normalize_schema:normalize_int_fields(
        [<<"color">>, <<"position">>],
        Role2
    ),
    guild_data_normalize_schema:normalize_nullable_int_fields([<<"hoist_position">>], Role3);
role(Role) ->
    Role.

-spec channel(term()) -> term().
channel(Channel) when is_map(Channel) ->
    Channel1 = normalize_required_snowflake_field(<<"id">>, Channel),
    Channel2 = normalize_optional_snowflake_field(<<"guild_id">>, Channel1),
    Channel3 = normalize_nullable_snowflake_fields(
        [<<"owner_id">>, <<"parent_id">>, <<"last_message_id">>],
        Channel2
    ),
    Channel4 = normalize_recipient_list(Channel3),
    Channel5 = normalize_nick_map(Channel4),
    Channel6 = normalize_overwrite_list(Channel5),
    Channel7 = guild_data_normalize_schema:normalize_int_fields(
        [<<"type">>, <<"position">>, <<"content_warning_level">>, <<"rate_limit_per_user">>],
        Channel6
    ),
    guild_data_normalize_schema:normalize_nullable_int_fields(
        [<<"bitrate">>, <<"user_limit">>, <<"voice_connection_limit">>],
        Channel7
    );
channel(Channel) ->
    Channel.

-spec guild(map()) -> map().
guild(Guild) ->
    Guild1 = normalize_required_snowflake_fields([<<"id">>, <<"owner_id">>], Guild),
    Guild2 = normalize_nullable_snowflake_fields(
        [<<"system_channel_id">>, <<"rules_channel_id">>, <<"afk_channel_id">>],
        Guild1
    ),
    Guild3 = normalize_permission_field(<<"permissions">>, Guild2),
    Guild4 = guild_data_normalize_schema:normalize_int_fields(
        [
            <<"system_channel_flags">>,
            <<"afk_timeout">>,
            <<"verification_level">>,
            <<"mfa_level">>,
            <<"nsfw_level">>,
            <<"content_warning_level">>,
            <<"explicit_content_filter">>,
            <<"default_message_notifications">>,
            <<"disabled_operations">>,
            <<"member_count">>,
            <<"online_count">>
        ],
        Guild3
    ),
    guild_data_normalize_schema:normalize_nullable_int_fields(
        [
            <<"banner_width">>,
            <<"banner_height">>,
            <<"splash_width">>,
            <<"splash_height">>,
            <<"embed_splash_width">>,
            <<"embed_splash_height">>
        ],
        Guild4
    ).

-spec user(map()) -> map().
user(User) ->
    user_utils:normalize_user(User).

-spec normalize_overwrite_list(map()) -> map().
normalize_overwrite_list(Channel) ->
    case maps:get(<<"permission_overwrites">>, Channel, undefined) of
        Overwrites when is_list(Overwrites) ->
            Normalized = lists:filtermap(fun normalize_overwrite/1, Overwrites),
            Channel#{<<"permission_overwrites">> => Normalized};
        _ ->
            Channel
    end.

-spec normalize_recipient_list(map()) -> map().
normalize_recipient_list(Channel) ->
    case maps:get(<<"recipients">>, Channel, undefined) of
        Recipients when is_list(Recipients) ->
            Normalized = [user(Recipient) || Recipient <- Recipients, is_map(Recipient)],
            Channel#{<<"recipients">> => Normalized};
        _ ->
            Channel
    end.

-spec normalize_nick_map(map()) -> map().
normalize_nick_map(Channel) ->
    case maps:get(<<"nicks">>, Channel, undefined) of
        Nicks when is_map(Nicks) ->
            Channel#{<<"nicks">> => normalize_snowflake_keyed_map(Nicks)};
        _ ->
            Channel
    end.

-spec normalize_snowflake_keyed_map(map()) -> map().
normalize_snowflake_keyed_map(Map) ->
    maps:fold(fun add_snowflake_keyed_value/3, #{}, Map).

-spec add_snowflake_keyed_value(term(), term(), map()) -> map().
add_snowflake_keyed_value(Key, Value, Acc) ->
    Acc#{snowflake_id:parse(Key) => Value}.

-spec normalize_overwrite(term()) -> false | {true, map()}.
normalize_overwrite(Overwrite) when is_map(Overwrite) ->
    Id = snowflake_id:parse_optional(maps:get(<<"id">>, Overwrite, undefined)),
    Allow = permission_bits:parse_optional(maps:get(<<"allow">>, Overwrite, undefined)),
    Deny = permission_bits:parse_optional(maps:get(<<"deny">>, Overwrite, undefined)),
    Type = overwrite_type(maps:get(<<"type">>, Overwrite, undefined)),
    case {Id, Type, Allow, Deny} of
        {OverwriteId, OverwriteType, AllowBits, DenyBits} when
            is_integer(OverwriteId),
            is_integer(OverwriteType),
            is_integer(AllowBits),
            is_integer(DenyBits)
        ->
            {true, #{
                <<"id">> => OverwriteId,
                <<"type">> => OverwriteType,
                <<"allow">> => AllowBits,
                <<"deny">> => DenyBits
            }};
        _ ->
            false
    end;
normalize_overwrite(_) ->
    false.

-spec overwrite_type(term()) -> 0 | 1 | undefined.
overwrite_type(0) ->
    0;
overwrite_type(1) ->
    1;
overwrite_type(<<"0">>) ->
    0;
overwrite_type(<<"1">>) ->
    1;
overwrite_type("0") ->
    0;
overwrite_type("1") ->
    1;
overwrite_type(_) ->
    undefined.

-spec normalize_nested_map(binary(), fun((map()) -> map()), map()) -> map().
normalize_nested_map(Key, Fun, Map) ->
    case maps:get(Key, Map, undefined) of
        Nested when is_map(Nested) -> Map#{Key => Fun(Nested)};
        _ -> Map
    end.

-spec normalize_required_snowflake_fields([binary()], map()) -> map().
normalize_required_snowflake_fields(Keys, Map) ->
    lists:foldl(fun normalize_required_snowflake_field/2, Map, Keys).

-spec normalize_nullable_snowflake_fields([binary()], map()) -> map().
normalize_nullable_snowflake_fields(Keys, Map) ->
    lists:foldl(fun normalize_nullable_snowflake_field/2, Map, Keys).

-spec normalize_required_snowflake_field(binary(), map()) -> map().
normalize_required_snowflake_field(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> put_normalized_snowflake(Key, Value, Map);
        error -> Map
    end.

-spec normalize_optional_snowflake_field(binary(), map()) -> map().
normalize_optional_snowflake_field(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, null} -> maps:remove(Key, Map);
        {ok, undefined} -> maps:remove(Key, Map);
        {ok, Value} -> put_normalized_snowflake(Key, Value, Map);
        error -> Map
    end.

-spec normalize_nullable_snowflake_field(binary(), map()) -> map().
normalize_nullable_snowflake_field(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, null} -> Map#{Key => null};
        {ok, Value} -> put_normalized_snowflake(Key, Value, Map);
        error -> Map
    end.

-spec put_normalized_snowflake(binary(), term(), map()) -> map().
put_normalized_snowflake(Key, Value, Map) ->
    Map#{Key => snowflake_id:parse(Value)}.

-spec normalize_permission_field(binary(), map()) -> map().
normalize_permission_field(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> put_normalized_permission(Key, Value, Map);
        error -> Map
    end.

-spec put_normalized_permission(binary(), term(), map()) -> map().
put_normalized_permission(Key, Value, Map) ->
    Map#{Key => permission_bits:parse(Value)}.

-spec normalize_snowflake_list_field(binary(), map()) -> map().
normalize_snowflake_list_field(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Map#{Key => snowflake_id:parse_list(Value)};
        error -> Map
    end.
