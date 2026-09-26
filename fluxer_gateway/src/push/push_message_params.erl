%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_message_params).
-typing([eqwalizer]).

-export([context/1, owner_key/1]).

-export_type([context/0]).

-define(MAX_GUILD_FEATURES, 64).

-type context() :: #{
    message_data := map(),
    user_ids := [pos_integer()],
    guild_id := non_neg_integer(),
    author_id := pos_integer(),
    channel_id := pos_integer(),
    message_id := pos_integer(),
    guild_default_notifications := integer(),
    guild_name := binary() | undefined,
    channel_name := binary() | undefined,
    role_names := map(),
    user_roles := map(),
    connected_users := map(),
    markdown_context := map(),
    large_guild_metadata := map() | undefined
}.

-spec context(map()) -> {ok, context()} | {error, term()}.
context(Params) ->
    case maps:get(message_data, Params, #{}) of
        MessageData when is_map(MessageData) -> context_from_message_data(Params, MessageData);
        _ -> {error, invalid_message_data}
    end.

-spec context_from_message_data(map(), map()) -> {ok, context()} | {error, term()}.
context_from_message_data(Params, MessageData) ->
    UserIds = user_ids(maps:get(user_ids, Params, [])),
    GuildId = guild_id(maps:get(guild_id, Params, undefined)),
    AuthorId = snowflake_id:parse_maybe(maps:get(author_id, Params, undefined)),
    ChannelId = snowflake_id:parse_maybe(
        maps:get(<<"channel_id">>, MessageData, undefined)
    ),
    MessageId = snowflake_id:parse_maybe(maps:get(<<"id">>, MessageData, undefined)),
    Notifications = push_normalize:notification_level(
        maps:get(guild_default_notifications, Params, undefined)
    ),
    RoleNames = role_names_map(maps:get(role_names, Params, #{})),
    validate(#{
        message_data => MessageData,
        user_ids => UserIds,
        guild_id => GuildId,
        author_id => AuthorId,
        channel_id => ChannelId,
        message_id => MessageId,
        guild_default_notifications => Notifications,
        guild_name => maps:get(guild_name, Params, undefined),
        channel_name => maps:get(channel_name, Params, undefined),
        role_names => RoleNames,
        user_roles => maps:get(user_roles, Params, #{}),
        connected_users => maps:get(connected_users, Params, #{}),
        markdown_context => markdown_context(
            MessageData, GuildId, RoleNames, maps:get(markdown_context, Params, undefined)
        ),
        large_guild_metadata => large_guild_metadata(Params)
    }).

-spec large_guild_metadata(map()) -> map() | undefined.
large_guild_metadata(Params) ->
    MemberCount = maps:get(guild_member_count, Params, undefined),
    Features = maps:get(guild_features, Params, undefined),
    build_large_guild_metadata(MemberCount, Features).

-spec build_large_guild_metadata(term(), term()) -> map() | undefined.
build_large_guild_metadata(MemberCount, Features) when
    is_integer(MemberCount), MemberCount >= 0, is_list(Features)
->
    #{member_count => MemberCount, features => bounded_features(Features, [], 0)};
build_large_guild_metadata(_MemberCount, _Features) ->
    undefined.

-spec bounded_features(term(), [binary()], non_neg_integer()) -> [binary()].
bounded_features(_Features, Acc, ?MAX_GUILD_FEATURES) ->
    lists:reverse(Acc);
bounded_features([Feature | Rest], Acc, Count) when is_binary(Feature) ->
    bounded_features(Rest, [Feature | Acc], Count + 1);
bounded_features([_Feature | Rest], Acc, Count) ->
    bounded_features(Rest, Acc, Count);
bounded_features(_Features, Acc, _Count) ->
    lists:reverse(Acc).

-spec owner_key(map()) -> term().
owner_key(Params) ->
    case user_ids(maps:get(user_ids, Params, [])) of
        [UserId | _] -> UserId;
        [] -> owner_fallback_key(Params)
    end.

-spec validate(map()) -> {ok, context()} | {error, term()}.
validate(#{user_ids := []}) ->
    {error, no_valid_users};
validate(#{guild_id := invalid}) ->
    {error, invalid_guild_id};
validate(#{author_id := undefined}) ->
    {error, invalid_author_id};
validate(#{channel_id := undefined}) ->
    {error, invalid_channel_id};
validate(#{message_id := undefined}) ->
    {error, invalid_message_id};
validate(#{guild_default_notifications := undefined}) ->
    {error, invalid_guild_default_notifications};
validate(Context) ->
    {ok, Context}.

-spec owner_fallback_key(map()) -> term().
owner_fallback_key(Params) ->
    case snowflake_id:parse_maybe(maps:get(author_id, Params, undefined)) of
        undefined -> push_normalize:optional_guild_id(maps:get(guild_id, Params, undefined));
        AuthorId -> AuthorId
    end.

-spec guild_id(term()) -> non_neg_integer() | invalid.
guild_id(undefined) ->
    invalid;
guild_id(null) ->
    invalid;
guild_id(0) ->
    0;
guild_id(<<"0">>) ->
    0;
guild_id(Value) ->
    case snowflake_id:parse_maybe(Value) of
        undefined -> invalid;
        GuildId -> GuildId
    end.

-spec user_ids(term()) -> [pos_integer()].
user_ids(Values) when is_list(Values) ->
    lists:filtermap(fun snowflake_id:filter/1, Values);
user_ids(_) ->
    [].

-spec role_names_map(term()) -> map().
role_names_map(Value) when is_map(Value) ->
    Value;
role_names_map(_) ->
    #{}.

-spec optional_map(term()) -> map().
optional_map(Value) when is_map(Value) ->
    Value;
optional_map(_) ->
    #{}.

-spec markdown_context(map(), term(), map(), term()) -> map().
markdown_context(_MessageData, _GuildId, _RoleNames, RawContext) when
    is_map(RawContext), map_size(RawContext) > 0
->
    RawContext;
markdown_context(MessageData, GuildId, RoleNames, _RawContext) when
    is_integer(GuildId), GuildId >= 0
->
    push_notification_format:build_markdown_context(MessageData, GuildId, RoleNames, #{});
markdown_context(_MessageData, _GuildId, _RoleNames, RawContext) ->
    optional_map(RawContext).
