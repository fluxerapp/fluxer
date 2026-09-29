%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_message_params).
-typing([eqwalizer]).

-export([context/1, owner_key/1, suppresses_notifications/1]).

-export_type([context/0]).

-define(MAX_GUILD_FEATURES, 64).
-define(MESSAGE_TYPE_DEFAULT, 0).
-define(MESSAGE_TYPE_REPLY, 19).
-define(PUSHABLE_MESSAGE_TYPES, [?MESSAGE_TYPE_DEFAULT, ?MESSAGE_TYPE_REPLY]).
-define(MESSAGE_FLAG_SUPPRESS_NOTIFICATIONS, 4096).

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
    validate_message_type(message_type(Context), Context).

-spec validate_message_type(integer(), context()) -> {ok, context()} | {error, term()}.
validate_message_type(Type, Context) ->
    case lists:member(Type, ?PUSHABLE_MESSAGE_TYPES) of
        true -> validate_notifications(Context);
        false -> {error, {unpushable_message_type, Type}}
    end.

-spec validate_notifications(context()) -> {ok, context()} | {error, term()}.
validate_notifications(#{message_data := MessageData} = Context) ->
    case suppresses_notifications(MessageData) of
        true -> {error, suppressed_notifications};
        false -> {ok, Context}
    end.

-spec suppresses_notifications(map()) -> boolean().
suppresses_notifications(#{<<"flags">> := Flags}) when is_integer(Flags) ->
    Flags band ?MESSAGE_FLAG_SUPPRESS_NOTIFICATIONS =/= 0;
suppresses_notifications(_MessageData) ->
    false.

-spec message_type(context()) -> integer().
message_type(#{message_data := MessageData}) ->
    normalize_message_type(maps:get(<<"type">>, MessageData, ?MESSAGE_TYPE_DEFAULT)).

-spec normalize_message_type(term()) -> integer().
normalize_message_type(Type) when is_integer(Type) ->
    Type;
normalize_message_type(_Type) ->
    ?MESSAGE_TYPE_DEFAULT.

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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

params_with_message_type(Type) ->
    #{
        message_data => #{
            <<"channel_id">> => <<"1472201127385612376">>,
            <<"id">> => <<"1472201127385612377">>,
            <<"type">> => Type
        },
        user_ids => [<<"1474262819227156566">>],
        guild_id => <<"1472200708085309475">>,
        author_id => <<"1472583967301656587">>,
        guild_default_notifications => 0
    }.

context_allows_a_default_message_test() ->
    ?assertMatch({ok, _}, context(params_with_message_type(?MESSAGE_TYPE_DEFAULT))).

context_allows_a_reply_test() ->
    ?assertMatch({ok, _}, context(params_with_message_type(?MESSAGE_TYPE_REPLY))).

context_rejects_a_call_system_message_test() ->
    ?assertEqual(
        {error, {unpushable_message_type, 3}}, context(params_with_message_type(3))
    ).

context_rejects_every_non_authored_message_type_test() ->
    lists:foreach(
        fun(Type) ->
            ?assertEqual(
                {error, {unpushable_message_type, Type}},
                context(params_with_message_type(Type))
            )
        end,
        [1, 2, 3, 4, 5, 6, 7, 99]
    ).

context_treats_a_missing_message_type_as_pushable_test() ->
    Params = params_with_message_type(?MESSAGE_TYPE_DEFAULT),
    MessageData = maps:remove(<<"type">>, maps:get(message_data, Params)),
    ?assertMatch({ok, _}, context(Params#{message_data := MessageData})).

context_treats_a_malformed_message_type_as_pushable_test() ->
    ?assertMatch({ok, _}, context(params_with_message_type(<<"nonsense">>))).

params_with_flags(Flags, ChannelType, GuildId) ->
    Params = params_with_message_type(?MESSAGE_TYPE_DEFAULT),
    MessageData = maps:get(message_data, Params),
    Params#{
        message_data := MessageData#{<<"flags">> => Flags, <<"channel_type">> => ChannelType},
        guild_id := GuildId
    }.

context_rejects_a_silent_dm_test() ->
    ?assertEqual(
        {error, suppressed_notifications}, context(params_with_flags(4096, 1, 0))
    ).

context_rejects_a_silent_group_dm_test() ->
    ?assertEqual(
        {error, suppressed_notifications}, context(params_with_flags(4096, 3, 0))
    ).

context_rejects_a_silent_guild_message_that_mentions_the_recipient_test() ->
    Params = params_with_flags(4096 bor 4, 0, <<"1472200708085309475">>),
    MessageData = maps:get(message_data, Params),
    Mentioned = MessageData#{
        <<"mentions">> => [#{<<"id">> => <<"1474262819227156566">>}],
        <<"mention_everyone">> => true
    },
    ?assertEqual(
        {error, suppressed_notifications}, context(Params#{message_data := Mentioned})
    ).

context_allows_a_message_with_other_flags_test() ->
    ?assertMatch({ok, _}, context(params_with_flags(4 bor 8192, 1, 0))),
    ?assertMatch({ok, _}, context(params_with_flags(0, 3, 0))).

-endif.
