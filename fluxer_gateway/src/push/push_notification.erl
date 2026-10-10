%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_notification).
-typing([eqwalizer]).

-export([build_notification_title/5, thread_fields/2]).

-define(THREAD_PUSH_KEY, <<"__thread_push">>).

-spec build_notification_title(
    binary(), map(), integer(), binary() | undefined, binary() | undefined
) -> binary().
build_notification_title(AuthorUsername, MessageData, GuildId, GuildName, ChannelName) ->
    ChannelType = maps:get(<<"channel_type">>, MessageData, 1),
    case GuildId of
        0 ->
            format_dm_title(AuthorUsername, ChannelType);
        _ ->
            format_guild_title(
                AuthorUsername, GuildName, channel_label(MessageData, ChannelName)
            )
    end.

-spec channel_label(map(), binary() | undefined) -> binary() | undefined.
channel_label(MessageData, ChannelName) when is_binary(ChannelName) ->
    case thread_parent_name(MessageData) of
        undefined -> ChannelName;
        ParentName -> iolist_to_binary([ChannelName, <<", #">>, ParentName])
    end;
channel_label(_MessageData, ChannelName) ->
    ChannelName.

-spec thread_parent_name(map()) -> binary() | undefined.
thread_parent_name(#{?THREAD_PUSH_KEY := #{<<"parent_name">> := Name}}) when
    is_binary(Name), byte_size(Name) > 0
->
    Name;
thread_parent_name(_MessageData) ->
    undefined.

-spec thread_fields(map(), binary() | undefined) -> map().
thread_fields(
    #{?THREAD_PUSH_KEY := #{<<"parent_id">> := ParentId}} = MessageData, ChannelName
) when
    is_integer(ParentId)
->
    maps:filter(
        fun(_Key, Value) -> is_binary(Value) end,
        #{
            <<"channel_name">> => ChannelName,
            <<"parent_id">> => integer_to_binary(ParentId),
            <<"parent_name">> => thread_parent_name(MessageData)
        }
    );
thread_fields(_MessageData, _ChannelName) ->
    #{}.

-spec format_dm_title(binary(), term()) -> binary().
format_dm_title(AuthorUsername, 3) ->
    iolist_to_binary([AuthorUsername, <<" (Group DM)">>]);
format_dm_title(AuthorUsername, _ChannelType) ->
    AuthorUsername.

-spec format_guild_title(binary(), binary() | undefined, binary() | undefined) -> binary().
format_guild_title(AuthorUsername, undefined, _) ->
    AuthorUsername;
format_guild_title(AuthorUsername, _, undefined) ->
    AuthorUsername;
format_guild_title(AuthorUsername, GName, ChanName) ->
    iolist_to_binary([AuthorUsername, <<" (#">>, ChanName, <<", ">>, GName, <<")">>]).
