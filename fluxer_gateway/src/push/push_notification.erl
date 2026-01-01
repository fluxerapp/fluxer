%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(push_notification).

-export([sanitize_mentions/2, build_notification_title/5, build_notification_payload/10]).

sanitize_mentions(Content, Mentions) ->
    lists:foldl(
        fun(Mention, Acc) ->
            case
                {
                    maps:get(<<"id">>, Mention, undefined),
                    maps:get(<<"username">>, Mention, undefined)
                }
            of
                {undefined, _} ->
                    Acc;
                {_, undefined} ->
                    Acc;
                {Id, Username} ->
                    Pattern = <<"<@", Id/binary, ">">>,
                    Replacement = <<"@", Username/binary>>,
                    binary:replace(Acc, Pattern, Replacement, [global])
            end
        end,
        Content,
        Mentions
    ).

build_notification_title(AuthorUsername, MessageData, GuildId, GuildName, ChannelName) ->
    ChannelType = maps:get(<<"channel_type">>, MessageData, 1),
    case GuildId of
        0 ->
            case ChannelType of
                3 ->
                    iolist_to_binary([AuthorUsername, <<" (Group DM)">>]);
                _ ->
                    AuthorUsername
            end;
        _ ->
            case {ChannelName, GuildName} of
                {undefined, _} ->
                    AuthorUsername;
                {_, undefined} ->
                    AuthorUsername;
                {ChanName, GName} ->
                    iolist_to_binary([
                        AuthorUsername,
                        <<" (#">>,
                        ChanName,
                        <<", ">>,
                        GName,
                        <<")">>
                    ])
            end
    end.

build_notification_payload(
    MessageData,
    GuildId,
    ChannelId,
    MessageId,
    GuildName,
    ChannelName,
    AuthorUsername,
    AuthorAvatarUrl,
    TargetUserId,
    BadgeCount
) ->
    Content = maps:get(<<"content">>, MessageData, <<"">>),
    Mentions = maps:get(<<"mentions">>, MessageData, []),
    SanitizedContent = sanitize_mentions(Content, Mentions),
    ContentPreview =
        case byte_size(SanitizedContent) > 100 of
            true -> binary:part(SanitizedContent, 0, 100);
            false -> SanitizedContent
        end,
    Title = build_notification_title(AuthorUsername, MessageData, GuildId, GuildName, ChannelName),
    BadgeValue = max(0, BadgeCount),
    #{
        <<"title">> => Title,
        <<"body">> => ContentPreview,
        <<"icon">> => AuthorAvatarUrl,
        <<"badge">> => <<"https://fluxerstatic.com/web/apple-touch-icon.png">>,
        <<"data">> =>
            #{
                <<"channel_id">> => integer_to_binary(ChannelId),
                <<"message_id">> => integer_to_binary(MessageId),
                <<"guild_id">> =>
                    case GuildId of
                        0 -> null;
                        _ -> integer_to_binary(GuildId)
                    end,
                <<"url">> =>
                    case GuildId of
                        0 ->
                            iolist_to_binary([
                                <<"/channels/@me/">>,
                                integer_to_binary(ChannelId),
                                <<"/">>,
                                integer_to_binary(MessageId)
                            ]);
                        _ ->
                            iolist_to_binary([
                                <<"/channels/">>,
                                integer_to_binary(GuildId),
                                <<"/">>,
                                integer_to_binary(ChannelId),
                                <<"/">>,
                                integer_to_binary(MessageId)
                            ])
                    end,
                <<"badge_count">> => BadgeValue,
                <<"target_user_id">> => integer_to_binary(TargetUserId)
            }
    }.
