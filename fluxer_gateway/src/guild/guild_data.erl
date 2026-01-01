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

-module(guild_data).

-export([get_guild_data/2]).
-export([get_guild_member/2]).
-export([has_member/2]).
-export([list_guild_members/2]).
-export([get_vanity_url_channel/1]).
-export([get_first_viewable_text_channel/1]).
-export([get_guild_state/2]).
-export([find_everyone_viewable_text_channel/2]).

-type guild_state() :: map().
-type guild_reply(T) :: {reply, T, guild_state()}.
-type guild_data_map() :: map().
-type guild_member() :: map().
-type channel_list() :: [map()].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec get_guild_data(map(), guild_state()) -> guild_reply(map()).
get_guild_data(#{user_id := UserId}, State) ->
    Data = guild_data_map(State),
    case UserId of
        null ->
            GuildData = build_complete_guild_data(Data, State),
            Reply = #{guild_data => GuildData},
            {reply, Reply, State};
        _ ->
            Members = map_utils:ensure_list(maps:get(<<"members">>, Data, [])),
            case member_in_list(UserId, Members) of
                false ->
                    {reply, #{guild_data => null, error_reason => <<"forbidden">>}, State};
                true ->
                    GuildData = build_complete_guild_data(Data, State),
                    {reply, #{guild_data => GuildData}, State}
            end
    end.

-spec get_guild_member(map(), guild_state()) -> guild_reply(map()).
get_guild_member(#{user_id := UserId}, State) ->
    case find_member_by_user_id(UserId, State) of
        undefined ->
            {reply, #{success => false, member_data => null}, State};
        Member ->
            {reply, #{success => true, member_data => Member}, State}
    end.

-spec has_member(map(), guild_state()) -> guild_reply(map()).
has_member(#{user_id := UserId}, State) ->
    case find_member_by_user_id(UserId, State) of
        undefined ->
            {reply, #{has_member => false}, State};
        _ ->
            {reply, #{has_member => true}, State}
    end.

-spec list_guild_members(map(), guild_state()) -> guild_reply(map()).
list_guild_members(#{limit := Limit, offset := Offset}, State) ->
    Data = guild_data_map(State),
    AllMembers = map_utils:ensure_list(maps:get(<<"members">>, Data, [])),
    TotalCount = length(AllMembers),
    PaginatedMembers = paginate_members(AllMembers, Limit, Offset),
    {reply, #{members => PaginatedMembers, total => TotalCount}, State}.

-spec get_vanity_url_channel(guild_state()) -> guild_reply(map()).
get_vanity_url_channel(State) ->
    Channels = channels_from_state(State),
    EveryoneChannelId = find_everyone_viewable_text_channel(Channels, State),
    {reply, #{channel_id => EveryoneChannelId}, State}.

-spec get_first_viewable_text_channel(guild_state()) -> guild_reply(map()).
get_first_viewable_text_channel(State) ->
    Channels = channels_from_state(State),
    EveryoneChannelId = find_everyone_viewable_text_channel(Channels, State),
    {reply, #{channel_id => EveryoneChannelId}, State}.

-spec get_guild_state(integer(), guild_state()) -> map().
get_guild_state(UserId, State) ->
    Data = guild_data_map(State),
    GuildId = map_utils:get_integer(State, id, 0),
    AllChannels = channels_from_data(Data),
    AllMembers = map_utils:ensure_list(maps:get(<<"members">>, Data, [])),
    Member = find_member_by_user_id(UserId, State),
    {ViewableChannels, JoinedAt} = derive_member_view(UserId, Member, State, AllChannels),
    OnlineCount = guild_member_list:get_online_count(State),
    OwnMemberList = case Member of
        undefined -> [];
        M -> [M]
    end,
    #{
        <<"id">> => integer_to_binary(GuildId),
        <<"properties">> => maps:get(<<"guild">>, Data, #{}),
        <<"roles">> => map_utils:ensure_list(maps:get(<<"roles">>, Data, [])),
        <<"channels">> => ViewableChannels,
        <<"emojis">> => maps:get(<<"emojis">>, Data, []),
        <<"stickers">> => maps:get(<<"stickers">>, Data, []),
        <<"members">> => OwnMemberList,
        <<"member_count">> => length(AllMembers),
        <<"online_count">> => OnlineCount,
        <<"presences">> => [],
        <<"voice_states">> => guild_voice:get_voice_states_list(State),
        <<"joined_at">> => JoinedAt
    }.

-spec find_everyone_viewable_text_channel(channel_list(), guild_state()) -> integer() | null.
find_everyone_viewable_text_channel(Channels, State) ->
    GuildId = map_utils:get_integer(State, id, 0),
    Data = guild_data_map(State),
    Roles = map_utils:ensure_list(maps:get(<<"roles">>, Data, [])),
    EveryonePerms = role_permissions_for_id(Roles, GuildId),
    lists:foldl(
        fun(Channel, Acc) ->
            case Acc of
                null ->
                    select_first_viewable(Channel, GuildId, EveryonePerms);
                _ ->
                    Acc
            end
        end,
        null,
        map_utils:ensure_list(Channels)
    ).

find_member_by_user_id(UserId, State) ->
    guild_permissions:find_member_by_user_id(UserId, State).

-spec guild_data_map(guild_state()) -> guild_data_map().
guild_data_map(State) ->
    map_utils:ensure_map(map_utils:get_safe(State, data, #{})).

-spec build_complete_guild_data(guild_data_map(), guild_state()) -> map().
build_complete_guild_data(Data, _State) ->
    GuildProperties = maps:get(<<"guild">>, Data, #{}),
    maps:merge(GuildProperties, #{
        <<"roles">> => map_utils:ensure_list(maps:get(<<"roles">>, Data, [])),
        <<"channels">> => map_utils:ensure_list(maps:get(<<"channels">>, Data, [])),
        <<"emojis">> => map_utils:ensure_list(maps:get(<<"emojis">>, Data, [])),
        <<"stickers">> => map_utils:ensure_list(maps:get(<<"stickers">>, Data, []))
    }).

-spec channels_from_state(guild_state()) -> channel_list().
channels_from_state(State) ->
    Data = guild_data_map(State),
    channels_from_data(Data).

-spec channels_from_data(guild_data_map()) -> channel_list().
channels_from_data(Data) ->
    map_utils:ensure_list(maps:get(<<"channels">>, Data, [])).

-spec member_in_list(integer(), [guild_member()]) -> boolean().
member_in_list(UserId, Members) ->
    lists:any(fun(Member) -> member_matches(UserId, Member) end, Members).

-spec member_matches(integer(), guild_member()) -> boolean().
member_matches(UserId, Member) ->
    MemberUser = map_utils:ensure_map(maps:get(<<"user">>, Member, #{})),
    case map_utils:get_integer(MemberUser, <<"id">>, undefined) of
        undefined -> false;
        Id -> Id =:= UserId
    end.

-spec paginate_members([guild_member()], non_neg_integer(), non_neg_integer()) -> [guild_member()].
paginate_members(Members, Limit, Offset) ->
    case Offset >= length(Members) of
        true ->
            [];
        false ->
            Remaining = lists:nthtail(Offset, Members),
            case length(Remaining) > Limit of
                true -> lists:sublist(Remaining, Limit);
                false -> Remaining
            end
    end.

-spec derive_member_view(integer(), guild_member() | undefined, guild_state(), channel_list()) ->
    {channel_list(), term()}.
derive_member_view(_UserId, undefined, _State, _Channels) ->
    {[], null};
derive_member_view(UserId, Member, State, Channels) ->
    Filtered =
        lists:filter(
            fun(Channel) ->
                ChannelId = map_utils:get_integer(Channel, <<"id">>, undefined),
                case ChannelId of
                    undefined -> false;
                    _ -> guild_permissions:can_view_channel(UserId, ChannelId, Member, State)
                end
            end,
            Channels
        ),
    JoinedAt = maps:get(<<"joined_at">>, Member, null),
    {Filtered, JoinedAt}.

-spec role_permissions_for_id(list(), integer()) -> integer().
role_permissions_for_id(Roles, GuildId) ->
    lists:foldl(
        fun(Role, Acc) ->
            case map_utils:get_integer(Role, <<"id">>, undefined) of
                GuildId -> map_utils:get_integer(Role, <<"permissions">>, 0);
                _ -> Acc
            end
        end,
        0,
        map_utils:ensure_list(Roles)
    ).

-spec select_first_viewable(map(), integer(), integer()) -> integer() | null.
select_first_viewable(Channel, GuildId, BasePerms) ->
    ChannelType = map_utils:get_integer(Channel, <<"type">>, undefined),
    ChannelId = map_utils:get_integer(Channel, <<"id">>, undefined),
    select_first_viewable(ChannelType, ChannelId, Channel, GuildId, BasePerms).

select_first_viewable(0, ChannelId, Channel, GuildId, BasePerms) when is_integer(ChannelId) ->
    case (BasePerms band constants:administrator_permission()) =/= 0 of
        true ->
            ChannelId;
        false ->
            FinalPerms = guild_permissions:apply_channel_overwrites(
                BasePerms, GuildId, [GuildId], Channel, GuildId
            ),
            case (FinalPerms band constants:view_channel_permission()) =/= 0 of
                true -> ChannelId;
                false -> null
            end
    end;
select_first_viewable(_, _, _, _, _) ->
    null.

-ifdef(TEST).

get_guild_data_membership_gate_test() ->
    State = test_state(),
    {reply, Reply1, _} = get_guild_data(#{user_id => 999}, State),
    ?assertEqual(null, maps:get(guild_data, Reply1)),
    ?assertEqual(<<"forbidden">>, maps:get(error_reason, Reply1)),

    {reply, Reply2, _} = get_guild_data(#{user_id => 200}, State),
    Guild = maps:get(guild_data, Reply2),
    ?assertEqual(<<"Fluxer">>, maps:get(<<"name">>, Guild)),
    Roles = maps:get(<<"roles">>, Guild, []),
    ?assert(length(Roles) > 0).

get_guild_state_filters_channels_test() ->
    State = test_state(),
    GuildState = get_guild_state(200, State),
    Channels = maps:get(<<"channels">>, GuildState),
    ?assert(lists:any(fun(Chan) -> maps:get(<<"id">>, Chan) =:= <<"500">> end, Channels)),
    ?assertEqual(<<"2024-01-01T00:00:00Z">>, maps:get(<<"joined_at">>, GuildState)).

find_everyone_viewable_text_channel_test() ->
    State = test_state(),
    Data = guild_data_map(State),
    Channels = maps:get(<<"channels">>, Data),
    ChannelId = find_everyone_viewable_text_channel(Channels, State),
    ?assertEqual(500, ChannelId).

test_state() ->
    GuildId = 100,
    ViewPerm = constants:view_channel_permission(),
    #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"name">> => <<"Fluxer">>},
            <<"roles">> => [
                #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"permissions">> => integer_to_binary(ViewPerm)
                }
            ],
            <<"channels">> => [
                #{
                    <<"id">> => <<"500">>,
                    <<"type">> => 0,
                    <<"permission_overwrites">> => []
                },
                #{
                    <<"id">> => <<"501">>,
                    <<"type">> => 2,
                    <<"permission_overwrites">> => []
                }
            ],
            <<"members">> => [
                #{
                    <<"user">> => #{<<"id">> => <<"200">>},
                    <<"roles">> => [integer_to_binary(GuildId)],
                    <<"joined_at">> => <<"2024-01-01T00:00:00Z">>
                }
            ],
            <<"emojis">> => [],
            <<"stickers">> => []
        }
    }.

-endif.
