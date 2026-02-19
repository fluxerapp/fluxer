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

-module(guild_query_handler).

-export([handle_call/3]).

-type guild_state() :: map().
-type user_id() :: integer().

-spec handle_call(term(), gen_server:from(), guild_state()) ->
    {reply, term(), guild_state()}
    | {noreply, guild_state()}.
handle_call({very_large_guild_prime_member, Member}, _From, State) when is_map(Member) ->
    Data0 = maps:get(data, State, #{}),
    Data = guild_data_index:put_member(Member, Data0),
    {reply, ok, maps:put(data, Data, State)};
handle_call({very_large_guild_prime_member, _}, _From, State) ->
    {reply, ok, State};
handle_call({very_large_guild_get_members, UserIds}, _From, State) when is_list(UserIds) ->
    Data = maps:get(data, State, #{}),
    MemberMap = guild_data_index:member_map(Data),
    Reply = lists:foldl(
        fun(UserId, Acc) ->
            case maps:get(UserId, MemberMap, undefined) of
                Member when is_map(Member) -> maps:put(UserId, Member, Acc);
                _ -> Acc
            end
        end,
        #{},
        UserIds
    ),
    {reply, Reply, State};
handle_call({get_counts}, _From, State) ->
    MemberCount = maps:get(member_count, State, 0),
    OnlineCount = guild_member_list:get_online_count(State),
    GuildId = maps:get(id, State, undefined),
    case is_integer(GuildId) of
        true -> guild_counts_cache:update(GuildId, MemberCount, OnlineCount);
        false -> ok
    end,
    {reply, #{member_count => MemberCount, presence_count => OnlineCount}, State};
handle_call({get_large_guild_metadata}, _From, State) ->
    MemberCount = maps:get(member_count, State, 0),
    Data = maps:get(data, State, #{}),
    Guild = maps:get(<<"guild">>, Data, #{}),
    Features = maps:get(<<"features">>, Guild, []),
    {reply, #{member_count => MemberCount, features => Features}, State};
handle_call({get_users_to_mention_by_roles, Request}, _From, State) ->
    spawn_async_reply(
        _From,
        fun() ->
            {reply, Reply, _} = guild_members:get_users_to_mention_by_roles(Request, State),
            Reply
        end
    ),
    {noreply, State};
handle_call({get_users_to_mention_by_user_ids, Request}, _From, State) ->
    spawn_async_reply(
        _From,
        fun() ->
            {reply, Reply, _} = guild_members:get_users_to_mention_by_user_ids(Request, State),
            Reply
        end
    ),
    {noreply, State};
handle_call({get_all_users_to_mention, Request}, _From, State) ->
    spawn_async_reply(
        _From,
        fun() ->
            {reply, Reply, _} = guild_members:get_all_users_to_mention(Request, State),
            Reply
        end
    ),
    {noreply, State};
handle_call({resolve_all_mentions, Request}, _From, State) ->
    spawn_async_reply(
        _From,
        fun() ->
            {reply, Reply, _} = guild_members:resolve_all_mentions(Request, State),
            Reply
        end
    ),
    {noreply, State};
handle_call({get_members_with_role, Request}, _From, State) ->
    spawn_async_reply(
        _From,
        fun() ->
            {reply, Reply, _} = guild_members:get_members_with_role(Request, State),
            Reply
        end
    ),
    {noreply, State};
handle_call({check_permission, Request}, _From, State) ->
    spawn_async_reply(
        _From,
        fun() ->
            #{user_id := UserId, permission := Permission, channel_id := ChannelId} = Request,
            true = is_integer(Permission),
            HasPermission =
                case owner_id(State) =:= UserId of
                    true ->
                        true;
                    false ->
                        Permissions = guild_permissions:get_member_permissions(
                            UserId, ChannelId, State
                        ),
                        (Permissions band Permission) =:= Permission
                end,
            #{has_permission => HasPermission}
        end
    ),
    {noreply, State};
handle_call({get_user_permissions, Request}, _From, State) ->
    spawn_async_reply(
        _From,
        fun() ->
            #{user_id := UserId, channel_id := ChannelId} = Request,
            Permissions = guild_permissions:get_member_permissions(UserId, ChannelId, State),
            #{permissions => Permissions}
        end
    ),
    {noreply, State};
handle_call({can_manage_roles, Request}, _From, State) ->
    guild_members:can_manage_roles(Request, State);
handle_call({can_manage_role, Request}, _From, State) ->
    guild_members:can_manage_role(Request, State);
handle_call({get_guild_data, Request}, _From, State) ->
    guild_data:get_guild_data(Request, State);
handle_call({get_assignable_roles, Request}, _From, State) ->
    guild_members:get_assignable_roles(Request, State);
handle_call({get_user_max_role_position, Request}, _From, State) ->
    #{user_id := UserId} = Request,
    Position = guild_permissions:get_max_role_position(UserId, State),
    {reply, #{position => Position}, State};
handle_call({check_target_member, Request}, _From, State) ->
    guild_members:check_target_member(Request, State);
handle_call({get_viewable_channels, Request}, _From, State) ->
    spawn_async_reply(
        _From,
        fun() ->
            {reply, Reply, _} = guild_members:get_viewable_channels(Request, State),
            Reply
        end
    ),
    {noreply, State};
handle_call({get_guild_member, Request}, _From, State) ->
    guild_data:get_guild_member(Request, State);
handle_call({has_member, Request}, _From, State) ->
    guild_data:has_member(Request, State);
handle_call({list_guild_members, Request}, _From, State) ->
    guild_data:list_guild_members(Request, State);
handle_call({list_guild_members_cursor, Request}, _From, State) ->
    guild_member_list:get_members_cursor(Request, State);
handle_call({get_vanity_url_channel}, _From, State) ->
    guild_data:get_vanity_url_channel(State);
handle_call({get_first_viewable_text_channel}, _From, State) ->
    guild_data:get_first_viewable_text_channel(State);
handle_call({get_category_channel_count, Request}, _From, State) ->
    #{category_id := CategoryId} = Request,
    Data = maps:get(data, State),
    Channels = maps:get(<<"channels">>, Data, []),
    Count = length([
        Ch
     || Ch <- Channels,
        map_utils:get_integer(Ch, <<"parent_id">>, undefined) =:= CategoryId
    ]),
    {reply, #{count => Count}, State};
handle_call({get_channel_count}, _From, State) ->
    Data = maps:get(data, State),
    Channels = maps:get(<<"channels">>, Data, []),
    Count = length(Channels),
    {reply, #{count => Count}, State};
handle_call({get_sessions}, _From, State) ->
    {reply, State, State};
handle_call({get_push_base_state}, _From, State) ->
    {reply,
        #{
            id => maps:get(id, State, 0),
            data => maps:get(data, State, #{}),
            virtual_channel_access => maps:get(virtual_channel_access, State, #{})
        },
        State};
handle_call({get_cluster_merge_state}, _From, State) ->
    {reply,
        #{
            sessions => maps:get(sessions, State, #{}),
            voice_states => maps:get(voice_states, State, #{}),
            virtual_channel_access => maps:get(virtual_channel_access, State, #{}),
            virtual_channel_access_pending => maps:get(virtual_channel_access_pending, State, #{}),
            virtual_channel_access_preserve => maps:get(virtual_channel_access_preserve, State, #{}),
            virtual_channel_access_move_pending =>
                maps:get(virtual_channel_access_move_pending, State, #{})
        },
        State}.

-spec spawn_async_reply(gen_server:from(), fun(() -> term())) -> ok.
spawn_async_reply(From, ReplyFun) ->
    spawn(fun() ->
        Reply =
            try
                ReplyFun()
            catch
                _:_ ->
                    #{error => async_handler_failed}
            end,
        gen_server:reply(From, Reply)
    end),
    ok.

-spec owner_id(guild_state()) -> user_id().
owner_id(State) ->
    case resolve_data_map(State) of
        undefined ->
            0;
        Data ->
            Guild = maps:get(<<"guild">>, Data, #{}),
            type_conv:to_integer(maps:get(<<"owner_id">>, Guild, <<"0">>))
    end.

-spec resolve_data_map(guild_state() | map()) -> map() | undefined.
resolve_data_map(State) when is_map(State) ->
    case maps:find(data, State) of
        {ok, Data} when is_map(Data) ->
            Data;
        {ok, Data} when is_map(Data) =:= false ->
            undefined;
        error ->
            case State of
                #{<<"members">> := _} ->
                    State;
                _ ->
                    undefined
            end
    end;
resolve_data_map(_) ->
    undefined.
