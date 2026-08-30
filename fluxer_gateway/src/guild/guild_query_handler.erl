%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_query_handler).
-typing([eqwalizer]).

-export([handle_call/3]).
-export_type([guild_state/0]).

-type guild_state() :: map().
-type user_id() :: integer().

-spec handle_call(term(), gen_server:from(), guild_state()) ->
    {reply, term(), guild_state()}
    | {noreply, guild_state()}.
handle_call({get_counts}, _From, State) ->
    handle_get_counts(State);
handle_call({get_user_counts, UserId}, _From, State) when is_integer(UserId) ->
    handle_get_user_counts(UserId, State);
handle_call({get_channel_member_counts, Request}, _From, State) when is_map(Request) ->
    handle_get_channel_member_counts(Request, State);
handle_call({get_large_guild_metadata}, _From, State) ->
    handle_get_large_guild_metadata(State);
handle_call(Msg, From, State) ->
    handle_call_dispatch(Msg, From, State).

-spec handle_get_counts(guild_state()) -> {reply, map(), guild_state()}.
handle_get_counts(State) ->
    MemberCount = maps:get(member_count, State, 0),
    PublicOnlineCount = guild_public_online:compute_count(State),
    ok = maybe_put_guild_count_cache(State, MemberCount, PublicOnlineCount),
    {reply, #{member_count => MemberCount, presence_count => PublicOnlineCount}, State}.

-spec handle_get_user_counts(user_id(), guild_state()) -> {reply, map(), guild_state()}.
handle_get_user_counts(UserId, State) ->
    MemberCount = maps:get(member_count, State, 0),
    OnlineCount = guild_mutual_online:compute_count(UserId, State),
    {reply, #{member_count => MemberCount, online_count => OnlineCount}, State}.

-spec handle_get_channel_member_counts(map(), guild_state()) -> {reply, map(), guild_state()}.
handle_get_channel_member_counts(Request, State) ->
    {Entries, NewState} =
        case request_session_data(Request, State) of
            undefined ->
                {[], State};
            SessionData ->
                channel_member_count_entries(
                    maps:get(channel_ids, Request, []), SessionData, State
                )
        end,
    {reply, #{counts => Entries}, NewState}.

-spec request_session_data(map(), guild_state()) -> map() | undefined.
request_session_data(Request, State) ->
    SessionId = maps:get(session_id, Request, undefined),
    UserId = maps:get(user_id, Request, undefined),
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        #{user_id := UserId} = SessionData when is_integer(UserId), UserId > 0 ->
            SessionData;
        _ ->
            undefined
    end.

-spec channel_member_count_entries([term()], map(), guild_state()) -> {[map()], guild_state()}.
channel_member_count_entries(ChannelIds, SessionData, State) when is_list(ChannelIds) ->
    {Entries, NewState} =
        lists:foldl(
            fun(ChannelId, {Acc, CurrentState}) ->
                channel_member_count_entries_acc(ChannelId, SessionData, Acc, CurrentState)
            end,
            {[], State},
            ChannelIds
        ),
    {lists:reverse(Entries), NewState};
channel_member_count_entries(_, _SessionData, State) ->
    {[], State}.

-spec channel_member_count_entries_acc(term(), map(), [map()], guild_state()) ->
    {[map()], guild_state()}.
channel_member_count_entries_acc(ChannelId, SessionData, Acc, State) ->
    case channel_member_count_entry(ChannelId, SessionData, State) of
        {true, Entry, NextState} -> {[Entry | Acc], NextState};
        false -> {Acc, State}
    end.

-spec channel_member_count_entry(term(), map(), guild_state()) ->
    {true, map(), guild_state()} | false.
channel_member_count_entry(ChannelId, SessionData, State) when
    is_integer(ChannelId), ChannelId > 0
->
    case
        guild_member_list_connected:session_can_view_channel_members(
            SessionData, ChannelId, State
        )
    of
        true -> channel_member_count_entry_for_visible_channel(ChannelId, State);
        false -> false
    end;
channel_member_count_entry(_ChannelId, _SessionData, _State) ->
    false.

-spec channel_member_count_entry_for_visible_channel(integer(), guild_state()) ->
    {true, map(), guild_state()} | false.
channel_member_count_entry_for_visible_channel(ChannelId, State) ->
    case guild_member_list:calculate_list_id(ChannelId, State) of
        undefined ->
            false;
        ListId ->
            NewState = guild_member_list_channel_engine:ensure(ListId, State),
            {MemberCount, OnlineCount} = guild_member_list:get_counts(ListId, NewState),
            {true,
                #{
                    channel_id => ChannelId,
                    member_count => MemberCount,
                    online_count => OnlineCount
                },
                NewState}
    end.

-spec handle_get_large_guild_metadata(guild_state()) -> {reply, map(), guild_state()}.
handle_get_large_guild_metadata(State) ->
    MemberCount = maps:get(member_count, State, 0),
    Data = maps:get(data, State, #{}),
    Guild = maps:get(<<"guild">>, Data, #{}),
    Features = maps:get(<<"features">>, Guild, []),
    {reply, #{member_count => MemberCount, features => Features}, State}.

-spec handle_call_dispatch(term(), gen_server:from(), guild_state()) ->
    {reply, term(), guild_state()} | {noreply, guild_state()}.
handle_call_dispatch({get_users_to_mention_by_roles, Req}, From, State) ->
    async_member_query(
        From, State, request_map(Req), fun guild_members:get_users_to_mention_by_roles/2
    );
handle_call_dispatch({get_users_to_mention_by_user_ids, Req}, From, State) ->
    handle_async_member_query({get_users_to_mention_by_user_ids, Req}, From, State);
handle_call_dispatch({check_permission, Request}, From, State) ->
    handle_check_permission(request_map(Request), From, State);
handle_call_dispatch({get_user_permissions, Request}, From, State) ->
    handle_get_user_permissions(request_map(Request), From, State);
handle_call_dispatch(Msg, From, State) ->
    handle_async_member_query(Msg, From, State).

-spec handle_async_member_query(term(), gen_server:from(), guild_state()) ->
    {reply, term(), guild_state()} | {noreply, guild_state()}.
handle_async_member_query({get_users_to_mention_by_user_ids, Req}, From, State) ->
    async_member_query(
        From, State, request_map(Req), fun guild_members:get_users_to_mention_by_user_ids/2
    );
handle_async_member_query({get_all_users_to_mention, Req}, From, State) ->
    async_member_query(
        From, State, request_map(Req), fun guild_members:get_all_users_to_mention/2
    );
handle_async_member_query({resolve_all_mentions, Req}, From, State) ->
    async_member_query(From, State, request_map(Req), fun guild_members:resolve_all_mentions/2);
handle_async_member_query({resolve_mention_sources, Req}, From, State) ->
    async_member_query(
        From, State, request_map(Req), fun guild_members:resolve_mention_sources/2
    );
handle_async_member_query({resolve_mention_sources_page, Req}, From, State) ->
    async_member_query(
        From, State, request_map(Req), fun guild_members:resolve_mention_sources_page/2
    );
handle_async_member_query({resolve_channel_mentions, Req}, From, State) ->
    async_member_query(
        From, State, request_map(Req), fun guild_members:resolve_channel_mentions/2
    );
handle_async_member_query({get_members_with_role, Req}, From, State) ->
    async_member_query(
        From, State, request_map(Req), fun guild_members:get_members_with_role/2
    );
handle_async_member_query({get_viewable_channels, Req}, From, State) ->
    async_member_query(
        From, State, request_map(Req), fun guild_members:get_viewable_channels/2
    );
handle_async_member_query(Msg, _From, State) ->
    handle_call_sync(Msg, State).

-spec async_member_query(
    gen_server:from(), guild_state(), map(), fun((map(), map()) -> {reply, term(), term()})
) ->
    {noreply, guild_state()}.
async_member_query(From, State, Request, QueryFun) ->
    QS = build_query_snapshot(State),
    spawn_async_reply(From, fun() ->
        {reply, Reply, _} = QueryFun(Request, QS),
        Reply
    end),
    {noreply, State}.

-spec handle_check_permission(map(), gen_server:from(), guild_state()) ->
    {noreply, guild_state()}.
handle_check_permission(Request, From, State) ->
    QS = build_query_snapshot(State),
    spawn_async_reply(From, fun() ->
        #{user_id := UserId, permission := Permission, channel_id := ChannelId} = Request,
        true = is_integer(Permission),
        HasPermission = check_user_permission(UserId, Permission, ChannelId, QS),
        #{has_permission => HasPermission}
    end),
    {noreply, State}.

-spec check_user_permission(user_id(), integer(), integer(), map()) -> boolean().
check_user_permission(UserId, Permission, ChannelId, QS) ->
    case owner_id(QS) =:= UserId of
        true ->
            true;
        false ->
            Perms = guild_permissions:get_member_permissions(UserId, ChannelId, QS),
            permission_bits:has(Perms, Permission)
    end.

-spec handle_get_user_permissions(map(), gen_server:from(), guild_state()) ->
    {noreply, guild_state()}.
handle_get_user_permissions(Request, From, State) ->
    QS = build_query_snapshot(State),
    spawn_async_reply(From, fun() ->
        #{user_id := UserId, channel_id := ChannelId} = Request,
        #{permissions => guild_permissions:get_member_permissions(UserId, ChannelId, QS)}
    end),
    {noreply, State}.

-spec handle_call_sync(term(), guild_state()) -> {reply, term(), guild_state()}.
handle_call_sync({can_manage_roles, Req}, State) ->
    guild_members:can_manage_roles(request_map(Req), State);
handle_call_sync({can_manage_role, Req}, State) ->
    guild_members:can_manage_role(request_map(Req), State);
handle_call_sync({get_assignable_roles, Req}, State) ->
    guild_members:get_assignable_roles(request_map(Req), State);
handle_call_sync({get_user_max_role_position, Req}, State) ->
    handle_max_role_position(request_map(Req), State);
handle_call_sync({check_target_member, Req}, State) ->
    guild_members:check_target_member(request_map(Req), State);
handle_call_sync(Msg, State) ->
    handle_call_data(Msg, State).

-spec handle_max_role_position(map(), guild_state()) -> {reply, map(), guild_state()}.
handle_max_role_position(#{user_id := UserId}, State) ->
    Position = guild_permissions:get_max_role_position(UserId, State),
    {reply, #{position => Position}, State}.

-spec handle_call_data(term(), guild_state()) -> {reply, term(), guild_state()}.
handle_call_data({get_guild_data, Req}, State) ->
    guild_data:get_guild_data(request_map(Req), State);
handle_call_data({get_guild_auth_context, Req}, State) ->
    guild_data:get_auth_context(request_map(Req), State);
handle_call_data({get_guild_member, Req}, State) ->
    guild_data:get_guild_member(request_map(Req), State);
handle_call_data({get_guild_members_batch, Req}, State) ->
    guild_data:get_guild_members_batch(request_map(Req), State);
handle_call_data({has_member, Req}, State) ->
    guild_data:has_member(request_map(Req), State);
handle_call_data({list_guild_members, Req}, State) ->
    guild_data:list_guild_members(request_map(Req), State);
handle_call_data({search_guild_members, Req}, State) ->
    guild_data:search_guild_members(request_map(Req), State);
handle_call_data({list_guild_members_cursor, Req}, State) ->
    guild_member_list:get_members_cursor(request_map(Req), State);
handle_call_data({get_vanity_url_channel}, State) ->
    guild_data:get_vanity_url_channel(State);
handle_call_data({get_first_viewable_text_channel}, State) ->
    guild_data:get_first_viewable_text_channel(State);
handle_call_data({get_category_channel_count, Req}, State) ->
    handle_category_channel_count(request_map(Req), State);
handle_call_data({get_channel_count}, State) ->
    handle_channel_count(State);
handle_call_data({get_sessions}, State) ->
    {reply, State, State};
handle_call_data({get_push_base_state}, State) ->
    {reply, build_push_base_state(State), State};
handle_call_data({get_cluster_merge_state}, State) ->
    {reply, build_cluster_merge_state(State), State}.

-spec handle_category_channel_count(map(), guild_state()) -> {reply, map(), guild_state()}.
handle_category_channel_count(Request, State) ->
    #{category_id := CategoryId} = Request,
    Data = maps:get(data, State),
    Channels = maps:get(<<"channels">>, Data, []),
    Count = length([
        Ch
     || Ch <- Channels,
        snowflake_id:parse_optional(maps:get(<<"parent_id">>, Ch, undefined)) =:=
            CategoryId
    ]),
    {reply, #{count => Count}, State}.

-spec handle_channel_count(guild_state()) -> {reply, map(), guild_state()}.
handle_channel_count(State) ->
    Data = maps:get(data, State),
    Channels = maps:get(<<"channels">>, Data, []),
    {reply, #{count => length(Channels)}, State}.

-spec request_map(term()) -> map().
request_map(Request) when is_map(Request) ->
    Request;
request_map(Request) ->
    erlang:error({bad_request, Request}).

-spec build_push_base_state(guild_state()) -> map().
build_push_base_state(State) ->
    #{
        id => maps:get(id, State, undefined),
        data => maps:get(data, State, #{}),
        virtual_channel_access => maps:get(virtual_channel_access, State, #{})
    }.

-spec build_cluster_merge_state(guild_state()) -> map().
build_cluster_merge_state(State) ->
    #{
        sessions => maps:get(sessions, State, #{}),
        voice_states => maps:get(voice_states, State, #{}),
        virtual_channel_access => maps:get(virtual_channel_access, State, #{}),
        virtual_channel_access_pending => maps:get(virtual_channel_access_pending, State, #{}),
        virtual_channel_access_preserve => maps:get(
            virtual_channel_access_preserve, State, #{}
        ),
        virtual_channel_access_move_pending =>
            maps:get(virtual_channel_access_move_pending, State, #{})
    }.

-spec spawn_async_reply(gen_server:from(), fun(() -> term())) -> ok.
spawn_async_reply(From, ReplyFun) ->
    proc_lib:spawn(fun() -> send_async_reply(From, ReplyFun) end),
    ok.

-spec send_async_reply(gen_server:from(), fun(() -> term())) -> ok.
send_async_reply(From, ReplyFun) ->
    gen_server:reply(From, safe_async_reply(ReplyFun)).

-spec safe_async_reply(fun(() -> term())) -> term().
safe_async_reply(ReplyFun) ->
    try
        ReplyFun()
    catch
        _:_ ->
            #{error => async_handler_failed}
    end.

-spec build_query_snapshot(guild_state()) -> map().
build_query_snapshot(State) ->
    maps:with([id, data, sessions, virtual_channel_access], State).

-spec maybe_put_guild_count_cache(guild_state(), non_neg_integer(), non_neg_integer()) -> ok.
maybe_put_guild_count_cache(State, MemberCount, OnlineCount) ->
    case
        {
            maps:get(disable_guild_count_cache_updates, State, false),
            maps:get(id, State, undefined)
        }
    of
        {true, _} ->
            ok;
        {false, GuildId} when is_integer(GuildId) ->
            guild_counts_cache:update(GuildId, MemberCount, OnlineCount);
        _ ->
            ok
    end.

-spec owner_id(guild_state()) -> user_id() | undefined.
owner_id(State) ->
    case resolve_data_map(State) of
        undefined ->
            undefined;
        Data ->
            Guild = maps:get(<<"guild">>, Data, #{}),
            snowflake_id:parse_optional(maps:get(<<"owner_id">>, Guild, undefined))
    end.

-spec resolve_data_map(map()) -> map() | undefined.
resolve_data_map(State) when is_map(State) ->
    case maps:find(data, State) of
        {ok, Data} when is_map(Data) ->
            Data;
        {ok, _Data} ->
            undefined;
        error ->
            resolve_data_payload(State)
    end.

-spec resolve_data_payload(map()) -> map() | undefined.
resolve_data_payload(#{<<"members">> := _} = State) ->
    State;
resolve_data_payload(_State) ->
    undefined.
