%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_query_handler).
-typing([eqwalizer]).

-export([handle_call/3, call/3]).
-export_type([guild_state/0]).

-ifdef(TEST).
-export([strip_member_alias/1, restore_member_alias/2]).
-endif.

-define(INLINE_MEMBER_QUERY_MAX_IDS, 100).
-define(INFERRED_DEADLINE_MARGIN_MS, 1000).

-type guild_state() :: map().
-type user_id() :: integer().
-type query_fun() :: fun((map(), map()) -> {reply, term(), term()}).

-spec call(pid(), {atom(), map()}, pos_integer()) -> term().
call(GuildPid, {Tag, Request}, Timeout) ->
    Deadline = os:system_time(millisecond) + Timeout,
    MonotonicDeadline = erlang:monotonic_time(millisecond) + Timeout,
    gen_server:call(
        GuildPid,
        {Tag, Request#{deadline => Deadline, deadline_monotonic => MonotonicDeadline}},
        Timeout
    ).

-spec handle_call(term(), gen_server:from(), guild_state()) ->
    {reply, term(), guild_state()}
    | {noreply, guild_state()}.
handle_call(Msg, From, State) ->
    case is_expired(Msg, From) of
        true -> {noreply, State};
        false -> handle_query(Msg, From, State)
    end.

-spec is_expired(term(), gen_server:from()) -> boolean().
is_expired(Msg, From) ->
    case deadline_expired(Msg, From) of
        unknown -> inferred_deadline_expired(Msg);
        Expired -> Expired
    end.

-spec deadline_expired(term(), gen_server:from()) -> boolean() | unknown.
deadline_expired({_Tag, #{deadline_monotonic := Deadline}}, {Caller, _ReplyTag}) when
    is_integer(Deadline)
->
    case gateway_clock_offset:offset(node(Caller)) of
        undefined -> unknown;
        Offset -> erlang:monotonic_time(millisecond) > Deadline - Offset
    end;
deadline_expired(_Msg, _From) ->
    unknown.

-spec inferred_deadline_expired(term()) -> boolean().
inferred_deadline_expired(Msg) when is_tuple(Msg), tuple_size(Msg) > 0 ->
    case caller_timeout_ms(element(1, Msg)) of
        undefined -> false;
        TimeoutMs -> waited_longer_than(TimeoutMs + ?INFERRED_DEADLINE_MARGIN_MS)
    end;
inferred_deadline_expired(_Msg) ->
    false.

-spec waited_longer_than(pos_integer()) -> boolean().
waited_longer_than(Ms) ->
    case guild_mailbox_age:min_age_ms() of
        undefined -> false;
        Age when Age > Ms -> inferred_deadlines_enabled();
        _ -> false
    end.

-spec inferred_deadlines_enabled() -> boolean().
inferred_deadlines_enabled() ->
    application:get_env(fluxer_gateway, guild_query_inferred_deadlines, true) =/= false.

-spec caller_timeout_ms(term()) -> pos_integer() | undefined.
caller_timeout_ms(get_large_guild_metadata) -> 200;
caller_timeout_ms(get_user_counts) -> 2000;
caller_timeout_ms(get_viewer_counts) -> 2000;
caller_timeout_ms(get_channel_member_counts) -> 2000;
caller_timeout_ms(check_permission) -> 5000;
caller_timeout_ms(get_guild_members_batch) -> 5000;
caller_timeout_ms(list_guild_members) -> 10000;
caller_timeout_ms(search_guild_members) -> 10000;
caller_timeout_ms(Tag) -> rpc_caller_timeout_ms(Tag).

-spec rpc_caller_timeout_ms(term()) -> pos_integer() | undefined.
rpc_caller_timeout_ms(get_user_permissions) -> 4000;
rpc_caller_timeout_ms(can_manage_roles) -> 4000;
rpc_caller_timeout_ms(can_manage_role) -> 4000;
rpc_caller_timeout_ms(get_assignable_roles) -> 4000;
rpc_caller_timeout_ms(get_user_max_role_position) -> 4000;
rpc_caller_timeout_ms(get_guild_data) -> 4000;
rpc_caller_timeout_ms(get_guild_auth_context) -> 4000;
rpc_caller_timeout_ms(get_guild_member) -> 4000;
rpc_caller_timeout_ms(has_member) -> 4000;
rpc_caller_timeout_ms(get_members_with_role) -> 4000;
rpc_caller_timeout_ms(check_target_member) -> 4000;
rpc_caller_timeout_ms(list_guild_members_cursor) -> 4000;
rpc_caller_timeout_ms(get_viewable_channels) -> 4000;
rpc_caller_timeout_ms(resolve_channel_mentions) -> 4000;
rpc_caller_timeout_ms(get_vanity_url_channel) -> 4000;
rpc_caller_timeout_ms(get_first_viewable_text_channel) -> 4000;
rpc_caller_timeout_ms(get_category_channel_count) -> 4000;
rpc_caller_timeout_ms(get_channel_count) -> 4000;
rpc_caller_timeout_ms(get_users_to_mention_by_roles) -> 4000;
rpc_caller_timeout_ms(get_users_to_mention_by_user_ids) -> 4000;
rpc_caller_timeout_ms(get_all_users_to_mention) -> 4000;
rpc_caller_timeout_ms(resolve_all_mentions) -> 4000;
rpc_caller_timeout_ms(resolve_mention_sources) -> 4000;
rpc_caller_timeout_ms(resolve_mention_sources_page) -> 4000;
rpc_caller_timeout_ms(_Tag) -> undefined.

-spec handle_query(term(), gen_server:from(), guild_state()) ->
    {reply, term(), guild_state()}
    | {noreply, guild_state()}.
handle_query({get_counts}, _From, State) ->
    handle_get_counts(State);
handle_query({get_user_counts, UserId}, _From, State) when is_integer(UserId) ->
    handle_get_user_counts(UserId, State);
handle_query({get_viewer_counts, #{user_id := UserId}}, _From, State) when is_integer(UserId) ->
    handle_get_user_counts(UserId, State);
handle_query({get_channel_member_counts, Request}, _From, State) when is_map(Request) ->
    handle_get_channel_member_counts(Request, State);
handle_query({get_large_guild_metadata}, _From, State) ->
    handle_get_large_guild_metadata(State);
handle_query(Msg, From, State) ->
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
handle_call_dispatch({check_permission, Request}, _From, State) ->
    handle_check_permission(request_map(Request), State);
handle_call_dispatch({get_user_permissions, Request}, _From, State) ->
    handle_get_user_permissions(request_map(Request), State);
handle_call_dispatch(Msg, From, State) ->
    handle_async_member_query(Msg, From, State).

-spec handle_async_member_query(term(), gen_server:from(), guild_state()) ->
    {reply, term(), guild_state()} | {noreply, guild_state()}.
handle_async_member_query({Tag, Req}, From, State) when is_atom(Tag) ->
    case member_query_fun(Tag) of
        undefined -> handle_call_sync({Tag, Req}, State);
        QueryFun -> member_query(Tag, From, State, request_map(Req), QueryFun)
    end;
handle_async_member_query(Msg, _From, State) ->
    handle_call_sync(Msg, State).

-spec member_query_fun(atom()) -> query_fun() | undefined.
member_query_fun(get_users_to_mention_by_roles) ->
    fun guild_members:get_users_to_mention_by_roles/2;
member_query_fun(get_users_to_mention_by_user_ids) ->
    fun guild_members:get_users_to_mention_by_user_ids/2;
member_query_fun(get_all_users_to_mention) ->
    fun guild_members:get_all_users_to_mention/2;
member_query_fun(resolve_all_mentions) ->
    fun guild_members:resolve_all_mentions/2;
member_query_fun(resolve_mention_sources) ->
    fun guild_members:resolve_mention_sources/2;
member_query_fun(resolve_mention_sources_page) ->
    fun guild_members:resolve_mention_sources_page/2;
member_query_fun(resolve_channel_mentions) ->
    fun guild_members:resolve_channel_mentions/2;
member_query_fun(get_members_with_role) ->
    fun guild_members:get_members_with_role/2;
member_query_fun(get_viewable_channels) ->
    fun guild_members:get_viewable_channels/2;
member_query_fun(_Tag) ->
    undefined.

-spec member_query(atom(), gen_server:from(), guild_state(), map(), query_fun()) ->
    {reply, term(), guild_state()} | {noreply, guild_state()}.
member_query(Tag, From, State, Request, QueryFun) ->
    case bounded_member_query(Tag, Request) of
        true -> inline_member_query(State, Request, QueryFun);
        false -> async_member_query(From, State, Request, QueryFun)
    end.

-spec bounded_member_query(atom(), map()) -> boolean().
bounded_member_query(get_users_to_mention_by_user_ids, Request) ->
    few_ids(maps:get(user_ids, Request, undefined));
bounded_member_query(resolve_all_mentions, Request) ->
    direct_mentions_only(Request);
bounded_member_query(resolve_mention_sources, Request) ->
    direct_mentions_only(Request);
bounded_member_query(resolve_mention_sources_page, Request) ->
    direct_mentions_only(Request);
bounded_member_query(resolve_channel_mentions, Request) ->
    few_ids(maps:get(channel_ids, Request, undefined));
bounded_member_query(get_viewable_channels, _Request) ->
    true;
bounded_member_query(_Tag, _Request) ->
    false.

-spec direct_mentions_only(map()) -> boolean().
direct_mentions_only(Request) ->
    maps:get(mention_everyone, Request, undefined) =:= false andalso
        maps:get(mention_here, Request, undefined) =:= false andalso
        maps:get(role_ids, Request, undefined) =:= [] andalso
        few_ids(maps:get(user_ids, Request, undefined)).

-spec few_ids(term()) -> boolean().
few_ids(Ids) when is_list(Ids) ->
    length(Ids) =< ?INLINE_MEMBER_QUERY_MAX_IDS;
few_ids(_Ids) ->
    false.

-spec inline_member_query(guild_state(), map(), query_fun()) -> {reply, term(), guild_state()}.
inline_member_query(State, Request, QueryFun) ->
    QS = build_query_snapshot(State),
    Reply = safe_reply(fun() ->
        {reply, QueryReply, _} = QueryFun(Request, QS),
        QueryReply
    end),
    {reply, Reply, State}.

-spec async_member_query(gen_server:from(), guild_state(), map(), query_fun()) ->
    {noreply, guild_state()}.
async_member_query(From, State, Request, QueryFun) ->
    {Aliased, QS} = strip_member_alias(build_query_snapshot(State)),
    spawn_async_reply(From, fun() ->
        {reply, Reply, _} = QueryFun(Request, restore_member_alias(Aliased, QS)),
        Reply
    end),
    {noreply, State}.

-spec strip_member_alias(map()) -> {boolean(), map()}.
strip_member_alias(
    #{data := #{<<"members">> := Members, members_normalized := Members} = Data} = QS
) ->
    {true, QS#{data := maps:remove(members_normalized, Data)}};
strip_member_alias(QS) ->
    {false, QS}.

-spec restore_member_alias(boolean(), map()) -> map().
restore_member_alias(true, #{data := #{<<"members">> := Members} = Data} = QS) ->
    QS#{data := Data#{members_normalized => Members}};
restore_member_alias(_Aliased, QS) ->
    QS.

-spec handle_check_permission(map(), guild_state()) -> {reply, map(), guild_state()}.
handle_check_permission(Request, State) ->
    Reply = safe_reply(fun() ->
        #{user_id := UserId, permission := Permission, channel_id := ChannelId} = Request,
        true = is_integer(Permission),
        HasPermission = check_user_permission(UserId, Permission, ChannelId, State),
        #{has_permission => HasPermission}
    end),
    {reply, Reply, State}.

-spec check_user_permission(user_id(), integer(), integer(), guild_state()) -> boolean().
check_user_permission(UserId, Permission, ChannelId, State) ->
    case owner_id(State) =:= UserId of
        true ->
            true;
        false ->
            Perms = guild_permissions:get_member_permissions(UserId, ChannelId, State),
            permission_bits:has(Perms, Permission)
    end.

-spec handle_get_user_permissions(map(), guild_state()) -> {reply, map(), guild_state()}.
handle_get_user_permissions(Request, State) ->
    Reply = safe_reply(fun() ->
        #{user_id := UserId, channel_id := ChannelId} = Request,
        #{permissions => guild_permissions:get_member_permissions(UserId, ChannelId, State)}
    end),
    {reply, Reply, State}.

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
    gen_server:reply(From, safe_reply(ReplyFun)).

-spec safe_reply(fun(() -> term())) -> term().
safe_reply(ReplyFun) ->
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

monotonic_deadline_ignores_the_legacy_wall_clock_test() ->
    From = {self(), make_ref()},
    Now = erlang:monotonic_time(millisecond),
    ?assertNot(
        is_expired({get_data, #{deadline => 0, deadline_monotonic => Now + 5000}}, From)
    ),
    ?assert(
        is_expired(
            {get_data, #{deadline => 9999999999999, deadline_monotonic => Now - 1}}, From
        )
    ),
    ?assertNot(is_expired({get_data, #{deadline => 0}}, From)).

deadline_less_queries_expire_once_every_known_caller_has_given_up_test() ->
    Self = self(),
    Ref = make_ref(),
    spawn(fun() ->
        From = {Self, make_ref()},
        self() ! queued_request,
        ok = guild_mailbox_age:note(),
        Fresh = is_expired({get_large_guild_metadata}, From),
        timer:sleep(1300),
        Future = erlang:monotonic_time(millisecond) + 5000,
        Results = #{
            fresh => Fresh,
            metadata => is_expired({get_large_guild_metadata}, From),
            counts => is_expired({get_user_counts, 1}, From),
            unknown_tag => is_expired({get_sessions}, From),
            explicit_deadline =>
                is_expired({get_large_guild_metadata, #{deadline_monotonic => Future}}, From)
        },
        ok = application:set_env(fluxer_gateway, guild_query_inferred_deadlines, false),
        Disabled = is_expired({get_large_guild_metadata}, From),
        ok = application:unset_env(fluxer_gateway, guild_query_inferred_deadlines),
        Self ! {Ref, Results#{disabled => Disabled}}
    end),
    Results =
        receive
            {Ref, R} -> R
        after 5000 -> error(timeout)
        end,
    ?assertEqual(
        #{
            fresh => false,
            metadata => true,
            counts => false,
            unknown_tag => false,
            explicit_deadline => false,
            disabled => false
        },
        Results
    ).

call_keeps_the_legacy_deadline_and_adds_a_monotonic_deadline_test() ->
    Guild = spawn(fun() ->
        receive
            {'$gen_call', From, {get_data, Request}} -> gen_server:reply(From, Request)
        end
    end),
    WallBefore = os:system_time(millisecond),
    MonotonicBefore = erlang:monotonic_time(millisecond),
    #{deadline := WallDeadline, deadline_monotonic := MonotonicDeadline} =
        call(Guild, {get_data, #{}}, 2000),
    ?assert(WallDeadline >= WallBefore + 2000),
    ?assert(WallDeadline =< os:system_time(millisecond) + 2000),
    ?assert(MonotonicDeadline >= MonotonicBefore + 2000),
    ?assert(MonotonicDeadline =< erlang:monotonic_time(millisecond) + 2000).

-endif.
