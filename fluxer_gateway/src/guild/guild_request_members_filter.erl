%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_request_members_filter).
-typing([eqwalizer]).

-export([
    check_permission/5,
    check_request_rate_limit/1,
    check_guild_request_rate_limit/1,
    check_full_list_bot_rate_limit/4,
    is_full_member_list/3,
    enforce_single_guild_for_bots/2,
    dispatch_full_list_rate_limited/4,
    lookup_session_guild/2
]).

-export_type([session_state/0]).

-define(REQUEST_MEMBERS_RATE_LIMIT_TABLE, guild_request_members_rate_limit).
-define(REQUEST_MEMBERS_RATE_LIMIT_WINDOW_MS, 10000).
-define(REQUEST_MEMBERS_RATE_LIMIT_MAX_EVENTS, 12).
-define(REQUEST_MEMBERS_GUILD_RATE_LIMIT_TABLE, guild_request_members_guild_rate_limit).
-define(REQUEST_MEMBERS_GUILD_RATE_LIMIT_WINDOW_MS, 10000).
-define(REQUEST_MEMBERS_GUILD_RATE_LIMIT_MAX_EVENTS, 40).
-define(FULL_LIST_BOT_RATE_LIMIT_TABLE, guild_request_members_bot_full_list_rate_limit).
-define(FULL_LIST_BOT_RATE_LIMIT_WINDOW_MS, 30000).

-type session_state() :: map().

-spec is_full_member_list(binary(), non_neg_integer(), [integer()]) -> boolean().
is_full_member_list(Query, Limit, UserIds) ->
    Query =:= <<>> andalso Limit =:= 0 andalso UserIds =:= [].

-spec enforce_single_guild_for_bots(boolean(), [integer()]) -> ok | {error, atom()}.
enforce_single_guild_for_bots(true, [_, _ | _]) ->
    {error, too_many_guild_ids};
enforce_single_guild_for_bots(_, _) ->
    ok.

-spec check_permission(
    integer(), boolean(), integer(), boolean(), session_state()
) -> ok | {error, atom()}.
check_permission(_UserId, _IsBot, _GuildId, false, _SessionState) ->
    ok;
check_permission(_UserId, true, _GuildId, true, _SessionState) ->
    ok;
check_permission(UserId, false, GuildId, true, SessionState) ->
    check_management_permission_via_guild(UserId, GuildId, SessionState).

-spec check_management_permission_via_guild(
    integer(), integer(), session_state()
) -> ok | {error, atom()}.
check_management_permission_via_guild(UserId, GuildId, SessionState) ->
    case lookup_session_guild(GuildId, SessionState) of
        {ok, GuildPid} ->
            check_management_permission(UserId, GuildPid);
        {error, _} ->
            {error, guild_not_found}
    end.

-spec check_management_permission(integer(), pid()) -> ok | {error, atom()}.
check_management_permission(UserId, GuildPid) ->
    RequiredPermission = request_members_management_permission(),
    PermRequest = #{
        user_id => UserId,
        permission => RequiredPermission,
        channel_id => undefined
    },
    case gen_server:call(GuildPid, {check_permission, PermRequest}, 5000) of
        #{has_permission := true} -> ok;
        #{has_permission := false} -> {error, missing_permission};
        _ -> {error, permission_check_failed}
    end.

-spec request_members_management_permission() -> non_neg_integer().
request_members_management_permission() ->
    constants:manage_roles_permission() bor
        constants:kick_members_permission() bor
        constants:ban_members_permission().

-spec lookup_session_guild(integer(), session_state()) -> {ok, pid()} | {error, not_found}.
lookup_session_guild(GuildId, SessionState) ->
    Guilds = maps:get(guilds, SessionState, #{}),
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, _Ref} when is_pid(Pid) -> {ok, Pid};
        _ -> {error, not_found}
    end.

-spec check_full_list_bot_rate_limit(
    boolean(), boolean(), integer() | undefined, integer()
) -> ok | {rate_limited, non_neg_integer()}.
check_full_list_bot_rate_limit(false, _IsBot, _UserId, _GuildId) ->
    ok;
check_full_list_bot_rate_limit(true, false, _UserId, _GuildId) ->
    ok;
check_full_list_bot_rate_limit(true, true, UserId, GuildId) when
    is_integer(UserId), UserId > 0, is_integer(GuildId), GuildId > 0
->
    case gateway_handler_rate_limit:rate_limits_disabled() of
        true -> ok;
        false -> check_bot_rate_limit_window(UserId, GuildId)
    end;
check_full_list_bot_rate_limit(true, true, _UserId, _GuildId) ->
    ok.

-spec check_bot_rate_limit_window(integer(), integer()) ->
    ok | {rate_limited, non_neg_integer()}.
check_bot_rate_limit_window(UserId, GuildId) ->
    ensure_ets_table(?FULL_LIST_BOT_RATE_LIMIT_TABLE),
    Now = erlang:system_time(millisecond),
    Key = {UserId, GuildId},
    Window = ?FULL_LIST_BOT_RATE_LIMIT_WINDOW_MS,
    case ets:lookup(?FULL_LIST_BOT_RATE_LIMIT_TABLE, Key) of
        [{Key, Last}] when (Now - Last) < Window ->
            {rate_limited, max(0, Window - (Now - Last))};
        _ ->
            ets:insert(?FULL_LIST_BOT_RATE_LIMIT_TABLE, {Key, Now}),
            ok
    end.

-spec check_request_rate_limit(integer() | undefined) -> ok | {error, atom()}.
check_request_rate_limit(UserId) when is_integer(UserId), UserId > 0 ->
    ensure_ets_table(?REQUEST_MEMBERS_RATE_LIMIT_TABLE),
    check_sliding_window(
        ?REQUEST_MEMBERS_RATE_LIMIT_TABLE,
        UserId,
        ?REQUEST_MEMBERS_RATE_LIMIT_WINDOW_MS,
        ?REQUEST_MEMBERS_RATE_LIMIT_MAX_EVENTS
    );
check_request_rate_limit(_) ->
    {error, invalid_session}.

-spec check_guild_request_rate_limit(integer()) -> ok | {error, atom()}.
check_guild_request_rate_limit(GuildId) when is_integer(GuildId), GuildId > 0 ->
    ensure_ets_table(?REQUEST_MEMBERS_GUILD_RATE_LIMIT_TABLE),
    check_sliding_window(
        ?REQUEST_MEMBERS_GUILD_RATE_LIMIT_TABLE,
        GuildId,
        ?REQUEST_MEMBERS_GUILD_RATE_LIMIT_WINDOW_MS,
        ?REQUEST_MEMBERS_GUILD_RATE_LIMIT_MAX_EVENTS
    );
check_guild_request_rate_limit(_) ->
    {error, invalid_guild_id}.

-spec check_sliding_window(atom(), term(), pos_integer(), pos_integer()) ->
    ok | {error, atom()}.
check_sliding_window(Table, Key, WindowMs, MaxEvents) ->
    Now = erlang:system_time(millisecond),
    case ets:lookup(Table, Key) of
        [] ->
            ets:insert(Table, {Key, [Now]}),
            ok;
        [{Key, Timestamps}] ->
            Recent = [T || T <- Timestamps, (Now - T) < WindowMs],
            check_window_count(Table, Key, Now, Recent, MaxEvents)
    end.

-spec check_window_count(
    atom(), term(), integer(), [integer()], pos_integer()
) -> ok | {error, atom()}.
check_window_count(_Table, _Key, _Now, Recent, MaxEvents) when
    length(Recent) >= MaxEvents
->
    {error, rate_limited};
check_window_count(Table, Key, Now, Recent, _MaxEvents) ->
    ets:insert(Table, {Key, [Now | Recent]}),
    ok.

-spec ensure_ets_table(atom()) -> ok.
ensure_ets_table(Name) ->
    case ets:whereis(Name) of
        undefined ->
            create_ets_table(Name);
        _ ->
            ok
    end.

-spec create_ets_table(atom()) -> ok.
create_ets_table(Name) ->
    try
        _ = ets:new(Name, [
            named_table,
            public,
            set,
            {read_concurrency, true},
            {write_concurrency, true}
        ]),
        ok
    catch
        error:badarg -> ok
    end.

-spec dispatch_full_list_rate_limited(
    session_state(), integer(), map(), non_neg_integer()
) -> ok.
dispatch_full_list_rate_limited(SessionState, GuildId, Request, RetryAfterMs) ->
    case maps:get(session_pid, SessionState, undefined) of
        Pid when is_pid(Pid) ->
            Payload = build_rate_limit_payload(GuildId, Request, RetryAfterMs),
            gateway_dispatch_relay:dispatch(Pid, rate_limited, Payload, GuildId),
            ok;
        _ ->
            ok
    end.

-spec build_rate_limit_payload(integer(), map(), non_neg_integer()) -> map().
build_rate_limit_payload(GuildId, Request, RetryAfterMs) ->
    Nonce = maps:get(nonce, Request, null),
    Meta0 = #{<<"guild_id">> => integer_to_binary(GuildId)},
    Meta = add_nonce_to_meta(Meta0, Nonce),
    #{
        <<"opcode">> => constants:opcode_to_num(request_guild_members),
        <<"retry_after">> => RetryAfterMs / 1000,
        <<"meta">> => Meta
    }.

-spec add_nonce_to_meta(map(), term()) -> map().
add_nonce_to_meta(Meta, null) -> Meta;
add_nonce_to_meta(Meta, Nonce) -> Meta#{<<"nonce">> => Nonce}.
