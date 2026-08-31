%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_connection_guild_resolve).
-typing([eqwalizer]).

-export([do_local_guild_connect/1]).

-export_type([guild_id/0, attempt/0, connect_ctx/0]).

-define(GUILD_MANAGER_START_TIMEOUT_MS, 20000).
-define(GUILD_MANAGER_LOOKUP_FALLBACK_TIMEOUT_MS, 200).
-define(MAX_GUILD_OWNER_REDIRECTS, 3).

-type guild_id() :: session:guild_id().
-type attempt() :: non_neg_integer().

-type connect_ctx() :: #{
    session_pid := pid(),
    guild_id := guild_id(),
    attempt := attempt(),
    session_id := binary(),
    user_id := integer(),
    bot := boolean(),
    is_staff := boolean(),
    initial_guild_id := guild_id() | undefined,
    user_data := map()
}.

-spec do_local_guild_connect(connect_ctx()) ->
    pending | {ok_cached_unavailable, map()} | {error, term()}.
do_local_guild_connect(Ctx) ->
    case fluxer_gateway_sup:role_enabled(guilds) of
        false -> try_remote_fallback(Ctx);
        true -> do_local_lookup(Ctx)
    end.

-spec try_remote_fallback(connect_ctx()) ->
    pending | {ok_cached_unavailable, map()} | {error, term()}.
try_remote_fallback(#{guild_id := GuildId} = Ctx) ->
    case resolve_owner_node(GuildId) of
        {ok, OwnerNode} when OwnerNode =/= node() ->
            do_remote_guild_connect(OwnerNode, Ctx);
        _ ->
            {error, {guild_manager_failed, {error, unavailable}}}
    end.

-spec resolve_owner_node(guild_id()) ->
    {ok, node()} | unavailable.
resolve_owner_node(GuildId) ->
    try gateway_node_router:owner_node_result(GuildId, guilds) of
        {ok, OwnerNode} when is_atom(OwnerNode) ->
            {ok, OwnerNode};
        {error, _Reason} ->
            unavailable
    catch
        error:_Reason -> unavailable;
        exit:_Reason -> unavailable
    end.

-spec do_local_lookup(connect_ctx()) ->
    pending | {ok_cached_unavailable, map()} | {error, term()}.
do_local_lookup(#{guild_id := GuildId} = Ctx) ->
    Timeout = ?GUILD_MANAGER_LOOKUP_FALLBACK_TIMEOUT_MS,
    case guild_manager:lookup(GuildId, Timeout) of
        {ok, GuildPid} ->
            start_connect_async(GuildPid, Ctx);
        {error, {not_owner, OwnerNode}} when is_atom(OwnerNode) ->
            do_remote_guild_connect(OwnerNode, Ctx);
        {error, not_found} ->
            handle_not_found(Ctx);
        Error ->
            {error, {guild_manager_failed, Error}}
    end.

-spec handle_not_found(connect_ctx()) ->
    pending | {ok_cached_unavailable, map()} | {error, term()}.
handle_not_found(#{guild_id := GuildId} = Ctx) ->
    Timeout = ?GUILD_MANAGER_START_TIMEOUT_MS,
    case guild_manager:start_or_lookup(GuildId, Timeout) of
        {ok, GuildPid} when is_pid(GuildPid) ->
            start_connect_async(GuildPid, Ctx);
        {error, {not_owner, OwnerNode}} when is_atom(OwnerNode) ->
            do_remote_guild_connect(OwnerNode, Ctx);
        {error, timeout} ->
            {error, {guild_manager_failed, {error, timeout}}};
        {error, R} ->
            {error, {guild_manager_failed, {error, R}}}
    end.

-spec do_remote_guild_connect(node(), connect_ctx()) ->
    pending | {ok_cached_unavailable, map()} | {error, term()}.
do_remote_guild_connect(OwnerNode, Ctx) ->
    do_remote_guild_connect_loop(
        OwnerNode, Ctx, ?MAX_GUILD_OWNER_REDIRECTS, []
    ).

-spec do_remote_guild_connect_loop(
    node(), connect_ctx(), non_neg_integer(), [node()]
) -> pending | {ok_cached_unavailable, map()} | {error, term()}.
do_remote_guild_connect_loop(
    OwnerNode, Ctx, RedirectsLeft, SeenOwnerNodes
) ->
    case lists:member(OwnerNode, SeenOwnerNodes) of
        true ->
            LoopErr = {owner_redirect_loop, OwnerNode},
            {error, {guild_manager_failed, {error, LoopErr}}};
        false ->
            do_remote_guild_connect_attempt(
                OwnerNode, Ctx, RedirectsLeft, SeenOwnerNodes
            )
    end.

-spec do_remote_guild_connect_attempt(
    node(), connect_ctx(), non_neg_integer(), [node()]
) -> pending | {ok_cached_unavailable, map()} | {error, term()}.
do_remote_guild_connect_attempt(
    OwnerNode,
    #{guild_id := GuildId} = Ctx,
    RedirectsLeft,
    SeenOwnerNodes
) ->
    Timeout = ?GUILD_MANAGER_START_TIMEOUT_MS,
    RemoteStartedAt = gateway_timings:start(),
    Result =
        try
            gen_server:call(
                {guild_manager, OwnerNode},
                {start_or_lookup, GuildId},
                Timeout
            )
        catch
            exit:{timeout, _} -> {error, timeout};
            exit:Reason -> {'EXIT', Reason};
            error:Reason -> {'EXIT', Reason}
        end,
    notify_remote_timing(
        Ctx,
        guild_manager,
        OwnerNode,
        <<"session_connection_guild_resolve:do_remote_guild_connect_attempt/4">>,
        RemoteStartedAt
    ),
    Seen = [OwnerNode | SeenOwnerNodes],
    handle_remote_result(
        Result, Ctx, RedirectsLeft, Seen
    ).

-spec handle_remote_result(
    term(), connect_ctx(), non_neg_integer(), [node()]
) -> pending | {ok_cached_unavailable, map()} | {error, term()}.
handle_remote_result({ok, GuildPid}, Ctx, _, _) when is_pid(GuildPid) ->
    start_connect_async(GuildPid, Ctx);
handle_remote_result({error, {not_owner, Next}}, Ctx, RL, Seen) when
    is_atom(Next), RL > 0
->
    do_remote_guild_connect_loop(Next, Ctx, RL - 1, Seen);
handle_remote_result({error, timeout}, _, _, _) ->
    {error, {guild_manager_failed, {error, timeout}}};
handle_remote_result({error, R}, _, _, _) ->
    {error, {guild_manager_failed, {error, R}}};
handle_remote_result({'EXIT', {timeout, _}}, _, _, _) ->
    {error, {guild_manager_failed, {error, timeout}}};
handle_remote_result({'EXIT', R}, _, _, _) ->
    {error, {guild_manager_failed, {exit, R}}};
handle_remote_result(Other, _, _, _) ->
    {error, {guild_manager_failed, Other}}.

-spec start_connect_async(pid(), connect_ctx()) ->
    pending | {ok_cached_unavailable, map()} | {error, term()}.
start_connect_async(GuildPid, Ctx) ->
    #{guild_id := GuildId, user_data := UserData} = Ctx,
    UnavailCheck =
        session_connection_unavailability:maybe_build_unavailable_response_from_cache(
            GuildId, UserData
        ),
    case UnavailCheck of
        {ok, UnavailableResponse} ->
            {ok_cached_unavailable, UnavailableResponse};
        not_unavailable ->
            do_start_connect_async(GuildPid, Ctx)
    end.

-spec do_start_connect_async(pid(), connect_ctx()) -> pending | {error, term()}.
do_start_connect_async(GuildPid, Ctx) ->
    #{
        guild_id := GuildId,
        attempt := Attempt,
        session_id := SessionId,
        user_id := UserId,
        bot := Bot,
        is_staff := IsStaff,
        initial_guild_id := InitialGuildId,
        session_pid := SessionPid
    } = Ctx,
    ActiveGuilds =
        session_connection_unavailability:build_initial_active_guilds(
            InitialGuildId, GuildId
        ),
    Request = #{
        session_id => SessionId,
        user_id => UserId,
        session_pid => SessionPid,
        bot => Bot,
        is_staff => IsStaff,
        initial_guild_id => InitialGuildId,
        active_guilds => ActiveGuilds
    },
    CastMsg =
        {session_connect_async, #{
            guild_id => GuildId, attempt => Attempt, request => Request
        }},
    send_connect_cast(GuildPid, CastMsg, SessionPid).

-spec send_connect_cast(pid(), term(), pid()) -> pending | {error, term()}.
send_connect_cast(GuildPid, CastMsg, SessionPid) ->
    CastStartedAt = gateway_timings:start(),
    Result = shard_utils:safe_cast(GuildPid, CastMsg),
    notify_remote_timing(
        SessionPid,
        guild,
        node(GuildPid),
        <<"session_connection_guild_resolve:send_connect_cast/3">>,
        CastStartedAt
    ),
    case Result of
        ok -> pending;
        {error, overloaded} -> {error, {guild_manager_failed, {error, overloaded}}}
    end.

-spec notify_remote_timing(connect_ctx() | pid(), term(), node(), binary(), integer()) -> ok.
notify_remote_timing(#{session_pid := SessionPid}, Role, OwnerNode, FunctionName, StartedAt) ->
    notify_remote_timing(SessionPid, Role, OwnerNode, FunctionName, StartedAt);
notify_remote_timing(SessionPid, Role, OwnerNode, FunctionName, StartedAt) when
    is_pid(SessionPid), is_atom(OwnerNode)
->
    _ =
        case gateway_timings:remote_node(Role, OwnerNode) of
            Remote when is_map(Remote) ->
                Timings = gateway_timings:record_function(
                    remote_operation,
                    FunctionName,
                    StartedAt,
                    #{remote => Remote},
                    gateway_timings:new()
                ),
                SessionPid ! {gateway_timing_update, Timings};
            _ ->
                ok
        end,
    ok;
notify_remote_timing(_, _, _, _, _) ->
    ok.
