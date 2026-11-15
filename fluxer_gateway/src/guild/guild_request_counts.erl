%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_request_counts).

-typing([eqwalizer]).

-export([handle_request/3]).

-export_type([session_state/0]).

-define(MAX_GUILD_IDS, 100).
-define(GUILD_CALL_TIMEOUT_MS, 2000).
-define(BATCH_OVERALL_TIMEOUT_MS, 3000).
-define(MAX_NONCE_BYTES, 64).

-type session_state() :: map().

-spec handle_request(map(), pid(), session_state()) -> ok.
handle_request(Data, _SocketPid, SessionState) when is_map(Data) ->
    SessionPid = maps:get(session_pid, SessionState, undefined),
    UserId = snowflake_id:parse_maybe(maps:get(user_id, SessionState, undefined)),
    case is_pid(SessionPid) andalso is_integer(UserId) of
        false ->
            ok;
        true ->
            GuildIds = parse_guild_ids(maps:get(<<"guild_ids">>, Data, [])),
            Nonce = parse_nonce(maps:get(<<"nonce">>, Data, undefined)),
            Guilds = session_visits:member_guilds(SessionState),
            Targets = build_targets(GuildIds, Guilds),
            Entries = parallel_fetch(Targets, UserId),
            dispatch_counts(SessionPid, Entries, Nonce),
            ok
    end;
handle_request(_, _, _) ->
    ok.

-spec parse_guild_ids(term()) -> [integer()].
parse_guild_ids(GuildIds) when is_list(GuildIds) ->
    Parsed = lists:filtermap(
        fun snowflake_id:filter/1,
        GuildIds
    ),
    lists:sublist(lists:usort(Parsed), ?MAX_GUILD_IDS);
parse_guild_ids(_) ->
    [].

-spec build_targets([integer()], map()) -> [{integer(), pid()}].
build_targets(GuildIds, Guilds) ->
    lists:filtermap(
        fun(GuildId) ->
            target_guild(GuildId, Guilds)
        end,
        GuildIds
    ).

-spec target_guild(integer(), map()) -> {true, {integer(), pid()}} | false.
target_guild(GuildId, Guilds) ->
    case lookup_guild_pid(GuildId, Guilds) of
        {ok, GuildPid} -> {true, {GuildId, GuildPid}};
        error -> false
    end.

-spec lookup_guild_pid(integer(), map()) -> {ok, pid()} | error.
lookup_guild_pid(GuildId, Guilds) ->
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, _Ref} when is_pid(Pid) -> {ok, Pid};
        _ -> error
    end.

-spec parallel_fetch([{integer(), pid()}], integer()) -> [map()].
parallel_fetch([], _UserId) ->
    [];
parallel_fetch(Targets, UserId) ->
    Self = self(),
    Tag = make_ref(),
    Pending = lists:foldl(
        fun({GuildId, GuildPid}, Acc) ->
            spawn_fetch_worker(Self, Tag, GuildId, GuildPid, UserId),
            Acc + 1
        end,
        0,
        Targets
    ),
    Deadline = erlang:monotonic_time(millisecond) + ?BATCH_OVERALL_TIMEOUT_MS,
    collect_responses(Pending, Tag, Deadline, []).

-spec spawn_fetch_worker(pid(), reference(), integer(), pid(), integer()) -> pid().
spawn_fetch_worker(Self, Tag, GuildId, GuildPid, UserId) ->
    spawn(fun() -> worker(Self, Tag, GuildId, GuildPid, UserId) end).

-spec worker(pid(), reference(), integer(), pid(), integer()) -> ok.
worker(Parent, Tag, GuildId, GuildPid, UserId) ->
    Parent ! {Tag, GuildId, fetch_counts(GuildPid, UserId)},
    ok.

-spec fetch_counts(pid(), integer()) -> {ok, non_neg_integer(), non_neg_integer()} | error.
fetch_counts(GuildPid, UserId) ->
    fetch_viewer_counts(GuildPid, UserId).

-spec fetch_viewer_counts(pid(), integer()) ->
    {ok, non_neg_integer(), non_neg_integer()} | error.
fetch_viewer_counts(GuildPid, UserId) ->
    Request = {get_viewer_counts, #{user_id => UserId}},
    try guild_query_handler:call(GuildPid, Request, ?GUILD_CALL_TIMEOUT_MS) of
        Reply ->
            counts_result(Reply)
    catch
        _:_ -> error
    end.

-spec counts_result(term()) -> {ok, non_neg_integer(), non_neg_integer()} | error.
counts_result(#{member_count := MemberCount, online_count := OnlineCount}) when
    is_integer(MemberCount), MemberCount >= 0, is_integer(OnlineCount), OnlineCount >= 0
->
    {ok, MemberCount, OnlineCount};
counts_result(_) ->
    error.

-spec collect_responses(non_neg_integer(), reference(), integer(), [map()]) -> [map()].
collect_responses(0, _Tag, _Deadline, Acc) ->
    lists:reverse(Acc);
collect_responses(Pending, Tag, Deadline, Acc) ->
    Now = erlang:monotonic_time(millisecond),
    Remaining = max(0, Deadline - Now),
    receive
        {Tag, _GuildId, error} ->
            collect_responses(Pending - 1, Tag, Deadline, Acc);
        {Tag, GuildId, {ok, MemberCount, OnlineCount}} ->
            Entry = build_entry(GuildId, MemberCount, OnlineCount),
            collect_responses(Pending - 1, Tag, Deadline, [Entry | Acc])
    after Remaining ->
        lists:reverse(Acc)
    end.

-spec build_entry(integer(), non_neg_integer(), non_neg_integer()) -> map().
build_entry(GuildId, MemberCount, OnlineCount) ->
    #{
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"member_count">> => MemberCount,
        <<"online_count">> => OnlineCount
    }.

-spec dispatch_counts(pid(), [map()], binary() | undefined) -> ok.
dispatch_counts(SessionPid, Entries, Nonce) ->
    Base = #{<<"counts">> => Entries},
    Payload =
        case Nonce of
            undefined -> Base;
            _ -> Base#{<<"nonce">> => Nonce}
        end,
    gateway_dispatch_relay:dispatch(SessionPid, guild_counts_update, Payload),
    ok.

-spec parse_nonce(term()) -> binary() | undefined.
parse_nonce(Nonce) when
    is_binary(Nonce), byte_size(Nonce) > 0, byte_size(Nonce) =< ?MAX_NONCE_BYTES
->
    Nonce;
parse_nonce(_) ->
    undefined.
