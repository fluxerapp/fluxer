%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_request_forum_unreads).
-typing([eqwalizer]).

-export([handle_request/3, scope/3]).

-define(MAX_THREADS, 40).
-define(MAX_COUNT, 25).
-define(GUILD_CALL_TIMEOUT_MS, 2000).

-type request_context() :: #{
    guild_id := integer(), channel_id := integer(), user_id := integer()
}.
-type thread_request() :: {integer(), integer() | undefined}.
-type thread_scope() :: {stored, integer() | undefined} | unstored.

-spec handle_request(map(), pid(), map()) -> ok.
handle_request(Data, SocketPid, SessionState) ->
    UserId = snowflake_id:parse_maybe(maps:get(user_id, SessionState, undefined)),
    case session_viewer(UserId, SessionState) of
        false ->
            SocketPid ! forum_unreads_unknown_opcode,
            ok;
        true ->
            handle_viewer_request(Data, UserId, SessionState)
    end.

-spec session_viewer(integer() | undefined, map()) -> boolean().
session_viewer(UserId, SessionState) ->
    Bot = type_conv:is_true(maps:get(bot, SessionState, false)),
    Capable = type_conv:is_true(maps:get(thread_channels_capable, SessionState, false)),
    case channel_threads_config:loaded() of
        false -> (Bot orelse Capable) andalso is_integer(UserId);
        true -> guild_thread_gate:compute_viewer(true, Bot, Capable, UserId)
    end.

-spec handle_viewer_request(map(), integer() | undefined, map()) -> ok.
handle_viewer_request(Data, UserId, SessionState) ->
    GuildId = snowflake_id:parse_maybe(maps:get(<<"guild_id">>, Data, undefined)),
    ChannelId = snowflake_id:parse_maybe(maps:get(<<"channel_id">>, Data, undefined)),
    SessionPid = maps:get(session_pid, SessionState, undefined),
    SessionId = maps:get(session_id, SessionState, undefined),
    Parsed =
        case
            {GuildId, ChannelId, parse_threads(maps:get(<<"threads">>, Data, undefined)),
                SessionPid}
        of
            {ParsedGuildId, ParsedChannelId, [_ | _] = ParsedThreads, ParsedPid} when
                is_integer(ParsedGuildId),
                is_integer(ParsedChannelId),
                is_integer(UserId),
                is_pid(ParsedPid),
                is_binary(SessionId)
            ->
                {
                    #{
                        guild_id => ParsedGuildId,
                        channel_id => ParsedChannelId,
                        user_id => UserId
                    },
                    ParsedThreads,
                    ParsedPid,
                    SessionId
                };
            _ ->
                undefined
        end,
    maybe
        {#{guild_id := G, channel_id := C, user_id := U} = Context, Threads, Pid, S} ?= Parsed,
        Request = #{
            session_id => S,
            user_id => U,
            channel_id => C,
            thread_ids => [ThreadId || {ThreadId, _Ack} <- Threads]
        },
        Guilds = session_visits:member_guilds(SessionState),
        {ok, Scopes} ?= guild_scope(maps:get(G, Guilds, undefined), Request),
        Entries = resolve(Context, Threads, Scopes),
        dispatch(Pid, Context, Entries)
    else
        _ -> ok
    end.

-spec parse_threads(term()) -> [thread_request()].
parse_threads(Threads) when is_list(Threads) ->
    take_threads(Threads, #{}, []);
parse_threads(_Threads) ->
    [].

-spec take_threads([term()], #{integer() => true}, [thread_request()]) -> [thread_request()].
take_threads(_Rest, _Seen, Acc) when length(Acc) >= ?MAX_THREADS ->
    lists:reverse(Acc);
take_threads([], _Seen, Acc) ->
    lists:reverse(Acc);
take_threads([#{<<"thread_id">> := RawId} = Entry | Rest], Seen, Acc) ->
    case snowflake_id:parse_maybe(RawId) of
        ThreadId when is_integer(ThreadId), not is_map_key(ThreadId, Seen) ->
            Ack = snowflake_id:parse_maybe(maps:get(<<"ack_message_id">>, Entry, undefined)),
            take_threads(Rest, Seen#{ThreadId => true}, [{ThreadId, Ack} | Acc]);
        _ ->
            take_threads(Rest, Seen, Acc)
    end;
take_threads([_ | Rest], Seen, Acc) ->
    take_threads(Rest, Seen, Acc).

-spec guild_scope(term(), map()) -> {ok, map()} | denied.
guild_scope({GuildPid, _Ref}, Request) when is_pid(GuildPid) ->
    try
        guild_query_handler:call(
            GuildPid, {get_forum_unread_scope, Request}, ?GUILD_CALL_TIMEOUT_MS
        )
    of
        {ok, Scopes} when is_map(Scopes) -> {ok, Scopes};
        _ -> denied
    catch
        exit:{noproc, _} ->
            denied;
        exit:{timeout, _} ->
            denied;
        Class:Reason:Stacktrace ->
            logger:warning(
                "guild_forum_unread_scope_failed: guild_pid=~p request=~p "
                "class=~p reason=~p stack=~p",
                [GuildPid, Request, Class, Reason, Stacktrace]
            ),
            denied
    end;
guild_scope(_Guild, _Request) ->
    denied.

-spec scope(map() | undefined, map(), map()) ->
    {ok, #{integer() => {stored, integer() | undefined} | unstored}} | denied.
scope(undefined, _Request, _State) ->
    denied;
scope(Session, #{channel_id := ForumId, thread_ids := ThreadIds}, State) when
    is_integer(ForumId), is_list(ThreadIds)
->
    case
        guild_thread_gate:session_viewer(Session) andalso is_forum(ForumId, State) andalso
            guild_member_list_connected:session_can_view_channel(Session, ForumId, State)
    of
        true -> {ok, thread_scopes(ForumId, ThreadIds, State)};
        false -> denied
    end;
scope(_Session, _Request, _State) ->
    denied.

-spec is_forum(integer(), map()) -> boolean().
is_forum(ChannelId, State) ->
    case
        maps:get(
            ChannelId,
            guild_data_index_channels:channel_index(maps:get(data, State)),
            undefined
        )
    of
        #{<<"type">> := Type} -> channel_types:is_thread_only(Type);
        _ -> false
    end.

-spec thread_scopes(integer(), [term()], map()) -> #{integer() => thread_scope()}.
thread_scopes(ForumId, ThreadIds, State) ->
    maps:from_list([
        {ThreadId, Scope}
     || ThreadId <- ThreadIds,
        is_integer(ThreadId),
        Scope <- [thread_scope(ThreadId, ForumId, State)],
        Scope =/= foreign
    ]).

-spec thread_scope(integer(), integer(), map()) -> thread_scope() | foreign.
thread_scope(ThreadId, ForumId, State) ->
    Thread = guild_thread_gate:thread(ThreadId, State),
    ParentId =
        case Thread of
            undefined -> undefined;
            _ -> snowflake_id:parse_maybe(maps:get(<<"parent_id">>, Thread, undefined))
        end,
    case {Thread, ParentId} of
        {undefined, _} ->
            unstored;
        {_, ForumId} ->
            {stored,
                snowflake_id:parse_maybe(maps:get(<<"last_message_id">>, Thread, undefined))};
        _ ->
            foreign
    end.

-spec resolve(request_context(), [thread_request()], #{
    integer() => thread_scope()
}) ->
    [map()].
resolve(Context, Threads, Scopes) ->
    Local = [
        {ThreadId, local_entry(Ack, maps:get(ThreadId, Scopes))}
     || {ThreadId, Ack} <- Threads, is_map_key(ThreadId, Scopes)
    ],
    Remote = remote_entries(
        Context, [{ThreadId, Ack} || {ThreadId, {remote, Ack}} <- Local]
    ),
    lists:flatmap(
        fun
            ({ThreadId, {remote, _Ack}}) ->
                [Entry || {ok, Entry} <- [maps:find(ThreadId, Remote)]];
            ({ThreadId, Fields}) ->
                [Fields#{<<"thread_id">> => integer_to_binary(ThreadId)}]
        end,
        Local
    ).

-spec local_entry(integer() | undefined, thread_scope()) ->
    map() | {remote, integer() | undefined}.
local_entry(undefined, {stored, _LastMessageId}) ->
    #{<<"missing">> => true};
local_entry(Ack, {stored, LastMessageId}) when
    is_integer(Ack) andalso (LastMessageId =:= undefined orelse LastMessageId =< Ack)
->
    #{<<"count">> => 0};
local_entry(Ack, _Scope) ->
    {remote, Ack}.

-spec remote_entries(request_context(), [thread_request()]) ->
    #{integer() => map()}.
remote_entries(_Context, []) ->
    #{};
remote_entries(#{guild_id := GuildId, channel_id := ChannelId, user_id := UserId}, Threads) ->
    Request = #{
        <<"type">> => <<"forum_unreads">>,
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"user_id">> => integer_to_binary(UserId),
        <<"threads">> => [thread_rpc_entry(ThreadId, Ack) || {ThreadId, Ack} <- Threads]
    },
    maybe
        {ok, #{<<"threads">> := Entries}} ?= rpc_client:call(Request),
        true ?= is_list(Entries),
        Requested = maps:from_keys([ThreadId || {ThreadId, _Ack} <- Threads], true),
        maps:from_list([
            Result
         || Entry <- Entries, {true, Result} <- [rpc_entry(Entry, Requested)]
        ])
    else
        _ -> #{}
    end.

-spec thread_rpc_entry(integer(), integer() | undefined) -> map().
thread_rpc_entry(ThreadId, undefined) ->
    #{<<"thread_id">> => integer_to_binary(ThreadId)};
thread_rpc_entry(ThreadId, Ack) ->
    #{
        <<"thread_id">> => integer_to_binary(ThreadId),
        <<"ack_message_id">> => integer_to_binary(Ack)
    }.

-spec rpc_entry(term(), #{integer() => true}) -> {true, {integer(), map()}} | false.
rpc_entry(#{<<"thread_id">> := RawId} = Entry, Requested) ->
    Parsed =
        case snowflake_id:parse_maybe(RawId) of
            ThreadId when is_integer(ThreadId), is_map_key(ThreadId, Requested) ->
                {ThreadId, rpc_fields(Entry)};
            _ ->
                false
        end,
    case Parsed of
        {Id, #{} = Fields} -> {true, {Id, Fields#{<<"thread_id">> => integer_to_binary(Id)}}};
        _ -> false
    end;
rpc_entry(_Entry, _Requested) ->
    false.

-spec rpc_fields(map()) -> map() | undefined.
rpc_fields(#{<<"missing">> := true}) ->
    #{<<"missing">> => true};
rpc_fields(#{<<"count">> := Count}) when is_integer(Count), Count >= 0 ->
    #{<<"count">> => min(Count, ?MAX_COUNT)};
rpc_fields(_Entry) ->
    undefined.

-spec dispatch(pid(), request_context(), [map()]) -> ok.
dispatch(SessionPid, #{guild_id := GuildId, channel_id := ChannelId}, Entries) ->
    gateway_dispatch_relay:dispatch(SessionPid, forum_unreads, #{
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"threads">> => Entries
    }).
