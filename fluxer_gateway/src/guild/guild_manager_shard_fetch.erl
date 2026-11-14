%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_manager_shard_fetch).
-typing([eqwalizer]).

-export([fetch_guild_data/1]).

-export_type([guild_id/0, fetch_result/0]).

-type guild_id() :: integer().
-type fetch_result() :: {ok, map()} | {error, term()}.

-define(GUILD_COLLECTION_FETCH_TIMEOUT_MS, 120000).
-define(GUILD_MEMBER_COLLECTION_LIMIT, 1000).
-define(WORKER_STOP_TIMEOUT_MS, 5000).
-define(GUILD_COLLECTIONS, [
    <<"guild">>,
    <<"roles">>,
    <<"channels">>,
    <<"emojis">>,
    <<"stickers">>,
    <<"members">>,
    <<"voice_states">>
]).

-spec fetch_guild_data(guild_id()) -> fetch_result().
fetch_guild_data(GuildId) ->
    Parent = self(),
    Ref = make_ref(),
    Workers = maps:from_list([
        spawn_collection_worker(Parent, Ref, GuildId, Collection)
     || Collection <- ?GUILD_COLLECTIONS
    ]),
    DeadlineMs = erlang:monotonic_time(millisecond) + ?GUILD_COLLECTION_FETCH_TIMEOUT_MS,
    {Result, RemainingWorkers} =
        collect_results(Ref, ?GUILD_COLLECTIONS, #{}, Workers, DeadlineMs),
    stop_workers(RemainingWorkers),
    flush_results(Ref),
    Result.

-spec spawn_collection_worker(pid(), reference(), guild_id(), binary()) ->
    {reference(), pid()}.
spawn_collection_worker(Parent, Ref, GuildId, Collection) ->
    {Pid, MRef} = spawn_monitor(fun() ->
        erlang:process_flag(fullsweep_after, 0),
        Parent ! {Ref, Collection, fetch_guild_collection(GuildId, Collection)}
    end),
    {MRef, Pid}.

-spec collect_results(reference(), [binary()], map(), #{reference() => pid()}, integer()) ->
    {fetch_result(), #{reference() => pid()}}.
collect_results(_Ref, [], Acc, Workers, _DeadlineMs) ->
    {{ok, Acc}, Workers};
collect_results(Ref, PendingCollections, Acc, Workers, DeadlineMs) ->
    RemainingMs = DeadlineMs - erlang:monotonic_time(millisecond),
    case RemainingMs > 0 of
        false ->
            {{error, {guild_collection_fetch_timeout, PendingCollections}}, Workers};
        true ->
            await_collection(Ref, PendingCollections, Acc, Workers, DeadlineMs, RemainingMs)
    end.

-spec await_collection(
    reference(), [binary()], map(), #{reference() => pid()}, integer(), integer()
) ->
    {fetch_result(), #{reference() => pid()}}.
await_collection(Ref, PendingCollections, Acc, Workers, DeadlineMs, RemainingMs) ->
    receive
        {Ref, Collection, {ok, Data}} ->
            NewPending = lists:delete(Collection, PendingCollections),
            collect_results(Ref, NewPending, Acc#{Collection => Data}, Workers, DeadlineMs);
        {Ref, Collection, {ok, Data, Extras}} when is_map(Extras) ->
            NewPending = lists:delete(Collection, PendingCollections),
            collect_results(
                Ref,
                NewPending,
                maps:merge(Acc#{Collection => Data}, Extras),
                Workers,
                DeadlineMs
            );
        {Ref, Collection, {error, Reason}} ->
            {{error, {guild_collection_fetch_failed, Collection, Reason}}, Workers};
        {'DOWN', MRef, process, _, normal} when is_map_key(MRef, Workers) ->
            collect_results(
                Ref, PendingCollections, Acc, maps:remove(MRef, Workers), DeadlineMs
            );
        {'DOWN', MRef, process, _, Reason} when is_map_key(MRef, Workers) ->
            {{error, {guild_collection_worker_down, Reason}}, maps:remove(MRef, Workers)}
    after RemainingMs ->
        {{error, {guild_collection_fetch_timeout, PendingCollections}}, Workers}
    end.

-spec stop_workers(#{reference() => pid()}) -> ok.
stop_workers(Workers) ->
    maps:foreach(fun stop_worker/2, Workers).

-spec stop_worker(reference(), pid()) -> ok.
stop_worker(MRef, Pid) ->
    exit(Pid, kill),
    receive
        {'DOWN', MRef, process, _, _} -> ok
    after ?WORKER_STOP_TIMEOUT_MS ->
        _ = erlang:demonitor(MRef, [flush]),
        ok
    end.

-spec flush_results(reference()) -> ok.
flush_results(Ref) ->
    receive
        {Ref, _, _} -> flush_results(Ref)
    after 0 ->
        ok
    end.

-spec fetch_guild_collection(guild_id(), binary()) ->
    {ok, term()} | {ok, term(), map()} | {error, term()}.
fetch_guild_collection(GuildId, <<"members">>) ->
    fetch_members_stream(GuildId, undefined, []);
fetch_guild_collection(GuildId, <<"channels">>) ->
    RpcRequest = #{
        <<"type">> => <<"guild_collection">>,
        <<"guild_id">> => type_conv:to_binary(GuildId),
        <<"collection">> => <<"channels">>
    },
    parse_channels_response(rpc_client:call(RpcRequest));
fetch_guild_collection(GuildId, Collection) ->
    RpcRequest = #{
        <<"type">> => <<"guild_collection">>,
        <<"guild_id">> => type_conv:to_binary(GuildId),
        <<"collection">> => Collection
    },
    parse_collection_response(Collection, rpc_client:call(RpcRequest)).

-spec parse_collection_response(binary(), term()) -> {ok, term()} | {error, term()}.
parse_collection_response(Collection, {ok, Data}) when is_map(Data) ->
    case maps:get(Collection, Data, undefined) of
        undefined -> {error, {invalid_collection_response, Collection}};
        Value -> {ok, Value}
    end;
parse_collection_response(_Collection, {error, Reason}) ->
    {error, Reason}.

-spec parse_channels_response(term()) ->
    {ok, term()} | {ok, [map()], map()} | {error, term()}.
parse_channels_response({ok, #{<<"channels">> := Channels} = Data}) when
    is_list(Channels)
->
    case maps:is_key(<<"thread_gate">>, Data) orelse maps:is_key(<<"thread_tainted">>, Data) of
        true ->
            {Merged, Extras} = guild_thread_load:channels_collection(Data),
            {ok, Merged, Extras};
        false ->
            {ok, Channels}
    end;
parse_channels_response(Result) ->
    parse_collection_response(<<"channels">>, Result).

-spec fetch_members_stream(guild_id(), binary() | undefined, [[map()]]) ->
    {ok, [map()]} | {error, term()}.
fetch_members_stream(GuildId, AfterUserId, ChunksAcc) ->
    RpcRequest0 = #{
        <<"type">> => <<"guild_collection">>,
        <<"guild_id">> => type_conv:to_binary(GuildId),
        <<"collection">> => <<"members">>,
        <<"limit">> => ?GUILD_MEMBER_COLLECTION_LIMIT
    },
    RpcRequest = maybe_put_after(AfterUserId, RpcRequest0),
    case rpc_client:call(RpcRequest) of
        {ok, Data} -> parse_members_page(GuildId, Data, ChunksAcc);
        {error, Reason} -> {error, Reason}
    end.

-spec parse_members_page(guild_id(), map(), [[map()]]) -> {ok, [map()]} | {error, term()}.
parse_members_page(GuildId, Data, ChunksAcc) ->
    Members = maps:get(<<"members">>, Data, undefined),
    HasMore = maps:get(<<"has_more">>, Data, false),
    NextAfter = maps:get(<<"next_after_user_id">>, Data, null),
    case Members of
        MemberList when is_list(MemberList) ->
            parse_members_result(GuildId, MemberList, HasMore, NextAfter, ChunksAcc);
        _ ->
            {error, invalid_members_collection_payload}
    end.

-spec parse_members_result(guild_id(), [map()], term(), term(), [[map()]]) ->
    {ok, [map()]} | {error, term()}.
parse_members_result(GuildId, MemberList, true, NextAfter, ChunksAcc) when
    is_binary(NextAfter), MemberList =/= []
->
    fetch_members_stream(GuildId, NextAfter, [MemberList | ChunksAcc]);
parse_members_result(_GuildId, [], true, _NextAfter, _ChunksAcc) ->
    {error, invalid_members_collection_empty_page};
parse_members_result(_GuildId, _MemberList, true, _NextAfter, _ChunksAcc) ->
    {error, invalid_members_collection_cursor};
parse_members_result(_GuildId, MemberList, false, _NextAfter, ChunksAcc) ->
    {ok, lists:append(lists:reverse([MemberList | ChunksAcc]))};
parse_members_result(_GuildId, _MemberList, _HasMore, _NextAfter, _ChunksAcc) ->
    {error, invalid_members_collection_has_more}.

-spec maybe_put_after(binary() | undefined, map()) -> map().
maybe_put_after(undefined, RpcRequest) ->
    RpcRequest;
maybe_put_after(AfterUserId, RpcRequest) when is_binary(AfterUserId) ->
    RpcRequest#{<<"after_user_id">> => AfterUserId}.
