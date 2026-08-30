%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_manager_shard_fetch).
-typing([eqwalizer]).

-export([fetch_guild_data/1]).

-export_type([guild_id/0, fetch_result/0]).

-type guild_id() :: integer().
-type fetch_result() :: {ok, map()} | {error, term()}.

-define(GUILD_COLLECTION_FETCH_TIMEOUT_MS, 120000).
-define(GUILD_MEMBER_COLLECTION_LIMIT, 1000).
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
    _ = [
        spawn_monitor(fun() ->
            erlang:process_flag(fullsweep_after, 0),
            Parent ! {Ref, Collection, fetch_guild_collection(GuildId, Collection)}
        end)
     || Collection <- ?GUILD_COLLECTIONS
    ],
    DeadlineMs = erlang:monotonic_time(millisecond) + ?GUILD_COLLECTION_FETCH_TIMEOUT_MS,
    collect_results(Ref, ?GUILD_COLLECTIONS, #{}, DeadlineMs).

-spec collect_results(reference(), [binary()], map(), integer()) -> fetch_result().
collect_results(_Ref, [], Acc, _DeadlineMs) ->
    {ok, Acc};
collect_results(Ref, PendingCollections, Acc, DeadlineMs) ->
    RemainingMs = DeadlineMs - erlang:monotonic_time(millisecond),
    case RemainingMs > 0 of
        false ->
            {error, {guild_collection_fetch_timeout, PendingCollections}};
        true ->
            await_collection(Ref, PendingCollections, Acc, DeadlineMs, RemainingMs)
    end.

-spec await_collection(reference(), [binary()], map(), integer(), integer()) ->
    fetch_result().
await_collection(Ref, PendingCollections, Acc, DeadlineMs, RemainingMs) ->
    receive
        {Ref, Collection, {ok, Data}} ->
            NewPending = lists:delete(Collection, PendingCollections),
            collect_results(Ref, NewPending, Acc#{Collection => Data}, DeadlineMs);
        {Ref, Collection, {error, Reason}} ->
            {error, {guild_collection_fetch_failed, Collection, Reason}};
        {'DOWN', _, process, _, _} ->
            collect_results(Ref, PendingCollections, Acc, DeadlineMs)
    after RemainingMs ->
        {error, {guild_collection_fetch_timeout, PendingCollections}}
    end.

-spec fetch_guild_collection(guild_id(), binary()) -> {ok, term()} | {error, term()}.
fetch_guild_collection(GuildId, <<"members">>) ->
    fetch_members_stream(GuildId, undefined, []);
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_members_result_final_page_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}}
    ],
    ?assertEqual(
        {ok, Members},
        parse_members_result(42, Members, false, null, [])
    ).

parse_members_result_invalid_cursor_test() ->
    Members = [
        #{<<"user">> => #{<<"id">> => <<"1">>}}
    ],
    ?assertEqual(
        {error, invalid_members_collection_cursor},
        parse_members_result(42, Members, true, null, [])
    ).

maybe_put_after_test() ->
    BaseRequest = #{
        <<"type">> => <<"guild_collection">>,
        <<"collection">> => <<"members">>
    },
    ?assertEqual(BaseRequest, maybe_put_after(undefined, BaseRequest)),
    WithCursor = maybe_put_after(<<"100">>, BaseRequest),
    ?assertEqual(<<"100">>, maps:get(<<"after_user_id">>, WithCursor)).

-endif.
