%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_unified_subscriptions).
-typing([eqwalizer]).

-export([handle_subscriptions/3, replay_guild_subscription/6]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type session_state() :: map().
-type session_id() :: binary() | undefined.
-export_type([session_state/0, session_id/0]).

-define(MAX_RANGE_END, 100000).
-define(MAX_RANGE_SPAN, 99).
-define(MAX_RANGES_PER_CHANNEL, 10).
-define(MAX_MEMBER_SUBSCRIPTION_IDS, 1000).
-define(MAX_THREAD_MEMBER_LISTS, 10).

-spec handle_subscriptions(term(), pid(), session_state()) -> ok.
handle_subscriptions(Data, SocketPid, SessionState) when is_map(Data) ->
    maybe_store_subscription_state(Data, SessionState),
    Subscriptions = maps:get(<<"subscriptions">>, Data, #{}),
    case is_map(Subscriptions) of
        true ->
            process_all_guild_subscriptions(Subscriptions, SocketPid, SessionState);
        false ->
            ok
    end;
handle_subscriptions(_, _, _) ->
    ok.

-spec process_all_guild_subscriptions(map(), pid(), session_state()) -> ok.
process_all_guild_subscriptions(Subscriptions, SocketPid, SessionState) ->
    Guilds = maps:get(guilds, SessionState, #{}),
    SessionId = maps:get(id, SessionState, undefined),
    maps:foreach(
        fun
            (GIdBin, GSub) when is_binary(GIdBin), is_map(GSub) ->
                process_guild_subscription(
                    GIdBin, GSub, Guilds, SessionId, SocketPid, SessionState
                );
            (_, _) ->
                ok
        end,
        Subscriptions
    ),
    ok.

-spec replay_guild_subscription(
    integer(), pid(), map(), session_id(), pid() | undefined, session_state()
) -> ok.
replay_guild_subscription(
    GuildId, GuildPid, GuildSubData, SessionId, SocketPid, SessionState
) when
    is_integer(GuildId), is_pid(GuildPid), is_map(GuildSubData)
->
    process_guild_sub_options(
        GuildId, GuildPid, GuildSubData, SessionId, SocketPid, SessionState
    );
replay_guild_subscription(_, _, _, _, _, _) ->
    ok.

-spec maybe_store_subscription_state(map(), session_state()) -> ok.
maybe_store_subscription_state(Data, SessionState) ->
    case maps:find(session_pid, SessionState) of
        {ok, SessionPid} when is_pid(SessionPid) ->
            _ = shard_utils:safe_cast(SessionPid, {store_guild_subscriptions, Data}),
            ok;
        _ ->
            ok
    end.

-spec process_guild_subscription(binary(), map(), map(), session_id(), pid(), session_state()) ->
    ok.
process_guild_subscription(
    GuildIdBin, GuildSubData, Guilds, SessionId, SocketPid, SessionState
) ->
    case validation:validate_snowflake(<<"guild_id">>, GuildIdBin) of
        {ok, GuildId} ->
            maybe_process_guild(
                GuildId, GuildSubData, Guilds, SessionId, SocketPid, SessionState
            );
        {error, _, _} ->
            ok
    end.

-spec maybe_process_guild(integer(), map(), map(), session_id(), pid(), session_state()) -> ok.
maybe_process_guild(GuildId, GuildSubData, Guilds, SessionId, SocketPid, SessionState) ->
    case maps:find(GuildId, Guilds) of
        {ok, {GuildPid, _Ref}} when is_pid(GuildPid) ->
            process_guild_sub_options(
                GuildId, GuildPid, GuildSubData, SessionId, SocketPid, SessionState
            );
        _ ->
            ok
    end.

-spec process_guild_sub_options(
    integer(), pid(), map(), session_id(), pid() | undefined, session_state()
) ->
    ok.
process_guild_sub_options(GuildId, GuildPid, GuildSubData, SessionId, SocketPid, SessionState) ->
    WasActive = not session_passive:is_passive(GuildId, SessionState),
    ActiveChanged = process_active_flag(GuildSubData, GuildPid, SessionId, WasActive),
    process_sync_flag(GuildSubData, GuildId, GuildPid, SessionId, ActiveChanged),
    process_member_list_channels(GuildSubData, GuildId, GuildPid, SessionId, SocketPid),
    process_member_subscriptions(GuildSubData, GuildPid, SessionId),
    process_typing_flag(GuildSubData, GuildPid, SessionId),
    process_thread_subscriptions(GuildSubData, GuildPid, SessionId, SessionState),
    ok.

-spec process_thread_subscriptions(map(), pid(), session_id(), session_state()) -> ok.
process_thread_subscriptions(GuildSubData, GuildPid, SessionId, SessionState) when
    is_binary(SessionId)
->
    Capable =
        maps:get(bot, SessionState, false) =:= true orelse
            maps:get(thread_channels_capable, SessionState, false) =:= true,
    Threads =
        case maps:get(<<"threads">>, GuildSubData, undefined) of
            Flag when is_boolean(Flag) -> Flag;
            _ -> undefined
        end,
    Lists =
        case maps:get(<<"thread_member_lists">>, GuildSubData, undefined) of
            Ids when is_list(Ids) -> parse_thread_ids(Ids);
            _ -> undefined
        end,
    case {Capable, Threads, Lists} of
        {false, _, _} ->
            ok;
        {true, undefined, undefined} ->
            ok;
        {true, _, _} ->
            _ = shard_utils:safe_cast(
                GuildPid,
                {thread_subscriptions, SessionId, #{threads => Threads, member_lists => Lists}}
            ),
            ok
    end;
process_thread_subscriptions(_GuildSubData, _GuildPid, _SessionId, _SessionState) ->
    ok.

-spec parse_thread_ids(list()) -> [integer()].
parse_thread_ids(Ids) ->
    lists:sublist(
        [
            Id
         || Raw <- Ids,
            Id <- [snowflake_id:parse_maybe(Raw)],
            is_integer(Id)
        ],
        ?MAX_THREAD_MEMBER_LISTS
    ).

-spec process_active_flag(map(), pid(), session_id(), boolean()) -> boolean().
process_active_flag(GuildSubData, GuildPid, SessionId, WasActive) ->
    case maps:get(<<"active">>, GuildSubData, undefined) of
        undefined ->
            false;
        true ->
            _ = shard_utils:safe_cast(GuildPid, {set_session_active, SessionId}),
            not WasActive;
        false ->
            _ = shard_utils:safe_cast(GuildPid, {set_session_passive, SessionId}),
            WasActive
    end.

-spec process_sync_flag(map(), integer(), pid(), session_id(), boolean()) -> ok.
process_sync_flag(GuildSubData, _GuildId, GuildPid, SessionId, ActiveChanged) ->
    ShouldSync = maps:get(<<"sync">>, GuildSubData, false) orelse ActiveChanged,
    case ShouldSync of
        true ->
            _ = shard_utils:safe_cast(GuildPid, {send_guild_sync, SessionId}),
            ok;
        false ->
            ok
    end.

-spec process_member_list_channels(map(), integer(), pid(), session_id(), pid() | undefined) ->
    ok.
process_member_list_channels(GuildSubData, GuildId, GuildPid, SessionId, SocketPid) ->
    case maps:find(<<"member_list_channels">>, GuildSubData) of
        {ok, MLC} when is_map(MLC) ->
            foreach_channel_subscribe(MLC, GuildId, GuildPid, SessionId, SocketPid);
        _ ->
            ok
    end.

-spec foreach_channel_subscribe(map(), integer(), pid(), session_id(), pid() | undefined) -> ok.
foreach_channel_subscribe(MemberListChannels, GuildId, GuildPid, SessionId, SocketPid) ->
    maps:foreach(
        fun(ChIdBin, Ranges) ->
            process_channel_lazy_subscribe(
                ChIdBin, Ranges, GuildId, GuildPid, SessionId, SocketPid
            )
        end,
        MemberListChannels
    ).

-spec process_channel_lazy_subscribe(
    binary(), list(), integer(), pid(), session_id(), pid() | undefined
) -> ok.
process_channel_lazy_subscribe(ChannelIdBin, Ranges, _GuildId, GuildPid, SessionId, _SocketPid) ->
    case validation:validate_snowflake(<<"channel_id">>, ChannelIdBin) of
        {ok, ChannelId} ->
            safe_lazy_subscribe(GuildPid, SessionId, ChannelId, Ranges);
        {error, _, _} ->
            ok
    end,
    ok.

-spec safe_lazy_subscribe(pid(), session_id(), integer(), list()) -> ok.
safe_lazy_subscribe(GuildPid, SessionId, ChannelId, Ranges) ->
    ParsedRanges = parse_ranges(Ranges),
    try
        gen_server:call(
            GuildPid,
            {lazy_subscribe, #{
                session_id => SessionId,
                channel_id => ChannelId,
                ranges => ParsedRanges
            }},
            2000
        )
    catch
        exit:{timeout, _} -> ok;
        exit:{noproc, _} -> ok;
        exit:{normal, _} -> ok
    end.

-spec parse_ranges(term()) -> [{non_neg_integer(), non_neg_integer()}].
parse_ranges(Ranges) when is_list(Ranges) ->
    lists:sublist(lists:filtermap(fun validate_range/1, Ranges), ?MAX_RANGES_PER_CHANNEL);
parse_ranges(_) ->
    [].

-spec validate_range(term()) -> {true, {non_neg_integer(), non_neg_integer()}} | false.
validate_range([Start, End]) when
    is_integer(Start),
    is_integer(End),
    Start >= 0,
    End >= Start,
    End =< ?MAX_RANGE_END,
    End - Start =< ?MAX_RANGE_SPAN
->
    {true, {Start, End}};
validate_range(_) ->
    false.

-spec process_member_subscriptions(map(), pid(), session_id()) -> ok.
process_member_subscriptions(GuildSubData, GuildPid, SessionId) ->
    case maps:find(<<"members">>, GuildSubData) of
        error ->
            ok;
        {ok, Members} when is_list(Members) ->
            MemberIds = lists:sublist(parse_member_ids(Members), ?MAX_MEMBER_SUBSCRIPTION_IDS),
            Msg = {update_member_subscriptions, SessionId, MemberIds},
            _ = shard_utils:safe_cast(GuildPid, Msg),
            ok;
        _ ->
            ok
    end.

-spec parse_member_ids(term()) -> [integer()].
parse_member_ids(Members) when is_list(Members) ->
    lists:filtermap(fun validate_member_id/1, Members);
parse_member_ids(_) ->
    [].

-spec validate_member_id(term()) -> {true, integer()} | false.
validate_member_id(MemberIdRaw) ->
    case validation:validate_snowflake(<<"member_id">>, MemberIdRaw) of
        {ok, MemberId} -> {true, MemberId};
        {error, _, _} -> false
    end.

-spec process_typing_flag(map(), pid(), session_id()) -> ok.
process_typing_flag(GuildSubData, GuildPid, SessionId) ->
    case maps:find(<<"typing">>, GuildSubData) of
        error ->
            ok;
        {ok, TypingFlag} when is_boolean(TypingFlag) ->
            Msg = {set_session_typing_override, SessionId, TypingFlag},
            _ = shard_utils:safe_cast(GuildPid, Msg),
            ok;
        _ ->
            ok
    end.

-ifdef(TEST).

parse_ranges_valid_test() ->
    ?assertEqual([{0, 99}, {100, 199}], parse_ranges([[0, 99], [100, 199]])).

parse_ranges_invalid_test() ->
    ?assertEqual([], parse_ranges([[100, 50]])),
    ?assertEqual([], parse_ranges([[-1, 99]])),
    ?assertEqual([], parse_ranges([[<<"0">>, 99]])),
    ?assertEqual([], parse_ranges([[0, 100]])),
    ?assertEqual([], parse_ranges([[0, 100001]])).

parse_ranges_mixed_test() ->
    ?assertEqual([{0, 99}], parse_ranges([[0, 99], [100, 50], <<"invalid">>])).

parse_ranges_non_list_test() ->
    ?assertEqual([], parse_ranges(undefined)),
    ?assertEqual([], parse_ranges(#{})).

parse_member_ids_valid_test() ->
    ?assertEqual([123, 456], parse_member_ids([<<"123">>, <<"456">>])).

parse_member_ids_invalid_test() ->
    ?assertEqual([], parse_member_ids([<<"not_a_number">>])).

parse_member_ids_mixed_test() ->
    ?assertEqual([123], parse_member_ids([<<"123">>, <<"invalid">>])).

parse_member_ids_non_list_test() ->
    ?assertEqual([], parse_member_ids(undefined)),
    ?assertEqual([], parse_member_ids(#{})).

handle_subscriptions_non_map_data_test() ->
    ?assertEqual(ok, handle_subscriptions(not_a_map, self(), #{})),
    ?assertEqual(ok, handle_subscriptions(42, self(), #{})),
    ?assertEqual(ok, handle_subscriptions(<<"string">>, self(), #{})).

handle_subscriptions_non_map_subscriptions_value_test() ->
    ?assertEqual(ok, handle_subscriptions(#{<<"subscriptions">> => [1, 2, 3]}, self(), #{})),
    ?assertEqual(ok, handle_subscriptions(#{<<"subscriptions">> => <<"bad">>}, self(), #{})),
    ?assertEqual(ok, handle_subscriptions(#{<<"subscriptions">> => 42}, self(), #{})).

handle_subscriptions_empty_map_test() ->
    ?assertEqual(ok, handle_subscriptions(#{}, self(), #{})).

handle_subscriptions_empty_subscriptions_test() ->
    ?assertEqual(ok, handle_subscriptions(#{<<"subscriptions">> => #{}}, self(), #{})).

handle_subscriptions_non_binary_guild_id_key_test() ->
    Subs = #{123 => #{<<"active">> => true}},
    ?assertEqual(ok, handle_subscriptions(#{<<"subscriptions">> => Subs}, self(), #{})).

handle_subscriptions_non_map_guild_sub_data_test() ->
    Subs = #{<<"12345">> => <<"not_a_map">>, <<"67890">> => [1, 2]},
    ?assertEqual(ok, handle_subscriptions(#{<<"subscriptions">> => Subs}, self(), #{})).

handle_subscriptions_invalid_guild_id_snowflake_test() ->
    Subs = #{<<"not_a_snowflake">> => #{<<"active">> => true}},
    ?assertEqual(ok, handle_subscriptions(#{<<"subscriptions">> => Subs}, self(), #{})).

parse_ranges_caps_at_max_test() ->
    Ranges = [[I * 100, I * 100 + 99] || I <- lists:seq(0, 14)],
    Parsed = parse_ranges(Ranges),
    ?assertEqual(?MAX_RANGES_PER_CHANNEL, length(Parsed)).

parse_ranges_under_cap_keeps_all_test() ->
    Ranges = [[0, 99], [100, 199], [200, 299]],
    Parsed = parse_ranges(Ranges),
    ?assertEqual(3, length(Parsed)).

parse_member_ids_caps_at_max_test() ->
    BigMembers = [
        integer_to_binary(I)
     || I <- lists:seq(1, ?MAX_MEMBER_SUBSCRIPTION_IDS + 200)
    ],
    AllParsed = parse_member_ids(BigMembers),
    ?assertEqual(?MAX_MEMBER_SUBSCRIPTION_IDS + 200, length(AllParsed)),
    Capped = lists:sublist(AllParsed, ?MAX_MEMBER_SUBSCRIPTION_IDS),
    ?assertEqual(?MAX_MEMBER_SUBSCRIPTION_IDS, length(Capped)).

thread_keys_are_forwarded_only_for_capable_sessions_test() ->
    Sub = #{<<"threads">> => true, <<"thread_member_lists">> => [<<"5">>, <<"x">>]},
    ok = process_thread_subscriptions(Sub, self(), <<"s">>, #{}),
    ok = process_thread_subscriptions(Sub, self(), <<"s">>, #{thread_channels_capable => true}),
    receive
        {'$gen_cast', {thread_subscriptions, <<"s">>, Subs}} ->
            ?assertEqual(#{threads => true, member_lists => [5]}, Subs)
    after 1000 -> ?assert(false)
    end,
    ok = process_thread_subscriptions(#{<<"active">> => true}, self(), <<"s">>, #{bot => true}),
    receive
        {'$gen_cast', {thread_subscriptions, _, _}} = Unexpected ->
            ?assertEqual(none, Unexpected)
    after 100 -> ok
    end.

-endif.
