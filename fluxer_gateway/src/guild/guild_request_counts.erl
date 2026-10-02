%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_request_counts).
-typing([eqwalizer]).

-export([handle_request/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export_type([session_state/0]).

-define(MAX_GUILD_IDS, 100).
-define(GUILD_CALL_TIMEOUT_MS, 2000).
-define(BATCH_OVERALL_TIMEOUT_MS, 3000).
-define(MAX_NONCE_BYTES, 64).
-define(LEGACY_NODE_KEY(Node), {?MODULE, legacy_node, Node}).

-type session_state() :: map().

-spec handle_request(map(), pid(), session_state()) -> ok.
handle_request(Data, _SocketPid, SessionState) when is_map(Data) ->
    SessionPid = maps:get(session_pid, SessionState, undefined),
    UserId = parse_user_id(maps:get(user_id, SessionState, undefined)),
    case is_pid(SessionPid) andalso is_integer(UserId) of
        false ->
            ok;
        true ->
            GuildIds = parse_guild_ids(maps:get(<<"guild_ids">>, Data, [])),
            Nonce = parse_nonce(maps:get(<<"nonce">>, Data, undefined)),
            Guilds = maps:get(guilds, SessionState, #{}),
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
        fun parse_guild_id_filter/1,
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

-spec parse_guild_id_filter(term()) -> {true, integer()} | false.
parse_guild_id_filter(Id) ->
    case to_int(Id) of
        N when is_integer(N), N > 0 -> {true, N};
        _ -> false
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
    case is_legacy_node(node(GuildPid)) of
        true -> fetch_legacy_counts(GuildPid, UserId);
        false -> fetch_viewer_counts(GuildPid, UserId)
    end.

-spec fetch_viewer_counts(pid(), integer()) ->
    {ok, non_neg_integer(), non_neg_integer()} | error.
fetch_viewer_counts(GuildPid, UserId) ->
    Request = {get_viewer_counts, #{user_id => UserId}},
    try guild_query_handler:call(GuildPid, Request, ?GUILD_CALL_TIMEOUT_MS) of
        ok ->
            ok = remember_legacy_node(node(GuildPid)),
            fetch_legacy_counts(GuildPid, UserId);
        Reply ->
            counts_result(Reply)
    catch
        _:_ -> error
    end.

-spec is_legacy_node(node()) -> boolean().
is_legacy_node(Node) ->
    persistent_term:get(?LEGACY_NODE_KEY(Node), false) =:= true.

-spec remember_legacy_node(node()) -> ok.
remember_legacy_node(Node) ->
    persistent_term:put(?LEGACY_NODE_KEY(Node), true).

-spec fetch_legacy_counts(pid(), integer()) ->
    {ok, non_neg_integer(), non_neg_integer()} | error.
fetch_legacy_counts(GuildPid, UserId) ->
    try gen_server:call(GuildPid, {get_user_counts, UserId}, ?GUILD_CALL_TIMEOUT_MS) of
        Reply -> counts_result(Reply)
    catch
        _:_ -> error
    end.

-spec counts_result(term()) -> {ok, non_neg_integer(), non_neg_integer()} | error.
counts_result(#{member_count := MemberCount, online_count := OnlineCount}) ->
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

-spec parse_user_id(term()) -> integer() | undefined.
parse_user_id(Value) ->
    snowflake_id:parse_maybe(Value).

-spec to_int(term()) -> integer() | undefined.
to_int(Value) ->
    snowflake_id:parse_maybe(Value).

-ifdef(TEST).

parse_guild_ids_filters_invalid_test() ->
    ?assertEqual([1, 2], parse_guild_ids([<<"1">>, <<"2">>, <<"abc">>, 0, -3])).

parse_guild_ids_dedupes_and_sorts_test() ->
    ?assertEqual([1, 2, 3], parse_guild_ids([<<"3">>, <<"1">>, <<"2">>, <<"1">>])).

parse_guild_ids_caps_at_max_test() ->
    Many = [integer_to_binary(N) || N <- lists:seq(1, ?MAX_GUILD_IDS + 50)],
    Result = parse_guild_ids(Many),
    ?assertEqual(?MAX_GUILD_IDS, length(Result)).

parse_guild_ids_handles_non_list_test() ->
    ?assertEqual([], parse_guild_ids(undefined)),
    ?assertEqual([], parse_guild_ids(<<"hi">>)).

parse_user_id_test() ->
    ?assertEqual(42, parse_user_id(42)),
    ?assertEqual(42, parse_user_id(<<"42">>)),
    ?assertEqual(undefined, parse_user_id(0)),
    ?assertEqual(undefined, parse_user_id(<<"0">>)),
    ?assertEqual(undefined, parse_user_id(<<"abc">>)),
    ?assertEqual(undefined, parse_user_id(undefined)).

build_entry_shape_test() ->
    Entry = build_entry(123, 50, 10),
    ?assertEqual(<<"123">>, maps:get(<<"guild_id">>, Entry)),
    ?assertEqual(50, maps:get(<<"member_count">>, Entry)),
    ?assertEqual(10, maps:get(<<"online_count">>, Entry)).

lookup_guild_pid_test() ->
    Pid = self(),
    Ref = make_ref(),
    Guilds = #{1 => {Pid, Ref}, 2 => undefined},
    ?assertEqual({ok, Pid}, lookup_guild_pid(1, Guilds)),
    ?assertEqual(error, lookup_guild_pid(2, Guilds)),
    ?assertEqual(error, lookup_guild_pid(3, Guilds)).

handle_request_no_session_pid_returns_ok_test() ->
    SessionState = #{user_id => <<"100">>},
    ?assertEqual(ok, handle_request(#{<<"guild_ids">> => [<<"1">>]}, self(), SessionState)).

handle_request_dispatches_empty_when_no_guilds_test() ->
    Self = self(),
    SessionState = #{session_pid => Self, user_id => <<"100">>, guilds => #{}},
    ok = handle_request(#{<<"guild_ids">> => [<<"1">>]}, Self, SessionState),
    receive
        {'$gen_cast', {dispatch, guild_counts_update, Payload}} ->
            ?assertEqual([], maps:get(<<"counts">>, Payload)),
            ?assertNot(maps:is_key(<<"nonce">>, Payload))
    after 1000 ->
        ?assert(false)
    end.

handle_request_echoes_nonce_test() ->
    Self = self(),
    SessionState = #{session_pid => Self, user_id => <<"100">>, guilds => #{}},
    ok = handle_request(
        #{<<"guild_ids">> => [<<"1">>], <<"nonce">> => <<"abc123">>}, Self, SessionState
    ),
    receive
        {'$gen_cast', {dispatch, guild_counts_update, Payload}} ->
            ?assertEqual(<<"abc123">>, maps:get(<<"nonce">>, Payload)),
            ?assertEqual([], maps:get(<<"counts">>, Payload))
    after 1000 ->
        ?assert(false)
    end.

forget_legacy_nodes() ->
    _ = persistent_term:erase(?LEGACY_NODE_KEY(node())),
    ok.

handle_request_fetches_viewer_counts_with_deadline_test() ->
    ok = forget_legacy_nodes(),
    Self = self(),
    Guild = spawn(fun() ->
        receive
            {'$gen_call', From, {get_viewer_counts, #{user_id := 100, deadline := D}}} when
                is_integer(D)
            ->
                gen_server:reply(From, #{member_count => 50, online_count => 10})
        end
    end),
    SessionState = #{
        session_pid => Self, user_id => <<"100">>, guilds => #{7 => {Guild, make_ref()}}
    },
    ok = handle_request(#{<<"guild_ids">> => [<<"7">>]}, Self, SessionState),
    receive
        {'$gen_cast', {dispatch, guild_counts_update, Payload}} ->
            ?assertEqual([build_entry(7, 50, 10)], maps:get(<<"counts">>, Payload))
    after 1000 ->
        ?assert(false)
    end.

legacy_guild(Replies) ->
    spawn(fun() -> legacy_guild_loop(Replies) end).

legacy_guild_loop(Replies) ->
    receive
        {'$gen_call', From, {get_viewer_counts, #{user_id := 100, deadline := D}}} when
            is_integer(D)
        ->
            gen_server:reply(From, ok),
            legacy_guild_loop(Replies);
        {'$gen_call', From, {get_user_counts, 100}} ->
            [Reply | Rest] = Replies,
            gen_server:reply(From, Reply),
            legacy_guild_loop(Rest)
    after 5000 ->
        ok
    end.

request_counts_payload(Guilds) ->
    Self = self(),
    SessionState = #{session_pid => Self, user_id => <<"100">>, guilds => Guilds},
    GuildIds = [integer_to_binary(Id) || Id <- maps:keys(Guilds)],
    ok = handle_request(#{<<"guild_ids">> => GuildIds}, Self, SessionState),
    receive
        {'$gen_cast', {dispatch, guild_counts_update, Payload}} -> Payload
    after 1000 ->
        error(no_dispatch)
    end.

handle_request_falls_back_to_user_counts_on_legacy_guild_test() ->
    ok = forget_legacy_nodes(),
    Guild = legacy_guild([#{member_count => 50, online_count => 10}]),
    Payload = request_counts_payload(#{7 => {Guild, make_ref()}}),
    ?assertEqual([build_entry(7, 50, 10)], maps:get(<<"counts">>, Payload)).

handle_request_omits_guild_when_legacy_fallback_fails_test() ->
    ok = forget_legacy_nodes(),
    Guild = legacy_guild([ok]),
    Payload = request_counts_payload(#{7 => {Guild, make_ref()}}),
    ?assertEqual([], maps:get(<<"counts">>, Payload)).

handle_request_mixes_legacy_and_current_guilds_test() ->
    ok = forget_legacy_nodes(),
    Legacy = legacy_guild([#{member_count => 50, online_count => 10}]),
    Current = spawn(fun() ->
        receive
            {'$gen_call', From, {get_viewer_counts, #{user_id := 100}}} ->
                gen_server:reply(From, #{member_count => 80, online_count => 20});
            {'$gen_call', From, {get_user_counts, 100}} ->
                gen_server:reply(From, #{member_count => 80, online_count => 20})
        end
    end),
    Payload = request_counts_payload(#{7 => {Legacy, make_ref()}, 9 => {Current, make_ref()}}),
    ?assertEqual(
        [build_entry(7, 50, 10), build_entry(9, 80, 20)],
        lists:sort(maps:get(<<"counts">>, Payload))
    ).

fetch_counts_does_not_fall_back_on_current_guild_test() ->
    ok = forget_legacy_nodes(),
    Self = self(),
    Guild = spawn(fun() ->
        receive
            {'$gen_call', From, {get_viewer_counts, #{user_id := 100}}} ->
                gen_server:reply(From, #{member_count => 3, online_count => 1})
        end,
        receive
            {'$gen_call', From2, Msg} ->
                Self ! {unexpected_call, Msg},
                gen_server:reply(From2, ok)
        after 300 -> ok
        end
    end),
    ?assertEqual({ok, 3, 1}, fetch_counts(Guild, 100)),
    receive
        {unexpected_call, Msg} -> ?assertEqual(none, Msg)
    after 400 -> ok
    end.

fetch_counts_goes_straight_to_user_counts_after_a_legacy_answer_test() ->
    ok = forget_legacy_nodes(),
    Self = self(),
    Guild = spawn(fun() -> recording_legacy_guild(Self, 3) end),
    ?assertEqual({ok, 50, 10}, fetch_counts(Guild, 100)),
    ?assertEqual({ok, 50, 10}, fetch_counts(Guild, 100)),
    Calls = [
        receive
            {legacy_call, Tag} -> Tag
        after 1000 -> none
        end
     || _ <- [1, 2, 3]
    ],
    ok = forget_legacy_nodes(),
    ?assertEqual([get_viewer_counts, get_user_counts, get_user_counts], Calls).

recording_legacy_guild(_Parent, 0) ->
    ok;
recording_legacy_guild(Parent, N) ->
    receive
        {'$gen_call', From, {get_viewer_counts, _}} ->
            Parent ! {legacy_call, get_viewer_counts},
            gen_server:reply(From, ok);
        {'$gen_call', From, {get_user_counts, 100}} ->
            Parent ! {legacy_call, get_user_counts},
            gen_server:reply(From, #{member_count => 50, online_count => 10})
    after 1000 ->
        ok
    end,
    recording_legacy_guild(Parent, N - 1).

fetch_counts_errors_on_malformed_reply_test() ->
    ok = forget_legacy_nodes(),
    Guild = spawn(fun() ->
        receive
            {'$gen_call', From, {get_viewer_counts, _}} -> gen_server:reply(From, #{})
        end
    end),
    ?assertEqual(error, fetch_counts(Guild, 100)).

parse_nonce_test() ->
    ?assertEqual(<<"x">>, parse_nonce(<<"x">>)),
    ?assertEqual(<<"abc">>, parse_nonce(<<"abc">>)),
    ?assertEqual(undefined, parse_nonce(undefined)),
    ?assertEqual(undefined, parse_nonce(<<>>)),
    ?assertEqual(undefined, parse_nonce(123)),
    ?assertEqual(undefined, parse_nonce("string")),
    Big = binary:copy(<<"a">>, ?MAX_NONCE_BYTES + 1),
    ?assertEqual(undefined, parse_nonce(Big)),
    Edge = binary:copy(<<"a">>, ?MAX_NONCE_BYTES),
    ?assertEqual(Edge, parse_nonce(Edge)).

-endif.
