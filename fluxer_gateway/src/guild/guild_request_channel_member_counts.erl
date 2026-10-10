%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_request_channel_member_counts).
-typing([eqwalizer]).

-export([handle_request/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export_type([session_state/0]).

-define(MAX_CHANNEL_IDS, 25).
-define(GUILD_CALL_TIMEOUT_MS, 2000).
-define(MAX_NONCE_BYTES, 64).

-type session_state() :: map().

-spec handle_request(map(), pid(), session_state()) -> ok.
handle_request(Data, _SocketPid, SessionState) when is_map(Data) ->
    SessionPid = maps:get(session_pid, SessionState, undefined),
    UserId = parse_user_id(maps:get(user_id, SessionState, undefined)),
    SessionId = parse_session_id(maps:get(session_id, SessionState, undefined)),
    case is_pid(SessionPid) andalso is_integer(UserId) andalso is_binary(SessionId) of
        false ->
            ok;
        true ->
            handle_valid_request(Data, SessionPid, SessionId, UserId, SessionState)
    end;
handle_request(_, _, _) ->
    ok.

-spec handle_valid_request(map(), pid(), binary(), integer(), session_state()) -> ok.
handle_valid_request(Data, SessionPid, SessionId, UserId, SessionState) ->
    GuildId = parse_guild_id(maps:get(<<"guild_id">>, Data, undefined)),
    ChannelIds = parse_channel_ids(Data),
    Nonce = parse_nonce(maps:get(<<"nonce">>, Data, undefined)),
    Entries = fetch_counts(GuildId, ChannelIds, SessionId, UserId, SessionState),
    dispatch_counts(SessionPid, Entries, Nonce),
    ok.

-spec fetch_counts(
    integer() | undefined, [integer()], binary(), integer(), session_state()
) -> [map()].
fetch_counts(undefined, _ChannelIds, _SessionId, _UserId, _SessionState) ->
    [];
fetch_counts(_GuildId, [], _SessionId, _UserId, _SessionState) ->
    [];
fetch_counts(GuildId, ChannelIds, SessionId, UserId, SessionState) ->
    case lookup_guild_pid(GuildId, maps:get(guilds, SessionState, #{})) of
        {ok, GuildPid} ->
            request_guild_channel_counts(GuildPid, GuildId, ChannelIds, SessionId, UserId);
        error ->
            []
    end.

-spec request_guild_channel_counts(pid(), integer(), [integer()], binary(), integer()) ->
    [map()].
request_guild_channel_counts(GuildPid, GuildId, ChannelIds, SessionId, UserId) ->
    Request = #{session_id => SessionId, user_id => UserId, channel_ids => ChannelIds},
    try
        gen_server:call(GuildPid, {get_channel_member_counts, Request}, ?GUILD_CALL_TIMEOUT_MS)
    of
        #{counts := Entries} when is_list(Entries) ->
            normalize_entries(GuildId, Entries);
        _ ->
            []
    catch
        _:_ -> []
    end.

-spec normalize_entries(integer(), [term()]) -> [map()].
normalize_entries(GuildId, Entries) ->
    lists:filtermap(fun(Entry) -> normalize_entry(GuildId, Entry) end, Entries).

-spec normalize_entry(integer(), term()) -> {true, map()} | false.
normalize_entry(GuildId, #{
    channel_id := ChannelId,
    member_count := MemberCount,
    online_count := OnlineCount
}) when
    is_integer(ChannelId),
    ChannelId > 0,
    is_integer(MemberCount),
    MemberCount >= 0,
    is_integer(OnlineCount),
    OnlineCount >= 0
->
    {true, build_entry(GuildId, ChannelId, MemberCount, OnlineCount)};
normalize_entry(_GuildId, _) ->
    false.

-spec build_entry(integer(), integer(), non_neg_integer(), non_neg_integer()) -> map().
build_entry(GuildId, ChannelId, MemberCount, OnlineCount) ->
    #{
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"channel_id">> => integer_to_binary(ChannelId),
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
    gateway_dispatch_relay:dispatch(SessionPid, channel_member_counts_update, Payload),
    ok.

-spec parse_channel_ids(map()) -> [integer()].
parse_channel_ids(Data) ->
    RawIds =
        case maps:get(<<"channel_ids">>, Data, undefined) of
            Ids when is_list(Ids) -> Ids;
            _ -> [maps:get(<<"channel_id">>, Data, undefined)]
        end,
    Parsed = lists:filtermap(fun parse_channel_id_filter/1, RawIds),
    lists:sublist(lists:usort(Parsed), ?MAX_CHANNEL_IDS).

-spec parse_channel_id_filter(term()) -> {true, integer()} | false.
parse_channel_id_filter(Value) ->
    case to_int(Value) of
        N when is_integer(N), N > 0 -> {true, N};
        _ -> false
    end.

-spec lookup_guild_pid(integer(), map()) -> {ok, pid()} | error.
lookup_guild_pid(GuildId, Guilds) ->
    case maps:get(GuildId, Guilds, undefined) of
        {Pid, _Ref} when is_pid(Pid) -> {ok, Pid};
        _ -> error
    end.

-spec parse_guild_id(term()) -> integer() | undefined.
parse_guild_id(Value) ->
    to_int(Value).

-spec parse_user_id(term()) -> integer() | undefined.
parse_user_id(Value) ->
    to_int(Value).

-spec parse_session_id(term()) -> binary() | undefined.
parse_session_id(SessionId) when is_binary(SessionId), byte_size(SessionId) > 0 ->
    SessionId;
parse_session_id(_) ->
    undefined.

-spec parse_nonce(term()) -> binary() | undefined.
parse_nonce(Nonce) when
    is_binary(Nonce), byte_size(Nonce) > 0, byte_size(Nonce) =< ?MAX_NONCE_BYTES
->
    Nonce;
parse_nonce(_) ->
    undefined.

-spec to_int(term()) -> integer() | undefined.
to_int(Value) ->
    snowflake_id:parse_maybe(Value).

-ifdef(TEST).

parse_channel_ids_accepts_single_channel_id_test() ->
    ?assertEqual([42], parse_channel_ids(#{<<"channel_id">> => <<"42">>})).

parse_channel_ids_dedupes_sorts_and_filters_test() ->
    ?assertEqual(
        [1, 2],
        parse_channel_ids(#{<<"channel_ids">> => [<<"2">>, <<"1">>, <<"2">>, 0, <<"bad">>]})
    ).

parse_channel_ids_caps_at_max_test() ->
    Many = [integer_to_binary(N) || N <- lists:seq(1, ?MAX_CHANNEL_IDS + 50)],
    ?assertEqual(?MAX_CHANNEL_IDS, length(parse_channel_ids(#{<<"channel_ids">> => Many}))).

parse_nonce_test() ->
    ?assertEqual(<<"n">>, parse_nonce(<<"n">>)),
    ?assertEqual(undefined, parse_nonce(<<>>)),
    ?assertEqual(undefined, parse_nonce(123)),
    Big = binary:copy(<<"a">>, ?MAX_NONCE_BYTES + 1),
    ?assertEqual(undefined, parse_nonce(Big)).

handle_request_dispatches_empty_when_no_guild_test() ->
    Self = self(),
    SessionState = #{
        session_pid => Self,
        session_id => <<"s1">>,
        user_id => <<"100">>,
        guilds => #{}
    },
    ok = handle_request(
        #{<<"guild_id">> => <<"1">>, <<"channel_id">> => <<"2">>}, Self, SessionState
    ),
    receive
        {'$gen_cast', {dispatch, channel_member_counts_update, Payload}} ->
            ?assertEqual([], maps:get(<<"counts">>, Payload))
    after 1000 ->
        ?assert(false)
    end.

-endif.
