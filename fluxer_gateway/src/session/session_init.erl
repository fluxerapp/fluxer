%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_init).
-typing([eqwalizer]).

-export([
    normalize_guild_ids/1,
    replay_buffer_bytes/1,
    normalize_buffer/1,
    normalize_seq/1,
    normalize_active_guilds/1,
    build_ignored_events_map/1,
    load_private_channels/1,
    load_relationships/1,
    ensure_bot_ready_map/1,
    build_state/1,
    schedule_timers/1
]).

-export_type([guild_id/0, session_state/0]).

-type guild_id() :: session:guild_id().
-type session_state() :: session:session_state().
-type seq() :: session:seq().

-spec normalize_guild_ids(term()) -> [guild_id()].
normalize_guild_ids(Value) when is_list(Value) ->
    lists:filtermap(
        fun
            (GuildId) when is_integer(GuildId), GuildId > 0 ->
                {true, GuildId};
            (_) ->
                false
        end,
        Value
    );
normalize_guild_ids(Value) when is_map(Value) -> normalize_guild_ids(maps:keys(Value));
normalize_guild_ids(_) ->
    [].

-spec normalize_active_guilds(term()) -> sets:set(guild_id()).
normalize_active_guilds(Value) ->
    sets:from_list(normalize_guild_ids(active_guild_source(Value))).

-spec active_guild_source(term()) -> term().
active_guild_source(Value) ->
    try sets:to_list(eqwalizer:dynamic_cast(Value)) of
        List when is_list(List) -> List
    catch
        _:_ -> active_guild_source_fallback(Value)
    end.

-spec active_guild_source_fallback(term()) -> term().
active_guild_source_fallback(Value) when is_map(Value) ->
    maps:keys(Value);
active_guild_source_fallback(Value) ->
    Value.

-spec normalize_seq(term()) -> session:seq().
normalize_seq(Value) when is_integer(Value), Value >= 0 -> Value;
normalize_seq(_) -> 0.

-spec normalize_buffer(term()) -> [map()].
normalize_buffer(Buffer) when is_list(Buffer) ->
    lists:filtermap(fun normalize_buffer_event/1, Buffer);
normalize_buffer(Buffer) when is_map(Buffer) ->
    lists:filtermap(fun normalize_buffer_event/1, deque_to_list(Buffer));
normalize_buffer(_) ->
    [].

-spec deque_to_list(map()) -> [term()].
deque_to_list(Buffer) ->
    try limited_deque:to_list(eqwalizer:dynamic_cast(Buffer)) of
        List -> List
    catch
        error:_Reason -> [];
        exit:_Reason -> []
    end.

-spec normalize_buffer_event(term()) -> {true, map()} | false.
normalize_buffer_event(Event) when is_map(Event) ->
    case maps:get(seq, Event, -1) of
        Seq when is_integer(Seq), Seq >= 0 -> {true, Event};
        _ -> false
    end;
normalize_buffer_event(_) ->
    false.

-spec init_buffer_bytes(term(), [map()]) -> non_neg_integer().
init_buffer_bytes(_Raw, []) ->
    0;
init_buffer_bytes(Raw, Buffer) when is_map(Raw) ->
    restored_deque_bytes(Raw, Buffer);
init_buffer_bytes(_Raw, Buffer) ->
    replay_buffer_bytes(Buffer).

-spec restored_deque_bytes(map(), [map()]) -> non_neg_integer().
restored_deque_bytes(Raw, Buffer) ->
    try limited_deque:bytes(eqwalizer:dynamic_cast(Raw)) of
        Bytes when is_integer(Bytes), Bytes >= 0 -> Bytes;
        _ -> replay_buffer_bytes(Buffer)
    catch
        error:_Reason -> replay_buffer_bytes(Buffer);
        exit:_Reason -> replay_buffer_bytes(Buffer)
    end.

-spec replay_buffer_bytes([map()]) -> non_neg_integer().
replay_buffer_bytes(Buffer) ->
    WordSize = erlang:system_info(wordsize),
    lists:foldl(
        fun(Event, Acc) -> trunc(Acc + erts_debug:flat_size(Event) * WordSize) end,
        0,
        Buffer
    ).

-spec replay_payload_bytes([map()]) -> non_neg_integer().
replay_payload_bytes(Buffer) ->
    lists:foldl(fun(Event, Acc) -> trunc(Acc + entry_payload_bytes(Event)) end, 0, Buffer).

-spec entry_payload_bytes(term()) -> non_neg_integer().
entry_payload_bytes(#{data := {pre_encoded, Payload}}) when is_binary(Payload) ->
    byte_size(Payload);
entry_payload_bytes(_Event) ->
    0.

-spec build_ignored_events_map(term()) -> #{binary() => true}.
build_ignored_events_map(Events) when is_list(Events) ->
    maps:from_list([{Event, true} || Event <- Events, is_binary(Event)]);
build_ignored_events_map(_) ->
    #{}.

-spec load_private_channels(map() | undefined) -> #{session:channel_id() => map()}.
load_private_channels(Ready) when is_map(Ready) ->
    maps:from_list([
        {ChannelId, C}
     || C <- maps:get(<<"private_channels">>, Ready, []),
        ChannelId <- [type_conv:extract_id(C, <<"id">>)],
        ChannelId =/= undefined
    ]);
load_private_channels(_) ->
    #{}.

-spec load_relationships(map() | undefined) -> #{session:user_id() => integer()}.
load_relationships(Ready) when is_map(Ready) ->
    maps:from_list([
        {UserId, Type}
     || R <- maps:get(<<"relationships">>, Ready, []),
        UserId <- [type_conv:extract_id(R, <<"id">>)],
        Type <- [maps:get(<<"type">>, R, undefined)],
        UserId =/= undefined,
        is_integer(Type)
    ]);
load_relationships(_) ->
    #{}.

-spec ensure_bot_ready_map(term()) -> map().
ensure_bot_ready_map(undefined) -> #{<<"guilds">> => []};
ensure_bot_ready_map(Ready) when is_map(Ready) -> Ready#{<<"guilds">> => []};
ensure_bot_ready_map(_) -> #{<<"guilds">> => []}.

-spec build_state(map()) -> session_state().
build_state(SessionData) ->
    BuildStartedAt = gateway_timings:start(),
    GwTimings0 = maps:get(gw_timings, SessionData, gateway_timings:new()),
    BaseState0 = extract_fields(SessionData),
    VoiceQueueState = session_voice:init_voice_queue(),
    BaseState = maps:merge(BaseState0, VoiceQueueState),
    GwTimings = gateway_timings:record_function(
        session_init_build_state,
        <<"session_init:build_state/1">>,
        BuildStartedAt,
        GwTimings0
    ),
    gateway_timings:put_state(GwTimings, BaseState).

-spec extract_fields(map()) -> session_state().
extract_fields(#{socket_pid := SocketPid} = D) ->
    Bot = maps:get(bot, D, false),
    Ready = init_ready(Bot, maps:get(ready, D)),
    Buffer = normalize_buffer(maps:get(buffer, D, [])),
    Seq = normalize_seq(maps:get(seq, D, 0)),
    AckSeq = init_ack_seq(normalize_seq(maps:get(ack_seq, D, 0)), Seq),
    GuildIds = normalize_guild_ids(maps:get(guilds, D, [])),
    IsStaff = extract_is_staff(maps:get(user_data, D, #{})),
    Core = extract_core_fields(
        D, Bot, IsStaff, Ready, Buffer, Seq, AckSeq, SocketPid, GuildIds
    ),
    Extra = extract_extra_fields(D, Ready),
    maps:merge(Core, Extra).

-spec extract_core_fields(
    map(),
    boolean(),
    boolean(),
    map() | undefined,
    [map()],
    seq(),
    seq(),
    pid(),
    [guild_id()]
) -> session_state().
extract_core_fields(
    #{
        id := Id,
        user_id := UserId,
        user_data := UserData,
        version := Version,
        token_hash := TokenHash,
        auth_session_id_hash := AuthSessionIdHash,
        properties := Properties,
        status := Status
    } = D,
    Bot,
    IsStaff,
    Ready,
    Buffer,
    Seq,
    AckSeq,
    SocketPid,
    GuildIds
) ->
    #{
        id => Id,
        user_id => UserId,
        user_data => UserData,
        custom_status => maps:get(custom_status, D, null),
        version => Version,
        token_hash => TokenHash,
        auth_session_id_hash => AuthSessionIdHash,
        buffer => Buffer,
        buffer_bytes => init_buffer_bytes(maps:get(buffer, D, []), Buffer),
        replay_payload_bytes => replay_payload_bytes(Buffer),
        seq => Seq,
        ack_seq => AckSeq,
        replay_floor => init_replay_floor(normalize_seq(maps:get(replay_floor, D, 0)), Seq),
        properties => Properties,
        status => Status,
        resume_status => maps:get(resume_status, D, Status),
        afk => maps:get(afk, D, false),
        mobile => maps:get(mobile, D, false),
        presence_pid => undefined,
        presence_mref => undefined,
        socket_pid => SocketPid,
        socket_mref => monitor(process, SocketPid),
        resume_timer => undefined,
        offline_timer => undefined,
        guilds => maps:from_list([{Gid, undefined} || Gid <- GuildIds]),
        calls => #{},
        channels => maps:get(channels, D, load_private_channels(Ready)),
        ready => Ready,
        gw_timings => maps:get(gw_timings, D, gateway_timings:new()),
        bot => Bot,
        shard => maps:get(shard, D, undefined),
        is_staff => IsStaff,
        e2ee_capable => maps:get(e2ee_capable, D, false)
    }.

-spec extract_extra_fields(map(), map() | undefined) -> map().
extract_extra_fields(D, Ready) ->
    #{
        ignored_events => build_ignored_events_map(maps:get(ignored_events, D, [])),
        initial_guild_id => maps:get(initial_guild_id, D, undefined),
        active_guilds => initial_active_guilds(D),
        collected_guild_states => maps:get(collected_guild_states, D, []),
        collected_sessions => maps:get(collected_sessions, D, []),
        collected_presences => maps:get(collected_presences, D, []),
        guild_subscription_state => maps:get(guild_subscription_state, D, #{}),
        relationships => maps:get(relationships, D, load_relationships(Ready)),
        suppress_presence_updates => true,
        pending_presences => [],
        guild_connect_inflight => #{},
        guild_connect_workers => #{},
        guild_connect_timers => #{},
        debounce_reactions => maps:get(debounce_reactions, D, false),
        reaction_buffer => [],
        reaction_buffer_timer => undefined
    }.

-spec initial_active_guilds(map()) -> sets:set(guild_id()).
initial_active_guilds(D) ->
    Explicit = normalize_active_guilds(maps:get(active_guilds, D, [])),
    FromSubscriptions = active_guilds_from_subscription_state(
        maps:get(guild_subscription_state, D, #{})
    ),
    sets:union(Explicit, FromSubscriptions).

-spec active_guilds_from_subscription_state(term()) -> sets:set(guild_id()).
active_guilds_from_subscription_state(Subscriptions) when is_map(Subscriptions) ->
    maps:fold(fun active_guild_from_subscription/3, sets:new(), Subscriptions);
active_guilds_from_subscription_state(_) ->
    sets:new().

-spec active_guild_from_subscription(term(), term(), sets:set(guild_id())) ->
    sets:set(guild_id()).
active_guild_from_subscription(GuildId, #{<<"active">> := true}, Acc) ->
    case snowflake_id:parse_optional(GuildId) of
        Id when is_integer(Id), Id > 0 -> sets:add_element(Id, Acc);
        _ -> Acc
    end;
active_guild_from_subscription(_GuildId, _GuildSubData, Acc) ->
    Acc.

-spec extract_is_staff(map()) -> boolean().
extract_is_staff(UserData) when is_map(UserData) ->
    guild_availability_check:is_user_staff_from_user_data(UserData);
extract_is_staff(_) ->
    false.

-spec init_ready(boolean(), map() | undefined) -> map() | undefined.
init_ready(true, Ready) -> ensure_bot_ready_map(Ready);
init_ready(false, Ready) -> Ready.

-spec init_ack_seq(seq(), seq()) -> seq().
init_ack_seq(AckSeq, Seq) when AckSeq =< Seq -> AckSeq;
init_ack_seq(_AckSeq, Seq) -> Seq.

-spec init_replay_floor(seq(), seq()) -> seq().
init_replay_floor(Floor, Seq) when Floor =< Seq -> Floor;
init_replay_floor(_Floor, Seq) -> Seq.

-spec schedule_timers(session_state()) -> ok.
schedule_timers(#{bot := Bot, guilds := GuildsMap}) ->
    GuildIds = maps:keys(GuildsMap),
    self() ! {presence_connect, 0},
    init_bot_ready_signal(Bot),
    init_prewarm_guilds(Bot, GuildIds),
    lists:foreach(fun(Gid) -> self() ! {guild_connect, Gid, 0} end, GuildIds),
    erlang:send_after(5000, self(), premature_readiness),
    erlang:send_after(10000, self(), enable_presence_updates),
    erlang:send_after(60000, self(), check_ack_lag),
    ok.

-spec init_bot_ready_signal(boolean()) -> ok.
init_bot_ready_signal(true) ->
    self() ! bot_initial_ready,
    ok;
init_bot_ready_signal(false) ->
    ok.

-spec init_prewarm_guilds(boolean(), [guild_id()]) -> ok.
init_prewarm_guilds(true, GuildIds) ->
    spawn(fun() -> safe_prewarm_guild_pid_cache(GuildIds) end),
    ok;
init_prewarm_guilds(false, GuildIds) ->
    prewarm_guild_pid_cache(GuildIds).

-spec safe_prewarm_guild_pid_cache([guild_id()]) -> ok.
safe_prewarm_guild_pid_cache(GuildIds) ->
    try
        prewarm_guild_pid_cache(GuildIds)
    catch
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec prewarm_guild_pid_cache([guild_id()]) -> ok.
prewarm_guild_pid_cache(GuildIds) ->
    ByNode = group_guilds_by_node(GuildIds),
    maps:foreach(fun prewarm_node_guilds/2, ByNode),
    ok.

-spec prewarm_node_guilds(atom(), [guild_id()]) -> ok.
prewarm_node_guilds(Node, Ids) ->
    try
        rpc:call(Node, gateway_rpc_guild, batch_lookup_guild_pids, [Ids], 3000)
    catch
        error:_Reason -> ok;
        exit:_Reason -> ok
    end,
    ok.

-spec group_guilds_by_node([guild_id()]) -> #{atom() => [guild_id()]}.
group_guilds_by_node(GuildIds) ->
    lists:foldl(fun group_guild_by_node/2, #{}, GuildIds).

-spec group_guild_by_node(guild_id(), #{atom() => [guild_id()]}) -> #{atom() => [guild_id()]}.
group_guild_by_node(GuildId, Acc) ->
    case resolve_guild_node(GuildId) of
        {ok, Node} -> add_guild_to_node(Node, GuildId, Acc);
        error -> Acc
    end.

-spec add_guild_to_node(atom(), guild_id(), #{atom() => [guild_id()]}) ->
    #{atom() => [guild_id()]}.
add_guild_to_node(Node, GuildId, Acc) ->
    Existing = maps:get(Node, Acc, []),
    Acc#{Node => [GuildId | Existing]}.

-spec resolve_guild_node(guild_id()) -> {ok, atom()} | error.
resolve_guild_node(GuildId) ->
    try gateway_node_router:owner_node_result(GuildId, guilds) of
        {ok, Node} when is_atom(Node) -> {ok, Node};
        _ -> error
    catch
        error:_Reason -> error;
        exit:_Reason -> error
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_ignored_events_map_test() ->
    ?assertEqual(#{}, build_ignored_events_map([])),
    ?assertEqual(#{<<"TYPING_START">> => true}, build_ignored_events_map([<<"TYPING_START">>])),
    ?assertEqual(
        #{<<"TYPING_START">> => true, <<"PRESENCE_UPDATE">> => true},
        build_ignored_events_map([<<"TYPING_START">>, <<"PRESENCE_UPDATE">>])
    ),
    ?assertEqual(#{}, build_ignored_events_map(not_a_list)),
    ok.

load_private_channels_test() ->
    ?assertEqual(#{}, load_private_channels(undefined)),
    ?assertEqual(#{}, load_private_channels(#{})),
    Ready = #{
        <<"private_channels">> => [
            #{<<"id">> => <<"123">>, <<"type">> => 1},
            #{<<"id">> => <<"456">>, <<"type">> => 3}
        ]
    },
    Channels = load_private_channels(Ready),
    ?assertEqual(2, maps:size(Channels)),
    ?assert(maps:is_key(123, Channels)),
    ?assert(maps:is_key(456, Channels)),
    ok.

load_relationships_test() ->
    ?assertEqual(#{}, load_relationships(undefined)),
    ?assertEqual(#{}, load_relationships(#{})),
    Ready = #{
        <<"relationships">> => [
            #{<<"id">> => <<"100">>, <<"type">> => 1},
            #{<<"id">> => <<"200">>, <<"type">> => 3}
        ]
    },
    Rels = load_relationships(Ready),
    ?assertEqual(2, maps:size(Rels)),
    ?assertEqual(1, maps:get(100, Rels)),
    ?assertEqual(3, maps:get(200, Rels)),
    ok.

load_relationships_uses_top_level_relationship_id_test() ->
    Ready = #{
        <<"relationships">> => [
            #{<<"id">> => <<"300">>, <<"user">> => #{<<"id">> => <<"300">>}, <<"type">> => 1},
            #{<<"id">> => <<"400">>, <<"user">> => #{<<"id">> => <<"400">>}, <<"type">> => 3}
        ]
    },
    Rels = load_relationships(Ready),
    ?assertEqual(2, maps:size(Rels)),
    ?assertEqual(1, maps:get(300, Rels)),
    ?assertEqual(3, maps:get(400, Rels)),
    ok.

build_state_loads_relationship_ids_from_ready_test() ->
    State = build_state(
        base_session_data(#{
            <<"relationships">> => [
                #{
                    <<"id">> => <<"300">>,
                    <<"type">> => 1,
                    <<"user">> => #{<<"id">> => <<"300">>}
                }
            ],
            <<"private_channels">> => [
                #{<<"id">> => <<"700">>, <<"type">> => 1}
            ]
        })
    ),
    ?assertEqual(#{300 => 1}, maps:get(relationships, State)),
    ?assert(maps:is_key(700, maps:get(channels, State))).

build_state_restores_replay_floor_test() ->
    Data = (base_session_data(#{}))#{seq => 20, replay_floor => 7},
    ?assertEqual(7, maps:get(replay_floor, build_state(Data))).

build_state_clamps_replay_floor_to_seq_test() ->
    Data = (base_session_data(#{}))#{seq => 3, replay_floor => 99},
    ?assertEqual(3, maps:get(replay_floor, build_state(Data))).

build_state_defaults_replay_floor_to_zero_test() ->
    ?assertEqual(0, maps:get(replay_floor, build_state(base_session_data(#{})))).

base_session_data(Ready) ->
    #{
        id => <<"session-init-test">>,
        user_id => 1,
        user_data => #{
            <<"id">> => <<"1">>,
            <<"username">> => <<"self">>,
            <<"discriminator">> => <<"0001">>,
            <<"avatar">> => null,
            <<"flags">> => 0
        },
        version => 1,
        token_hash => <<"token-hash">>,
        auth_session_id_hash => <<"auth-session-hash">>,
        properties => #{},
        status => online,
        socket_pid => self(),
        guilds => [],
        ready => Ready
    }.

ensure_bot_ready_map_test() ->
    ?assertEqual(#{<<"guilds">> => []}, ensure_bot_ready_map(undefined)),
    ?assertEqual(
        #{<<"guilds">> => [], <<"user">> => #{}}, ensure_bot_ready_map(#{<<"user">> => #{}})
    ),
    ?assertEqual(#{<<"guilds">> => []}, ensure_bot_ready_map(not_a_map)),
    ok.

normalize_guild_ids_filters_invalid_values_test() ->
    ?assertEqual([1, 2], normalize_guild_ids([1, 0, -1, 2, <<"3">>])),
    ?assertEqual([10], normalize_guild_ids(#{10 => undefined, <<"bad">> => undefined})),
    ?assertEqual([], normalize_guild_ids(undefined)).

normalize_buffer_filters_invalid_entries_test() ->
    Buffer = [#{seq => 1}, #{seq => -1}, #{no_seq => true}, <<"bad">>],
    ?assertEqual([#{seq => 1}], normalize_buffer(Buffer)),
    ?assertEqual([], normalize_buffer(undefined)).

normalize_buffer_accepts_deque_test() ->
    Deque = test_deque([#{seq => 1}, #{seq => 2}]),
    ?assertEqual([#{seq => 1}, #{seq => 2}], normalize_buffer(Deque)).

normalize_buffer_filters_invalid_deque_entries_test() ->
    Deque = test_deque([#{seq => 1}, #{seq => -1}, #{no_seq => true}, <<"bad">>]),
    ?assertEqual([#{seq => 1}], normalize_buffer(Deque)).

normalize_buffer_ignores_non_deque_map_test() ->
    ?assertEqual([], normalize_buffer(#{seq => 1})),
    ?assertEqual([], normalize_buffer(#{})),
    ?assertEqual([], normalize_buffer(undefined)).

build_state_preserves_transferred_deque_buffer_test() ->
    Data = transferred_session_data(test_deque([#{seq => 13}, #{seq => 14}])),
    State = build_state(Data),
    ?assertEqual([#{seq => 13}, #{seq => 14}], maps:get(buffer, State)),
    ?assertEqual(
        replay_buffer_bytes([#{seq => 13}, #{seq => 14}]),
        maps:get(buffer_bytes, State)
    ),
    ?assertEqual(14, maps:get(seq, State)),
    ?assertEqual(12, maps:get(ack_seq, State)).

build_state_reuses_deque_byte_total_test() ->
    Events = [#{seq => 21}, #{seq => 22}],
    Data = transferred_session_data(test_deque(Events)),
    State = build_state(Data),
    ?assertEqual(limited_deque:bytes(test_deque(Events)), maps:get(buffer_bytes, State)),
    ?assertEqual(replay_buffer_bytes(Events), maps:get(buffer_bytes, State)).

replay_buffer_bytes_agrees_with_deque_accounting_test() ->
    Entries = [
        #{seq => 1, event => message_create, data => #{<<"content">> => <<"hi">>}},
        #{seq => 2, event => message_create, data => {pre_encoded, binary:copy(<<"x">>, 70000)}}
    ],
    lists:foreach(
        fun(Entry) ->
            ?assertEqual(
                limited_deque:bytes(limited_deque:from_list([Entry], 4096, 0)),
                replay_buffer_bytes([Entry])
            )
        end,
        Entries
    ).

build_state_walks_list_buffer_bytes_test() ->
    Events = [#{seq => 13}, #{seq => 14}],
    Data = transferred_session_data(Events),
    State = build_state(Data),
    ?assertEqual(replay_buffer_bytes(Events), maps:get(buffer_bytes, State)).

build_state_zeroes_bytes_for_non_deque_map_buffer_test() ->
    Data = transferred_session_data(#{seq => 13}),
    State = build_state(Data),
    ?assertEqual([], maps:get(buffer, State)),
    ?assertEqual(0, maps:get(buffer_bytes, State)).

transferred_session_data(Buffer) ->
    (base_session_data(#{}))#{buffer => Buffer, seq => 14, ack_seq => 12}.

test_deque(Events) ->
    limited_deque:from_list(Events, 4096, 16777216).

-endif.
