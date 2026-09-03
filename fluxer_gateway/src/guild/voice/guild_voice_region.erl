%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_region).
-typing([eqwalizer]).

-export([switch_voice_region_handler/2]).
-export([switch_voice_region/3]).

-export_type([
    guild_state/0,
    guild_reply/1,
    voice_state/0
]).

-type guild_state() :: map().
-type guild_reply(T) :: {reply, T | {error, atom(), atom()}, guild_state()}.
-type voice_state() :: map().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec switch_voice_region_handler(map(), guild_state()) -> guild_reply(map()).
switch_voice_region_handler(Request, State) ->
    #{channel_id := ChannelId} = Request,
    Channel = guild_voice_member:find_channel_by_id(ChannelId, State),
    case Channel of
        undefined ->
            {reply, gateway_errors:error(voice_channel_not_found), State};
        _ ->
            switch_voice_region_channel_reply(Channel, State)
    end.

-spec switch_voice_region_channel_reply(map(), guild_state()) -> guild_reply(map()).
switch_voice_region_channel_reply(Channel, State) ->
    case map_utils:get_integer(Channel, <<"type">>, undefined) of
        2 -> {reply, #{success => true}, State};
        _ -> {reply, gateway_errors:error(voice_channel_not_voice), State}
    end.

-spec switch_voice_region(integer(), integer(), pid()) -> ok.
switch_voice_region(GuildId, ChannelId, GuildPid) ->
    case guild_voice_server_state:guild_state_call(GuildPid, 10000) of
        State when is_map(State) ->
            do_switch_voice_region(GuildId, ChannelId, GuildPid, State);
        _ ->
            ok
    end.

-spec do_switch_voice_region(integer(), integer(), pid(), map()) -> ok.
do_switch_voice_region(GuildId, ChannelId, GuildPid, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    UsersInChannel = collect_users_in_channel(VoiceStates, ChannelId),
    lists:foreach(
        fun(UserInfo) ->
            maybe_send_region_switch_update(GuildId, ChannelId, GuildPid, State, UserInfo)
        end,
        UsersInChannel
    ).

-spec collect_users_in_channel(map(), integer()) ->
    [{integer(), binary() | undefined, binary(), voice_state()}].
collect_users_in_channel(VoiceStates, ChannelId) ->
    maps:fold(
        fun(ConnectionId, VoiceState, Acc) ->
            collect_user_in_channel(ConnectionId, VoiceState, ChannelId, Acc)
        end,
        [],
        VoiceStates
    ).

-spec maybe_send_region_switch_update(
    integer(),
    integer(),
    pid(),
    map(),
    {integer(), binary() | undefined, binary(), voice_state()}
) -> ok.
maybe_send_region_switch_update(
    _GuildId, _ChannelId, _GuildPid, _State, {_UserId, undefined, _ConnId, _VS}
) ->
    ok;
maybe_send_region_switch_update(
    GuildId, ChannelId, GuildPid, State, {UserId, SessionId, ExistingConnectionId, VoiceState}
) ->
    request_and_broadcast_region_switch(
        GuildId,
        ChannelId,
        UserId,
        SessionId,
        ExistingConnectionId,
        VoiceState,
        GuildPid,
        State
    ).

-spec collect_user_in_channel(binary(), voice_state(), integer(), list()) -> list().
collect_user_in_channel(ConnectionId, VoiceState, ChannelId, Acc) ->
    case voice_state_utils:voice_state_channel_id(VoiceState) of
        ChannelId -> collect_user_voice_state(ConnectionId, VoiceState, Acc);
        _ -> Acc
    end.

-spec collect_user_voice_state(binary(), voice_state(), list()) -> list().
collect_user_voice_state(ConnectionId, VoiceState, Acc) ->
    case voice_state_utils:voice_state_user_id(VoiceState) of
        undefined ->
            Acc;
        UserId ->
            SessionId = maps:get(<<"session_id">>, VoiceState, undefined),
            [{UserId, SessionId, ConnectionId, VoiceState} | Acc]
    end.

-spec request_and_broadcast_region_switch(
    integer(),
    integer(),
    integer(),
    binary(),
    binary(),
    voice_state(),
    pid(),
    map()
) -> ok.
request_and_broadcast_region_switch(
    GuildId,
    ChannelId,
    UserId,
    SessionId,
    ExistingConnectionId,
    ExistingVoiceState,
    GuildPid,
    State
) ->
    VoicePerms = voice_utils:compute_voice_permissions(UserId, ChannelId, State),
    TokenNonce = voice_utils:generate_token_nonce(),
    Lat = maps:get(<<"latitude">>, ExistingVoiceState, undefined),
    Lng = maps:get(<<"longitude">>, ExistingVoiceState, undefined),
    TokenResult = guild_voice_connection:request_voice_token(
        GuildId,
        ChannelId,
        UserId,
        ExistingConnectionId,
        VoicePerms,
        TokenNonce,
        Lat,
        Lng
    ),
    Switch = #{
        guild_id => GuildId,
        channel_id => ChannelId,
        user_id => UserId,
        session_id => SessionId,
        existing_voice_state => ExistingVoiceState,
        guild_pid => GuildPid,
        state => State,
        token_nonce => TokenNonce
    },
    handle_region_token_result(TokenResult, Switch).

-spec handle_region_token_result({ok, map()} | {error, term()}, map()) -> ok.
handle_region_token_result({ok, TokenData}, Switch) ->
    broadcast_region_switch(
        maps:get(guild_id, Switch),
        maps:get(channel_id, Switch),
        maps:get(user_id, Switch),
        maps:get(session_id, Switch),
        maps:get(existing_voice_state, Switch),
        maps:get(guild_pid, Switch),
        maps:get(state, Switch),
        maps:get(token_nonce, Switch),
        TokenData
    );
handle_region_token_result({error, _Reason}, _Switch) ->
    ok.

-spec broadcast_region_switch(
    integer(), integer(), integer(), binary(), voice_state(), pid(), map(), binary(), map()
) -> ok.
broadcast_region_switch(
    GuildId,
    ChannelId,
    UserId,
    SessionId,
    ExistingVoiceState,
    GuildPid,
    State,
    TokenNonce,
    TokenData
) ->
    Token = maps:get(token, TokenData),
    Endpoint = maps:get(endpoint, TokenData),
    ConnectionId = maps:get(connection_id, TokenData),
    PendingMetadata = build_pending_metadata(
        UserId, GuildId, ChannelId, SessionId, ExistingVoiceState, TokenNonce, TokenData
    ),
    _ = store_pending_connection(GuildId, GuildPid, ConnectionId, PendingMetadata),
    guild_voice_broadcast:broadcast_voice_server_update_to_session(
        GuildId, ChannelId, SessionId, Token, Endpoint, ConnectionId, State
    ).

-spec build_pending_metadata(
    integer(), integer(), integer(), binary(), voice_state(), binary(), map()
) -> map().
build_pending_metadata(
    UserId, GuildId, ChannelId, SessionId, ExistingVoiceState, TokenNonce, TokenData
) ->
    Now = erlang:system_time(millisecond),
    #{
        user_id => UserId,
        guild_id => GuildId,
        channel_id => ChannelId,
        session_id => SessionId,
        self_mute => maps:get(<<"self_mute">>, ExistingVoiceState, false),
        self_deaf => maps:get(<<"self_deaf">>, ExistingVoiceState, false),
        self_video => maps:get(<<"self_video">>, ExistingVoiceState, false),
        self_stream => maps:get(<<"self_stream">>, ExistingVoiceState, false),
        is_mobile => maps:get(<<"is_mobile">>, ExistingVoiceState, false),
        server_mute => maps:get(<<"mute">>, ExistingVoiceState, false),
        server_deaf => maps:get(<<"deaf">>, ExistingVoiceState, false),
        member => maps:get(<<"member">>, ExistingVoiceState, #{}),
        latitude => maps:get(<<"latitude">>, ExistingVoiceState, undefined),
        longitude => maps:get(<<"longitude">>, ExistingVoiceState, undefined),
        region_id => maps:get(region_id, TokenData, undefined),
        server_id => maps:get(server_id, TokenData, undefined),
        viewer_stream_keys => [],
        token_nonce => TokenNonce,
        created_at => Now,
        expires_at => Now + 30000
    }.

-spec store_pending_connection(integer(), pid(), binary(), map()) -> ok.
store_pending_connection(GuildId, GuildPid, ConnectionId, Metadata) ->
    TargetPid = resolve_voice_server(GuildId, GuildPid),
    gen_server:call(
        TargetPid,
        {store_pending_connection, ConnectionId, Metadata},
        10000
    ).

-spec resolve_voice_server(integer(), pid()) -> pid().
resolve_voice_server(GuildId, FallbackPid) ->
    guild_voice_server:resolve(GuildId, FallbackPid).

-ifdef(TEST).

switch_voice_region_handler_not_found_test() ->
    State = #{data => #{<<"channels">> => []}},
    Request = #{channel_id => 999},
    {reply, Error, _} = switch_voice_region_handler(Request, State),
    ?assertEqual({error, not_found, voice_channel_not_found}, Error).

switch_voice_region_handler_not_voice_test() ->
    State = #{
        data => #{
            <<"channels">> => [
                #{<<"id">> => <<"100">>, <<"type">> => 0}
            ]
        }
    },
    Request = #{channel_id => 100},
    {reply, Error, _} = switch_voice_region_handler(Request, State),
    ?assertEqual({error, validation_error, voice_channel_not_voice}, Error).

switch_voice_region_handler_success_test() ->
    State = #{
        data => #{
            <<"channels">> => [
                #{<<"id">> => <<"100">>, <<"type">> => 2}
            ]
        }
    },
    Request = #{channel_id => 100},
    {reply, #{success := true}, _} = switch_voice_region_handler(Request, State).

collect_users_in_channel_test() ->
    VoiceState = #{
        <<"channel_id">> => <<"100">>,
        <<"user_id">> => <<"10">>,
        <<"session_id">> => <<"sess1">>
    },
    VoiceStates = #{<<"conn1">> => VoiceState},
    Result = collect_users_in_channel(VoiceStates, 100),
    ?assertEqual(1, length(Result)),
    [{UserId, SessionId, ConnectionId, _}] = Result,
    ?assertEqual(10, UserId),
    ?assertEqual(<<"sess1">>, SessionId),
    ?assertEqual(<<"conn1">>, ConnectionId).

switch_voice_region_reads_guild_state_once_test() ->
    with_region_stubs(fun() ->
        {Calls, _Dispatches, _Pending} = run_region_switch(fun switch_voice_region/3),
        ?assertEqual(1, Calls)
    end).

switch_voice_region_matches_per_user_refetch_reference_test() ->
    with_region_stubs(fun region_reference_equivalence_scenario/0).

region_reference_equivalence_scenario() ->
    {RefCalls, RefDispatches, RefPending} = run_region_switch(
        fun reference_switch_voice_region/3
    ),
    {NewCalls, NewDispatches, NewPending} = run_region_switch(fun switch_voice_region/3),
    ?assertEqual(3, RefCalls),
    ?assertEqual(1, NewCalls),
    ?assertEqual(2, length(RefDispatches)),
    ?assertEqual(RefDispatches, NewDispatches),
    ?assertEqual(RefPending, NewPending).

switch_voice_region_skips_users_without_a_session_id_test() ->
    with_region_stubs(fun region_missing_session_id_scenario/0).

region_missing_session_id_scenario() ->
    Anonymous = maps:remove(
        <<"session_id">>, region_voice_state(<<"conn-c">>, 10, <<"sess1">>)
    ),
    State = region_state(),
    Stateless = State#{voice_states => #{<<"conn-c">> => Anonymous}},
    {Calls, Dispatches, Pending} = run_region_switch_with(
        fun switch_voice_region/3, Stateless
    ),
    ?assertEqual(1, Calls),
    ?assertEqual([], Dispatches),
    ?assertEqual([], Pending).

%% The pre-change implementation, kept as a live oracle: it refetched the guild state once per
%% user in the channel instead of reusing the one the enumeration already fetched.
reference_switch_voice_region(GuildId, ChannelId, GuildPid) ->
    case guild_voice_server_state:guild_state_call(GuildPid, 10000) of
        State when is_map(State) ->
            reference_do_switch_voice_region(GuildId, ChannelId, GuildPid, State);
        _ ->
            ok
    end.

reference_do_switch_voice_region(GuildId, ChannelId, GuildPid, State) ->
    VoiceStates = voice_state_utils:voice_states(State),
    UsersInChannel = collect_users_in_channel(VoiceStates, ChannelId),
    lists:foreach(
        fun(UserInfo) -> reference_send_update(GuildId, ChannelId, GuildPid, UserInfo) end,
        UsersInChannel
    ).

reference_send_update(_GuildId, _ChannelId, _GuildPid, {_UserId, undefined, _ConnId, _VS}) ->
    ok;
reference_send_update(GuildId, ChannelId, GuildPid, {UserId, SessionId, ConnId, VoiceState}) ->
    case guild_voice_server_state:guild_state_call(GuildPid, 10000) of
        State when is_map(State) ->
            request_and_broadcast_region_switch(
                GuildId, ChannelId, UserId, SessionId, ConnId, VoiceState, GuildPid, State
            );
        _ ->
            ok
    end.

run_region_switch(Fun) ->
    run_region_switch_with(Fun, region_state()).

run_region_switch_with(Fun, State) ->
    drain_region_dispatches(),
    drain_region_pending(),
    erlang:put(region_token_index, 0),
    GuildPid = spawn_region_guild(State),
    try
        Fun(999, 200, GuildPid),
        {region_guild_calls(GuildPid), drain_region_dispatches(), drain_region_pending()}
    after
        GuildPid ! stop
    end.

with_region_stubs(Fun) ->
    meck:new(rpc_client, [passthrough, no_link, non_strict]),
    meck:expect(rpc_client, call, fun(_Request) -> {ok, next_region_token()} end),
    meck:new(guild_voice_server, [passthrough, no_link]),
    meck:expect(guild_voice_server, resolve, fun(_GuildId, GuildPid) -> GuildPid end),
    try
        Fun()
    after
        meck:unload(guild_voice_server),
        meck:unload(rpc_client)
    end.

next_region_token() ->
    Index = erlang:get(region_token_index) + 1,
    erlang:put(region_token_index, Index),
    #{
        <<"token">> => <<"tok-", (integer_to_binary(Index))/binary>>,
        <<"endpoint">> => <<"wss://voice.example">>,
        <<"connectionId">> => <<"new-conn-", (integer_to_binary(Index))/binary>>,
        <<"regionId">> => <<"us-east">>,
        <<"serverId">> => <<"voice-1">>
    }.

spawn_region_guild(State) ->
    Owner = self(),
    spawn(fun() -> region_guild_loop(State, Owner, 0) end).

region_guild_loop(State, Owner, Calls) ->
    receive
        {'$gen_call', From, {get_voice_guild_state}} ->
            gen_server:reply(From, State),
            region_guild_loop(State, Owner, Calls + 1);
        {'$gen_call', From, {store_pending_connection, ConnId, Meta}} ->
            Owner ! {region_pending, ConnId, Meta},
            gen_server:reply(From, ok),
            region_guild_loop(State, Owner, Calls);
        {'$gen_call', From, _Other} ->
            gen_server:reply(From, ok),
            region_guild_loop(State, Owner, Calls);
        {region_calls, Caller} ->
            Caller ! {region_calls, Calls},
            region_guild_loop(State, Owner, Calls);
        stop ->
            ok
    after 30000 ->
        ok
    end.

region_guild_calls(GuildPid) ->
    GuildPid ! {region_calls, self()},
    receive
        {region_calls, Calls} -> Calls
    after 1000 ->
        error(region_guild_unresponsive)
    end.

drain_region_dispatches() ->
    receive
        {'$gen_cast', {dispatch, voice_server_update, Payload}} ->
            [Payload | drain_region_dispatches()]
    after 100 ->
        []
    end.

%% token_nonce and the timestamps are freshly generated per call by design, so they are
%% excluded before the reference and the threaded run are compared.
drain_region_pending() ->
    receive
        {region_pending, ConnId, Meta} ->
            Stable = maps:without([token_nonce, created_at, expires_at], Meta),
            [{ConnId, Stable} | drain_region_pending()]
    after 100 ->
        []
    end.

region_state() ->
    #{
        id => 999,
        data => #{
            <<"id">> => <<"999">>,
            <<"guild">> => #{<<"owner_id">> => <<"10">>},
            <<"channels">> => [#{<<"id">> => <<"200">>, <<"type">> => 2}],
            <<"members">> => [region_member(10), region_member(11)]
        },
        sessions => #{
            <<"sess1">> => #{user_id => 10, pid => self(), pending_connect => false},
            <<"sess2">> => #{user_id => 11, pid => self(), pending_connect => false}
        },
        voice_states => #{
            <<"conn-a">> => region_voice_state(<<"conn-a">>, 10, <<"sess1">>),
            <<"conn-b">> => region_voice_state(<<"conn-b">>, 11, <<"sess2">>)
        },
        pending_voice_connections => #{}
    }.

region_member(UserId) ->
    #{
        <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
        <<"mute">> => false,
        <<"deaf">> => false
    }.

region_voice_state(ConnId, UserId, SessionId) ->
    #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"guild_id">> => <<"999">>,
        <<"channel_id">> => <<"200">>,
        <<"connection_id">> => ConnId,
        <<"session_id">> => SessionId,
        <<"self_mute">> => false,
        <<"self_deaf">> => true,
        <<"member">> => region_member(UserId)
    }.

build_pending_metadata_preserves_voice_routing_test() ->
    Metadata = build_pending_metadata(
        10,
        20,
        30,
        <<"sess1">>,
        #{},
        <<"nonce">>,
        #{region_id => <<"us-east">>, server_id => <<"voice-ewr-1">>}
    ),
    ?assertEqual(<<"us-east">>, maps:get(region_id, Metadata)),
    ?assertEqual(<<"voice-ewr-1">>, maps:get(server_id, Metadata)).

-endif.
