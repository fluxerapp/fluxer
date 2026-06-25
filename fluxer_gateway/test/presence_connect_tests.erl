%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_connect_tests).
-typing([eqwalizer]).

-include_lib("eunit/include/eunit.hrl").

session_connect_sends_cached_friend_presence_to_new_session_test() ->
    maybe_start_presence_bus(),
    maybe_start_presence_cache(),
    FriendPresence = #{
        <<"status">> => <<"online">>,
        <<"user">> => #{<<"id">> => <<"2">>}
    },
    ok = presence_cache:put(2, FriendPresence),
    sync_presence_cache(),
    {ok, PresencePid} = presence:start_link(presence_data([2])),
    ConnectReq = #{
        session_id => <<"friend-session">>,
        status => online,
        afk => false,
        mobile => false,
        socket_pid => undefined
    },
    SessionPid = start_session_probe(PresencePid, ConnectReq),
    assert_session_connected(SessionPid),
    assert_probe_dispatch(SessionPid, presence_update, FriendPresence),
    SessionPid ! stop,
    ok = gen_server:stop(PresencePid).

session_connect_sends_live_friend_presence_when_cache_missing_test() ->
    maybe_start_presence_bus(),
    maybe_start_presence_cache(),
    {ManagerPid, ManagerStarted} = maybe_start_presence_manager(),
    WatcherId = 11,
    TargetId = 22,
    {ok, TargetPid} = presence_manager:start_or_lookup(presence_data(TargetId, [])),
    {ok, _TargetSessions} = gen_server:call(
        TargetPid, {session_connect, connect_req(<<"target-session">>)}, 5000
    ),
    sync_presence_cache(),
    ok = presence_cache:delete(TargetId),
    sync_presence_cache(),
    ?assertEqual(not_found, presence_cache:get(TargetId)),
    {ok, PresencePid} = presence:start_link(presence_data(WatcherId, [TargetId])),
    SessionPid = start_session_probe(PresencePid, connect_req(<<"watcher-session">>)),
    assert_session_connected(SessionPid),
    assert_probe_presence(SessionPid, TargetId),
    sync_presence_cache(),
    ?assertMatch({ok, _}, presence_cache:get(TargetId)),
    SessionPid ! stop,
    ok = gen_server:stop(PresencePid),
    ok = gen_server:stop(TargetPid),
    stop_presence_manager(ManagerPid, ManagerStarted).

terminate_session_call_removes_only_matching_sessions_test() ->
    maybe_start_presence_bus(),
    maybe_start_presence_cache(),
    {DropPid, DropRef} = start_terminate_probe(<<"drop_hash">>),
    {KeepPid, KeepRef} = start_terminate_probe(<<"keep_hash">>),
    Sessions = #{
        <<"drop">> => terminate_probe_session(<<"drop">>, DropPid, DropRef),
        <<"keep">> => terminate_probe_session(<<"keep">>, KeepPid, KeepRef)
    },
    {reply, ok, NewState} = presence_connect:handle_terminate_session_call(
        [base64url:encode(<<"drop_hash">>)], terminate_probe_state(Sessions)
    ),
    NewSessions = maps:get(sessions, NewState),
    ?assertEqual(false, maps:is_key(<<"drop">>, NewSessions)),
    ?assertEqual(true, maps:is_key(<<"keep">>, NewSessions)),
    assert_probe_reply(DropPid, terminated),
    assert_probe_reply(KeepPid, ignored),
    KeepPid ! stop.

assert_probe_reply(Pid, Reply) ->
    receive
        {terminate_probe, Pid, Reply} -> ok
    after 1000 ->
        ?assert(false)
    end.

maybe_start_presence_bus() ->
    case whereis(presence_bus) of
        undefined -> start_presence_bus();
        _ -> ok
    end.

start_presence_bus() ->
    case presence_bus:start_link() of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok;
        Other -> Other
    end.

maybe_start_presence_cache() ->
    case whereis(presence_cache) of
        undefined -> start_presence_cache();
        _ -> ok
    end.

start_presence_cache() ->
    case presence_cache:start_link() of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok;
        Other -> Other
    end.

maybe_start_presence_manager() ->
    process_registry:init(),
    case whereis(presence_manager) of
        undefined ->
            case presence_manager:start_link() of
                {ok, Pid} -> {Pid, true};
                {error, {already_started, Pid}} -> {Pid, false}
            end;
        Existing when is_pid(Existing) ->
            {Existing, false}
    end.

stop_presence_manager(_Pid, false) ->
    ok;
stop_presence_manager(Pid, true) ->
    try gen_server:stop(Pid) of
        ok -> ok
    catch
        error:_ -> ok;
        exit:_ -> ok
    end.

sync_presence_cache() ->
    case whereis(presence_cache) of
        Pid when is_pid(Pid) ->
            _ = sys:get_state(Pid),
            ok;
        undefined ->
            ok
    end.

start_session_probe(PresencePid, ConnectReq) ->
    Parent = self(),
    spawn(fun() -> session_probe_connect(PresencePid, ConnectReq, Parent) end).

session_probe_connect(PresencePid, ConnectReq, Parent) ->
    Reply = gen_server:call(PresencePid, {session_connect, ConnectReq}, 5000),
    Parent ! {session_probe_connected, self(), Reply},
    session_probe_loop(Parent).

session_probe_loop(Parent) ->
    receive
        {'$gen_cast', {dispatch, Event, Payload}} ->
            Parent ! {session_probe_dispatch, self(), Event, Payload},
            session_probe_loop(Parent);
        stop ->
            ok
    after infinity ->
        ok
    end.

assert_session_connected(SessionPid) ->
    receive
        {session_probe_connected, SessionPid, {ok, _Sessions}} -> ok
    after 1000 ->
        ?assert(false)
    end.

assert_probe_dispatch(SessionPid, Event, Payload) ->
    receive
        {session_probe_dispatch, SessionPid, Event, Payload} -> ok
    after 1000 ->
        ?assert(false)
    end.

assert_probe_presence(SessionPid, UserId) ->
    receive
        {session_probe_dispatch, SessionPid, presence_update, Payload} ->
            User = maps:get(<<"user">>, Payload, #{}),
            case snowflake_id:parse_maybe(maps:get(<<"id">>, User)) of
                UserId ->
                    ?assertEqual(<<"online">>, maps:get(<<"status">>, Payload));
                _ ->
                    assert_probe_presence(SessionPid, UserId)
            end
    after 1000 ->
        ?assert(false)
    end.

connect_req(SessionId) ->
    #{
        session_id => SessionId,
        status => online,
        afk => false,
        mobile => false,
        socket_pid => undefined
    }.

presence_data(FriendIds) ->
    presence_data(1, FriendIds).

presence_data(UserId, FriendIds) ->
    #{
        user_id => UserId,
        user_data => #{
            <<"id">> => integer_to_binary(UserId),
            <<"username">> => <<"test">>,
            <<"discriminator">> => <<"0001">>,
            <<"avatar">> => null,
            <<"flags">> => 0
        },
        guild_ids => [],
        friend_ids => FriendIds,
        group_dm_recipients => #{},
        status => online,
        custom_status => null
    }.

start_terminate_probe(AuthHash) ->
    Parent = self(),
    Pid = spawn(fun() -> terminate_probe_loop(AuthHash, Parent) end),
    {Pid, monitor(process, Pid)}.

terminate_probe_loop(AuthHash, Parent) ->
    receive
        {'$gen_call', From, {terminate, SessionIdHashes}} ->
            handle_terminate_probe_call(AuthHash, Parent, From, SessionIdHashes);
        {'$gen_cast', _Message} ->
            terminate_probe_loop(AuthHash, Parent);
        stop ->
            ok
    after infinity ->
        ok
    end.

handle_terminate_probe_call(AuthHash, Parent, From, SessionIdHashes) ->
    DecodedHashes = [base64url:decode(Hash) || Hash <- SessionIdHashes],
    case lists:member(AuthHash, DecodedHashes) of
        true ->
            Parent ! {terminate_probe, self(), terminated},
            gen_server:reply(From, terminated);
        false ->
            Parent ! {terminate_probe, self(), ignored},
            gen_server:reply(From, ignored),
            terminate_probe_loop(AuthHash, Parent)
    end.

terminate_probe_session(SessionId, Pid, Ref) ->
    #{
        session_id => SessionId,
        status => online,
        afk => false,
        mobile => false,
        pid => Pid,
        mref => Ref,
        socket_pid => undefined
    }.

terminate_probe_state(Sessions) ->
    #{
        user_id => 1,
        user_data => #{
            <<"id">> => <<"1">>,
            <<"username">> => <<"test">>,
            <<"discriminator">> => <<"0001">>,
            <<"avatar">> => null,
            <<"flags">> => 0
        },
        custom_status => null,
        sessions => Sessions,
        push_buffer => [],
        subscriptions => #{},
        is_bot => false,
        last_published_presence => undefined
    }.
