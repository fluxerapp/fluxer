%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_manager_shard_lookup).
-typing([eqwalizer]).

-export([
    lookup_session/2,
    lookup_session_anywhere/2,
    lookup_or_rehydrate/4,
    lookup_remote_on_nodes/2,
    remote_lookup_nodes/2,
    select_remote_session_pid/1,
    validate_transfer_token/2,
    parse_presence/2,
    parse_guild_ids/1,
    get_presence_custom_status/1,
    normalize_status/1,
    select_initial_status/2,
    normalize_coordinate/1,
    add_coordinates/3
]).

-export_type([session_id/0, lookup_reply/0, rehydrate_lookup_reply/0, state/0]).

-define(REMOTE_SESSION_LOOKUP_TIMEOUT_MS, 1000).

-type session_id() :: binary().
-type lookup_reply() :: {ok, pid()} | {error, not_found}.
-type rehydrate_lookup_reply() :: {ok, pid()} | {error, not_found} | {error, invalid_token}.
-type session_ref() :: {pid(), reference()}.
-type state() :: #{
    sessions := #{session_id() => session_ref()},
    pending_identifies := map(),
    identify_workers := map(),
    shard_index := non_neg_integer(),
    _ => _
}.

-spec lookup_session(session_id(), state()) -> {lookup_reply(), state()}.
lookup_session(SessionId, State) ->
    Sessions = maps:get(sessions, State),
    case maps:get(SessionId, Sessions, undefined) of
        {Pid, Ref} ->
            handle_found_session(SessionId, Pid, Ref, Sessions, State);
        undefined ->
            handle_missing_session(SessionId, Sessions, State)
    end.

-spec handle_found_session(session_id(), pid(), reference(), map(), state()) ->
    {lookup_reply(), state()}.
handle_found_session(SessionId, Pid, Ref, _Sessions, State) ->
    case process_liveness:is_alive(Pid) of
        true ->
            {{ok, Pid}, State};
        false ->
            demonitor(Ref, [flush]),
            SessionName = process_registry:build_process_key(session, SessionId),
            process_registry:safe_unregister(SessionName),
            NewState = session_manager_shard_session_index:remove(SessionId, State),
            lookup_session(SessionId, NewState)
    end.

-spec handle_missing_session(session_id(), map(), state()) ->
    {lookup_reply(), state()}.
handle_missing_session(SessionId, _Sessions, State) ->
    SessionName = process_registry:build_process_key(session, SessionId),
    case process_registry:registry_whereis(SessionName) of
        undefined ->
            {{error, not_found}, State};
        Pid ->
            Ref = monitor(process, Pid),
            NewState = session_manager_shard_session_index:put(SessionId, {Pid, Ref}, State),
            {{ok, Pid}, NewState}
    end.

-spec lookup_session_anywhere(session_id(), state()) -> {lookup_reply(), state()}.
lookup_session_anywhere(SessionId, State) ->
    case lookup_session(SessionId, State) of
        {{ok, Pid}, NewState} ->
            {{ok, Pid}, NewState};
        {{error, not_found}, NewState} ->
            lookup_remote_live_session(SessionId, NewState)
    end.

-spec lookup_or_rehydrate(session_id(), binary(), pid(), state()) ->
    {rehydrate_lookup_reply(), state()}.
lookup_or_rehydrate(SessionId, Token, SocketPid, State) ->
    case lookup_session_anywhere(SessionId, State) of
        {{ok, Pid}, NewState} ->
            {{ok, Pid}, NewState};
        {{error, not_found}, NewState} ->
            rehydrate_session_from_transfer(SessionId, Token, SocketPid, NewState)
    end.

-spec lookup_remote_live_session(session_id(), state()) ->
    {lookup_reply(), state()}.
lookup_remote_live_session(SessionId, State) ->
    ActiveNodes = gateway_node_router:active_nodes(sessions),
    Nodes = remote_lookup_nodes(SessionId, ActiveNodes),
    SessionName = process_registry:build_process_key(session, SessionId),
    case lookup_remote_on_nodes(Nodes, SessionName) of
        {ok, Pid} -> {{ok, Pid}, State};
        {error, not_found} -> {{error, not_found}, State}
    end.

-spec lookup_remote_on_nodes([node()], process_registry:process_key()) ->
    lookup_reply().
lookup_remote_on_nodes([], _SessionName) ->
    {error, not_found};
lookup_remote_on_nodes([OwnerNode | Nodes], SessionName) ->
    case lookup_remote_on_node(OwnerNode, SessionName) of
        {ok, Pid} -> {ok, Pid};
        {error, not_found} -> lookup_remote_on_nodes_fanout(Nodes, SessionName)
    end.

-spec lookup_remote_on_node(node(), process_registry:process_key()) ->
    lookup_reply().
lookup_remote_on_node(Node, SessionName) ->
    case
        rpc:call(
            Node,
            process_registry,
            registry_whereis,
            [SessionName],
            ?REMOTE_SESSION_LOOKUP_TIMEOUT_MS
        )
    of
        Pid when is_pid(Pid) -> {ok, Pid};
        _ -> {error, not_found}
    end.

-spec lookup_remote_on_nodes_fanout([node()], process_registry:process_key()) ->
    lookup_reply().
lookup_remote_on_nodes_fanout([], _SessionName) ->
    {error, not_found};
lookup_remote_on_nodes_fanout(Nodes, SessionName) ->
    {Replies, _BadNodes} = rpc:multicall(
        Nodes,
        process_registry,
        registry_whereis,
        [SessionName],
        ?REMOTE_SESSION_LOOKUP_TIMEOUT_MS
    ),
    select_remote_session_pid(Replies).

-spec remote_lookup_nodes(session_id(), [node()]) -> [node()].
remote_lookup_nodes(SessionId, ActiveNodes) ->
    Nodes = [N || N <- lists:usort(ActiveNodes), is_atom(N), N =/= node()],
    prioritize_owner_node(SessionId, ActiveNodes, Nodes).

-spec prioritize_owner_node(session_id(), [node()], [node()]) -> [node()].
prioritize_owner_node(_SessionId, _ActiveNodes, []) ->
    [];
prioritize_owner_node(SessionId, ActiveNodes, Nodes) ->
    OwnerNode = gateway_node_router:select_owner_node(SessionId, ActiveNodes),
    case lists:member(OwnerNode, Nodes) of
        true -> [OwnerNode | lists:delete(OwnerNode, Nodes)];
        false -> Nodes
    end.

-spec select_remote_session_pid([term()]) -> lookup_reply().
select_remote_session_pid([]) ->
    {error, not_found};
select_remote_session_pid([Pid | _Replies]) when is_pid(Pid) ->
    {ok, Pid};
select_remote_session_pid([_Reply | Replies]) ->
    select_remote_session_pid(Replies).

-spec rehydrate_session_from_transfer(session_id(), binary(), pid(), state()) ->
    {rehydrate_lookup_reply(), state()}.
rehydrate_session_from_transfer(SessionId, Token, SocketPid, State) ->
    case session_state_transfer:pop_state(SessionId) of
        {ok, TransferState} when is_map(TransferState) ->
            restore_or_start(SessionId, Token, SocketPid, TransferState, State);
        _ ->
            rehydrate_from_remote(SessionId, Token, SocketPid, State)
    end.

-spec rehydrate_from_remote(session_id(), binary(), pid(), state()) ->
    {rehydrate_lookup_reply(), state()}.
rehydrate_from_remote(SessionId, Token, SocketPid, State) ->
    ActiveNodes = gateway_node_router:active_nodes(sessions),
    OwnerNode = gateway_node_router:select_owner_node(SessionId, ActiveNodes),
    IsLocal = OwnerNode =:= node() orelse not lists:member(OwnerNode, ActiveNodes),
    case IsLocal of
        true ->
            {{error, not_found}, State};
        false ->
            fetch_remote_transfer(SessionId, Token, SocketPid, OwnerNode, State)
    end.

-spec fetch_remote_transfer(session_id(), binary(), pid(), node(), state()) ->
    {rehydrate_lookup_reply(), state()}.
fetch_remote_transfer(SessionId, Token, SocketPid, OwnerNode, State) ->
    try gen_server:call({session_state_transfer, OwnerNode}, {pop_state, SessionId}, 3000) of
        {ok, TransferState} when is_map(TransferState) ->
            restore_or_start(SessionId, Token, SocketPid, TransferState, State);
        _ ->
            {{error, not_found}, State}
    catch
        error:_Reason -> {{error, not_found}, State};
        exit:_Reason -> {{error, not_found}, State}
    end.

-spec restore_or_start(session_id(), binary(), pid(), map(), state()) ->
    {rehydrate_lookup_reply(), state()}.
restore_or_start(SessionId, Token, SocketPid, TransferState, State) ->
    case validate_transfer_token(Token, TransferState) of
        ok ->
            do_restore_or_start(SessionId, SocketPid, TransferState, State);
        {error, invalid_token} ->
            _ = restore_transfer_state(SessionId, TransferState),
            {{error, invalid_token}, State}
    end.

-spec do_restore_or_start(session_id(), pid(), map(), state()) ->
    {rehydrate_lookup_reply(), state()}.
do_restore_or_start(SessionId, SocketPid, TransferState, State) ->
    SessionData = TransferState#{id => SessionId, socket_pid => SocketPid},
    Sessions = maps:get(sessions, State),
    {reply, StartReply, NewState} = session_manager_shard_lifecycle:start_session_process(
        SessionData, SessionId, Sessions, State
    ),
    handle_restore_reply(StartReply, SessionId, TransferState, NewState).

-spec handle_restore_reply(term(), session_id(), map(), state()) ->
    {rehydrate_lookup_reply(), state()}.
handle_restore_reply({success, Pid}, _SessionId, _TransferState, NewState) when is_pid(Pid) ->
    {{ok, Pid}, NewState};
handle_restore_reply({error, invalid_token}, SessionId, TransferState, NewState) ->
    _ = restore_transfer_state(SessionId, TransferState),
    {{error, invalid_token}, NewState};
handle_restore_reply({error, _Reason}, SessionId, TransferState, NewState) ->
    _ = restore_transfer_state(SessionId, TransferState),
    {{error, not_found}, NewState}.

-spec validate_transfer_token(binary(), map()) -> ok | {error, invalid_token}.
validate_transfer_token(Token, TransferState) ->
    case maps:get(token_hash, TransferState, undefined) of
        TokenHash when is_binary(TokenHash), TokenHash =/= <<>> ->
            compare_token_hash(Token, TokenHash);
        _ ->
            {error, invalid_token}
    end.

-spec compare_token_hash(binary(), binary()) -> ok | {error, invalid_token}.
compare_token_hash(Token, TokenHash) ->
    HashedInput = utils:hash_token(Token),
    case byte_size(HashedInput) =:= byte_size(TokenHash) of
        true ->
            compare_equal_length_token_hash(HashedInput, TokenHash);
        false ->
            {error, invalid_token}
    end.

-spec compare_equal_length_token_hash(binary(), binary()) -> ok | {error, invalid_token}.
compare_equal_length_token_hash(HashedInput, TokenHash) ->
    case crypto:hash_equals(HashedInput, TokenHash) of
        true -> ok;
        false -> {error, invalid_token}
    end.

-spec restore_transfer_state(session_id(), map()) -> ok.
restore_transfer_state(SessionId, TransferState) ->
    case session_state_transfer:push_state(node(), SessionId, TransferState) of
        ok -> ok;
        {error, _Reason} -> ok
    end.

-spec parse_presence(map(), map()) -> online | offline | idle | dnd | invisible.
parse_presence(Data, IdentifyData) ->
    StoredStatus = get_stored_status(Data),
    PresenceStatus = extract_presence_status(IdentifyData),
    SelectedStatus = select_initial_status(PresenceStatus, StoredStatus),
    normalize_status_atom(utils:parse_status(SelectedStatus)).

-spec extract_presence_status(map()) -> binary() | undefined.
extract_presence_status(IdentifyData) ->
    case map_utils:get_safe(IdentifyData, presence, null) of
        null ->
            undefined;
        Presence when is_map(Presence) ->
            normalize_status(map_utils:get_safe(Presence, <<"status">>, <<"online">>));
        _ ->
            undefined
    end.

-spec parse_guild_ids(map()) -> [integer()].
parse_guild_ids(Data) ->
    Guilds = map_utils:ensure_list(map_utils:get_safe(Data, <<"guilds">>, [])),
    RawIds = [maps:get(<<"id">>, G, undefined) || G <- Guilds, is_map(G)],
    [
        GuildId
     || Id <- RawIds, GuildId <- [utils:binary_to_integer_safe(Id)], is_integer(GuildId)
    ].

-spec get_presence_custom_status(term()) -> map() | null.
get_presence_custom_status(null) ->
    null;
get_presence_custom_status(Map) when is_map(Map) ->
    case map_utils:get_safe(Map, <<"custom_status">>, null) of
        CustomStatus when is_map(CustomStatus) -> CustomStatus;
        null -> null;
        _ -> null
    end;
get_presence_custom_status(_) ->
    null.

-spec get_stored_status(map()) -> binary().
get_stored_status(Data) ->
    case map_utils:get_safe(Data, <<"user_settings">>, null) of
        null -> <<"online">>;
        UserSettings when is_map(UserSettings) -> extract_stored_status(UserSettings);
        _ -> <<"online">>
    end.

-spec extract_stored_status(map()) -> binary().
extract_stored_status(UserSettings) ->
    RawStatus = map_utils:get_safe(UserSettings, <<"status">>, <<"online">>),
    case normalize_status(RawStatus) of
        undefined -> <<"online">>;
        Value -> Value
    end.

-spec select_initial_status(binary() | undefined, binary()) -> binary().
select_initial_status(PresenceStatus, StoredStatus) ->
    case {normalize_status(PresenceStatus), StoredStatus} of
        {undefined, Stored} -> Stored;
        {<<"unknown">>, Stored} -> Stored;
        {<<"online">>, Stored} when Stored =/= <<"online">> -> Stored;
        {Presence, _} -> Presence
    end.

-spec normalize_status(term()) -> binary() | undefined.
normalize_status(undefined) ->
    undefined;
normalize_status(null) ->
    undefined;
normalize_status(Status) when is_binary(Status) -> Status;
normalize_status(Status) when is_atom(Status) ->
    try constants:status_type_atom(Status) of
        Value when is_binary(Value) -> Value
    catch
        error:_Reason -> undefined;
        exit:_Reason -> undefined
    end;
normalize_status(_) ->
    undefined.

-spec normalize_status_atom(term()) -> online | offline | idle | dnd | invisible.
normalize_status_atom(online) -> online;
normalize_status_atom(offline) -> offline;
normalize_status_atom(idle) -> idle;
normalize_status_atom(dnd) -> dnd;
normalize_status_atom(invisible) -> invisible;
normalize_status_atom(_) -> online.

-spec normalize_coordinate(term()) -> term() | undefined.
normalize_coordinate(undefined) -> undefined;
normalize_coordinate(null) -> undefined;
normalize_coordinate(Value) -> Value.

-spec add_coordinates(map(), term(), term()) -> map().
add_coordinates(Request, undefined, undefined) -> Request;
add_coordinates(Request, Lat, undefined) -> Request#{<<"latitude">> => Lat};
add_coordinates(Request, undefined, Lon) -> Request#{<<"longitude">> => Lon};
add_coordinates(Request, Lat, Lon) -> Request#{<<"latitude">> => Lat, <<"longitude">> => Lon}.
