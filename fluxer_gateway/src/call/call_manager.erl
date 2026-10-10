%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(call_manager).
-typing([eqwalizer]).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([start_link/0, local_call_ids/0, handoff_to_topology/1]).
-export([lookup/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type channel_id() :: integer().
-type call_data() :: map().
-type state() :: #{calls := map()}.
-type handoff_result() :: #{attempted := non_neg_integer(), handed_off := non_neg_integer()}.
-type call_request() ::
    {create, channel_id(), call_data()}
    | {lookup, channel_id()}
    | {terminate_call, channel_id()}
    | {start_transferred, channel_id(), map()}
    | {stop_call, channel_id(), term()}
    | {handoff_to_topology, [node()]}
    | get_local_count
    | get_global_count
    | list_local_call_ids.
-type info_message() :: {'DOWN', reference(), process, pid(), term()}.

-define(CALL_LOOKUP_TIMEOUT, 5000).
-define(CALL_CREATE_TIMEOUT, 10000).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    normalize_start_link(gen_server:start_link({local, ?MODULE}, ?MODULE, [], [])).

-spec local_call_ids() -> [integer()].
local_call_ids() ->
    try gen_server:call(?MODULE, list_local_call_ids, 5000) of
        {ok, Ids} when is_list(Ids) -> Ids;
        _ -> []
    catch
        exit:_ -> []
    end.

-spec handoff_to_topology([node()]) ->
    handoff_result() | {error, timeout | unavailable}.
handoff_to_topology(TargetNodes) ->
    Result = safe_local_call({handoff_to_topology, TargetNodes}, ?CALL_CREATE_TIMEOUT),
    decode_handoff_result(Result).

-spec lookup(channel_id()) -> {ok, pid()} | {error, not_found | timeout | unavailable}.
lookup(ChannelId) ->
    decode_lookup_result(route_request({lookup, ChannelId}, ChannelId, ?CALL_LOOKUP_TIMEOUT)).

-spec normalize_start_link(gen_server:start_ret()) -> {ok, pid()} | {error, term()}.
normalize_start_link({ok, Pid}) ->
    {ok, Pid};
normalize_start_link({error, Reason}) ->
    {error, Reason};
normalize_start_link(ignore) ->
    {error, ignored}.

-spec decode_handoff_result(term()) -> handoff_result() | {error, timeout | unavailable}.
decode_handoff_result(#{attempted := Attempted, handed_off := HandedOff}) when
    is_integer(Attempted), Attempted >= 0, is_integer(HandedOff), HandedOff >= 0
->
    #{attempted => Attempted, handed_off => HandedOff};
decode_handoff_result({error, timeout}) ->
    {error, timeout};
decode_handoff_result(_Result) ->
    {error, unavailable}.

-spec decode_lookup_result(term()) -> {ok, pid()} | {error, not_found | timeout | unavailable}.
decode_lookup_result({ok, Pid}) when is_pid(Pid) ->
    {ok, Pid};
decode_lookup_result({error, not_found}) ->
    {error, not_found};
decode_lookup_result({error, timeout}) ->
    {error, timeout};
decode_lookup_result({error, unavailable}) ->
    {error, unavailable};
decode_lookup_result(_Result) ->
    {error, unavailable}.

-spec decode_call_request(term()) -> {ok, call_request()} | error.
decode_call_request({create, ChannelId, CallData}) when
    is_integer(ChannelId), is_map(CallData)
->
    {ok, {create, ChannelId, CallData}};
decode_call_request({lookup, ChannelId}) when is_integer(ChannelId) ->
    {ok, {lookup, ChannelId}};
decode_call_request({terminate_call, ChannelId}) when is_integer(ChannelId) ->
    {ok, {terminate_call, ChannelId}};
decode_call_request({start_transferred, ChannelId, TransferState}) when
    is_integer(ChannelId), is_map(TransferState)
->
    {ok, {start_transferred, ChannelId, TransferState}};
decode_call_request({stop_call, ChannelId, Reason}) when is_integer(ChannelId) ->
    {ok, {stop_call, ChannelId, Reason}};
decode_call_request({handoff_to_topology, TargetNodes}) when is_list(TargetNodes) ->
    case decode_node_list(TargetNodes) of
        {ok, DecodedNodes} -> {ok, {handoff_to_topology, DecodedNodes}};
        error -> error
    end;
decode_call_request(get_local_count) ->
    {ok, get_local_count};
decode_call_request(get_global_count) ->
    {ok, get_global_count};
decode_call_request(list_local_call_ids) ->
    {ok, list_local_call_ids};
decode_call_request(_) ->
    error.

-spec decode_node_list(term()) -> {ok, [node()]} | error.
decode_node_list(TargetNodes) when is_list(TargetNodes) ->
    decode_node_list(TargetNodes, []);
decode_node_list(_TargetNodes) ->
    error.

-spec decode_node_list([term()], [node()]) -> {ok, [node()]} | error.
decode_node_list([], Acc) ->
    {ok, lists:reverse(Acc)};
decode_node_list([Node | Rest], Acc) when is_atom(Node) ->
    decode_node_list(Rest, [Node | Acc]);
decode_node_list([_Node | _Rest], _Acc) ->
    error.

-spec decode_info_message(term()) -> {ok, info_message()} | error.
decode_info_message({'DOWN', Ref, process, Pid, Reason}) when is_reference(Ref), is_pid(Pid) ->
    {ok, {'DOWN', Ref, process, Pid, Reason}};
decode_info_message(_) ->
    error.

-spec route_request(term(), channel_id(), timeout()) -> term().
route_request(Request, ChannelId, Timeout) ->
    case fluxer_gateway_sup:role_enabled(calls) of
        true ->
            safe_local_call(Request, Timeout);
        false ->
            route_owner_request(ChannelId, Request, Timeout)
    end.

-spec route_owner_request(channel_id(), term(), timeout()) -> term().
route_owner_request(ChannelId, Request, Timeout) ->
    case resolve_owner_node(ChannelId) of
        {ok, OwnerNode} -> call_manager_node(OwnerNode, Request, Timeout);
        {error, unavailable} -> {error, unavailable}
    end.

-spec resolve_owner_node(channel_id()) -> {ok, node()} | {error, unavailable}.
resolve_owner_node(ChannelId) ->
    try call_owner_node_result(ChannelId) of
        {ok, Node} when is_atom(Node) -> {ok, Node};
        {error, _Reason} -> {error, unavailable}
    catch
        throw:_ -> {error, unavailable};
        error:_ -> {error, unavailable};
        exit:_ -> {error, unavailable}
    end.

-spec call_owner_node_result(channel_id()) -> {ok, node()} | {error, term()}.
call_owner_node_result(ChannelId) ->
    case gateway_node_router:owner_node_result(ChannelId, calls) of
        {ok, Node} when is_atom(Node) -> {ok, Node};
        {error, Reason} -> {error, Reason}
    end.

-spec init([]) -> {ok, state(), hibernate}.
init([]) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 10),
    _ = ets:new(call_pid_cache, [
        named_table, public, set, {read_concurrency, true}, {write_concurrency, true}
    ]),
    {ok, #{calls => #{}}, hibernate}.

-spec handle_call(term(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call(Request, From, State) ->
    case decode_call_request(Request) of
        {ok, CallRequest} -> handle_call_request(CallRequest, From, State);
        error -> {reply, ok, State}
    end.

-spec handle_call_request(call_request(), gen_server:from(), state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call_request({create, ChannelId, CallData} = Request, From, State) ->
    handle_owner_call(
        ChannelId,
        Request,
        From,
        ?CALL_CREATE_TIMEOUT,
        fun() -> call_manager_ops:do_create_call(ChannelId, CallData, State) end,
        State
    );
handle_call_request({lookup, ChannelId} = Request, From, State) ->
    handle_owner_call(
        ChannelId,
        Request,
        From,
        ?CALL_LOOKUP_TIMEOUT,
        fun() -> call_manager_ops:do_lookup_call(ChannelId, State) end,
        State
    );
handle_call_request({terminate_call, ChannelId} = Request, From, State) ->
    handle_owner_call(
        ChannelId,
        Request,
        From,
        ?CALL_LOOKUP_TIMEOUT,
        fun() -> call_manager_ops:do_terminate_call(ChannelId, State) end,
        State
    );
handle_call_request({start_transferred, ChannelId, TransferState}, _From, State) ->
    call_manager_ops:do_start_transferred_call(ChannelId, TransferState, State);
handle_call_request({stop_call, ChannelId, Reason}, _From, State) ->
    call_manager_ops:do_stop_call(ChannelId, Reason, State);
handle_call_request({handoff_to_topology, TargetNodes}, _From, State) ->
    {Result, NewState} = call_manager_ops:do_handoff_to_topology(TargetNodes, State),
    {reply, Result, NewState};
handle_call_request(get_local_count, _From, #{calls := Calls} = State) ->
    {reply, {ok, process_registry:get_count(Calls)}, State};
handle_call_request(get_global_count, _From, #{calls := Calls} = State) ->
    {reply, {ok, process_registry:get_count(Calls)}, State};
handle_call_request(list_local_call_ids, _From, #{calls := Calls} = State) ->
    {reply, {ok, call_manager_ops:collect_active_call_ids(Calls)}, State}.

-spec handle_owner_call(
    channel_id(),
    term(),
    gen_server:from(),
    timeout(),
    fun(() -> {reply, term(), state()}),
    state()
) -> {reply, term(), state()} | {noreply, state()}.
handle_owner_call(ChannelId, Request, From, Timeout, LocalFun, State) ->
    case owner_scope(ChannelId) of
        local ->
            LocalFun();
        {remote, OwnerNode} ->
            reply_from_owner(OwnerNode, Request, From, Timeout, State);
        unavailable ->
            {reply, {error, unavailable}, State}
    end.

-spec reply_from_owner(node(), term(), gen_server:from(), timeout(), state()) ->
    {noreply, state()}.
reply_from_owner(OwnerNode, Request, From, Timeout, State) ->
    spawn(fun() ->
        Reply = call_manager_node(OwnerNode, Request, Timeout),
        gen_server:reply(From, Reply)
    end),
    {noreply, State}.

-spec owner_scope(channel_id()) -> local | {remote, node()} | unavailable.
owner_scope(ChannelId) ->
    LocalNode = node(),
    try call_owner_node_result(ChannelId) of
        {ok, OwnerNode} when OwnerNode =:= LocalNode -> local;
        {ok, OwnerNode} when is_atom(OwnerNode) -> {remote, OwnerNode};
        {error, _Reason} -> unavailable
    catch
        throw:_ -> unavailable;
        error:_ -> unavailable;
        exit:_ -> unavailable
    end.

-spec call_manager_node(node(), term(), timeout()) -> term().
call_manager_node(TargetNode, Request, Timeout) ->
    try
        gen_server:call(call_manager_server_ref(TargetNode), Request, Timeout)
    catch
        exit:{timeout, _} -> {error, timeout};
        exit:{nodedown, _} -> {error, unavailable};
        exit:{noproc, _} -> {error, unavailable};
        exit:_ -> {error, unavailable}
    end.

-spec call_manager_server_ref(node()) -> atom() | {atom(), node()}.
call_manager_server_ref(TargetNode) when TargetNode =:= node() ->
    ?MODULE;
call_manager_server_ref(TargetNode) ->
    {?MODULE, TargetNode}.

-spec safe_local_call(term(), timeout()) -> term().
safe_local_call(Request, Timeout) ->
    try
        gen_server:call(?MODULE, Request, Timeout)
    catch
        exit:{timeout, _} -> {error, timeout};
        exit:{noproc, _} -> {error, unavailable};
        exit:_ -> {error, unavailable}
    end.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(Info, State) ->
    case decode_info_message(Info) of
        {ok, InfoMessage} -> handle_info_message(InfoMessage, State);
        error -> {noreply, State}
    end.

-spec handle_info_message(info_message(), state()) -> {noreply, state()}.
handle_info_message({'DOWN', _Ref, process, Pid, _Reason}, #{calls := Calls} = State) ->
    cleanup_call_pid_cache(Pid, Calls),
    NewCalls = process_registry:cleanup_on_down(call, Pid, Calls),
    {noreply, State#{calls := NewCalls}}.

-spec cleanup_call_pid_cache(pid(), map()) -> ok.
cleanup_call_pid_cache(DeadPid, Calls) ->
    maps:foreach(
        fun
            (ChannelId, {Pid, _Ref}) when Pid =:= DeadPid, is_integer(ChannelId) ->
                ets:delete(call_pid_cache, ChannelId);
            (_Key, _Value) ->
                ok
        end,
        Calls
    ),
    ok.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #{calls := _Calls}) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-ifdef(TEST).

owner_scope_uses_local_node_when_single_member_test() ->
    persistent_term:erase({gateway_cluster_membership, members}),
    ?assertEqual(local, owner_scope(123)).

owner_scope_returns_unavailable_when_call_role_absent_test() ->
    persistent_term:put({fluxer_gateway, runtime_config}, #{gateway_role => websocket}),
    persistent_term:put({gateway_cluster_membership, members_by_role}, #{
        guilds => [node()]
    }),
    ?assertEqual(unavailable, owner_scope(123)),
    persistent_term:erase({gateway_cluster_membership, members_by_role}),
    persistent_term:erase({fluxer_gateway, runtime_config}).

owner_scope_routes_remote_owner_test() ->
    RemoteNode = 'gateway_b@example',
    Nodes = lists:usort([node(), RemoteNode]),
    ChannelId = channel_id_with_remote_owner(Nodes, 1),
    RoleMap = #{calls => Nodes, all => Nodes},
    persistent_term:put({gateway_cluster_membership, members}, Nodes),
    persistent_term:put({gateway_cluster_membership, members_by_role}, RoleMap),
    try
        ?assertEqual({remote, RemoteNode}, owner_scope(ChannelId))
    after
        persistent_term:erase({gateway_cluster_membership, members}),
        persistent_term:erase({gateway_cluster_membership, members_by_role})
    end.

remote_owner_call_replies_async_test_() ->
    {timeout, 5, fun remote_owner_call_replies_async/0}.

remote_owner_call_replies_async() ->
    RemoteNode = 'gateway_b@example',
    Nodes = lists:usort([node(), RemoteNode]),
    ChannelId = channel_id_with_remote_owner(Nodes, 1),
    RoleMap = #{calls => Nodes, all => Nodes},
    persistent_term:put({gateway_cluster_membership, members}, Nodes),
    persistent_term:put({gateway_cluster_membership, members_by_role}, RoleMap),
    try
        assert_remote_owner_call_replies_async(ChannelId)
    after
        persistent_term:erase({gateway_cluster_membership, members}),
        persistent_term:erase({gateway_cluster_membership, members_by_role})
    end.

assert_remote_owner_call_replies_async(ChannelId) ->
    State = #{calls => #{}},
    Ref = make_ref(),
    From = {self(), Ref},
    ?assertEqual(
        {noreply, State},
        handle_owner_call(
            ChannelId, {lookup, ChannelId}, From, 1, fun local_owner_path_called/0, State
        )
    ),
    assert_remote_owner_reply(Ref).

local_owner_path_called() ->
    error(local_owner_path_called).

assert_remote_owner_reply(Ref) ->
    receive
        {Ref, {error, unavailable}} -> ok;
        {Ref, {error, timeout}} -> ok
    after 1000 ->
        error(async_reply_not_received)
    end.

channel_id_with_remote_owner(Nodes, ChannelId) ->
    case gateway_node_router:select_owner_node(ChannelId, Nodes) =/= node() of
        true -> ChannelId;
        false when ChannelId < 100000 -> channel_id_with_remote_owner(Nodes, ChannelId + 1);
        false -> error(no_remote_owner_channel_id)
    end.

-endif.
