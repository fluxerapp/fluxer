%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_guild_pin_remote).
-typing([eqwalizer]).

-export([
    busy_copies/3,
    sweep/2,
    stop_stray/2,
    patch_peer/4,
    ensure_router/3,
    unpin_peer/2,
    routes_here/2,
    read_beam/2,
    parse_md5_list/1,
    as_pins/1,
    pin_guilds/3,
    unpin_guilds/3,
    pin_local/1,
    unpin_local/1,
    routed_here/2,
    release_peers/2,
    run_parallel/3
]).

-define(ROUTER, gateway_node_router).
-define(LOADED_BEAM_NAME, "/tmp/fluxer-guild-pin/gateway_node_router.beam").
-define(RPC_TIMEOUT_MS, 2000).
-define(STOP_TIMEOUT_MS, 5000).
-define(PATCH_TIMEOUT_MS, 10000).
-define(RELEASE_ROUNDS, 3).
-define(RELEASE_RETRY_MS, 200).

-type guild_id() :: pos_integer().
-type beam() :: #{binary := binary(), md5 := binary()}.
-type outcome() :: ok | {error, term()}.

-export_type([guild_id/0, beam/0, outcome/0]).

-spec busy_copies([guild_id()], [node()], non_neg_integer()) -> [{node(), term()}].
busy_copies(GuildIds, Nodes, Max) ->
    [
        {Node, Len}
     || GuildId <- GuildIds,
        Key <- [process_registry:build_process_key(guild, GuildId)],
        Node <- stray_nodes(Key, Nodes),
        Len <- [copy_mailbox(Node, Key)],
        not (is_integer(Len) andalso Len =< Max)
    ].

-spec copy_mailbox(node(), process_registry:process_key()) -> term().
copy_mailbox(Node, Key) ->
    case rpc:call(Node, ets, lookup, [process_registry_table, Key], ?RPC_TIMEOUT_MS) of
        [{_, Pid}] when is_pid(Pid) -> mailbox_len(Node, Pid);
        _ -> 0
    end.

-spec mailbox_len(node(), pid()) -> term().
mailbox_len(Node, Pid) ->
    case rpc:call(Node, erlang, process_info, [Pid, message_queue_len], ?RPC_TIMEOUT_MS) of
        {message_queue_len, Len} -> Len;
        undefined -> 0;
        Other -> {unreadable, Other}
    end.

-spec sweep([guild_id()], [node()]) -> #{stopped := [map()], unpinned := [node()]}.
sweep(GuildIds, Nodes) ->
    #{
        unpinned => lists:usort(
            lists:flatmap(fun(G) -> unpinned_nodes(G, Nodes) end, GuildIds)
        ),
        stopped => lists:flatmap(fun(GuildId) -> sweep_guild(GuildId, Nodes) end, GuildIds)
    }.

-spec unpinned_nodes(guild_id(), [node()]) -> [node()].
unpinned_nodes(_GuildId, []) ->
    [];
unpinned_nodes(GuildId, Nodes) ->
    Expected = {ok, node()},
    Answers = erpc:multicall(
        Nodes, ?ROUTER, owner_node_result, [GuildId, guilds], ?RPC_TIMEOUT_MS
    ),
    [Node || {Node, Answer} <- lists:zip(Nodes, Answers), Answer =/= {ok, Expected}].

-spec sweep_guild(guild_id(), [node()]) -> [map()].
sweep_guild(GuildId, Nodes) ->
    Key = process_registry:build_process_key(guild, GuildId),
    case process_registry:registry_whereis(Key) of
        undefined ->
            [];
        _LocalPid ->
            [
                #{guild_id => GuildId, node => Node, result => stop_stray(GuildId, Node)}
             || Node <- stray_nodes(Key, Nodes)
            ]
    end.

-spec stray_nodes(process_registry:process_key(), [node()]) -> [node()].
stray_nodes(_Key, []) ->
    [];
stray_nodes(Key, Nodes) ->
    Results = erpc:multicall(
        Nodes, ets, lookup, [process_registry_table, Key], ?RPC_TIMEOUT_MS
    ),
    [Node || {Node, {ok, [{_, Pid}]}} <- lists:zip(Nodes, Results), is_pid(Pid)].

-spec stop_stray(guild_id(), node()) -> term().
stop_stray(GuildId, Node) ->
    case guild_handoff_freeze:shard_pid(GuildId, Node) of
        {ok, ShardPid} ->
            Request = {stop_guild, GuildId, {shutdown, handoff}},
            shard_utils:safe_gen_call_remote(ShardPid, Request, ?STOP_TIMEOUT_MS);
        {error, _Reason} = Error ->
            Error
    end.

-spec patch_peer(node(), beam(), [binary()], [guild_id()]) -> {atom(), outcome()}.
patch_peer(Node, Beam, BaseMd5s, GuildIds) ->
    Role = peer_role(Node),
    Outcome =
        maybe
            ok ?= ensure_router(Node, Beam, BaseMd5s),
            ok ?= pin_peer(Node, GuildIds),
            verify_pins(Node, GuildIds)
        end,
    {Role, Outcome}.

-spec ensure_router(node(), beam(), [binary()]) -> ok | {error, term()}.
ensure_router(Node, #{binary := Bin, md5 := PinnedMd5}, BaseMd5s) ->
    case remote_router_md5(Node) of
        {ok, PinnedMd5} ->
            ok;
        {ok, Md5} ->
            replace_router(lists:member(Md5, BaseMd5s), Node, Bin, PinnedMd5, Md5);
        {error, _Reason} = Error ->
            Error
    end.

-spec replace_router(boolean(), node(), binary(), binary(), binary()) -> ok | {error, term()}.
replace_router(true, Node, Bin, PinnedMd5, _Md5) ->
    load_router(Node, Bin, PinnedMd5);
replace_router(false, Node, _Bin, _PinnedMd5, Md5) ->
    pin_aware_router(Node, Md5).

-spec pin_aware_router(node(), binary()) -> ok | {error, term()}.
pin_aware_router(Node, Md5) ->
    Args = [?ROUTER, pinned_guild_owner, 1],
    case rpc:call(Node, erlang, function_exported, Args, ?RPC_TIMEOUT_MS) of
        true -> ok;
        _ -> {error, {unexpected_router_md5, binary:encode_hex(Md5)}}
    end.

-spec remote_router_md5(node()) -> {ok, binary()} | {error, term()}.
remote_router_md5(Node) ->
    case rpc:call(Node, code, ensure_loaded, [?ROUTER], ?RPC_TIMEOUT_MS) of
        {module, ?ROUTER} -> loaded_router_md5(Node);
        Other -> {error, {router_unavailable, Other}}
    end.

-spec loaded_router_md5(node()) -> {ok, binary()} | {error, term()}.
loaded_router_md5(Node) ->
    case rpc:call(Node, erlang, get_module_info, [?ROUTER, md5], ?RPC_TIMEOUT_MS) of
        Md5 when is_binary(Md5) -> {ok, Md5};
        Other -> {error, {router_md5_unavailable, Other}}
    end.

-spec load_router(node(), binary(), binary()) -> ok | {error, term()}.
load_router(Node, Bin, PinnedMd5) ->
    case rpc:call(Node, erlang, check_old_code, [?ROUTER], ?RPC_TIMEOUT_MS) of
        false -> load_router_binary(Node, Bin, PinnedMd5);
        Other -> {error, {router_old_code, Other}}
    end.

-spec load_router_binary(node(), binary(), binary()) -> ok | {error, term()}.
load_router_binary(Node, Bin, PinnedMd5) ->
    Args = [?ROUTER, ?LOADED_BEAM_NAME, Bin],
    case rpc:call(Node, code, load_binary, Args, ?RPC_TIMEOUT_MS) of
        {module, ?ROUTER} -> confirm_router(Node, PinnedMd5);
        Other -> {error, {router_load_failed, Other}}
    end.

-spec confirm_router(node(), binary()) -> ok | {error, term()}.
confirm_router(Node, PinnedMd5) ->
    case remote_router_md5(Node) of
        {ok, PinnedMd5} ->
            _ = rpc:call(Node, code, soft_purge, [?ROUTER], ?RPC_TIMEOUT_MS),
            ok;
        Other ->
            {error, {router_md5_after_load, Other}}
    end.

-spec pin_peer(node(), [guild_id()]) -> ok | {error, term()}.
pin_peer(Node, GuildIds) ->
    update_remote_pins(Node, fun(Pins) -> pin_guilds(Pins, GuildIds, node()) end).

-spec unpin_peer(node(), [guild_id()]) -> ok | {error, term()}.
unpin_peer(Node, GuildIds) ->
    update_remote_pins(Node, fun(Pins) -> unpin_guilds(Pins, GuildIds, node()) end).

-spec update_remote_pins(node(), fun((#{guild_id() => node()}) -> #{guild_id() => node()})) ->
    ok | {error, term()}.
update_remote_pins(Node, Update) ->
    case
        rpc:call(
            Node, application, get_env, [fluxer_gateway, guild_owner_pins, #{}], ?RPC_TIMEOUT_MS
        )
    of
        Pins when is_map(Pins) -> write_remote_pins(Node, as_pins(Pins), Update(as_pins(Pins)));
        Other -> {error, {pins_unreadable, Other}}
    end.

-spec write_remote_pins(node(), #{guild_id() => node()}, #{guild_id() => node()}) ->
    ok | {error, term()}.
write_remote_pins(_Node, Same, Same) ->
    ok;
write_remote_pins(Node, _Old, New) when map_size(New) =:= 0 ->
    remote_env(Node, unset_env, [fluxer_gateway, guild_owner_pins]);
write_remote_pins(Node, _Old, New) ->
    remote_env(Node, set_env, [fluxer_gateway, guild_owner_pins, New]).

-spec remote_env(node(), set_env | unset_env, [term()]) -> ok | {error, term()}.
remote_env(Node, Function, Args) ->
    case rpc:call(Node, application, Function, Args, ?RPC_TIMEOUT_MS) of
        ok -> ok;
        Other -> {error, {Function, Other}}
    end.

-spec verify_pins(node(), [guild_id()]) -> ok | {error, term()}.
verify_pins(Node, GuildIds) ->
    Expected = {ok, node()},
    Answers = [
        {GuildId,
            rpc:call(Node, ?ROUTER, owner_node_result, [GuildId, guilds], ?RPC_TIMEOUT_MS)}
     || GuildId <- GuildIds
    ],
    case [Answer || {_GuildId, Got} = Answer <- Answers, Got =/= Expected] of
        [] -> ok;
        Wrong -> {error, {pin_not_effective, Wrong}}
    end.

-spec peer_role(node()) -> atom().
peer_role(Node) ->
    case rpc:call(Node, fluxer_gateway_sup, current_role, [], ?RPC_TIMEOUT_MS) of
        Role when is_atom(Role) -> Role;
        _Other -> unknown
    end.

-spec routes_here(node(), [guild_id()]) -> released | {pinned | unverified, term()}.
routes_here(Node, GuildIds) ->
    Answers = [
        rpc:call(Node, ?ROUTER, owner_node_result, [GuildId, guilds], ?RPC_TIMEOUT_MS)
     || GuildId <- GuildIds
    ],
    case [Answer || Answer <- Answers, not released_answer(Answer)] of
        [] -> released;
        Wrong -> release_status(Wrong)
    end.

-spec released_answer(term()) -> boolean().
released_answer({ok, Owner}) -> Owner =/= node();
released_answer({error, {no_active_nodes, _}}) -> true;
released_answer({badrpc, nodedown}) -> true;
released_answer(_Answer) -> false.

-spec release_status([term()]) -> {pinned | unverified, term()}.
release_status(Wrong) ->
    case lists:member({ok, node()}, Wrong) of
        true -> {pinned, Wrong};
        false -> {unverified, Wrong}
    end.

-spec read_beam(term(), term()) -> beam() | {error, term()}.
read_beam(Path, ExpectedHex) when is_list(Path), is_binary(ExpectedHex) ->
    case file:read_file(eqwalizer:dynamic_cast(Path)) of
        {ok, Bin} -> check_beam(Bin, decode_md5(ExpectedHex));
        {error, Reason} -> {error, {read_failed, Path, Reason}}
    end;
read_beam(_Path, _ExpectedHex) ->
    {error, not_configured}.

-spec check_beam(binary(), {ok, binary()} | error) -> beam() | {error, term()}.
check_beam(_Bin, error) ->
    {error, invalid_expected_md5};
check_beam(Bin, {ok, Expected}) ->
    case beam_lib:md5(Bin) of
        {ok, {?ROUTER, Expected}} -> #{binary => Bin, md5 => Expected};
        {ok, {Module, Md5}} -> {error, {beam_mismatch, Module, binary:encode_hex(Md5)}};
        {error, beam_lib, Reason} -> {error, {invalid_beam, Reason}}
    end.

-spec parse_md5_list(term()) -> [binary()].
parse_md5_list(Value) when is_binary(Value) ->
    [
        Md5
     || Token <- string:lexemes(binary_to_list(Value), ", "), {ok, Md5} <- [decode_md5(Token)]
    ];
parse_md5_list(_Value) ->
    [].

-spec decode_md5(string() | binary()) -> {ok, binary()} | error.
decode_md5(Hex) when is_list(Hex) ->
    decode_md5(list_to_binary(Hex));
decode_md5(Hex) when byte_size(Hex) =:= 32 ->
    try binary:decode_hex(Hex) of
        Md5 -> {ok, Md5}
    catch
        error:badarg -> error
    end;
decode_md5(_Hex) ->
    error.

-spec as_pins(term()) -> #{guild_id() => node()}.
as_pins(Pins) when is_map(Pins) ->
    eqwalizer:dynamic_cast(
        maps:filter(fun(Key, Value) -> is_integer(Key) andalso is_atom(Value) end, Pins)
    );
as_pins(_Pins) ->
    #{}.

-spec pin_guilds(#{guild_id() => node()}, [guild_id()], node()) -> #{guild_id() => node()}.
pin_guilds(Pins, GuildIds, Owner) ->
    maps:merge(Pins, maps:from_list([{GuildId, Owner} || GuildId <- GuildIds])).

-spec unpin_guilds(#{guild_id() => node()}, [guild_id()], node()) -> #{guild_id() => node()}.
unpin_guilds(Pins, GuildIds, Owner) ->
    maps:filter(
        fun(GuildId, Node) -> not (Node =:= Owner andalso lists:member(GuildId, GuildIds)) end,
        Pins
    ).

-spec pin_local([guild_id()]) -> ok.
pin_local(GuildIds) ->
    Pins = gateway_guild_pin_remote:as_pins(
        application:get_env(fluxer_gateway, guild_owner_pins, #{})
    ),
    application:set_env(
        fluxer_gateway,
        guild_owner_pins,
        gateway_guild_pin_remote:pin_guilds(Pins, GuildIds, node())
    ).

-spec unpin_local([guild_id()]) -> ok.
unpin_local(GuildIds) ->
    Pins = gateway_guild_pin_remote:as_pins(
        application:get_env(fluxer_gateway, guild_owner_pins, #{})
    ),
    application:set_env(
        fluxer_gateway,
        guild_owner_pins,
        gateway_guild_pin_remote:unpin_guilds(Pins, GuildIds, node())
    ).

-spec routed_here(guild_id(), [node()]) -> boolean().
routed_here(_GuildId, []) ->
    true;
routed_here(GuildId, Peers) ->
    Answers = erpc:multicall(
        Peers, ?ROUTER, owner_node_result, [GuildId, guilds], ?RPC_TIMEOUT_MS
    ),
    lists:all(fun(Answer) -> Answer =:= {ok, {ok, node()}} end, Answers).

-spec release_peers([node()], [guild_id()]) -> [{node(), term()}].
release_peers(Peers, GuildIds) ->
    release_peers(Peers, GuildIds, ?RELEASE_ROUNDS).

-spec release_peers([node()], [guild_id()], pos_integer()) -> [{node(), term()}].
release_peers([], _GuildIds, _Left) ->
    [];
release_peers(Peers, GuildIds, Left) ->
    _ = run_parallel(
        fun(Node) -> gateway_guild_pin_remote:unpin_peer(Node, GuildIds) end,
        Peers,
        ?PATCH_TIMEOUT_MS
    ),
    Checked = run_parallel(
        fun(Node) -> gateway_guild_pin_remote:routes_here(Node, GuildIds) end,
        Peers,
        ?PATCH_TIMEOUT_MS
    ),
    case [{Node, Answer} || {Node, Answer} <- Checked, Answer =/= released] of
        [] ->
            [];
        Still when Left =:= 1 ->
            Still;
        Still ->
            timer:sleep(?RELEASE_RETRY_MS),
            release_peers([Node || {Node, _Answer} <- Still], GuildIds, Left - 1)
    end.

-spec run_parallel(fun((node()) -> term()), [node()], pos_integer()) -> [{node(), term()}].
run_parallel(Fun, Nodes, TimeoutMs) ->
    Parent = self(),
    Ref = make_ref(),
    Pids = [{spawn(fun() -> Parent ! {Ref, Node, Fun(Node)} end), Node} || Node <- Nodes],
    Deadline = erlang:monotonic_time(millisecond) + TimeoutMs,
    Results = collect_parallel(Ref, Nodes, #{}, Deadline),
    [exit(Pid, kill) || {Pid, Node} <- Pids, not maps:is_key(Node, Results)],
    [{Node, maps:get(Node, Results, {error, timeout})} || Node <- Nodes].

-spec collect_parallel(reference(), [node()], #{node() => term()}, integer()) ->
    #{node() => term()}.
collect_parallel(_Ref, Nodes, Results, _Deadline) when map_size(Results) =:= length(Nodes) ->
    Results;
collect_parallel(Ref, Nodes, Results, Deadline) ->
    Wait = max(0, Deadline - erlang:monotonic_time(millisecond)),
    receive
        {Ref, Node, Result} -> collect_parallel(Ref, Nodes, Results#{Node => Result}, Deadline)
    after Wait ->
        Results
    end.
