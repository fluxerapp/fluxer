%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_cutover).
-feature(maybe_expr, enable).
-typing([eqwalizer]).

-export([run/1, reverse/1, move/4, routes/2]).

-define(PIN_KEY, guild_owner_pins).
-define(DEFAULT_ROUTE_TIMEOUT, 5000).
-define(DEFAULT_RELOAD_TIMEOUT, 120000).
-define(DEFAULT_MAX_MAILBOX, 100).
-define(PROBE_KEY, 0).

-type guild_id() :: integer().
-type saved_pins() :: #{node() => {ok, term()} | undefined}.
-type report() :: map().

-export_type([report/0]).

-spec run(map()) -> {ok, report()} | {error, report()}.
run(#{guild_id := GuildId, source_node := SourceNode} = Opts) when
    is_integer(GuildId), is_atom(SourceNode)
->
    move(GuildId, SourceNode, node(), maps:merge(#{transfer_sessions => true}, Opts)).

-spec reverse(map()) -> {ok, report()} | {error, report()}.
reverse(#{guild_id := GuildId, target_node := TargetNode} = Opts) when
    is_integer(GuildId), is_atom(TargetNode)
->
    move(GuildId, node(), TargetNode, maps:merge(#{transfer_sessions => false}, Opts)).

-spec routes(guild_id(), [node()]) -> #{node() => term()}.
routes(GuildId, Nodes) ->
    Args = [GuildId, guilds],
    Results = erpc:multicall(
        Nodes, gateway_node_router, owner_node_result, Args, ?DEFAULT_ROUTE_TIMEOUT
    ),
    maps:from_list(lists:zip(Nodes, Results)).

-spec move(guild_id(), node(), node(), map()) -> {ok, report()} | {error, report()}.
move(GuildId, From, To, Opts) ->
    Nodes = route_nodes(From, To, Opts),
    case preflight(GuildId, From, To, Nodes, Opts) of
        {ok, SrcPid, SrcShard, Saved} ->
            TransferOpts = transfer_opts(GuildId, From, To, Nodes, Saved, Opts),
            Result = guild_handoff_freeze:transfer(GuildId, SrcPid, SrcShard, To, TransferOpts),
            finish(GuildId, From, To, Nodes, Result, Opts);
        {error, Reason} ->
            {error, #{phase => preflight, reason => Reason, from => From, to => To}}
    end.

-spec route_nodes(node(), node(), map()) -> [node()].
route_nodes(From, To, Opts) ->
    Nodes = maps:get(route_nodes, Opts, [node() | nodes()]),
    lists:usort([From, To | [N || N <- Nodes, is_atom(N)]]).

-spec preflight(guild_id(), node(), node(), [node()], map()) ->
    {ok, pid(), pid(), saved_pins()} | {error, term()}.
preflight(_GuildId, Node, Node, _Nodes, _Opts) ->
    {error, same_node};
preflight(GuildId, From, To, Nodes, Opts) ->
    maybe
        ok ?= connected([From, To]),
        ok ?= expect_routes(GuildId, Nodes, From),
        {ok, SrcShard} ?= guild_handoff_freeze:shard_pid(GuildId, From),
        {ok, SrcPid} ?= lookup(SrcShard, GuildId),
        ok ?= absent_on(GuildId, To),
        ok ?= mailbox_below(SrcPid, maps:get(max_mailbox, Opts, ?DEFAULT_MAX_MAILBOX)),
        false ?= guild_handoff_freeze:is_frozen(SrcPid),
        {ok, Saved} ?= read_pins(Nodes),
        ok ?= probe_pins(Nodes, To, Saved),
        {ok, SrcPid, SrcShard, Saved}
    else
        true -> {error, source_already_frozen};
        {error, _} = Error -> Error
    end.

-spec transfer_opts(guild_id(), node(), node(), [node()], saved_pins(), map()) ->
    guild_handoff_freeze:opts().
transfer_opts(GuildId, From, To, Nodes, Saved, Opts) ->
    Base = maps:with(
        [
            suspend_timeout,
            export_timeout,
            start_timeout,
            stop_timeout,
            barrier_timeout,
            rpc_timeout,
            guard_timeout,
            freeze_budget,
            forward_rounds,
            transfer_sessions,
            max_heap_words,
            measure
        ],
        Opts
    ),
    Others = [N || N <- Nodes, N =/= To],
    Base#{
        before_start => fun() -> pin_and_verify([To], GuildId, To, Saved) end,
        after_start => fun(_NewPid) -> pin_and_verify(Others, GuildId, To, Saved) end,
        on_abort => fun() -> restore_and_verify(Nodes, GuildId, From, Saved) end
    }.

-spec finish(guild_id(), node(), node(), [node()], guild_handoff_freeze:result(), map()) ->
    {ok, report()} | {error, report()}.
finish(GuildId, From, To, Nodes, {ok, Report}, Opts) ->
    Reload = maybe_reload(GuildId, To, Opts),
    {ok, Report#{
        from => From,
        to => To,
        reload => Reload,
        routes_after => routes(GuildId, Nodes)
    }};
finish(GuildId, From, To, Nodes, {error, Report}, _Opts) ->
    {error, Report#{from => From, to => To, routes_after => routes(GuildId, Nodes)}}.

-spec maybe_reload(guild_id(), node(), map()) -> skipped | term().
maybe_reload(GuildId, To, #{reload := true} = Opts) ->
    Timeout = maps:get(reload_timeout, Opts, ?DEFAULT_RELOAD_TIMEOUT),
    Args = [guild_manager, {reload_guild, GuildId}, Timeout],
    try erpc:call(To, gen_server, call, Args, Timeout + 5000) of
        Reply -> Reply
    catch
        Class:Reason -> {error, {Class, Reason}}
    end;
maybe_reload(_GuildId, _To, _Opts) ->
    skipped.

-spec connected([node()]) -> ok | {error, term()}.
connected(Nodes) ->
    case [N || N <- Nodes, N =/= node(), not lists:member(N, nodes())] of
        [] -> ok;
        Missing -> {error, {not_connected, Missing}}
    end.

-spec expect_routes(guild_id(), [node()], node()) -> ok | {error, term()}.
expect_routes(GuildId, Nodes, Owner) ->
    case [{N, R} || {N, R} <- maps:to_list(routes(GuildId, Nodes)), R =/= {ok, {ok, Owner}}] of
        [] -> ok;
        Wrong -> {error, {routes_disagree, Owner, Wrong}}
    end.

-spec lookup(pid(), guild_id()) -> {ok, pid()} | {error, term()}.
lookup(Shard, GuildId) ->
    try gen_server:call(Shard, {lookup, GuildId}, ?DEFAULT_ROUTE_TIMEOUT) of
        {ok, Pid} when is_pid(Pid) -> {ok, Pid};
        Other -> {error, {source_lookup, Other}}
    catch
        exit:Reason -> {error, {source_lookup, Reason}}
    end.

-spec absent_on(guild_id(), node()) -> ok | {error, term()}.
absent_on(GuildId, Node) ->
    case guild_handoff_freeze:shard_pid(GuildId, Node) of
        {ok, Shard} ->
            try gen_server:call(Shard, {lookup, GuildId}, ?DEFAULT_ROUTE_TIMEOUT) of
                {error, not_found} -> ok;
                Other -> {error, {target_not_empty, Other}}
            catch
                exit:Reason -> {error, {target_lookup, Reason}}
            end;
        {error, _} = Error ->
            Error
    end.

-spec mailbox_below(pid(), non_neg_integer()) -> ok | {error, term()}.
mailbox_below(Pid, Max) ->
    Args = [Pid, message_queue_len],
    try erpc:call(node(Pid), erlang, process_info, Args, ?DEFAULT_ROUTE_TIMEOUT) of
        {message_queue_len, Len} when Len =< Max -> ok;
        Other -> {error, {source_mailbox, Other}}
    catch
        Class:Reason -> {error, {source_mailbox, Class, Reason}}
    end.

-spec read_pins([node()]) -> {ok, saved_pins()} | {error, term()}.
read_pins(Nodes) ->
    Results = erpc:multicall(
        Nodes, application, get_env, [fluxer_gateway, ?PIN_KEY], ?DEFAULT_ROUTE_TIMEOUT
    ),
    case [{N, R} || {N, R} <- lists:zip(Nodes, Results), not readable_pin(R)] of
        [] -> {ok, maps:from_list([{N, V} || {N, {ok, V}} <- lists:zip(Nodes, Results)])};
        Failed -> {error, {pins_unreadable, Failed}}
    end.

-spec readable_pin(term()) -> boolean().
readable_pin({ok, undefined}) -> true;
readable_pin({ok, {ok, Pins}}) when is_map(Pins) -> true;
readable_pin(_) -> false.

-spec probe_pins([node()], node(), saved_pins()) -> ok | {error, term()}.
probe_pins(Nodes, Owner, Saved) ->
    Probe = pin_and_verify(Nodes, ?PROBE_KEY, Owner, Saved),
    Restore = write_pins([{N, maps:get(N, Saved, undefined)} || N <- Nodes]),
    case {Probe, Restore} of
        {ok, ok} -> ok;
        _ -> {error, {pin_probe_failed, Probe, Restore}}
    end.

-spec pin_and_verify([node()], guild_id(), node(), saved_pins()) -> ok | {error, term()}.
pin_and_verify([], _GuildId, _Owner, _Saved) ->
    ok;
pin_and_verify(Nodes, GuildId, Owner, Saved) ->
    Values = [{N, {ok, pinned(maps:get(N, Saved, undefined), GuildId, Owner)}} || N <- Nodes],
    maybe
        ok ?= write_pins(Values),
        expect_routes(GuildId, Nodes, Owner)
    end.

-spec restore_and_verify([node()], guild_id(), node(), saved_pins()) -> ok | {error, term()}.
restore_and_verify(Nodes, GuildId, Owner, Saved) ->
    Values = [{N, maps:get(N, Saved, undefined)} || N <- Nodes],
    maybe
        ok ?= write_pins(Values),
        expect_routes(GuildId, Nodes, Owner)
    end.

-spec pinned({ok, term()} | undefined, guild_id(), node()) -> map().
pinned({ok, Pins}, GuildId, Owner) when is_map(Pins) ->
    Pins#{GuildId => Owner};
pinned(_Previous, GuildId, Owner) ->
    #{GuildId => Owner}.

-spec write_pins([{node(), {ok, term()} | undefined}]) -> ok | {error, term()}.
write_pins(Values) ->
    Groups = maps:groups_from_list(fun({_N, V}) -> V end, fun({N, _V}) -> N end, Values),
    Failed = maps:fold(fun write_group/3, [], Groups),
    case Failed of
        [] -> ok;
        _ -> {error, {pin_write_failed, Failed}}
    end.

-spec write_group({ok, term()} | undefined, [node()], list()) -> list().
write_group({ok, Value}, Nodes, Acc) ->
    Args = [fluxer_gateway, ?PIN_KEY, Value],
    collect_failures(
        Nodes, erpc:multicall(Nodes, application, set_env, Args, ?DEFAULT_ROUTE_TIMEOUT), Acc
    );
write_group(undefined, Nodes, Acc) ->
    Args = [fluxer_gateway, ?PIN_KEY],
    collect_failures(
        Nodes, erpc:multicall(Nodes, application, unset_env, Args, ?DEFAULT_ROUTE_TIMEOUT), Acc
    ).

-spec collect_failures([node()], list(), list()) -> list().
collect_failures(Nodes, Results, Acc) ->
    [{N, R} || {N, R} <- lists:zip(Nodes, Results), R =/= {ok, ok}] ++ Acc.
