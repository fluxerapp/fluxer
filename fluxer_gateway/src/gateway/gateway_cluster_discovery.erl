%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_cluster_discovery).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([
    start_link/0, start_link/1,
    peers/0,
    subscribe/1,
    force_refresh/0
]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([ip_addrs_to_peers/2, poll/1, add_subscriber/2, remove_subscriber/2]).

-define(PEERS_KEY, {gateway_cluster_discovery, peers}).
-define(DEFAULT_POLL_INTERVAL_MS, 5000).
-define(DEFAULT_NODE_BASENAME, "fluxer_gateway").
-define(MAX_DISCOVERED_PEERS, 512).

-type peer() :: node().
-type resolver() :: fun((string()) -> {ok, [inet:ip_address()]} | {error, term()}).

-type options() :: #{
    dns_name => string() | undefined,
    node_basename => string(),
    poll_interval_ms => pos_integer(),
    resolver => resolver(),
    static_peers => [peer()]
}.

-type state() :: #{
    dns_name := string() | undefined,
    node_basename := string(),
    poll_interval_ms := pos_integer(),
    resolver := resolver(),
    static_peers := [peer()],
    peers := [peer()],
    subscribers := [{pid(), reference()}],
    timer := reference() | undefined
}.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    start_link(#{}).

-spec start_link(options()) -> gen_server:start_ret().
start_link(Options) when is_map(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

-spec peers() -> [peer()].
peers() ->
    persistent_term:get(?PEERS_KEY, []).

-spec subscribe(pid()) -> ok.
subscribe(Pid) when is_pid(Pid) ->
    gen_server:cast(?MODULE, {subscribe, Pid}).

-spec force_refresh() -> {ok, [peer()]} | {error, term()}.
force_refresh() ->
    case gen_server:call(?MODULE, get_resolve_inputs, 5000) of
        {ok, Inputs} ->
            do_force_refresh(Inputs);
        {error, Reason} ->
            {error, Reason}
    end.

-spec do_force_refresh(map()) -> {ok, [peer()]} | {error, term()}.
do_force_refresh(#{static_peers := StaticPeers}) when StaticPeers =/= [] ->
    NewPeers = filter_self(StaticPeers),
    gen_server:cast(?MODULE, {apply_resolved_peers, NewPeers}),
    {ok, NewPeers};
do_force_refresh(#{dns_name := undefined}) ->
    {ok, []};
do_force_refresh(#{dns_name := DnsName, resolver := Resolver, node_basename := Base}) ->
    case Resolver(DnsName) of
        {ok, Addrs} ->
            NewPeers = ip_addrs_to_peers(Addrs, Base),
            gen_server:cast(?MODULE, {apply_resolved_peers, NewPeers}),
            {ok, NewPeers};
        {error, Reason} ->
            gateway_cluster_metrics:record_discovery_resolve_failure(),
            {error, Reason}
    end.

-spec init(options()) -> {ok, state()}.
init(Options) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 0),
    ExistingPeers = peers(),
    DnsName = optional_string_value(
        option_or_config(Options, dns_name, cluster_discovery_dns_name, undefined)
    ),
    NodeBasename = required_string_value(
        option_or_config(
            Options, node_basename, cluster_discovery_node_basename, ?DEFAULT_NODE_BASENAME
        ),
        ?DEFAULT_NODE_BASENAME
    ),
    PollIntervalMs = positive_integer_value(
        option_or_config(
            Options,
            poll_interval_ms,
            cluster_discovery_poll_interval_ms,
            ?DEFAULT_POLL_INTERVAL_MS
        ),
        ?DEFAULT_POLL_INTERVAL_MS
    ),
    StaticPeers = normalize_static_peers(
        option_or_config(Options, static_peers, cluster_static_peers, [])
    ),
    State = #{
        dns_name => DnsName,
        node_basename => NodeBasename,
        poll_interval_ms => PollIntervalMs,
        resolver => resolver_value(maps:get(resolver, Options, fun default_resolver/1)),
        static_peers => StaticPeers,
        peers => ExistingPeers,
        subscribers => [],
        timer => undefined
    },
    persistent_term:put(?PEERS_KEY, ExistingPeers),
    {ok, schedule_poll(0, State)}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call(get_resolve_inputs, _From, State) ->
    Inputs = #{
        dns_name => maps:get(dns_name, State),
        resolver => maps:get(resolver, State),
        node_basename => maps:get(node_basename, State),
        static_peers => maps:get(static_peers, State)
    },
    {reply, {ok, Inputs}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({subscribe, Pid}, State) when is_pid(Pid) ->
    {noreply, add_subscriber(Pid, State)};
handle_cast({apply_resolved_peers, NewPeers}, #{peers := OldPeers} = State) when
    is_list(NewPeers)
->
    {_Peers, State1} = apply_resolved_peers(eqwalizer:dynamic_cast(NewPeers), OldPeers, State),
    {noreply, State1};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(poll, State0) ->
    State1 = State0#{timer := undefined},
    {_Peers, State2} = poll(State1),
    Interval = maps:get(poll_interval_ms, State2),
    {noreply, schedule_poll(Interval, State2)};
handle_info({'DOWN', _MonRef, process, Pid, _Reason}, State) when is_pid(Pid) ->
    {noreply, remove_subscriber(Pid, State)};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    cancel_timer(State),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:process_flag(fullsweep_after, 0),
    erlang:garbage_collect(),
    {ok, State}.

-spec poll(state()) -> {[peer()], state()}.
poll(#{static_peers := StaticPeers, peers := OldPeers} = State) when StaticPeers =/= [] ->
    NewPeers = filter_self(StaticPeers),
    apply_resolved_peers(NewPeers, OldPeers, State);
poll(#{dns_name := undefined} = State) ->
    {[], State};
poll(
    #{
        dns_name := DnsName,
        resolver := Resolver,
        node_basename := Base,
        peers := OldPeers
    } = State
) ->
    case Resolver(DnsName) of
        {ok, Addrs} ->
            NewPeers = ip_addrs_to_peers(Addrs, Base),
            apply_resolved_peers(NewPeers, OldPeers, State);
        {error, Reason} ->
            gateway_cluster_metrics:record_discovery_resolve_failure(),
            logger:warning(
                "cluster discovery: DNS resolve failed for ~p: ~p",
                [DnsName, Reason]
            ),
            {OldPeers, State}
    end.

-spec apply_resolved_peers([peer()], [peer()], state()) -> {[peer()], state()}.
apply_resolved_peers([], OldPeers, State) when OldPeers =/= [] ->
    gateway_cluster_metrics:record_discovery_resolve_failure(),
    logger:warning("cluster discovery: DNS resolved no peers; retaining previous peers"),
    {OldPeers, State};
apply_resolved_peers(NewPeers, OldPeers, State) ->
    case NewPeers =/= OldPeers of
        true ->
            persistent_term:put(?PEERS_KEY, NewPeers),
            notify_subscribers(NewPeers, State),
            {NewPeers, State#{peers := NewPeers}};
        false ->
            {NewPeers, State}
    end.

-spec ip_addrs_to_peers([inet:ip_address()], string()) -> [peer()].
ip_addrs_to_peers(Addrs, Base) ->
    Sorted = lists:usort(Addrs),
    Bounded = lists:sublist(Sorted, ?MAX_DISCOVERED_PEERS),
    Peers = lists:filtermap(fun(Addr) -> ip_addr_to_peer(Addr, Base) end, Bounded),
    filter_self(Peers).

-spec ip_addr_to_peer(inet:ip_address(), string()) -> {true, peer()} | false.
ip_addr_to_peer(Addr, Base) ->
    case inet:ntoa(Addr) of
        AddrString when is_list(AddrString) -> node_name_to_peer(Base, AddrString);
        {error, einval} -> false
    end.

-spec node_name_to_peer(string(), string()) -> {true, peer()} | false.
node_name_to_peer(Base, AddrString) ->
    case gateway_node_name:from_parts(Base, AddrString) of
        {ok, Peer} -> {true, Peer};
        error -> false
    end.

-spec default_resolver(string()) -> {ok, [inet:ip_address()]} | {error, term()}.
default_resolver(DnsName) ->
    case inet:getaddrs(DnsName, inet) of
        {ok, Addrs} ->
            {ok, Addrs};
        {error, NativeReason} ->
            fallback_resolver(DnsName, NativeReason)
    end.

-spec fallback_resolver(string(), term()) -> {ok, [inet:ip_address()]} | {error, term()}.
fallback_resolver(DnsName, NativeReason) ->
    case inet_res:resolve(DnsName, in, a) of
        {ok, Msg} ->
            Addrs = [
                inet_dns:rr(RR, data)
             || RR <- inet_dns:msg(Msg, anlist),
                inet_dns:rr(RR, type) =:= a
            ],
            {ok, Addrs};
        {error, DnsReason} ->
            {error, {NativeReason, DnsReason}}
    end.

-spec notify_subscribers([peer()], state()) -> ok.
notify_subscribers(Peers, #{subscribers := Subs}) ->
    Msg = {cluster_peers_changed, Peers},
    lists:foreach(fun({Pid, _Ref}) -> Pid ! Msg end, Subs),
    ok.

-spec add_subscriber(pid(), state()) -> state().
add_subscriber(Pid, #{subscribers := Subs, peers := Peers} = State) ->
    case lists:keyfind(Pid, 1, Subs) of
        false ->
            Ref = erlang:monitor(process, Pid),
            Pid ! {cluster_peers_changed, Peers},
            State#{subscribers := [{Pid, Ref} | Subs]};
        _ ->
            State
    end.

-spec remove_subscriber(pid(), state()) -> state().
remove_subscriber(Pid, #{subscribers := Subs} = State) ->
    case lists:keytake(Pid, 1, Subs) of
        {value, {Pid, Ref}, Rest} ->
            erlang:demonitor(Ref, [flush]),
            State#{subscribers := Rest};
        false ->
            State
    end.

-spec schedule_poll(non_neg_integer(), state()) -> state().
schedule_poll(DelayMs, State) ->
    cancel_timer(State),
    Ref = erlang:send_after(DelayMs, self(), poll),
    State#{timer := Ref}.

-spec cancel_timer(state() | reference() | undefined) -> ok.
cancel_timer(#{timer := undefined}) ->
    ok;
cancel_timer(#{timer := Ref}) when is_reference(Ref) ->
    _ = erlang:cancel_timer(Ref),
    ok;
cancel_timer(undefined) ->
    ok;
cancel_timer(Ref) when is_reference(Ref) ->
    _ = erlang:cancel_timer(Ref),
    ok.

-spec config(atom(), term()) -> term().
config(Key, Default) ->
    case fluxer_gateway_env:get(Key) of
        undefined -> Default;
        Value -> Value
    end.

-spec option_or_config(map(), atom(), atom(), term()) -> term().
option_or_config(Options, OptionKey, ConfigKey, Default) ->
    case maps:find(OptionKey, Options) of
        {ok, Value} -> Value;
        error -> config(ConfigKey, Default)
    end.

-spec optional_string_value(term()) -> string() | undefined.
optional_string_value(undefined) ->
    undefined;
optional_string_value(Value) when is_binary(Value) ->
    binary_to_list(Value);
optional_string_value(Value) when is_list(Value) ->
    validate_string(Value);
optional_string_value(_Value) ->
    undefined.

-spec required_string_value(term(), string()) -> string().
required_string_value(Value, _Default) when is_binary(Value) ->
    binary_to_list(Value);
required_string_value(Value, Default) when is_list(Value) ->
    case validate_string(Value) of
        undefined -> Default;
        S -> S
    end;
required_string_value(_Value, Default) ->
    Default.

-spec validate_string([term()]) -> string() | undefined.
validate_string([]) ->
    [];
validate_string([H | _] = List) when is_integer(H) ->
    case lists:all(fun is_integer/1, List) of
        true -> lists:map(fun require_char/1, List);
        false -> undefined
    end;
validate_string(_) ->
    undefined.

-spec require_char(term()) -> char().
require_char(C) when is_integer(C) -> C;
require_char(_) -> $?.

-spec positive_integer_value(term(), pos_integer()) -> pos_integer().
positive_integer_value(Value, _Default) when is_integer(Value), Value > 0 ->
    Value;
positive_integer_value(_Value, Default) ->
    Default.

-spec resolver_value(term()) -> resolver().
resolver_value(Value) when is_function(Value, 1) ->
    wrap_resolver(Value);
resolver_value(_Value) ->
    fun default_resolver/1.

-spec wrap_resolver(fun((term()) -> term())) -> resolver().
wrap_resolver(Fun) ->
    fun(Name) -> coerce_resolver_result(Fun(Name)) end.

-spec coerce_resolver_result(term()) -> {ok, [inet:ip_address()]} | {error, term()}.
coerce_resolver_result({ok, Addrs}) when is_list(Addrs) ->
    {ok, filter_ip_addresses(Addrs)};
coerce_resolver_result({error, Reason}) ->
    {error, Reason};
coerce_resolver_result(Other) ->
    {error, {unexpected_resolver_result, Other}}.

-spec normalize_static_peers(term()) -> [peer()].
normalize_static_peers(Peers) when is_list(Peers) ->
    lists:usort([Peer || Peer <- Peers, is_atom(Peer)]);
normalize_static_peers(_Peers) ->
    [].

-spec filter_self([peer()]) -> [peer()].
filter_self(Peers) ->
    Self = node(),
    [P || P <- lists:usort(Peers), P =/= Self].

-spec filter_ip_addresses([term()]) -> [inet:ip_address()].
filter_ip_addresses(List) ->
    lists:filtermap(fun coerce_ip_address/1, List).

-spec coerce_ip_address(term()) -> {true, inet:ip_address()} | false.
coerce_ip_address({A, B, C, D} = Addr) when
    is_integer(A), is_integer(B), is_integer(C), is_integer(D)
->
    {true, Addr};
coerce_ip_address({A, B, C, D, E, F, G, H} = Addr) when
    is_integer(A),
    is_integer(B),
    is_integer(C),
    is_integer(D),
    is_integer(E),
    is_integer(F),
    is_integer(G),
    is_integer(H)
->
    {true, Addr};
coerce_ip_address(_) ->
    false.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

poll_empty_dns_answer_keeps_existing_peers_test() ->
    OldPeers = ['fluxer_gateway@127.0.0.2'],
    State0 = discovery_test_state(fun(_Name) -> {ok, []} end, OldPeers),
    {Peers, State1} = poll(State0),
    ?assertEqual(OldPeers, Peers),
    ?assertEqual(OldPeers, maps:get(peers, State1)).

poll_empty_initial_dns_answer_stays_empty_test() ->
    State0 = discovery_test_state(fun(_Name) -> {ok, []} end, []),
    {Peers, State1} = poll(State0),
    ?assertEqual([], Peers),
    ?assertEqual([], maps:get(peers, State1)).

init_seeds_existing_peers_from_persistent_term_test() ->
    PreviousPeers = persistent_term:get(?PEERS_KEY, undefined),
    OldPeers = ['fluxer_gateway@127.0.0.2'],
    persistent_term:put(?PEERS_KEY, OldPeers),
    {ok, State} = init(#{
        dns_name => undefined,
        node_basename => "fluxer_gateway",
        poll_interval_ms => 5000,
        resolver => fun(_Name) -> {ok, []} end
    }),
    ?assertEqual(OldPeers, maps:get(peers, State)),
    cancel_timer(maps:get(timer, State)),
    case PreviousPeers of
        undefined -> persistent_term:erase(?PEERS_KEY);
        _ -> persistent_term:put(?PEERS_KEY, PreviousPeers)
    end.

discovery_test_state(Resolver, Peers) ->
    #{
        dns_name => "fluxer-gateway-headless.fluxer.svc.cluster.local",
        node_basename => "fluxer_gateway",
        poll_interval_ms => 5000,
        resolver => Resolver,
        peers => Peers,
        subscribers => [],
        timer => undefined
    }.

-endif.
