%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_shutdown_drain).
-typing([eqwalizer]).

-export([run/0]).

-define(FRAME_FLUSH_MS, 500).

-spec run() -> ok.
run() ->
    case gateway_node_router:is_draining() of
        true -> ok;
        false -> drain()
    end.

-spec drain() -> ok.
drain() ->
    persistent_term:put({fluxer_gateway, draining}, true),
    ok = gateway_cluster_handoff:drain_notify_role(),
    logger:info("Gateway shutdown drain: notified clients"),
    timer:sleep(?FRAME_FLUSH_MS),
    ok.
