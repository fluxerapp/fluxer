%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(fluxer_gateway_app).
-typing([eqwalizer]).
-behaviour(application).
-export([start/2, prep_stop/1, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    erlang:system_flag(fullsweep_after, 10),
    init_subsystems(),
    {ok, Pid} = fluxer_gateway_sup:start_link(),
    {ok, _} = start_cowboy(),
    {ok, Pid}.

-spec init_subsystems() -> ok.
init_subsystems() ->
    _ = fluxer_gateway_env:load(),
    ok = gateway_guild_pin_keeper:apply_boot_pins(),
    gateway_compress:init(),
    gateway_cluster_metrics:init(),
    process_registry:init(),
    passive_sync_registry:init(),
    ok.

-spec start_cowboy() -> {ok, pid()} | {error, term()}.
start_cowboy() ->
    Port = fluxer_gateway_env:get(port),
    Dispatch = cowboy_router:compile([
        {'_', [
            {<<"/_health">>, health_handler, liveness},
            {<<"/_health/ready">>, health_handler, readiness},
            {<<"/_health/drain">>, health_handler, drain},
            {<<"/_health/undrain">>, health_handler, undrain},
            {<<"/_metrics">>, metrics_handler, []},
            {<<"/">>, gateway_handler, []}
        ]}
    ]),
    case
        cowboy:start_clear(
            http,
            #{
                socket_opts => [{port, Port}],
                max_connections => infinity
            },
            #{
                env => #{dispatch => Dispatch},
                max_frame_size => 4096,
                connection_type => supervisor,
                dynamic_buffer => {512, 131072}
            }
        )
    of
        {ok, Pid} -> {ok, Pid};
        {error, E} -> {error, E}
    end.

-spec prep_stop(term()) -> term().
prep_stop(State) ->
    State.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
