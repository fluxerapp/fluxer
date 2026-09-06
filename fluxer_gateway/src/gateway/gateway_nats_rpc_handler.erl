%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_nats_rpc_handler).
-typing([eqwalizer]).

-export([
    handle_rpc_request/3,
    is_rpc_subject/1,
    rpc_subjects_for_role/1,
    do_connect/1,
    do_subscribe/1,
    schedule_reconnect/1,
    max_handlers/0,
    parse_nats_url/1,
    build_connect_opts/1
]).

-define(DEFAULT_MAX_HANDLERS, 512).
-define(RECONNECT_DELAY_MS, 2000).
-define(CONNECT_TIMEOUT_MS, 10000).
-define(RPC_SUBJECT_WILDCARD, <<"rpc.gateway.>">>).
-define(QUEUE_GROUP, <<"gateway">>).

-spec rpc_subjects_for_role(atom()) -> [binary()].
rpc_subjects_for_role(all) ->
    [?RPC_SUBJECT_WILDCARD];
rpc_subjects_for_role(guilds) ->
    [<<"rpc.gateway.guild.>">>, <<"rpc.gateway.voice.>">>, <<"rpc.gateway.process.>">>];
rpc_subjects_for_role(sessions) ->
    [];
rpc_subjects_for_role(presence) ->
    [<<"rpc.gateway.presence.>">>];
rpc_subjects_for_role(calls) ->
    [<<"rpc.gateway.call.>">>, <<"rpc.gateway.voice.>">>];
rpc_subjects_for_role(push) ->
    [<<"rpc.gateway.push.>">>];
rpc_subjects_for_role(_) ->
    [].

-spec handle_rpc_request(binary(), binary(), binary()) -> ok.
handle_rpc_request(Subject, Payload, ReplyTo) ->
    Method = strip_rpc_prefix(Subject),
    Response = execute_rpc_method(Method, Payload),
    ResponseBin = iolist_to_binary(json:encode(Response)),
    _ = gateway_nats_pool:pub_reply(ReplyTo, ResponseBin),
    ok.

-spec strip_rpc_prefix(binary()) -> binary().
strip_rpc_prefix(<<"rpc.gateway.", Method/binary>>) ->
    Method;
strip_rpc_prefix(Subject) ->
    Subject.

-spec execute_rpc_method(binary(), binary()) -> map().
execute_rpc_method(Method, PayloadBin) ->
    try
        Params = json:decode(PayloadBin),
        execute_rpc_params(Method, Params)
    catch
        error:{gateway_rpc_error, Message} ->
            handle_throw_error(Method, Message);
        error:{validation, _Reason} ->
            handle_throw_error(Method, <<"invalid_params">>);
        throw:{error, Message} ->
            handle_throw_error(Method, Message);
        throw:Message ->
            handle_throw_error(Method, Message);
        exit:timeout ->
            logger:warning("Gateway NATS RPC exit timeout", #{method => Method}),
            #{<<"ok">> => false, <<"error">> => <<"timeout">>};
        exit:{timeout, Info} ->
            logger:warning("Gateway NATS RPC exit timeout", #{method => Method, info => Info}),
            #{<<"ok">> => false, <<"error">> => <<"timeout">>};
        Class:Reason ->
            logger:error("Gateway NATS RPC method execution failed", #{
                method => Method, class => Class, reason => Reason
            }),
            #{<<"ok">> => false, <<"error">> => <<"internal_error">>}
    end.

-spec execute_rpc_params(binary(), term()) -> map().
execute_rpc_params(Method, Params) when is_map(Params) ->
    Result = guild_data_wire:payload(gateway_rpc_router:execute(Method, Params)),
    #{<<"ok">> => true, <<"result">> => Result};
execute_rpc_params(_Method, _Params) ->
    error(badarg).

-spec handle_throw_error(binary(), term()) -> map().
handle_throw_error(Method, Message) ->
    ErrBin = error_binary(Message),
    case lists:member(ErrBin, [<<"guild_not_found">>, <<"forbidden">>]) of
        true ->
            ok;
        false ->
            logger:warning(
                "Gateway NATS RPC throw error. method=~ts error=~ts",
                [Method, ErrBin]
            )
    end,
    #{<<"ok">> => false, <<"error">> => ErrBin}.

-spec error_binary(term()) -> binary().
error_binary(Value) when is_binary(Value) ->
    Value;
error_binary(Value) when is_list(Value) ->
    characters_to_binary_or_format(Value);
error_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
error_binary(Value) ->
    format_error_binary(Value).

-spec characters_to_binary_or_format(term()) -> binary().
characters_to_binary_or_format(Value) ->
    case type_conv:unicode_to_binary(Value) of
        Bin when is_binary(Bin) -> Bin;
        undefined -> format_error_binary(Value)
    end.

-spec format_error_binary(term()) -> binary().
format_error_binary(Value) ->
    iolist_to_binary(io_lib:format("~p", [Value])).

-spec is_rpc_subject(binary()) -> boolean().
is_rpc_subject(<<"rpc.gateway.", _/binary>>) -> true;
is_rpc_subject(_) -> false.

-spec do_connect(map()) -> map().
do_connect(State) ->
    NatsUrl = fluxer_gateway_env:get(nats_core_url),
    AuthToken = fluxer_gateway_env:get(nats_auth_token),
    case parse_nats_url(NatsUrl) of
        {ok, Host, Port} ->
            spawn_connect(Host, Port, AuthToken, State);
        {error, Reason} ->
            logger:error("Gateway NATS RPC failed to parse URL", #{reason => Reason}),
            schedule_reconnect(State)
    end.

-spec spawn_connect(string(), inet:port_number(), term(), map()) -> map().
spawn_connect(Host, Port, AuthToken, State) ->
    Opts = build_connect_opts(AuthToken),
    Parent = self(),
    Token = make_ref(),
    ConnPid = spawn(fun() -> connect_and_notify(Host, Port, Opts, Parent) end),
    TimerRef = erlang:send_after(?CONNECT_TIMEOUT_MS, self(), {connect_timeout, Token}),
    State#{
        connecting_pid => ConnPid,
        connecting_token => Token,
        connecting_timer => TimerRef
    }.

-spec connect_and_notify(string(), inet:port_number(), map(), pid()) -> ok.
connect_and_notify(Host, Port, Opts, Parent) ->
    _ =
        case nats:connect(Host, Port, Opts) of
            {ok, Conn} ->
                ok = nats:controlling_process(Conn, Parent),
                Parent ! {nats_connect_result, self(), {ok, Conn}};
            {error, Reason} ->
                Parent ! {nats_connect_result, self(), {error, Reason}}
        end,
    ok.

-spec do_subscribe(map()) -> map().
do_subscribe(#{conn := Conn, rpc_enabled := RpcEnabled} = State) when
    Conn =/= undefined
->
    case RpcEnabled of
        false ->
            logger:info(
                "Gateway NATS RPC connected but subscription disabled"
                " (GATEWAY_NATS_RPC_ENABLED=false)"
            ),
            State;
        _ ->
            subscribe_for_role(Conn, State)
    end;
do_subscribe(State) ->
    State.

-spec subscribe_for_role(nats:conn(), map()) -> map().
subscribe_for_role(Conn, State) ->
    Role = fluxer_gateway_sup:current_role(),
    Subjects = rpc_subjects_for_role(Role),
    case Subjects of
        [] ->
            logger:info("Gateway NATS RPC connected but not subscribing", #{role => Role}),
            State;
        _ ->
            Sids = subscribe_subjects(Conn, Role, Subjects),
            State#{sub => Sids}
    end.

-spec subscribe_subjects(nats:conn(), atom(), [binary()]) -> [term()].
subscribe_subjects(Conn, Role, Subjects) ->
    lists:filtermap(
        fun(Subject) ->
            subscribe_subject(Conn, Role, Subject)
        end,
        Subjects
    ).

-spec subscribe_subject(nats:conn(), atom(), binary()) -> {true, term()} | false.
subscribe_subject(Conn, Role, Subject) ->
    case nats:sub(Conn, Subject, #{queue_group => ?QUEUE_GROUP}) of
        {ok, Sid} ->
            logger:info("Gateway NATS RPC subscribed", #{subject => Subject, role => Role}),
            {true, Sid};
        {error, Reason} ->
            logger:error("Gateway NATS RPC failed to subscribe", #{
                subject => Subject, reason => Reason
            }),
            false
    end.

-spec schedule_reconnect(map()) -> map().
schedule_reconnect(State) ->
    erlang:send_after(?RECONNECT_DELAY_MS, self(), connect),
    State.

-spec max_handlers() -> pos_integer().
max_handlers() ->
    case fluxer_gateway_env:get(gateway_nats_rpc_max_handlers) of
        Value when is_integer(Value), Value > 0 -> Value;
        _ -> ?DEFAULT_MAX_HANDLERS
    end.

-spec parse_nats_url(term()) -> {ok, string(), inet:port_number()} | {error, term()}.
parse_nats_url(Url) ->
    gateway_nats_pool_conn:parse_nats_url(Url).

-spec build_connect_opts(term()) -> map().
build_connect_opts(AuthToken) ->
    gateway_nats_pool_conn:build_connect_opts(AuthToken).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_nats_url_test() ->
    ?assertEqual({ok, "127.0.0.1", 4222}, parse_nats_url(<<"nats://127.0.0.1:4222">>)),
    ?assertEqual({ok, "localhost", 4222}, parse_nats_url(<<"nats://localhost:4222">>)),
    ?assertEqual({ok, "localhost", 4222}, parse_nats_url(<<"nats://localhost">>)),
    ?assertEqual({ok, "127.0.0.1", 4222}, parse_nats_url("nats://127.0.0.1:4222")),
    ?assertEqual({error, invalid_nats_url}, parse_nats_url(undefined)).

execute_rpc_method_maps_validation_failure_to_invalid_params_test() ->
    Payload = iolist_to_binary(
        json:encode(#{
            <<"guild_id">> => <<"nope">>,
            <<"user_id">> => <<"2">>,
            <<"channel_id">> => <<"0">>
        })
    ),
    ?assertEqual(
        #{<<"ok">> => false, <<"error">> => <<"invalid_params">>},
        execute_rpc_method(<<"guild.get_user_permissions">>, Payload)
    ).

execute_rpc_method_maps_oversized_batch_to_batch_too_large_test() ->
    GuildIds = [integer_to_binary(N) || N <- lists:seq(1, 101)],
    Payload = iolist_to_binary(
        json:encode(#{
            <<"guild_ids">> => GuildIds,
            <<"user_id">> => <<"1">>,
            <<"channel_id">> => <<"0">>
        })
    ),
    ?assertEqual(
        #{<<"ok">> => false, <<"error">> => <<"batch_too_large">>},
        execute_rpc_method(<<"guild.get_user_permissions_batch">>, Payload)
    ).

rpc_subjects_for_role_does_not_route_rpc_to_websocket_test() ->
    ?assertEqual([], rpc_subjects_for_role(websocket)).

rpc_subjects_for_role_routes_namespaces_to_state_roles_test() ->
    ?assertEqual([?RPC_SUBJECT_WILDCARD], rpc_subjects_for_role(all)),
    ?assert(lists:member(<<"rpc.gateway.guild.>">>, rpc_subjects_for_role(guilds))),
    ?assertEqual([], rpc_subjects_for_role(sessions)),
    ?assert(lists:member(<<"rpc.gateway.presence.>">>, rpc_subjects_for_role(presence))),
    ?assert(lists:member(<<"rpc.gateway.call.>">>, rpc_subjects_for_role(calls))),
    ?assert(lists:member(<<"rpc.gateway.push.>">>, rpc_subjects_for_role(push))).

rpc_subjects_for_role_covers_all_router_prefixes_test() ->
    Subjects = lists:append([
        rpc_subjects_for_role(guilds),
        rpc_subjects_for_role(presence),
        rpc_subjects_for_role(calls),
        rpc_subjects_for_role(push)
    ]),
    Prefixes = [
        <<"guild">>, <<"presence">>, <<"push">>, <<"call">>, <<"voice">>, <<"process">>
    ],
    lists:foreach(
        fun(Prefix) ->
            ExpectedSubject = <<"rpc.gateway.", Prefix/binary, ".>">>,
            ?assert(lists:member(ExpectedSubject, Subjects))
        end,
        Prefixes
    ).

-endif.
