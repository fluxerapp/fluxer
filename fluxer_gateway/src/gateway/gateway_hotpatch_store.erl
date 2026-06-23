%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_hotpatch_store).
-typing([eqwalizer]).

-export([
    connect/0,
    fetch_events/1,
    append_event/5,
    append_event/6,
    audit_applied/5,
    normalize_event_rows/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(STMT_FETCH_EVENTS, gateway_hotpatch_fetch_events_by_build).
-define(STMT_APPEND_EVENT, gateway_hotpatch_append_event).
-define(STMT_AUDIT_APPLIED, gateway_hotpatch_audit_applied).

-spec connect() -> ok | {error, term()}.
connect() ->
    case gateway_hotpatch_runtime:get(hotpatch_cassandra_hosts, undefined) of
        Hosts when is_list(Hosts), Hosts =/= [] -> connect_hosts(Hosts);
        Hosts when is_binary(Hosts), byte_size(Hosts) > 0 ->
            start_erlcass(binary_to_list(Hosts));
        _ ->
            {error, missing_hotpatch_cassandra_hosts}
    end.

-spec connect_hosts(term()) -> ok | {error, term()}.
connect_hosts(Hosts) ->
    case type_conv:to_list(Hosts) of
        String when is_list(String), String =/= [] -> start_erlcass(String);
        _ -> {error, missing_hotpatch_cassandra_hosts}
    end.

-spec fetch_events(binary()) -> {ok, [map()]} | {error, term()}.
fetch_events(BuildSha) when is_binary(BuildSha) ->
    case erlcass:execute(?STMT_FETCH_EVENTS, [BuildSha]) of
        {ok, _Columns, Rows} -> {ok, normalize_event_rows(Rows)};
        ok -> {ok, []};
        {error, Reason} -> {error, Reason}
    end.

-spec append_event(binary(), binary(), binary(), binary(), binary()) ->
    {ok, binary()} | {error, term()}.
append_event(CreatedBy, SignerKeyId, Signature, BundleSha256, Bundle) when
    is_binary(CreatedBy),
    is_binary(SignerKeyId),
    is_binary(Signature),
    is_binary(BundleSha256),
    is_binary(Bundle)
->
    BuildSha = gateway_hotpatch_runtime:build_sha(),
    append_event(BuildSha, CreatedBy, SignerKeyId, Signature, BundleSha256, Bundle).

-spec append_event(binary(), binary(), binary(), binary(), binary(), binary()) ->
    {ok, binary()} | {error, term()}.
append_event(BuildSha, CreatedBy, SignerKeyId, Signature, BundleSha256, Bundle) when
    is_binary(BuildSha),
    is_binary(CreatedBy),
    is_binary(SignerKeyId),
    is_binary(Signature),
    is_binary(BundleSha256),
    is_binary(Bundle)
->
    case erlcass_uuid:gen_time() of
        {ok, EventId} ->
            Params = append_event_params(
                BuildSha, EventId, CreatedBy, SignerKeyId, BundleSha256, Signature, Bundle
            ),
            execute_append_event(EventId, Params);
        {error, Reason} ->
            {error, {event_id_failed, Reason}}
    end.

-spec append_event_params(
    binary(), binary(), binary(), binary(), binary(), binary(), binary()
) -> list().
append_event_params(BuildSha, EventId, CreatedBy, SignerKeyId, BundleSha256, Signature, Bundle) ->
    [
        BuildSha,
        EventId,
        1,
        <<"beam_bundle">>,
        CreatedBy,
        SignerKeyId,
        BundleSha256,
        Signature,
        Bundle
    ].

-spec execute_append_event(binary(), list()) -> {ok, binary()} | {error, term()}.
execute_append_event(EventId, Params) ->
    case erlcass:execute(?STMT_APPEND_EVENT, Params) of
        ok -> {ok, EventId};
        {ok, _Columns, _Rows} -> {ok, EventId};
        {error, Reason} -> {error, Reason}
    end.

-spec audit_applied(binary(), binary(), binary(), map(), ok | {error, term()}) ->
    ok | {error, term()}.
audit_applied(BuildSha, NodeName, EventId, Summary, Result) when
    is_binary(BuildSha), is_binary(NodeName), is_binary(EventId), is_map(Summary)
->
    ModuleCount = maps:get(module_count, Summary, 0),
    BundleSha256 = maps:get(bundle_sha256, Summary, <<>>),
    Status = audit_status(Result),
    Error = audit_error(Result),
    Params = [BuildSha, NodeName, EventId, ModuleCount, BundleSha256, Status, Error],
    case erlcass:execute(?STMT_AUDIT_APPLIED, Params) of
        ok -> ok;
        {ok, _Columns, _Rows} -> ok;
        {error, Reason} -> {error, Reason}
    end.

-spec normalize_event_rows([list()]) -> [map()].
normalize_event_rows(Rows) when is_list(Rows) ->
    lists:filtermap(fun normalize_event_row/1, Rows).

-spec start_erlcass(string()) -> ok | {error, term()}.
start_erlcass(Hosts) ->
    Keyspace = gateway_hotpatch_runtime:get(hotpatch_cassandra_keyspace, <<"fluxer">>),
    Port = gateway_hotpatch_runtime:get(hotpatch_cassandra_port, 9042),
    Options0 = [
        {contact_points, type_conv:ensure_binary(Hosts)},
        {port, Port},
        {latency_aware_routing, true},
        {token_aware_routing, true},
        {tcp_nodelay, true},
        {tcp_keepalive, {true, 60}},
        {connect_timeout, 5000},
        {request_timeout, 5000},
        {retry_policy, {default, true}},
        {default_consistency_level, 6}
    ],
    case maybe_credentials(Options0) of
        {ok, Options} -> start_erlcass_with_options(Keyspace, Options);
        {error, Reason} -> {error, Reason}
    end.

-spec start_erlcass_with_options(binary(), list()) -> ok | {error, term()}.
start_erlcass_with_options(Keyspace, Options) ->
    application:set_env(erlcass, keyspace, Keyspace),
    application:set_env(erlcass, cluster_options, Options),
    application:set_env(erlcass, log_level, 2),
    case application:ensure_all_started(erlcass) of
        {ok, _Apps} -> prepare_statements();
        {error, {erlcass, {already_started, erlcass}}} -> prepare_statements();
        {error, Reason} -> {error, {erlcass_start_failed, Reason}}
    end.

-spec prepare_statements() -> ok | {error, term()}.
prepare_statements() ->
    Statements = [
        {?STMT_FETCH_EVENTS, <<
            "SELECT event_id, schema_version, kind, created_by, signer_key_id, "
            "bundle_sha256, signature, bundle "
            "FROM gateway_hotpatch_events_by_build WHERE build_sha = ?"
        >>},
        {?STMT_APPEND_EVENT, <<
            "INSERT INTO gateway_hotpatch_events_by_build "
            "(build_sha, event_id, schema_version, kind, created_at, created_by, "
            "signer_key_id, bundle_sha256, signature, bundle) "
            "VALUES (?, ?, ?, ?, toTimestamp(now()), ?, ?, ?, ?, ?)"
        >>},
        {?STMT_AUDIT_APPLIED, <<
            "INSERT INTO gateway_hotpatch_applied_by_node "
            "(build_sha, node_name, event_id, applied_at, module_count, bundle_sha256, "
            "status, error) "
            "VALUES (?, ?, ?, toTimestamp(now()), ?, ?, ?, ?)"
        >>}
    ],
    prepare_statements(Statements).

-spec prepare_statements([{atom(), binary()}]) -> ok | {error, term()}.
prepare_statements([]) ->
    ok;
prepare_statements([{Id, Query} | Rest]) ->
    case prepare_statement(Id, Query) of
        ok -> prepare_statements(Rest);
        {error, Reason} -> {error, Reason}
    end.

-spec prepare_statement(atom(), binary()) -> ok | {error, term()}.
prepare_statement(Id, Query) ->
    case erlcass:add_prepare_statement(Id, Query) of
        ok -> ok;
        {error, already_exist} -> ok;
        {error, Reason} -> {error, {prepare_failed, Id, Reason}}
    end.

-spec maybe_credentials(list()) -> {ok, list()} | {error, term()}.
maybe_credentials(Options) ->
    Username = gateway_hotpatch_runtime:get(hotpatch_cassandra_username, undefined),
    Password = gateway_hotpatch_runtime:get(hotpatch_cassandra_password, undefined),
    case {Username, Password} of
        {U, P} when is_binary(U), byte_size(U) > 0, is_binary(P), byte_size(P) > 0 ->
            {ok, [{credentials, {U, P}} | Options]};
        _ ->
            {error, missing_hotpatch_cassandra_credentials}
    end.

-spec normalize_event_row(list()) -> {true, map()} | false.
normalize_event_row([
    EventId,
    SchemaVersion,
    Kind,
    CreatedBy,
    SignerKeyId,
    BundleSha256,
    Signature,
    Bundle
]) when
    is_binary(EventId),
    is_binary(SignerKeyId),
    is_binary(BundleSha256),
    is_binary(Signature),
    is_binary(Bundle)
->
    {true, #{
        event_id => EventId,
        schema_version => SchemaVersion,
        kind => Kind,
        created_by => CreatedBy,
        signer_key_id => SignerKeyId,
        bundle_sha256 => BundleSha256,
        signature => Signature,
        bundle => Bundle
    }};
normalize_event_row(_Row) ->
    false.

-spec audit_status(ok | {error, term()}) -> binary().
audit_status(ok) -> <<"ok">>;
audit_status({error, _Reason}) -> <<"error">>.

-spec audit_error(ok | {error, term()}) -> binary().
audit_error(ok) ->
    <<>>;
audit_error({error, Reason}) ->
    iolist_to_binary(io_lib:format("~0tp", [Reason])).

-ifdef(TEST).

normalize_event_rows_test() ->
    EventId = <<1:128>>,
    Row = [
        EventId,
        1,
        <<"beam_bundle">>,
        <<"hampus">>,
        <<"ops">>,
        <<2:256>>,
        <<3:512>>,
        <<"bundle">>
    ],
    ?assertEqual(
        [
            #{
                event_id => EventId,
                schema_version => 1,
                kind => <<"beam_bundle">>,
                created_by => <<"hampus">>,
                signer_key_id => <<"ops">>,
                bundle_sha256 => <<2:256>>,
                signature => <<3:512>>,
                bundle => <<"bundle">>
            }
        ],
        normalize_event_rows([Row, [invalid]])
    ).

-endif.
