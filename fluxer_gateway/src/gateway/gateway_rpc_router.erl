%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_router).
-typing([eqwalizer]).

-export([execute/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec execute(binary(), map()) -> term().
execute(Method, Params) ->
    ensure_event_pause_allows(Method),
    route_method(Method, Params).

-spec ensure_event_pause_allows(binary()) -> ok.
ensure_event_pause_allows(Method) ->
    case gateway_event_pause:is_paused() andalso method_is_mutating(Method) of
        true ->
            gateway_rpc_error:raise(<<"event_mutations_paused">>);
        false ->
            ok
    end.

-spec method_is_mutating(binary()) -> boolean().
method_is_mutating(Method) ->
    case binary:split(Method, <<".">>) of
        [<<"process">>, _Operation] ->
            false;
        [_Namespace, Operation] ->
            is_mutating_operation(Operation);
        _ ->
            true
    end.

-spec is_mutating_operation(binary()) -> boolean().
is_mutating_operation(<<"get", _/binary>>) -> false;
is_mutating_operation(<<"list", _/binary>>) -> false;
is_mutating_operation(<<"has", _/binary>>) -> false;
is_mutating_operation(<<"check", _/binary>>) -> false;
is_mutating_operation(<<"can", _/binary>>) -> false;
is_mutating_operation(<<"resolve", _/binary>>) -> false;
is_mutating_operation(_) -> true.

-spec route_method(binary(), map()) -> term().
route_method(<<"guild.", _/binary>> = Method, Params) ->
    gateway_rpc_guild:execute_method(Method, Params);
route_method(<<"presence.", _/binary>> = Method, Params) ->
    gateway_rpc_presence:execute_method(Method, Params);
route_method(<<"push.", _/binary>> = Method, Params) ->
    gateway_rpc_push:execute_method(Method, Params);
route_method(<<"call.", _/binary>> = Method, Params) ->
    gateway_rpc_call:execute_method(Method, Params);
route_method(<<"voice.", _/binary>> = Method, Params) ->
    gateway_rpc_voice:execute_method(Method, Params);
route_method(<<"process.", _/binary>> = Method, Params) ->
    gateway_rpc_misc:execute_method(Method, Params);
route_method(Method, _Params) ->
    gateway_rpc_error:raise(<<"Unknown method: ", Method/binary>>).

-ifdef(TEST).

method_is_mutating_test() ->
    ?assert(method_is_mutating(<<"guild.dispatch">>)),
    ?assert(method_is_mutating(<<"call.create">>)),
    ?assertNot(method_is_mutating(<<"guild.get_data">>)),
    ?assertNot(method_is_mutating(<<"guild.list_members">>)),
    ?assertNot(method_is_mutating(<<"guild.has_member">>)),
    ?assertNot(method_is_mutating(<<"guild.can_manage_roles">>)),
    ?assertNot(method_is_mutating(<<"guild.resolve_all_mentions">>)),
    ?assertNot(method_is_mutating(<<"guild.resolve_mention_sources">>)),
    ?assertNot(method_is_mutating(<<"guild.resolve_mention_sources_page">>)),
    ?assertNot(method_is_mutating(<<"process.node_stats">>)).

ensure_event_pause_allows_blocks_mutations_test() ->
    Original = persistent_term:get({gateway_event_pause, paused}, false),
    persistent_term:put({gateway_event_pause, paused}, true),
    try
        ?assertError(
            {gateway_rpc_error, <<"event_mutations_paused">>},
            ensure_event_pause_allows(<<"guild.dispatch">>)
        ),
        ?assertEqual(ok, ensure_event_pause_allows(<<"guild.get_data">>))
    after
        persistent_term:put({gateway_event_pause, paused}, Original)
    end.

-endif.
