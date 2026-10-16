%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_cache_api).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([
    cast_owner/2,
    safe_call_if_enabled/2,
    safe_cast_if_enabled/1,
    rebalance/0,
    generation/0
]).

-spec cast_owner(integer(), term()) -> ok.
cast_owner(UserId, Request) ->
    case presence_cache_bulk:resolve_owner_nodes(UserId) of
        [OwnerNode | _] when OwnerNode =:= node() -> cast_local_if_enabled(Request);
        [OwnerNode | _] -> presence_cache_bulk:fire_remote_cast(OwnerNode, Request);
        [] -> ok
    end.

-spec safe_call_if_enabled(term(), term()) -> term().
safe_call_if_enabled(Request, Default) ->
    case fluxer_gateway_sup:role_enabled(presence) of
        true -> safe_call(Request, ?DEFAULT_GEN_SERVER_TIMEOUT, Default);
        false -> Default
    end.

-spec safe_cast_if_enabled(term()) -> ok.
safe_cast_if_enabled(Msg) ->
    case persistent_term:get(presence_noop, false) of
        true -> ok;
        false -> cast_local_if_enabled(Msg)
    end.

-spec rebalance() -> ok.
rebalance() ->
    case persistent_term:get(presence_noop, false) of
        true -> ok;
        false -> rebalance_if_enabled()
    end.

-spec generation() -> non_neg_integer().
generation() ->
    case fluxer_gateway_sup:role_enabled(presence) of
        true -> normalize_generation(safe_call(get_generation, 2000, 0));
        false -> 0
    end.

-spec normalize_generation(term()) -> non_neg_integer().
normalize_generation(G) when is_integer(G), G >= 0 -> G;
normalize_generation(_) -> 0.

-spec rebalance_if_enabled() -> ok.
rebalance_if_enabled() ->
    case fluxer_gateway_sup:role_enabled(presence) of
        true ->
            _ = safe_call(rebalance, ?DEFAULT_GEN_SERVER_TIMEOUT, ok),
            ok;
        false ->
            ok
    end.

-spec cast_local_if_enabled(term()) -> ok.
cast_local_if_enabled(Request) ->
    case fluxer_gateway_sup:role_enabled(presence) of
        true ->
            _ = safe_cast(Request),
            ok;
        false ->
            ok
    end.

-spec safe_call(term(), timeout(), term()) -> term().
safe_call(Request, Timeout, Default) ->
    try gen_server:call(presence_cache, Request, Timeout) of
        Reply -> Reply
    catch
        error:_ -> Default;
        exit:_ -> Default
    end.

-spec safe_cast(term()) -> ok.
safe_cast(Msg) ->
    try gen_server:cast(presence_cache, Msg) of
        _ -> ok
    catch
        error:_ -> ok;
        exit:_ -> ok
    end.
