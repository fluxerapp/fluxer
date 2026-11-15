%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_manager_handoff).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([do/1]).

-export_type([state/0]).

-type user_id() :: integer().
-type shard() :: #{pid := pid(), ref := reference()}.
-type state() :: #{
    shards := #{non_neg_integer() => shard()}, shard_count := pos_integer(), _ => _
}.

-spec do(state()) -> ok.
do(State) ->
    LocalNode = node(),
    lists:foreach(
        fun(Presence) -> handoff_presence(Presence, LocalNode) end,
        collect_all_presences(State)
    ),
    ok.

-spec handoff_presence({user_id(), pid()}, node()) -> ok.
handoff_presence({UserId, Pid}, LocalNode) ->
    case presence_manager_routing:resolve_owner_node(UserId) of
        {ok, OwnerNode} when OwnerNode =:= LocalNode ->
            ok;
        {ok, OwnerNode} ->
            handoff_to_owner(UserId, Pid, OwnerNode);
        unavailable ->
            ok
    end.

-spec handoff_to_owner(user_id(), pid(), node()) -> ok.
handoff_to_owner(UserId, Pid, OwnerNode) ->
    case confirm_target_ready(UserId, OwnerNode) of
        true ->
            safe_stop_presence(Pid),
            presence_manager_cache:clean(UserId, Pid);
        false ->
            ok
    end.

-spec confirm_target_ready(user_id(), node()) -> boolean().
confirm_target_ready(UserId, OwnerNode) ->
    case presence_manager_routing:resolve_owner_node(UserId) of
        {ok, OwnerNode} -> node_reachable(OwnerNode);
        _ -> false
    end.

-spec node_reachable(node()) -> boolean().
node_reachable(OwnerNode) ->
    lists:member(OwnerNode, gateway_node_router:active_nodes(presence)).

-spec collect_all_presences(state()) -> [{user_id(), pid()}].
collect_all_presences(State) ->
    Shards = maps:get(shards, State),
    lists:flatmap(
        fun(#{pid := Pid}) -> collect_shard_presences(Pid) end,
        maps:values(Shards)
    ).

-spec collect_shard_presences(pid()) -> [{user_id(), pid()}].
collect_shard_presences(Pid) ->
    case
        shard_utils:safe_gen_call_wrapped(Pid, get_all_presences, ?DEFAULT_GEN_SERVER_TIMEOUT)
    of
        {ok, {ok, Presences}} when is_map(Presences) ->
            extract_presence_pairs(Presences);
        _ ->
            []
    end.

-spec extract_presence_pairs(map()) -> [{user_id(), pid()}].
extract_presence_pairs(Presences) ->
    maps:fold(
        fun
            (UserId, {PresencePid, _Ref}, Acc) when
                is_integer(UserId), is_pid(PresencePid)
            ->
                [{UserId, PresencePid} | Acc];
            (_UserId, _PresenceRef, Acc) ->
                Acc
        end,
        [],
        Presences
    ).

-spec safe_stop_presence(pid()) -> ok.
safe_stop_presence(Pid) ->
    try gen_server:stop(Pid, shutdown, 5000) of
        _ -> ok
    catch
        error:_ -> ok;
        exit:_ -> ok
    end.
