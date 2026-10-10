%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(process_liveness).
-typing([eqwalizer]).

-export([is_alive/1, are_alive/1]).

-define(REMOTE_LIVENESS_TIMEOUT_MS, 1000).
-define(BATCH_LIVENESS_TIMEOUT_MS, 1500).

-spec is_alive(term()) -> boolean().
is_alive(Pid) when is_pid(Pid) ->
    case node(Pid) =:= node() of
        true ->
            erlang:is_process_alive(Pid);
        false ->
            remote_is_alive(Pid)
    end;
is_alive(_) ->
    false.

-spec are_alive([term()]) -> #{term() => boolean()}.
are_alive(Pids) ->
    {Local, Remote} = lists:partition(fun is_local_pid/1, Pids),
    LocalResults = [{P, is_alive(P)} || P <- Local],
    RemoteResults = remote_are_alive(Remote),
    maps:from_list(LocalResults ++ RemoteResults).

-spec is_local_pid(term()) -> boolean().
is_local_pid(Pid) when is_pid(Pid) ->
    node(Pid) =:= node();
is_local_pid(_) ->
    false.

-spec remote_are_alive([term()]) -> [{term(), boolean()}].
remote_are_alive([]) ->
    [];
remote_are_alive(Pids) ->
    Parent = self(),
    Tags = [{Pid, make_ref()} || Pid <- Pids],
    lists:foreach(
        fun(Probe) -> spawn_liveness_probe(Parent, Probe) end,
        Tags
    ),
    [{Pid, gather_remote(Tag)} || {Pid, Tag} <- Tags].

-spec spawn_liveness_probe(pid(), {term(), reference()}) -> pid().
spawn_liveness_probe(Parent, {Pid, Tag}) ->
    spawn(fun() -> Parent ! {Tag, is_alive(Pid)} end).

-spec gather_remote(reference()) -> boolean().
gather_remote(Tag) ->
    receive
        {Tag, Result} -> Result
    after ?BATCH_LIVENESS_TIMEOUT_MS ->
        false
    end.

-spec remote_is_alive(pid()) -> boolean().
remote_is_alive(Pid) ->
    PidNode = node(Pid),
    case lists:member(PidNode, nodes()) of
        true -> remote_is_alive_call(PidNode, Pid);
        false -> false
    end.

-spec remote_is_alive_call(node(), pid()) -> boolean().
remote_is_alive_call(PidNode, Pid) ->
    try rpc:call(PidNode, erlang, is_process_alive, [Pid], ?REMOTE_LIVENESS_TIMEOUT_MS) of
        true -> true;
        _ -> false
    catch
        throw:true -> true;
        throw:_ -> false;
        error:_ -> false;
        exit:_ -> false
    end.
