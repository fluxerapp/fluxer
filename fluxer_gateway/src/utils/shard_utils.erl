%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(shard_utils).
-typing([eqwalizer]).

-export([
    find_shard_by_ref/2,
    find_shard_by_pid/2,
    safe_gen_call/3,
    safe_gen_call_detailed/3,
    safe_gen_call_remote/3,
    safe_gen_call_wrapped/3,
    safe_apply/2,
    safe_cast/2,
    check_mailbox_pressure/1,
    normalize_start_link/1
]).

-export_type([shard/0]).

-type shard() :: #{pid := pid(), ref := reference()}.

-spec find_shard_by_ref(reference(), #{non_neg_integer() => shard()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_ref(Ref, Shards) ->
    maps:fold(
        fun
            (Index, #{ref := R}, _) when R =:= Ref -> {ok, Index};
            (_, _, Acc) -> Acc
        end,
        not_found,
        Shards
    ).

-spec find_shard_by_pid(pid(), #{non_neg_integer() => shard()}) ->
    {ok, non_neg_integer()} | not_found.
find_shard_by_pid(Pid, Shards) ->
    maps:fold(
        fun
            (Index, #{pid := P}, _) when P =:= Pid -> {ok, Index};
            (_, _, Acc) -> Acc
        end,
        not_found,
        Shards
    ).

-spec safe_gen_call(pid() | atom(), term(), pos_integer()) -> term().
safe_gen_call(Pid, Request, Timeout) ->
    try
        gen_server:call(Pid, Request, Timeout)
    catch
        throw:_ -> {error, unavailable};
        error:_ -> {error, unavailable};
        exit:_ -> {error, unavailable}
    end.

-spec safe_gen_call_detailed(pid(), term(), pos_integer()) -> term().
safe_gen_call_detailed(Pid, Request, Timeout) ->
    try gen_server:call(Pid, Request, Timeout) of
        Reply -> Reply
    catch
        exit:{timeout, _Call} -> {error, timeout};
        exit:{noproc, _Call} -> {error, unavailable};
        exit:Reason -> {error, {exit, Reason}}
    end.

-spec safe_gen_call_remote(pid() | atom(), term(), pos_integer()) -> term().
safe_gen_call_remote(Server, Request, Timeout) ->
    try gen_server:call(Server, Request, Timeout) of
        Reply -> Reply
    catch
        exit:{timeout, _Call} -> {error, timeout};
        exit:{noproc, _Call} -> {error, unavailable};
        exit:{nodedown, _Node} -> {error, unavailable};
        exit:Reason -> {error, {exit, Reason}}
    end.

-spec safe_gen_call_wrapped(gen_server:server_ref(), term(), timeout()) ->
    {ok, term()} | {exit, term()}.
safe_gen_call_wrapped(ServerRef, Request, Timeout) ->
    try gen_server:call(ServerRef, Request, Timeout) of
        Reply -> {ok, Reply}
    catch
        error:Reason -> {exit, {error, Reason}};
        exit:Reason -> {exit, Reason}
    end.

-spec safe_apply(fun(() -> T), T) -> T when T :: term().
safe_apply(Fun, Fallback) ->
    try
        Fun()
    catch
        throw:_ -> Fallback;
        error:_ -> Fallback;
        exit:_ -> Fallback
    end.

-define(MAILBOX_PRESSURE_THRESHOLD, 5000).

-spec safe_cast(pid() | atom() | {atom(), node()}, term()) -> ok | {error, overloaded}.
safe_cast(Dest, Msg) ->
    case check_mailbox_pressure(Dest) of
        ok ->
            gen_server:cast(Dest, Msg),
            ok;
        overloaded ->
            {error, overloaded}
    end.

-spec check_mailbox_pressure(pid() | atom() | {atom(), node()}) -> ok | overloaded.
check_mailbox_pressure(Dest) when is_pid(Dest) ->
    check_pid_mailbox_pressure(Dest);
check_mailbox_pressure(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> ok;
        Pid when is_pid(Pid) -> check_mailbox_pressure(Pid)
    end;
check_mailbox_pressure({Name, Node}) when is_atom(Name), is_atom(Node), Node =:= node() ->
    check_mailbox_pressure(Name);
check_mailbox_pressure(_) ->
    ok.

-spec check_pid_mailbox_pressure(pid()) -> ok | overloaded.
check_pid_mailbox_pressure(Pid) when node(Pid) =/= node() ->
    ok;
check_pid_mailbox_pressure(Pid) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, Len} when Len > ?MAILBOX_PRESSURE_THRESHOLD -> overloaded;
        _ -> ok
    end.

-spec normalize_start_link(gen_server:start_ret()) -> {ok, pid()} | {error, term()}.
normalize_start_link({ok, Pid}) ->
    {ok, Pid};
normalize_start_link({error, Reason}) ->
    {error, Reason};
normalize_start_link(ignore) ->
    {error, ignore}.
