%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_outbox_worker).
-typing([eqwalizer]).

-export([send/3, reply_result/1, down_result/1, kill_expired/2]).

-spec send(binary(), binary(), pos_integer()) -> ok | {error, term()}.
send(Subject, Body, Timeout) ->
    try push_job_publisher:request(Subject, Body, Timeout) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

-spec reply_result(term()) -> ok | {error, term()}.
reply_result(ok) -> ok;
reply_result({error, Reason}) -> {error, Reason};
reply_result(Other) -> {error, {invalid_worker_reply, Other}}.

-spec down_result(term()) -> {error, term()}.
down_result(killed) -> {error, timeout};
down_result(Reason) -> {error, {worker_down, Reason}}.

-spec kill_expired(pid(), #{pid() => term()}) -> ok.
kill_expired(Pid, Inflight) ->
    case maps:is_key(Pid, Inflight) of
        true ->
            exit(Pid, kill),
            ok;
        false ->
            ok
    end.
