%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_handoff_freeze_remote).
-typing([eqwalizer]).

-export([
    reload_frozen/3,
    freeze/2,
    thaw/2,
    release_guard/1,
    export/2,
    barrier/2,
    capture/3,
    source_alive/2,
    safe_call/3,
    rpc_call/5
]).

-define(GUARD_EXPRS,
    "R = erlang:monitor(process, C),"
    " C ! {freeze_guard_ready, self()},"
    " receive"
    " {freeze_guard_release, C} -> erlang:demonitor(R, [flush]), released;"
    " {'DOWN', R, process, C, _} ->"
    " try sys:resume(P, 60000) of ok -> resumed catch _:Reason -> {resume_failed, Reason} end"
    " end."
).
-define(CAPTURE_EXPRS,
    "F = fun({'$gen_call', _, _}) -> true;"
    " ({'$gen_cast', _}) -> true;"
    " ({presence, U, _}) when is_integer(U) -> true;"
    " ({reconcile_user_presence, _}) -> true;"
    " ({clear_stale_cached_voice_states, _}) -> true;"
    " (_) -> false end,"
    " case erlang:process_info(P, messages) of"
    " {messages, Ms} ->"
    " R = [M || M <- Ms, F(M)], L = length(R),"
    " {L, case L >= S of true -> lists:nthtail(S, R); false -> [] end};"
    " undefined -> undefined"
    " end."
).
-define(REPAIR_EXPRS,
    "D = erlang:monotonic_time(millisecond) + T,"
    " case M:fetch_guild_data(G) of"
    " {ok, Data} ->"
    " case erlang:monotonic_time(millisecond) < D of"
    " true ->"
    " R = fun(S) -> case A:handle_call({reload, Data}, {self(), make_ref()}, S) of"
    " {reply, _, S1} -> S1;"
    " {reply, _, S1, _} -> S1"
    " end end,"
    " _ = sys:replace_state(P, R, max(1, D - erlang:monotonic_time(millisecond))),"
    " reloaded;"
    " false -> late"
    " end;"
    " Other -> {fetch_failed, Other}"
    " end."
).
-define(EXPORT_EXPRS,
    "maps:merge(maps:update_with(data, fun(D) -> maps:without(K, D) end,"
    " guild_handoff:export_handoff_state(sys:get_state(P, T))), X)."
).

-type freeze() :: #{pid := pid(), guard := pid(), guard_ref := reference()}.

-export_type([freeze/0]).

-spec reload_frozen(guild_handoff_freeze:guild_id(), pid(), pos_integer()) ->
    ok | {error, term()}.
reload_frozen(GuildId, SrcPid, Timeout) ->
    Bindings = bindings([
        {'G', GuildId},
        {'P', SrcPid},
        {'T', Timeout},
        {'M', guild_manager_shard_fetch},
        {'A', guild}
    ]),
    Args = [parse(?REPAIR_EXPRS), Bindings],
    case rpc_call(node(SrcPid), erl_eval, exprs, Args, Timeout + 5000) of
        {ok, {value, reloaded, _}} -> ok;
        {ok, {value, Other, _}} -> {error, Other};
        {error, _} = Error -> Error
    end.

-spec freeze(pid(), guild_handoff_freeze:opts()) -> {ok, freeze()} | {error, term()}.
freeze(SrcPid, Opts) ->
    case spawn_guard(SrcPid, maps:get(guard_timeout, Opts)) of
        {ok, Guard, GuardRef} ->
            Freeze = #{pid => SrcPid, guard => Guard, guard_ref => GuardRef},
            suspend_guarded(Freeze, Opts);
        {error, Reason} ->
            {error, {guard_failed, Reason}}
    end.

-spec suspend_guarded(freeze(), guild_handoff_freeze:opts()) ->
    {ok, freeze()} | {error, term()}.
suspend_guarded(#{pid := SrcPid} = Freeze, Opts) ->
    case suspend(SrcPid, maps:get(suspend_timeout, Opts)) of
        ok ->
            {ok, Freeze};
        {error, Reason} ->
            _ = thaw(Freeze, Opts),
            {error, {suspend_failed, Reason}}
    end.

-spec thaw(freeze(), guild_handoff_freeze:opts()) -> ok | {error, term()}.
thaw(#{pid := SrcPid} = Freeze, Opts) ->
    Result =
        try sys:resume(SrcPid, maps:get(suspend_timeout, Opts)) of
            ok -> ok
        catch
            exit:Reason -> {error, Reason}
        end,
    release_guard(Freeze),
    Result.

-spec suspend(pid(), pos_integer()) -> ok | {error, term()}.
suspend(SrcPid, Timeout) ->
    try sys:suspend(SrcPid, Timeout) of
        ok -> ok
    catch
        exit:Reason -> {error, Reason}
    end.

-spec spawn_guard(pid(), pos_integer()) -> {ok, pid(), reference()} | {error, term()}.
spawn_guard(SrcPid, Timeout) ->
    Bindings = bindings([{'C', self()}, {'P', SrcPid}]),
    Guard = erlang:spawn(node(SrcPid), erl_eval, exprs, [parse(?GUARD_EXPRS), Bindings]),
    Ref = erlang:monitor(process, Guard),
    receive
        {freeze_guard_ready, Guard} -> {ok, Guard, Ref};
        {'DOWN', Ref, process, Guard, Reason} -> {error, Reason}
    after Timeout ->
        erlang:demonitor(Ref, [flush]),
        exit(Guard, kill),
        {error, guard_timeout}
    end.

-spec release_guard(freeze()) -> ok.
release_guard(#{guard := Guard, guard_ref := Ref}) ->
    erlang:demonitor(Ref, [flush]),
    Guard ! {freeze_guard_release, self()},
    ok.

-spec export(pid(), guild_handoff_freeze:opts()) -> {ok, map()} | {error, term()}.
export(SrcPid, Opts) ->
    Timeout = maps:get(export_timeout, Opts),
    Bindings = bindings([
        {'P', SrcPid},
        {'T', Timeout},
        {'K', guild_handoff:derived_data_keys()},
        {'X', export_overrides(Opts)}
    ]),
    Args = [parse(?EXPORT_EXPRS), Bindings],
    case rpc_call(node(SrcPid), erl_eval, exprs, Args, Timeout + 5000) of
        {ok, {value, Export, _}} -> validate_export(Export);
        {ok, Other} -> {error, {unexpected_export, Other}};
        {error, _} = Error -> Error
    end.

-spec export_overrides(guild_handoff_freeze:opts()) -> map().
export_overrides(#{transfer_sessions := false}) ->
    #{sessions => #{}};
export_overrides(_Opts) ->
    #{}.

-spec validate_export(term()) -> {ok, map()} | {error, term()}.
validate_export(Export) when is_map(Export) ->
    case guild_handoff:validate_handoff_state(Export) of
        ok -> {ok, Export};
        {error, Missing} -> {error, {invalid_export, Missing}}
    end;
validate_export(Other) ->
    {error, {invalid_export, Other}}.

-spec barrier(pid(), pos_integer()) -> ok | {error, term()}.
barrier(NewPid, Timeout) ->
    case safe_call(NewPid, {get_guild_id}, Timeout) of
        Id when is_integer(Id) -> ok;
        Other -> {error, Other}
    end.

-spec capture(pid(), non_neg_integer(), guild_handoff_freeze:opts()) ->
    {ok, non_neg_integer(), [term()]} | {error, term()}.
capture(SrcPid, Seen, Opts) ->
    Args = [parse(?CAPTURE_EXPRS), bindings([{'P', SrcPid}, {'S', Seen}])],
    case rpc_call(node(SrcPid), erl_eval, exprs, Args, maps:get(rpc_timeout, Opts)) of
        {ok, {value, {Len, New}, _}} when is_integer(Len), is_list(New) -> {ok, Len, New};
        {ok, {value, undefined, _}} -> {error, noproc};
        {ok, Other} -> {error, {unexpected, Other}};
        {error, _} = Error -> Error
    end.

-spec source_alive(pid(), guild_handoff_freeze:opts()) -> boolean() | {error, term()}.
source_alive(SrcPid, Opts) ->
    Args = [SrcPid],
    case rpc_call(node(SrcPid), erlang, is_process_alive, Args, maps:get(rpc_timeout, Opts)) of
        {ok, Alive} when is_boolean(Alive) -> Alive;
        {ok, Other} -> {error, {unexpected, Other}};
        {error, _} = Error -> Error
    end.

-spec safe_call(pid(), term(), pos_integer()) -> term().
safe_call(Server, Request, Timeout) ->
    try
        gen_server:call(Server, Request, Timeout)
    catch
        exit:{timeout, _} -> {error, timeout};
        exit:Reason -> {error, {exit, Reason}}
    end.

-spec rpc_call(node(), module(), atom(), [term()], pos_integer()) ->
    {ok, term()} | {error, term()}.
rpc_call(Node, Module, Function, Args, Timeout) ->
    try erpc:call(Node, Module, Function, Args, Timeout) of
        Result -> {ok, Result}
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

-spec parse(string()) -> [erl_parse:abstract_expr()].
parse(Source) ->
    {ok, Tokens, _} = erl_scan:string(Source),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    Exprs.

-spec bindings([{atom(), term()}]) -> erl_eval:binding_struct().
bindings(Pairs) ->
    lists:foldl(
        fun({Name, Value}, Acc) -> erl_eval:add_binding(Name, Value, Acc) end,
        erl_eval:new_bindings(),
        Pairs
    ).
