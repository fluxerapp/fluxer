%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_pg_scope).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/0, presence_scope/0, ensure_presence_scope/0]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(PRESENCE_SCOPE, fluxer_presence).

-type state() :: #{
    scope := atom(),
    pg_pid := pid(),
    owns_pg := boolean()
}.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec presence_scope() -> atom().
presence_scope() ->
    ?PRESENCE_SCOPE.

-spec ensure_presence_scope() -> {ok, pid()} | {error, term()}.
ensure_presence_scope() ->
    case ensure_scope(?PRESENCE_SCOPE) of
        {ok, PgPid, _OwnsPg} -> {ok, PgPid};
        {error, Reason} -> {error, Reason}
    end.

-spec init([]) -> {ok, state()} | {stop, term()}.
init([]) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 50),
    case ensure_scope(?PRESENCE_SCOPE) of
        {ok, PgPid, OwnsPg} ->
            {ok, #{scope => ?PRESENCE_SCOPE, pg_pid => PgPid, owns_pg => OwnsPg}};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call(status, _From, State) ->
    {reply, State, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'EXIT', PgPid, Reason}, #{pg_pid := PgPid, scope := Scope} = State) ->
    logger:warning(
        "Gateway pg scope process exited; group memberships lost, recreating scope",
        #{scope => Scope, reason => Reason}
    ),
    case ensure_scope(Scope) of
        {ok, NewPgPid, OwnsPg} ->
            {noreply, State#{pg_pid := NewPgPid, owns_pg := OwnsPg}};
        {error, _Error} ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #{pg_pid := PgPid, owns_pg := true}) ->
    safe_shutdown_pg(PgPid),
    ok;
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec ensure_scope(atom()) -> {ok, pid(), boolean()} | {error, term()}.
ensure_scope(Scope) ->
    case safe_start_pg(Scope) of
        {ok, PgPid} ->
            {ok, PgPid, true};
        {error, {already_started, PgPid}} when is_pid(PgPid) ->
            ensure_existing_scope(PgPid);
        {'EXIT', Reason} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason}
    end.

-spec ensure_existing_scope(pid()) -> {ok, pid(), false} | {error, term()}.
ensure_existing_scope(PgPid) ->
    case link_pg(PgPid) of
        ok -> {ok, PgPid, false};
        {error, Reason} -> {error, Reason}
    end.

-spec link_pg(pid()) -> ok | {error, term()}.
link_pg(PgPid) ->
    case safe_link(PgPid) of
        true -> ok;
        {'EXIT', Reason} -> {error, Reason}
    end.

-spec safe_shutdown_pg(pid()) -> ok.
safe_shutdown_pg(PgPid) ->
    try exit(PgPid, shutdown) of
        _ -> ok
    catch
        throw:_Reason -> ok;
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec safe_start_pg(atom()) -> {ok, pid()} | {error, term()} | {'EXIT', term()}.
safe_start_pg(Scope) ->
    normalize_start_pg_result(safe_pg_call(fun() -> pg:start_link(Scope) end)).

-spec safe_link(pid()) -> true | {'EXIT', term()}.
safe_link(PgPid) ->
    normalize_link_result(safe_pg_call(fun() -> link(PgPid) end)).

-spec normalize_start_pg_result(term()) -> {ok, pid()} | {error, term()} | {'EXIT', term()}.
normalize_start_pg_result({ok, PgPid}) when is_pid(PgPid) ->
    {ok, PgPid};
normalize_start_pg_result({error, Reason}) ->
    {error, Reason};
normalize_start_pg_result({'EXIT', Reason}) ->
    {'EXIT', Reason};
normalize_start_pg_result(Other) ->
    {error, Other}.

-spec normalize_link_result(term()) -> true | {'EXIT', term()}.
normalize_link_result(true) ->
    true;
normalize_link_result({'EXIT', Reason}) ->
    {'EXIT', Reason};
normalize_link_result(Other) ->
    {'EXIT', Other}.

-spec safe_pg_call(fun(() -> term())) -> term() | {'EXIT', term()}.
safe_pg_call(Fun) ->
    try Fun() of
        Result -> Result
    catch
        throw:Reason -> Reason;
        error:Reason:Stack -> {'EXIT', {Reason, Stack}};
        exit:Reason -> {'EXIT', Reason}
    end.
