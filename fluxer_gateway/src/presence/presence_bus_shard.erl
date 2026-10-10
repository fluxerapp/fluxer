%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_bus_shard).
-typing([eqwalizer]).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-export([test_receiver/3]).
-endif.

-type state() :: #{scope := atom(), pg_pid := pid(), shard_index := non_neg_integer()}.

-spec start_link(non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(ShardIndex) ->
    normalize_start_link(gen_server:start_link(?MODULE, #{shard_index => ShardIndex}, [])).

-spec init(map()) -> {ok, state(), hibernate} | {stop, term()}.
init(#{shard_index := ShardIndex}) ->
    process_flag(trap_exit, true),
    erlang:process_flag(fullsweep_after, 10),
    Scope = scope_name(ShardIndex),
    case gateway_pg_scope:ensure_presence_scope() of
        {ok, PgPid} ->
            {ok, #{scope => Scope, pg_pid => PgPid, shard_index => ShardIndex}, hibernate};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({subscribe, UserId, Pid}, _From, State) when is_integer(UserId), is_pid(Pid) ->
    Scope = maps:get(scope, State),
    {reply, do_subscribe(Scope, UserId, Pid), State};
handle_call({unsubscribe, UserId, Pid}, _From, State) when is_integer(UserId), is_pid(Pid) ->
    Scope = maps:get(scope, State),
    {reply, do_unsubscribe(Scope, UserId, Pid), State};
handle_call({publish, UserId, Payload}, _From, State) when is_integer(UserId) ->
    Scope = maps:get(scope, State),
    {reply, do_publish(Scope, UserId, Payload), State};
handle_call(diagnostic_info, _From, State) ->
    Scope = maps:get(scope, State),
    ShardIndex = maps:get(shard_index, State),
    TotalMembers = count_all_members(Scope),
    LocalMembers = count_all_local_members(Scope),
    Key = list_to_atom("shard_" ++ integer_to_list(ShardIndex) ++ "_members"),
    LocalKey = list_to_atom("shard_" ++ integer_to_list(ShardIndex) ++ "_local_members"),
    {reply, #{Key => TotalMembers, LocalKey => LocalMembers}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'EXIT', PgPid, _Reason}, State) ->
    StoredPgPid = maps:get(pg_pid, State),
    case PgPid =:= StoredPgPid of
        true -> {noreply, try_recover_pg_scope(State)};
        false -> {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec try_recover_pg_scope(state()) -> state().
try_recover_pg_scope(State) ->
    case gateway_pg_scope:ensure_presence_scope() of
        {ok, NewPgPid} -> State#{pg_pid := NewPgPid};
        {error, _} -> State
    end.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    erlang:garbage_collect(),
    {ok, State}.

-spec do_subscribe(atom(), integer(), pid()) -> ok.
do_subscribe(Scope, UserId, Pid) ->
    Group = {presence, UserId},
    safe_pg_join(Scope, Group, Pid).

-spec do_unsubscribe(atom(), integer(), pid()) -> ok.
do_unsubscribe(Scope, UserId, Pid) ->
    Group = {presence, UserId},
    safe_pg_leave(Scope, Group, Pid).

-spec do_publish(atom(), integer(), term()) -> ok.
do_publish(Scope, UserId, Payload) ->
    Group = {presence, UserId},
    Members = safe_pg_members(Scope, Group),
    publish_to_members(Members, UserId, Payload).

-spec publish_to_members([pid()], integer(), term()) -> ok.
publish_to_members([], _UserId, _Payload) ->
    ok;
publish_to_members(Members, UserId, Payload) ->
    lists:foreach(
        fun(TargetPid) -> safe_send_presence(TargetPid, UserId, Payload) end,
        Members
    ),
    ok.

-spec scope_name(non_neg_integer()) -> atom().
scope_name(_Index) ->
    gateway_pg_scope:presence_scope().

-spec count_all_members(atom()) -> non_neg_integer().
count_all_members(Scope) ->
    trunc(
        lists:foldl(
            fun(Group, Acc) ->
                Acc + length(safe_pg_members(Scope, Group))
            end,
            0,
            safe_pg_groups(Scope)
        )
    ).

-spec count_all_local_members(atom()) -> non_neg_integer().
count_all_local_members(Scope) ->
    trunc(
        lists:foldl(
            fun(Group, Acc) ->
                Acc + length(safe_pg_local_members(Scope, Group))
            end,
            0,
            safe_pg_groups(Scope)
        )
    ).

-spec safe_pg_join(atom(), term(), pid()) -> ok.
safe_pg_join(Scope, Group, Pid) ->
    try pg:join(Scope, Group, Pid) of
        _ -> ok
    catch
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec safe_pg_leave(atom(), term(), pid()) -> ok.
safe_pg_leave(Scope, Group, Pid) ->
    try pg:leave(Scope, Group, Pid) of
        _ -> ok
    catch
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec safe_pg_members(atom(), term()) -> [pid()].
safe_pg_members(Scope, Group) ->
    try pg:get_members(Scope, Group) of
        Members -> Members
    catch
        error:_Reason -> [];
        exit:_Reason -> []
    end.

-spec safe_pg_local_members(atom(), term()) -> [pid()].
safe_pg_local_members(Scope, Group) ->
    try pg:get_local_members(Scope, Group) of
        Members -> Members
    catch
        error:_Reason -> [];
        exit:_Reason -> []
    end.

-spec safe_pg_groups(atom()) -> [term()].
safe_pg_groups(Scope) ->
    try pg:which_groups(Scope) of
        Groups -> Groups
    catch
        error:_Reason -> [];
        exit:_Reason -> []
    end.

-spec safe_send_presence(pid(), integer(), term()) -> ok.
safe_send_presence(TargetPid, UserId, Payload) ->
    try TargetPid ! {presence, UserId, Payload} of
        _ -> ok
    catch
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec normalize_start_link(gen_server:start_ret()) -> {ok, pid()} | {error, term()}.
normalize_start_link({ok, Pid}) ->
    {ok, Pid};
normalize_start_link({error, Reason}) ->
    {error, Reason};
normalize_start_link(ignore) ->
    {error, ignore}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

publish_delivers_to_scope_members_test() ->
    Scope = presence_bus_test_scope_3,
    start_pg_scope(Scope),
    UserId = erlang:unique_integer([positive]),
    Parent = self(),
    Payload = #{<<"status">> => <<"online">>},
    Receiver = spawn(?MODULE, test_receiver, [UserId, Payload, Parent]),
    ?assertEqual(ok, do_subscribe(Scope, UserId, Receiver)),
    ?assertEqual(ok, do_publish(Scope, UserId, Payload)),
    receive
        {presence_delivered, Payload} -> ok
    after 1000 ->
        ?assert(false)
    end,
    ?assertEqual(ok, do_unsubscribe(Scope, UserId, Receiver)).

test_receiver(UserId, Payload, Parent) ->
    receive
        {presence, UserId, Payload} ->
            Parent ! {presence_delivered, Payload}
    after 5000 ->
        Parent ! {presence_delivered, timeout}
    end.

start_pg_scope(Scope) ->
    try pg:start(Scope) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        _ -> ok
    catch
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.
-endif.
