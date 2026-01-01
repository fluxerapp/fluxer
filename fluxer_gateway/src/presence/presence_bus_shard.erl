%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(presence_bus_shard).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type state() :: #{scope := atom(), pg_pid := pid(), shard_index := non_neg_integer()}.

-define(SCOPE_PREFIX, presence_bus).

-spec start_link(non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(ShardIndex) ->
    gen_server:start_link(?MODULE, #{shard_index => ShardIndex}, []).

-spec init(map()) -> {ok, state()} | {stop, term()}.
init(#{shard_index := ShardIndex}) ->
    process_flag(trap_exit, true),
    Scope = scope_name(ShardIndex),
    case ensure_pg_scope(Scope) of
        {ok, PgPid} ->
            {ok, #{scope => Scope, pg_pid => PgPid, shard_index => ShardIndex}};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call({subscribe, UserId, Pid}, _From, State) ->
    Scope = maps:get(scope, State),
    {reply, do_subscribe(Scope, UserId, Pid), State};
handle_call({unsubscribe, UserId, Pid}, _From, State) ->
    Scope = maps:get(scope, State),
    {reply, do_unsubscribe(Scope, UserId, Pid), State};
handle_call({publish, UserId, Payload}, _From, State) ->
    Scope = maps:get(scope, State),
    {reply, do_publish(Scope, UserId, Payload), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'EXIT', PgPid, Reason}, State) ->
    StoredPgPid = maps:get(pg_pid, State),
    case PgPid =:= StoredPgPid of
        true ->
            Scope = maps:get(scope, State),
            ShardIndex = maps:get(shard_index, State),
            logger:warning(
                "[presence_bus_shard ~p] pg process exited: ~p; restarting scope",
                [ShardIndex, Reason]
            ),
            case ensure_pg_scope(Scope) of
                {ok, NewPgPid} ->
                    {noreply, State#{pg_pid := NewPgPid}};
                {error, _} ->
                    {noreply, State}
            end;
        false ->
            {noreply, State}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), term(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) when is_map(State) ->
    {ok, State};
code_change(_OldVsn, {state, Scope, PgPid, ShardIndex}, _Extra) ->
    {ok, #{scope => Scope, pg_pid => PgPid, shard_index => ShardIndex}}.

-spec do_subscribe(atom(), integer(), pid()) -> ok.
do_subscribe(Scope, UserId, Pid) ->
    Group = {presence, UserId},
    case catch pg:join(Scope, Group, Pid) of
        ok ->
            ok;
        {'EXIT', Reason} ->
            logger:warning("[presence_bus_shard] failed to join group ~p: ~p", [Group, Reason]),
            ok;
        _ ->
            ok
    end.

-spec do_unsubscribe(atom(), integer(), pid()) -> ok.
do_unsubscribe(Scope, UserId, Pid) ->
    Group = {presence, UserId},
    case catch pg:leave(Scope, Group, Pid) of
        ok ->
            ok;
        {'EXIT', Reason} ->
            logger:warning("[presence_bus_shard] failed to leave group ~p: ~p", [Group, Reason]),
            ok;
        _ ->
            ok
    end.

-spec do_publish(atom(), integer(), term()) -> ok.
do_publish(Scope, UserId, Payload) ->
    Group = {presence, UserId},
    Members =
        case catch pg:get_members(Scope, Group) of
            {'EXIT', _} -> [];
            List when is_list(List) -> List;
            _ -> []
        end,
    case Members of
        [] ->
            ok;
        _ ->
            lists:foreach(
                fun(TargetPid) ->
                    catch TargetPid ! {presence, UserId, Payload}
                end,
                Members
            ),
            ok
    end.

-spec ensure_pg_scope(atom()) -> {ok, pid()} | {error, term()}.
ensure_pg_scope(Scope) ->
    case catch pg:start_link(Scope) of
        {ok, PgPid} ->
            {ok, PgPid};
        {error, {already_started, PgPid}} ->
            link(PgPid),
            {ok, PgPid};
        {'EXIT', Reason} ->
            {error, Reason};
        Error ->
            Error
    end.

-spec scope_name(non_neg_integer()) -> atom().
scope_name(Index) ->
    list_to_atom(atom_to_list(?SCOPE_PREFIX) ++ "_" ++ integer_to_list(Index)).
