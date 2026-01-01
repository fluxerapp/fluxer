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

-module(call_manager).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type channel_id() :: integer().
-type call_ref() :: {pid(), reference()}.
-type call_data() :: map().
-type state() :: #{calls := #{channel_id() => call_ref()}}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    {ok, #{calls => #{}}}.

-spec handle_call(Request, From, State) -> Result when
    Request ::
        {create, channel_id(), call_data()}
        | {lookup, channel_id()}
        | {get_or_create, channel_id(), call_data()}
        | {terminate_call, channel_id()}
        | get_local_count
        | get_global_count
        | term(),
    From :: gen_server:from(),
    State :: state(),
    Result :: {reply, Reply, state()},
    Reply ::
        {ok, pid()}
        | {error, already_exists}
        | {error, not_found}
        | {error, term()}
        | ok
        | {ok, non_neg_integer()}.
handle_call({create, ChannelId, CallData}, _From, State) ->
    do_create_call(ChannelId, CallData, State);
handle_call({lookup, ChannelId}, _From, State) ->
    do_lookup_call(ChannelId, State);
handle_call({get_or_create, ChannelId, CallData}, _From, State) ->
    do_get_or_create_call(ChannelId, CallData, State);
handle_call({terminate_call, ChannelId}, _From, State) ->
    do_terminate_call(ChannelId, State);
handle_call(get_local_count, _From, #{calls := Calls} = State) ->
    {reply, {ok, process_registry:get_count(Calls)}, State};
handle_call(get_global_count, _From, #{calls := Calls} = State) ->
    {reply, {ok, process_registry:get_count(Calls)}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(Info, State) -> {noreply, state()} when
    Info :: {'DOWN', reference(), process, pid(), term()} | term(),
    State :: state().
handle_info({'DOWN', _Ref, process, Pid, _Reason}, #{calls := Calls} = State) ->
    NewCalls = process_registry:cleanup_on_down(Pid, Calls),
    {noreply, State#{calls := NewCalls}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason, State) -> ok when
    Reason :: term(),
    State :: state().
terminate(_Reason, #{calls := _Calls}) ->
    ok.

-spec code_change(OldVsn, State, Extra) -> {ok, state()} when
    OldVsn :: term(),
    State :: state() | {state, map()},
    Extra :: term().
code_change(_OldVsn, {state, Calls}, _Extra) ->
    {ok, #{calls => Calls}};
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec do_create_call(channel_id(), call_data(), state()) ->
    {reply, {ok, pid()} | {error, already_exists | term()}, state()}.
do_create_call(ChannelId, CallData, #{calls := Calls} = State) ->
    case maps:get(ChannelId, Calls, undefined) of
        {Pid, _Ref} when is_pid(Pid) ->
            {reply, {error, already_exists}, State};
        undefined ->
            CallName = process_registry:build_process_name(call, ChannelId),
            case whereis(CallName) of
                undefined ->
                    case call:start_link(CallData) of
                        {ok, Pid} ->
                            case process_registry:register_and_monitor(CallName, Pid, Calls) of
                                {ok, RegisteredPid, Ref, NewCalls0} ->
                                    CleanCalls = maps:remove(CallName, NewCalls0),
                                    NewCalls = maps:put(
                                        ChannelId, {RegisteredPid, Ref}, CleanCalls
                                    ),
                                    NewState = State#{calls := NewCalls},
                                    {reply, {ok, RegisteredPid}, NewState};
                                {error, Reason} ->
                                    {reply, {error, Reason}, State}
                            end;
                        {error, Reason} ->
                            {reply, {error, Reason}, State}
                    end;
                _ExistingPid ->
                    {reply, {error, already_exists}, State}
            end
    end.

-spec do_lookup_call(channel_id(), state()) -> {reply, {ok, pid()} | {error, not_found}, state()}.
do_lookup_call(ChannelId, #{calls := Calls} = State) ->
    case maps:get(ChannelId, Calls, undefined) of
        {Pid, _Ref} when is_pid(Pid) ->
            {reply, {ok, Pid}, State};
        undefined ->
            CallName = process_registry:build_process_name(call, ChannelId),
            case process_registry:lookup_or_monitor(CallName, ChannelId, Calls) of
                {ok, Pid, _Ref, NewCalls} ->
                    {reply, {ok, Pid}, State#{calls := NewCalls}};
                {error, not_found} ->
                    {reply, {error, not_found}, State}
            end
    end.

-spec do_get_or_create_call(channel_id(), call_data(), state()) ->
    {reply, {ok, pid()} | {error, term()}, state()}.
do_get_or_create_call(ChannelId, CallData, #{calls := Calls} = State) ->
    case maps:get(ChannelId, Calls, undefined) of
        {Pid, _Ref} when is_pid(Pid) ->
            {reply, {ok, Pid}, State};
        undefined ->
            do_create_call(ChannelId, CallData, State)
    end.

-spec do_terminate_call(channel_id(), state()) -> {reply, ok | {error, not_found}, state()}.
do_terminate_call(ChannelId, #{calls := Calls} = State) ->
    case maps:get(ChannelId, Calls, undefined) of
        {Pid, Ref} ->
            demonitor(Ref, [flush]),
            gen_server:stop(Pid, normal, ?SHUTDOWN_TIMEOUT),
            CallName = process_registry:build_process_name(call, ChannelId),
            process_registry:safe_unregister(CallName),
            NewCalls = maps:remove(ChannelId, Calls),
            {reply, ok, State#{calls := NewCalls}};
        undefined ->
            {reply, {error, not_found}, State}
    end.
