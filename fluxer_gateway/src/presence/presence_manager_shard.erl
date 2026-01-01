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

-module(presence_manager_shard).
-behaviour(gen_server).

-include_lib("fluxer_gateway/include/timeout_config.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type user_id() :: integer().
-type presence_ref() :: {pid(), reference()}.
-type status() :: online | offline | idle | dnd.
-type event_type() :: atom() | binary().

-type start_or_lookup_request() :: #{
    user_id := user_id(),
    user_data := map(),
    guild_ids := [integer()],
    status := status(),
    friend_ids := [user_id()],
    group_dm_recipients := map()
}.

-type state() :: #{presences := #{user_id() => presence_ref()}}.

-spec start_link(non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(ShardIndex) ->
    gen_server:start_link(?MODULE, #{shard_index => ShardIndex}, []).

-spec init(map()) -> {ok, state()}.
init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #{presences => #{}}}.

-spec handle_call(Request, From, State) -> Result when
    Request ::
        {lookup, user_id()}
        | {start_or_lookup, start_or_lookup_request()}
        | {dispatch, user_id(), event_type(), term()}
        | get_local_count
        | get_global_count
        | term(),
    From :: gen_server:from(),
    State :: state(),
    Result :: {reply, Reply, state()},
    Reply ::
        {ok, pid()}
        | {error, not_found}
        | {error, registration_failed}
        | {error, process_disappeared}
        | {error, term()}
        | {ok, non_neg_integer()}
        | ok.
handle_call({lookup, UserId}, _From, State) ->
    do_lookup(UserId, State);
handle_call({dispatch, UserId, Event, Data}, _From, State) ->
    case lookup_presence(UserId, State) of
        {ok, PresencePid, NewState} ->
            gen_server:cast(PresencePid, {dispatch, Event, Data}),
            {reply, ok, NewState};
        {error, not_found, NewState} ->
            {reply, {error, not_found}, NewState}
    end;
handle_call({start_or_lookup, Request}, _From, State) ->
    do_start_or_lookup(Request, State);
handle_call({terminate_all_sessions, UserId}, _From, State) ->
    case terminate_sessions_for_user(UserId, State) of
        {Result, NewState} ->
            {reply, Result, NewState}
    end;
handle_call(get_local_count, _From, State) ->
    Presences = maps:get(presences, State),
    Count = process_registry:get_count(Presences),
    {reply, {ok, Count}, State};
handle_call(get_global_count, _From, State) ->
    Presences = maps:get(presences, State),
    Count = process_registry:get_count(Presences),
    {reply, {ok, Count}, State};
handle_call(_Unknown, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Unknown, State) ->
    {noreply, State}.

-spec handle_info(Info, State) -> {noreply, state()} when
    Info :: {'DOWN', reference(), process, pid(), term()} | term(),
    State :: state().
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    Presences = maps:get(presences, State),
    NewPresences = process_registry:cleanup_on_down(Pid, Presences),
    {noreply, State#{presences := NewPresences}};
handle_info(_Unknown, State) ->
    {noreply, State}.

-spec terminate(Reason, State) -> ok when
    Reason :: term(),
    State :: state().
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), term(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) when is_map(State) ->
    {ok, State};
code_change(_OldVsn, {state, Presences}, _Extra) ->
    {ok, #{presences => Presences}}.

-spec do_lookup(user_id(), state()) -> {reply, {ok, pid()} | {error, not_found}, state()}.
do_lookup(UserId, State) ->
    case lookup_presence(UserId, State) of
        {ok, Pid, NewState} ->
            {reply, {ok, Pid}, NewState};
        {error, not_found, NewState} ->
            {reply, {error, not_found}, NewState}
    end.

-spec do_start_or_lookup(start_or_lookup_request(), state()) ->
    {reply, {ok, pid()} | {error, registration_failed | process_disappeared | term()}, state()}.
do_start_or_lookup(Request, State) ->
    Presences = maps:get(presences, State),
    #{
        user_id := UserId,
        user_data := UserData,
        guild_ids := GuildIds,
        status := Status
    } = Request,
    case maps:get(UserId, Presences, undefined) of
        {Pid, _Ref} ->
            {reply, {ok, Pid}, State};
        undefined ->
            PresenceName = process_registry:build_process_name(presence, UserId),
            case whereis(PresenceName) of
                undefined ->
                    FriendIds = maps:get(friend_ids, Request, []),
                    GroupDmRecipients = maps:get(group_dm_recipients, Request, #{}),
                    PresenceData = #{
                        user_id => UserId,
                        user_data => UserData,
                        guild_ids => GuildIds,
                        status => Status,
                        friend_ids => FriendIds,
                        group_dm_recipients => GroupDmRecipients,
                        custom_status => maps:get(custom_status, Request, null)
                    },
                    case presence:start_link(PresenceData) of
                        {ok, Pid} ->
                            case
                                process_registry:register_and_monitor(PresenceName, Pid, Presences)
                            of
                                {ok, RegisteredPid, Ref, NewPresences0} ->
                                    CleanPresences = maps:remove(PresenceName, NewPresences0),
                                    NewPresences = maps:put(
                                        UserId, {RegisteredPid, Ref}, CleanPresences
                                    ),
                                    {reply, {ok, RegisteredPid}, State#{
                                        presences := NewPresences
                                    }};
                                {error, registration_race_condition} ->
                                    {reply, {error, registration_failed}, State};
                                {error, _Reason} = Error ->
                                    {reply, Error, State}
                            end;
                        Error ->
                            {reply, Error, State}
                    end;
                _ExistingPid ->
                    case process_registry:lookup_or_monitor(PresenceName, UserId, Presences) of
                        {ok, Pid, _Ref, NewPresences} ->
                            {reply, {ok, Pid}, State#{presences := NewPresences}};
                        {error, not_found} ->
                            {reply, {error, process_disappeared}, State}
                    end
            end
    end.

-spec lookup_presence(user_id(), state()) -> {ok, pid(), state()} | {error, not_found, state()}.
lookup_presence(UserId, State) ->
    Presences = maps:get(presences, State),
    case maps:get(UserId, Presences, undefined) of
        {Pid, _Ref} ->
            {ok, Pid, State};
        undefined ->
            PresenceName = process_registry:build_process_name(presence, UserId),
            case process_registry:lookup_or_monitor(PresenceName, UserId, Presences) of
                {ok, Pid, Ref, NewPresences0} ->
                    CleanPresences = maps:remove(PresenceName, NewPresences0),
                    FinalPresences = maps:put(UserId, {Pid, Ref}, CleanPresences),
                    {ok, Pid, State#{presences := FinalPresences}};
                {error, not_found} ->
                    {error, not_found, State}
            end
    end.

terminate_sessions_for_user(UserId, State) ->
    Presences = maps:get(presences, State),
    case maps:get(UserId, Presences, undefined) of
        {Pid, _Ref} ->
            gen_server:cast(Pid, {terminate_all_sessions}),
            {ok, State};
        undefined ->
            PresenceName = process_registry:build_process_name(presence, UserId),
            case process_registry:lookup_or_monitor(PresenceName, UserId, Presences) of
                {ok, Pid, Ref, NewPresences0} ->
                    CleanPresences = maps:remove(PresenceName, NewPresences0),
                    FinalPresences = maps:put(UserId, {Pid, Ref}, CleanPresences),
                    gen_server:cast(Pid, {terminate_all_sessions}),
                    {ok, State#{presences := FinalPresences}};
                {error, not_found} ->
                    {ok, State}
            end
    end.
