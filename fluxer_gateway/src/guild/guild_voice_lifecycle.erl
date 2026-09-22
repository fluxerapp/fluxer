%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_lifecycle).
-typing([eqwalizer]).

-export([
    ensure_voice_server/1,
    handle_voice_server_exit/3,
    reply_voice_server_pid/1,
    clear_stale_cached_voice_states/2,
    authoritative_voice_states/1,
    cast_disconnect_voice_user/2
]).

-type guild_state() :: map().

-export_type([guild_state/0]).

-define(VOICE_CACHE_RECOVERY_GRACE_MS, 120000).

-spec handle_voice_server_exit(pid(), term(), guild_state()) -> guild_state().
handle_voice_server_exit(VoiceServerPid, Reason, State) ->
    GuildId = maps:get(id, State, undefined),
    VoiceStates = maps:get(voice_states, State, #{}),
    logger:warning(
        "guild_voice_server_exit:"
        " guild_id=~p voice_server_pid=~p reason=~p"
        " cached_voice_state_count=~p",
        [GuildId, VoiceServerPid, Reason, maps:size(VoiceStates)]
    ),
    case ensure_voice_server(maps:remove(voice_server_pid, State)) of
        {ok, _NewPid, NewState} -> NewState;
        {{error, _}, NewState} -> NewState
    end.

-spec ensure_voice_server(guild_state()) ->
    {ok, pid(), guild_state()} | {{error, atom()}, guild_state()}.
ensure_voice_server(State) ->
    case maps:get(voice_server_pid, State, undefined) of
        Pid when is_pid(Pid) ->
            ensure_alive_voice_server(Pid, State);
        _ ->
            adopt_or_start(State, missing_voice_server)
    end.

-spec ensure_alive_voice_server(pid(), guild_state()) ->
    {ok, pid(), guild_state()} | {{error, atom()}, guild_state()}.
ensure_alive_voice_server(Pid, State) ->
    case process_liveness:is_alive(Pid) of
        true ->
            {ok, Pid, State};
        false ->
            CleanState = maps:remove(voice_server_pid, State),
            adopt_or_start(CleanState, dead_voice_server)
    end.

-spec reply_voice_server_pid(guild_state()) ->
    {reply, {ok, pid()} | {error, term()}, guild_state()}.
reply_voice_server_pid(State) ->
    case ensure_voice_server(State) of
        {ok, Pid, NewState} -> {reply, {ok, Pid}, NewState};
        {{error, Reason}, NewState} -> {reply, {error, Reason}, NewState}
    end.

-spec clear_stale_cached_voice_states([binary()], guild_state()) -> guild_state().
clear_stale_cached_voice_states(ConnectionIds, State) ->
    case read_authoritative_voice_states(State) of
        {ok, AuthoritativeVS} ->
            clear_stale_vs(ConnectionIds, AuthoritativeVS, State);
        {error, _} ->
            State
    end.

-spec clear_stale_vs([binary()], map(), guild_state()) -> guild_state().
clear_stale_vs(ConnectionIds, AuthoritativeVS, State) ->
    LocalVS = maps:get(voice_states, State, #{}),
    StaleVS = maps:filter(
        fun(ConnId, _VS) ->
            lists:member(ConnId, ConnectionIds) andalso
                not maps:is_key(ConnId, AuthoritativeVS)
        end,
        LocalVS
    ),
    remove_stale_voice_states(StaleVS, LocalVS, State).

-spec remove_stale_voice_states(map(), map(), guild_state()) -> guild_state().
remove_stale_voice_states(StaleVS, _LocalVS, State) when map_size(StaleVS) =:= 0 ->
    State;
remove_stale_voice_states(StaleVS, LocalVS, State) ->
    NewVS = maps:without(maps:keys(StaleVS), LocalVS),
    NewState = State#{voice_states => NewVS},
    GuildId = maps:get(id, State, undefined),
    StaleCount = maps:size(StaleVS),
    logger:warning(
        "guild_stale_cached_voice_states_cleared:"
        " guild_id=~p cleared_count=~p",
        [GuildId, StaleCount]
    ),
    voice_state_utils:broadcast_disconnects(StaleVS, NewState),
    NewState.

-spec adopt_or_start(guild_state(), atom()) ->
    {ok, pid(), guild_state()} | {{error, atom()}, guild_state()}.
adopt_or_start(State, Reason) ->
    case state_guild_id(State) of
        {ok, GuildId} ->
            adopt_registered_or_start(State, GuildId, Reason);
        error ->
            {{error, no_voice_server}, maps:remove(voice_server_pid, State)}
    end.

-spec adopt_registered_or_start(guild_state(), integer(), atom()) ->
    {ok, pid(), guild_state()} | {{error, atom()}, guild_state()}.
adopt_registered_or_start(State, GuildId, Reason) ->
    case guild_voice_server:lookup_registered(GuildId) of
        {ok, VoiceServerPid} ->
            logger:warning(
                "guild_voice_server_adopted: guild_id=~p voice_server_pid=~p reason=~p",
                [GuildId, VoiceServerPid, Reason]
            ),
            {ok, VoiceServerPid, State#{voice_server_pid => VoiceServerPid}};
        {error, not_found} ->
            start_empty_replacement(State, GuildId, Reason)
    end.

-spec start_empty_replacement(guild_state(), integer(), atom()) ->
    {ok, pid(), guild_state()} | {{error, atom()}, guild_state()}.
start_empty_replacement(State, GuildId, Reason) ->
    CachedVoiceStates = voice_state_utils:ensure_voice_states(
        maps:get(voice_states, State, #{})
    ),
    CachedCount = maps:size(CachedVoiceStates),
    maybe
        {ok, VoicePid} ?= guild_voice_server:start_link(GuildId, self(), #{}),
        logger:warning(
            "guild_voice_server_restarted_empty:"
            " guild_id=~p voice_server_pid=~p reason=~p"
            " cached_voice_state_count=~p",
            [GuildId, VoicePid, Reason, CachedCount]
        ),
        ok = schedule_stale_cleanup(maps:keys(CachedVoiceStates)),
        {ok, VoicePid, State#{voice_server_pid => VoicePid}}
    else
        {error, StartReason} ->
            logger:error(
                "guild_voice_server_restart_failed: guild_id=~p reason=~p start_reason=~p",
                [GuildId, Reason, StartReason]
            ),
            {{error, no_voice_server}, maps:remove(voice_server_pid, State)}
    end.

-spec schedule_stale_cleanup([binary()]) -> ok.
schedule_stale_cleanup([]) ->
    ok;
schedule_stale_cleanup(ConnectionIds) ->
    _ = erlang:send_after(
        ?VOICE_CACHE_RECOVERY_GRACE_MS,
        self(),
        {clear_stale_cached_voice_states, lists:usort(ConnectionIds)}
    ),
    ok.

-spec read_authoritative_voice_states(guild_state()) -> {ok, map()} | {error, term()}.
read_authoritative_voice_states(State) ->
    case voice_server_pid(State) of
        {ok, VoiceServerPid} -> read_from_pid(VoiceServerPid);
        error -> {error, no_voice_server}
    end.

-spec authoritative_voice_states(guild_state()) -> map().
authoritative_voice_states(State) ->
    case read_authoritative_voice_states(State) of
        {ok, VoiceStates} -> VoiceStates;
        {error, _Reason} -> voice_state_utils:voice_states(State)
    end.

-spec cast_disconnect_voice_user(integer(), guild_state()) -> ok.
cast_disconnect_voice_user(UserId, State) when is_integer(UserId), UserId > 0 ->
    case voice_server_pid(State) of
        {ok, VoiceServerPid} ->
            gen_server:cast(
                VoiceServerPid,
                {disconnect_voice_user, #{user_id => UserId, connection_id => null}}
            );
        error ->
            ok
    end;
cast_disconnect_voice_user(_UserId, _State) ->
    ok.

-spec voice_server_pid(guild_state()) -> {ok, pid()} | error.
voice_server_pid(State) ->
    case maps:get(voice_server_pid, State, undefined) of
        Pid when is_pid(Pid) -> live_voice_server_pid(Pid, State);
        _ -> registered_voice_server_pid(State)
    end.

-spec live_voice_server_pid(pid(), guild_state()) -> {ok, pid()} | error.
live_voice_server_pid(Pid, State) ->
    case Pid =/= self() andalso process_liveness:is_alive(Pid) of
        true -> {ok, Pid};
        false -> registered_voice_server_pid(State)
    end.

-spec registered_voice_server_pid(guild_state()) -> {ok, pid()} | error.
registered_voice_server_pid(State) ->
    case state_guild_id(State) of
        {ok, Id} -> registered_pid_for_guild(Id);
        error -> error
    end.

-spec registered_pid_for_guild(integer()) -> {ok, pid()} | error.
registered_pid_for_guild(Id) ->
    case guild_voice_server:lookup_registered(Id) of
        {ok, Pid} when Pid =/= self() -> {ok, Pid};
        _ -> error
    end.

-spec state_guild_id(guild_state()) -> {ok, integer()} | error.
state_guild_id(State) ->
    case maps:get(id, State, undefined) of
        Id when is_integer(Id) -> {ok, Id};
        _ -> error
    end.

-spec read_from_pid(pid()) -> {ok, map()} | {error, term()}.
read_from_pid(VoiceServerPid) ->
    try gen_server:call(VoiceServerPid, {get_voice_states_map}, 500) of
        VS when is_map(VS) -> {ok, voice_state_utils:ensure_voice_states(VS)};
        Other -> {error, Other}
    catch
        exit:Reason -> {error, Reason}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

cast_disconnect_voice_user_reaches_the_voice_server_test() ->
    Self = self(),
    VoicePid = spawn(fun() ->
        receive
            Message -> Self ! {voice_server_got, Message}
        end
    end),
    ok = cast_disconnect_voice_user(7, #{id => 42, voice_server_pid => VoicePid}),
    receive
        {voice_server_got, {'$gen_cast', {disconnect_voice_user, Request}}} ->
            ?assertEqual(#{user_id => 7, connection_id => null}, Request)
    after 200 ->
        exit(VoicePid, kill),
        ?assert(false)
    end.

cast_disconnect_voice_user_without_a_voice_server_is_a_noop_test() ->
    ?assertEqual(ok, cast_disconnect_voice_user(7, #{id => 987654329})),
    ?assertEqual(ok, cast_disconnect_voice_user(undefined, #{id => 987654329})).

authoritative_voice_states_falls_back_to_the_guild_copy_test() ->
    Cached = #{<<"conn">> => #{<<"user_id">> => <<"7">>}},
    State = #{id => 987654331, voice_states => Cached},
    ?assertEqual(Cached, authoritative_voice_states(State)).

authoritative_voice_states_prefers_the_voice_server_test() ->
    Live = #{<<"live">> => #{<<"user_id">> => <<"8">>}},
    VoicePid = spawn(fun() -> voice_states_reply_loop(Live) end),
    State = #{id => 987654333, voice_states => #{}, voice_server_pid => VoicePid},
    try
        ?assertEqual(Live, authoritative_voice_states(State))
    after
        exit(VoicePid, kill)
    end.

voice_states_reply_loop(VoiceStates) ->
    receive
        {'$gen_call', From, {get_voice_states_map}} ->
            gen_server:reply(From, VoiceStates),
            voice_states_reply_loop(VoiceStates);
        _ ->
            voice_states_reply_loop(VoiceStates)
    end.

-endif.
