%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_cleanup).
-typing([eqwalizer]).

-export([
    unsubscribe_presence/1,
    safe_cleanup/2,
    cleanup_per_guild_ets/1,
    cleanup_passive_sync_registry/1,
    cleanup_voice_server/1,
    cleanup_member_list_subs/1,
    cleanup_member_list_engine/1
]).

-spec unsubscribe_presence(integer()) -> ok.
unsubscribe_presence(UserId) ->
    try presence_bus:unsubscribe(UserId) of
        _ -> ok
    catch
        throw:_Reason -> ok;
        error:_Reason -> ok;
        exit:_Reason -> ok
    end.

-spec safe_cleanup(fun(() -> term()), string()) -> ok.
safe_cleanup(Fun, Label) ->
    try Fun() of
        _ -> ok
    catch
        Class:Reason ->
            logger:warning(
                "guild_terminate_cleanup_failed: step=~s class=~p reason=~p",
                [Label, Class, Reason]
            ),
            ok
    end.

-spec cleanup_per_guild_ets(map()) -> ok.
cleanup_per_guild_ets(State) ->
    Data = maps:get(data, State, #{}),
    safe_delete_ets(maps:get(members_ets, Data, undefined)),
    safe_delete_ets(maps:get(thread_store, Data, undefined)),
    safe_delete_ets(maps:get(member_presence, State, undefined)),
    safe_delete_ets(maps:get(viewable_channels_cache, State, undefined)),
    ok.

-spec cleanup_passive_sync_registry(map()) -> ok.
cleanup_passive_sync_registry(#{id := GuildId} = State) when is_integer(GuildId) ->
    maps:foreach(
        fun(SessionId, _Session) -> passive_sync_registry:delete(SessionId, GuildId) end,
        maps:get(sessions, State, #{})
    );
cleanup_passive_sync_registry(_State) ->
    ok.

-spec cleanup_voice_server(map()) -> ok.
cleanup_voice_server(State) ->
    case maps:get(voice_server_pid, State, undefined) of
        Pid when is_pid(Pid) ->
            stop_voice_server_if_alive(Pid);
        _ ->
            ok
    end.

-spec stop_voice_server_if_alive(pid()) -> ok.
stop_voice_server_if_alive(Pid) ->
    case process_liveness:is_alive(Pid) of
        true -> safe_stop_voice_server(Pid);
        false -> ok
    end.

-spec safe_stop_voice_server(pid()) -> ok.
safe_stop_voice_server(Pid) ->
    try gen_server:stop(Pid, shutdown, 5000) of
        _ -> ok
    catch
        exit:_Reason -> ok
    end.

-spec cleanup_member_list_subs(map()) -> ok.
cleanup_member_list_subs(State) ->
    case maps:get(member_list_subscriptions, State, undefined) of
        Tab when Tab =/= undefined ->
            guild_member_list_subs:destroy(Tab);
        _ ->
            ok
    end.

-spec cleanup_member_list_engine(map()) -> ok.
cleanup_member_list_engine(State) ->
    _ = guild_member_list_channel_engine:destroy_all(State),
    case maps:get(member_list_engine, State, undefined) of
        Ref when Ref =/= undefined ->
            guild_member_list_engine:destroy(Ref);
        _ ->
            ok
    end.

-spec safe_delete_ets(term()) -> ok.
safe_delete_ets(undefined) ->
    ok;
safe_delete_ets(Tab) ->
    try ets:delete(eqwalizer:dynamic_cast(Tab)) of
        _ -> ok
    catch
        error:badarg -> ok
    end.
