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

-module(session_monitor).

-export([
    handle_process_down/3,
    find_guild_by_ref/2,
    find_call_by_ref/2
]).

handle_process_down(Ref, _Reason, State) ->
    SocketRef = maps:get(socket_mref, State, undefined),
    PresenceRef = maps:get(presence_mref, State, undefined),
    Guilds = maps:get(guilds, State),
    Calls = maps:get(calls, State, #{}),

    case Ref of
        SocketRef when Ref =:= SocketRef ->
            self() ! {presence_update, #{status => offline}},
            erlang:send_after(10000, self(), resume_timeout),
            {noreply, maps:merge(State, #{socket_pid => undefined, socket_mref => undefined})};
        PresenceRef when Ref =:= PresenceRef ->
            self() ! {presence_connect, 0},
            {noreply, maps:put(presence_pid, undefined, State)};
        _ ->
            case find_guild_by_ref(Ref, Guilds) of
                {ok, GuildId} ->
                    handle_guild_down(GuildId, _Reason, State, Guilds);
                not_found ->
                    case find_call_by_ref(Ref, Calls) of
                        {ok, ChannelId} ->
                            handle_call_down(ChannelId, _Reason, State, Calls);
                        not_found ->
                            {noreply, State}
                    end
            end
    end.

handle_guild_down(GuildId, Reason, State, Guilds) ->
    case Reason of
        killed ->
            gen_server:cast(self(), {guild_leave, GuildId}),
            {noreply, State};
        _ ->
            GuildDeleteData = #{
                <<"id">> => integer_to_binary(GuildId),
                <<"unavailable">> => true
            },
            {noreply, UpdatedState} = session_dispatch:handle_dispatch(
                guild_delete, GuildDeleteData, State
            ),

            NewGuilds = maps:put(GuildId, undefined, Guilds),
            erlang:send_after(1000, self(), {guild_connect, GuildId, 0}),
            {noreply, maps:put(guilds, NewGuilds, UpdatedState)}
    end.

handle_call_down(ChannelId, Reason, State, Calls) ->
    case Reason of
        killed ->
            NewCalls = maps:remove(ChannelId, Calls),
            {noreply, maps:put(calls, NewCalls, State)};
        _ ->
            CallDeleteData = #{
                <<"channel_id">> => integer_to_binary(ChannelId),
                <<"unavailable">> => true
            },
            {noreply, UpdatedState} = session_dispatch:handle_dispatch(
                call_delete, CallDeleteData, State
            ),

            NewCalls = maps:put(ChannelId, undefined, Calls),
            erlang:send_after(1000, self(), {call_reconnect, ChannelId, 0}),
            {noreply, maps:put(calls, NewCalls, UpdatedState)}
    end.

find_guild_by_ref(Ref, Guilds) ->
    find_by_ref(Ref, Guilds).

find_call_by_ref(Ref, Calls) ->
    find_by_ref(Ref, Calls).

find_by_ref(Ref, Map) ->
    maps:fold(
        fun
            (Id, {_Pid, R}, _) when R =:= Ref -> {ok, Id};
            (_, _, Acc) -> Acc
        end,
        not_found,
        Map
    ).
