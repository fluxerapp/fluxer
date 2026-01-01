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

-module(guild_passive_sync).

-export([
    schedule_passive_sync/1,
    handle_passive_sync/1,
    send_passive_updates_to_sessions/1,
    compute_delta/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(PASSIVE_SYNC_INTERVAL, 30000).

schedule_passive_sync(State) ->
    erlang:send_after(?PASSIVE_SYNC_INTERVAL, self(), passive_sync),
    State.

handle_passive_sync(State) ->
    NewState = send_passive_updates_to_sessions(State),
    schedule_passive_sync(NewState),
    {noreply, NewState}.

send_passive_updates_to_sessions(State) ->
    GuildId = maps:get(id, State),
    Sessions = maps:get(sessions, State, #{}),
    Data = maps:get(data, State, #{}),
    Channels = maps:get(<<"channels">>, Data, []),

    MemberCount = maps:get(member_count, State, undefined),

    IsLargeGuild = case MemberCount of
        undefined -> false;
        Count when is_integer(Count) -> Count > 250
    end,

    PassiveSessions = maps:filter(
        fun(_SessionId, SessionData) ->
            IsLargeGuild andalso session_passive:is_passive(GuildId, SessionData)
        end,
        Sessions
    ),

    case map_size(PassiveSessions) of
        0 ->
            State;
        _ ->
            UpdatedSessions = lists:foldl(
                fun({SessionId, SessionData}, AccSessions) ->
                    Pid = maps:get(pid, SessionData),
                    UserId = maps:get(user_id, SessionData),
                    Member = guild_permissions:find_member_by_user_id(UserId, State),

                    CurrentLastMessageIds = build_last_message_ids(Channels, UserId, Member, State),
                    PreviousLastMessageIds = maps:get(previous_passive_updates, SessionData, #{}),
                    Delta = compute_delta(CurrentLastMessageIds, PreviousLastMessageIds),

                    case {map_size(Delta), is_pid(Pid)} of
                        {0, _} ->
                            AccSessions;
                        {_, true} ->
                            EventData = #{
                                <<"guild_id">> => integer_to_binary(GuildId),
                                <<"channels">> => Delta
                            },
                            gen_server:cast(Pid, {dispatch, passive_updates, EventData}),
                            MergedLastMessageIds = maps:merge(PreviousLastMessageIds, Delta),
                            UpdatedSessionData = maps:put(previous_passive_updates, MergedLastMessageIds, SessionData),
                            maps:put(SessionId, UpdatedSessionData, AccSessions);
                        _ ->
                            AccSessions
                    end
                end,
                Sessions,
                maps:to_list(PassiveSessions)
            ),
            maps:put(sessions, UpdatedSessions, State)
    end.

compute_delta(CurrentLastMessageIds, PreviousLastMessageIds) ->
    maps:filter(
        fun(ChannelId, CurrentValue) ->
            case maps:get(ChannelId, PreviousLastMessageIds, undefined) of
                undefined -> true;
                PreviousValue -> CurrentValue =/= PreviousValue
            end
        end,
        CurrentLastMessageIds
    ).

build_last_message_ids(Channels, UserId, Member, State) ->
    lists:foldl(
        fun(Channel, Acc) ->
            ChannelIdBin = maps:get(<<"id">>, Channel, undefined),
            LastMessageId = maps:get(<<"last_message_id">>, Channel, null),
            case {ChannelIdBin, LastMessageId} of
                {undefined, _} ->
                    Acc;
                {_, null} ->
                    Acc;
                _ ->
                    ChannelId = validation:snowflake_or_default(<<"id">>, ChannelIdBin, 0),
                    case Member of
                        undefined ->
                            Acc;
                        _ ->
                            case guild_permissions:can_view_channel(UserId, ChannelId, Member, State) of
                                true ->
                                    maps:put(ChannelIdBin, LastMessageId, Acc);
                                false ->
                                    Acc
                            end
                    end
            end
        end,
        #{},
        Channels
    ).

-ifdef(TEST).

compute_delta_empty_previous_test() ->
    Current = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Previous = #{},
    Delta = compute_delta(Current, Previous),
    ?assertEqual(Current, Delta),
    ok.

compute_delta_no_changes_test() ->
    Current = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Previous = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Delta = compute_delta(Current, Previous),
    ?assertEqual(#{}, Delta),
    ok.

compute_delta_partial_changes_test() ->
    Current = #{<<"1">> => <<"101">>, <<"2">> => <<"200">>, <<"3">> => <<"300">>},
    Previous = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Delta = compute_delta(Current, Previous),
    ?assertEqual(#{<<"1">> => <<"101">>, <<"3">> => <<"300">>}, Delta),
    ok.

compute_delta_only_new_channels_test() ->
    Current = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>, <<"3">> => <<"300">>},
    Previous = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Delta = compute_delta(Current, Previous),
    ?assertEqual(#{<<"3">> => <<"300">>}, Delta),
    ok.

compute_delta_ignores_removed_channels_test() ->
    Current = #{<<"1">> => <<"100">>},
    Previous = #{<<"1">> => <<"100">>, <<"2">> => <<"200">>},
    Delta = compute_delta(Current, Previous),
    ?assertEqual(#{}, Delta),
    ok.

-endif.
