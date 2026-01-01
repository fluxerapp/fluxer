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

-module(guild_availability).

-export([
    is_guild_unavailable_for_user/2,
    is_user_staff/2,
    check_unavailability_transition/2,
    handle_unavailability_transition/2
]).

-import(guild_permissions, [find_member_by_user_id/2]).
-import(guild_data, [get_guild_state/2]).

is_guild_unavailable_for_user(UserId, State) ->
    Data = maps:get(data, State),
    Guild = maps:get(<<"guild">>, Data),
    Features = maps:get(<<"features">>, Guild, []),

    HasUnavailableForEveryone = lists:member(<<"UNAVAILABLE_FOR_EVERYONE">>, Features),
    HasUnavailableForEveryoneButStaff =
        lists:member(<<"UNAVAILABLE_FOR_EVERYONE_BUT_STAFF">>, Features),

    case {HasUnavailableForEveryone, HasUnavailableForEveryoneButStaff} of
        {true, _} ->
            true;
        {false, true} ->
            not is_user_staff(UserId, State);
        {false, false} ->
            false
    end.

is_user_staff(UserId, State) ->
    case find_member_by_user_id(UserId, State) of
        undefined ->
            false;
        Member ->
            User = maps:get(<<"user">>, Member, #{}),
            Flags = utils:binary_to_integer_safe(maps:get(<<"flags">>, User, <<"0">>)),
            (Flags band 16#1) =:= 16#1
    end.

check_unavailability_transition(OldState, NewState) ->
    OldData = maps:get(data, OldState),
    OldGuild = maps:get(<<"guild">>, OldData),
    OldFeatures = maps:get(<<"features">>, OldGuild, []),

    NewData = maps:get(data, NewState),
    NewGuild = maps:get(<<"guild">>, NewData),
    NewFeatures = maps:get(<<"features">>, NewGuild, []),

    OldUnavailableForEveryone = lists:member(<<"UNAVAILABLE_FOR_EVERYONE">>, OldFeatures),
    NewUnavailableForEveryone = lists:member(<<"UNAVAILABLE_FOR_EVERYONE">>, NewFeatures),

    OldUnavailableForEveryoneButStaff =
        lists:member(<<"UNAVAILABLE_FOR_EVERYONE_BUT_STAFF">>, OldFeatures),
    NewUnavailableForEveryoneButStaff =
        lists:member(<<"UNAVAILABLE_FOR_EVERYONE_BUT_STAFF">>, NewFeatures),

    OldIsUnavailable = OldUnavailableForEveryone orelse OldUnavailableForEveryoneButStaff,
    NewIsUnavailable = NewUnavailableForEveryone orelse NewUnavailableForEveryoneButStaff,

    case {OldIsUnavailable, NewIsUnavailable} of
        {false, true} ->
            {unavailable_enabled, NewUnavailableForEveryoneButStaff};
        {true, false} ->
            unavailable_disabled;
        _ ->
            case
                {OldUnavailableForEveryoneButStaff, NewUnavailableForEveryoneButStaff,
                    OldUnavailableForEveryone, NewUnavailableForEveryone}
            of
                {true, false, false, true} ->
                    {unavailable_enabled, false};
                {false, true, true, false} ->
                    {unavailable_enabled, true};
                _ ->
                    no_change
            end
    end.

handle_unavailability_transition(OldState, NewState) ->
    GuildId = maps:get(id, NewState),
    UnavailablePayload = #{
        <<"id">> => integer_to_binary(GuildId),
        <<"unavailable">> => true
    },

    case check_unavailability_transition(OldState, NewState) of
        {unavailable_enabled, StaffOnly} ->
            Sessions = maps:get(sessions, NewState, #{}),
            lists:foreach(
                fun({_SessionId, SessionData}) ->
                    UserId = maps:get(user_id, SessionData),
                    Pid = maps:get(pid, SessionData),

                    ShouldBeUnavailable =
                        case StaffOnly of
                            true -> not is_user_staff(UserId, NewState);
                            false -> true
                        end,

                    case ShouldBeUnavailable of
                        true ->
                            gen_server:cast(Pid, {dispatch, guild_delete, UnavailablePayload});
                        false ->
                            ok
                    end
                end,
                maps:to_list(Sessions)
            );
        unavailable_disabled ->
            Sessions = maps:get(sessions, NewState, #{}),
            GuildId = maps:get(id, NewState),
            BulkPresences = presence_utils:collect_guild_member_presences(NewState),
            lists:foreach(
                fun({_SessionId, SessionData}) ->
                    UserId = maps:get(user_id, SessionData),
                    Pid = maps:get(pid, SessionData),
                    GuildState = get_guild_state(UserId, NewState),
                    gen_server:cast(Pid, {dispatch, guild_create, GuildState}),
                    presence_utils:send_presence_bulk(Pid, GuildId, UserId, BulkPresences)
                end,
                maps:to_list(Sessions)
            );
        no_change ->
            ok
    end.
