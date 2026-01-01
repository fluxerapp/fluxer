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

-module(guild_user_data).

-export([
    update_user_data/2,
    maybe_update_cached_user_data/3,
    handle_user_data_update/3,
    check_user_data_differs/2
]).

-import(guild_permissions, [find_member_by_user_id/2]).

update_user_data(EventData, State) ->
    UserId = utils:binary_to_integer_safe(maps:get(<<"id">>, EventData)),
    Data = maps:get(data, State),
    Members = maps:get(<<"members">>, Data, []),

    UpdatedMembers = lists:map(
        fun(Member) when is_map(Member) ->
            MUser = maps:get(<<"user">>, Member, #{}),
            MemberId =
                case is_map(MUser) of
                    true ->
                        utils:binary_to_integer_safe(maps:get(<<"id">>, MUser, <<"0">>));
                    false ->
                        undefined
                end,
            if
                MemberId =:= UserId ->
                    maps:put(<<"user">>, EventData, Member);
                true ->
                    Member
            end
        end,
        Members
    ),

    UpdatedData = maps:put(<<"members">>, UpdatedMembers, Data),
    UpdatedState = maps:put(data, UpdatedData, State),

    UpdatedMember = find_member_by_user_id(UserId, UpdatedState),
    case UpdatedMember of
        undefined -> ok;
        M -> gen_server:cast(self(), {dispatch, #{event => guild_member_update, data => M}})
    end,

    {noreply, UpdatedState}.

handle_user_data_update(UserId, UserData, State) ->
    Data = maps:get(data, State),
    Members = maps:get(<<"members">>, Data, []),

    CurrentMember = find_member_by_user_id(UserId, State),
    case CurrentMember of
        undefined ->
            State;
        Member ->
            CurrentUserData = maps:get(<<"user">>, Member, #{}),
            IsDifferent = check_user_data_differs(CurrentUserData, UserData),
            if
                IsDifferent ->
                    UpdatedMembers = lists:map(
                        fun(M) when is_map(M) ->
                            MUser = maps:get(<<"user">>, M, #{}),
                            MemberId =
                                case is_map(MUser) of
                                    true ->
                                        utils:binary_to_integer_safe(
                                            maps:get(<<"id">>, MUser, <<"0">>)
                                        );
                                    false ->
                                        undefined
                                end,
                            if
                                MemberId =:= UserId ->
                                    maps:put(<<"user">>, UserData, M);
                                true ->
                                    M
                            end
                        end,
                        Members
                    ),

                    UpdatedData = maps:put(<<"members">>, UpdatedMembers, Data),
                    UpdatedState = maps:put(data, UpdatedData, State),

                    UpdatedMember = find_member_by_user_id(UserId, UpdatedState),
                    case UpdatedMember of
                        undefined ->
                            ok;
                        M ->
                            GuildId = maps:get(id, UpdatedState),
                            MemberUpdateData = maps:put(
                                <<"guild_id">>, integer_to_binary(GuildId), M
                            ),
                            gen_server:cast(
                                self(),
                                {dispatch, #{
                                    event => guild_member_update, data => MemberUpdateData
                                }}
                            )
                    end,

                    UpdatedState;
                true ->
                    State
            end
    end.

check_user_data_differs(CurrentUserData, NewUserData) ->
    utils:check_user_data_differs(CurrentUserData, NewUserData).

maybe_update_cached_user_data(Event, EventData, State) ->
    case Event of
        E when E =:= message_create; E =:= message_update ->
            case maps:get(<<"author">>, EventData, undefined) of
                undefined ->
                    State;
                AuthorData ->
                    UserId = utils:binary_to_integer_safe(maps:get(<<"id">>, AuthorData, <<"0">>)),
                    case find_member_by_user_id(UserId, State) of
                        undefined ->
                            State;
                        Member ->
                            CurrentUserData = maps:get(<<"user">>, Member, #{}),
                            case check_user_data_differs(CurrentUserData, AuthorData) of
                                true ->
                                    handle_user_data_update(UserId, AuthorData, State);
                                false ->
                                    State
                            end
                    end
            end;
        _ ->
            State
    end.
