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

-module(presence_status).

-export([
    get_current_status/1,
    get_flattened_mobile/1,
    get_flattened_afk/1,
    collect_sessions_for_replace/1
]).

get_current_status(Sessions) ->
    AllStatuses = [maps:get(status, S) || S <- maps:values(Sessions)],

    case lists:member(invisible, AllStatuses) of
        true ->
            invisible;
        false ->
            StatusPrecedence = [online, dnd, idle],

            lists:foldl(
                fun(Status, Acc) ->
                    case Acc of
                        offline ->
                            case lists:member(Status, AllStatuses) of
                                true -> Status;
                                false -> Acc
                            end;
                        _ ->
                            Acc
                    end
                end,
                offline,
                StatusPrecedence
            )
    end.

get_flattened_mobile(Sessions) ->
    lists:any(
        fun(Session) ->
            maps:get(mobile, Session, false)
        end,
        maps:values(Sessions)
    ).

get_flattened_afk(Sessions) ->
    HasMobile = lists:any(
        fun(Session) ->
            maps:get(mobile, Session, false)
        end,
        maps:values(Sessions)
    ),

    case HasMobile of
        true ->
            false;
        false ->
            case maps:size(Sessions) of
                0 ->
                    false;
                _ ->
                    lists:all(
                        fun(Session) ->
                            maps:get(afk, Session, false)
                        end,
                        maps:values(Sessions)
                    )
            end
    end.

collect_sessions_for_replace(Sessions) ->
    Status = get_current_status(Sessions),
    Mobile = get_flattened_mobile(Sessions),
    Afk = get_flattened_afk(Sessions),
    BaseSessions = [
        #{
            <<"session_id">> => <<"all">>,
            <<"status">> => constants:status_type_atom(Status),
            <<"mobile">> => Mobile,
            <<"afk">> => Afk
        }
    ],

    SessionEntries = lists:map(
        fun({SessionId, Session}) ->
            #{
                <<"session_id">> => SessionId,
                <<"status">> => constants:status_type_atom(maps:get(status, Session)),
                <<"afk">> => maps:get(afk, Session, false),
                <<"mobile">> => maps:get(mobile, Session, false)
            }
        end,
        maps:to_list(Sessions)
    ),

    BaseSessions ++ SessionEntries.
