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

-module(presence_targets).

-export([
    friend_ids_from_state/1,
    group_dm_recipients_from_state/1
]).

friend_ids_from_state(State) ->
    Relationships = maps:get(relationships, State, #{}),
    [
        UserId
     || {UserId, Type} <- maps:to_list(Relationships),
        Type =:= 1 orelse Type =:= 3
    ].

group_dm_recipients_from_state(State) ->
    UserId = maps:get(user_id, State),
    Channels = maps:get(channels, State, #{}),
    maps:from_list(
        [
            {ChannelId, map_from_ids([Rid || Rid <- RecipientIds, Rid =/= UserId])}
         || {ChannelId, Channel} <- maps:to_list(Channels),
            maps:get(<<"type">>, Channel, 0) =:= 3,
            RecipientIds <- [extract_recipient_ids(Channel)]
        ]
    ).

extract_recipient_ids(Channel) ->
    Recipients = maps:get(<<"recipients">>, Channel, maps:get(<<"recipient_ids">>, Channel, [])),
    Unique =
        lists:foldl(
            fun(Entry, Acc) ->
                case extract_recipient_id(Entry) of
                    undefined ->
                        Acc;
                    Value ->
                        case lists:member(Value, Acc) of
                            true -> Acc;
                            false -> [Value | Acc]
                        end
                end
            end,
            [],
            Recipients
        ),
    lists:reverse(Unique).

extract_recipient_id(Entry) when is_map(Entry) ->
    type_conv:extract_id(Entry, <<"id">>);
extract_recipient_id(Entry) ->
    case Entry of
        Bin when is_binary(Bin) ->
            type_conv:extract_id(#{<<"id">> => Bin}, <<"id">>);
        Int when is_integer(Int) ->
            Int;
        _ ->
            undefined
    end.

map_from_ids(Ids) when is_list(Ids) ->
    maps:from_list([{Id, true} || Id <- Ids]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

friend_ids_from_state_filters_relationship_types_test() ->
    State = #{
        relationships =>
            #{
                10 => 1,
                11 => 3,
                12 => 4,
                13 => 2
            }
    },
    Ids = lists:sort(friend_ids_from_state(State)),
    ?assertEqual([10, 11], Ids),
    ok.

-endif.
