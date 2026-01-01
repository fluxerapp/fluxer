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

-module(rendezvous_router).

-export([select/2, group_keys/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(HASH_LIMIT, 16#FFFFFFFF).

-spec select(term(), pos_integer()) -> non_neg_integer().
select(Key, ShardCount) when ShardCount > 0 ->
    Indices = lists:seq(0, ShardCount - 1),
    {Index, _Weight} =
        lists:foldl(
            fun(CurrentIndex, {BestIndex, BestWeight}) ->
                Weight = weight(Key, CurrentIndex),
                case
                    (Weight > BestWeight) orelse
                        (Weight =:= BestWeight andalso CurrentIndex < BestIndex)
                of
                    true ->
                        {CurrentIndex, Weight};
                    false ->
                        {BestIndex, BestWeight}
                end
            end,
            {0, -1},
            Indices
        ),
    Index;
select(_Key, _ShardCount) ->
    0.

-spec group_keys([term()], pos_integer()) -> [{non_neg_integer(), [term()]}].
group_keys(Keys, ShardCount) when is_list(Keys), ShardCount > 0 ->
    Sorted =
        maps:to_list(
            lists:foldl(
                fun(Key, Acc) ->
                    Index = select(Key, ShardCount),
                    Existing = maps:get(Index, Acc, []),
                    maps:put(Index, [Key | Existing], Acc)
                end,
                #{},
                Keys
            )
        ),
    lists:sort(
        fun({IdxA, _}, {IdxB, _}) -> IdxA =< IdxB end,
        [{Index, lists:usort(Group)} || {Index, Group} <- Sorted]
    );
group_keys(_Keys, _ShardCount) ->
    [].

-spec weight(term(), non_neg_integer()) -> non_neg_integer().
weight(Key, Index) ->
    erlang:phash2({Key, Index}, ?HASH_LIMIT).

-ifdef(TEST).
select_returns_valid_index_test() ->
    ?assertEqual(0, select(test_key, 1)),
    Index = select(test_key, 5),
    ?assert(Index >= 0),
    ?assert(Index < 5).

select_is_stable_for_same_inputs_test() ->
    ?assertEqual(select(<<"abc">>, 8), select(<<"abc">>, 8)),
    ?assertEqual(select(12345, 3), select(12345, 3)).

group_keys_sorts_and_deduplicates_test() ->
    Keys = [1, 2, 3, 1, 2],
    Groups = group_keys(Keys, 2),
    ?assertMatch([{_, _}, {_, _}], Groups),
    lists:foreach(
        fun({_Index, GroupKeys}) ->
            ?assertEqual(GroupKeys, lists:usort(GroupKeys))
        end,
        Groups
    ).

group_keys_handles_empty_test() ->
    ?assertEqual([], group_keys([], 4)).
-endif.
