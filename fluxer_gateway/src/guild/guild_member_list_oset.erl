%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_oset).
-typing([eqwalizer]).

-export([
    new/0, from_sorted/2, destroy/1, insert/2, delete/2, range/3, to_list/1
]).

-type oset() :: reference().
-type key() :: {non_neg_integer(), binary(), integer()}.

-define(MAX_RANGE_COUNT, 65536).

-export_type([oset/0, key/0]).

-spec new() -> oset().
new() ->
    guild_member_list_oset_nif:new().

-spec from_sorted(oset(), [key()]) -> ok.
from_sorted(OSet, Keys) ->
    guild_member_list_oset_nif:from_sorted(OSet, Keys).

-spec destroy(oset()) -> ok.
destroy(OSet) ->
    try guild_member_list_oset_nif:destroy(OSet) of
        ok -> ok
    catch
        _:_ -> ok
    end.

-spec insert(oset(), key()) -> non_neg_integer().
insert(OSet, Key) ->
    guild_member_list_oset_nif:insert(OSet, Key).

-spec delete(oset(), key()) -> non_neg_integer() | not_found.
delete(OSet, Key) ->
    guild_member_list_oset_nif:delete(OSet, Key).

-spec range(oset(), integer(), non_neg_integer()) -> [key()].
range(_OSet, _Start, 0) ->
    [];
range(OSet, Start, Count) when Start < 0 ->
    range(OSet, 0, max(0, Count + Start));
range(OSet, Start, Count) when Count =< ?MAX_RANGE_COUNT ->
    guild_member_list_oset_nif:range(OSet, Start, Count);
range(OSet, Start, Count) ->
    range_paged(OSet, Start, Count, []).

-spec range_paged(oset(), non_neg_integer(), non_neg_integer(), [[key()]]) -> [key()].
range_paged(_OSet, _Start, 0, Acc) ->
    lists:append(lists:reverse(Acc));
range_paged(OSet, Start, Remaining, Acc) ->
    Take = min(Remaining, ?MAX_RANGE_COUNT),
    Chunk = guild_member_list_oset_nif:range(OSet, Start, Take),
    case length(Chunk) < Take of
        true ->
            lists:append(lists:reverse([Chunk | Acc]));
        false ->
            range_paged(OSet, Start + Take, Remaining - Take, [Chunk | Acc])
    end.

-spec to_list(oset()) -> [key()].
to_list(OSet) ->
    guild_member_list_oset_nif:to_list(OSet).
