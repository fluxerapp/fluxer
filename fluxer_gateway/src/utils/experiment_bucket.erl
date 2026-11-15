%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(experiment_bucket).
-typing([eqwalizer]).

-export([bucket/2, resolution/0]).

-define(RESOLUTION, 10000).
-define(FNV_OFFSET_BASIS_32, 16#811c9dc5).
-define(FNV_PRIME_32, 16#01000193).

-spec resolution() -> pos_integer().
resolution() ->
    ?RESOLUTION.

-spec bucket(binary(), binary()) -> non_neg_integer().
bucket(Id, Salt) ->
    hash(<<Salt/binary, ":", Id/binary>>, ?FNV_OFFSET_BASIS_32) rem ?RESOLUTION.

-spec hash(binary(), non_neg_integer()) -> non_neg_integer().
hash(<<>>, Hash) ->
    Hash;
hash(<<Byte:8, Rest/binary>>, Hash) ->
    hash(Rest, ((Hash bxor Byte) * ?FNV_PRIME_32) band 16#ffffffff).
