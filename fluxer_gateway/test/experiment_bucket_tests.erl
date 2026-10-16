%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(experiment_bucket_tests).
-typing([eqwalizer]).
-include_lib("eunit/include/eunit.hrl").

-define(VECTORS_PATH, "../packages/schema/src/domains/experiment/ExperimentBucketVectors.json").

bucket_matches_shared_vectors_test() ->
    Vectors = read_vectors(),
    ?assertMatch([_ | _], Vectors),
    lists:foreach(fun run_vector/1, Vectors).

bucket_stays_below_resolution_test() ->
    Resolution = experiment_bucket:resolution(),
    lists:foreach(
        fun(N) ->
            Bucket = experiment_bucket:bucket(integer_to_binary(N), <<"range-check">>),
            ?assert(Bucket >= 0 andalso Bucket < Resolution)
        end,
        lists:seq(0, 500)
    ).

read_vectors() ->
    case file:read_file(?VECTORS_PATH) of
        {ok, Contents} -> decode_vectors(Contents);
        {error, Reason} -> erlang:error({bucket_vectors_unreadable, ?VECTORS_PATH, Reason})
    end.

decode_vectors(Contents) ->
    case json:decode(Contents) of
        [_ | _] = Vectors -> Vectors;
        _ -> erlang:error({bucket_vectors_empty, ?VECTORS_PATH})
    end.

run_vector([Salt, Id, Expected] = Vector) when
    is_binary(Salt), is_binary(Id), is_integer(Expected)
->
    ?assertEqual({Vector, Expected}, {Vector, experiment_bucket:bucket(Id, Salt)});
run_vector(Vector) ->
    erlang:error({bucket_vector_malformed, Vector}).
