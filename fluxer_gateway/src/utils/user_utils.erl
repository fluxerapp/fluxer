%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(user_utils).
-typing([eqwalizer]).

-export([normalize_user/1, partial_user_fields/0]).

-spec partial_user_fields() -> [binary()].
partial_user_fields() ->
    [
        <<"id">>,
        <<"username">>,
        <<"discriminator">>,
        <<"global_name">>,
        <<"avatar">>,
        <<"avatar_color">>,
        <<"bot">>,
        <<"system">>,
        <<"flags">>,
        <<"mention_flags">>
    ].

-spec normalize_user(map() | term()) -> map().
normalize_user(User) when is_map(User) ->
    lists:foldl(
        fun(Key, Acc) -> add_normalized_field(Key, User, Acc) end,
        #{},
        partial_user_fields()
    );
normalize_user(_) ->
    #{}.

-spec normalize_field(binary(), term()) -> term() | undefined.
normalize_field(<<"id">>, Value) ->
    snowflake_id:parse(Value);
normalize_field(<<"avatar_color">>, null) ->
    null;
normalize_field(<<"avatar_color">>, Value) ->
    guild_data_normalize_schema:int(Value);
normalize_field(<<"flags">>, Value) ->
    bitset:parse(Value);
normalize_field(<<"mention_flags">>, Value) ->
    bitset:parse(Value);
normalize_field(_Key, Value) ->
    Value.

-spec add_normalized_field(binary(), map(), map()) -> map().
add_normalized_field(Key, User, Acc) ->
    case maps:get(Key, User, undefined) of
        undefined -> Acc;
        Value -> maybe_add_normalized_field(Key, normalize_field(Key, Value), Acc)
    end.

-spec maybe_add_normalized_field(binary(), term(), map()) -> map().
maybe_add_normalized_field(_Key, undefined, Acc) ->
    Acc;
maybe_add_normalized_field(Key, Normalized, Acc) ->
    Acc#{Key => Normalized}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

normalize_user_valid_test() ->
    User = #{
        <<"id">> => <<"123">>,
        <<"username">> => <<"testuser">>,
        <<"discriminator">> => <<"0001">>,
        <<"email">> => <<"test@example.com">>
    },
    Result = normalize_user(User),
    ?assertEqual(123, maps:get(<<"id">>, Result)),
    ?assertEqual(<<"testuser">>, maps:get(<<"username">>, Result)),
    ?assertEqual(<<"0001">>, maps:get(<<"discriminator">>, Result)),
    ?assertEqual(error, maps:find(<<"email">>, Result)).

-endif.
