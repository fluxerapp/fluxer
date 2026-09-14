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
    user_flags:parse(Value);
normalize_field(<<"mention_flags">>, Value) ->
    user_flags:parse(Value);
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

normalize_user_all_fields_test() ->
    User = #{
        <<"id">> => <<"123">>,
        <<"username">> => <<"test">>,
        <<"discriminator">> => <<"0">>,
        <<"global_name">> => <<"Test User">>,
        <<"avatar">> => <<"abc123">>,
        <<"avatar_color">> => 16#ff0000,
        <<"bot">> => false,
        <<"system">> => false,
        <<"flags">> => 0
    },
    Result = normalize_user(User),
    ?assertEqual(9, maps:size(Result)),
    ?assertEqual(16#ff0000, maps:get(<<"avatar_color">>, Result)).

normalize_user_passes_mention_flags_test() ->
    User = #{
        <<"id">> => <<"123">>,
        <<"username">> => <<"test">>,
        <<"discriminator">> => <<"0">>,
        <<"mention_flags">> => 1
    },
    Result = normalize_user(User),
    ?assertEqual(1, maps:get(<<"mention_flags">>, Result)).

normalize_user_undefined_values_test() ->
    User = #{
        <<"id">> => <<"123">>,
        <<"username">> => <<"test">>,
        <<"avatar">> => undefined
    },
    Result = normalize_user(User),
    ?assertEqual(2, maps:size(Result)),
    ?assertEqual(error, maps:find(<<"avatar">>, Result)).

normalize_user_not_map_test() ->
    ?assertEqual(#{}, normalize_user(not_a_map)),
    ?assertEqual(#{}, normalize_user(123)),
    ?assertEqual(#{}, normalize_user(<<"binary">>)),
    ?assertEqual(#{}, normalize_user(undefined)).

normalize_user_empty_map_test() ->
    ?assertEqual(#{}, normalize_user(#{})).

partial_user_fields_test() ->
    Fields = partial_user_fields(),
    ?assert(is_list(Fields)),
    ?assertEqual(10, length(Fields)),
    ?assert(lists:member(<<"id">>, Fields)),
    ?assert(lists:member(<<"username">>, Fields)),
    ?assert(lists:member(<<"flags">>, Fields)).

-endif.
