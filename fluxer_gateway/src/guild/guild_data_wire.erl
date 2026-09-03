%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data_wire).
-typing([eqwalizer]).

-export([payload/1]).

-type value_kind() :: scalar | scalar_list | maybe_scalar_list | id | restrict | generic.
-type key_kind() :: drop | value_kind().

-spec payload(term()) -> term().
payload(Value) ->
    fast_payload(Value, false).

-spec fast_payload(term(), boolean()) -> term().
fast_payload(Value, Restricted) when is_map(Value) ->
    maps:fold(
        fun(Key, FieldValue, Acc) ->
            fast_map_field(Restricted, Key, FieldValue, Acc)
        end,
        #{},
        Value
    );
fast_payload(Value, Restricted) when is_list(Value) ->
    [fast_payload(Item, Restricted) || Item <- Value];
fast_payload(Value, _Restricted) ->
    Value.

-spec fast_map_field(boolean(), term(), term(), map()) -> map().
fast_map_field(Restricted, Key, Value, Acc) when is_binary(Key) ->
    fast_put(binary_key_kind(Key), Restricted, Key, Value, Acc);
fast_map_field(Restricted, Key, Value, Acc) when is_atom(Key) ->
    Name = atom_to_binary(Key, utf8),
    fast_put(named_or_suffix_kind(Name), Restricted, Name, Value, Acc);
fast_map_field(Restricted, Key, Value, Acc) when is_integer(Key), Key > 0 ->
    fast_put(maybe_scalar_list, Restricted, integer_to_binary(Key), Value, Acc);
fast_map_field(Restricted, Key, Value, Acc) when is_integer(Key) ->
    fast_put(generic, Restricted, integer_to_binary(Key), Value, Acc);
fast_map_field(Restricted, Key, Value, Acc) ->
    fast_put(generic, Restricted, Key, Value, Acc).

-spec fast_put(key_kind(), boolean(), term(), term(), map()) -> map().
fast_put(drop, _Restricted, _Key, _Value, Acc) ->
    Acc;
fast_put(Kind, Restricted, Key, Value, Acc) ->
    Acc#{Key => fast_field(Kind, Restricted, Value)}.

-spec fast_field(key_kind(), boolean(), term()) -> term().
fast_field(generic, Restricted, Value) ->
    fast_payload(Value, Restricted);
fast_field(scalar, Restricted, Value) ->
    fast_scalar(Value, Restricted);
fast_field(scalar_list, Restricted, Value) ->
    fast_scalar_list(Value, Restricted);
fast_field(maybe_scalar_list, Restricted, Value) ->
    fast_maybe_scalar_list(Value, Restricted);
fast_field(id, false, Value) ->
    fast_scalar(Value, false);
fast_field(id, true, Value) ->
    fast_payload(Value, true);
fast_field(restrict, _Restricted, Value) ->
    fast_payload(Value, true).

-spec fast_scalar(term(), boolean()) -> term().
fast_scalar(Value, _Restricted) when is_integer(Value) ->
    integer_to_binary(Value);
fast_scalar(Value, Restricted) ->
    fast_payload(Value, Restricted).

-spec fast_scalar_list(term(), boolean()) -> term().
fast_scalar_list(Values, Restricted) when is_list(Values) ->
    [fast_scalar(Item, Restricted) || Item <- Values];
fast_scalar_list(Value, Restricted) ->
    fast_payload(Value, Restricted).

-spec fast_maybe_scalar_list(term(), boolean()) -> term().
fast_maybe_scalar_list(Value, Restricted) ->
    case is_fast_scalar_snowflake_list(Value) of
        true -> fast_scalar_list(Value, Restricted);
        false -> fast_payload(Value, Restricted)
    end.

-spec is_fast_scalar_snowflake_list(term()) -> boolean().
is_fast_scalar_snowflake_list([]) ->
    true;
is_fast_scalar_snowflake_list(Values) when is_list(Values) ->
    lists:all(fun is_fast_snowflake_scalar/1, Values);
is_fast_scalar_snowflake_list(_) ->
    false.

-spec is_fast_snowflake_scalar(term()) -> boolean().
is_fast_snowflake_scalar(Value) when is_integer(Value), Value > 0 ->
    true;
is_fast_snowflake_scalar(<<First, _/binary>> = Value) when First >= $1, First =< $9 ->
    snowflake_id:is_valid(Value);
is_fast_snowflake_scalar(_) ->
    false.

-spec binary_key_kind(binary()) -> key_kind().
binary_key_kind(<<First, _/binary>> = Key) when First >= $1, First =< $9 ->
    numeric_binary_key_kind(Key);
binary_key_kind(Key) ->
    named_or_suffix_kind(Key).

-spec named_or_suffix_kind(binary()) -> key_kind().
named_or_suffix_kind(Key) ->
    case named_key_kind(Key) of
        unknown -> suffix_key_kind(Key);
        Kind -> Kind
    end.

-spec numeric_binary_key_kind(binary()) -> value_kind().
numeric_binary_key_kind(Key) ->
    case snowflake_id:is_valid(Key) of
        true -> maybe_scalar_list;
        false -> suffix_key_kind(Key)
    end.

-spec suffix_key_kind(binary()) -> value_kind().
suffix_key_kind(Key) ->
    case has_suffix(Key, <<"_ids">>) of
        true -> scalar_list;
        false -> trailing_id_key_kind(Key)
    end.

-spec trailing_id_key_kind(binary()) -> value_kind().
trailing_id_key_kind(Key) ->
    case has_suffix(Key, <<"_id">>) of
        true -> scalar;
        false -> generic
    end.

-spec named_key_kind(binary()) -> key_kind() | unknown.
named_key_kind(<<"id">>) -> id;
named_key_kind(<<"permissions">>) -> scalar;
named_key_kind(<<"allow">>) -> scalar;
named_key_kind(<<"deny">>) -> scalar;
named_key_kind(<<"session_id">>) -> scalar;
named_key_kind(<<"connection_id">>) -> scalar;
named_key_kind(<<"subscription_id">>) -> scalar;
named_key_kind(<<"app_id">>) -> scalar;
named_key_kind(<<"device_id">>) -> scalar;
named_key_kind(<<"region_id">>) -> scalar;
named_key_kind(<<"server_id">>) -> scalar;
named_key_kind(<<"target_id">>) -> scalar;
named_key_kind(<<"mention_roles">>) -> scalar_list;
named_key_kind(<<"participants">>) -> scalar_list;
named_key_kind(<<"ringing">>) -> scalar_list;
named_key_kind(<<"nsfw_emojis">>) -> scalar_list;
named_key_kind(<<"pinned_dms">>) -> scalar_list;
named_key_kind(<<"restricted_guilds">>) -> scalar_list;
named_key_kind(<<"bot_restricted_guilds">>) -> scalar_list;
named_key_kind(<<"roles">>) -> maybe_scalar_list;
named_key_kind(<<"mentions">>) -> maybe_scalar_list;
named_key_kind(<<"recipients">>) -> maybe_scalar_list;
named_key_kind(<<"guild_folders">>) -> restrict;
named_key_kind(<<"rtc_regions">>) -> restrict;
named_key_kind(<<"recipient_ids">>) -> drop;
named_key_kind(<<"role_index">>) -> drop;
named_key_kind(<<"channel_index">>) -> drop;
named_key_kind(<<"member_role_index">>) -> drop;
named_key_kind(<<"role_perms_cache">>) -> drop;
named_key_kind(<<"overwrite_perms_cache">>) -> drop;
named_key_kind(_) -> unknown.

-spec has_suffix(binary(), binary()) -> boolean().
has_suffix(Value, Suffix) ->
    Size = byte_size(Value),
    SuffixSize = byte_size(Suffix),
    case Size >= SuffixSize of
        true ->
            has_suffix(Value, Suffix, Size - SuffixSize, SuffixSize);
        false ->
            false
    end.

-spec has_suffix(binary(), binary(), non_neg_integer(), non_neg_integer()) -> boolean().
has_suffix(Value, Suffix, PrefixSize, SuffixSize) ->
    case Value of
        <<_:PrefixSize/binary, Suffix:SuffixSize/binary>> -> true;
        _ -> false
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-type field_kind() :: snowflake | permission | snowflake_list | id_string | generic.
-type path() :: [binary()].

fast_payload_matches_original_test() ->
    lists:foreach(fun assert_fast_matches_original/1, corpus()).

fast_payload_matches_original_for_mixed_keys_test() ->
    lists:foreach(fun assert_fast_matches_original/1, mixed_key_maps()).

fast_payload_matches_original_under_restricted_paths_test() ->
    lists:foreach(fun assert_fast_matches_original/1, restricted_corpus()).

fast_payload_keeps_restricted_ids_opaque_test() ->
    Data = #{<<"rtc_regions">> => [#{<<"id">> => <<"us-east">>, <<"guild_id">> => 7}]},
    ?assertEqual(
        #{<<"rtc_regions">> => [#{<<"id">> => <<"us-east">>, <<"guild_id">> => <<"7">>}]},
        fast_payload(Data, false)
    ).

fast_payload_drops_internal_keys_test() ->
    Data = #{<<"id">> => 1, <<"role_index">> => #{}, role_perms_cache => #{}},
    ?assertEqual(#{<<"id">> => <<"1">>}, fast_payload(Data, false)).

pre_encoded_payload_passes_through_unchanged_test() ->
    Data = {pre_encoded, <<"{}">>},
    ?assertEqual(Data, payload(Data)).

assert_fast_matches_original(Value) ->
    ?assertEqual({Value, reference_payload(Value, [])}, {Value, fast_payload(Value, false)}).

corpus() ->
    [#{Key => Value} || Key <- sample_keys(), Value <- sample_values()].

restricted_corpus() ->
    lists:flatmap(fun restricted_variants/1, corpus()).

restricted_variants(Map) ->
    [
        #{<<"guild_folders">> => [Map]},
        #{<<"rtc_regions">> => Map},
        #{guild_folders => #{<<"nested">> => Map}},
        #{<<"data">> => [#{<<"rtc_regions">> => [Map]}]},
        #{<<"data">> => #{<<"more">> => Map}}
    ].

mixed_key_maps() ->
    [
        #{<<"id">> => 1, id => 2},
        #{<<"100">> => [1], 100 => [2]},
        #{<<"permissions">> => [1], permissions => 2},
        #{<<"recipient_ids">> => [1], recipient_ids => [2], <<"id">> => 3},
        #{1 => [2], -1 => [3], 0 => [4]},
        #{<<"guild_folders">> => [#{<<"id">> => 1, <<"guild_ids">> => [2], id => 3}]},
        #{<<"roles">> => [1, 2], <<"mentions">> => [#{<<"id">> => 3}], <<"deny">> => 4}
    ].

sample_keys() ->
    [
        <<"id">>,
        <<"guild_id">>,
        <<"session_id">>,
        <<"connection_id">>,
        <<"subscription_id">>,
        <<"app_id">>,
        <<"device_id">>,
        <<"region_id">>,
        <<"server_id">>,
        <<"target_id">>,
        <<"permissions">>,
        <<"allow">>,
        <<"deny">>,
        <<"roles">>,
        <<"mentions">>,
        <<"recipients">>,
        <<"ringing">>,
        <<"mention_roles">>,
        <<"participants">>,
        <<"nsfw_emojis">>,
        <<"pinned_dms">>,
        <<"restricted_guilds">>,
        <<"bot_restricted_guilds">>,
        <<"recipient_ids">>,
        <<"role_index">>,
        <<"channel_index">>,
        <<"member_role_index">>,
        <<"role_perms_cache">>,
        <<"overwrite_perms_cache">>,
        <<"guild_folders">>,
        <<"rtc_regions">>,
        <<"guild_ids">>,
        <<"username">>,
        <<"_id">>,
        <<"_ids">>,
        <<"id_">>,
        <<"9_ids">>,
        <<"100">>,
        <<"0100">>,
        <<"1x">>,
        <<>>,
        id,
        guild_id,
        session_id,
        roles,
        permissions,
        role_perms_cache,
        guild_folders,
        '100',
        100,
        0,
        -1,
        1.5,
        {tuple, key}
    ].

sample_values() ->
    [
        42,
        -7,
        0,
        <<"text">>,
        <<"42">>,
        null,
        true,
        undefined,
        [],
        [1, 2],
        [<<"3">>, 4],
        [<<"abc">>],
        [[1, 2]],
        [#{<<"id">> => 5}],
        #{<<"id">> => 6, <<"session_id">> => <<"s">>, <<"user_id">> => 7},
        #{<<"id">> => 8, <<"roles">> => [9], <<"role_index">> => #{}},
        #{<<"nested">> => #{<<"id">> => 10, <<"guild_ids">> => [11]}}
    ].

-spec reference_payload(term(), path()) -> term().
reference_payload(Value, Path) when is_map(Value) ->
    maps:fold(
        fun(Key, FieldValue, Acc) ->
            reference_payload_map_field(Path, Key, FieldValue, Acc)
        end,
        #{},
        Value
    );
reference_payload(Value, Path) when is_list(Value) ->
    [reference_payload(Item, Path) || Item <- Value];
reference_payload(Value, _Path) ->
    Value.

-spec reference_payload_map_field(path(), term(), term(), map()) -> map().
reference_payload_map_field(Path, Key, FieldValue, Acc) ->
    case reference_keep_payload_field(Key) of
        true ->
            Acc#{reference_payload_key(Key) => reference_payload_field(Path, Key, FieldValue)};
        false ->
            Acc
    end.

-spec reference_keep_payload_field(term()) -> boolean().
reference_keep_payload_field(Key) ->
    not lists:member(reference_key_binary(Key), [
        <<"recipient_ids">>,
        <<"role_index">>,
        <<"channel_index">>,
        <<"member_role_index">>,
        <<"role_perms_cache">>,
        <<"overwrite_perms_cache">>
    ]).

-spec reference_payload_key(term()) -> term().
reference_payload_key(Key) when is_integer(Key) ->
    integer_to_binary(Key);
reference_payload_key(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
reference_payload_key(Key) ->
    Key.

-spec reference_payload_field(path(), term(), term()) -> term().
reference_payload_field(Path, Key, Value) ->
    FieldPath = reference_path_push(Key, Path),
    case reference_field_kind(Path, Key, Value) of
        snowflake -> reference_payload_snowflake(Value, FieldPath);
        permission -> reference_payload_permission(Value, FieldPath);
        snowflake_list -> reference_payload_snowflake_list(Value, FieldPath);
        id_string -> reference_payload_id_string(Value, FieldPath);
        generic -> reference_payload(Value, FieldPath)
    end.

-spec reference_payload_snowflake(term(), path()) -> term().
reference_payload_snowflake(Value, _Path) when is_integer(Value) ->
    integer_to_binary(Value);
reference_payload_snowflake(Value, Path) ->
    reference_payload(Value, Path).

-spec reference_payload_permission(term(), path()) -> term().
reference_payload_permission(Value, _Path) when is_integer(Value) ->
    integer_to_binary(Value);
reference_payload_permission(Value, Path) ->
    reference_payload(Value, Path).

-spec reference_payload_snowflake_list(term(), path()) -> term().
reference_payload_snowflake_list(Values, Path) when is_list(Values) ->
    [reference_payload_snowflake(Item, Path) || Item <- Values];
reference_payload_snowflake_list(Value, Path) ->
    reference_payload(Value, Path).

-spec reference_payload_id_string(term(), path()) -> term().
reference_payload_id_string(Value, _Path) when is_integer(Value) ->
    integer_to_binary(Value);
reference_payload_id_string(Value, Path) ->
    reference_payload(Value, Path).

-spec reference_field_kind(path(), term(), term()) -> field_kind().
reference_field_kind(Path, Key, Value) ->
    case reference_is_snowflake_record_list_value(Key, Value) of
        true -> snowflake_list;
        false -> reference_field_kind_binary(Path, reference_key_binary(Key), Value)
    end.

-spec reference_field_kind_binary(path(), binary() | undefined, term()) -> field_kind().
reference_field_kind_binary(_Path, <<"permissions">>, _Value) ->
    permission;
reference_field_kind_binary(_Path, <<"allow">>, _Value) ->
    permission;
reference_field_kind_binary(_Path, <<"deny">>, _Value) ->
    permission;
reference_field_kind_binary(Path, Key, Value) when is_binary(Key) ->
    reference_field_kind_for_binary(Path, Key, Value);
reference_field_kind_binary(_Path, _Key, _Value) ->
    generic.

-spec reference_field_kind_for_binary(path(), binary(), term()) -> field_kind().
reference_field_kind_for_binary(Path, Key, Value) ->
    case reference_is_snowflake_list_key(Key, Value) of
        true -> snowflake_list;
        false -> reference_field_kind_scalar(Path, Key)
    end.

-spec reference_field_kind_scalar(path(), binary()) -> snowflake | id_string | generic.
reference_field_kind_scalar(Path, Key) ->
    case reference_is_snowflake_key(Path, Key) of
        true -> snowflake;
        false -> reference_opaque_id_kind(Key)
    end.

-spec reference_opaque_id_kind(binary()) -> id_string | generic.
reference_opaque_id_kind(Key) ->
    case reference_is_opaque_id_key(Key) of
        true -> id_string;
        false -> generic
    end.

-spec reference_is_snowflake_key(path(), binary()) -> boolean().
reference_is_snowflake_key(Path, <<"id">>) ->
    not reference_has_any_path([<<"guild_folders">>, <<"rtc_regions">>], Path);
reference_is_snowflake_key(_Path, Key) ->
    has_suffix(Key, <<"_id">>) andalso not reference_is_opaque_id_key(Key).

-spec reference_is_opaque_id_key(binary()) -> boolean().
reference_is_opaque_id_key(<<"session_id">>) -> true;
reference_is_opaque_id_key(<<"connection_id">>) -> true;
reference_is_opaque_id_key(<<"subscription_id">>) -> true;
reference_is_opaque_id_key(<<"app_id">>) -> true;
reference_is_opaque_id_key(<<"device_id">>) -> true;
reference_is_opaque_id_key(<<"region_id">>) -> true;
reference_is_opaque_id_key(<<"server_id">>) -> true;
reference_is_opaque_id_key(<<"target_id">>) -> true;
reference_is_opaque_id_key(_) -> false.

-spec reference_is_snowflake_list_key(binary(), term()) -> boolean().
reference_is_snowflake_list_key(<<"mention_roles">>, _Value) ->
    true;
reference_is_snowflake_list_key(<<"participants">>, _Value) ->
    true;
reference_is_snowflake_list_key(<<"ringing">>, _Value) ->
    true;
reference_is_snowflake_list_key(<<"nsfw_emojis">>, _Value) ->
    true;
reference_is_snowflake_list_key(<<"pinned_dms">>, _Value) ->
    true;
reference_is_snowflake_list_key(<<"restricted_guilds">>, _Value) ->
    true;
reference_is_snowflake_list_key(<<"bot_restricted_guilds">>, _Value) ->
    true;
reference_is_snowflake_list_key(<<"roles">>, Value) ->
    reference_is_scalar_snowflake_list(Value);
reference_is_snowflake_list_key(<<"mentions">>, Value) ->
    reference_is_scalar_snowflake_list(Value);
reference_is_snowflake_list_key(<<"recipients">>, Value) ->
    reference_is_scalar_snowflake_list(Value);
reference_is_snowflake_list_key(Key, _Value) ->
    has_suffix(Key, <<"_ids">>).

-spec reference_is_snowflake_record_list_value(term(), term()) -> boolean().
reference_is_snowflake_record_list_value(Key, Value) ->
    reference_key_is_snowflake(Key) andalso reference_is_scalar_snowflake_list(Value).

-spec reference_key_is_snowflake(term()) -> boolean().
reference_key_is_snowflake(Key) when is_integer(Key), Key > 0 ->
    true;
reference_key_is_snowflake(Key) when is_binary(Key) ->
    snowflake_id:is_valid(Key);
reference_key_is_snowflake(_) ->
    false.

-spec reference_is_scalar_snowflake_list(term()) -> boolean().
reference_is_scalar_snowflake_list([]) ->
    true;
reference_is_scalar_snowflake_list(Values) when is_list(Values) ->
    lists:all(fun reference_is_snowflake_scalar/1, Values);
reference_is_scalar_snowflake_list(_) ->
    false.

-spec reference_is_snowflake_scalar(term()) -> boolean().
reference_is_snowflake_scalar(Value) when is_integer(Value), Value > 0 ->
    true;
reference_is_snowflake_scalar(Value) when is_binary(Value) ->
    snowflake_id:is_valid(Value);
reference_is_snowflake_scalar(_) ->
    false.

-spec reference_key_binary(term()) -> binary() | undefined.
reference_key_binary(Key) when is_binary(Key) ->
    Key;
reference_key_binary(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
reference_key_binary(_) ->
    undefined.

-spec reference_path_push(term(), path()) -> path().
reference_path_push(Key, Path) ->
    case reference_key_binary(Key) of
        undefined -> Path;
        Binary -> [Binary | Path]
    end.

-spec reference_has_any_path([binary()], path()) -> boolean().
reference_has_any_path(Keys, Path) ->
    lists:any(fun(Key) -> lists:member(Key, Path) end, Keys).

-endif.
