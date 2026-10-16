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
binary_key_kind(<<"_fluxer_", _/binary>>) ->
    drop;
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
named_key_kind(<<"member_list_revision">>) -> drop;
named_key_kind(<<"role_perms_cache">>) -> drop;
named_key_kind(<<"overwrite_perms_cache">>) -> drop;
named_key_kind(<<"thread_index">>) -> drop;
named_key_kind(<<"member_ids_preview">>) -> drop;
named_key_kind(<<"applied_tags">>) -> scalar_list;
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

fast_payload_keeps_restricted_ids_opaque_test() ->
    Data = #{<<"rtc_regions">> => [#{<<"id">> => <<"us-east">>, <<"guild_id">> => 7}]},
    ?assertEqual(
        #{<<"rtc_regions">> => [#{<<"id">> => <<"us-east">>, <<"guild_id">> => <<"7">>}]},
        fast_payload(Data, false)
    ).

fast_payload_drops_internal_keys_test() ->
    Data = #{<<"id">> => 1, <<"role_index">> => #{}, role_perms_cache => #{}},
    ?assertEqual(#{<<"id">> => <<"1">>}, fast_payload(Data, false)).

fast_payload_shapes_thread_keys_test() ->
    Data = #{
        <<"id">> => 1,
        <<"applied_tags">> => [2, 3],
        <<"member_ids_preview">> => [4],
        <<"_fluxer_thread">> => #{},
        <<"thread_index">> => #{}
    },
    ?assertEqual(
        #{<<"id">> => <<"1">>, <<"applied_tags">> => [<<"2">>, <<"3">>]},
        fast_payload(Data, false)
    ).

pre_encoded_payload_passes_through_unchanged_test() ->
    Data = {pre_encoded, <<"{}">>},
    ?assertEqual(Data, payload(Data)).

member_list_revision_is_internal_at_every_depth_test() ->
    Data = #{
        member_list_revision => make_ref(),
        <<"nested">> => [#{<<"member_list_revision">> => make_ref(), <<"id">> => 9}]
    },
    ?assertEqual(#{<<"nested">> => [#{<<"id">> => <<"9">>}]}, payload(Data)).

-endif.
