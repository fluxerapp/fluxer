%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data_wire).
-typing([eqwalizer]).

-export([payload/1]).

-type field_kind() :: snowflake | permission | snowflake_list | id_string | generic.
-type path() :: [binary()].

-spec payload(term()) -> term().
payload(Value) ->
    payload(Value, []).

-spec payload(term(), path()) -> term().
payload(Value, Path) when is_map(Value) ->
    maps:fold(
        fun(Key, FieldValue, Acc) ->
            payload_map_field(Path, Key, FieldValue, Acc)
        end,
        #{},
        Value
    );
payload(Value, Path) when is_list(Value) ->
    [payload(Item, Path) || Item <- Value];
payload(Value, _Path) ->
    Value.

-spec payload_map_field(path(), term(), term(), map()) -> map().
payload_map_field(Path, Key, FieldValue, Acc) ->
    KeyBinary = key_binary(Key),
    case keep_payload_field(KeyBinary) of
        true -> Acc#{payload_key(Key) => payload_field(Path, Key, KeyBinary, FieldValue)};
        false -> Acc
    end.

-spec keep_payload_field(binary() | undefined) -> boolean().
keep_payload_field(<<"recipient_ids">>) -> false;
keep_payload_field(<<"role_index">>) -> false;
keep_payload_field(<<"channel_index">>) -> false;
keep_payload_field(<<"member_role_index">>) -> false;
keep_payload_field(<<"role_perms_cache">>) -> false;
keep_payload_field(<<"overwrite_perms_cache">>) -> false;
keep_payload_field(_KeyBinary) -> true.

-spec payload_key(term()) -> term().
payload_key(Key) when is_integer(Key) ->
    integer_to_binary(Key);
payload_key(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
payload_key(Key) ->
    Key.

-spec payload_field(path(), term(), binary() | undefined, term()) -> term().
payload_field(Path, Key, KeyBinary, Value) ->
    FieldPath = path_push(KeyBinary, Path),
    case field_kind(Path, Key, KeyBinary, Value) of
        snowflake -> payload_snowflake(Value, FieldPath);
        permission -> payload_permission(Value, FieldPath);
        snowflake_list -> payload_snowflake_list(Value, FieldPath);
        id_string -> payload_id_string(Value, FieldPath);
        generic -> payload(Value, FieldPath)
    end.

-spec payload_snowflake(term(), path()) -> term().
payload_snowflake(Value, _Path) when is_integer(Value) ->
    integer_to_binary(Value);
payload_snowflake(Value, Path) ->
    payload(Value, Path).

-spec payload_permission(term(), path()) -> term().
payload_permission(Value, _Path) when is_integer(Value) ->
    integer_to_binary(Value);
payload_permission(Value, Path) ->
    payload(Value, Path).

-spec payload_snowflake_list(term(), path()) -> term().
payload_snowflake_list(Values, Path) when is_list(Values) ->
    [payload_snowflake(Item, Path) || Item <- Values];
payload_snowflake_list(Value, Path) ->
    payload(Value, Path).

-spec payload_id_string(term(), path()) -> term().
payload_id_string(Value, _Path) when is_integer(Value) ->
    integer_to_binary(Value);
payload_id_string(Value, Path) ->
    payload(Value, Path).

-spec field_kind(path(), term(), binary() | undefined, term()) -> field_kind().
field_kind(Path, Key, KeyBinary, Value) ->
    case is_snowflake_record_list_value(Key, Value) of
        true -> snowflake_list;
        false -> field_kind_binary(Path, KeyBinary, Value)
    end.

-spec field_kind_binary(path(), binary() | undefined, term()) -> field_kind().
field_kind_binary(_Path, <<"permissions">>, _Value) ->
    permission;
field_kind_binary(_Path, <<"allow">>, _Value) ->
    permission;
field_kind_binary(_Path, <<"deny">>, _Value) ->
    permission;
field_kind_binary(Path, Key, Value) when is_binary(Key) ->
    field_kind_for_binary(Path, Key, Value);
field_kind_binary(_Path, _Key, _Value) ->
    generic.

-spec field_kind_for_binary(path(), binary(), term()) -> field_kind().
field_kind_for_binary(Path, Key, Value) ->
    case is_snowflake_list_key(Key, Value) of
        true -> snowflake_list;
        false -> field_kind_scalar(Path, Key)
    end.

-spec field_kind_scalar(path(), binary()) -> snowflake | id_string | generic.
field_kind_scalar(Path, Key) ->
    case is_snowflake_key(Path, Key) of
        true -> snowflake;
        false -> opaque_id_kind(Key)
    end.

-spec opaque_id_kind(binary()) -> id_string | generic.
opaque_id_kind(Key) ->
    case is_opaque_id_key(Key) of
        true -> id_string;
        false -> generic
    end.

-spec is_snowflake_key(path(), binary()) -> boolean().
is_snowflake_key(Path, <<"id">>) ->
    not has_any_path([<<"guild_folders">>, <<"rtc_regions">>], Path);
is_snowflake_key(_Path, Key) ->
    has_suffix(Key, <<"_id">>) andalso not is_opaque_id_key(Key).

-spec is_opaque_id_key(binary()) -> boolean().
is_opaque_id_key(<<"session_id">>) -> true;
is_opaque_id_key(<<"connection_id">>) -> true;
is_opaque_id_key(<<"subscription_id">>) -> true;
is_opaque_id_key(<<"app_id">>) -> true;
is_opaque_id_key(<<"device_id">>) -> true;
is_opaque_id_key(<<"region_id">>) -> true;
is_opaque_id_key(<<"server_id">>) -> true;
is_opaque_id_key(<<"target_id">>) -> true;
is_opaque_id_key(_) -> false.

-spec is_snowflake_list_key(binary(), term()) -> boolean().
is_snowflake_list_key(<<"mention_roles">>, _Value) -> true;
is_snowflake_list_key(<<"participants">>, _Value) -> true;
is_snowflake_list_key(<<"ringing">>, _Value) -> true;
is_snowflake_list_key(<<"nsfw_emojis">>, _Value) -> true;
is_snowflake_list_key(<<"pinned_dms">>, _Value) -> true;
is_snowflake_list_key(<<"restricted_guilds">>, _Value) -> true;
is_snowflake_list_key(<<"bot_restricted_guilds">>, _Value) -> true;
is_snowflake_list_key(<<"roles">>, Value) -> is_scalar_snowflake_list(Value);
is_snowflake_list_key(<<"mentions">>, Value) -> is_scalar_snowflake_list(Value);
is_snowflake_list_key(<<"recipients">>, Value) -> is_scalar_snowflake_list(Value);
is_snowflake_list_key(Key, _Value) -> has_suffix(Key, <<"_ids">>).

-spec is_snowflake_record_list_value(term(), term()) -> boolean().
is_snowflake_record_list_value(Key, Value) ->
    key_is_snowflake(Key) andalso is_scalar_snowflake_list(Value).

-spec key_is_snowflake(term()) -> boolean().
key_is_snowflake(Key) when is_integer(Key), Key > 0 ->
    true;
key_is_snowflake(<<First, _/binary>>) when First < $1; First > $9 ->
    false;
key_is_snowflake(Key) when is_binary(Key) ->
    snowflake_id:is_valid(Key);
key_is_snowflake(_) ->
    false.

-spec is_scalar_snowflake_list(term()) -> boolean().
is_scalar_snowflake_list([]) ->
    true;
is_scalar_snowflake_list(Values) when is_list(Values) ->
    lists:all(fun is_snowflake_scalar/1, Values);
is_scalar_snowflake_list(_) ->
    false.

-spec is_snowflake_scalar(term()) -> boolean().
is_snowflake_scalar(Value) when is_integer(Value), Value > 0 ->
    true;
is_snowflake_scalar(Value) when is_binary(Value) ->
    snowflake_id:is_valid(Value);
is_snowflake_scalar(_) ->
    false.

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

-spec key_binary(term()) -> binary() | undefined.
key_binary(Key) when is_binary(Key) ->
    Key;
key_binary(Key) when is_atom(Key) ->
    atom_to_binary(Key, utf8);
key_binary(_) ->
    undefined.

-spec path_push(binary() | undefined, path()) -> path().
path_push(undefined, Path) ->
    Path;
path_push(Binary, Path) ->
    [Binary | Path].

-spec has_any_path([binary()], path()) -> boolean().
has_any_path(Keys, Path) ->
    lists:any(fun(Key) -> lists:member(Key, Path) end, Keys).
