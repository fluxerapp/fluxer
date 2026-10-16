%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_codec).
-typing([eqwalizer]).

-export([
    encode/2,
    decode/2,
    parse_encoding/1
]).

-type encoding() :: json.
-type frame_type() :: text | binary.

-export_type([encoding/0, frame_type/0]).

-spec parse_encoding(binary() | undefined) -> encoding().
parse_encoding(_) ->
    json.

-spec encode(map(), encoding()) -> {ok, iodata(), frame_type()} | {error, term()}.
encode(Message, json) ->
    try
        Encoded = iolist_to_binary(json:encode(Message)),
        {ok, Encoded, text}
    catch
        _:Reason ->
            {error, {encode_failed, Reason}}
    end.

-spec decode(binary(), encoding()) -> {ok, map()} | {error, term()}.
decode(Data, json) ->
    try
        Decoded = json:decode(Data),
        case Decoded of
            M when is_map(M) -> {ok, M};
            _ -> {error, {decode_failed, not_a_map}}
        end
    catch
        _:Reason ->
            {error, {decode_failed, Reason}}
    end.
