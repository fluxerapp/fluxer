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

-module(gateway_codec).

-export([
    encode/2,
    decode/2,
    parse_encoding/1
]).

-type encoding() :: json.
-export_type([encoding/0]).

-spec parse_encoding(binary() | undefined) -> encoding().
parse_encoding(_) -> json.

-spec encode(map(), encoding()) -> {ok, iodata(), text | binary} | {error, term()}.
encode(Message, json) ->
    try
        Encoded = jsx:encode(Message),
        {ok, Encoded, text}
    catch
        _:Reason ->
            {error, {encode_failed, Reason}}
    end.

-spec decode(binary(), encoding()) -> {ok, map()} | {error, term()}.
decode(Data, json) ->
    try
        Decoded = jsx:decode(Data, [{return_maps, true}]),
        {ok, Decoded}
    catch
        _:Reason ->
            {error, {decode_failed, Reason}}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_encoding_test() ->
    ?assertEqual(json, parse_encoding(<<"json">>)),
    ?assertEqual(json, parse_encoding(<<"etf">>)),
    ?assertEqual(json, parse_encoding(undefined)),
    ?assertEqual(json, parse_encoding(<<"invalid">>)).

encode_json_test() ->
    Message = #{<<"op">> => 0, <<"d">> => #{<<"test">> => true}},
    {ok, Encoded, text} = encode(Message, json),
    ?assert(is_binary(Encoded)).

decode_json_test() ->
    Data = <<"{\"op\":0,\"d\":{\"test\":true}}">>,
    {ok, Decoded} = decode(Data, json),
    ?assertEqual(0, maps:get(<<"op">>, Decoded)).

roundtrip_json_test() ->
    Original = #{<<"op">> => 10, <<"d">> => #{<<"heartbeat_interval">> => 41250}},
    {ok, Encoded, _} = encode(Original, json),
    {ok, Decoded} = decode(iolist_to_binary(Encoded), json),
    ?assertEqual(Original, Decoded).

-endif.
