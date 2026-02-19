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

-module(gateway_compress).

-export([
    new_context/1,
    compress/2,
    decompress/2,
    parse_compression/1,
    close_context/1,
    get_type/1
]).

-type compression() :: none | zstd_stream.

-opaque compress_ctx() :: #{type := compression()}.

-export_type([compression/0, compress_ctx/0]).

-spec parse_compression(binary() | undefined) -> compression().
parse_compression(<<"none">>) ->
    none;
parse_compression(<<"zstd-stream">>) ->
    none;
parse_compression(_) ->
    none.

-spec new_context(compression()) -> compress_ctx().
new_context(none) ->
    #{type => none};
new_context(zstd_stream) ->
    #{type => zstd_stream}.

-spec close_context(compress_ctx()) -> ok.
close_context(#{}) ->
    ok.

-spec get_type(compress_ctx()) -> compression().
get_type(#{type := Type}) ->
    Type.

-spec compress(iodata(), compress_ctx()) -> {ok, binary(), compress_ctx()} | {error, term()}.
compress(Data, Ctx = #{type := none}) ->
    {ok, iolist_to_binary(Data), Ctx};
compress(Data, Ctx = #{type := zstd_stream}) ->
    zstd_compress(Data, Ctx).

-spec decompress(binary(), compress_ctx()) -> {ok, binary(), compress_ctx()} | {error, term()}.
decompress(Data, Ctx = #{type := none}) ->
    {ok, Data, Ctx};
decompress(Data, Ctx = #{type := zstd_stream}) ->
    zstd_decompress(Data, Ctx).

zstd_compress(Data, Ctx) ->
    case ezstd_available() of
        true ->
            try
                Binary = iolist_to_binary(Data),
                case erlang:apply(ezstd, compress, [Binary, 3]) of
                    Compressed when is_binary(Compressed) ->
                        {ok, Compressed, Ctx};
                    {error, Reason} ->
                        {error, {compress_failed, Reason}}
                end
            catch
                _:Exception ->
                    {error, {compress_failed, Exception}}
            end;
        false ->
            {error, {compress_failed, zstd_not_available}}
    end.

zstd_decompress(Data, Ctx) ->
    case ezstd_available() of
        true ->
            try
                case erlang:apply(ezstd, decompress, [Data]) of
                    Decompressed when is_binary(Decompressed) ->
                        {ok, Decompressed, Ctx};
                    {error, Reason} ->
                        {error, {decompress_failed, Reason}}
                end
            catch
                _:Exception ->
                    {error, {decompress_failed, Exception}}
            end;
        false ->
            {error, {decompress_failed, zstd_not_available}}
    end.

-spec ezstd_available() -> boolean().
ezstd_available() ->
    case code:ensure_loaded(ezstd) of
        {module, ezstd} ->
            erlang:function_exported(ezstd, compress, 2) andalso
                erlang:function_exported(ezstd, decompress, 1);
        _ ->
            false
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_compression_test_() ->
    [
        ?_assertEqual(none, parse_compression(undefined)),
        ?_assertEqual(none, parse_compression(<<>>)),
        ?_assertEqual(none, parse_compression(<<"none">>)),
        ?_assertEqual(none, parse_compression(<<"invalid">>)),
        ?_assertEqual(none, parse_compression(<<"zstd-stream">>))
    ].

new_context_test_() ->
    [
        ?_assertEqual(none, get_type(new_context(none))),
        ?_assertEqual(zstd_stream, get_type(new_context(zstd_stream)))
    ].

close_context_test() ->
    Ctx = new_context(none),
    ?assertEqual(ok, close_context(Ctx)).

compress_none_test() ->
    Ctx = new_context(none),
    Data = <<"hello world">>,
    {ok, Compressed, Ctx2} = compress(Data, Ctx),
    ?assertEqual(Data, Compressed),
    ?assertEqual(none, get_type(Ctx2)).

compress_none_iolist_test() ->
    Ctx = new_context(none),
    Data = [<<"hello">>, <<" ">>, <<"world">>],
    {ok, Compressed, _} = compress(Data, Ctx),
    ?assertEqual(<<"hello world">>, Compressed).

decompress_none_test() ->
    Ctx = new_context(none),
    Data = <<"hello world">>,
    {ok, Decompressed, _} = decompress(Data, Ctx),
    ?assertEqual(Data, Decompressed).

-ifdef(DEV_MODE).
zstd_roundtrip_test() ->
    ?assertEqual(skip, skip).

zstd_compression_ratio_test() ->
    ?assertEqual(skip, skip).
-else.
zstd_roundtrip_test() ->
    case ezstd_available() of
        true ->
            Ctx = new_context(zstd_stream),
            Data = <<"hello world, this is a test message for zstd compression">>,
            {ok, Compressed, Ctx2} = compress(Data, Ctx),
            ?assert(is_binary(Compressed)),
            {ok, Decompressed, _} = decompress(Compressed, Ctx2),
            ?assertEqual(Data, Decompressed),
            ok = close_context(Ctx2);
        false ->
            ?assertEqual(skip, skip)
    end.

zstd_compression_ratio_test() ->
    case ezstd_available() of
        true ->
            Ctx = new_context(zstd_stream),
            Data = binary:copy(<<"aaaaaaaaaa">>, 100),
            {ok, Compressed, _} = compress(Data, Ctx),
            ?assert(byte_size(Compressed) < byte_size(Data));
        false ->
            ?assertEqual(skip, skip)
    end.
-endif.

-endif.
