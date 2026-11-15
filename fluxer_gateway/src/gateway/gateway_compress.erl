%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_compress).
-typing([eqwalizer]).

-export([
    init/0,
    new_context/1,
    compress/2,
    decompress/2,
    decompress/3,
    parse_compression/1,
    parse_compression/2,
    close_context/1,
    get_type/1
]).

-export_type([compression/0, compress_ctx/0]).

-define(MAX_DECOMPRESSED_SIZE, 10 * 1024 * 1024).
-define(ZSTD_STREAM_BUFFER_SIZE, 64 * 1024).
-define(ZSTD_COMPRESSION_LEVEL, 3).
-define(ZSTD_DECOMPRESS_WINDOW_LOG_MAX, 21).
-define(ZSTD_DEFAULT_WINDOW_LOG, 17).
-define(ZSTD_MIN_WINDOW_LOG, 10).
-define(ZSTD_MAX_WINDOW_LOG, 21).
-define(ZSTD_MAX_TABLE_LOG, 15).
-define(ZSTD_STREAM_MAX_CHUNKS, 1000).
-define(ZSTD_AVAILABLE_KEY, {?MODULE, zstd_available}).
-define(ZSTD_STREAM_AVAILABLE_KEY, {?MODULE, zstd_stream_available}).

-type compression() :: none | zstd_frame | zstd_stream.

-opaque compress_ctx() ::
    #{type := none}
    | #{type := zstd_frame}
    | #{
        type := zstd_stream,
        stream_ctx => reference() | undefined,
        decompress_stream_ctx => reference()
    }.
-type zstd_stream_ctx() :: #{
    type := zstd_stream,
    stream_ctx => reference() | undefined,
    decompress_stream_ctx => reference()
}.

-spec init() -> ok.
init() ->
    _ = cache_availability(?ZSTD_AVAILABLE_KEY, probe_ezstd_available()),
    _ = cache_availability(?ZSTD_STREAM_AVAILABLE_KEY, probe_ezstd_stream_available()),
    ok.

-spec parse_compression(binary() | undefined) -> compression().
parse_compression(Value) ->
    parse_compression(Value, undefined).

-spec parse_compression(binary() | undefined, binary() | undefined) -> compression().
parse_compression(<<"none">>, _Stream) ->
    none;
parse_compression(<<"zstd-stream">>, Stream) ->
    case parse_stream_flag(Stream) of
        true -> zstd_stream;
        false -> none
    end;
parse_compression(_, _) ->
    none.

-spec parse_stream_flag(binary() | undefined) -> boolean().
parse_stream_flag(<<"1">>) ->
    true;
parse_stream_flag(<<"true">>) ->
    true;
parse_stream_flag(_) ->
    false.

-spec new_context(compression()) -> compress_ctx().
new_context(none) ->
    #{type => none};
new_context(zstd_frame) ->
    #{type => zstd_frame};
new_context(zstd_stream) ->
    #{type => zstd_stream, stream_ctx => undefined}.

-spec close_context(compress_ctx()) -> ok.
close_context(#{}) ->
    ok.

-spec get_type(compress_ctx()) -> compression().
get_type(#{type := Type}) ->
    Type.

-spec compress(iodata(), compress_ctx()) -> {ok, binary(), compress_ctx()} | {error, term()}.
compress(Data, #{type := none} = Ctx) ->
    {ok, iolist_to_binary(Data), Ctx};
compress(Data, #{type := zstd_frame} = Ctx) ->
    zstd_frame_compress(Data, Ctx);
compress(Data, #{type := zstd_stream, stream_ctx := _} = Ctx) ->
    zstd_stream_compress(Data, Ctx);
compress(Data, #{type := zstd_stream} = Ctx) ->
    zstd_frame_compress(Data, Ctx).

-spec decompress(binary(), compress_ctx()) -> {ok, binary(), compress_ctx()} | {error, term()}.
decompress(Data, Ctx) ->
    decompress(Data, Ctx, ?MAX_DECOMPRESSED_SIZE).

-spec decompress(binary(), compress_ctx(), pos_integer()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
decompress(Data, #{type := none} = Ctx, _MaxSize) ->
    {ok, Data, Ctx};
decompress(Data, #{type := zstd_frame} = Ctx, MaxSize) ->
    zstd_frame_decompress(Data, Ctx, MaxSize);
decompress(Data, #{type := zstd_stream, stream_ctx := _} = Ctx, MaxSize) ->
    zstd_stream_decompress(Data, Ctx, MaxSize);
decompress(Data, #{type := zstd_stream} = Ctx, MaxSize) ->
    zstd_frame_decompress(Data, Ctx, MaxSize).

-spec zstd_frame_compress(iodata(), compress_ctx()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
zstd_frame_compress(Data, Ctx) ->
    case ezstd_available() of
        true -> zstd_frame_compress_available(Data, Ctx);
        false -> {error, {compress_failed, zstd_not_available}}
    end.

-spec zstd_frame_compress_available(iodata(), compress_ctx()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
zstd_frame_compress_available(Data, Ctx) ->
    try
        Binary = iolist_to_binary(Data),
        handle_zstd_frame_compress_result(
            erlang:apply(ezstd, compress, [Binary, ?ZSTD_COMPRESSION_LEVEL]), Ctx
        )
    catch
        _:Exception ->
            {error, {compress_failed, Exception}}
    end.

-spec handle_zstd_frame_compress_result(term(), compress_ctx()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
handle_zstd_frame_compress_result(Compressed, Ctx) when is_binary(Compressed) ->
    {ok, Compressed, Ctx};
handle_zstd_frame_compress_result({error, Reason}, _Ctx) ->
    {error, {compress_failed, Reason}};
handle_zstd_frame_compress_result(Other, _Ctx) ->
    {error, {compress_failed, {case_clause, Other}}}.

-spec zstd_stream_compress(iodata(), compress_ctx()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
zstd_stream_compress(Data, Ctx) ->
    case ezstd_stream_available() of
        true -> zstd_stream_compress_available(Data, Ctx);
        false -> {error, {compress_failed, zstd_stream_not_available}}
    end.

-spec zstd_stream_compress_available(iodata(), compress_ctx()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
zstd_stream_compress_available(Data, Ctx) ->
    try
        Binary = iolist_to_binary(Data),
        case ensure_zstd_stream_context(Ctx) of
            {ok, StreamCtx, NewCtx} ->
                handle_zstd_stream_compress_result(
                    erlang:apply(ezstd, compress_streaming, [StreamCtx, Binary]), NewCtx
                );
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:Exception ->
            {error, {compress_failed, Exception}}
    end.

-spec ensure_zstd_stream_context(compress_ctx()) ->
    {ok, reference(), compress_ctx()} | {error, term()}.
ensure_zstd_stream_context(#{type := zstd_stream, stream_ctx := StreamCtx} = Ctx) when
    is_reference(StreamCtx)
->
    {ok, StreamCtx, Ctx};
ensure_zstd_stream_context(#{type := zstd_stream} = Ctx) ->
    case erlang:apply(ezstd, create_compression_context, [?ZSTD_STREAM_BUFFER_SIZE]) of
        StreamCtx when is_reference(StreamCtx) ->
            set_zstd_stream_compression_parameters(StreamCtx, Ctx);
        {error, Reason} ->
            {error, {compress_failed, Reason}};
        Other ->
            {error, {compress_failed, {case_clause, Other}}}
    end.

-spec set_zstd_stream_compression_parameters(reference(), zstd_stream_ctx()) ->
    {ok, reference(), zstd_stream_ctx()} | {error, term()}.
set_zstd_stream_compression_parameters(StreamCtx, Ctx) ->
    WindowLog = zstd_window_log(),
    TableLog = min(?ZSTD_MAX_TABLE_LOG, WindowLog - 1),
    Parameters = [
        {zstd_c_compression_level, ?ZSTD_COMPRESSION_LEVEL},
        {zstd_c_window_log, WindowLog},
        {zstd_c_hash_log, TableLog},
        {zstd_c_chain_log, TableLog}
    ],
    case apply_zstd_compression_parameters(StreamCtx, Parameters, ok) of
        ok -> {ok, StreamCtx, Ctx#{stream_ctx => StreamCtx}};
        {error, Reason} -> {error, {compress_failed, Reason}};
        Other -> {error, {compress_failed, {case_clause, Other}}}
    end.

-spec apply_zstd_compression_parameters(reference(), [{atom(), integer()}], term()) ->
    term().
apply_zstd_compression_parameters(StreamCtx, [{Name, Value} | Rest], ok) ->
    Result = erlang:apply(ezstd, set_compression_parameter, [StreamCtx, Name, Value]),
    apply_zstd_compression_parameters(StreamCtx, Rest, Result);
apply_zstd_compression_parameters(_StreamCtx, _Parameters, Result) ->
    Result.

-spec zstd_window_log() -> pos_integer().
zstd_window_log() ->
    case fluxer_gateway_env:get(zstd_window_log) of
        WindowLog when is_integer(WindowLog) ->
            max(?ZSTD_MIN_WINDOW_LOG, min(?ZSTD_MAX_WINDOW_LOG, WindowLog));
        _ ->
            ?ZSTD_DEFAULT_WINDOW_LOG
    end.

-spec handle_zstd_stream_compress_result(term(), compress_ctx()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
handle_zstd_stream_compress_result(Compressed, Ctx) when is_binary(Compressed) ->
    {ok, Compressed, Ctx};
handle_zstd_stream_compress_result(Compressed, Ctx) when is_list(Compressed) ->
    {ok, iolist_to_binary(eqwalizer:dynamic_cast(Compressed)), Ctx};
handle_zstd_stream_compress_result({error, Reason}, _Ctx) ->
    {error, {compress_failed, Reason}};
handle_zstd_stream_compress_result(Other, _Ctx) ->
    {error, {compress_failed, {case_clause, Other}}}.

-spec zstd_frame_decompress(binary(), compress_ctx(), pos_integer()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
zstd_frame_decompress(Data, Ctx, MaxSize) ->
    case ezstd_available() of
        true -> zstd_frame_decompress_available(Data, Ctx, MaxSize);
        false -> {error, {decompress_failed, zstd_not_available}}
    end.

-spec zstd_frame_decompress_available(binary(), compress_ctx(), pos_integer()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
zstd_frame_decompress_available(Data, Ctx, MaxSize) ->
    try
        handle_zstd_decompress_result(erlang:apply(ezstd, decompress, [Data]), Ctx, MaxSize)
    catch
        _:Exception ->
            {error, {decompress_failed, Exception}}
    end.

-spec handle_zstd_decompress_result(term(), compress_ctx(), pos_integer()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
handle_zstd_decompress_result(Decompressed, Ctx, MaxSize) when is_binary(Decompressed) ->
    check_decompressed_size(Decompressed, Ctx, MaxSize);
handle_zstd_decompress_result({error, Reason}, _Ctx, _MaxSize) ->
    {error, {decompress_failed, Reason}};
handle_zstd_decompress_result(Other, _Ctx, _MaxSize) ->
    {error, {decompress_failed, {case_clause, Other}}}.

-spec zstd_stream_decompress(binary(), compress_ctx(), pos_integer()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
zstd_stream_decompress(Data, Ctx, MaxSize) ->
    case ezstd_stream_available() of
        true -> zstd_stream_decompress_available(Data, Ctx, MaxSize);
        false -> {error, {decompress_failed, zstd_stream_not_available}}
    end.

-spec zstd_stream_decompress_available(binary(), compress_ctx(), pos_integer()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
zstd_stream_decompress_available(Data, Ctx, MaxSize) ->
    try
        case ensure_zstd_decompress_stream_context(Ctx) of
            {ok, StreamCtx, NewCtx} ->
                State = #{
                    stream_ctx => StreamCtx, data => Data, max_size => MaxSize, ctx => NewCtx
                },
                decompress_stream_chunks(State, 0, [], 0, ?ZSTD_STREAM_MAX_CHUNKS);
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:Exception ->
            {error, {decompress_failed, Exception}}
    end.

-type stream_chunk_state() :: #{
    stream_ctx := reference(),
    data := binary(),
    max_size := pos_integer(),
    ctx := compress_ctx()
}.

-spec decompress_stream_chunks(
    stream_chunk_state(), non_neg_integer(), iolist(), non_neg_integer(), non_neg_integer()
) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
decompress_stream_chunks(_State, _Offset, _Acc, _Size, 0) ->
    {error, {decompress_failed, decompressor_stuck}};
decompress_stream_chunks(
    #{stream_ctx := StreamCtx, data := Data} = State, Offset, Acc, Size, ChunksLeft
) ->
    Result = erlang:apply(ezstd_nif, decompress_streaming_chunk, [StreamCtx, Data, Offset]),
    handle_stream_chunk(Result, State, Acc, Size, ChunksLeft).

-spec handle_stream_chunk(
    term(), stream_chunk_state(), iolist(), non_neg_integer(), non_neg_integer()
) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
handle_stream_chunk(
    {ok, Chunk}, #{max_size := MaxSize, ctx := Ctx}, Acc, Size, _ChunksLeft
) when
    is_binary(Chunk), Size + byte_size(Chunk) =< MaxSize
->
    {ok, iolist_to_binary([Acc, Chunk]), Ctx};
handle_stream_chunk(
    {continue, Chunk, NextOffset}, #{max_size := MaxSize} = State, Acc, Size, ChunksLeft
) when
    is_binary(Chunk),
    is_integer(NextOffset),
    NextOffset >= 0,
    Size + byte_size(Chunk) =< MaxSize
->
    decompress_stream_chunks(
        State, NextOffset, [Acc, Chunk], Size + byte_size(Chunk), ChunksLeft - 1
    );
handle_stream_chunk({ok, Chunk}, _State, _Acc, _Size, _ChunksLeft) when is_binary(Chunk) ->
    {error, decompression_too_large};
handle_stream_chunk({continue, Chunk, _NextOffset}, _State, _Acc, _Size, _ChunksLeft) when
    is_binary(Chunk)
->
    {error, decompression_too_large};
handle_stream_chunk({error, Reason}, _State, _Acc, _Size, _ChunksLeft) ->
    {error, {decompress_failed, Reason}};
handle_stream_chunk(Other, _State, _Acc, _Size, _ChunksLeft) ->
    {error, {decompress_failed, {case_clause, Other}}}.

-spec ensure_zstd_decompress_stream_context(compress_ctx()) ->
    {ok, reference(), compress_ctx()} | {error, term()}.
ensure_zstd_decompress_stream_context(
    #{type := zstd_stream, decompress_stream_ctx := StreamCtx} = Ctx
) when
    is_reference(StreamCtx)
->
    {ok, StreamCtx, Ctx};
ensure_zstd_decompress_stream_context(#{type := zstd_stream} = Ctx) ->
    case erlang:apply(ezstd, create_decompression_context, [?ZSTD_STREAM_BUFFER_SIZE]) of
        StreamCtx when is_reference(StreamCtx) ->
            set_zstd_decompress_window_log_max(StreamCtx, Ctx);
        {error, Reason} ->
            {error, {decompress_failed, Reason}};
        Other ->
            {error, {decompress_failed, {case_clause, Other}}}
    end.

-spec set_zstd_decompress_window_log_max(reference(), zstd_stream_ctx()) ->
    {ok, reference(), zstd_stream_ctx()} | {error, term()}.
set_zstd_decompress_window_log_max(StreamCtx, Ctx) ->
    Result = erlang:apply(ezstd, set_decompression_parameter, [
        StreamCtx, zstd_d_window_log_max, ?ZSTD_DECOMPRESS_WINDOW_LOG_MAX
    ]),
    case Result of
        ok -> {ok, StreamCtx, Ctx#{decompress_stream_ctx => StreamCtx}};
        {error, Reason} -> {error, {decompress_failed, Reason}};
        Other -> {error, {decompress_failed, {case_clause, Other}}}
    end.

-spec check_decompressed_size(binary(), compress_ctx(), pos_integer()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
check_decompressed_size(Decompressed, Ctx, MaxSize) ->
    case byte_size(Decompressed) > MaxSize of
        true -> {error, decompression_too_large};
        false -> {ok, Decompressed, Ctx}
    end.

-spec ezstd_available() -> boolean().
ezstd_available() ->
    case persistent_term:get(?ZSTD_AVAILABLE_KEY, undefined) of
        true -> true;
        false -> false;
        _ -> cache_availability(?ZSTD_AVAILABLE_KEY, probe_ezstd_available())
    end.

-spec ezstd_stream_available() -> boolean().
ezstd_stream_available() ->
    case persistent_term:get(?ZSTD_STREAM_AVAILABLE_KEY, undefined) of
        true -> true;
        false -> false;
        _ -> cache_availability(?ZSTD_STREAM_AVAILABLE_KEY, probe_ezstd_stream_available())
    end.

-spec cache_availability(term(), boolean()) -> boolean().
cache_availability(Key, Available) ->
    persistent_term:put(Key, Available),
    Available.

-spec probe_ezstd_available() -> boolean().
probe_ezstd_available() ->
    case code:ensure_loaded(ezstd) of
        {module, ezstd} ->
            erlang:function_exported(ezstd, compress, 2) andalso
                erlang:function_exported(ezstd, decompress, 1);
        _ ->
            false
    end.

-spec probe_ezstd_stream_available() -> boolean().
probe_ezstd_stream_available() ->
    case code:ensure_loaded(ezstd) of
        {module, ezstd} ->
            erlang:function_exported(ezstd, create_compression_context, 1) andalso
                erlang:function_exported(ezstd, set_compression_parameter, 3) andalso
                erlang:function_exported(ezstd, compress_streaming, 2) andalso
                erlang:function_exported(ezstd, create_decompression_context, 1) andalso
                erlang:function_exported(ezstd, decompress_streaming, 2) andalso
                ezstd_nif_stream_chunk_available();
        _ ->
            false
    end.

-spec ezstd_nif_stream_chunk_available() -> boolean().
ezstd_nif_stream_chunk_available() ->
    case code:ensure_loaded(ezstd_nif) of
        {module, ezstd_nif} ->
            erlang:function_exported(ezstd_nif, decompress_streaming_chunk, 3);
        _ ->
            false
    end.
