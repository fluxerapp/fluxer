%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_compress).
-typing([eqwalizer]).

-export([
    init/0,
    new_context/1,
    compress/2,
    decompress/2,
    parse_compression/1,
    parse_compression/2,
    close_context/1,
    get_type/1
]).

-export_type([compression/0, compress_ctx/0]).

-define(MAX_DECOMPRESSED_SIZE, 10 * 1024 * 1024).
-define(ZSTD_STREAM_BUFFER_SIZE, 64 * 1024).
-define(ZSTD_COMPRESSION_LEVEL, 3).
-define(ZSTD_DECOMPRESS_WINDOW_LOG_MAX, 23).
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
decompress(Data, #{type := none} = Ctx) ->
    {ok, Data, Ctx};
decompress(Data, #{type := zstd_frame} = Ctx) ->
    zstd_frame_decompress(Data, Ctx);
decompress(Data, #{type := zstd_stream, stream_ctx := _} = Ctx) ->
    zstd_stream_decompress(Data, Ctx);
decompress(Data, #{type := zstd_stream} = Ctx) ->
    zstd_frame_decompress(Data, Ctx).

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
            set_zstd_stream_compression_level(StreamCtx, Ctx);
        {error, Reason} ->
            {error, {compress_failed, Reason}};
        Other ->
            {error, {compress_failed, {case_clause, Other}}}
    end.

-spec set_zstd_stream_compression_level(reference(), zstd_stream_ctx()) ->
    {ok, reference(), zstd_stream_ctx()} | {error, term()}.
set_zstd_stream_compression_level(StreamCtx, Ctx) ->
    Result = erlang:apply(ezstd, set_compression_parameter, [
        StreamCtx, zstd_c_compression_level, ?ZSTD_COMPRESSION_LEVEL
    ]),
    case Result of
        ok -> {ok, StreamCtx, Ctx#{stream_ctx => StreamCtx}};
        {error, Reason} -> {error, {compress_failed, Reason}};
        Other -> {error, {compress_failed, {case_clause, Other}}}
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

-spec zstd_frame_decompress(binary(), compress_ctx()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
zstd_frame_decompress(Data, Ctx) ->
    case ezstd_available() of
        true -> zstd_frame_decompress_available(Data, Ctx);
        false -> {error, {decompress_failed, zstd_not_available}}
    end.

-spec zstd_frame_decompress_available(binary(), compress_ctx()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
zstd_frame_decompress_available(Data, Ctx) ->
    try
        handle_zstd_decompress_result(erlang:apply(ezstd, decompress, [Data]), Ctx)
    catch
        _:Exception ->
            {error, {decompress_failed, Exception}}
    end.

-spec handle_zstd_decompress_result(term(), compress_ctx()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
handle_zstd_decompress_result(Decompressed, Ctx) when is_binary(Decompressed) ->
    check_decompressed_size(Decompressed, Ctx);
handle_zstd_decompress_result(Decompressed, Ctx) when is_list(Decompressed) ->
    IoList = eqwalizer:dynamic_cast(Decompressed),
    case erlang:iolist_size(IoList) > ?MAX_DECOMPRESSED_SIZE of
        true -> {error, decompression_too_large};
        false -> check_decompressed_size(iolist_to_binary(IoList), Ctx)
    end;
handle_zstd_decompress_result({error, Reason}, _Ctx) ->
    {error, {decompress_failed, Reason}};
handle_zstd_decompress_result(Other, _Ctx) ->
    {error, {decompress_failed, {case_clause, Other}}}.

-spec zstd_stream_decompress(binary(), compress_ctx()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
zstd_stream_decompress(Data, Ctx) ->
    case ezstd_stream_available() of
        true -> zstd_stream_decompress_available(Data, Ctx);
        false -> {error, {decompress_failed, zstd_stream_not_available}}
    end.

-spec zstd_stream_decompress_available(binary(), compress_ctx()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
zstd_stream_decompress_available(Data, Ctx) ->
    try
        case ensure_zstd_decompress_stream_context(Ctx) of
            {ok, StreamCtx, NewCtx} ->
                handle_zstd_decompress_result(
                    erlang:apply(ezstd, decompress_streaming, [StreamCtx, Data]), NewCtx
                );
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:Exception ->
            {error, {decompress_failed, Exception}}
    end.

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

-spec set_zstd_decompress_window_log_max(reference(), compress_ctx()) ->
    {ok, reference(), compress_ctx()} | {error, term()}.
set_zstd_decompress_window_log_max(StreamCtx, Ctx) ->
    Result = erlang:apply(ezstd, set_decompression_parameter, [
        StreamCtx, zstd_d_window_log_max, ?ZSTD_DECOMPRESS_WINDOW_LOG_MAX
    ]),
    case Result of
        ok -> {ok, StreamCtx, Ctx#{decompress_stream_ctx => StreamCtx}};
        {error, Reason} -> {error, {decompress_failed, Reason}};
        Other -> {error, {decompress_failed, {case_clause, Other}}}
    end.

-spec check_decompressed_size(binary(), compress_ctx()) ->
    {ok, binary(), compress_ctx()} | {error, term()}.
check_decompressed_size(Decompressed, Ctx) ->
    case byte_size(Decompressed) > ?MAX_DECOMPRESSED_SIZE of
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
                erlang:function_exported(ezstd, decompress_streaming, 2);
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
        ?_assertEqual(none, parse_compression(<<"zstd-stream">>)),
        ?_assertEqual(none, parse_compression(<<"zstd-stream">>, undefined)),
        ?_assertEqual(none, parse_compression(<<"zstd-stream">>, <<"0">>)),
        ?_assertEqual(zstd_stream, parse_compression(<<"zstd-stream">>, <<"1">>)),
        ?_assertEqual(zstd_stream, parse_compression(<<"zstd-stream">>, <<"true">>))
    ].

legacy_zstd_stream_ctx_routes_to_frame_test() ->
    case ezstd_available() of
        true ->
            Legacy = #{type => zstd_stream},
            Data = <<"hello world, legacy one-shot frame payload">>,
            {ok, Compressed, Ctx2} = compress(Data, Legacy),
            ?assert(is_binary(Compressed)),
            ?assertEqual(Legacy, Ctx2),
            {ok, Decompressed, _} = decompress(Compressed, Legacy),
            ?assertEqual(Data, Decompressed);
        false ->
            ?assertEqual(skip, skip)
    end.

new_context_test_() ->
    [
        ?_assertEqual(none, get_type(new_context(none))),
        ?_assertEqual(zstd_frame, get_type(new_context(zstd_frame))),
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

check_decompressed_size_allows_normal_payload_test() ->
    Ctx = new_context(zstd_frame),
    Data = <<"normal payload">>,
    ?assertEqual({ok, Data, Ctx}, check_decompressed_size(Data, Ctx)).

check_decompressed_size_rejects_oversized_payload_test() ->
    Ctx = new_context(zstd_frame),
    Oversized = binary:copy(<<0>>, ?MAX_DECOMPRESSED_SIZE + 1),
    ?assertEqual({error, decompression_too_large}, check_decompressed_size(Oversized, Ctx)).

check_decompressed_size_allows_exact_limit_test() ->
    Ctx = new_context(zstd_frame),
    ExactLimit = binary:copy(<<0>>, ?MAX_DECOMPRESSED_SIZE),
    ?assertMatch({ok, _, _}, check_decompressed_size(ExactLimit, Ctx)).

init_caches_availability_test() ->
    with_saved_availability(fun() ->
        erase_cached_availability(),
        ok = init(),
        ?assertEqual(probe_ezstd_available(), persistent_term:get(?ZSTD_AVAILABLE_KEY)),
        ?assertEqual(
            probe_ezstd_stream_available(), persistent_term:get(?ZSTD_STREAM_AVAILABLE_KEY)
        )
    end).

availability_falls_back_to_probe_when_uncached_test() ->
    with_saved_availability(fun() ->
        erase_cached_availability(),
        ?assertEqual(probe_ezstd_available(), ezstd_available()),
        ?assertEqual(probe_ezstd_stream_available(), ezstd_stream_available()),
        ?assertEqual(probe_ezstd_available(), persistent_term:get(?ZSTD_AVAILABLE_KEY)),
        ?assertEqual(
            probe_ezstd_stream_available(), persistent_term:get(?ZSTD_STREAM_AVAILABLE_KEY)
        )
    end).

cached_availability_false_rejects_zstd_test() ->
    with_cached_availability(false, fun assert_zstd_unavailable/0).

cached_availability_true_round_trips_test() ->
    case probe_ezstd_stream_available() of
        true -> with_cached_availability(true, fun assert_zstd_round_trip/0);
        false -> ?assertEqual(skip, skip)
    end.

assert_zstd_unavailable() ->
    Data = <<"cached availability payload">>,
    NoneCtx = new_context(none),
    ?assertEqual({ok, Data, NoneCtx}, compress(Data, NoneCtx)),
    ?assertEqual({ok, Data, NoneCtx}, decompress(Data, NoneCtx)),
    FrameCtx = new_context(zstd_frame),
    ?assertEqual({error, {compress_failed, zstd_not_available}}, compress(Data, FrameCtx)),
    ?assertEqual({error, {decompress_failed, zstd_not_available}}, decompress(Data, FrameCtx)),
    StreamCtx = new_context(zstd_stream),
    ?assertEqual(
        {error, {compress_failed, zstd_stream_not_available}}, compress(Data, StreamCtx)
    ),
    ?assertEqual(
        {error, {decompress_failed, zstd_stream_not_available}}, decompress(Data, StreamCtx)
    ).

assert_zstd_round_trip() ->
    Data = <<"cached availability round trip payload">>,
    FrameCtx = new_context(zstd_frame),
    {ok, Frame, FrameCtx2} = compress(Data, FrameCtx),
    ?assertMatch({ok, Data, _}, decompress(Frame, FrameCtx2)),
    {ok, Streamed, _} = compress(Data, new_context(zstd_stream)),
    ?assertMatch({ok, Data, _}, decompress(Streamed, new_context(zstd_stream))).

availability_keys() ->
    [?ZSTD_AVAILABLE_KEY, ?ZSTD_STREAM_AVAILABLE_KEY].

with_saved_availability(Fun) ->
    Previous = [{Key, persistent_term:get(Key, undefined)} || Key <- availability_keys()],
    try
        Fun()
    after
        lists:foreach(fun({Key, Value}) -> restore_availability(Key, Value) end, Previous)
    end.

with_cached_availability(Available, Fun) ->
    with_saved_availability(fun() ->
        put_cached_availability(Available),
        Fun()
    end).

put_cached_availability(Available) ->
    lists:foreach(fun(Key) -> cache_availability(Key, Available) end, availability_keys()).

erase_cached_availability() ->
    lists:foreach(fun(Key) -> restore_availability(Key, undefined) end, availability_keys()).

restore_availability(Key, undefined) ->
    persistent_term:erase(Key);
restore_availability(Key, Value) ->
    persistent_term:put(Key, Value).

-ifdef(DEV_MODE).
zstd_roundtrip_test() ->
    ?assertEqual(skip, skip).

zstd_compression_ratio_test() ->
    ?assertEqual(skip, skip).
-else.
zstd_roundtrip_test() ->
    case ezstd_available() of
        true ->
            Ctx = new_context(zstd_frame),
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
            Ctx = new_context(zstd_frame),
            Data = binary:copy(<<"aaaaaaaaaa">>, 100),
            {ok, Compressed, _} = compress(Data, Ctx),
            ?assert(byte_size(Compressed) < byte_size(Data));
        false ->
            ?assertEqual(skip, skip)
    end.
-endif.

-endif.
