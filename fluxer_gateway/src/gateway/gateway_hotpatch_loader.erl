%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_hotpatch_loader).
-typing([eqwalizer]).

-export([
    apply_bundle/1,
    current_md5/1,
    beam_module/1,
    beam_md5/1,
    hex/1
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type module_entry() :: map().
-type apply_result() :: {ok, applied | skipped, atom(), binary()} | {error, term()}.

-spec apply_bundle(map()) -> {ok, map()} | {error, term()}.
apply_bundle(Bundle) when is_map(Bundle) ->
    case
        {
            entry_value(version, Bundle),
            entry_value(build_sha, Bundle),
            entry_value(modules, Bundle)
        }
    of
        {1, BundleBuildSha, Modules} when is_list(Modules) ->
            apply_versioned_bundle(BundleBuildSha, Modules);
        _ ->
            {error, invalid_bundle}
    end;
apply_bundle(_Bundle) ->
    {error, invalid_bundle}.

-spec apply_versioned_bundle(term(), [module_entry()]) -> {ok, map()} | {error, term()}.
apply_versioned_bundle(BundleBuildSha, Modules) ->
    CurrentBuildSha = gateway_hotpatch_runtime:build_sha(),
    case normalize_binary(BundleBuildSha) of
        CurrentBuildSha -> apply_modules(Modules, #{applied => [], skipped => []});
        Other -> {error, {build_sha_mismatch, Other, CurrentBuildSha}}
    end.

-spec current_md5(atom()) -> {ok, binary()} | {error, term()}.
current_md5(Module) when is_atom(Module) ->
    case hotpatch_loaded_md5(Module) of
        {ok, Md5} -> {ok, Md5};
        error -> current_md5_result(Module, current_md5_from_loaded_or_file(Module))
    end.

-spec current_md5_result(atom(), {ok, binary()} | {error, term()}) ->
    {ok, binary()} | {error, term()}.
current_md5_result(_Module, {ok, Md5}) ->
    {ok, Md5};
current_md5_result(Module, {error, Reason}) ->
    fallback_current_md5(Module, Reason).

-spec fallback_current_md5(atom(), term()) -> {ok, binary()} | {error, term()}.
fallback_current_md5(Module, Reason) ->
    case hotpatch_loaded_md5(Module) of
        {ok, Md5} -> {ok, Md5};
        error -> {error, Reason}
    end.

-spec current_md5_from_loaded_or_file(atom()) -> {ok, binary()} | {error, term()}.
current_md5_from_loaded_or_file(Module) ->
    case code:get_object_code(Module) of
        {Module, Beam, _File} when is_binary(Beam) -> beam_md5(Beam);
        error -> current_md5_from_file(Module)
    end.

-spec beam_module(binary()) -> {ok, atom()} | {error, term()}.
beam_module(Beam) when is_binary(Beam) ->
    case beam_lib:info(Beam) of
        Info when is_list(Info) -> beam_module_from_info(Info);
        Other -> {error, {beam_info_failed, Other}}
    end.

-spec beam_module_from_info(list()) -> {ok, atom()} | {error, term()}.
beam_module_from_info(Info) ->
    case lists:keyfind(module, 1, Info) of
        {module, Module} when is_atom(Module) -> {ok, Module};
        _ -> {error, module_not_found}
    end.

-spec beam_md5(binary()) -> {ok, binary()} | {error, term()}.
beam_md5(Beam) when is_binary(Beam) ->
    case beam_lib:md5(Beam) of
        {ok, {_Module, Md5}} when is_binary(Md5) -> {ok, Md5};
        Error -> {error, {beam_md5_failed, Error}}
    end.

-spec hex(binary()) -> binary().
hex(Binary) when is_binary(Binary) ->
    iolist_to_binary([[hex_nibble(High), hex_nibble(Low)] || <<High:4, Low:4>> <= Binary]).

-spec apply_modules([module_entry()], map()) -> {ok, map()} | {error, term()}.
apply_modules([], Acc) ->
    {ok, Acc#{module_count => length(maps:get(applied, Acc)) + length(maps:get(skipped, Acc))}};
apply_modules([Entry | Rest], Acc) ->
    case apply_module(Entry) of
        {ok, applied, Module, TargetMd5} ->
            apply_modules(
                Rest,
                Acc#{applied => [{Module, hex(TargetMd5)} | maps:get(applied, Acc)]}
            );
        {ok, skipped, Module, TargetMd5} ->
            apply_modules(
                Rest,
                Acc#{skipped => [{Module, hex(TargetMd5)} | maps:get(skipped, Acc)]}
            );
        {error, Reason} ->
            {error, Reason}
    end.

-spec apply_module(module_entry()) -> apply_result().
apply_module(Entry) ->
    with_entry(Entry, fun apply_valid_entry/4).

-spec apply_valid_entry(atom(), binary(), binary(), binary()) -> apply_result().
apply_valid_entry(Module, ExpectedMd5, TargetMd5, Beam) ->
    case current_md5(Module) of
        {ok, TargetMd5} ->
            {ok, skipped, Module, TargetMd5};
        {ok, ExpectedMd5} ->
            load_module(Module, Beam, TargetMd5);
        {ok, CurrentMd5} ->
            {error, {md5_mismatch, Module, hex(CurrentMd5), hex(ExpectedMd5)}};
        {error, Reason} ->
            {error, {current_md5_failed, Module, Reason}}
    end.

-spec with_entry(module_entry(), fun((atom(), binary(), binary(), binary()) -> apply_result())) ->
    apply_result().
with_entry(Entry, Fun) when is_map(Entry), is_function(Fun, 4) ->
    case normalize_module(entry_value(module, Entry)) of
        {ok, Module} ->
            ExpectedMd5 = entry_value(expected_current_md5, Entry),
            TargetMd5 = entry_value(target_md5, Entry),
            BeamZstd = entry_value(beam_zstd, Entry),
            with_entry_beam(Module, ExpectedMd5, TargetMd5, BeamZstd, Fun);
        {error, Reason} ->
            {error, Reason}
    end;
with_entry(_Entry, _Fun) ->
    {error, invalid_module_entry}.

-spec with_entry_beam(atom(), term(), term(), term(), fun(
    (atom(), binary(), binary(), binary()) -> apply_result()
)) ->
    apply_result().
with_entry_beam(Module, ExpectedMd5, TargetMd5, BeamZstd, Fun) when
    is_binary(ExpectedMd5),
    is_binary(TargetMd5),
    byte_size(ExpectedMd5) =:= 16,
    byte_size(TargetMd5) =:= 16,
    is_binary(BeamZstd)
->
    case decompress_beam(BeamZstd) of
        {ok, Beam} -> validate_beam(Module, ExpectedMd5, TargetMd5, Beam, Fun);
        {error, Reason} -> {error, {beam_decompress_failed, Module, Reason}}
    end;
with_entry_beam(Module, _ExpectedMd5, _TargetMd5, _BeamZstd, _Fun) ->
    {error, {invalid_module_entry, Module}}.

-spec validate_beam(atom(), binary(), binary(), binary(), fun(
    (atom(), binary(), binary(), binary()) -> apply_result()
)) ->
    apply_result().
validate_beam(Module, ExpectedMd5, TargetMd5, Beam, Fun) ->
    case beam_module(Beam) of
        {ok, Module} -> validate_beam_md5(Module, ExpectedMd5, TargetMd5, Beam, Fun);
        {ok, OtherModule} -> {error, {beam_module_mismatch, Module, OtherModule}};
        {error, Reason} -> {error, {beam_module_failed, Module, Reason}}
    end.

-spec validate_beam_md5(atom(), binary(), binary(), binary(), fun(
    (atom(), binary(), binary(), binary()) -> apply_result()
)) ->
    apply_result().
validate_beam_md5(Module, ExpectedMd5, TargetMd5, Beam, Fun) ->
    case beam_md5(Beam) of
        {ok, TargetMd5} ->
            Fun(Module, ExpectedMd5, TargetMd5, Beam);
        {ok, OtherMd5} ->
            {error, {target_md5_mismatch, Module, hex(OtherMd5), hex(TargetMd5)}};
        {error, Reason} ->
            {error, {target_md5_failed, Module, Reason}}
    end.

-spec load_module(atom(), binary(), binary()) -> apply_result().
load_module(Module, Beam, TargetMd5) ->
    case code:soft_purge(Module) of
        true -> load_purged_module(Module, Beam, TargetMd5);
        false -> {error, {soft_purge_failed, Module}}
    end.

-spec load_purged_module(atom(), binary(), binary()) -> apply_result().
load_purged_module(Module, Beam, TargetMd5) ->
    case code:load_binary(Module, atom_to_list(Module) ++ ".beam", Beam) of
        {module, Module} ->
            put_hotpatch_loaded_md5(Module, TargetMd5),
            verify_loaded_module(Module, TargetMd5);
        {error, Reason} ->
            {error, {load_binary_failed, Module, Reason}}
    end.

-spec verify_loaded_module(atom(), binary()) -> apply_result().
verify_loaded_module(Module, TargetMd5) ->
    case current_md5(Module) of
        {ok, TargetMd5} ->
            {ok, applied, Module, TargetMd5};
        {ok, OtherMd5} ->
            {error, {post_load_md5_mismatch, Module, hex(OtherMd5), hex(TargetMd5)}};
        {error, Reason} ->
            {error, {post_load_md5_failed, Module, Reason}}
    end.

-spec current_md5_from_file(atom()) -> {ok, binary()} | {error, term()}.
current_md5_from_file(Module) ->
    case code:which(Module) of
        File when is_list(File) -> current_md5_from_path(Module, File);
        preloaded -> {error, preloaded};
        non_existing -> {error, not_loaded};
        Other -> {error, {not_loadable, Other}}
    end.

-spec current_md5_from_path(atom(), file:filename()) -> {ok, binary()} | {error, term()}.
current_md5_from_path(Module, File) ->
    case beam_lib:md5(File) of
        {ok, {Module, Md5}} when is_binary(Md5) -> {ok, Md5};
        Error -> {error, {beam_file_md5_failed, Error}}
    end.

-spec hotpatch_loaded_md5(atom()) -> {ok, binary()} | error.
hotpatch_loaded_md5(Module) ->
    case persistent_term:get({?MODULE, loaded_md5, Module}, undefined) of
        Md5 when is_binary(Md5), byte_size(Md5) =:= 16 -> {ok, Md5};
        _ -> error
    end.

-spec put_hotpatch_loaded_md5(atom(), binary()) -> ok.
put_hotpatch_loaded_md5(Module, Md5) ->
    persistent_term:put({?MODULE, loaded_md5, Module}, Md5).

-spec decompress_beam(binary()) -> {ok, binary()} | {error, term()}.
decompress_beam(Compressed) ->
    try erlang:apply(ezstd, decompress, [Compressed]) of
        Beam when is_binary(Beam) -> {ok, Beam};
        Beam when is_list(Beam) -> {ok, iolist_to_binary(Beam)};
        {error, Reason} -> {error, Reason};
        Other -> {error, Other}
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

-spec normalize_module(term()) -> {ok, atom()} | {error, term()}.
normalize_module(Module) when is_atom(Module) ->
    {ok, Module};
normalize_module(Module) when is_binary(Module) ->
    try
        {ok, binary_to_existing_atom(Module, utf8)}
    catch
        error:badarg -> {error, {unknown_module, Module}}
    end;
normalize_module(Module) when is_list(Module) ->
    normalize_module(type_conv:ensure_binary(Module));
normalize_module(Module) ->
    {error, {invalid_module, Module}}.

-spec normalize_binary(term()) -> binary().
normalize_binary(Bin) when is_binary(Bin) -> Bin;
normalize_binary(List) when is_list(List) -> type_conv:ensure_binary(List);
normalize_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
normalize_binary(Other) -> term_to_binary(Other).

-spec entry_value(atom(), map()) -> term().
entry_value(Key, Entry) ->
    maps:get(Key, Entry, maps:get(atom_to_binary(Key, utf8), Entry, undefined)).

-spec hex_nibble(0..15) -> integer().
hex_nibble(N) when N < 10 -> $0 + N;
hex_nibble(N) -> $a + (N - 10).

-ifdef(TEST).

beam_module_and_md5_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    {?MODULE, Binary, _File} = code:get_object_code(?MODULE),
    ?assertEqual({ok, ?MODULE}, beam_module(Binary)),
    {ok, Md5} = beam_md5(Binary),
    ?assertEqual(16, byte_size(Md5)).

apply_bundle_rejects_build_mismatch_test() ->
    Bundle = #{version => 1, build_sha => <<"definitely-not-this-build">>, modules => []},
    ?assertMatch({error, {build_sha_mismatch, _, _}}, apply_bundle(Bundle)).

apply_bundle_accepts_binary_keys_test() ->
    Bundle = #{
        <<"version">> => 1,
        <<"build_sha">> => gateway_hotpatch_runtime:build_sha(),
        <<"modules">> => []
    },
    ?assertEqual(
        {ok, #{applied => [], skipped => [], module_count => 0}}, apply_bundle(Bundle)
    ).

apply_bundle_loads_new_beam_test() ->
    Module = gateway_hotpatch_loader_test_target,
    cleanup_test_module(Module),
    try
        Beam1 = compile_test_module(Module, 1),
        Beam2 = compile_test_module(Module, 2),
        {module, Module} = load_test_beam(Module, Beam1),
        ?assertEqual(1, erlang:apply(Module, version, [])),
        {ok, ExpectedMd5} = current_md5(Module),
        {Entry, TargetMd5} = beam_entry(Module, ExpectedMd5, Beam2),
        Bundle = #{
            version => 1,
            build_sha => gateway_hotpatch_runtime:build_sha(),
            modules => [Entry]
        },
        ?assertEqual(
            {ok, #{applied => [{Module, hex(TargetMd5)}], skipped => [], module_count => 1}},
            apply_bundle(Bundle)
        ),
        ?assertEqual(2, erlang:apply(Module, version, []))
    after
        cleanup_test_module(Module)
    end.

apply_bundle_leaves_prior_module_loaded_when_later_entry_fails_test() ->
    ModuleA = gateway_hotpatch_loader_test_partial_a,
    ModuleB = gateway_hotpatch_loader_test_partial_b,
    cleanup_test_module(ModuleA),
    cleanup_test_module(ModuleB),
    try
        BeamA1 = compile_test_module(ModuleA, 1),
        BeamA2 = compile_test_module(ModuleA, 2),
        BeamB1 = compile_test_module(ModuleB, 1),
        BeamB2 = compile_test_module(ModuleB, 2),
        {module, ModuleA} = load_test_beam(ModuleA, BeamA1),
        {module, ModuleB} = load_test_beam(ModuleB, BeamB1),
        {ok, Md5A1} = current_md5(ModuleA),
        WrongExpectedMd5 = <<0:128>>,
        {EntryA, _Md5A2} = beam_entry(ModuleA, Md5A1, BeamA2),
        {EntryB, _Md5B2} = beam_entry(ModuleB, WrongExpectedMd5, BeamB2),
        Bundle = #{
            version => 1,
            build_sha => gateway_hotpatch_runtime:build_sha(),
            modules => [EntryA, EntryB]
        },
        ?assertMatch({error, {md5_mismatch, ModuleB, _, _}}, apply_bundle(Bundle)),
        ?assertEqual(2, erlang:apply(ModuleA, version, [])),
        ?assertEqual(1, erlang:apply(ModuleB, version, []))
    after
        cleanup_test_module(ModuleA),
        cleanup_test_module(ModuleB)
    end.

hex_test() ->
    ?assertEqual(<<"0001020f10ff">>, hex(<<0, 1, 2, 15, 16, 255>>)).

compile_test_module(Module, Version) ->
    Dir = test_compile_dir(),
    Source = filename:join(Dir, atom_to_list(Module) ++ ".erl"),
    SourceText = io_lib:format(
        "-module(~p).~n-export([version/0]).~nversion() -> ~p.~n",
        [Module, Version]
    ),
    ok = file:write_file(Source, SourceText),
    case compile:file(Source, [binary, return_errors, return_warnings]) of
        {ok, Module, Beam} -> Beam;
        {ok, Module, Beam, _Warnings} -> Beam;
        Error -> erlang:error({test_module_compile_failed, Module, Error})
    end.

test_compile_dir() ->
    Root =
        case os:getenv("TMPDIR") of
            false -> "/tmp";
            Value -> Value
        end,
    Dir = filename:join(Root, "fluxer_hotpatch_loader_tests"),
    ok = filelib:ensure_dir(filename:join(Dir, "placeholder")),
    Dir.

load_test_beam(Module, Beam) ->
    cleanup_test_module(Module),
    BeamPath = filename:join(test_compile_dir(), atom_to_list(Module) ++ ".beam"),
    ok = file:write_file(BeamPath, Beam),
    code:load_abs(filename:rootname(BeamPath)).

beam_entry(Module, ExpectedMd5, TargetBeam) ->
    {ok, TargetMd5} = beam_md5(TargetBeam),
    {ok, BeamZstd} = compress_test_beam(TargetBeam),
    {
        #{
            module => Module,
            expected_current_md5 => ExpectedMd5,
            target_md5 => TargetMd5,
            beam_zstd => BeamZstd
        },
        TargetMd5
    }.

compress_test_beam(Beam) ->
    try erlang:apply(ezstd, compress, [Beam, 3]) of
        Compressed when is_binary(Compressed) -> {ok, Compressed};
        {error, Reason} -> {error, Reason};
        Other -> {error, Other}
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

cleanup_test_module(Module) ->
    _ = code:soft_purge(Module),
    _ = code:purge(Module),
    _ = code:delete(Module),
    _ = code:purge(Module),
    erase_hotpatch_loaded_md5(Module),
    _ = file:delete(filename:join(test_compile_dir(), atom_to_list(Module) ++ ".erl")),
    _ = file:delete(filename:join(test_compile_dir(), atom_to_list(Module) ++ ".beam")),
    ok.

-spec erase_hotpatch_loaded_md5(atom()) -> ok.
erase_hotpatch_loaded_md5(Module) ->
    _ = persistent_term:erase({?MODULE, loaded_md5, Module}),
    ok.

-endif.
