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

-module(hot_reload).

-export([
    reload_module/1,
    reload_modules/1,
    reload_modules/2,
    reload_beams/2,
    reload_all_changed/0,
    reload_all_changed/1,
    get_loaded_modules/0,
    get_module_info/1
]).

-define(CRITICAL_MODULES, [
    code,
    kernel,
    erlang,
    init,
    erl_prim_loader,
    prim_file,
    prim_inet,
    prim_zip,
    zlib,
    otp_ring0,
    erts_internal,
    erts_code_purger,
    application,
    application_controller,
    application_master,
    supervisor,
    gen_server,
    gen_event,
    gen_statem,
    proc_lib,
    error_handler,
    heart,
    logger,
    logger_handler_watcher,
    logger_server,
    logger_config,
    logger_simple_h
]).

-type purge_mode() :: none | soft | hard.
-type reload_opts() :: #{purge => purge_mode()}.
-type reload_result() :: map().

-spec reload_module(atom()) -> {ok, map()} | {error, term()}.
reload_module(Module) when is_atom(Module) ->
    case is_critical_module(Module) of
        true ->
            {error, {critical_module, Module}};
        false ->
            {ok, Result} = reload_modules([Module], #{purge => soft}),
            case Result of
                [One] ->
                    case maps:get(status, One) of
                        ok -> {ok, One};
                        error -> {error, maps:get(reason, One, unknown)}
                    end;
                _ ->
                    {error, unexpected_result}
            end
    end.

-spec reload_modules([atom()]) -> {ok, [reload_result()]}.
reload_modules(Modules) when is_list(Modules) ->
    reload_modules(Modules, #{purge => soft}).

-spec reload_modules([atom()], reload_opts()) -> {ok, [reload_result()]}.
reload_modules(Modules, Opts) when is_list(Modules), is_map(Opts) ->
    Purge = maps:get(purge, Opts, soft),
    Results = lists:map(
        fun(Module) ->
            reload_one(Module, Purge)
        end,
        Modules
    ),
    {ok, Results}.

-spec reload_beams([{atom(), binary()}], reload_opts()) -> {ok, [reload_result()]}.
reload_beams(Pairs, Opts) when is_list(Pairs), is_map(Opts) ->
    Purge = maps:get(purge, Opts, soft),
    Results =
        lists:map(
            fun({Module, BeamBin}) ->
                reload_one_beam(Module, BeamBin, Purge)
            end,
            Pairs
        ),
    {ok, Results}.

-spec reload_all_changed() -> {ok, [reload_result()]}.
reload_all_changed() ->
    reload_all_changed(soft).

-spec reload_all_changed(purge_mode()) -> {ok, [reload_result()]}.
reload_all_changed(Purge) ->
    ChangedModules = get_changed_modules(),
    reload_modules(ChangedModules, #{purge => Purge}).

-spec get_loaded_modules() -> [atom()].
get_loaded_modules() ->
    [M || {M, _} <- code:all_loaded(), is_fluxer_module(M)].

-spec get_module_info(atom()) -> {ok, map()} | {error, not_loaded}.
get_module_info(Module) when is_atom(Module) ->
    case code:is_loaded(Module) of
        false ->
            {error, not_loaded};
        {file, BeamPath} ->
            LoadedTime = get_loaded_time(Module),
            DiskTime = get_disk_time(BeamPath),
            LoadedMd5 = loaded_md5(Module),
            DiskMd5 = disk_md5(BeamPath),
            {ok, #{
                module => Module,
                beam_path => BeamPath,
                loaded_time => LoadedTime,
                disk_time => DiskTime,
                loaded_md5 => hex_or_null(LoadedMd5),
                disk_md5 => hex_or_null(DiskMd5),
                changed => (code:module_status(Module) =:= modified),
                is_critical => is_critical_module(Module)
            }}
    end.

-spec reload_one(atom(), purge_mode()) -> reload_result().
reload_one(Module, Purge) ->
    case is_critical_module(Module) of
        true ->
            #{module => Module, status => error, reason => {critical_module, Module}};
        false ->
            do_reload_one(Module, Purge)
    end.

-spec reload_one_beam(atom(), binary(), purge_mode()) -> reload_result().
reload_one_beam(Module, BeamBin, Purge) ->
    case is_critical_module(Module) of
        true ->
            #{module => Module, status => error, reason => {critical_module, Module}};
        false ->
            do_reload_one_beam(Module, BeamBin, Purge)
    end.

-spec do_reload_one(atom(), purge_mode()) -> reload_result().
do_reload_one(Module, Purge) ->
    OldLoadedMd5 = loaded_md5(Module),
    OldBeamPath = code:which(Module),
    OldDiskMd5 = disk_md5(OldBeamPath),
    ok = maybe_purge_before_load(Module, Purge),
    case code:load_file(Module) of
        {module, Module} ->
            NewLoadedMd5 = loaded_md5(Module),
            NewBeamPath = code:which(Module),
            NewDiskMd5 = disk_md5(NewBeamPath),
            Verified =
                (NewLoadedMd5 =/= undefined) andalso
                    (NewDiskMd5 =/= undefined) andalso
                    (NewLoadedMd5 =:= NewDiskMd5),
            {PurgedOld, LingeringCount} = maybe_purge_old_after_load(Module, Purge),
            #{
                module => Module,
                status => ok,
                old_loaded_md5 => hex_or_null(OldLoadedMd5),
                old_disk_md5 => hex_or_null(OldDiskMd5),
                new_loaded_md5 => hex_or_null(NewLoadedMd5),
                new_disk_md5 => hex_or_null(NewDiskMd5),
                verified => Verified,
                purged_old_code => PurgedOld,
                lingering_count => LingeringCount
            };
        {error, Reason} ->
            #{
                module => Module,
                status => error,
                reason => Reason,
                old_loaded_md5 => hex_or_null(OldLoadedMd5),
                old_disk_md5 => hex_or_null(OldDiskMd5),
                verified => false,
                purged_old_code => false,
                lingering_count => 0
            }
    end.

-spec do_reload_one_beam(atom(), binary(), purge_mode()) -> reload_result().
do_reload_one_beam(Module, BeamBin, Purge) ->
    OldLoadedMd5 = loaded_md5(Module),
    ExpectedMd5 =
        case beam_lib:md5(BeamBin) of
            {ok, {Module, Md5}} ->
                Md5;
            {ok, {Other, _}} ->
                erlang:error({beam_module_mismatch, Module, Other});
            _ ->
                erlang:error(invalid_beam)
        end,
    ok = maybe_purge_before_load(Module, Purge),
    Filename = atom_to_list(Module) ++ ".beam(hot)",
    case code:load_binary(Module, Filename, BeamBin) of
        {module, Module} ->
            NewLoadedMd5 = loaded_md5(Module),
            Verified = (NewLoadedMd5 =:= ExpectedMd5),
            {PurgedOld, LingeringCount} = maybe_purge_old_after_load(Module, Purge),
            #{
                module => Module,
                status => ok,
                old_loaded_md5 => hex_or_null(OldLoadedMd5),
                expected_md5 => hex_or_null(ExpectedMd5),
                new_loaded_md5 => hex_or_null(NewLoadedMd5),
                verified => Verified,
                purged_old_code => PurgedOld,
                lingering_count => LingeringCount
            };
        {error, Reason} ->
            #{
                module => Module,
                status => error,
                reason => Reason,
                old_loaded_md5 => hex_or_null(OldLoadedMd5),
                expected_md5 => hex_or_null(ExpectedMd5),
                verified => false,
                purged_old_code => false,
                lingering_count => 0
            }
    end.

-spec maybe_purge_before_load(atom(), purge_mode()) -> ok.
maybe_purge_before_load(_Module, none) ->
    ok;
maybe_purge_before_load(_Module, soft) ->
    ok;
maybe_purge_before_load(Module, hard) ->
    _ = code:purge(Module),
    ok.

-spec maybe_purge_old_after_load(atom(), purge_mode()) -> {boolean(), non_neg_integer()}.
maybe_purge_old_after_load(_Module, none) ->
    {false, 0};
maybe_purge_old_after_load(Module, hard) ->
    _ = code:soft_purge(Module),
    Purged = code:purge(Module),
    LingeringCount =
        case Purged of
            true -> 0;
            false -> count_lingering(Module)
        end,
    {Purged, LingeringCount};
maybe_purge_old_after_load(Module, soft) ->
    Purged = wait_soft_purge(Module, 40, 50),
    LingeringCount =
        case Purged of
            true -> 0;
            false -> count_lingering(Module)
        end,
    {Purged, LingeringCount}.

-spec wait_soft_purge(atom(), non_neg_integer(), pos_integer()) -> boolean().
wait_soft_purge(_Module, 0, _SleepMs) ->
    false;
wait_soft_purge(Module, N, SleepMs) ->
    case code:soft_purge(Module) of
        true ->
            true;
        false ->
            receive
            after SleepMs -> ok
            end,
            wait_soft_purge(Module, N - 1, SleepMs)
    end.

-spec count_lingering(atom()) -> non_neg_integer().
count_lingering(Module) ->
    lists:foldl(
        fun(Pid, Acc) ->
            case erlang:check_process_code(Pid, Module) of
                true -> Acc + 1;
                false -> Acc
            end
        end,
        0,
        processes()
    ).

-spec get_changed_modules() -> [atom()].
get_changed_modules() ->
    Modified = code:modified_modules(),
    [M || M <- Modified, is_fluxer_module(M), not is_critical_module(M)].

-spec is_critical_module(atom()) -> boolean().
is_critical_module(Module) ->
    lists:member(Module, ?CRITICAL_MODULES).

-spec is_fluxer_module(atom()) -> boolean().
is_fluxer_module(Module) ->
    ModuleStr = atom_to_list(Module),
    lists:prefix("fluxer_", ModuleStr) orelse
        lists:prefix("gateway", ModuleStr) orelse
        lists:prefix("gateway_http_", ModuleStr) orelse
        lists:prefix("session", ModuleStr) orelse
        lists:prefix("guild", ModuleStr) orelse
        lists:prefix("passive_sync_registry", ModuleStr) orelse
        lists:prefix("presence", ModuleStr) orelse
        lists:prefix("push", ModuleStr) orelse
        lists:prefix("push_dispatcher", ModuleStr) orelse
        lists:prefix("call", ModuleStr) orelse
        lists:prefix("health", ModuleStr) orelse
        lists:prefix("hot_reload", ModuleStr) orelse
        lists:prefix("rpc_client", ModuleStr) orelse
        lists:prefix("rendezvous", ModuleStr) orelse
        lists:prefix("process_", ModuleStr) orelse
        lists:prefix("metrics_", ModuleStr) orelse
        lists:prefix("dm_voice", ModuleStr) orelse
        lists:prefix("voice_", ModuleStr) orelse
        lists:prefix("constants", ModuleStr) orelse
        lists:prefix("validation", ModuleStr) orelse
        lists:prefix("backoff_", ModuleStr) orelse
        lists:prefix("list_ops", ModuleStr) orelse
        lists:prefix("map_utils", ModuleStr) orelse
        lists:prefix("type_conv", ModuleStr) orelse
        lists:prefix("utils", ModuleStr) orelse
        lists:prefix("snowflake_", ModuleStr) orelse
        lists:prefix("user_utils", ModuleStr) orelse
        lists:prefix("custom_status", ModuleStr) orelse
        lists:prefix("otel_", ModuleStr) orelse
        lists:prefix("event_", ModuleStr).

-spec loaded_md5(atom()) -> binary() | undefined.
loaded_md5(Module) ->
    try
        Module:module_info(md5)
    catch
        _:_ -> undefined
    end.

-spec disk_md5(string() | atom()) -> binary() | undefined.
disk_md5(Path) when is_list(Path) ->
    case beam_lib:md5(Path) of
        {ok, {_M, Md5}} -> Md5;
        _ -> undefined
    end;
disk_md5(_) ->
    undefined.

-spec hex_or_null(binary() | undefined) -> binary() | null.
hex_or_null(undefined) ->
    null;
hex_or_null(Bin) when is_binary(Bin) ->
    binary:encode_hex(Bin, lowercase).

-spec get_loaded_time(atom()) -> term().
get_loaded_time(Module) ->
    try
        case Module:module_info(compile) of
            CompileInfo when is_list(CompileInfo) ->
                proplists:get_value(time, CompileInfo, undefined);
            _ ->
                undefined
        end
    catch
        _:_ -> undefined
    end.

-spec get_disk_time(string() | atom()) -> calendar:datetime() | undefined.
get_disk_time(BeamPath) when is_list(BeamPath) ->
    case file:read_file_info(BeamPath) of
        {ok, FileInfo} ->
            element(6, FileInfo);
        _ ->
            undefined
    end;
get_disk_time(_) ->
    undefined.
