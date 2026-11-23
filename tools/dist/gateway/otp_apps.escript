#!/usr/bin/env escript
%% SPDX-License-Identifier: AGPL-3.0-or-later

main([Mode, Packages]) when Mode =:= "apps"; Mode =:= "modules" ->
    Lib = code:lib_dir() ++ "/",
    Beams = filelib:wildcard(filename:join(Packages, "*/ebin/*.beam")),
    Beams =/= [] orelse fail("no beam files in ~s", [Packages]),
    Otp = [M || M <- imports(Beams), owner(Lib, M) =/= []],
    case Mode of
        "modules" ->
            Own = [filename:basename(B, ".beam") || B <- Beams],
            [io:format("~s~n", [M]) || M <- lists:sort(Own ++ [atom_to_list(M) || M <- Otp])];
        "apps" ->
            AppFiles = filelib:wildcard(filename:join(Packages, "*/ebin/*.app")),
            FromApps = lists:append([
                listed(Props)
             || F <- AppFiles, {ok, [{application, _, Props}]} <- [file:consult(F)]
            ]),
            FromImports = lists:append([owner(Lib, M) || M <- Otp]),
            Apps = walk(Lib, [kernel, stdlib | FromApps ++ FromImports], []),
            [io:format("~s~n", [code:lib_dir(A)]) || A <- lists:sort(Apps)]
    end,
    ok;
main(_) ->
    fail("usage: otp_apps.escript apps|modules <erlang-shipment-dir>", []).

imports(Beams) ->
    lists:usort([
        M
     || B <- Beams,
        {ok, {_, [{imports, Is}]}} <- [beam_lib:chunks(B, [imports])],
        {M, _, _} <- Is
    ]).

owner(Lib, Module) ->
    case code:which(Module) of
        Path when is_list(Path) ->
            case lists:prefix(Lib, Path) of
                true -> app_in(filename:dirname(Path));
                false -> []
            end;
        _ ->
            []
    end.

app_in(Ebin) ->
    case filelib:wildcard(filename:join(Ebin, "*.app")) of
        [File] ->
            {ok, [{application, App, _}]} = file:consult(File),
            [App];
        _ ->
            fail("~s has no single .app file", [Ebin])
    end.

listed(Props) ->
    proplists:get_value(applications, Props, []) ++
        proplists:get_value(included_applications, Props, []).

walk(_Lib, [], Seen) ->
    Seen;
walk(Lib, [App | Rest], Seen) ->
    case lists:member(App, Seen) orelse not otp(Lib, App) of
        true ->
            walk(Lib, Rest, Seen);
        false ->
            case application:load(App) of
                ok -> ok;
                {error, {already_loaded, App}} -> ok;
                {error, Reason} -> fail("cannot load ~p: ~p", [App, Reason])
            end,
            {ok, Props} = application:get_all_key(App),
            walk(Lib, listed(Props) ++ Rest, [App | Seen])
    end.

otp(Lib, App) ->
    case code:lib_dir(App) of
        Dir when is_list(Dir) -> lists:prefix(Lib, Dir);
        _ -> false
    end.

fail(Format, Args) ->
    io:format(standard_error, "otp_apps.escript: " ++ Format ++ "~n", Args),
    halt(1).
