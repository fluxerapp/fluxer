%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_markdown_plaintext_nif).
-typing([eqwalizer]).
-on_load(init/0).

-export([available/0, render_push_preview_nif/2]).

-define(NIF_NAME, "push_markdown_plaintext_nif").

-spec init() -> ok.
init() ->
    case erlang:load_nif(nif_path(), 0) of
        ok ->
            ok;
        {error, Reason} ->
            persistent_term:put({?MODULE, load_error}, Reason),
            ok
    end.

-spec available() -> boolean().
available() ->
    erlang:nif_error(nif_not_loaded).

-spec render_push_preview_nif(binary(), binary()) -> binary().
render_push_preview_nif(_Content, _ContextJson) ->
    erlang:nif_error(nif_not_loaded).

-spec nif_path() -> file:filename_all().
nif_path() ->
    filename:join(priv_dir(), ?NIF_NAME).

-spec priv_dir() -> file:filename_all().
priv_dir() ->
    case code:priv_dir(fluxer_gateway) of
        {error, _Reason} -> priv_dir_from_beam();
        Dir -> Dir
    end.

-spec priv_dir_from_beam() -> file:filename_all().
priv_dir_from_beam() ->
    case code:which(?MODULE) of
        Beam when is_list(Beam) ->
            AppDir = filename:dirname(filename:dirname(Beam)),
            filename:join(AppDir, "priv");
        _ ->
            "priv"
    end.
