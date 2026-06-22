%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_oset_nif).
-typing([eqwalizer]).
-compile({no_auto_import, [size/1]}).
-on_load(init/0).

-export([
    new/0,
    from_sorted/2,
    destroy/1,
    size/1,
    memory_bytes/1,
    insert/2,
    delete/2,
    rank/2,
    at/2,
    range/3,
    to_list/1
]).

-define(NIF_NAME, "guild_member_list_oset_nif").

-type oset() :: reference().
-type key() :: guild_member_list_oset:key().

-export_type([oset/0, key/0]).

-spec init() -> ok.
init() ->
    case erlang:load_nif(nif_path(), 0) of
        ok ->
            ok;
        {error, Reason} ->
            erlang:error({guild_member_list_oset_nif_load_failed, nif_path(), Reason})
    end.

-spec new() -> oset().
new() ->
    erlang:nif_error(nif_not_loaded).

-spec from_sorted(oset(), [key()]) -> ok.
from_sorted(_OSet, _Keys) ->
    erlang:nif_error(nif_not_loaded).

-spec destroy(oset()) -> ok.
destroy(_OSet) ->
    erlang:nif_error(nif_not_loaded).

-spec size(oset()) -> non_neg_integer().
size(_OSet) ->
    erlang:nif_error(nif_not_loaded).

-spec memory_bytes(oset()) -> non_neg_integer().
memory_bytes(_OSet) ->
    erlang:nif_error(nif_not_loaded).

-spec insert(oset(), key()) -> non_neg_integer().
insert(_OSet, _Key) ->
    erlang:nif_error(nif_not_loaded).

-spec delete(oset(), key()) -> non_neg_integer() | not_found.
delete(_OSet, _Key) ->
    erlang:nif_error(nif_not_loaded).

-spec rank(oset(), key()) -> non_neg_integer() | not_found.
rank(_OSet, _Key) ->
    erlang:nif_error(nif_not_loaded).

-spec at(oset(), integer()) -> key() | none.
at(_OSet, _Index) ->
    erlang:nif_error(nif_not_loaded).

-spec range(oset(), integer(), non_neg_integer()) -> [key()].
range(_OSet, _Start, _Count) ->
    erlang:nif_error(nif_not_loaded).

-spec to_list(oset()) -> [key()].
to_list(_OSet) ->
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
