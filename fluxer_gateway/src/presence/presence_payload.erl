%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_payload).
-typing([eqwalizer]).

-export([build/5]).

-export_type([status/0, custom_status/0]).

-type status() :: online | offline | idle | dnd | invisible | binary().
-type custom_status() :: map() | null.

-spec build(map(), status(), boolean(), boolean(), custom_status()) -> map().
build(UserData, Status, Mobile, Afk, CustomStatus) ->
    StatusBin = ensure_status_binary(Status),
    #{
        <<"user">> => user_utils:normalize_user(UserData),
        <<"status">> => StatusBin,
        <<"mobile">> => Mobile,
        <<"afk">> => Afk,
        <<"custom_status">> => custom_status_for(
            StatusBin, visible_custom_status(UserData, CustomStatus)
        )
    }.

-spec visible_custom_status(map(), term()) -> term().
visible_custom_status(UserData, CustomStatus) ->
    case user_utils:profile_restricted(UserData) of
        true -> null;
        false -> CustomStatus
    end.

-spec ensure_status_binary(term()) -> binary().
ensure_status_binary(online) -> <<"online">>;
ensure_status_binary(offline) -> <<"offline">>;
ensure_status_binary(idle) -> <<"idle">>;
ensure_status_binary(dnd) -> <<"dnd">>;
ensure_status_binary(invisible) -> <<"offline">>;
ensure_status_binary(<<"invisible">>) -> <<"offline">>;
ensure_status_binary(Status) when is_binary(Status) -> Status;
ensure_status_binary(_) -> <<"offline">>.

-spec custom_status_for(binary(), term()) -> custom_status().
custom_status_for(<<"offline">>, _CustomStatus) ->
    null;
custom_status_for(<<"invisible">>, _CustomStatus) ->
    null;
custom_status_for(_StatusBin, CustomStatus) ->
    custom_status_expiry:clear_if_expired(normalize_custom_status(CustomStatus)).

-spec normalize_custom_status(term()) -> custom_status().
normalize_custom_status(null) -> null;
normalize_custom_status(CustomStatus) when is_map(CustomStatus) -> CustomStatus;
normalize_custom_status(_) -> null.
