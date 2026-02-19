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

-module(presence_payload).

-export([build/5]).

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
        <<"custom_status">> => custom_status_for(StatusBin, CustomStatus)
    }.

-spec ensure_status_binary(status()) -> binary().
ensure_status_binary(online) -> <<"online">>;
ensure_status_binary(offline) -> <<"offline">>;
ensure_status_binary(idle) -> <<"idle">>;
ensure_status_binary(dnd) -> <<"dnd">>;
ensure_status_binary(invisible) -> <<"offline">>;
ensure_status_binary(<<"invisible">>) -> <<"offline">>;
ensure_status_binary(Status) when is_binary(Status) -> Status;
ensure_status_binary(_) -> <<"offline">>.

-spec custom_status_for(binary(), custom_status()) -> custom_status().
custom_status_for(StatusBin, CustomStatus) ->
    case StatusBin of
        <<"offline">> ->
            null;
        <<"invisible">> ->
            null;
        _ ->
            normalize_custom_status(CustomStatus)
    end.

-spec normalize_custom_status(term()) -> custom_status().
normalize_custom_status(null) ->
    null;
normalize_custom_status(CustomStatus) when is_map(CustomStatus) ->
    CustomStatus;
normalize_custom_status(_) ->
    null.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ensure_status_binary_atom_test() ->
    ?assertEqual(<<"online">>, ensure_status_binary(online)),
    ?assertEqual(<<"offline">>, ensure_status_binary(offline)),
    ?assertEqual(<<"idle">>, ensure_status_binary(idle)),
    ?assertEqual(<<"dnd">>, ensure_status_binary(dnd)),
    ?assertEqual(<<"offline">>, ensure_status_binary(invisible)).

ensure_status_binary_binary_test() ->
    ?assertEqual(<<"online">>, ensure_status_binary(<<"online">>)),
    ?assertEqual(<<"offline">>, ensure_status_binary(<<"invisible">>)),
    ?assertEqual(<<"custom">>, ensure_status_binary(<<"custom">>)).

ensure_status_binary_unknown_test() ->
    ?assertEqual(<<"offline">>, ensure_status_binary(123)),
    ?assertEqual(<<"offline">>, ensure_status_binary(undefined)).

custom_status_for_visible_test() ->
    Status = #{<<"text">> => <<"hello">>},
    ?assertEqual(Status, custom_status_for(<<"online">>, Status)),
    ?assertEqual(Status, custom_status_for(<<"idle">>, Status)),
    ?assertEqual(Status, custom_status_for(<<"dnd">>, Status)).

custom_status_for_invisible_test() ->
    Status = #{<<"text">> => <<"hello">>},
    ?assertEqual(null, custom_status_for(<<"offline">>, Status)),
    ?assertEqual(null, custom_status_for(<<"invisible">>, Status)).

custom_status_for_null_test() ->
    ?assertEqual(null, custom_status_for(<<"online">>, null)).

normalize_custom_status_test() ->
    ?assertEqual(null, normalize_custom_status(null)),
    ?assertEqual(#{<<"text">> => <<"hi">>}, normalize_custom_status(#{<<"text">> => <<"hi">>})),
    ?assertEqual(null, normalize_custom_status(<<"invalid">>)),
    ?assertEqual(null, normalize_custom_status(123)).

build_invisible_atom_normalized_to_offline_test() ->
    User = #{<<"id">> => <<"1">>, <<"username">> => <<"Test">>},
    CustomStatus = #{<<"text">> => <<"hello">>},
    Result = build(User, invisible, false, false, CustomStatus),
    ?assertEqual(<<"offline">>, maps:get(<<"status">>, Result)),
    ?assertEqual(null, maps:get(<<"custom_status">>, Result)).

build_invisible_binary_normalized_to_offline_test() ->
    User = #{<<"id">> => <<"1">>, <<"username">> => <<"Test">>},
    CustomStatus = #{<<"text">> => <<"hello">>},
    Result = build(User, <<"invisible">>, false, false, CustomStatus),
    ?assertEqual(<<"offline">>, maps:get(<<"status">>, Result)),
    ?assertEqual(null, maps:get(<<"custom_status">>, Result)).
-endif.
