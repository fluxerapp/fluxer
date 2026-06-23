%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(presence_payload).
-typing([eqwalizer]).

-export([build/6]).

-export_type([status/0, custom_status/0]).

-type status() :: online | offline | idle | dnd | invisible | binary().
-type custom_status() :: map() | null.

-spec build(map(), status(), boolean(), boolean(), custom_status(), [map()]) -> map().
build(UserData, Status, Mobile, Afk, CustomStatus, Activities) ->
    StatusBin = ensure_status_binary(Status),
    #{
        <<"user">> => user_utils:normalize_user(UserData),
        <<"status">> => StatusBin,
        <<"mobile">> => Mobile,
        <<"afk">> => Afk,
        <<"custom_status">> => custom_status_for(StatusBin, CustomStatus),
        <<"activities">> => activities_for(StatusBin, Activities)
    }.

-spec ensure_status_binary(term()) -> binary().
ensure_status_binary(online) -> <<"online">>;
ensure_status_binary(offline) -> <<"offline">>;
ensure_status_binary(idle) -> <<"idle">>;
ensure_status_binary(dnd) -> <<"dnd">>;
ensure_status_binary(invisible) -> <<"offline">>;
ensure_status_binary(<<"invisible">>) -> <<"offline">>;
ensure_status_binary(Status) when is_binary(Status) -> Status;
ensure_status_binary(_) -> <<"offline">>.

-spec custom_status_for(binary(), custom_status()) -> custom_status().
custom_status_for(<<"offline">>, _CustomStatus) -> null;
custom_status_for(<<"invisible">>, _CustomStatus) -> null;
custom_status_for(_StatusBin, CustomStatus) -> normalize_custom_status(CustomStatus).

-spec normalize_custom_status(term()) -> custom_status().
normalize_custom_status(null) -> null;
normalize_custom_status(CustomStatus) when is_map(CustomStatus) -> CustomStatus;
normalize_custom_status(_) -> null.

-spec activities_for(binary(), [map()]) -> [map()].
activities_for(<<"offline">>, _Activities) -> [];
activities_for(<<"invisible">>, _Activities) -> [];
activities_for(_StatusBin, Activities) when is_list(Activities) -> lists:sublist(Activities, 5);
activities_for(_StatusBin, _Activities) -> [].

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
    Result = build(User, invisible, false, false, CustomStatus, []),
    ?assertEqual(<<"offline">>, maps:get(<<"status">>, Result)),
    ?assertEqual(null, maps:get(<<"custom_status">>, Result)).

build_invisible_binary_normalized_to_offline_test() ->
    User = #{<<"id">> => <<"1">>, <<"username">> => <<"Test">>},
    CustomStatus = #{<<"text">> => <<"hello">>},
    Result = build(User, <<"invisible">>, false, false, CustomStatus, []),
    ?assertEqual(<<"offline">>, maps:get(<<"status">>, Result)),
    ?assertEqual(null, maps:get(<<"custom_status">>, Result)).
-endif.
