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
        <<"custom_status">> => custom_status_for(StatusBin, CustomStatus)
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
    CustomStatus = #{<<"text">> => <<"hello">>},
    Result = build(test_user(), invisible, false, false, CustomStatus),
    ?assertEqual(<<"offline">>, maps:get(<<"status">>, Result)),
    ?assertEqual(null, maps:get(<<"custom_status">>, Result)).

build_invisible_binary_normalized_to_offline_test() ->
    CustomStatus = #{<<"text">> => <<"hello">>},
    Result = build(test_user(), <<"invisible">>, false, false, CustomStatus),
    ?assertEqual(<<"offline">>, maps:get(<<"status">>, Result)),
    ?assertEqual(null, maps:get(<<"custom_status">>, Result)).

build_clears_expired_custom_status_test() ->
    with_expiry_enabled(fun() ->
        Result = build(test_user(), online, false, false, expired_custom_status()),
        ?assertEqual(<<"online">>, maps:get(<<"status">>, Result)),
        ?assertEqual(null, maps:get(<<"custom_status">>, Result))
    end).

build_coalesces_an_expired_status_with_no_status_test() ->
    with_expiry_enabled(fun() ->
        ?assertEqual(
            build(test_user(), online, false, false, null),
            build(test_user(), online, false, false, expired_custom_status())
        )
    end).

build_keeps_unexpired_custom_status_test() ->
    with_expiry_enabled(fun() ->
        Live = future_custom_status(),
        Result = build(test_user(), dnd, false, false, Live),
        ?assertEqual(Live, maps:get(<<"custom_status">>, Result))
    end).

build_keeps_custom_status_without_expires_at_test() ->
    with_expiry_enabled(fun() ->
        Live = #{<<"text">> => <<"forever">>},
        Result = build(test_user(), idle, false, false, Live),
        ?assertEqual(Live, maps:get(<<"custom_status">>, Result))
    end).

build_keeps_malformed_expires_at_test() ->
    with_expiry_enabled(fun() ->
        Live = #{<<"text">> => <<"hi">>, <<"expires_at">> => <<"not-a-date">>},
        Result = build(test_user(), online, false, false, Live),
        ?assertEqual(Live, maps:get(<<"custom_status">>, Result))
    end).

build_leaves_expired_status_alone_while_disabled_test() ->
    application:unset_env(fluxer_gateway, custom_status_expiry_enabled),
    Expired = expired_custom_status(),
    Result = build(test_user(), online, false, false, Expired),
    ?assertEqual(Expired, maps:get(<<"custom_status">>, Result)).

test_user() ->
    #{<<"id">> => <<"1">>, <<"username">> => <<"Test">>}.

expired_custom_status() ->
    #{
        <<"emoji_animated">> => false,
        <<"emoji_name">> => null,
        <<"expires_at">> => <<"2026-05-13T13:02:27.497Z">>,
        <<"text">> => <<"brb">>
    }.

future_custom_status() ->
    ExpiresAt = calendar:system_time_to_rfc3339(
        erlang:system_time(millisecond) + 3600000, [{unit, millisecond}, {offset, "Z"}]
    ),
    #{<<"text">> => <<"brb">>, <<"expires_at">> => list_to_binary(ExpiresAt)}.

with_expiry_enabled(Fun) ->
    Key = custom_status_expiry_enabled,
    Previous = application:get_env(fluxer_gateway, Key),
    application:set_env(fluxer_gateway, Key, true),
    try
        Fun()
    after
        restore_expiry_env(Key, Previous)
    end.

restore_expiry_env(Key, undefined) ->
    application:unset_env(fluxer_gateway, Key);
restore_expiry_env(Key, {ok, Value}) ->
    application:set_env(fluxer_gateway, Key, Value).
-endif.
