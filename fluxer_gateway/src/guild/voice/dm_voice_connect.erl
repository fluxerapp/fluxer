%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(dm_voice_connect).
-typing([eqwalizer]).

-export([handle_dm_voice_with_channel/5]).
-export([handle_dm_connect_or_update/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export_type([
    dm_state/0,
    voice_state/0,
    voice_state_map/0,
    connect_request/0
]).

-type dm_state() :: map().
-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.
-type connect_request() :: map().

-define(DM_VOICE_CONNECTION_LIMIT, 5).

-spec handle_dm_voice_with_channel(map(), integer(), integer(), map(), dm_state()) ->
    {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
handle_dm_voice_with_channel(Channel, ChannelIdValue, UserId, Request, State) ->
    #{
        session_id := SessionId,
        self_mute := SelfMute,
        self_deaf := SelfDeaf,
        self_video := SelfVideo
    } = Request,
    Opts = extract_request_opts(Request),
    ChannelType = map_utils:get_integer(Channel, <<"type">>, undefined),
    case dm_voice_ring:is_dm_channel_type(ChannelType) of
        false ->
            {reply, gateway_errors:error(dm_invalid_channel_type), State};
        true ->
            handle_dm_recipient_check(
                build_connect_request(
                    Opts,
                    ChannelIdValue,
                    UserId,
                    SessionId,
                    SelfMute,
                    SelfDeaf,
                    SelfVideo,
                    State
                )
            )
    end.

-spec build_connect_request(
    map(), integer(), integer(), binary(), boolean(), boolean(), boolean(), dm_state()
) -> connect_request().
build_connect_request(
    Opts, ChannelIdValue, UserId, SessionId, SelfMute, SelfDeaf, SelfVideo, State
) ->
    #{
        connection_id => maps:get(connection_id, Opts),
        channel_id => ChannelIdValue,
        user_id => UserId,
        session_id => SessionId,
        self_mute => SelfMute,
        self_deaf => SelfDeaf,
        self_video => SelfVideo,
        self_stream => maps:get(self_stream, Opts),
        viewer_stream_keys => maps:get(viewer_stream_keys, Opts),
        is_mobile => maps:get(is_mobile, Opts),
        latitude => maps:get(latitude, Opts),
        longitude => maps:get(longitude, Opts),
        e2ee_capable => maps:get(e2ee_capable, Opts),
        bot => maps:get(bot, Opts),
        p2p => maps:get(p2p, Opts),
        country_code => maps:get(country_code, Opts),
        voice_states => maps:get(dm_voice_states, State, #{}),
        state => State
    }.

-spec handle_dm_recipient_check(connect_request()) ->
    {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
handle_dm_recipient_check(
    #{channel_id := ChannelIdValue, user_id := UserId, state := State} = Req
) ->
    case dm_voice_ring:check_recipient(UserId, ChannelIdValue, State) of
        false -> {reply, gateway_errors:error(dm_not_recipient), State};
        true -> handle_dm_connect_or_update(Req)
    end.

-spec extract_request_opts(map()) -> map().
extract_request_opts(Request) ->
    #{
        connection_id => maps:get(connection_id, Request, undefined),
        self_stream => maps:get(self_stream, Request, false),
        is_mobile => maps:get(is_mobile, Request, false),
        viewer_stream_keys => maps:get(viewer_stream_keys, Request, undefined),
        latitude => maps:get(latitude, Request, null),
        longitude => maps:get(longitude, Request, null),
        e2ee_capable => maps:get(e2ee_capable, Request, false),
        bot => maps:get(bot, Request, false),
        p2p => maps:get(p2p, Request, undefined),
        country_code => maps:get(country_code, Request, undefined)
    }.

-spec handle_dm_connect_or_update(
    connect_request()
) -> {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
handle_dm_connect_or_update(#{connection_id := ConnectionId, state := State} = Req) when
    ConnectionId =:= undefined; ConnectionId =:= null
->
    VoiceStates = maps:get(dm_voice_states, State, #{}),
    UserId = maps:get(user_id, Req),
    ChannelIdValue = maps:get(channel_id, Req),
    case dm_connection_limit_allows_join(UserId, ChannelIdValue, VoiceStates) of
        false ->
            {reply, gateway_errors:error(voice_connection_limit_reached), State};
        true ->
            handle_new_connection(Req#{voice_states => VoiceStates})
    end;
handle_dm_connect_or_update(
    #{connection_id := ConnectionId, voice_states := VoiceStates, state := State} = Req
) ->
    case maps:get(ConnectionId, VoiceStates, undefined) of
        undefined ->
            {reply, gateway_errors:error(voice_connection_not_found), State};
        ExistingVoiceState ->
            handle_existing_connection(Req#{existing_voice_state => ExistingVoiceState})
    end.

-spec handle_new_connection(
    connect_request()
) -> {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
handle_new_connection(
    #{
        channel_id := ChannelIdValue,
        viewer_stream_keys := ViewerStreamKeys,
        voice_states := VoiceStates
    } = Req
) ->
    State = maps:get(state, Req),
    case
        dm_voice_state:validate_dm_viewer_stream_keys(
            ViewerStreamKeys, ChannelIdValue, VoiceStates
        )
    of
        {error, ErrorAtom} ->
            {reply, gateway_errors:error(ErrorAtom), State};
        {ok, ParsedViewerKey} ->
            check_e2ee_and_create(Req#{viewer_stream_keys => ParsedViewerKey})
    end.

-spec check_e2ee_and_create(
    connect_request()
) -> {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
check_e2ee_and_create(Req) ->
    ChannelIdValue = maps:get(channel_id, Req),
    State = maps:get(state, Req),
    DmJoinResult =
        case guild_voice_e2ee:is_e2ee_enabled_for_dm() of
            true ->
                guild_voice_e2ee:check_join_allowed_dm(
                    ChannelIdValue,
                    maps:get(e2ee_capable, Req),
                    maps:get(bot, Req),
                    maps:get(voice_states, Req)
                );
            false ->
                ok
        end,
    case DmJoinResult of
        {error, ErrorAtom2} ->
            {reply, gateway_errors:error(ErrorAtom2), State};
        ok ->
            dm_voice_token:get_dm_voice_token_and_create_state(Req)
    end.

-spec handle_existing_connection(
    connect_request()
) -> {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
handle_existing_connection(#{existing_voice_state := ExistingVoiceState, state := State} = Req) ->
    UserId = maps:get(user_id, Req),
    case guild_voice_state:user_matches_voice_state(ExistingVoiceState, UserId) of
        false ->
            {reply, gateway_errors:error(voice_user_mismatch), State};
        true ->
            apply_existing_update(Req)
    end.

-spec apply_existing_update(
    connect_request()
) -> {reply, map(), dm_state()} | {reply, {error, atom(), atom()}, dm_state()}.
apply_existing_update(
    #{
        channel_id := ChannelIdValue,
        session_id := SessionId,
        viewer_stream_keys := ViewerStreamKeys,
        existing_voice_state := ExistingVoiceState,
        voice_states := VoiceStates,
        state := State
    } = Req
) ->
    ExistingSessId = maps:get(<<"session_id">>, ExistingVoiceState, undefined),
    EffSessId = dm_voice_state:resolve_effective_session_id(ExistingSessId, SessionId),
    case
        resolve_existing_viewer_stream_keys(
            ViewerStreamKeys, ChannelIdValue, VoiceStates, ExistingVoiceState
        )
    of
        {error, ErrorAtom} ->
            {reply, gateway_errors:error(ErrorAtom), State};
        {ok, ParsedViewerKey} ->
            commit_existing_update(Req#{
                effective_session_id => EffSessId,
                viewer_stream_keys => ParsedViewerKey
            })
    end.

-spec resolve_existing_viewer_stream_keys(term(), integer(), voice_state_map(), voice_state()) ->
    {ok, list()} | {error, atom()}.
resolve_existing_viewer_stream_keys(
    undefined, _ChannelIdValue, _VoiceStates, ExistingVoiceState
) ->
    {ok, existing_viewer_stream_keys(ExistingVoiceState)};
resolve_existing_viewer_stream_keys(
    ViewerStreamKeys, ChannelIdValue, VoiceStates, _ExistingVoiceState
) ->
    dm_voice_state:validate_dm_viewer_stream_keys(
        ViewerStreamKeys, ChannelIdValue, VoiceStates
    ).

-spec existing_viewer_stream_keys(voice_state()) -> list().
existing_viewer_stream_keys(ExistingVoiceState) ->
    case maps:get(<<"viewer_stream_keys">>, ExistingVoiceState, []) of
        Keys when is_list(Keys) -> Keys;
        _ -> []
    end.

-spec commit_existing_update(connect_request()) -> {reply, map(), dm_state()}.
commit_existing_update(
    #{
        connection_id := ConnectionId,
        channel_id := ChannelIdValue,
        effective_session_id := EffSessId,
        existing_voice_state := ExistingVoiceState,
        voice_states := VoiceStates,
        state := State
    } = Req
) ->
    OldChannelId = maps:get(<<"channel_id">>, ExistingVoiceState, null),
    NeedsToken = not snowflake_id:equal(ChannelIdValue, OldChannelId),
    P2p =
        voice_p2p:is_p2p(ExistingVoiceState) andalso not NeedsToken andalso
            not voice_p2p:declined(maps:get(p2p, Req, undefined)),
    UpdatedVS = build_updated_voice_state(ExistingVoiceState, Req#{p2p => P2p}),
    NewVoiceStates = VoiceStates#{ConnectionId => UpdatedVS},
    NewState = State#{dm_voice_states => NewVoiceStates},
    _ = voice_state_counts_cache:upsert_voice_state(UpdatedVS),
    dm_voice_ring:broadcast_voice_state_update(ChannelIdValue, UpdatedVS, NewState),
    UserId = maps:get(user_id, Req),
    maybe_spawn_call_voice_state_update(NeedsToken, ChannelIdValue, UserId, UpdatedVS),
    dm_voice_token:maybe_spawn_join_call(
        NeedsToken, ChannelIdValue, UserId, UpdatedVS, EffSessId, State
    ),
    {reply, #{success => true, needs_token => NeedsToken}, NewState}.

-spec maybe_spawn_call_voice_state_update(boolean(), integer(), integer(), voice_state()) -> ok.
maybe_spawn_call_voice_state_update(true, _ChannelId, _UserId, _VoiceState) ->
    ok;
maybe_spawn_call_voice_state_update(false, ChannelId, UserId, VoiceState) ->
    spawn(fun() -> update_call_voice_state(ChannelId, UserId, VoiceState) end),
    ok.

-spec update_call_voice_state(integer(), integer(), voice_state()) -> ok.
update_call_voice_state(ChannelId, UserId, VoiceState) ->
    case call_manager:lookup(ChannelId) of
        {ok, CallPid} ->
            _ = gateway_rpc_call_lookup:safe_gen_server_call(
                CallPid, {update_voice_state, UserId, VoiceState}, 5000
            ),
            ok;
        _ ->
            ok
    end.

-spec build_updated_voice_state(voice_state(), connect_request()) -> voice_state().
build_updated_voice_state(ExistingVoiceState, Req) ->
    OldVersion = voice_state_utils:voice_state_version(ExistingVoiceState),
    voice_state_utils:complete_voice_state(ExistingVoiceState#{
        <<"guild_id">> => null,
        <<"channel_id">> => integer_to_binary(maps:get(channel_id, Req)),
        <<"session_id">> => maps:get(effective_session_id, Req),
        <<"mute">> => false,
        <<"deaf">> => false,
        <<"self_mute">> => maps:get(self_mute, Req),
        <<"self_deaf">> => maps:get(self_deaf, Req),
        <<"self_video">> => maps:get(self_video, Req),
        <<"self_stream">> => maps:get(self_stream, Req),
        <<"is_mobile">> => maps:get(is_mobile, Req),
        <<"suppress">> => false,
        <<"viewer_stream_keys">> => maps:get(viewer_stream_keys, Req),
        <<"p2p">> => maps:get(p2p, Req),
        <<"version">> => OldVersion + 1
    }).

-spec dm_connection_limit_allows_join(integer(), integer(), voice_state_map()) -> boolean().
dm_connection_limit_allows_join(UserId, ChannelIdValue, VoiceStates) ->
    dm_user_connection_count(UserId, ChannelIdValue, VoiceStates) < ?DM_VOICE_CONNECTION_LIMIT.

-spec dm_user_connection_count(integer(), integer(), voice_state_map()) -> non_neg_integer().
dm_user_connection_count(UserId, ChannelIdValue, VoiceStates) ->
    maps:fold(
        fun(_ConnId, VoiceState, Acc) ->
            increment_matching_dm_connection(VoiceState, UserId, ChannelIdValue, Acc)
        end,
        0,
        VoiceStates
    ).

-spec increment_matching_dm_connection(voice_state(), integer(), integer(), non_neg_integer()) ->
    non_neg_integer().
increment_matching_dm_connection(VoiceState, UserId, ChannelIdValue, Acc) ->
    case
        {
            voice_state_utils:voice_state_user_id(VoiceState),
            voice_state_utils:voice_state_channel_id(VoiceState)
        }
    of
        {UserId, ChannelIdValue} -> Acc + 1;
        _ -> Acc
    end.

-ifdef(TEST).

dm_connection_limit_allows_join_test() ->
    VoiceStates = #{
        <<"conn-1">> => #{<<"channel_id">> => <<"100">>, <<"user_id">> => <<"10">>},
        <<"conn-2">> => #{<<"channel_id">> => <<"100">>, <<"user_id">> => <<"10">>},
        <<"conn-3">> => #{<<"channel_id">> => <<"100">>, <<"user_id">> => <<"10">>},
        <<"conn-4">> => #{<<"channel_id">> => <<"100">>, <<"user_id">> => <<"10">>},
        <<"conn-5">> => #{<<"channel_id">> => <<"100">>, <<"user_id">> => <<"10">>},
        <<"conn-6">> => #{<<"channel_id">> => <<"200">>, <<"user_id">> => <<"10">>}
    },
    ?assertNot(dm_connection_limit_allows_join(10, 100, VoiceStates)),
    ?assert(dm_connection_limit_allows_join(10, 200, VoiceStates)),
    ?assert(dm_connection_limit_allows_join(20, 100, VoiceStates)).

-endif.
