%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_state).
-typing([eqwalizer]).

-include_lib("fluxer_gateway/include/voice_state.hrl").

-export([get_voice_state/2]).
-export([get_voice_states_list/1]).
-export([update_voice_state_data/1]).
-export([user_matches_voice_state/2]).
-export([create_voice_state/2]).
-export([extract_session_info_from_voice_state/2]).
-export([has_voice_state_change/2]).

-export_type([
    guild_state/0,
    voice_state/0,
    voice_state_map/0,
    voice_flags/0
]).

-type guild_state() :: map().
-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.

-spec get_voice_state(map(), guild_state()) -> {reply, map(), guild_state()}.
get_voice_state(Request, State) ->
    case maps:get(connection_id, Request, null) of
        null ->
            {reply, #{voice_state => null}, State};
        ConnectionId ->
            VoiceStates = voice_state_utils:voice_states(State),
            VoiceState = maps:get(ConnectionId, VoiceStates, null),
            {reply, #{voice_state => external_or_null(VoiceState)}, State}
    end.

-spec external_or_null(voice_state() | null) -> voice_state() | null.
external_or_null(null) -> null;
external_or_null(VoiceState) -> voice_state_utils:external_voice_state(VoiceState).

-spec get_voice_states_list(guild_state()) -> [voice_state()].
get_voice_states_list(State) ->
    maps:values(voice_state_utils:voice_states(State)).

-spec update_voice_state_data(
    map()
) -> {reply, map(), guild_state()}.
update_voice_state_data(
    #{
        connection_id := ConnectionId,
        channel_id := ChannelIdBin,
        flags := Flags,
        member := Member,
        existing_voice_state := ExistingVoiceState,
        voice_states := VoiceStates,
        state := State,
        needs_token := NeedsToken,
        viewer_stream_keys := ViewerStreamKeys
    }
) ->
    Resolved = resolve_voice_fields(
        Flags, Member, ExistingVoiceState, ChannelIdBin, ViewerStreamKeys
    ),
    case Resolved of
        #{has_change := false} ->
            {reply,
                #{
                    success => true,
                    voice_state => voice_state_utils:external_voice_state(ExistingVoiceState)
                },
                State};
        #{has_change := true} = R ->
            apply_voice_state_change(
                ConnectionId,
                ChannelIdBin,
                ExistingVoiceState,
                VoiceStates,
                State,
                NeedsToken,
                R
            )
    end.

-spec resolve_voice_fields(voice_flags(), map(), voice_state(), binary(), list()) -> map().
resolve_voice_fields(Flags, Member, ExistingVoiceState, ChannelIdBin, ViewerStreamKeys) ->
    ServerMute = maps:get(<<"mute">>, Member, false),
    ServerDeaf = maps:get(<<"deaf">>, Member, false),
    Fields = #{
        server_mute => ServerMute,
        server_deaf => ServerDeaf,
        self_mute => maps:get(self_mute, Flags),
        self_deaf => maps:get(self_deaf, Flags),
        self_video => maps:get(self_video, Flags),
        self_stream => maps:get(self_stream, Flags),
        is_mobile => maps:get(is_mobile, Flags),
        suppress => maps:get(suppress, Flags),
        channel_id => ChannelIdBin,
        viewer_stream_keys => ViewerStreamKeys
    },
    Fields#{
        member => Member,
        has_change => has_voice_state_change(ExistingVoiceState, Fields)
    }.

-spec apply_voice_state_change(
    binary(),
    binary(),
    voice_state(),
    voice_state_map(),
    guild_state(),
    boolean(),
    map()
) -> {reply, map(), guild_state()}.
apply_voice_state_change(
    ConnectionId,
    ChannelIdBin,
    ExistingVoiceState,
    VoiceStates,
    State,
    NeedsToken,
    R
) ->
    OldChannelIdBin = maps:get(<<"channel_id">>, ExistingVoiceState, null),
    UpdatedVoiceState = build_updated_voice_state(ExistingVoiceState, ChannelIdBin, R),
    NewVoiceStates = VoiceStates#{ConnectionId => UpdatedVoiceState},
    NewState = State#{voice_states => NewVoiceStates},
    broadcast_voice_change(
        OldChannelIdBin,
        ChannelIdBin,
        ExistingVoiceState,
        UpdatedVoiceState,
        ConnectionId,
        NewState
    ),
    Reply = build_update_reply(UpdatedVoiceState, NeedsToken),
    {reply, Reply, NewState}.

-spec build_updated_voice_state(voice_state(), binary(), map()) -> voice_state().
build_updated_voice_state(ExistingVoiceState, ChannelIdBin, R) ->
    OldVersion = voice_state_utils:voice_state_version(ExistingVoiceState),
    Updated = ExistingVoiceState#{
        <<"channel_id">> => ChannelIdBin,
        <<"mute">> => maps:get(server_mute, R),
        <<"deaf">> => maps:get(server_deaf, R),
        <<"self_mute">> => maps:get(self_mute, R),
        <<"self_deaf">> => maps:get(self_deaf, R),
        <<"self_video">> => maps:get(self_video, R),
        <<"self_stream">> => maps:get(self_stream, R),
        <<"is_mobile">> => maps:get(is_mobile, R),
        <<"suppress">> => maps:get(suppress, R),
        <<"viewer_stream_keys">> => maps:get(viewer_stream_keys, R),
        <<"version">> => OldVersion + 1
    },
    voice_state_utils:complete_voice_state(refresh_member(Updated, maps:get(member, R, #{}))).

-spec refresh_member(voice_state(), map()) -> voice_state().
refresh_member(VoiceState, Member) when is_map(Member), map_size(Member) > 0 ->
    VoiceState#{<<"member">> => Member};
refresh_member(VoiceState, _Member) ->
    VoiceState.

-spec broadcast_voice_change(
    binary() | null, binary(), voice_state(), voice_state(), binary(), guild_state()
) -> ok.
broadcast_voice_change(
    OldChannelIdBin,
    ChannelIdBin,
    ExistingVoiceState,
    UpdatedVoiceState,
    ConnectionId,
    NewState
) ->
    case OldChannelIdBin =/= ChannelIdBin of
        true ->
            DisconnectState = ExistingVoiceState#{
                <<"channel_id">> => null, <<"connection_id">> => ConnectionId
            },
            guild_voice_broadcast:broadcast_voice_state_update(
                DisconnectState, NewState, OldChannelIdBin
            ),
            guild_voice_broadcast:broadcast_voice_state_update(
                UpdatedVoiceState, NewState, ChannelIdBin
            );
        false ->
            guild_voice_broadcast:broadcast_voice_state_update(
                UpdatedVoiceState, NewState, ChannelIdBin
            )
    end,
    ok.

-spec build_update_reply(voice_state(), boolean()) -> map().
build_update_reply(UpdatedVoiceState, true) ->
    #{
        success => true,
        voice_state => voice_state_utils:external_voice_state(UpdatedVoiceState),
        needs_token => true
    };
build_update_reply(UpdatedVoiceState, false) ->
    #{
        success => true,
        voice_state => voice_state_utils:external_voice_state(UpdatedVoiceState)
    }.

-spec has_voice_state_change(voice_state(), map()) -> boolean().
has_voice_state_change(ExistingVoiceState, Fields) ->
    lists:any(
        fun({VoiceKey, FieldKey, Default}) ->
            maps:get(VoiceKey, ExistingVoiceState, Default) =/= maps:get(FieldKey, Fields)
        end,
        voice_state_change_fields()
    ).

-spec voice_state_change_fields() -> [{binary(), atom(), term()}].
voice_state_change_fields() ->
    [
        {<<"channel_id">>, channel_id, null},
        {<<"mute">>, server_mute, false},
        {<<"deaf">>, server_deaf, false},
        {<<"self_mute">>, self_mute, false},
        {<<"self_deaf">>, self_deaf, false},
        {<<"self_video">>, self_video, false},
        {<<"self_stream">>, self_stream, false},
        {<<"is_mobile">>, is_mobile, false},
        {<<"suppress">>, suppress, false},
        {<<"viewer_stream_keys">>, viewer_stream_keys, []}
    ].

-spec user_matches_voice_state(voice_state(), term()) -> boolean().
user_matches_voice_state(VoiceState, UserId) when is_integer(UserId), UserId > 0 ->
    case voice_state_utils:voice_state_user_id(VoiceState) of
        undefined -> false;
        VoiceUserId -> VoiceUserId =:= UserId
    end;
user_matches_voice_state(VoiceState, UserId) when is_binary(UserId) ->
    user_matches_normalized_voice_state(VoiceState, snowflake_id:parse(UserId));
user_matches_voice_state(VoiceState, UserId) when is_list(UserId) ->
    user_matches_normalized_voice_state(VoiceState, snowflake_id:parse(UserId));
user_matches_voice_state(_VoiceState, _UserId) ->
    false.

-spec user_matches_normalized_voice_state(voice_state(), term()) -> boolean().
user_matches_normalized_voice_state(VoiceState, UserIdInt) ->
    case UserIdInt of
        UserIdInt when is_integer(UserIdInt), UserIdInt > 0 ->
            user_matches_voice_state(VoiceState, UserIdInt);
        _ ->
            false
    end.

-spec create_voice_state(
    map(),
    voice_flags()
) -> voice_state().
create_voice_state(Fields, Flags) ->
    Base = base_voice_state(
        maps:get(guild_id, Fields),
        maps:get(channel_id, Fields),
        maps:get(user_id, Fields),
        maps:get(connection_id, Fields),
        maps:get(server_mute, Fields),
        maps:get(server_deaf, Fields),
        maps:get(viewer_stream_keys, Fields),
        maps:get(e2ee_capable, Fields, false)
    ),
    voice_state_utils:complete_voice_state(apply_voice_flags(Base, Flags)).

-spec base_voice_state(
    binary(),
    binary(),
    binary(),
    binary(),
    boolean(),
    boolean(),
    list(),
    boolean()
) -> voice_state().
base_voice_state(
    GuildIdBin,
    ChannelIdBin,
    UserIdBin,
    ConnectionId,
    ServerMute,
    ServerDeaf,
    ViewerStreamKeys,
    E2EECapable
) ->
    #{
        <<"guild_id">> => GuildIdBin,
        <<"channel_id">> => ChannelIdBin,
        <<"user_id">> => UserIdBin,
        <<"connection_id">> => ConnectionId,
        <<"mute">> => ServerMute,
        <<"deaf">> => ServerDeaf,
        <<"viewer_stream_keys">> => ViewerStreamKeys,
        <<"e2ee_capable">> => E2EECapable,
        <<"version">> => voice_state_utils:initial_voice_state_version()
    }.

-spec apply_voice_flags(voice_state(), voice_flags()) -> voice_state().
apply_voice_flags(State, Flags) ->
    #{
        self_mute := SM,
        self_deaf := SD,
        self_video := SV,
        self_stream := SS,
        is_mobile := IM,
        suppress := Sup
    } = Flags,
    State#{
        <<"self_mute">> => SM,
        <<"self_deaf">> => SD,
        <<"self_video">> => SV,
        <<"self_stream">> => SS,
        <<"is_mobile">> => IM,
        <<"suppress">> => Sup
    }.

-spec extract_session_info_from_voice_state(binary(), voice_state()) -> map().
extract_session_info_from_voice_state(ConnId, VoiceState) ->
    #{
        connection_id => ConnId,
        session_id => maps:get(<<"session_id">>, VoiceState, undefined),
        self_mute => maps:get(<<"self_mute">>, VoiceState, false),
        self_deaf => maps:get(<<"self_deaf">>, VoiceState, false),
        self_video => maps:get(<<"self_video">>, VoiceState, false),
        self_stream => maps:get(<<"self_stream">>, VoiceState, false),
        is_mobile => maps:get(<<"is_mobile">>, VoiceState, false),
        suppress => maps:get(<<"suppress">>, VoiceState, false),
        e2ee_capable => maps:get(<<"e2ee_capable">>, VoiceState, false),
        latitude => maps:get(<<"latitude">>, VoiceState, undefined),
        longitude => maps:get(<<"longitude">>, VoiceState, undefined),
        member => member_or_empty(VoiceState)
    }.

-spec member_or_empty(voice_state()) -> map().
member_or_empty(VoiceState) ->
    case maps:get(<<"member">>, VoiceState, #{}) of
        Member when is_map(Member) -> Member;
        _ -> #{}
    end.
