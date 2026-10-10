%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(voice_p2p).
-typing([eqwalizer]).

-export([
    agreed/2,
    declined/1,
    is_p2p/1,
    pinned/1,
    channel_mode/1,
    join_decision/4,
    guild_join_decision/3,
    admit/2,
    invariant_violation/1,
    invariant_violation/2,
    reports_rejection/1,
    to_sfu/1,
    add_to_token_request/3,
    grant/1,
    voice_server_update/4,
    signal_route/3,
    signal_payload/4
]).

-export_type([
    voice_state/0, channel_mode/0, rejection/0, join_decision/0, grant/0, token_grant/0
]).

-define(MAX_PARTICIPANTS, 4).

-type voice_state() :: map().
-type channel_mode() :: empty | p2p | sfu | mixed.
-type rejection() :: {reject, atom()}.
-type join_decision() :: {p2p, initiator | joiner, pos_integer()} | sfu | rejection().
-type grant() :: {p2p, binary(), [map()], pos_integer()}.
-type token_grant() :: grant() | {declined, voice_channel_full}.

-spec agreed(term(), term()) -> boolean().
agreed(true, true) -> false;
agreed(true, _Bot) -> true;
agreed(_P2p, _Bot) -> false.

-spec declined(term()) -> boolean().
declined(false) -> true;
declined(_) -> false.

-spec is_p2p(voice_state()) -> boolean().
is_p2p(#{<<"p2p">> := true}) -> true;
is_p2p(_VoiceState) -> false.

-spec pinned(map() | undefined) -> boolean().
pinned(#{<<"rtc_p2p">> := true}) -> true;
pinned(_Channel) -> false.

-spec channel_mode([voice_state()]) -> channel_mode().
channel_mode([]) ->
    empty;
channel_mode(VoiceStates) ->
    case {lists:all(fun is_p2p/1, VoiceStates), lists:any(fun is_p2p/1, VoiceStates)} of
        {true, _} -> p2p;
        {false, true} -> mixed;
        {false, false} -> sfu
    end.

-spec join_decision(boolean(), [voice_state()], boolean(), boolean()) -> join_decision().
join_decision(Agreed, VoiceStates, HasPendingJoins, Pinned) ->
    case {channel_mode(VoiceStates), HasPendingJoins} of
        {p2p, _} -> join_p2p_decision(Agreed, length(VoiceStates) + 1);
        {empty, false} -> start_decision(Agreed, Pinned);
        _ -> sfu
    end.

-spec join_p2p_decision(boolean(), pos_integer()) -> join_decision().
join_p2p_decision(false, _ParticipantCount) ->
    {reject, voice_p2p_consent_required};
join_p2p_decision(true, ParticipantCount) when ParticipantCount > ?MAX_PARTICIPANTS ->
    {reject, voice_channel_full};
join_p2p_decision(true, ParticipantCount) ->
    {p2p, joiner, ParticipantCount}.

-spec start_decision(boolean(), boolean()) -> join_decision().
start_decision(true, _Pinned) -> {p2p, initiator, 1};
start_decision(false, _Pinned) -> sfu.

-spec guild_join_decision(boolean(), integer(), map()) -> join_decision().
guild_join_decision(Agreed, ChannelId, State) ->
    ChannelVoiceStates = maps:values(
        voice_state_utils:channel_voice_states(ChannelId, voice_state_utils:voice_states(State))
    ),
    HasPendingJoins = lists:any(
        fun(Pending) -> maps:get(channel_id, Pending, undefined) =:= ChannelId end,
        maps:values(guild_voice_connection_pending:pending_voice_connections(State))
    ),
    Pinned = pinned(guild_voice_member:find_channel_by_id(ChannelId, State)),
    join_decision(Agreed, ChannelVoiceStates, HasPendingJoins, Pinned).

-spec admit(join_decision(), token_grant() | term()) -> grant() | sfu | rejection().
admit({p2p, _Role, _Count}, {p2p, _ConnectionId, _IceServers, _MaxParticipants} = Grant) ->
    Grant;
admit({p2p, _Role, _Count}, {declined, ErrorAtom}) ->
    {reject, ErrorAtom};
admit({p2p, initiator, _Count}, _Token) ->
    sfu;
admit({p2p, joiner, _Count}, _Token) ->
    {reject, voice_p2p_unavailable};
admit(sfu, {p2p, _ConnectionId, _IceServers, _MaxParticipants}) ->
    {reject, voice_token_failed};
admit(sfu, {declined, _ErrorAtom}) ->
    {reject, voice_token_failed};
admit(sfu, _Token) ->
    sfu.

-spec invariant_violation([voice_state()]) -> ok | {error, atom()}.
invariant_violation(VoiceStates) ->
    invariant_violation(VoiceStates, undefined).

-spec invariant_violation([voice_state()], term()) -> ok | {error, atom()}.
invariant_violation(VoiceStates, MaxParticipants) ->
    case channel_mode(VoiceStates) of
        mixed ->
            {error, voice_p2p_consent_required};
        p2p ->
            case length(VoiceStates) > participant_cap(MaxParticipants) of
                true -> {error, voice_channel_full};
                false -> ok
            end;
        _ ->
            ok
    end.

-spec participant_cap(term()) -> pos_integer().
participant_cap(MaxParticipants) when
    is_integer(MaxParticipants), MaxParticipants >= 1, MaxParticipants =< ?MAX_PARTICIPANTS
->
    MaxParticipants;
participant_cap(_MaxParticipants) ->
    ?MAX_PARTICIPANTS.

-spec reports_rejection(term()) -> boolean().
reports_rejection(voice_p2p_consent_required) -> true;
reports_rejection(voice_p2p_unavailable) -> true;
reports_rejection(voice_channel_full) -> true;
reports_rejection(_) -> false.

-spec to_sfu(voice_state()) -> voice_state().
to_sfu(VoiceState) ->
    VoiceState#{
        <<"p2p">> => false,
        <<"version">> => voice_state_utils:voice_state_version(VoiceState) + 1
    }.

-spec add_to_token_request(map(), join_decision(), binary() | undefined) -> map().
add_to_token_request(Request, {p2p, Role, ParticipantCount}, CountryCode) ->
    WithP2p = Request#{
        <<"p2p">> => true,
        <<"p2p_initiator">> => Role =:= initiator,
        <<"p2p_participant_count">> => ParticipantCount
    },
    case CountryCode of
        Code when is_binary(Code) -> WithP2p#{<<"country_code">> => Code};
        _ -> WithP2p
    end;
add_to_token_request(Request, _Decision, _CountryCode) ->
    Request.

-spec grant(map()) -> token_grant() | sfu.
grant(
    #{<<"p2p">> := true, <<"connectionId">> := ConnectionId, <<"iceServers">> := IceServers} =
        Data
) when
    is_binary(ConnectionId), is_list(IceServers)
->
    {p2p, ConnectionId,
        [
            maps:with([<<"urls">>], Server)
         || Server <- IceServers, is_map(Server)
        ],
        participant_cap(maps:get(<<"maxParticipants">>, Data, undefined))};
grant(#{<<"p2p">> := false, <<"declineReason">> := <<"channel_full">>}) ->
    {declined, voice_channel_full};
grant(_Data) ->
    sfu.

-spec voice_server_update(binary(), integer(), integer() | null, [map()]) -> map().
voice_server_update(ConnectionId, ChannelId, GuildId, IceServers) ->
    Update = #{
        <<"p2p">> => true,
        <<"ice_servers">> => IceServers,
        <<"connection_id">> => ConnectionId,
        <<"channel_id">> => integer_to_binary(ChannelId)
    },
    put_guild_id(Update, GuildId).

-spec signal_route([voice_state()], binary(), binary()) ->
    {ok, voice_state(), voice_state()} | error.
signal_route(VoiceStates, SessionId, To) ->
    Peers = [VoiceState || VoiceState <- VoiceStates, is_p2p(VoiceState)],
    Senders = [
        Peer
     || Peer <- Peers, maps:get(<<"session_id">>, Peer, undefined) =:= SessionId
    ],
    Targets = [Peer || Peer <- Peers, maps:get(<<"connection_id">>, Peer, undefined) =:= To],
    case {Senders, Targets} of
        {[Sender | _], [Target | _]} when Sender =/= Target -> {ok, Sender, Target};
        _ -> error
    end.

-spec signal_payload(integer() | null, integer(), voice_state(), map()) -> map().
signal_payload(GuildId, ChannelId, Sender, Data) ->
    Payload = #{
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"from">> => maps:get(<<"connection_id">>, Sender),
        <<"user_id">> => maps:get(<<"user_id">>, Sender),
        <<"data">> => Data
    },
    put_guild_id(Payload, GuildId).

-spec put_guild_id(map(), integer() | null) -> map().
put_guild_id(Payload, null) -> Payload;
put_guild_id(Payload, GuildId) -> Payload#{<<"guild_id">> => integer_to_binary(GuildId)}.
