%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_move).
-typing([eqwalizer]).

-export([move_member/2]).
-export([send_voice_server_updates_for_move/4]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export_type([
    guild_state/0,
    voice_state/0,
    voice_state_map/0,
    voice_reply/0,
    move_request/0
]).

-type guild_state() :: map().
-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.
-type voice_reply() :: {reply, map() | {error, atom(), atom()}, guild_state()}.
-type move_request() :: #{
    user_id := integer(),
    moderator_id := integer(),
    channel_id := integer() | null,
    connection_id => binary() | null,
    mute := boolean(),
    deaf := boolean()
}.

-spec move_member(move_request(), guild_state()) -> voice_reply().
move_member(Request, State) ->
    {UserId, ModeratorId, ChannelId, ConnectionId} = parse_move_request(Request),
    log_move_request(UserId, ModeratorId, ChannelId, ConnectionId),
    VoiceStates = voice_state_utils:voice_states(State),
    UserVoiceStates = find_user_voice_states(UserId, VoiceStates),
    execute_move(
        UserVoiceStates, ConnectionId, UserId, ModeratorId, ChannelId, VoiceStates, State
    ).

-spec parse_move_request(move_request()) ->
    {integer(), integer(), integer() | null, binary() | null}.
parse_move_request(
    #{user_id := UserId, moderator_id := ModeratorId, channel_id := ChannelIdRaw} = Req
) ->
    {UserId, ModeratorId, normalize_channel_id(ChannelIdRaw),
        maps:get(connection_id, Req, null)}.

-spec log_move_request(integer(), integer(), integer() | null, binary() | null) -> ok.
log_move_request(UserId, ModeratorId, ChannelId, ConnectionId) ->
    logger:debug(
        "Handling voice move_member request",
        #{
            user_id => UserId,
            moderator_id => ModeratorId,
            channel_id => ChannelId,
            connection_id => ConnectionId
        }
    ).

-spec execute_move(
    voice_state_map(),
    binary() | null,
    integer(),
    integer(),
    integer() | null,
    voice_state_map(),
    guild_state()
) -> voice_reply().
execute_move(UserVoiceStates, _ConnId, _UserId, _ModId, _ChId, _VS, State) when
    map_size(UserVoiceStates) =:= 0
->
    {reply, gateway_errors:error(voice_user_not_in_voice), State};
execute_move(UserVoiceStates, ConnectionId, UserId, ModeratorId, ChannelId, VoiceStates, State) ->
    ConnectionsToMove = select_connections_to_move(
        ConnectionId, UserId, VoiceStates, UserVoiceStates
    ),
    logger:debug(
        "Selected voice connections to move",
        #{
            user_id => UserId,
            connection_id => ConnectionId,
            connections_to_move_count => maps:size(ConnectionsToMove)
        }
    ),
    guild_voice_move_execute:handle_move(
        ConnectionsToMove, ChannelId, UserId, ModeratorId, ConnectionId, VoiceStates, State
    ).

-spec find_user_voice_states(integer(), voice_state_map()) -> voice_state_map().
find_user_voice_states(UserId, VoiceStates) ->
    maps:filter(
        fun(_ConnId, VoiceState) ->
            voice_state_utils:voice_state_user_id(VoiceState) =:= UserId
        end,
        VoiceStates
    ).

-spec select_connections_to_move(
    binary() | null, integer(), voice_state_map(), voice_state_map()
) -> voice_state_map().
select_connections_to_move(null, _UserId, _VoiceStates, UserVoiceStates) ->
    UserVoiceStates;
select_connections_to_move(ConnectionId, UserId, VoiceStates, _UserVoiceStates) ->
    case maps:get(ConnectionId, VoiceStates, undefined) of
        undefined ->
            #{};
        VoiceState ->
            select_matching_connection(ConnectionId, UserId, VoiceState)
    end.

-spec select_matching_connection(binary(), integer(), voice_state()) -> voice_state_map().
select_matching_connection(ConnectionId, UserId, VoiceState) ->
    case voice_state_utils:voice_state_user_id(VoiceState) of
        UserId -> #{ConnectionId => VoiceState};
        _ -> #{}
    end.

-spec normalize_channel_id(term()) -> integer() | null.
normalize_channel_id(null) ->
    null;
normalize_channel_id(Value) ->
    case normalize_channel_id_integer(Value) of
        Int when is_integer(Int), Int > 0 -> Int;
        _ -> null
    end.

-spec normalize_channel_id_integer(term()) -> integer() | undefined.
normalize_channel_id_integer(Value) when
    is_integer(Value); is_binary(Value); is_list(Value); is_atom(Value)
->
    type_conv:to_integer(Value);
normalize_channel_id_integer(_) ->
    undefined.

-spec send_voice_server_updates_for_move(integer(), integer(), [map()], pid()) -> ok.
send_voice_server_updates_for_move(GuildId, ChannelId, SessionDataList, GuildPid) ->
    CapturedGuildId = GuildId,
    CapturedChannelId = ChannelId,
    CapturedList = SessionDataList,
    CapturedPid = GuildPid,
    spawn(fun() ->
        send_captured_voice_server_updates(
            CapturedGuildId, CapturedChannelId, CapturedList, CapturedPid
        )
    end),
    ok.

-spec send_captured_voice_server_updates(integer(), integer(), [map()], pid()) -> ok.
send_captured_voice_server_updates(GuildId, ChannelId, SessionDataList, GuildPid) ->
    lists:foreach(
        fun(SessionInfo) ->
            guild_voice_move_execute:send_single_voice_server_update(
                GuildId, ChannelId, SessionInfo, GuildPid
            )
        end,
        SessionDataList
    ).

-ifdef(TEST).

move_member_user_not_in_voice_test() ->
    Request = #{
        user_id => 10,
        moderator_id => 20,
        channel_id => null,
        mute => false,
        deaf => false
    },
    State = test_state(#{}),
    {reply, {error, not_found, voice_user_not_in_voice}, _} = move_member(Request, State).

select_connections_to_move_specific_connection_test() ->
    VoiceStates = #{
        <<"conn-a">> => voice_state_fixture(10, 100, <<"conn-a">>),
        <<"conn-b">> => voice_state_fixture(11, 101, <<"conn-b">>)
    },
    Selected = select_connections_to_move(<<"conn-b">>, 11, VoiceStates, #{}),
    ?assertEqual(#{<<"conn-b">> => maps:get(<<"conn-b">>, VoiceStates)}, Selected),
    ?assertEqual(#{}, select_connections_to_move(<<"conn-b">>, 10, VoiceStates, #{})).

test_state(VoiceStates) ->
    #{
        id => 1,
        data => #{
            <<"members">> => [],
            <<"channels">> => []
        },
        voice_states => VoiceStates
    }.

voice_state_fixture(UserId, ChannelId, ConnId) ->
    #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"connection_id">> => ConnId,
        <<"member">> => #{
            <<"user">> => #{<<"id">> => integer_to_binary(UserId)}
        }
    }.

-endif.
