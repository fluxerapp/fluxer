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

-module(guild_voice_state).

-include_lib("fluxer_gateway/include/voice_state.hrl").

-export([get_voice_state/2]).
-export([get_voice_states_list/1]).
-export([update_voice_state_data/9]).
-export([user_matches_voice_state/2]).
-export([create_voice_state/8]).
-export([extract_session_info_from_voice_state/2]).

-type guild_state() :: map().
-type voice_state() :: map().
-type voice_state_map() :: #{binary() => voice_state()}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec get_voice_state(map(), guild_state()) -> {reply, map(), guild_state()}.
get_voice_state(Request, State) ->
    case maps:get(connection_id, Request, null) of
        null ->
            {reply, #{voice_state => null}, State};
        ConnectionId ->
            VoiceStates = voice_state_utils:voice_states(State),
            VoiceState = maps:get(ConnectionId, VoiceStates, null),
            {reply, #{voice_state => VoiceState}, State}
    end.

-spec get_voice_states_list(guild_state()) -> [voice_state()].
get_voice_states_list(State) ->
    maps:values(voice_state_utils:voice_states(State)).

-spec update_voice_state_data(
    binary(),
    binary(),
    voice_flags(),
    map(),
    voice_state(),
    voice_state_map(),
    guild_state(),
    boolean(),
    term()
) -> {reply, map(), guild_state()}.
update_voice_state_data(
    ConnectionId,
    ChannelIdBin,
    Flags,
    Member,
    ExistingVoiceState,
    VoiceStates,
    State,
    NeedsToken,
    ViewerStreamKey
) ->
    #voice_flags{
        self_mute = SelfMute,
        self_deaf = SelfDeaf,
        self_video = SelfVideo,
        self_stream = SelfStream,
        is_mobile = IsMobile
    } = Flags,
    ServerMute = maps:get(<<"mute">>, Member, false),
    ServerDeaf = maps:get(<<"deaf">>, Member, false),
    OldVersion = maps:get(<<"version">>, ExistingVoiceState, 0),
    UpdatedVoiceState = ExistingVoiceState#{
        <<"channel_id">> => ChannelIdBin,
        <<"mute">> => ServerMute,
        <<"deaf">> => ServerDeaf,
        <<"self_mute">> => SelfMute,
        <<"self_deaf">> => SelfDeaf,
        <<"self_video">> => SelfVideo,
        <<"self_stream">> => SelfStream,
        <<"is_mobile">> => IsMobile,
        <<"viewer_stream_key">> => ViewerStreamKey,
        <<"version">> => OldVersion + 1
    },
    NewVoiceStates = maps:put(ConnectionId, UpdatedVoiceState, VoiceStates),
    NewState = maps:put(voice_states, NewVoiceStates, State),
    guild_voice_broadcast:broadcast_voice_state_update(UpdatedVoiceState, NewState, ChannelIdBin),
    Reply =
        case NeedsToken of
            true -> #{success => true, voice_state => UpdatedVoiceState, needs_token => true};
            false -> #{success => true, voice_state => UpdatedVoiceState}
        end,
    {reply, Reply, NewState}.

-spec user_matches_voice_state(voice_state(), integer() | binary()) -> boolean().
user_matches_voice_state(VoiceState, UserId) when is_integer(UserId) ->
    case map_utils:get_integer(VoiceState, <<"user_id">>, undefined) of
        undefined -> false;
        VoiceUserId -> VoiceUserId =:= UserId
    end;
user_matches_voice_state(VoiceState, UserId) when is_binary(UserId) ->
    type_conv:to_binary(map_utils:get_binary(VoiceState, <<"user_id">>, undefined)) =:= UserId;
user_matches_voice_state(_VoiceState, _UserId) ->
    false.

-spec create_voice_state(
    binary(),
    binary(),
    binary(),
    binary(),
    boolean(),
    boolean(),
    voice_flags(),
    term()
) -> voice_state().
create_voice_state(
    GuildIdBin,
    ChannelIdBin,
    UserIdBin,
    ConnectionId,
    ServerMute,
    ServerDeaf,
    Flags,
    ViewerStreamKey
) ->
    #voice_flags{
        self_mute = SelfMute,
        self_deaf = SelfDeaf,
        self_video = SelfVideo,
        self_stream = SelfStream,
        is_mobile = IsMobile
    } = Flags,
    #{
        <<"guild_id">> => GuildIdBin,
        <<"channel_id">> => ChannelIdBin,
        <<"user_id">> => UserIdBin,
        <<"connection_id">> => ConnectionId,
        <<"mute">> => ServerMute,
        <<"deaf">> => ServerDeaf,
        <<"self_mute">> => SelfMute,
        <<"self_deaf">> => SelfDeaf,
        <<"self_video">> => SelfVideo,
        <<"self_stream">> => SelfStream,
        <<"is_mobile">> => IsMobile,
        <<"viewer_stream_key">> => ViewerStreamKey,
        <<"version">> => 0
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
        member => maps:get(<<"member">>, VoiceState, #{})
    }.

-ifdef(TEST).

user_matches_voice_state_integer_test() ->
    VoiceState = #{<<"user_id">> => <<"10">>},
    ?assert(user_matches_voice_state(VoiceState, 10)),
    ?assertNot(user_matches_voice_state(VoiceState, 11)).

update_voice_state_data_updates_version_test() ->
    VoiceState = #{<<"version">> => 1, <<"channel_id">> => <<"1">>},
    Member = #{<<"mute">> => true, <<"deaf">> => false},
    Flags = #voice_flags{
        self_mute = true,
        self_deaf = false,
        self_video = false,
        self_stream = false,
        is_mobile = false
    },
    {reply, #{voice_state := Updated}, _} =
        update_voice_state_data(
            <<"conn">>,
            <<"2">>,
            Flags,
            Member,
            VoiceState,
            #{<<"conn">> => VoiceState},
            #{voice_states => #{}},
            false,
            null
        ),
    ?assertEqual(2, maps:get(<<"version">>, Updated)),
    ?assertEqual(<<"2">>, maps:get(<<"channel_id">>, Updated)).

-endif.
