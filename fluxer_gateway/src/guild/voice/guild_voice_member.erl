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

-module(guild_voice_member).

-export([update_member_voice/2]).
-export([find_member_by_user_id/2]).
-export([find_channel_by_id/2]).

-type guild_state() :: map().
-type guild_reply(T) :: {reply, T, guild_state()}.
-type member() :: map().
-type voice_state() :: map().
-type request() :: #{
    user_id := integer(),
    mute := boolean(),
    deaf := boolean()
}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec update_member_voice(request(), guild_state()) -> guild_reply(map()).
update_member_voice(Request, State) ->
    #{user_id := UserId, mute := Mute, deaf := Deaf} = Request,
    VoiceStates = voice_state_utils:voice_states(State),
    GuildId = map_utils:get_integer(State, id, 0),

    case find_member_by_user_id(UserId, State) of
        undefined ->
            {reply, gateway_errors:error(voice_member_not_found), State};
        Member ->
            UpdatedMember = set_member_voice_flags(Member, Mute, Deaf),
            StateWithUpdatedMember = store_member(UpdatedMember, State),
            UserVoiceStates = user_voice_states(UserId, VoiceStates),

            case maps:size(UserVoiceStates) of
                0 ->
                    {reply, #{success => true}, StateWithUpdatedMember};
                _ ->
                    maybe_enforce_voice_states(
                        GuildId, UserId, Mute, Deaf, UserVoiceStates, State
                    ),
                    {NewVoiceStates, UpdatedStates} =
                        update_voice_states(UserVoiceStates, VoiceStates, Mute, Deaf),
                    FinalState = maps:put(voice_states, NewVoiceStates, StateWithUpdatedMember),
                    broadcast_voice_state_updates(UpdatedStates, FinalState),
                    {reply, #{success => true}, FinalState}
            end
    end.

find_member_by_user_id(UserId, State) ->
    guild_permissions:find_member_by_user_id(UserId, State).

find_channel_by_id(ChannelId, State) ->
    guild_permissions:find_channel_by_id(ChannelId, State).

enforce_participant_state_in_livekit(GuildId, ChannelId, UserId, Mute, Deaf) ->
    Req = voice_utils:build_update_participant_rpc_request(GuildId, ChannelId, UserId, Mute, Deaf),
    case rpc_client:call(Req) of
        {ok, _Data} ->
            logger:debug(
                "[guild_voice_member] Enforced participant state in LiveKit ~p",
                [
                    [
                        {guildId, GuildId},
                        {channelId, ChannelId},
                        {userId, UserId},
                        {mute, Mute},
                        {deaf, Deaf}
                    ]
                ]
            ),
            ok;
        {error, Reason} ->
            logger:warning(
                "[guild_voice_member] Failed to enforce participant state in LiveKit ~p",
                [
                    [
                        {guildId, GuildId},
                        {channelId, ChannelId},
                        {userId, UserId},
                        {mute, Mute},
                        {deaf, Deaf},
                        {error, Reason}
                    ]
                ]
            ),
            ok
    end.

-spec guild_data(guild_state()) -> map().
guild_data(State) ->
    map_utils:ensure_map(map_utils:get_safe(State, data, #{})).

-spec guild_members(guild_state()) -> [member()].
guild_members(State) ->
    map_utils:ensure_list(maps:get(<<"members">>, guild_data(State), [])).

-spec member_user_id(member()) -> integer() | undefined.
member_user_id(Member) when is_map(Member) ->
    User = map_utils:ensure_map(maps:get(<<"user">>, Member, #{})),
    map_utils:get_integer(User, <<"id">>, undefined);
member_user_id(_) ->
    undefined.

-spec set_member_voice_flags(member(), boolean(), boolean()) -> member().
set_member_voice_flags(Member, Mute, Deaf) ->
    Member#{<<"mute">> => Mute, <<"deaf">> => Deaf}.

-spec store_member(member(), guild_state()) -> guild_state().
store_member(Member, State) ->
    case member_user_id(Member) of
        undefined ->
            State;
        TargetId ->
            Data = guild_data(State),
            Members = guild_members(State),
            UpdatedMembers = lists:map(
                fun(Current) ->
                    case member_user_id(Current) of
                        TargetId -> Member;
                        _ -> Current
                    end
                end,
                Members
            ),
            UpdatedData = maps:put(<<"members">>, UpdatedMembers, Data),
            maps:put(data, UpdatedData, State)
    end.

-spec user_voice_states(integer(), map()) -> map().
user_voice_states(UserId, VoiceStates) when is_integer(UserId), is_map(VoiceStates) ->
    maps:filter(
        fun(_ConnId, VoiceState) ->
            voice_state_utils:voice_state_user_id(VoiceState) =:= UserId
        end,
        VoiceStates
    );
user_voice_states(_UserId, _VoiceStates) ->
    #{}.

-spec update_voice_states(map(), map(), boolean(), boolean()) -> {map(), [voice_state()]}.
update_voice_states(UserVoiceStates, VoiceStates, Mute, Deaf) ->
    maps:fold(
        fun(ConnId, VoiceState, {AccVoiceStates, AccUpdated}) ->
            UpdatedVoiceState = update_voice_state_flags(VoiceState, Mute, Deaf),
            {maps:put(ConnId, UpdatedVoiceState, AccVoiceStates), [UpdatedVoiceState | AccUpdated]}
        end,
        {VoiceStates, []},
        UserVoiceStates
    ).

-spec update_voice_state_flags(voice_state(), boolean(), boolean()) -> voice_state().
update_voice_state_flags(VoiceState, Mute, Deaf) ->
    OldVersion = maps:get(<<"version">>, VoiceState, 0),
    VoiceState#{<<"mute">> => Mute, <<"deaf">> => Deaf, <<"version">> => OldVersion + 1}.

-spec maybe_enforce_voice_states(integer(), integer(), boolean(), boolean(), map(), guild_state()) ->
    ok.
maybe_enforce_voice_states(GuildId, UserId, Mute, Deaf, VoiceStates, State) ->
    maps:foreach(
        fun(_ConnId, VoiceState) ->
            case voice_state_utils:voice_state_channel_id(VoiceState) of
                ChannelId when is_integer(ChannelId) ->
                    dispatch_livekit_enforcement(GuildId, ChannelId, UserId, Mute, Deaf, State);
                _ ->
                    ok
            end
        end,
        VoiceStates
    ).

-spec dispatch_livekit_enforcement(
    integer(), integer(), integer(), boolean(), boolean(), guild_state()
) -> ok.
dispatch_livekit_enforcement(GuildId, ChannelId, UserId, Mute, Deaf, State) ->
    case maps:get(test_livekit_fun, State, undefined) of
        Fun when is_function(Fun, 5) ->
            Fun(GuildId, ChannelId, UserId, Mute, Deaf);
        _ ->
            spawn(fun() ->
                enforce_participant_state_in_livekit(GuildId, ChannelId, UserId, Mute, Deaf)
            end)
    end.

-spec broadcast_voice_state_updates([voice_state()], guild_state()) -> ok.
broadcast_voice_state_updates([], _State) ->
    ok;
broadcast_voice_state_updates(UpdatedStates, State) ->
    lists:foreach(
        fun(UpdatedVoiceState) ->
            ChannelIdBin = maps:get(<<"channel_id">>, UpdatedVoiceState, null),
            guild_voice_broadcast:broadcast_voice_state_update(
                UpdatedVoiceState, State, ChannelIdBin
            )
        end,
        UpdatedStates
    ).

-ifdef(TEST).

update_member_voice_updates_member_flags_test() ->
    State = voice_member_test_state(#{}),
    Request = #{user_id => 10, mute => true, deaf => false},
    {reply, #{success := true}, UpdatedState} = update_member_voice(Request, State),
    Member = find_member_by_user_id(10, UpdatedState),
    ?assertEqual(true, maps:get(<<"mute">>, Member)),
    ?assertEqual(false, maps:get(<<"deaf">>, Member)).

update_member_voice_updates_voice_states_test() ->
    Self = self(),
    VoiceState = voice_state_fixture(10, 500),
    TestFun = fun(GuildId, ChannelId, UserId, Mute, Deaf) ->
        Self ! {enforced, GuildId, ChannelId, UserId, Mute, Deaf}
    end,
    State = voice_member_test_state(#{
        voice_states => #{<<"conn">> => VoiceState},
        test_livekit_fun => TestFun
    }),
    Request = #{user_id => 10, mute => true, deaf => true},
    {reply, #{success := true}, UpdatedState} = update_member_voice(Request, State),
    UpdatedVoiceStates = maps:get(voice_states, UpdatedState),
    UpdatedVoiceState = maps:get(<<"conn">>, UpdatedVoiceStates),
    ?assertEqual(true, maps:get(<<"mute">>, UpdatedVoiceState)),
    ?assertEqual(true, maps:get(<<"deaf">>, UpdatedVoiceState)),
    ?assertEqual(1, maps:get(<<"version">>, UpdatedVoiceState)),
    receive
        {enforced, 42, 500, 10, true, true} -> ok
    after 100 ->
        ?assert(false)
    end.

voice_member_test_state(Overrides) ->
    BaseData = #{
        <<"members">> => [member_fixture(10)]
    },
    BaseState = #{
        id => 42,
        data => BaseData,
        voice_states => #{}
    },
    maps:merge(BaseState, Overrides).

member_fixture(UserId) ->
    #{
        <<"user">> => #{<<"id">> => integer_to_binary(UserId)},
        <<"mute">> => false,
        <<"deaf">> => false
    }.

voice_state_fixture(UserId, ChannelId) ->
    #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"channel_id">> => integer_to_binary(ChannelId),
        <<"connection_id">> => <<"test-conn">>,
        <<"mute">> => false,
        <<"deaf">> => false,
        <<"version">> => 0,
        <<"member">> => member_fixture(UserId)
    }.

-endif.
