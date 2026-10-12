%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_voice_member).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-typing([eqwalizer]).

-export([update_member_voice/2]).

-export_type([
    guild_state/0,
    guild_reply/1,
    member/0,
    voice_state/0,
    request/0
]).

-type guild_state() :: map().
-type guild_reply(T) :: {reply, T | {error, atom(), atom()}, guild_state()}.
-type member() :: map().
-type voice_state() :: map().
-type request() :: #{
    user_id := integer(),
    mute := boolean(),
    deaf := boolean()
}.

-spec update_member_voice(request(), guild_state()) -> guild_reply(map()).
update_member_voice(Request, State) ->
    #{user_id := UserId, mute := Mute, deaf := Deaf} = Request,
    VoiceStates = voice_state_utils:voice_states(State),
    GuildId = state_guild_id(State),
    case guild_permissions_check:find_member_by_user_id(UserId, State) of
        undefined ->
            {reply, gateway_errors:error(voice_member_not_found), State};
        Member ->
            UpdatedMember = set_member_voice_flags(Member, Mute, Deaf),
            StateWithUpdatedMember = store_member(UpdatedMember, State),
            UserVoiceStates = user_voice_states(UserId, VoiceStates),
            apply_member_voice_update(
                UserId,
                GuildId,
                Mute,
                Deaf,
                UserVoiceStates,
                VoiceStates,
                State,
                StateWithUpdatedMember
            )
    end.

-spec apply_member_voice_update(
    integer(),
    integer() | undefined,
    boolean(),
    boolean(),
    map(),
    map(),
    guild_state(),
    guild_state()
) -> guild_reply(map()).
apply_member_voice_update(
    _UserId,
    _GuildId,
    _Mute,
    _Deaf,
    UserVoiceStates,
    _VoiceStates,
    _State,
    NewState
) when map_size(UserVoiceStates) =:= 0 ->
    {reply, #{success => true}, NewState};
apply_member_voice_update(
    UserId,
    GuildId,
    Mute,
    Deaf,
    UserVoiceStates,
    VoiceStates,
    State,
    NewState
) ->
    maybe_enforce_voice_states(GuildId, UserId, Mute, Deaf, UserVoiceStates, State),
    {NewVS, Updated} = update_voice_states(UserVoiceStates, VoiceStates, Mute, Deaf),
    FinalState = NewState#{voice_states => NewVS},
    broadcast_voice_state_updates(Updated, FinalState),
    {reply, #{success => true}, FinalState}.

-spec enforce_participant_state_in_livekit(
    integer(), integer(), integer(), boolean(), boolean(), voice_utils:voice_permissions()
) ->
    ok.
enforce_participant_state_in_livekit(GuildId, ChannelId, UserId, Mute, Deaf, VoicePermissions) ->
    Req = voice_utils:build_update_participant_rpc_request(
        GuildId, ChannelId, UserId, Mute, Deaf, VoicePermissions
    ),
    case rpc_client:call(Req) of
        {ok, _Data} ->
            ok;
        {error, _Reason} ->
            ok
    end.

-spec guild_data(guild_state()) -> map().
guild_data(State) ->
    map_utils:ensure_map(map_utils:get_safe(State, data, #{})).

-spec member_user_id(member()) -> integer() | undefined.
member_user_id(Member) when is_map(Member) ->
    User = map_utils:ensure_map(maps:get(<<"user">>, Member, #{})),
    snowflake_id:parse_maybe(
        maps:get(<<"id">>, User, undefined)
    ).

-spec set_member_voice_flags(member(), boolean(), boolean()) -> member().
set_member_voice_flags(Member, Mute, Deaf) ->
    Member#{<<"mute">> => Mute, <<"deaf">> => Deaf}.

-spec store_member(member(), guild_state()) -> guild_state().
store_member(Member, State) ->
    case member_user_id(Member) of
        undefined ->
            State;
        _TargetId ->
            Data = guild_data(State),
            UpdatedData = guild_data_index_members:put_member(Member, Data),
            State#{data => UpdatedData}
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
            {AccVoiceStates#{ConnId => UpdatedVoiceState}, [UpdatedVoiceState | AccUpdated]}
        end,
        {VoiceStates, []},
        UserVoiceStates
    ).

-spec update_voice_state_flags(voice_state(), boolean(), boolean()) -> voice_state().
update_voice_state_flags(VoiceState, Mute, Deaf) ->
    OldVersion = voice_state_utils:voice_state_version(VoiceState),
    voice_state_utils:complete_voice_state(VoiceState#{
        <<"member">> => sync_embedded_member(VoiceState, Mute, Deaf),
        <<"mute">> => Mute,
        <<"deaf">> => Deaf,
        <<"version">> => OldVersion + 1
    }).

-spec sync_embedded_member(voice_state(), boolean(), boolean()) -> member() | null.
sync_embedded_member(VoiceState, Mute, Deaf) ->
    case maps:get(<<"member">>, VoiceState, null) of
        Member when is_map(Member), map_size(Member) > 0 ->
            Member#{<<"mute">> => Mute, <<"deaf">> => Deaf};
        _ ->
            null
    end.

-spec maybe_enforce_voice_states(
    integer() | undefined, integer(), boolean(), boolean(), map(), guild_state()
) ->
    ok.
maybe_enforce_voice_states(GuildId, UserId, Mute, Deaf, VoiceStates, State) ->
    maps:foreach(
        fun(_ConnId, VoiceState) ->
            maybe_enforce_voice_state(GuildId, UserId, Mute, Deaf, VoiceState, State)
        end,
        VoiceStates
    ).

-spec maybe_enforce_voice_state(
    integer() | undefined, integer(), boolean(), boolean(), voice_state(), guild_state()
) -> ok.
maybe_enforce_voice_state(GuildId, UserId, Mute, Deaf, VoiceState, State) ->
    case {GuildId, voice_state_utils:voice_state_channel_id(VoiceState)} of
        {ResolvedGuildId, ChannelId} when is_integer(ResolvedGuildId), is_integer(ChannelId) ->
            VoicePermissions = voice_utils:compute_voice_permissions(UserId, ChannelId, State),
            dispatch_livekit_enforcement(
                ResolvedGuildId, ChannelId, UserId, Mute, Deaf, VoicePermissions, State
            );
        _ ->
            ok
    end.

-spec dispatch_livekit_enforcement(
    integer(),
    integer(),
    integer(),
    boolean(),
    boolean(),
    voice_utils:voice_permissions(),
    guild_state()
) -> ok.
dispatch_livekit_enforcement(GuildId, ChannelId, UserId, Mute, Deaf, VoicePermissions, State) ->
    case maps:get(test_livekit_fun, State, undefined) of
        Fun when is_function(Fun, 5) ->
            _ = Fun(GuildId, ChannelId, UserId, Mute, Deaf),
            ok;
        Fun when is_function(Fun, 6) ->
            _ = Fun(GuildId, ChannelId, UserId, Mute, Deaf, VoicePermissions),
            ok;
        _ ->
            spawn_livekit_enforcement(
                GuildId, ChannelId, UserId, Mute, Deaf, VoicePermissions
            )
    end.

-spec spawn_livekit_enforcement(
    integer(),
    integer(),
    integer(),
    boolean(),
    boolean(),
    voice_utils:voice_permissions()
) -> ok.
spawn_livekit_enforcement(GuildId, ChannelId, UserId, Mute, Deaf, VoicePermissions) ->
    spawn(fun() ->
        enforce_participant_state_in_livekit(
            GuildId, ChannelId, UserId, Mute, Deaf, VoicePermissions
        )
    end),
    ok.

-spec state_guild_id(guild_state()) -> integer() | undefined.
state_guild_id(State) ->
    snowflake_id:parse_maybe(
        maps:get(id, State, undefined)
    ).

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
    #{<<"mute">> := true, <<"deaf">> := false} = guild_permissions_check:find_member_by_user_id(
        10, UpdatedState
    ).

update_member_voice_updates_voice_states_test() ->
    VoiceState = voice_state_fixture(10, 500),
    State = voice_member_test_state(#{
        voice_states => #{<<"conn">> => VoiceState},
        test_livekit_fun => fun livekit_test_fun/6
    }),
    Request = #{user_id => 10, mute => true, deaf => true},
    {reply, #{success := true}, UpdatedState} = update_member_voice(Request, State),
    UpdatedVoiceStates = maps:get(voice_states, UpdatedState),
    UpdatedVoiceState = maps:get(<<"conn">>, UpdatedVoiceStates),
    ?assertEqual(true, maps:get(<<"mute">>, UpdatedVoiceState)),
    ?assertEqual(true, maps:get(<<"deaf">>, UpdatedVoiceState)),
    ?assertEqual(1, maps:get(<<"version">>, UpdatedVoiceState)),
    receive
        {enforced, 42, 500, 10, true, true, VoicePermissions} ->
            ?assertEqual(false, maps:get(can_speak, VoicePermissions)),
            ?assertEqual(false, maps:get(can_stream, VoicePermissions)),
            ?assertEqual(false, maps:get(can_video, VoicePermissions))
    after 100 ->
        ?assert(false)
    end.

livekit_test_fun(GuildId, ChannelId, UserId, Mute, Deaf, VoicePermissions) ->
    self() ! {enforced, GuildId, ChannelId, UserId, Mute, Deaf, VoicePermissions}.

update_member_voice_member_not_found_test() ->
    State = voice_member_test_state(#{}),
    Request = #{user_id => 999, mute => true, deaf => false},
    {reply, Error, _} = update_member_voice(Request, State),
    ?assertEqual({error, not_found, voice_member_not_found}, Error).

voice_member_test_state(Overrides) ->
    BaseData = #{
        <<"members">> => #{
            10 => member_fixture(10)
        }
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
        <<"version">> => voice_state_utils:initial_voice_state_version(),
        <<"member">> => member_fixture(UserId)
    }.

-endif.
