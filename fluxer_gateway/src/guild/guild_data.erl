%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data).
-typing([eqwalizer]).

-export([get_guild_data/2]).
-export([get_auth_context/2]).
-export([get_guild_member/2]).
-export([get_guild_members_batch/2]).
-export([has_member/2]).
-export([list_guild_members/2]).
-export([search_guild_members/2]).
-export([get_vanity_url_channel/1]).
-export([get_first_viewable_text_channel/1]).
-export([get_guild_state/2]).
-export([build_connect_snapshot/2]).
-export([fetch_latest_voice_states/1]).
-export([find_everyone_viewable_text_channel/2]).

-type guild_state() :: map().
-type guild_reply(T) :: {reply, T, guild_state()}.
-type user_id() :: integer().
-type guild_id() :: integer().

-define(CONNECT_SNAPSHOT_HEAVY_MEMBER_KEYS, [
    <<"members">>, members_normalized, <<"member_role_index">>, members_sorted_ids
]).
-define(CONNECT_SNAPSHOT_HEAVY_SESSION_KEYS, [active_guilds, user_roles, viewable_channels]).

-export_type([guild_state/0, guild_reply/1, user_id/0]).

-spec get_guild_data(map(), guild_state()) -> guild_reply(map()).
get_guild_data(#{user_id := UserId}, State) ->
    Data = guild_data_index:ensure_data_map(State),
    case UserId of
        null ->
            GuildData = build_complete_guild_data(Data, State),
            {reply, #{guild_data => GuildData}, State};
        _ ->
            get_guild_data_for_user(UserId, Data, State)
    end.

-spec get_auth_context(map(), guild_state()) -> guild_reply(map()).
get_auth_context(#{user_id := UserId, channel_id := ChannelId}, State) ->
    Data = guild_data_index:ensure_data_map(State),
    case UserId of
        null ->
            {reply, #{auth_context => build_auth_context(ChannelId, Data, State)}, State};
        _ ->
            get_auth_context_for_member(UserId, ChannelId, Data, State)
    end.

-spec get_auth_context_for_member(user_id(), integer() | null, map(), guild_state()) ->
    guild_reply(map()).
get_auth_context_for_member(UserId, ChannelId, Data, State) ->
    case guild_data_index:get_member(UserId, Data) of
        undefined ->
            {reply, #{auth_context => null, error_reason => <<"forbidden">>}, State};
        _Member ->
            {reply, #{auth_context => build_auth_context(ChannelId, Data, State)}, State}
    end.

-spec build_auth_context(integer() | null, map(), guild_state()) -> map().
build_auth_context(ChannelId, Data, State) ->
    #{
        <<"guild">> => build_auth_guild(Data, State),
        <<"parent_channel">> => find_channel(ChannelId, Data)
    }.

-spec build_auth_guild(map(), guild_state()) -> map().
build_auth_guild(Data, State) ->
    GuildProperties = map_utils:ensure_map(maps:get(<<"guild">>, Data, #{})),
    GuildProperties#{
        <<"roles">> => map_utils:ensure_list(maps:get(<<"roles">>, Data, [])),
        <<"member_count">> => maps:get(
            member_count, State, guild_data_index:member_count(Data)
        ),
        <<"online_count">> => guild_member_list:get_online_count(State)
    }.

-spec find_channel(integer() | null, map()) -> map() | null.
find_channel(null, _Data) ->
    null;
find_channel(ChannelId, Data) ->
    maps:get(ChannelId, guild_data_index:channel_index(Data), null).

-spec get_guild_member(map(), guild_state()) -> guild_reply(map()).
get_guild_member(Request, State) ->
    guild_data_members:get_guild_member(Request, State).

-spec get_guild_members_batch(map(), guild_state()) -> guild_reply(map()).
get_guild_members_batch(Request, State) ->
    guild_data_members:get_guild_members_batch(Request, State).

-spec has_member(map(), guild_state()) -> guild_reply(map()).
has_member(Request, State) ->
    guild_data_members:has_member(Request, State).

-spec list_guild_members(map(), guild_state()) -> guild_reply(map()).
list_guild_members(Request, State) ->
    guild_data_members:list_guild_members(Request, State).

-spec search_guild_members(map(), guild_state()) -> guild_reply(map()).
search_guild_members(Request, State) ->
    guild_data_members:search_guild_members(Request, State).

-spec get_vanity_url_channel(guild_state()) -> guild_reply(map()).
get_vanity_url_channel(State) ->
    get_everyone_viewable_text_channel(State).

-spec get_first_viewable_text_channel(guild_state()) -> guild_reply(map()).
get_first_viewable_text_channel(State) ->
    get_everyone_viewable_text_channel(State).

-spec get_everyone_viewable_text_channel(guild_state()) -> guild_reply(map()).
get_everyone_viewable_text_channel(State) ->
    Channels = guild_data_channels:channels_from_state(State),
    EveryoneChannelId = guild_data_channels:find_everyone_viewable_text_channel(
        Channels, State
    ),
    {reply, #{channel_id => EveryoneChannelId}, State}.

-spec find_everyone_viewable_text_channel([map()], guild_state()) -> integer() | null.
find_everyone_viewable_text_channel(Channels, State) ->
    guild_data_channels:find_everyone_viewable_text_channel(Channels, State).

-spec get_guild_state(user_id(), guild_state()) -> map().
get_guild_state(UserId, State) ->
    Data = guild_data_index:ensure_data_map(State),
    GuildId = guild_id(State),
    AllChannels = guild_data_channels:channels_from_data(Data),
    Member = guild_data_members:find_member_by_user_id(UserId, State),
    {ViewableChannels, JoinedAt} = guild_data_channels:derive_member_view(
        UserId, Member, State, AllChannels
    ),
    OnlineCount = guild_member_list:get_online_count(State),
    OwnMemberList = own_member_list(Member),
    StateWithVoice = fetch_latest_voice_states(State),
    AllVoiceStates = guild_voice:get_voice_states_list(StateWithVoice),
    ViewableChannelIds = channel_id_set(ViewableChannels),
    VoiceStates = filter_voice_states(AllVoiceStates, ViewableChannelIds),
    VoiceMembers = resolve_voice_members(VoiceStates, Data),
    Members = guild_data_channels:merge_members(OwnMemberList, VoiceMembers),
    MemberCount = maps:get(member_count, State, guild_data_index:member_count(Data)),
    build_guild_state_map(
        GuildId,
        Data,
        ViewableChannels,
        Members,
        MemberCount,
        OnlineCount,
        VoiceStates,
        JoinedAt
    ).

-spec build_connect_snapshot(map(), guild_state()) -> map().
build_connect_snapshot(Item, State) ->
    Base = maps:with(
        [
            id,
            data,
            sessions,
            member_count,
            voice_server_pid,
            voice_states,
            member_list_engine,
            virtual_channel_access
        ],
        State
    ),
    project_snapshot_sessions(maybe_trim_connect_snapshot(Item, Base, State)).

-spec project_snapshot_sessions(map()) -> map().
project_snapshot_sessions(#{sessions := Sessions} = Snapshot) when is_map(Sessions) ->
    Projected = maps:map(
        fun(_SessionId, SessionData) -> project_snapshot_session(SessionData) end,
        Sessions
    ),
    Snapshot#{sessions => Projected};
project_snapshot_sessions(Snapshot) ->
    Snapshot.

-spec project_snapshot_session(term()) -> term().
project_snapshot_session(SessionData) when is_map(SessionData) ->
    maps:without(?CONNECT_SNAPSHOT_HEAVY_SESSION_KEYS, SessionData);
project_snapshot_session(SessionData) ->
    SessionData.

-spec maybe_trim_connect_snapshot(map(), map(), guild_state()) -> map().
maybe_trim_connect_snapshot(Item, Base, State) ->
    case should_trim_connect_snapshot(State) of
        true -> trim_connect_snapshot(Item, Base);
        false -> Base
    end.

-spec should_trim_connect_snapshot(guild_state()) -> boolean().
should_trim_connect_snapshot(State) ->
    case connect_snapshot_trim_member_threshold() of
        undefined ->
            false;
        Threshold ->
            member_count_at_least(Threshold, State) andalso has_members_ets(State)
    end.

-spec member_count_at_least(pos_integer(), guild_state()) -> boolean().
member_count_at_least(Threshold, State) ->
    case maps:get(member_count, State, undefined) of
        Count when is_integer(Count) -> Count >= Threshold;
        _ -> false
    end.

-spec connect_snapshot_trim_member_threshold() -> pos_integer() | undefined.
connect_snapshot_trim_member_threshold() ->
    case
        application:get_env(fluxer_gateway, connect_snapshot_trim_member_threshold, undefined)
    of
        N when is_integer(N), N > 0 -> N;
        _ -> undefined
    end.

-spec has_members_ets(guild_state()) -> boolean().
has_members_ets(#{data := #{members_ets := Tab}}) -> is_reference(Tab);
has_members_ets(_) -> false.

-spec trim_connect_snapshot(map(), map()) -> map().
trim_connect_snapshot(Item, #{data := Data} = Base) when is_map(Data) ->
    Retained = retained_member_map(Item, Base, Data),
    Trimmed = maps:without(?CONNECT_SNAPSHOT_HEAVY_MEMBER_KEYS, Data),
    Base#{
        data => Trimmed#{
            <<"members">> => Retained,
            members_normalized => Retained,
            <<"member_role_index">> =>
                guild_data_index_members:build_member_role_index(Retained)
        }
    };
trim_connect_snapshot(_Item, Base) ->
    Base.

-spec retained_member_map(map(), map(), map()) -> #{integer() => map()}.
retained_member_map(Item, Base, Data) ->
    UserIds = [connect_user_id(Item) | voice_state_user_ids(Base)],
    lists:foldl(
        fun(UserId, Acc) -> retain_member(UserId, Data, Acc) end,
        #{},
        UserIds
    ).

-spec retain_member(term(), map(), #{integer() => map()}) -> #{integer() => map()}.
retain_member(UserId, Data, Acc) when is_integer(UserId) ->
    case guild_data_index_members:get_member_ets(UserId, Data) of
        Member when is_map(Member) -> Acc#{UserId => Member};
        _ -> Acc
    end;
retain_member(_UserId, _Data, Acc) ->
    Acc.

-spec connect_user_id(map()) -> integer() | undefined.
connect_user_id(Item) ->
    Request = maps:get(request, Item, #{}),
    case maps:get(user_id, Request, undefined) of
        UserId when is_integer(UserId) -> UserId;
        _ -> undefined
    end.

-spec voice_state_user_ids(map()) -> [integer()].
voice_state_user_ids(Base) ->
    maps:fold(
        fun(_Key, VoiceState, Acc) -> add_voice_state_user_id(VoiceState, Acc) end,
        [],
        voice_state_utils:voice_states(Base)
    ).

-spec add_voice_state_user_id(term(), [integer()]) -> [integer()].
add_voice_state_user_id(VoiceState, Acc) when is_map(VoiceState) ->
    case voice_state_utils:voice_state_user_id(VoiceState) of
        UserId when is_integer(UserId) -> [UserId | Acc];
        _ -> Acc
    end;
add_voice_state_user_id(_VoiceState, Acc) ->
    Acc.

-spec resolve_voice_members([map()], map()) -> [map()].
resolve_voice_members(VoiceStates, Data) ->
    lists:filtermap(fun(VS) -> resolve_voice_member_entry(VS, Data) end, VoiceStates).

-spec resolve_voice_member_entry(map(), map()) -> {true, map()} | false.
resolve_voice_member_entry(VoiceState, Data) ->
    case maps:get(<<"member">>, VoiceState, undefined) of
        Member when is_map(Member), map_size(Member) > 0 ->
            {true, Member};
        _ ->
            resolve_voice_member_by_lookup(VoiceState, Data)
    end.

-spec resolve_voice_member_by_lookup(map(), map()) -> {true, map()} | false.
resolve_voice_member_by_lookup(VoiceState, Data) ->
    case voice_state_utils:voice_state_user_id(VoiceState) of
        UserId when is_integer(UserId) ->
            resolve_voice_member_from_index(UserId, Data);
        _ ->
            false
    end.

-spec resolve_voice_member_from_index(integer(), map()) -> {true, map()} | false.
resolve_voice_member_from_index(UserId, Data) ->
    case guild_data_index_members:get_member_ets(UserId, Data) of
        Member when is_map(Member) -> {true, Member};
        _ -> false
    end.

-spec fetch_latest_voice_states(guild_state()) -> guild_state().
fetch_latest_voice_states(State) ->
    case maps:get(voice_server_pid, State, undefined) of
        VoiceServerPid when is_pid(VoiceServerPid), VoiceServerPid =/= self() ->
            fetch_from_voice_pid(VoiceServerPid, State);
        _ ->
            fetch_from_voice_registry(State)
    end.

-spec get_guild_data_for_user(user_id(), map(), guild_state()) -> guild_reply(map()).
get_guild_data_for_user(UserId, Data, State) ->
    case guild_data_index:get_member(UserId, Data) of
        undefined ->
            {reply, #{guild_data => null, error_reason => <<"forbidden">>}, State};
        Member ->
            GuildData = build_member_guild_data(UserId, Member, Data, State),
            {reply, #{guild_data => GuildData}, State}
    end.

-spec build_complete_guild_data(map(), guild_state()) -> map().
build_complete_guild_data(Data, State) ->
    GuildProperties = maps:get(<<"guild">>, Data, #{}),
    Channels = map_utils:ensure_list(maps:get(<<"channels">>, Data, [])),
    maps:merge(GuildProperties, build_guild_collection_data(Data, Channels, State)).

-spec build_member_guild_data(user_id(), map(), map(), guild_state()) -> map().
build_member_guild_data(UserId, Member, Data, State) ->
    GuildProperties = maps:get(<<"guild">>, Data, #{}),
    AllChannels = guild_data_channels:channels_from_data(Data),
    {ViewableChannels, _JoinedAt} = guild_data_channels:derive_member_view(
        UserId, Member, State, AllChannels
    ),
    maps:merge(GuildProperties, build_guild_collection_data(Data, ViewableChannels, State)).

-spec build_guild_collection_data(map(), [map()], guild_state()) -> map().
build_guild_collection_data(Data, Channels, State) ->
    #{
        <<"roles">> => map_utils:ensure_list(maps:get(<<"roles">>, Data, [])),
        <<"channels">> => map_utils:ensure_list(Channels),
        <<"emojis">> => map_utils:ensure_list(maps:get(<<"emojis">>, Data, [])),
        <<"stickers">> => map_utils:ensure_list(maps:get(<<"stickers">>, Data, [])),
        <<"member_count">> => maps:get(
            member_count, State, guild_data_index:member_count(Data)
        ),
        <<"online_count">> => guild_member_list:get_online_count(State)
    }.

-spec own_member_list(map() | undefined) -> [map()].
own_member_list(undefined) -> [];
own_member_list(M) -> [M].

-spec filter_voice_states([map()], sets:set(integer())) -> [map()].
filter_voice_states(AllVoiceStates, ViewableChannelIds) ->
    [
        guild_data_channels:sanitize_voice_state(VS)
     || VS <- AllVoiceStates,
        voice_state_in_viewable_channel(VS, ViewableChannelIds)
    ].

-spec build_guild_state_map(
    guild_id() | undefined,
    map(),
    [map()],
    [map()],
    non_neg_integer(),
    non_neg_integer(),
    [map()],
    term()
) -> map().
build_guild_state_map(
    GuildId,
    Data,
    Channels,
    Members,
    MemberCount,
    OnlineCount,
    VoiceStates,
    JoinedAt
) ->
    #{
        <<"id">> => guild_id_wire_value(GuildId),
        <<"properties">> => maps:get(<<"guild">>, Data, #{}),
        <<"roles">> => map_utils:ensure_list(maps:get(<<"roles">>, Data, [])),
        <<"channels">> => Channels,
        <<"emojis">> => maps:get(<<"emojis">>, Data, []),
        <<"stickers">> => maps:get(<<"stickers">>, Data, []),
        <<"members">> => Members,
        <<"member_count">> => MemberCount,
        <<"online_count">> => OnlineCount,
        <<"presences">> => [],
        <<"voice_states">> => VoiceStates,
        <<"joined_at">> => JoinedAt
    }.

-spec fetch_from_voice_pid(pid(), guild_state()) -> guild_state().
fetch_from_voice_pid(VoiceServerPid, State) ->
    case erlang:process_info(VoiceServerPid, message_queue_len) of
        undefined ->
            fetch_from_voice_registry(maps:remove(voice_server_pid, State));
        {message_queue_len, Q} when Q >= 50 ->
            State;
        _ ->
            try_voice_call(VoiceServerPid, State)
    end.

-spec try_voice_call(pid(), guild_state()) -> guild_state().
try_voice_call(VoiceServerPid, State) ->
    try gen_server:call(VoiceServerPid, {get_voice_states_map}, 200) of
        VoiceStates when is_map(VoiceStates) ->
            State#{voice_states => voice_state_utils:ensure_voice_states(VoiceStates)};
        _ ->
            State#{voice_states => #{}}
    catch
        exit:{timeout, _} -> State;
        exit:_ -> fetch_from_voice_registry(maps:remove(voice_server_pid, State))
    end.

-spec fetch_from_voice_registry(guild_state()) -> guild_state().
fetch_from_voice_registry(State) ->
    case guild_id(State) of
        undefined -> State#{voice_states => #{}};
        GuildId -> fetch_from_voice_registry(GuildId, State)
    end.

-spec fetch_from_voice_registry(guild_id(), guild_state()) -> guild_state().
fetch_from_voice_registry(GuildId, State) ->
    case guild_voice_server:lookup_registered(GuildId) of
        {ok, VoiceServerPid} when VoiceServerPid =/= self() ->
            fetch_latest_voice_states(State#{voice_server_pid => VoiceServerPid});
        _ ->
            State#{voice_states => #{}}
    end.

-spec guild_id(guild_state()) -> guild_id() | undefined.
guild_id(State) ->
    Data = guild_data_index:ensure_data_map(State),
    Guild = map_utils:ensure_map(maps:get(<<"guild">>, Data, #{})),
    first_snowflake([
        maps:get(id, State, undefined),
        maps:get(<<"id">>, State, undefined),
        maps:get(<<"id">>, Guild, undefined)
    ]).

-spec first_snowflake([term()]) -> guild_id() | undefined.
first_snowflake([]) ->
    undefined;
first_snowflake([Value | Rest]) ->
    case snowflake_id:parse_optional(Value) of
        undefined -> first_snowflake(Rest);
        GuildId -> GuildId
    end.

-spec channel_id_set([map()]) -> sets:set(integer()).
channel_id_set(Channels) ->
    sets:from_list(channel_ids(Channels)).

-spec channel_ids([map()]) -> [integer()].
channel_ids(Channels) ->
    lists:filtermap(fun channel_id_item/1, Channels).

-spec channel_id_item(map()) -> {true, integer()} | false.
channel_id_item(Channel) ->
    SafeChannel = map_utils:ensure_map(Channel),
    case snowflake_id:parse_optional(maps:get(<<"id">>, SafeChannel, undefined)) of
        ChannelId when is_integer(ChannelId) -> {true, ChannelId};
        _ -> false
    end.

-spec voice_state_in_viewable_channel(map(), sets:set(integer())) -> boolean().
voice_state_in_viewable_channel(VoiceState, ViewableChannelIds) ->
    SafeVoiceState = map_utils:ensure_map(VoiceState),
    case snowflake_id:parse_optional(maps:get(<<"channel_id">>, SafeVoiceState, undefined)) of
        ChannelId when is_integer(ChannelId) -> sets:is_element(ChannelId, ViewableChannelIds);
        _ -> false
    end.

-spec guild_id_wire_value(guild_id() | undefined) -> binary() | null.
guild_id_wire_value(undefined) ->
    null;
guild_id_wire_value(GuildId) ->
    integer_to_binary(GuildId).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

projection_snapshot() ->
    #{
        sessions => #{
            <<"s1">> => #{
                session_id => <<"s1">>,
                user_id => 7,
                pid => self(),
                bot => false,
                is_staff => true,
                pending_connect => false,
                active_guilds => sets:new(),
                user_roles => [1, 2],
                viewable_channels => #{10 => true}
            }
        }
    }.

projection_entry(Snapshot) ->
    maps:get(<<"s1">>, maps:get(sessions, Snapshot)).

project_snapshot_sessions_drops_only_heavy_keys_test() ->
    Entry = projection_entry(project_snapshot_sessions(projection_snapshot())),
    ?assertEqual(7, maps:get(user_id, Entry)),
    ?assertEqual(true, maps:get(is_staff, Entry)),
    ?assertEqual(false, maps:get(pending_connect, Entry)),
    ?assertEqual(<<"s1">>, maps:get(session_id, Entry)),
    ?assertNot(maps:is_key(active_guilds, Entry)),
    ?assertNot(maps:is_key(user_roles, Entry)),
    ?assertNot(maps:is_key(viewable_channels, Entry)).

projection_keeps_session_staff_lookup_test() ->
    Projected = project_snapshot_sessions(projection_snapshot()),
    ?assertEqual(true, guild_availability_check:is_user_staff(7, Projected)).

projection_leaves_snapshot_data_untouched_test() ->
    Data = #{<<"members">> => #{7 => #{}}, members_ets => make_ref()},
    Snapshot = maps:put(data, Data, projection_snapshot()),
    ?assertEqual(Data, maps:get(data, project_snapshot_sessions(Snapshot))).

projection_keeps_non_map_session_entries_test() ->
    Snapshot = #{sessions => #{<<"broken">> => not_a_map}},
    ?assertEqual(Snapshot, project_snapshot_sessions(Snapshot)).

projection_keeps_snapshot_without_sessions_test() ->
    Snapshot = #{id => 42, member_count => 3},
    ?assertEqual(Snapshot, project_snapshot_sessions(Snapshot)).

trim_state(MemberCount) ->
    #{id => 42, member_count => MemberCount, data => #{members_ets => make_ref()}}.

with_trim_threshold(Threshold, Fun) ->
    application:set_env(fluxer_gateway, connect_snapshot_trim_member_threshold, Threshold),
    try
        Fun()
    after
        application:unset_env(fluxer_gateway, connect_snapshot_trim_member_threshold)
    end.

trim_is_off_without_threshold_test() ->
    application:unset_env(fluxer_gateway, connect_snapshot_trim_member_threshold),
    ?assertEqual(false, should_trim_connect_snapshot(trim_state(49435))).

trim_needs_member_count_at_threshold_test() ->
    with_trim_threshold(20000, fun() ->
        ?assertEqual(true, should_trim_connect_snapshot(trim_state(49435))),
        ?assertEqual(true, should_trim_connect_snapshot(trim_state(20000))),
        ?assertEqual(false, should_trim_connect_snapshot(trim_state(19999))),
        NoCount = maps:remove(member_count, trim_state(1)),
        ?assertEqual(false, should_trim_connect_snapshot(NoCount))
    end).

trim_needs_members_ets_test() ->
    with_trim_threshold(1, fun() ->
        ?assertEqual(false, should_trim_connect_snapshot(#{member_count => 10, data => #{}})),
        ?assertEqual(
            false,
            should_trim_connect_snapshot(#{member_count => 10, data => #{members_ets => 7}})
        ),
        ?assertEqual(false, should_trim_connect_snapshot(#{member_count => 10}))
    end).

trim_ignores_invalid_threshold_test() ->
    with_trim_threshold(0, fun() ->
        ?assertEqual(false, should_trim_connect_snapshot(trim_state(49435)))
    end),
    with_trim_threshold(not_an_integer, fun() ->
        ?assertEqual(false, should_trim_connect_snapshot(trim_state(49435)))
    end).

trim_and_projection_target_disjoint_keys_test() ->
    ?assertEqual(
        [],
        [
            Key
         || Key <- ?CONNECT_SNAPSHOT_HEAVY_SESSION_KEYS,
            lists:member(Key, ?CONNECT_SNAPSHOT_HEAVY_MEMBER_KEYS)
        ]
    ).

-endif.
