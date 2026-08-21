%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_data).
-typing([eqwalizer]).

-export([get_guild_data/2]).
-export([get_guild_member/2]).
-export([get_guild_members_batch/2]).
-export([has_member/2]).
-export([list_guild_members/2]).
-export([search_guild_members/2]).
-export([get_vanity_url_channel/1]).
-export([get_first_viewable_text_channel/1]).
-export([get_guild_state/2]).
-export([fetch_latest_voice_states/1]).
-export([find_everyone_viewable_text_channel/2]).

-type guild_state() :: map().
-type guild_reply(T) :: {reply, T, guild_state()}.
-type user_id() :: integer().
-type guild_id() :: integer().

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
            case guild_data_index_members:get_member_ets(UserId, Data) of
                Member when is_map(Member) -> {true, Member};
                _ -> false
            end;
        _ ->
            false
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
