%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_dispatch_send).
-typing([eqwalizer]).

-include_lib("kernel/include/logger.hrl").

-export([
    dispatch_to_sessions/4,
    filter_visible_channels/4
]).

-type event() :: atom().
-type event_data() :: map().
-type guild_state() :: map().
-type session_id() :: binary().
-type guild_id() :: integer().
-type user_id() :: integer().
-type session_pair() :: {session_id(), map()}.
-export_type([event/0, event_data/0, guild_state/0, user_id/0, session_pair/0]).

-spec dispatch_to_sessions([session_pair()], event(), event_data(), guild_state()) ->
    non_neg_integer().
dispatch_to_sessions(FilteredSessions, Event, FinalData, UpdatedState) ->
    GuildId = maps:get(id, UpdatedState),
    case guild_dispatch_filter:is_bulk_update_event(Event) of
        true ->
            dispatch_bulk_update(FilteredSessions, Event, FinalData, UpdatedState);
        false ->
            dispatch_standard(FilteredSessions, Event, FinalData, GuildId, UpdatedState)
    end.

-spec dispatch_bulk_update([session_pair()], event(), event_data(), guild_state()) ->
    non_neg_integer().
dispatch_bulk_update(FilteredSessions, Event, FinalData, UpdatedState) ->
    GuildId = maps:get(id, UpdatedState),
    BulkChannels = maps:get(<<"channels">>, FinalData, []),
    IndexedChannels = [
        {
            guild_dispatch_decorate:parse_snowflake(
                <<"id">>,
                maps:get(<<"id">>, Ch, undefined)
            ),
            Ch
        }
     || Ch <- BulkChannels
    ],
    SuccessCount = lists:foldl(
        fun({_Sid, SessionData}, Acc) ->
            dispatch_bulk_to_one_session_indexed(
                SessionData, Event, FinalData, IndexedChannels, GuildId, UpdatedState, Acc
            )
        end,
        0,
        FilteredSessions
    ),
    normalize_success(SuccessCount).

-spec dispatch_bulk_to_one_session_indexed(
    map(),
    event(),
    event_data(),
    [{integer() | undefined, map()}],
    guild_id(),
    guild_state(),
    non_neg_integer()
) -> non_neg_integer().
dispatch_bulk_to_one_session_indexed(
    SessionData, Event, FinalData, IndexedChannels, GuildId, UpdatedState, Acc
) ->
    Pid = maps:get(pid, SessionData),
    case
        session_passive:should_receive_event(
            Event, FinalData, GuildId, SessionData, UpdatedState
        )
    of
        false ->
            Acc;
        true ->
            FilteredChannels = filter_indexed_for_session(
                SessionData, IndexedChannels, UpdatedState
            ),
            dispatch_bulk_to_pid(Pid, Event, FinalData, FilteredChannels, GuildId, Acc)
    end.

-spec filter_indexed_for_session(map(), [{integer() | undefined, map()}], guild_state()) ->
    [map()].
filter_indexed_for_session(SessionData, IndexedChannels, UpdatedState) ->
    case maps:get(viewable_channels, SessionData, undefined) of
        ViewableMap when is_map(ViewableMap) ->
            [
                Ch
             || {ChId, Ch} <- IndexedChannels, is_integer(ChId), maps:is_key(ChId, ViewableMap)
            ];
        _ ->
            UserId = maps:get(user_id, SessionData),
            Member = guild_permissions:find_member_by_user_id(UserId, UpdatedState),
            [
                Ch
             || {ChId, Ch} <- IndexedChannels,
                is_integer(ChId),
                guild_permissions:can_view_channel(UserId, ChId, Member, UpdatedState)
            ]
    end.

-spec filter_visible_channels([map()], user_id(), map() | undefined, guild_state()) -> [map()].
filter_visible_channels(Channels, UserId, Member, State) ->
    lists:filter(
        fun(Channel) ->
            is_channel_visible(Channel, UserId, Member, State)
        end,
        Channels
    ).

-spec is_channel_visible(map(), user_id(), map() | undefined, guild_state()) -> boolean().
is_channel_visible(_Channel, _UserId, undefined, _State) ->
    false;
is_channel_visible(Channel, UserId, Member, State) ->
    ChannelIdBin = maps:get(<<"id">>, Channel, undefined),
    case guild_dispatch_decorate:parse_snowflake(<<"id">>, ChannelIdBin) of
        undefined -> false;
        ChannelId -> guild_permissions:can_view_channel(UserId, ChannelId, Member, State)
    end.

-spec dispatch_bulk_to_pid(
    pid(), event(), event_data(), [map()], guild_id(), non_neg_integer()
) -> non_neg_integer().
dispatch_bulk_to_pid(_, _, _, [], _GuildId, Acc) ->
    Acc;
dispatch_bulk_to_pid(Pid, Event, FinalData, FilteredChannels, GuildId, Acc) when is_pid(Pid) ->
    CustomData = FinalData#{<<"channels">> => FilteredChannels},
    EncodedData =
        {pre_encoded,
            iolist_to_binary(
                json:encode(guild_data_wire:payload(CustomData), fun json:encode_value/2)
            )},
    try
        gateway_dispatch_relay:dispatch(Pid, Event, EncodedData, GuildId),
        Acc + 1
    catch
        _:_ -> Acc
    end;
dispatch_bulk_to_pid(_, _, _, _, _GuildId, Acc) ->
    Acc.

-spec dispatch_standard([session_pair()], event(), event_data(), guild_id(), guild_state()) ->
    non_neg_integer().
dispatch_standard(FilteredSessions, Event, FinalData, GuildId, State) ->
    ?LOG_DEBUG(
        "dispatch_standard: event=~p guild_id=~p filtered_sessions=~p member_count=~p",
        [Event, GuildId, length(FilteredSessions), maps:get(member_count, State, undefined)]
    ),
    EncodedData =
        {pre_encoded,
            iolist_to_binary(
                json:encode(guild_data_wire:payload(FinalData), fun json:encode_value/2)
            )},
    Pids = collect_eligible_pids(FilteredSessions, Event, FinalData, GuildId, State),
    dispatch_to_pids(Pids, Event, EncodedData, GuildId, State),
    normalize_dispatched(Pids).

-spec collect_eligible_pids(
    [session_pair()], event(), event_data(), guild_id(), guild_state()
) -> [pid()].
collect_eligible_pids(FilteredSessions, Event, FinalData, GuildId, State) ->
    lists:filtermap(
        fun({_Sid, SessionData}) ->
            check_eligible_pid(SessionData, Event, FinalData, GuildId, State)
        end,
        FilteredSessions
    ).

-spec check_eligible_pid(map(), event(), event_data(), guild_id(), guild_state()) ->
    {true, pid()} | false.
check_eligible_pid(SessionData, Event, FinalData, GuildId, State) ->
    Pid = maps:get(pid, SessionData),
    Eligible =
        is_pid(Pid) andalso
            session_passive:should_receive_event(Event, FinalData, GuildId, SessionData, State),
    case Eligible of
        true ->
            {true, Pid};
        false ->
            false
    end.

-spec dispatch_to_pids([pid()], event(), term(), guild_id(), guild_state()) -> ok.
dispatch_to_pids([], _Event, _EncodedData, _GuildId, _State) ->
    ok;
dispatch_to_pids(Pids, Event, EncodedData, GuildId, State) ->
    BroadcasterPid = maps:get(broadcaster_pid, State, undefined),
    case guild_broadcaster:cast_event(BroadcasterPid, Event, EncodedData, Pids) of
        true -> ok;
        false -> gateway_dispatch_relay:dispatch_many(Pids, Event, EncodedData, GuildId)
    end.

-spec normalize_success(non_neg_integer()) -> non_neg_integer().
normalize_success(Count) when Count > 0 -> 1;
normalize_success(_) -> 0.

-spec normalize_dispatched([pid()]) -> non_neg_integer().
normalize_dispatched([]) -> 0;
normalize_dispatched([_ | _]) -> 1.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

normalize_success_test() ->
    ?assertEqual(1, normalize_success(5)),
    ?assertEqual(1, normalize_success(1)),
    ?assertEqual(0, normalize_success(0)).

normalize_dispatched_test() ->
    ?assertEqual(0, normalize_dispatched([])),
    ?assertEqual(1, normalize_dispatched([self()])),
    ?assertEqual(1, normalize_dispatched([self(), self()])).

filter_visible_channels_test() ->
    {UserId, Member, State} = visibility_test_fixture(),
    Channels = [#{<<"id">> => <<"100">>}, #{<<"id">> => <<"101">>}],
    Result = filter_visible_channels(Channels, UserId, Member, State),
    ?assertEqual(1, length(Result)),
    ?assertEqual(<<"100">>, maps:get(<<"id">>, hd(Result))).

visibility_test_fixture() ->
    GuildId = 42,
    UserId = 10,
    VP = constants:view_channel_permission(),
    GIdBin = integer_to_binary(GuildId),
    VPBin = integer_to_binary(VP),
    Member = #{<<"user">> => #{<<"id">> => integer_to_binary(UserId)}, <<"roles">> => []},
    DenyOW = #{
        <<"id">> => GIdBin, <<"type">> => 0, <<"allow">> => <<"0">>, <<"deny">> => VPBin
    },
    State = #{
        id => GuildId,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [#{<<"id">> => GIdBin, <<"permissions">> => VPBin}],
            <<"members">> => [Member],
            <<"channels">> => [
                #{<<"id">> => <<"100">>, <<"permission_overwrites">> => []},
                #{<<"id">> => <<"101">>, <<"permission_overwrites">> => [DenyOW]}
            ]
        }
    },
    {UserId, Member, State}.

filter_visible_channels_undefined_member_test() ->
    State = #{data => #{<<"members">> => []}},
    Channels = [#{<<"id">> => <<"100">>}],
    Result = filter_visible_channels(Channels, 10, undefined, State),
    ?assertEqual([], Result).

passive_standard_structural_updates_dispatch_test() ->
    Events = [
        {guild_update, #{<<"name">> => <<"Updated">>}},
        {guild_role_update, #{<<"role">> => #{<<"id">> => <<"200">>, <<"name">> => <<"Role">>}}},
        {guild_role_update_bulk, #{
            <<"roles">> => [#{<<"id">> => <<"200">>, <<"name">> => <<"Role">>}]
        }},
        {channel_create, #{<<"id">> => <<"100">>, <<"name">> => <<"general">>}},
        {channel_update, #{<<"id">> => <<"100">>, <<"name">> => <<"general">>}},
        {channel_delete, #{<<"id">> => <<"100">>}},
        {guild_member_update, #{<<"user">> => #{<<"id">> => <<"10">>}, <<"roles">> => []}}
    ],
    lists:foreach(fun assert_passive_standard_dispatch/1, Events).

passive_channel_update_bulk_dispatches_visible_channels_test() ->
    flush_dispatches(),
    Data = #{
        <<"guild_id">> => <<"42">>,
        <<"channels">> => [
            #{<<"id">> => <<"100">>, <<"name">> => <<"visible">>},
            #{<<"id">> => <<"200">>, <<"name">> => <<"hidden">>}
        ]
    },
    ?assertEqual(
        1,
        dispatch_to_sessions(
            [passive_session_pair()], channel_update_bulk, Data, passive_dispatch_state()
        )
    ),
    Payload = receive_pre_encoded_payload(channel_update_bulk),
    ?assertEqual(
        [#{<<"id">> => <<"100">>, <<"name">> => <<"visible">>}],
        maps:get(<<"channels">>, Payload)
    ).

standard_dispatch_without_eligible_sessions_test() ->
    flush_dispatches(),
    Session = (passive_session_data())#{pid => undefined},
    ?assertEqual(
        0,
        dispatch_to_sessions(
            [{<<"offline">>, Session}],
            guild_update,
            #{<<"guild_id">> => <<"42">>, <<"name">> => <<"Updated">>},
            passive_dispatch_state()
        )
    ),
    assert_no_dispatch().

standard_dispatch_ignores_ineligible_sessions_test() ->
    flush_dispatches(),
    Offline = (passive_session_data())#{pid => undefined},
    ?assertEqual(
        1,
        dispatch_to_sessions(
            [{<<"offline">>, Offline}, passive_session_pair()],
            guild_update,
            #{<<"guild_id">> => <<"42">>, <<"name">> => <<"Updated">>},
            passive_dispatch_state()
        )
    ),
    _Payload = receive_pre_encoded_payload(guild_update),
    assert_no_dispatch().

assert_no_dispatch() ->
    receive
        {'$gen_cast', {dispatch, Event, _Payload}} ->
            ?assert(false, {unexpected_dispatch, Event})
    after 100 ->
        ok
    end.

assert_passive_standard_dispatch({Event, Data}) ->
    flush_dispatches(),
    ?assertEqual(
        1,
        dispatch_to_sessions(
            [passive_session_pair()],
            Event,
            Data#{<<"guild_id">> => <<"42">>},
            passive_dispatch_state()
        )
    ),
    _Payload = receive_pre_encoded_payload(Event),
    ok.

passive_session_pair() ->
    {<<"passive">>, passive_session_data()}.

passive_session_data() ->
    #{
        session_id => <<"passive">>,
        user_id => 10,
        pid => self(),
        active_guilds => sets:new(),
        bot => false,
        viewable_channels => #{100 => true}
    }.

passive_dispatch_state() ->
    #{
        id => 42,
        member_count => 300,
        data => #{
            <<"guild">> => #{<<"owner_id">> => <<"999">>},
            <<"roles">> => [],
            <<"members">> => [],
            <<"channels">> => []
        }
    }.

receive_pre_encoded_payload(Event) ->
    receive
        {'$gen_cast', {dispatch, Event, {pre_encoded, Bin}}} ->
            json:decode(Bin)
    after 1000 ->
        ?assert(false, {dispatch_not_received, Event})
    end.

flush_dispatches() ->
    receive
        {'$gen_cast', {dispatch, _Event, _Payload}} ->
            flush_dispatches()
    after 0 ->
        ok
    end.

-endif.
