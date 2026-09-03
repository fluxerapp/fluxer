%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_dispatch_send).
-typing([eqwalizer]).

-include_lib("kernel/include/logger.hrl").

-export([
    dispatch_to_sessions/4,
    filter_visible_channels/4
]).

-define(MAX_BULK_ENCODE_GROUPS, 1024).

-type event() :: atom().
-type event_data() :: map().
-type guild_state() :: map().
-type session_id() :: binary().
-type guild_id() :: integer().
-type user_id() :: integer().
-type session_pair() :: {session_id(), map()}.
-type bulk_group_key() :: [term()].
-type bulk_groups() :: #{bulk_group_key() => {[map()], [pid()]}}.
-type bulk_acc() :: {bulk_groups(), non_neg_integer()}.
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
    {Groups, Dispatched} = lists:foldl(
        fun({_Sid, SessionData}, Acc) ->
            collect_bulk_recipient(
                SessionData, Event, FinalData, IndexedChannels, GuildId, UpdatedState, Acc
            )
        end,
        {#{}, 0},
        FilteredSessions
    ),
    SuccessCount = dispatch_bulk_groups(Groups, Event, FinalData, GuildId, Dispatched),
    normalize_success(SuccessCount).

-spec collect_bulk_recipient(
    map(),
    event(),
    event_data(),
    [{integer() | undefined, map()}],
    guild_id(),
    guild_state(),
    bulk_acc()
) -> bulk_acc().
collect_bulk_recipient(SessionData, Event, FinalData, IndexedChannels, GuildId, State, Acc) ->
    Pid = maps:get(pid, SessionData),
    Eligible =
        is_pid(Pid) andalso
            session_passive:should_receive_event(Event, FinalData, GuildId, SessionData, State),
    case Eligible of
        false ->
            Acc;
        true ->
            Filtered = filter_indexed_for_session(SessionData, IndexedChannels, State),
            add_bulk_recipient(Pid, Filtered, Event, FinalData, GuildId, Acc)
    end.

-spec add_bulk_recipient(pid(), [map()], event(), event_data(), guild_id(), bulk_acc()) ->
    bulk_acc().
add_bulk_recipient(_Pid, [], _Event, _FinalData, _GuildId, Acc) ->
    Acc;
add_bulk_recipient(Pid, FilteredChannels, Event, FinalData, GuildId, {Groups, Dispatched}) ->
    Key = bulk_group_key(FilteredChannels),
    case Groups of
        #{Key := {Channels, Pids}} ->
            {Groups#{Key := {Channels, [Pid | Pids]}}, Dispatched};
        _ when map_size(Groups) >= ?MAX_BULK_ENCODE_GROUPS ->
            Sent = dispatch_bulk_to_pid(
                Pid, Event, FinalData, FilteredChannels, GuildId, Dispatched
            ),
            {Groups, Sent};
        _ ->
            {Groups#{Key => {FilteredChannels, [Pid]}}, Dispatched}
    end.

-spec bulk_group_key([map()]) -> bulk_group_key().
bulk_group_key(FilteredChannels) ->
    [maps:get(<<"id">>, Ch, undefined) || Ch <- FilteredChannels].

-spec dispatch_bulk_groups(
    bulk_groups(), event(), event_data(), guild_id(), non_neg_integer()
) -> non_neg_integer().
dispatch_bulk_groups(Groups, Event, FinalData, GuildId, Dispatched) ->
    maps:fold(
        fun(_Key, {FilteredChannels, Pids}, Acc) ->
            dispatch_bulk_to_pids(
                lists:reverse(Pids), Event, FinalData, FilteredChannels, GuildId, Acc
            )
        end,
        Dispatched,
        Groups
    ).

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
dispatch_bulk_to_pid(Pid, Event, FinalData, FilteredChannels, GuildId, Acc) when is_pid(Pid) ->
    EncodedData = encode_bulk_payload(FinalData, FilteredChannels),
    try
        gateway_dispatch_relay:dispatch(Pid, Event, EncodedData, GuildId),
        Acc + 1
    catch
        _:_ -> Acc
    end;
dispatch_bulk_to_pid(_, _, _, _, _GuildId, Acc) ->
    Acc.

-spec dispatch_bulk_to_pids(
    [pid()], event(), event_data(), [map()], guild_id(), non_neg_integer()
) -> non_neg_integer().
dispatch_bulk_to_pids([], _Event, _FinalData, _FilteredChannels, _GuildId, Acc) ->
    Acc;
dispatch_bulk_to_pids(Pids, Event, FinalData, FilteredChannels, GuildId, Acc) ->
    EncodedData = encode_bulk_payload(FinalData, FilteredChannels),
    try
        gateway_dispatch_relay:dispatch_many(Pids, Event, EncodedData, GuildId),
        Acc + length(Pids)
    catch
        _:_ -> Acc
    end.

-spec encode_bulk_payload(event_data(), [map()]) -> {pre_encoded, binary()}.
encode_bulk_payload(FinalData, FilteredChannels) ->
    CustomData = FinalData#{<<"channels">> => FilteredChannels},
    {pre_encoded,
        iolist_to_binary(
            json:encode(guild_data_wire:payload(CustomData), fun json:encode_value/2)
        )}.

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

-define(OVERFLOW_CHANNEL_BITS, 11).

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

grouped_bulk_encoding_matches_ungrouped_test() ->
    flush_dispatches(),
    Data = bulk_channel_data(),
    State = passive_dispatch_state(),
    Sessions = [bulk_session_pair(<<"a">>, #{100 => true})],
    ?assertEqual(1, reference_dispatch_bulk_update(Sessions, channel_update_bulk, Data, State)),
    Ungrouped = receive_pre_encoded_binary(channel_update_bulk),
    ?assertEqual(1, dispatch_to_sessions(Sessions, channel_update_bulk, Data, State)),
    ?assertEqual(Ungrouped, receive_pre_encoded_binary(channel_update_bulk)).

reference_dispatch_bulk_update(FilteredSessions, Event, FinalData, UpdatedState) ->
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
            reference_dispatch_bulk_to_one_session_indexed(
                SessionData, Event, FinalData, IndexedChannels, GuildId, UpdatedState, Acc
            )
        end,
        0,
        FilteredSessions
    ),
    normalize_success(SuccessCount).

reference_dispatch_bulk_to_one_session_indexed(
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

grouped_bulk_encodes_once_per_visible_set_test() ->
    flush_dispatches(),
    Data = bulk_channel_data(),
    State = passive_dispatch_state(),
    Sessions = [
        bulk_session_pair(<<"a">>, #{100 => true}),
        bulk_session_pair(<<"b">>, #{100 => true}),
        bulk_session_pair(<<"c">>, #{200 => true})
    ],
    ?assertEqual(1, dispatch_to_sessions(Sessions, channel_update_bulk, Data, State)),
    Bins = [receive_pre_encoded_binary(channel_update_bulk) || _ <- lists:seq(1, 3)],
    ?assertEqual(2, length(lists:usort(Bins))),
    Sets = [maps:get(<<"channels">>, json:decode(Bin)) || Bin <- Bins],
    ?assertEqual(2, length([S || S <- Sets, S =:= [bulk_channel(<<"100">>, <<"first">>)]])),
    ?assertEqual(1, length([S || S <- Sets, S =:= [bulk_channel(<<"200">>, <<"second">>)]])).

grouped_bulk_skips_sessions_without_visible_channels_test() ->
    flush_dispatches(),
    Data = bulk_channel_data(),
    State = passive_dispatch_state(),
    Sessions = [
        bulk_session_pair(<<"a">>, #{}),
        bulk_session_pair(<<"b">>, #{200 => true})
    ],
    ?assertEqual(1, dispatch_to_sessions(Sessions, channel_update_bulk, Data, State)),
    ?assertEqual(
        [bulk_channel(<<"200">>, <<"second">>)],
        maps:get(<<"channels">>, receive_pre_encoded_payload(channel_update_bulk))
    ),
    ?assertEqual(ok, assert_no_further_dispatch()).

grouped_bulk_dispatches_overflow_recipients_without_retaining_them_test() ->
    flush_dispatches(),
    Total = ?MAX_BULK_ENCODE_GROUPS + 1,
    State = passive_dispatch_state(),
    Data = #{
        <<"guild_id">> => <<"42">>,
        <<"channels">> => [
            bulk_channel(integer_to_binary(Id), <<"c">>)
         || Id <- lists:seq(1, ?OVERFLOW_CHANNEL_BITS)
        ]
    },
    Sessions = [overflow_session_pair(N) || N <- lists:seq(1, Total)],
    ?assertEqual(1, dispatch_to_sessions(Sessions, channel_update_bulk, Data, State)),
    Received = collect_bulk_channel_ids(Total, []),
    Expected = [overflow_channel_ids(N) || N <- lists:seq(1, Total)],
    ?assertEqual(lists:sort(Expected), lists:sort(Received)),
    ?assertEqual(ok, assert_no_further_dispatch()).

overflow_session_pair(N) ->
    Base = passive_session_data(),
    Sid = integer_to_binary(N),
    Viewable = maps:from_list([{Id, true} || Id <- overflow_visible_ids(N)]),
    {Sid, Base#{session_id => Sid, viewable_channels => Viewable}}.

overflow_visible_ids(N) ->
    [Id || Id <- lists:seq(1, ?OVERFLOW_CHANNEL_BITS), (N bsr (Id - 1)) band 1 =:= 1].

overflow_channel_ids(N) ->
    [integer_to_binary(Id) || Id <- overflow_visible_ids(N)].

collect_bulk_channel_ids(0, Acc) ->
    Acc;
collect_bulk_channel_ids(N, Acc) ->
    Payload = receive_pre_encoded_payload(channel_update_bulk),
    Ids = [maps:get(<<"id">>, Ch) || Ch <- maps:get(<<"channels">>, Payload)],
    collect_bulk_channel_ids(N - 1, [Ids | Acc]).

bulk_channel_data() ->
    #{
        <<"guild_id">> => <<"42">>,
        <<"channels">> => [
            bulk_channel(<<"100">>, <<"first">>),
            bulk_channel(<<"200">>, <<"second">>)
        ]
    }.

bulk_channel(Id, Name) ->
    #{<<"id">> => Id, <<"name">> => Name}.

bulk_session_pair(Sid, ViewableChannels) ->
    Base = passive_session_data(),
    {Sid, Base#{session_id => Sid, viewable_channels => ViewableChannels}}.

assert_no_further_dispatch() ->
    receive
        {'$gen_cast', {dispatch, Event, _Payload}} ->
            ?assert(false, {unexpected_dispatch, Event})
    after 0 ->
        ok
    end.

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
    json:decode(receive_pre_encoded_binary(Event)).

receive_pre_encoded_binary(Event) ->
    receive
        {'$gen_cast', {dispatch, Event, {pre_encoded, Bin}}} ->
            Bin
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
