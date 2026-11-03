%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_visitors).
-typing([eqwalizer]).

-export([
    is_visitor_request/1,
    member_request/1,
    admit/3,
    remove_session/2,
    handle_down/2,
    set_focus/3,
    sessions/1,
    focused_sessions/2,
    count/1,
    evict_ips/2,
    evict_disabled/1,
    evict_minors/1,
    evict_all/2,
    promote_user/2,
    refresh_views/1
]).

-export_type([visitor_session/0]).

-define(VERIFIED_TIER, <<"verified">>).

-type rejection() :: disabled | age_restricted | unavailable | busy.
-type candidate() :: #{
    request := map(),
    pid := pid(),
    session_id := session:session_id(),
    user_id := session:user_id(),
    adult := boolean(),
    operator := boolean(),
    tier := binary(),
    ip_key := binary() | undefined
}.
-type visitor_session() :: #{
    session_id := session:session_id(),
    user_id := session:user_id(),
    pid := pid(),
    mref := reference(),
    adult := boolean(),
    tier := binary(),
    ip_key := binary() | undefined,
    operator := boolean(),
    since_ms := integer(),
    focused_channel_id := integer() | undefined,
    thread_capable := boolean(),
    thread_viewer := boolean(),
    viewable_channels := #{integer() => true}
}.

-spec is_visitor_request(map()) -> boolean().
is_visitor_request(Request) ->
    type_conv:is_true(maps:get(visitor, Request, false)).

-spec member_request(map()) -> map().
member_request(Request) ->
    maps:without([visitor, operator, adult_content_allowed, visitor_tier, ip_key], Request).

-spec admit(map(), pid(), guild_data:guild_state()) ->
    {ok, map(), guild_data:guild_state()}
    | {rejected, disabled | age_restricted | unavailable | busy, guild_data:guild_state()}.
admit(Request, Pid, State0) ->
    #{session_id := SessionId, user_id := UserId} = Request,
    State = remove_session(SessionId, State0),
    Adult = type_conv:is_true(maps:get(adult_content_allowed, Request, false)),
    Operator = type_conv:is_true(maps:get(operator, Request, false)),
    case static_rejection(Operator, Adult, State) of
        {reject, Reason} ->
            reject(Reason, State);
        ok ->
            Candidate = #{
                request => Request,
                pid => Pid,
                session_id => SessionId,
                user_id => UserId,
                adult => Adult,
                operator => Operator,
                tier => visitor_request:tier(maps:get(visitor_tier, Request, undefined)),
                ip_key => visitor_request:ip_key(maps:get(ip_key, Request, undefined))
            },
            admit_within_caps(Candidate, State)
    end.

-spec reject(rejection(), guild_data:guild_state()) ->
    {rejected, rejection(), guild_data:guild_state()}.
reject(Reason, State) ->
    ok = guild_visitor_metrics:rejected(Reason),
    {rejected, Reason, State}.

-spec static_rejection(boolean(), boolean(), guild_data:guild_state()) ->
    ok | {reject, rejection()}.
static_rejection(Operator, Adult, State) ->
    Features = guild_features(State),
    Unavailable =
        lists:member(<<"UNAVAILABLE_FOR_EVERYONE">>, Features) orelse
            lists:member(<<"UNAVAILABLE_FOR_EVERYONE_BUT_STAFF">>, Features),
    Disabled = not Operator andalso not lists:member(<<"VISITS_ENABLED">>, Features),
    case {Unavailable, Disabled, Adult} of
        {true, _, _} -> {reject, unavailable};
        {false, true, _} -> {reject, disabled};
        {false, false, false} -> maybe_age_rejection(guild_data_index:ensure_data_map(State));
        {false, false, true} -> ok
    end.

-spec maybe_age_rejection(map()) -> ok | {reject, age_restricted}.
maybe_age_rejection(Data) ->
    case guild_visitor_permissions:guild_nsfw(Data) of
        true -> {reject, age_restricted};
        false -> ok
    end.

-spec admit_within_caps(candidate(), guild_data:guild_state()) ->
    {ok, map(), guild_data:guild_state()} | {rejected, rejection(), guild_data:guild_state()}.
admit_within_caps(#{user_id := UserId, ip_key := IpKey} = Candidate, State) ->
    Visitors = guild_visitor_state:get(State),
    ByUser = maps:get(UserId, maps:get(by_user, Visitors), 0),
    ByIp = ip_count(IpKey, State),
    case
        {
            ByUser >= config_int(guild_visitor_sessions_per_user),
            ByIp >= config_int(guild_visitor_sessions_per_ip),
            count(State) >= cap(State)
        }
    of
        {true, _, _} ->
            reject(busy, State);
        {_, true, _} ->
            reject(busy, State);
        {false, false, false} ->
            add_session(Candidate, State);
        {false, false, true} ->
            make_room(Candidate, State)
    end.

-spec make_room(candidate(), guild_data:guild_state()) ->
    {ok, map(), guild_data:guild_state()} | {rejected, rejection(), guild_data:guild_state()}.
make_room(#{tier := ?VERIFIED_TIER} = Candidate, State) ->
    case oldest_unverified(State) of
        undefined ->
            reject(busy, State);
        Victim ->
            Evicted = evict_sessions([Victim], busy, State),
            add_session(Candidate, Evicted)
    end;
make_room(_Candidate, State) ->
    reject(busy, State).

-spec oldest_unverified(guild_data:guild_state()) -> visitor_session() | undefined.
oldest_unverified(State) ->
    maps:fold(
        fun
            (_Sid, #{tier := Tier} = Session, undefined) when Tier =/= ?VERIFIED_TIER ->
                Session;
            (_Sid, #{tier := Tier, since_ms := Since} = Session, #{since_ms := Best}) when
                Tier =/= ?VERIFIED_TIER, Since < Best
            ->
                Session;
            (_Sid, _Session, Acc) ->
                Acc
        end,
        undefined,
        sessions(State)
    ).

-spec add_session(candidate(), guild_data:guild_state()) ->
    {ok, map(), guild_data:guild_state()}.
add_session(
    #{request := Request, pid := Pid, user_id := UserId} = Candidate, State
) ->
    #{thread_capable := Capable, thread_viewer := Viewer} = guild_thread_gate:session_fields(
        Request, UserId, State
    ),
    Visibility = #{adult => maps:get(adult, Candidate), thread_viewer => Viewer},
    {View, State1} = guild_visitor_snapshot:view(Visibility, State),
    MRef = monitor(process, Pid),
    Session = (maps:remove(request, Candidate))#{
        mref => MRef,
        since_ms => erlang:system_time(millisecond),
        focused_channel_id => undefined,
        thread_capable => Capable,
        thread_viewer => Viewer,
        viewable_channels => View
    },
    State2 = put_session(Session, State1),
    ok = guild_visitor_metrics:admitted(),
    ok = guild_visitor_metrics:session_added(),
    {ok, guild_visitor_snapshot:build(Session, State2),
        guild_sessions_connect_cleanup:clear_auto_stop_pending(State2)}.

-spec put_session(visitor_session(), guild_data:guild_state()) -> guild_data:guild_state().
put_session(#{session_id := SessionId, user_id := UserId, mref := MRef} = Session, State) ->
    IpKey = maps:get(ip_key, Session),
    Focus = maps:get(focused_channel_id, Session),
    Refocused = refocus(SessionId, undefined, Focus, State),
    Visitors = guild_visitor_state:get(Refocused),
    guild_visitor_state:put(
        Visitors#{
            sessions => (maps:get(sessions, Visitors))#{SessionId => Session},
            refs => (maps:get(refs, Visitors))#{MRef => SessionId},
            count => maps:get(count, Visitors) + 1,
            by_user => increment(UserId, maps:get(by_user, Visitors)),
            by_ip => increment_ip(IpKey, maps:get(by_ip, Visitors))
        },
        Refocused
    ).

-spec remove_session(session:session_id(), guild_data:guild_state()) ->
    guild_data:guild_state().
remove_session(SessionId, State) ->
    case maps:take(SessionId, sessions(State)) of
        error ->
            State;
        {#{mref := MRef, user_id := UserId} = Session, Remaining} ->
            demonitor(MRef, [flush]),
            ok = guild_visitor_metrics:session_removed(1),
            Unfocused = refocus(
                SessionId, maps:get(focused_channel_id, Session), undefined, State
            ),
            Visitors = guild_visitor_state:get(Unfocused),
            State1 = guild_visitor_state:put(
                Visitors#{
                    sessions => Remaining,
                    refs => maps:remove(MRef, maps:get(refs, Visitors)),
                    count => max(0, maps:get(count, Visitors) - 1),
                    by_user => decrement(UserId, maps:get(by_user, Visitors)),
                    by_ip => decrement_ip(maps:get(ip_key, Session), maps:get(by_ip, Visitors))
                },
                Unfocused
            ),
            maybe_mark_empty(State1)
    end.

-spec maybe_mark_empty(guild_data:guild_state()) -> guild_data:guild_state().
maybe_mark_empty(State) ->
    case count(State) =:= 0 andalso map_size(maps:get(sessions, State, #{})) =:= 0 of
        true -> guild_sessions_connect_cleanup:maybe_mark_auto_stop_pending(State);
        false -> State
    end.

-spec handle_down(reference(), guild_data:guild_state()) ->
    {handled, guild_data:guild_state()} | not_visitor.
handle_down(Ref, State) ->
    Visitors = guild_visitor_state:get(State),
    case maps:get(Ref, maps:get(refs, Visitors), undefined) of
        undefined -> not_visitor;
        SessionId -> {handled, remove_session(SessionId, State)}
    end.

-spec set_focus(session:session_id(), term(), guild_data:guild_state()) ->
    guild_data:guild_state().
set_focus(SessionId, ChannelId, State) ->
    case maps:get(SessionId, sessions(State), undefined) of
        Session when is_map(Session) ->
            Focus = keep_focus(ChannelId, Session, State),
            Refocused = refocus(SessionId, maps:get(focused_channel_id, Session), Focus, State),
            Visitors = guild_visitor_state:get(Refocused),
            Updated = (maps:get(sessions, Visitors))#{
                SessionId => Session#{focused_channel_id => Focus}
            },
            guild_visitor_state:put(Visitors#{sessions => Updated}, Refocused);
        _ ->
            State
    end.

-spec sessions(guild_data:guild_state()) -> #{session:session_id() => visitor_session()}.
sessions(State) ->
    maps:get(sessions, guild_visitor_state:get(State)).

-spec focused_sessions(integer(), guild_data:guild_state()) -> [visitor_session()].
focused_sessions(ChannelId, State) ->
    Sessions = sessions(State),
    Index = maps:get(focus, guild_visitor_state:get(State)),
    [
        Session
     || SessionId <- maps:keys(maps:get(ChannelId, Index, #{})),
        Session <- [maps:get(SessionId, Sessions, undefined)],
        is_map(Session)
    ].

-spec refocus(
    session:session_id(), integer() | undefined, integer() | undefined, guild_data:guild_state()
) ->
    guild_data:guild_state().
refocus(_SessionId, Same, Same, State) ->
    State;
refocus(SessionId, Old, New, State) ->
    Visitors = guild_visitor_state:get(State),
    Index0 = maps:get(focus, Visitors),
    Index1 =
        case Old of
            undefined -> Index0;
            _ -> unfocus(SessionId, Old, Index0)
        end,
    Index2 =
        case New of
            undefined -> Index1;
            _ -> Index1#{New => (maps:get(New, Index1, #{}))#{SessionId => true}}
        end,
    guild_visitor_state:put(Visitors#{focus => Index2}, State).

-spec unfocus(session:session_id(), integer(), map()) -> map().
unfocus(SessionId, ChannelId, Index) ->
    Remaining = maps:remove(SessionId, maps:get(ChannelId, Index, #{})),
    case map_size(Remaining) of
        0 -> maps:remove(ChannelId, Index);
        _ -> Index#{ChannelId => Remaining}
    end.

-spec count(guild_data:guild_state()) -> non_neg_integer().
count(State) ->
    maps:get(count, guild_visitor_state:get(State)).

-spec cap(guild_data:guild_state()) -> non_neg_integer().
cap(State) ->
    Max = config_int(guild_visitor_sessions_max),
    Floor = config_int(guild_visitor_sessions_floor),
    PerMember = config_int(guild_visitor_sessions_per_member),
    MemberCount =
        case maps:get(member_count, State, 0) of
            N when is_integer(N), N >= 0 -> N;
            _ -> 0
        end,
    min(Max, max(Floor, PerMember * MemberCount)).

-spec evict_ips([binary()], guild_data:guild_state()) ->
    {[session:user_id()], guild_data:guild_state()}.
evict_ips(IpKeys, State) ->
    Wanted = maps:from_keys(IpKeys, true),
    Victims = [
        S
     || S <- maps:values(sessions(State)), maps:is_key(maps:get(ip_key, S), Wanted)
    ],
    UserIds = lists:usort([maps:get(user_id, S) || S <- Victims]),
    {UserIds, evict_sessions(Victims, banned, State)}.

-spec evict_disabled(guild_data:guild_state()) -> guild_data:guild_state().
evict_disabled(State) ->
    case lists:member(<<"VISITS_ENABLED">>, guild_features(State)) of
        true ->
            State;
        false ->
            Victims = [S || S <- maps:values(sessions(State)), not maps:get(operator, S)],
            evict_sessions(Victims, disabled, State)
    end.

-spec evict_minors(guild_data:guild_state()) -> guild_data:guild_state().
evict_minors(State) ->
    case guild_visitor_permissions:guild_nsfw(guild_data_index:ensure_data_map(State)) of
        false ->
            State;
        true ->
            Victims = [S || S <- maps:values(sessions(State)), not maps:get(adult, S)],
            evict_sessions(Victims, age_restricted, State)
    end.

-spec evict_all(atom(), guild_data:guild_state()) -> guild_data:guild_state().
evict_all(Reason, State) ->
    evict_sessions(maps:values(sessions(State)), Reason, State).

-spec evict_sessions([visitor_session()], atom(), guild_data:guild_state()) ->
    guild_data:guild_state().
evict_sessions(Victims, Reason, State) ->
    GuildId = maps:get(id, State),
    ok = guild_visitor_metrics:evicted(Reason, length(Victims)),
    lists:foldl(
        fun(#{session_id := SessionId, pid := Pid}, Acc) ->
            _ = shard_utils:safe_cast(Pid, {visit_rejected, GuildId, Reason}),
            remove_session(SessionId, Acc)
        end,
        State,
        Victims
    ).

-spec promote_user(session:user_id() | undefined, guild_data:guild_state()) ->
    guild_data:guild_state().
promote_user(UserId, State) when is_integer(UserId) ->
    Victims = [S || S <- maps:values(sessions(State)), maps:get(user_id, S) =:= UserId],
    lists:foldl(fun promote_session/2, State, Victims);
promote_user(_UserId, State) ->
    State.

-spec promote_session(visitor_session(), guild_data:guild_state()) -> guild_data:guild_state().
promote_session(#{session_id := SessionId, user_id := UserId, pid := Pid} = Session, State) ->
    State1 = remove_session(SessionId, State),
    Request = #{
        session_id => SessionId,
        user_id => UserId,
        session_pid => Pid,
        bot => false,
        is_staff => false,
        active_guilds => sets:new(),
        thread_channels_capable => maps:get(thread_capable, Session)
    },
    case guild_connect_async:promote_visitor_session(Request, State1) of
        {ok, GuildState, State2} ->
            _ = shard_utils:safe_cast(
                Pid, {visit_promoted, maps:get(id, State2), self(), GuildState}
            ),
            ok = guild_visitor_metrics:converted(),
            State2;
        {error, State2} ->
            _ = shard_utils:safe_cast(Pid, {visit_rejected, maps:get(id, State2), unavailable}),
            State2
    end.

-spec refresh_views(guild_data:guild_state()) -> guild_data:guild_state().
refresh_views(State0) ->
    Visitors0 = guild_visitor_state:get(State0),
    State = guild_visitor_state:put(Visitors0#{views => #{}}, State0),
    maps:fold(
        fun(SessionId, Session, Acc) ->
            {New, Acc1} = guild_visitor_snapshot:view(Session, Acc),
            Viewing = Session#{viewable_channels => New},
            OldFocus = maps:get(focused_channel_id, Session),
            Focus = keep_focus(OldFocus, Viewing, Acc1),
            Updated = Viewing#{focused_channel_id => Focus},
            Refocused = refocus(SessionId, OldFocus, Focus, Acc1),
            Visitors = guild_visitor_state:get(Refocused),
            UpdatedSessions = (maps:get(sessions, Visitors))#{SessionId => Updated},
            guild_visitor_state:put(Visitors#{sessions => UpdatedSessions}, Refocused)
        end,
        State,
        sessions(State)
    ).

-spec keep_focus(term(), visitor_session(), guild_data:guild_state()) ->
    integer() | undefined.
keep_focus(ChannelId, Session, State) when is_integer(ChannelId) ->
    case guild_visitor_dispatch:channel_visible(Session, ChannelId, State) of
        true -> ChannelId;
        false -> undefined
    end;
keep_focus(_ChannelId, _Session, _State) ->
    undefined.

-spec guild_features(guild_data:guild_state()) -> [binary()].
guild_features(State) ->
    Data = guild_data_index:ensure_data_map(State),
    Guild = map_utils:ensure_map(maps:get(<<"guild">>, Data, #{})),
    [F || F <- map_utils:ensure_list(maps:get(<<"features">>, Guild, [])), is_binary(F)].

-spec ip_count(binary() | undefined, guild_data:guild_state()) -> non_neg_integer().
ip_count(undefined, _State) ->
    0;
ip_count(IpKey, State) ->
    Visitors = guild_visitor_state:get(State),
    maps:get(IpKey, maps:get(by_ip, Visitors), 0).

-spec increment(term(), map()) -> map().
increment(Key, Map) ->
    Map#{Key => maps:get(Key, Map, 0) + 1}.

-spec increment_ip(binary() | undefined, map()) -> map().
increment_ip(undefined, Map) -> Map;
increment_ip(IpKey, Map) -> increment(IpKey, Map).

-spec decrement(term(), map()) -> map().
decrement(Key, Map) ->
    case maps:get(Key, Map, 0) of
        N when N > 1 -> Map#{Key => N - 1};
        _ -> maps:remove(Key, Map)
    end.

-spec decrement_ip(binary() | undefined, map()) -> map().
decrement_ip(undefined, Map) -> Map;
decrement_ip(IpKey, Map) -> decrement(IpKey, Map).

-spec config_int(fluxer_gateway_config:visitor_limit_key()) -> non_neg_integer().
config_int(Key) ->
    case fluxer_gateway_env:get_optional(Key) of
        Value when is_integer(Value), Value >= 0 -> Value;
        _ -> fluxer_gateway_config:visitor_limit_default(Key)
    end.
