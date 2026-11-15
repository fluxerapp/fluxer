%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_thread_gate).
-typing([eqwalizer]).

-export([
    active/1,
    store/1,
    compute_viewer/4,
    session_viewer/1,
    session_fields/3,
    recompute_session_viewers/1,
    state_opts/1,
    thread/2,
    is_thread_id/2,
    channel_visible/3,
    pre_update_filter/3,
    event_scope/3,
    viewer_sessions/1,
    thread_sessions/6,
    thread_view_access/4,
    needs_variant/1,
    recipient_active/2
]).

-type thread_ctx() :: {map(), integer() | undefined, binary() | undefined, map()}.
-type access_memo() :: #{integer() => boolean()}.

-define(MAX_ACCESS_MEMO_ENTRIES, 8192).
-spec data(map()) -> map().
data(#{data := Data}) when is_map(Data) ->
    Data;
data(Data) ->
    Data.

-spec active(map()) -> boolean().
active(State) ->
    case data(State) of
        #{thread_gate := #{active := true}} -> true;
        _ -> false
    end.

-spec tainted(map()) -> boolean().
tainted(State) ->
    case data(State) of
        #{thread_tainted := true} -> true;
        _ -> false
    end.

-spec store(map()) -> ets:table() | undefined.
store(State) ->
    case data(State) of
        #{thread_store := Tab, thread_gate := #{active := true}} when Tab =/= undefined -> Tab;
        _ -> undefined
    end.

-spec compute_viewer(boolean(), boolean(), boolean(), term()) -> boolean().
compute_viewer(true, true, _Capable, UserId) when is_integer(UserId) ->
    not channel_threads_config:user_excluded(UserId);
compute_viewer(true, false, true, UserId) when is_integer(UserId) ->
    channel_threads_config:user_active(UserId);
compute_viewer(_Active, _Bot, _Capable, _UserId) ->
    false.

-spec session_viewer(map()) -> boolean().
session_viewer(#{thread_viewer := true}) -> true;
session_viewer(_) -> false.

-spec session_fields(map(), term(), map()) -> map().
session_fields(Request, UserId, State) ->
    Capable = type_conv:is_true(maps:get(thread_channels_capable, Request, false)),
    Bot = type_conv:is_true(maps:get(bot, Request, false)),
    #{
        thread_capable => Capable,
        thread_viewer => compute_viewer(active(State), Bot, Capable, UserId)
    }.

-spec recompute_session_viewers(map()) -> map().
recompute_session_viewers(State) ->
    Active = active(State),
    Loaded = channel_threads_config:enabled() orelse channel_threads_config:version() > 0,
    Sessions = maps:get(sessions, State),
    State#{
        sessions => maps:map(
            fun(_SessionId, Session) -> recompute_session_viewer(Active, Loaded, Session) end,
            Sessions
        )
    }.

-spec recompute_session_viewer(boolean(), boolean(), term()) -> term().
recompute_session_viewer(true, false, #{thread_viewer := _} = Session) ->
    Session;
recompute_session_viewer(Active, _Loaded, Session) when is_map(Session) ->
    Viewer = compute_viewer(
        Active,
        type_conv:is_true(maps:get(bot, Session, false)),
        type_conv:is_true(maps:get(thread_capable, Session, false)),
        maps:get(user_id, Session, undefined)
    ),
    case Viewer of
        true ->
            Session#{thread_viewer => true};
        false ->
            maps:without([thread_subscribed, thread_member_lists], Session#{
                thread_viewer => false
            })
    end;
recompute_session_viewer(_Active, _Loaded, Session) ->
    Session.

-spec state_opts(map()) -> map().
state_opts(Session) ->
    #{
        bot => type_conv:is_true(maps:get(bot, Session, false)),
        thread_viewer => session_viewer(Session)
    }.

-spec is_thread_type(term()) -> boolean().
is_thread_type(10) -> true;
is_thread_type(11) -> true;
is_thread_type(12) -> true;
is_thread_type(_) -> false.

-spec is_gated_channel_type(term()) -> boolean().
is_gated_channel_type(Type) ->
    is_thread_type(Type) orelse channel_types:is_thread_only(Type).

-spec channel_type(integer(), map()) -> integer() | undefined.
channel_type(ChannelId, State) ->
    case maps:get(ChannelId, guild_data_index_channels:channel_index(data(State)), undefined) of
        #{<<"type">> := Type} when is_integer(Type) -> Type;
        _ -> undefined
    end.

-spec thread(integer(), map()) -> map() | undefined.
thread(ThreadId, State) ->
    case store(State) of
        undefined -> undefined;
        Tab -> safe_get_thread(Tab, ThreadId)
    end.

-spec safe_get_thread(ets:table(), integer()) -> map() | undefined.
safe_get_thread(Tab, ThreadId) ->
    try
        guild_thread_store:get_thread(Tab, ThreadId)
    catch
        error:badarg -> undefined
    end.

-spec is_thread_id(integer(), map()) -> boolean().
is_thread_id(ChannelId, State) ->
    thread(ChannelId, State) =/= undefined.

-spec is_gated_channel_id(integer(), map()) -> boolean().
is_gated_channel_id(ChannelId, State) ->
    needs_variant(State) andalso
        case channel_type(ChannelId, State) of
            undefined -> true;
            Type -> is_gated_channel_type(Type)
        end.

-spec channel_visible(map(), term(), map()) -> boolean().
channel_visible(Session, ChannelId, State) when is_integer(ChannelId) ->
    session_viewer(Session) orelse non_viewer_visible(Session, ChannelId, State);
channel_visible(_Session, _ChannelId, _State) ->
    true.

-spec non_viewer_visible(map(), integer(), map()) -> boolean().
non_viewer_visible(Session, ChannelId, State) ->
    not needs_variant(State) orelse
        case channel_type(ChannelId, State) of
            4 -> category_visible(Session, ChannelId, State);
            undefined -> not is_thread_id(ChannelId, State);
            Type -> not is_gated_channel_type(Type)
        end.

-spec category_visible(map(), integer(), map()) -> boolean().
category_visible(Session, CategoryId, State) ->
    case maps:get(thread_forum_categories, data(State), #{}) of
        #{CategoryId := PlainChildren} ->
            plain_category_visible(Session, CategoryId, PlainChildren, State);
        _ ->
            true
    end.

-spec plain_category_visible(map(), integer(), [integer()], map()) -> boolean().
plain_category_visible(#{user_id := UserId}, CategoryId, PlainChildren, State) when
    is_integer(UserId)
->
    Member = guild_permissions_check:find_member_by_user_id(UserId, State),
    Member =/= undefined andalso
        (guild_permissions_check:can_view_channel_by_permissions(
            UserId, CategoryId, Member, State
        ) orelse
            lists:any(
                fun(ChildId) ->
                    guild_permissions_check:can_view_channel(UserId, ChildId, Member, State)
                end,
                PlainChildren
            ));
plain_category_visible(_Session, _CategoryId, _PlainChildren, _State) ->
    false.

-spec pre_update_filter(atom(), map(), map()) -> pass | drop | {filtered, map()}.
pre_update_filter(Event, Data, State) ->
    case active(State) of
        true -> pass;
        false -> inactive_filter(Event, Data)
    end.

-spec inactive_filter(atom(), map()) -> pass | drop | {filtered, map()}.
inactive_filter(Event, _Data) when
    Event =:= thread_create;
    Event =:= thread_update;
    Event =:= thread_delete;
    Event =:= thread_list_sync;
    Event =:= thread_member_update;
    Event =:= thread_members_update;
    Event =:= thread_member_list_update;
    Event =:= forum_unreads
->
    drop;
inactive_filter(Event, Data) when
    Event =:= channel_create; Event =:= channel_update; Event =:= channel_delete
->
    case is_gated_channel_type(maps:get(<<"type">>, Data, undefined)) of
        true -> drop;
        false -> pass
    end;
inactive_filter(channel_update_bulk, #{<<"channels">> := Channels} = Data) when
    is_list(Channels)
->
    case
        [
            C
         || C <- Channels,
            not (is_map(C) andalso is_gated_channel_type(maps:get(<<"type">>, C, undefined)))
        ]
    of
        Channels -> pass;
        [] -> drop;
        Kept -> {filtered, Data#{<<"channels">> => Kept}}
    end;
inactive_filter(message_create, #{<<"type">> := Type}) when Type =:= 18; Type =:= 21 ->
    drop;
inactive_filter(_Event, _Data) ->
    pass.

-spec event_scope(atom(), map(), map()) -> open | viewers | {thread, integer(), map()}.
event_scope(Event, Data, State) ->
    Scope = needs_variant(State) andalso viewer_only_event(Event, Data, State),
    case Scope of
        true -> viewers;
        false -> open;
        {thread, _ThreadId, _Thread} = Thread -> Thread
    end.

-spec viewer_sessions(map()) -> map().
viewer_sessions(Sessions) ->
    maps:filter(
        fun(_Sid, Session) -> is_map(Session) andalso session_viewer(Session) end, Sessions
    ).

-spec thread_sessions(atom(), map(), integer(), map(), binary() | undefined, map()) ->
    [{binary(), map()}].
thread_sessions(Event, Data, ThreadId, Thread, SessionIdOpt, State) ->
    MessageId =
        case guild_dispatch_filter:is_message_access_filtered_event(Event) of
            true -> guild_dispatch_filter:extract_message_id(Data);
            false -> undefined
        end,
    Ctx = {Thread, guild_thread_permissions:parent_id(Thread), MessageId, State},
    {Pairs, _Memo} = maps:fold(
        fun(Sid, Session, {Acc, Memo}) ->
            Candidate =
                Sid =/= SessionIdOpt andalso is_map(Session) andalso
                    session_viewer(Session) andalso
                    not type_conv:is_true(maps:get(pending_connect, Session, false)) andalso
                    thread_interested(Session, ThreadId, State) andalso Session,
            {Ok, Memo1} = thread_access(Candidate, Ctx, Memo),
            {[{Sid, Session} || Ok] ++ Acc, Memo1}
        end,
        {[], #{}},
        maps:get(sessions, State)
    ),
    Pairs.

-spec thread_interested(map(), integer(), map()) -> boolean().
thread_interested(Session, ThreadId, State) ->
    type_conv:is_true(maps:get(bot, Session, false)) orelse
        lists:member(ThreadId, maps:get(thread_member_lists, Session, [])) orelse
        is_thread_member(maps:get(user_id, Session, undefined), ThreadId, State) orelse
        (type_conv:is_true(maps:get(thread_subscribed, Session, false)) andalso
            not session_passive:is_passive(maps:get(id, State), Session)).

-spec thread_view_access(map() | false, map(), map(), #{integer() => boolean()}) ->
    {boolean(), #{integer() => boolean()}}.
thread_view_access(false, _Thread, _State, Memo) ->
    {false, Memo};
thread_view_access(Session, Thread, State, Memo) ->
    thread_access(
        Session, {Thread, guild_thread_permissions:parent_id(Thread), undefined, State}, Memo
    ).

-spec thread_access(map() | false, thread_ctx(), access_memo()) -> {boolean(), access_memo()}.
thread_access(#{user_id := UserId} = Session, Ctx, Memo) when is_integer(UserId) ->
    case parent_viewable(Session, Ctx) of
        true -> {true, Memo};
        false -> memo_thread_access(UserId, Ctx, Memo)
    end;
thread_access(_Session, _Ctx, Memo) ->
    {false, Memo}.

-spec parent_viewable(map(), thread_ctx()) -> boolean().
parent_viewable(#{viewable_channels := Viewable}, {Thread, ParentId, undefined, _State}) when
    is_map(Viewable), is_integer(ParentId)
->
    is_map_key(ParentId, Viewable) andalso maps:get(<<"type">>, Thread, undefined) =/= 12;
parent_viewable(_Session, _Ctx) ->
    false.

-spec memo_thread_access(integer(), thread_ctx(), access_memo()) -> {boolean(), access_memo()}.
memo_thread_access(UserId, {Thread, _ParentId, MessageId, State}, Memo) ->
    case Memo of
        #{UserId := Ok} ->
            {Ok, Memo};
        _ ->
            Ok = resolve_thread_access(UserId, Thread, MessageId, State),
            {Ok, store_access_memo(UserId, Ok, Memo)}
    end.

-spec store_access_memo(integer(), boolean(), access_memo()) -> access_memo().
store_access_memo(UserId, Ok, Memo) when map_size(Memo) < ?MAX_ACCESS_MEMO_ENTRIES ->
    Memo#{UserId => Ok};
store_access_memo(_UserId, _Ok, Memo) ->
    Memo.

-spec resolve_thread_access(integer(), map(), binary() | undefined, map()) -> boolean().
resolve_thread_access(UserId, Thread, MessageId, State) ->
    case guild_permissions_check:find_member_by_user_id(UserId, State) of
        undefined ->
            false;
        Member ->
            Perms = guild_thread_permissions:resolve(UserId, Thread, Member, State),
            bitset:has(Perms, constants:view_channel_permission()) andalso
                (MessageId =:= undefined orelse
                    guild_permissions_check:can_access_message_by_permissions(
                        Perms, MessageId, State
                    ))
    end.

-spec is_thread_member(term(), integer(), map()) -> boolean().
is_thread_member(UserId, ThreadId, State) when is_integer(UserId) ->
    case store(State) of
        undefined -> false;
        Tab -> guild_thread_store:is_member(Tab, ThreadId, UserId)
    end;
is_thread_member(_UserId, _ThreadId, _State) ->
    false.

-spec viewer_only_event(atom(), map(), map()) ->
    boolean() | {thread, integer(), map()}.
viewer_only_event(Event, Data, _State) when
    Event =:= channel_create; Event =:= channel_update; Event =:= channel_delete
->
    is_gated_channel_type(maps:get(<<"type">>, Data, undefined));
viewer_only_event(message_create, #{<<"type">> := Type} = Data, State) when
    Type =:= 18; Type =:= 21
->
    case channel_scoped_gated(Data, State) of
        {thread, _ThreadId, _Thread} = Thread -> Thread;
        _ -> true
    end;
viewer_only_event(message_update, #{<<"__thread_only_update">> := true}, _State) ->
    true;
viewer_only_event(guild_audit_log_entry_create, #{<<"__thread_scoped">> := _}, _State) ->
    true;
viewer_only_event(Event, Data, State) ->
    case guild_dispatch_filter:is_channel_scoped_event(Event) of
        true -> channel_scoped_gated(Data, State);
        false -> false
    end.

-spec channel_scoped_gated(map(), map()) -> boolean() | {thread, integer(), map()}.
channel_scoped_gated(Data, State) ->
    ChannelId = snowflake_id:parse_optional(maps:get(<<"channel_id">>, Data, undefined)),
    Thread =
        case ChannelId of
            ParsedId when is_integer(ParsedId) -> thread_or_context(ParsedId, Data, State);
            _ -> undefined
        end,
    case {ChannelId, Thread} of
        {Id, undefined} when is_integer(Id) -> is_gated_channel_id(Id, State);
        {Id, Found} when is_integer(Id) -> {thread, Id, Found};
        _ -> false
    end.

-spec thread_or_context(integer(), map(), map()) -> map() | undefined.
thread_or_context(ChannelId, Data, State) ->
    case thread(ChannelId, State) of
        undefined -> guild_state_threads:thread_context(ChannelId, Data);
        Thread -> Thread
    end.

-spec needs_variant(map()) -> boolean().
needs_variant(State) ->
    active(State) orelse tainted(State).

-spec recipient_active(integer(), map()) -> fun((integer()) -> boolean()).
recipient_active(ChannelId, State) ->
    Access =
        case is_gated_channel_id(ChannelId, State) andalso active(State) of
            false -> open;
            true -> channel_threads_config:config()
        end,
    fun(UserId) ->
        Access =:= open orelse
            channel_threads_config:user_active(Access, UserId) orelse
            (is_bot_member(UserId, State) andalso
                not channel_threads_config:user_excluded(Access, UserId))
    end.

-spec is_bot_member(integer(), map()) -> boolean().
is_bot_member(UserId, State) ->
    case guild_permissions_check:find_member_by_user_id(UserId, State) of
        #{<<"user">> := #{<<"bot">> := true}} -> true;
        _ -> false
    end.
