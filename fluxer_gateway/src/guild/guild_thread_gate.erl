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
    is_thread_only_type/1,
    thread/2,
    is_thread_id/2,
    channel_visible/3,
    pre_update_filter/3,
    event_scope/3,
    viewer_sessions/1,
    thread_sessions/6,
    thread_view_access/4,
    needs_variant/1,
    mask_payload/2,
    strip_channel_surface/1,
    mask_role/1,
    mask_overwrites/1,
    strip_internal/1,
    recipient_active/2
]).

-export_type([guild_state/0]).

-type guild_state() :: map().
-type session_pair() :: {binary(), map()}.
-type event_scope() :: open | viewers | {thread, integer(), map()}.
-type thread_ctx() :: {map(), integer() | undefined, binary() | undefined, guild_state()}.
-type access_memo() :: #{integer() => boolean()}.

-define(THREAD_MESSAGE_FLAGS, 16#120).
-define(MAX_ACCESS_MEMO_ENTRIES, 8192).
-define(THREAD_PERMISSION_BITS,
    (16#400000000 bor 16#800000000 bor 16#1000000000 bor 16#4000000000)
).
-define(SURFACE_KEYS, [
    <<"thread_metadata">>,
    <<"applied_tags">>,
    <<"available_tags">>,
    <<"default_auto_archive_duration">>,
    <<"default_thread_rate_limit_per_user">>,
    <<"default_reaction_emoji">>,
    <<"default_sort_order">>,
    <<"default_forum_layout">>,
    <<"default_tag_setting">>,
    <<"message_count">>,
    <<"total_message_sent">>,
    <<"member_ids_preview">>,
    <<"member_count">>,
    <<"flags">>
]).

-spec data(guild_state()) -> map().
data(#{data := Data}) when is_map(Data) ->
    Data;
data(Data) ->
    Data.

-spec active(guild_state()) -> boolean().
active(State) ->
    case data(State) of
        #{thread_gate := #{active := true}} -> true;
        _ -> false
    end.

-spec tainted(guild_state()) -> boolean().
tainted(State) ->
    case data(State) of
        #{thread_tainted := true} -> true;
        _ -> false
    end.

-spec store(guild_state()) -> ets:table() | undefined.
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

-spec session_fields(map(), term(), guild_state()) -> map().
session_fields(Request, UserId, State) ->
    Capable = maps:get(thread_channels_capable, Request, false) =:= true,
    Bot = maps:get(bot, Request, false) =:= true,
    #{
        thread_capable => Capable,
        thread_viewer => compute_viewer(active(State), Bot, Capable, UserId)
    }.

-spec recompute_session_viewers(guild_state()) -> guild_state().
recompute_session_viewers(State) ->
    Active = active(State),
    Loaded = channel_threads_config:enabled() orelse channel_threads_config:version() > 0,
    Sessions = maps:get(sessions, State, #{}),
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
        maps:get(bot, Session, false) =:= true,
        maps:get(thread_capable, Session, false) =:= true,
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
    #{bot => maps:get(bot, Session, false) =:= true, thread_viewer => session_viewer(Session)}.

-spec is_thread_type(term()) -> boolean().
is_thread_type(10) -> true;
is_thread_type(11) -> true;
is_thread_type(12) -> true;
is_thread_type(_) -> false.

-spec is_thread_only_type(term()) -> boolean().
is_thread_only_type(15) -> true;
is_thread_only_type(16) -> true;
is_thread_only_type(_) -> false.

-spec is_gated_channel_type(term()) -> boolean().
is_gated_channel_type(Type) ->
    is_thread_type(Type) orelse is_thread_only_type(Type).

-spec channel_type(integer(), guild_state()) -> integer() | undefined.
channel_type(ChannelId, State) ->
    case maps:get(ChannelId, guild_data_index:channel_index(data(State)), undefined) of
        #{<<"type">> := Type} when is_integer(Type) -> Type;
        _ -> undefined
    end.

-spec thread(integer(), guild_state()) -> map() | undefined.
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

-spec is_thread_id(integer(), guild_state()) -> boolean().
is_thread_id(ChannelId, State) ->
    thread(ChannelId, State) =/= undefined.

-spec is_gated_channel_id(integer(), guild_state()) -> boolean().
is_gated_channel_id(ChannelId, State) ->
    case active(State) orelse tainted(State) of
        false ->
            false;
        true ->
            case channel_type(ChannelId, State) of
                undefined -> true;
                Type -> is_gated_channel_type(Type)
            end
    end.

-spec channel_visible(map(), term(), guild_state()) -> boolean().
channel_visible(Session, ChannelId, State) when is_integer(ChannelId) ->
    session_viewer(Session) orelse non_viewer_visible(Session, ChannelId, State);
channel_visible(_Session, _ChannelId, _State) ->
    true.

-spec non_viewer_visible(map(), integer(), guild_state()) -> boolean().
non_viewer_visible(Session, ChannelId, State) ->
    case active(State) orelse tainted(State) of
        false ->
            true;
        true ->
            case channel_type(ChannelId, State) of
                4 ->
                    category_visible(Session, ChannelId, State);
                undefined ->
                    not is_thread_id(ChannelId, State);
                Type ->
                    not is_gated_channel_type(Type)
            end
    end.

-spec category_visible(map(), integer(), guild_state()) -> boolean().
category_visible(Session, CategoryId, State) ->
    case maps:get(thread_forum_categories, data(State), #{}) of
        #{CategoryId := PlainChildren} ->
            plain_category_visible(Session, CategoryId, PlainChildren, State);
        _ ->
            true
    end.

-spec plain_category_visible(map(), integer(), [integer()], guild_state()) -> boolean().
plain_category_visible(#{user_id := UserId}, CategoryId, PlainChildren, State) when
    is_integer(UserId)
->
    Member = guild_permissions:find_member_by_user_id(UserId, State),
    Member =/= undefined andalso
        (guild_permissions:can_view_channel_by_permissions(UserId, CategoryId, Member, State) orelse
            lists:any(
                fun(ChildId) ->
                    guild_permissions:can_view_channel(UserId, ChildId, Member, State)
                end,
                PlainChildren
            ));
plain_category_visible(_Session, _CategoryId, _PlainChildren, _State) ->
    false.

-spec pre_update_filter(atom(), map(), guild_state()) -> pass | drop | {filtered, map()}.
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

-spec event_scope(atom(), map(), guild_state()) -> event_scope().
event_scope(Event, Data, State) ->
    case active(State) orelse tainted(State) of
        false ->
            open;
        true ->
            case viewer_only_event(Event, Data, State) of
                true -> viewers;
                false -> open;
                {thread, _ThreadId, _Thread} = Thread -> Thread
            end
    end.

-spec viewer_sessions(map()) -> map().
viewer_sessions(Sessions) ->
    maps:filter(
        fun(_Sid, Session) -> is_map(Session) andalso session_viewer(Session) end, Sessions
    ).

-spec thread_sessions(atom(), map(), integer(), map(), binary() | undefined, guild_state()) ->
    [session_pair()].
thread_sessions(Event, Data, ThreadId, Thread, SessionIdOpt, State) ->
    MessageId =
        case guild_dispatch_filter:is_message_access_filtered_event(Event) of
            true -> guild_dispatch_filter:extract_message_id(Data);
            false -> undefined
        end,
    Ctx = {Thread, guild_thread_permissions:parent_id(Thread), MessageId, State},
    {Pairs, _Memo} = maps:fold(
        fun(Sid, Session, {Acc, Memo}) ->
            case
                Sid =/= SessionIdOpt andalso is_map(Session) andalso
                    session_viewer(Session) andalso
                    maps:get(pending_connect, Session, false) =/= true andalso
                    thread_interested(Session, ThreadId, State)
            of
                true ->
                    {Ok, Memo1} = thread_access(Session, Ctx, Memo),
                    {prepend(Ok, Sid, Session, Acc), Memo1};
                false ->
                    {Acc, Memo}
            end
        end,
        {[], #{}},
        maps:get(sessions, State, #{})
    ),
    Pairs.

-spec prepend(boolean(), binary(), map(), [session_pair()]) -> [session_pair()].
prepend(true, Sid, Session, Acc) -> [{Sid, Session} | Acc];
prepend(false, _Sid, _Session, Acc) -> Acc.

-spec thread_interested(map(), integer(), guild_state()) -> boolean().
thread_interested(Session, ThreadId, State) ->
    maps:get(bot, Session, false) =:= true orelse
        lists:member(ThreadId, maps:get(thread_member_lists, Session, [])) orelse
        is_thread_member(maps:get(user_id, Session, undefined), ThreadId, State) orelse
        (maps:get(thread_subscribed, Session, false) =:= true andalso
            not session_passive:is_passive(maps:get(id, State, 0), Session)).

-spec thread_view_access(map(), map(), guild_state(), access_memo()) ->
    {boolean(), access_memo()}.
thread_view_access(Session, Thread, State, Memo) ->
    thread_access(
        Session, {Thread, guild_thread_permissions:parent_id(Thread), undefined, State}, Memo
    ).

-spec thread_access(map(), thread_ctx(), access_memo()) -> {boolean(), access_memo()}.
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

-spec resolve_thread_access(integer(), map(), binary() | undefined, guild_state()) -> boolean().
resolve_thread_access(UserId, Thread, MessageId, State) ->
    case guild_permissions:find_member_by_user_id(UserId, State) of
        undefined ->
            false;
        Member ->
            Perms = guild_thread_permissions:resolve(UserId, Thread, Member, State),
            permission_bits:has(Perms, constants:view_channel_permission()) andalso
                (MessageId =:= undefined orelse
                    guild_permissions:can_access_message_by_permissions(
                        Perms, MessageId, State
                    ))
    end.

-spec is_thread_member(term(), integer(), guild_state()) -> boolean().
is_thread_member(UserId, ThreadId, State) when is_integer(UserId) ->
    case store(State) of
        undefined -> false;
        Tab -> guild_thread_store:is_member(Tab, ThreadId, UserId)
    end;
is_thread_member(_UserId, _ThreadId, _State) ->
    false.

-spec viewer_only_event(atom(), map(), guild_state()) ->
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

-spec channel_scoped_gated(map(), guild_state()) -> boolean() | {thread, integer(), map()}.
channel_scoped_gated(Data, State) ->
    case snowflake_id:parse_optional(maps:get(<<"channel_id">>, Data, undefined)) of
        ChannelId when is_integer(ChannelId) ->
            case thread_or_context(ChannelId, Data, State) of
                undefined -> is_gated_channel_id(ChannelId, State);
                Thread -> {thread, ChannelId, Thread}
            end;
        _ ->
            false
    end.

-spec thread_or_context(integer(), map(), guild_state()) -> map() | undefined.
thread_or_context(ChannelId, Data, State) ->
    case thread(ChannelId, State) of
        undefined -> guild_state_threads:thread_context(ChannelId, Data);
        Thread -> Thread
    end.

-spec needs_variant(guild_state()) -> boolean().
needs_variant(State) ->
    active(State) orelse tainted(State).

-spec mask_payload(atom(), map()) -> map().
mask_payload(Event, Data) when
    Event =:= message_create; Event =:= message_update
->
    mask_message(Data);
mask_payload(Event, Data) when
    Event =:= channel_create; Event =:= channel_update
->
    mask_overwrites(strip_channel_surface(Data));
mask_payload(channel_update_bulk, #{<<"channels">> := Channels} = Data) when
    is_list(Channels)
->
    Data#{
        <<"channels">> => [
            mask_overwrites(strip_channel_surface(Channel))
         || Channel <- Channels, is_map(Channel)
        ]
    };
mask_payload(Event, #{<<"role">> := Role} = Data) when
    is_map(Role), (Event =:= guild_role_create orelse Event =:= guild_role_update)
->
    Data#{<<"role">> => mask_role(Role)};
mask_payload(guild_role_update_bulk, #{<<"roles">> := Roles} = Data) when is_list(Roles) ->
    Data#{<<"roles">> => [mask_role(Role) || Role <- Roles, is_map(Role)]};
mask_payload(_Event, Data) ->
    Data.

-spec mask_message(map()) -> map().
mask_message(Message) ->
    Message1 = maps:remove(<<"thread">>, Message),
    Message2 =
        case maps:get(<<"flags">>, Message1, undefined) of
            Flags when is_integer(Flags) ->
                Message1#{<<"flags">> => Flags band bnot ?THREAD_MESSAGE_FLAGS};
            _ ->
                Message1
        end,
    Message3 =
        case maps:get(<<"referenced_message">>, Message2, undefined) of
            #{<<"type">> := 18} ->
                Message2#{<<"referenced_message">> => null};
            Referenced when is_map(Referenced) ->
                Message2#{<<"referenced_message">> => mask_message(Referenced)};
            _ ->
                Message2
        end,
    case maps:get(<<"message_snapshots">>, Message3, undefined) of
        Snapshots when is_list(Snapshots) ->
            Message3#{<<"message_snapshots">> => [mask_snapshot(S) || S <- Snapshots]};
        _ ->
            Message3
    end.

-spec mask_snapshot(term()) -> term().
mask_snapshot(#{<<"message">> := Message} = Snapshot) when is_map(Message) ->
    Snapshot#{<<"message">> => mask_message(Message)};
mask_snapshot(Snapshot) when is_map(Snapshot) ->
    mask_message(Snapshot);
mask_snapshot(Snapshot) ->
    Snapshot.

-spec strip_channel_surface(map()) -> map().
strip_channel_surface(Channel) ->
    maps:without(?SURFACE_KEYS, Channel).

-spec mask_role(map()) -> map().
mask_role(#{<<"permissions">> := Permissions} = Role) ->
    Role#{<<"permissions">> => mask_bits(Permissions)};
mask_role(Role) ->
    Role.

-spec mask_overwrites(map()) -> map().
mask_overwrites(#{<<"permission_overwrites">> := Overwrites} = Channel) when
    is_list(Overwrites)
->
    Channel#{<<"permission_overwrites">> => [mask_overwrite(O) || O <- Overwrites]};
mask_overwrites(Channel) ->
    Channel.

-spec mask_overwrite(term()) -> term().
mask_overwrite(Overwrite) when is_map(Overwrite) ->
    maps:map(
        fun
            (<<"allow">>, Value) -> mask_bits(Value);
            (<<"deny">>, Value) -> mask_bits(Value);
            (_Key, Value) -> Value
        end,
        Overwrite
    );
mask_overwrite(Overwrite) ->
    Overwrite.

-spec mask_bits(term()) -> term().
mask_bits(Value) when is_integer(Value) ->
    Value band bnot ?THREAD_PERMISSION_BITS;
mask_bits(Value) when is_binary(Value) ->
    case permission_bits:parse_optional(Value) of
        Bits when is_integer(Bits) -> integer_to_binary(Bits band bnot ?THREAD_PERMISSION_BITS);
        _ -> Value
    end;
mask_bits(Value) ->
    Value.

-spec strip_internal(map()) -> map().
strip_internal(Data) ->
    maps:filter(fun(Key, _Value) -> not internal_key(Key) end, Data).

-spec internal_key(term()) -> boolean().
internal_key(<<"_fluxer_", _/binary>>) -> true;
internal_key(<<"__thread_", _/binary>>) -> true;
internal_key(_) -> false.

-spec recipient_active(integer(), guild_state()) -> fun((integer()) -> boolean()).
recipient_active(ChannelId, State) ->
    case is_gated_channel_id(ChannelId, State) andalso active(State) of
        false ->
            fun(_UserId) -> true end;
        true ->
            Config = channel_threads_config:config(),
            fun(UserId) ->
                channel_threads_config:user_active(Config, UserId) orelse
                    (is_bot_member(UserId, State) andalso
                        not channel_threads_config:user_excluded(Config, UserId))
            end
    end.

-spec is_bot_member(integer(), guild_state()) -> boolean().
is_bot_member(UserId, State) ->
    case guild_permissions:find_member_by_user_id(UserId, State) of
        #{<<"user">> := #{<<"bot">> := true}} -> true;
        _ -> false
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

active_state() ->
    #{id => 1, data => #{thread_gate => #{active => true, version => 3}}}.

inactive_filter_drops_thread_events_test() ->
    State = #{id => 1, data => #{}},
    ?assertEqual(drop, pre_update_filter(thread_create, #{}, State)),
    ?assertEqual(drop, pre_update_filter(channel_create, #{<<"type">> => 15}, State)),
    ?assertEqual(drop, pre_update_filter(message_create, #{<<"type">> => 18}, State)),
    ?assertEqual(drop, pre_update_filter(channel_create, #{<<"type">> => 10}, State)),
    ?assertEqual(pass, pre_update_filter(channel_create, #{<<"type">> => 0}, State)),
    ?assertEqual(pass, pre_update_filter(channel_create, #{<<"type">> => 5}, State)),
    ?assertEqual(pass, pre_update_filter(message_create, #{<<"type">> => 0}, State)),
    ?assertEqual(pass, pre_update_filter(thread_create, #{}, active_state())),
    Text = #{<<"id">> => <<"1">>, <<"type">> => 0},
    Forum = #{<<"id">> => <<"2">>, <<"type">> => 15},
    ?assertEqual(
        pass, pre_update_filter(channel_update_bulk, #{<<"channels">> => [Text]}, State)
    ),
    ?assertEqual(
        {filtered, #{<<"channels">> => [Text]}},
        pre_update_filter(channel_update_bulk, #{<<"channels">> => [Text, Forum]}, State)
    ),
    ?assertEqual(
        drop, pre_update_filter(channel_update_bulk, #{<<"channels">> => [Forum]}, State)
    ).

event_scope_is_open_when_untainted_and_inactive_test() ->
    State = #{id => 1, data => #{}},
    ?assertEqual(open, event_scope(message_create, #{<<"type">> => 18}, State)),
    ?assertEqual(
        open, event_scope(message_delete_bulk, #{<<"channel_id">> => <<"77">>}, State)
    ).

event_scope_limits_gated_events_to_viewers_test() ->
    State = active_state(),
    ?assertEqual(viewers, event_scope(message_create, #{<<"type">> => 18}, State)),
    ?assertEqual(viewers, event_scope(channel_create, #{<<"type">> => 16}, State)),
    ?assertEqual(viewers, event_scope(channel_update, #{<<"type">> => 10}, State)),
    ?assertEqual(open, event_scope(channel_update, #{<<"type">> => 5}, State)),
    ?assertEqual(
        viewers, event_scope(message_update, #{<<"__thread_only_update">> => true}, State)
    ),
    ?assertEqual(open, event_scope(message_create, #{<<"type">> => 0}, State)),
    ?assertEqual(
        viewers, event_scope(message_delete_bulk, #{<<"channel_id">> => <<"77">>}, State)
    ),
    ?assertEqual(
        viewers,
        event_scope(
            message_delete_bulk,
            #{<<"channel_id">> => <<"77">>},
            #{id => 1, data => #{thread_tainted => true}}
        )
    ),
    Sessions = #{
        <<"a">> => #{user_id => 1, thread_viewer => true},
        <<"b">> => #{user_id => 2, thread_viewer => false}
    },
    ?assertEqual([<<"a">>], maps:keys(viewer_sessions(Sessions))).

mask_message_clears_nested_thread_artifacts_test() ->
    Message = #{
        <<"flags">> => 16#120 bor 4,
        <<"thread">> => #{<<"id">> => <<"9">>},
        <<"referenced_message">> => #{<<"flags">> => 16#20, <<"thread">> => #{}},
        <<"message_snapshots">> => [#{<<"message">> => #{<<"flags">> => 16#100}}]
    },
    Masked = mask_message(Message),
    ?assertEqual(4, maps:get(<<"flags">>, Masked)),
    ?assertNot(maps:is_key(<<"thread">>, Masked)),
    ?assertEqual(#{<<"flags">> => 0}, maps:get(<<"referenced_message">>, Masked)),
    ?assertEqual(
        [#{<<"message">> => #{<<"flags">> => 0}}], maps:get(<<"message_snapshots">>, Masked)
    ),
    ?assertEqual(
        null,
        maps:get(
            <<"referenced_message">>,
            mask_message(#{<<"referenced_message">> => #{<<"type">> => 18}})
        )
    ).

mask_role_and_overwrites_clear_thread_bits_test() ->
    Bits = integer_to_binary(?THREAD_PERMISSION_BITS bor 1024),
    ?assertEqual(#{<<"permissions">> => <<"1024">>}, mask_role(#{<<"permissions">> => Bits})),
    ?assertEqual(
        #{<<"permission_overwrites">> => [#{<<"allow">> => 1024, <<"deny">> => 0}]},
        mask_overwrites(#{
            <<"permission_overwrites">> => [
                #{<<"allow">> => ?THREAD_PERMISSION_BITS bor 1024, <<"deny">> => 1 bsl 38}
            ]
        })
    ).

strip_internal_drops_fluxer_keys_test() ->
    ?assertEqual(
        #{<<"id">> => 1},
        strip_internal(#{
            <<"id">> => 1, <<"_fluxer_thread">> => #{}, <<"__thread_only_update">> => true
        })
    ).

thread_sessions_skip_permission_resolve_for_viewable_public_parent_test() ->
    Viewer = #{
        user_id => 1, bot => true, thread_viewer => true, viewable_channels => #{10 => true}
    },
    Hidden = Viewer#{user_id => 2, viewable_channels => #{}},
    State = #{
        id => 1,
        data => #{},
        sessions => #{
            <<"a">> => Viewer, <<"b">> => Viewer, <<"c">> => Hidden, <<"d">> => Hidden
        }
    },
    Public = #{<<"id">> => <<"20">>, <<"parent_id">> => <<"10">>, <<"type">> => 11},
    meck:new(guild_thread_permissions, [passthrough, no_link]),
    meck:expect(guild_thread_permissions, resolve, fun(_UserId, _Thread, _Member, _State) ->
        0
    end),
    meck:new(guild_permissions, [passthrough, no_link]),
    meck:expect(guild_permissions, find_member_by_user_id, fun(_UserId, _State) -> #{} end),
    try
        Open = thread_sessions(typing_start, #{}, 20, Public, undefined, State),
        ?assertEqual([<<"a">>, <<"b">>], lists:sort([Sid || {Sid, _} <- Open])),
        ?assertEqual(1, meck:num_calls(guild_thread_permissions, resolve, '_')),
        meck:reset(guild_thread_permissions),
        Private = Public#{<<"type">> => 12},
        ?assertEqual([], thread_sessions(typing_start, #{}, 20, Private, undefined, State)),
        ?assertEqual(2, meck:num_calls(guild_thread_permissions, resolve, '_'))
    after
        meck:unload(guild_permissions),
        meck:unload(guild_thread_permissions)
    end.

compute_viewer_test() ->
    Key = channel_threads_config,
    Previous = persistent_term:get(Key, undefined),
    Config = (channel_threads_config:default_config())#{
        enabled => true,
        included_users => #{<<"5">> => true},
        excluded_users => #{<<"6">> => true}
    },
    persistent_term:put(Key, Config),
    try
        ?assert(compute_viewer(true, false, true, 5)),
        ?assertNot(compute_viewer(true, false, false, 5)),
        ?assertNot(compute_viewer(false, false, true, 5)),
        ?assertNot(compute_viewer(true, false, true, 7)),
        ?assert(compute_viewer(true, true, false, 7)),
        ?assertNot(compute_viewer(true, true, false, 6))
    after
        case Previous of
            undefined -> persistent_term:erase(Key);
            _ -> persistent_term:put(Key, Previous)
        end
    end.

-endif.
