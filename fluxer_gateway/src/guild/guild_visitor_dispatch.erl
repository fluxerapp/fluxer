%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_visitor_dispatch).
-typing([eqwalizer]).

-export([
    after_dispatch/4,
    channel_visible/3
]).

-define(GUILD_VIEW_EVENTS, [
    guild_update,
    guild_role_create,
    guild_role_update,
    guild_role_update_bulk,
    guild_role_delete
]).
-define(GUILD_EVENTS, ?GUILD_VIEW_EVENTS ++ [guild_emojis_update, guild_stickers_update]).
-define(CHANNEL_EVENTS, [channel_create, channel_update, channel_update_bulk, channel_delete]).
-define(FOCUSED_EVENTS, [
    message_create,
    message_update,
    message_delete,
    message_delete_bulk,
    message_reaction_add,
    message_reaction_remove,
    message_reaction_remove_all,
    message_reaction_remove_emoji
]).
-define(THREAD_EVENTS, [thread_create, thread_update, thread_delete]).
-define(VIEW_EVENTS, ?GUILD_VIEW_EVENTS ++ ?CHANNEL_EVENTS).

-type guild_state() :: map().
-type session() :: guild_visitors:visitor_session().
-type delivery() :: {atom(), pid(), map()}.

-spec allowed(atom()) -> boolean().
allowed(Event) ->
    scope(Event) =/= none.

-spec visitor_payload(atom(), map()) -> map().
visitor_payload(Event, Data) when
    Event =:= message_reaction_add; Event =:= message_reaction_remove
->
    maps:without([<<"user_id">>, <<"member">>, <<"session_id">>], Data);
visitor_payload(Event, Data) when
    Event =:= channel_create; Event =:= channel_update; Event =:= channel_delete
->
    strip_channel(Data);
visitor_payload(channel_update_bulk, #{<<"channels">> := Channels} = Data) when
    is_list(Channels)
->
    Data#{<<"channels">> => [strip_channel(C) || C <- Channels, is_map(C)]};
visitor_payload(Event, Data) when Event =:= thread_create; Event =:= thread_update ->
    maps:without([<<"member">>, <<"member_ids_preview">>], Data);
visitor_payload(thread_delete, Data) ->
    maps:with([<<"id">>, <<"guild_id">>, <<"parent_id">>, <<"type">>], Data);
visitor_payload(_Event, Data) ->
    Data.

-spec strip_channel(map()) -> map().
strip_channel(Channel) ->
    case snowflake_id:parse_maybe(maps:get(<<"guild_id">>, Channel, undefined)) of
        GuildId when is_integer(GuildId) ->
            guild_visitor_permissions:strip_overwrites(Channel, GuildId);
        _ ->
            Channel#{<<"permission_overwrites">> => []}
    end.

-spec after_dispatch(atom(), map(), map(), map()) -> map().
after_dispatch(guild_member_add, Data, _OldState, State) ->
    guild_visitors:promote_user(member_user_id(Data), State);
after_dispatch(Event, Data, OldState, State) ->
    case guild_visitors:count(State) of
        0 -> State;
        _ -> visitor_dispatch(Event, Data, OldState, State)
    end.

-spec visitor_dispatch(atom(), map(), guild_state(), guild_state()) -> guild_state().
visitor_dispatch(guild_delete, _Data, _OldState, State) ->
    guild_visitors:evict_all(removed, State);
visitor_dispatch(Event, Data0, OldState, State0) ->
    GuildId = maps:get(id, State0),
    Data = (guild_thread_payload:strip_internal(Data0))#{
        <<"guild_id">> => integer_to_binary(GuildId)
    },
    State1 =
        case Event of
            guild_update -> guild_visitors:evict_minors(guild_visitors:evict_disabled(State0));
            _ -> State0
        end,
    {ViewDeliveries, State2} = refresh_for(Event, Data, OldState, State1),
    Deliveries =
        case allowed(Event) of
            true -> event_deliveries(Event, Data, State2);
            false -> []
        end,
    ok = deliver(ViewDeliveries ++ Deliveries, State2),
    State2.

-spec refresh_for(atom(), map(), guild_state(), guild_state()) -> {[delivery()], guild_state()}.
refresh_for(Event, Data, OldState, State) ->
    case lists:member(Event, ?VIEW_EVENTS) of
        false ->
            {[], State};
        true ->
            Before = guild_visitors:sessions(State),
            State1 = guild_visitors:refresh_views(State),
            After = guild_visitors:sessions(State1),
            {view_deliveries(Event, Data, Before, After, OldState, State1), State1}
    end.

-spec view_deliveries(atom(), map(), map(), map(), guild_state(), guild_state()) ->
    [delivery()].
view_deliveries(Event, Data, Before, After, OldState, State) ->
    EventIds = event_channel_ids(Event, Data),
    OldIndex = channel_index(OldState),
    NewIndex = channel_index(State),
    GuildId = maps:get(id, State),
    Context = #{
        event => Event,
        data => Data,
        before => Before,
        event_ids => EventIds,
        old_index => OldIndex,
        new_index => NewIndex,
        guild_id => GuildId,
        state => State
    },
    maps:fold(
        fun(SessionId, Session, Acc) ->
            session_view_deliveries(SessionId, Session, Context) ++ Acc
        end,
        [],
        After
    ).

-spec session_view_deliveries(binary(), session(), map()) -> [delivery()].
session_view_deliveries(SessionId, #{pid := Pid, viewable_channels := New} = Session, #{
    event := Event,
    data := Data,
    before := Before,
    event_ids := EventIds,
    old_index := OldIndex,
    new_index := NewIndex,
    guild_id := GuildId,
    state := State
}) ->
    Old =
        case maps:get(SessionId, Before, undefined) of
            #{viewable_channels := View} -> View;
            _ -> New
        end,
    Lost = [Id || Id <- maps:keys(Old), not maps:is_key(Id, New)],
    Gained = [Id || Id <- maps:keys(New), not maps:is_key(Id, Old)],
    Deletes = [
        {channel_delete, Pid, lost_payload(Event, Id, Data, OldIndex, GuildId)}
     || Id <- Lost
    ],
    ChangesChannels = lists:member(Event, [channel_create, channel_update, channel_update_bulk]),
    Creates = [
        {channel_create, Pid,
            mask_for(
                Session, channel_create, strip_channel(with_guild(Channel, GuildId)), State
            )}
     || Id <- Gained,
        not (ChangesChannels andalso lists:member(Id, EventIds)),
        Channel <- [maps:get(Id, NewIndex, undefined)],
        is_map(Channel)
    ],
    Deletes ++ Creates.

-spec lost_payload(atom(), integer(), map(), map(), integer()) -> map().
lost_payload(channel_delete, Id, Data, OldIndex, GuildId) ->
    case snowflake_id:parse_maybe(maps:get(<<"id">>, Data, undefined)) of
        Id -> visitor_payload(channel_delete, Data);
        _ -> minimal_delete(Id, OldIndex, GuildId)
    end;
lost_payload(_Event, Id, _Data, OldIndex, GuildId) ->
    minimal_delete(Id, OldIndex, GuildId).

-spec minimal_delete(integer(), map(), integer()) -> map().
minimal_delete(Id, OldIndex, GuildId) ->
    Base = #{<<"id">> => integer_to_binary(Id), <<"guild_id">> => integer_to_binary(GuildId)},
    case maps:get(Id, OldIndex, undefined) of
        #{<<"type">> := Type} = Channel ->
            maps:merge(Base#{<<"type">> => Type}, maps:with([<<"parent_id">>], Channel));
        _ ->
            Base
    end.

-spec with_guild(map(), integer()) -> map().
with_guild(Channel, GuildId) ->
    Channel#{<<"guild_id">> => integer_to_binary(GuildId)}.

-spec event_channel_ids(atom(), map()) -> [integer()].
event_channel_ids(channel_update_bulk, #{<<"channels">> := Channels}) when is_list(Channels) ->
    [Id || C <- Channels, is_map(C), Id <- [channel_id(C)], is_integer(Id)];
event_channel_ids(Event, Data) when
    Event =:= channel_create; Event =:= channel_update; Event =:= channel_delete
->
    case channel_id(Data) of
        Id when is_integer(Id) -> [Id];
        _ -> []
    end;
event_channel_ids(_Event, _Data) ->
    [].

-spec event_deliveries(atom(), map(), guild_state()) -> [delivery()].
event_deliveries(Event, Data, State) ->
    Payload = visitor_payload(Event, Data),
    case scope(Event) of
        focused ->
            focused_deliveries(Event, Payload, Data, State);
        Scope ->
            scoped_deliveries(Scope, Event, Payload, Data, State)
    end.

-spec focused_deliveries(atom(), map(), map(), guild_state()) -> [delivery()].
focused_deliveries(Event, Payload, Data, State) ->
    case snowflake_id:parse_maybe(maps:get(<<"channel_id">>, Data, undefined)) of
        ChannelId when is_integer(ChannelId) ->
            [
                {Event, Pid, mask_for(S, Event, Payload, State)}
             || #{pid := Pid} = S <- guild_visitors:focused_sessions(ChannelId, State),
                channel_visible(S, ChannelId, State)
            ];
        _ ->
            []
    end.

-spec scoped_deliveries(
    guild | channel | pins | thread | none, atom(), map(), map(), guild_state()
) ->
    [delivery()].
scoped_deliveries(Scope, Event, Payload, Data, State) ->
    Sessions = maps:values(guild_visitors:sessions(State)),
    case Scope of
        guild ->
            [{Event, Pid, mask_for(S, Event, Payload, State)} || #{pid := Pid} = S <- Sessions];
        channel ->
            channel_deliveries(Event, Payload, Sessions, State);
        pins ->
            ChannelId = snowflake_id:parse_maybe(maps:get(<<"channel_id">>, Data, undefined)),
            [
                {Event, Pid, Payload}
             || #{pid := Pid} = S <- Sessions,
                is_integer(ChannelId),
                channel_visible(S, ChannelId, State)
            ];
        thread ->
            [
                {Event, Pid, Payload}
             || #{pid := Pid} = S <- Sessions,
                thread_visible(S, Data, State)
            ];
        none ->
            []
    end.

-spec channel_deliveries(atom(), map(), [session()], guild_state()) -> [delivery()].
channel_deliveries(channel_delete, _Payload, _Sessions, _State) ->
    [];
channel_deliveries(
    channel_update_bulk, #{<<"channels">> := Channels} = Payload, Sessions, State
) ->
    lists:filtermap(
        fun(Session) -> bulk_channel_delivery(Session, Channels, Payload, State) end,
        Sessions
    );
channel_deliveries(Event, Payload, Sessions, State) ->
    ChannelId = channel_id(Payload),
    [
        {Event, Pid, mask_for(S, Event, Payload, State)}
     || #{pid := Pid, viewable_channels := View} = S <- Sessions,
        is_integer(ChannelId),
        maps:is_key(ChannelId, View)
    ].

-spec bulk_channel_delivery(session(), [map()], map(), guild_state()) ->
    false | {true, delivery()}.
bulk_channel_delivery(
    #{pid := Pid, viewable_channels := View} = Session, Channels, Payload, State
) ->
    case [C || C <- Channels, is_map(C), maps:is_key(channel_id(C), View)] of
        [] ->
            false;
        Visible ->
            {true,
                {channel_update_bulk, Pid,
                    mask_for(
                        Session, channel_update_bulk, Payload#{<<"channels">> => Visible}, State
                    )}}
    end.

-spec scope(atom()) -> guild | channel | focused | pins | thread | none.
scope(Event) ->
    case
        {
            lists:member(Event, ?GUILD_EVENTS),
            lists:member(Event, ?CHANNEL_EVENTS),
            lists:member(Event, ?FOCUSED_EVENTS),
            lists:member(Event, ?THREAD_EVENTS)
        }
    of
        {true, _, _, _} -> guild;
        {_, true, _, _} -> channel;
        {_, _, true, _} -> focused;
        {_, _, _, true} -> thread;
        _ when Event =:= channel_pins_update -> pins;
        _ -> none
    end.

-spec mask_for(session(), atom(), map(), guild_state()) -> map().
mask_for(#{thread_viewer := true}, _Event, Payload, _State) ->
    Payload;
mask_for(_Session, Event, Payload, State) ->
    case guild_thread_gate:needs_variant(State) of
        true -> guild_thread_payload:mask_payload(Event, Payload);
        false -> Payload
    end.

-spec channel_visible(guild_visitors:visitor_session(), integer(), map()) -> boolean().
channel_visible(#{viewable_channels := View} = Session, ChannelId, State) ->
    maps:is_key(ChannelId, View) orelse
        case guild_thread_gate:thread(ChannelId, State) of
            Thread when is_map(Thread) -> thread_visible(Session, Thread, State);
            _ -> false
        end.

-spec thread_visible(session(), map(), guild_state()) -> boolean().
thread_visible(#{viewable_channels := View} = Session, Thread, State) ->
    Viewer = maps:get(thread_viewer, Session, false) orelse not guild_thread_gate:active(State),
    PublicThread =
        Viewer andalso
            channel_types:is_public_thread(
                map_utils:get_integer(Thread, <<"type">>, undefined)
            ),
    PublicThread andalso
        maps:is_key(
            snowflake_id:parse_maybe(maps:get(<<"parent_id">>, Thread, undefined)), View
        ).

-spec deliver([delivery()], guild_state()) -> ok.
deliver([], _State) ->
    ok;
deliver(Deliveries, State) ->
    GuildId = maps:get(id, State),
    Grouped = lists:foldl(
        fun({Event, Pid, Payload}, Acc) ->
            Key = {Event, Payload},
            Acc#{Key => [Pid | maps:get(Key, Acc, [])]}
        end,
        #{},
        Deliveries
    ),
    maps:foreach(
        fun({Event, Payload}, Pids) ->
            Encoded =
                {pre_encoded,
                    iolist_to_binary(
                        json:encode(guild_data_wire:payload(Payload), fun json:encode_value/2)
                    )},
            gateway_dispatch_relay:dispatch_many(lists:reverse(Pids), Event, Encoded, GuildId)
        end,
        Grouped
    ).

-spec channel_index(guild_state()) -> map().
channel_index(State) ->
    guild_data_index_channels:channel_index(guild_data_index:ensure_data_map(State)).

-spec channel_id(map()) -> integer() | undefined.
channel_id(Channel) ->
    snowflake_id:parse_maybe(maps:get(<<"id">>, Channel, undefined)).

-spec member_user_id(map()) -> integer() | undefined.
member_user_id(#{<<"user">> := #{<<"id">> := Id}}) ->
    snowflake_id:parse_maybe(Id);
member_user_id(#{<<"user_id">> := Id}) ->
    snowflake_id:parse_maybe(Id);
member_user_id(_) ->
    undefined.
