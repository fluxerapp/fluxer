%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_visitor_snapshot).
-typing([eqwalizer]).

-export([view/2, build/2]).

-define(MAX_SNAPSHOT_THREADS, 200).

-type visibility() :: #{adult := boolean(), thread_viewer := boolean(), atom() => term()}.

-spec view(
    #{adult := boolean(), thread_viewer := boolean(), atom() => term()},
    guild_data:guild_state()
) ->
    {#{integer() => true}, guild_data:guild_state()}.
view(#{adult := Adult, thread_viewer := Viewer} = Visibility, State) ->
    Visitors = guild_visitor_state:get(State),
    Views = maps:get(views, Visitors),
    Key = {Adult, Viewer},
    case maps:get(Key, Views, undefined) of
        View when is_map(View) ->
            {View, State};
        _ ->
            View = compute_view(Visibility, State),
            {View, guild_visitor_state:put(Visitors#{views => Views#{Key => View}}, State)}
    end.

-spec compute_view(visibility(), guild_data:guild_state()) -> #{integer() => true}.
compute_view(#{adult := Adult, thread_viewer := Viewer}, State) ->
    Full = guild_visitor_permissions:viewable_channel_map(State, Adult),
    case Viewer orelse not guild_thread_gate:needs_variant(State) of
        true ->
            Full;
        false ->
            Index = guild_data_index_channels:channel_index(
                guild_data_index:ensure_data_map(State)
            ),
            Hidden = [
                ChannelId
             || ChannelId <- maps:keys(Full),
                thread_only_channel(maps:get(ChannelId, Index, #{}))
            ],
            maps:without(Hidden, Full)
    end.

-spec thread_only_channel(term()) -> boolean().
thread_only_channel(Channel) when is_map(Channel) ->
    channel_types:is_thread_only(
        map_utils:get_integer(Channel, <<"type">>, undefined)
    );
thread_only_channel(_) ->
    false.

-spec build(guild_visitors:visitor_session(), guild_data:guild_state()) -> map().
build(Session, State) ->
    Data = guild_data_index:ensure_data_map(State),
    GuildId = maps:get(id, State),
    View = maps:get(viewable_channels, Session),
    Viewer = maps:get(thread_viewer, Session),
    AllChannels = map_utils:ensure_list(guild_data_channels:channels_from_data(Data)),
    Visible = [C || C <- AllChannels, maps:is_key(channel_id(C), View)],
    Channels = [
        mask_channel(Session, guild_visitor_permissions:strip_overwrites(C, GuildId), State)
     || C <- with_parents(Visible, AllChannels)
    ],
    Roles = [
        mask_role(Session, R, State)
     || R <- map_utils:ensure_list(maps:get(<<"roles">>, Data, []))
    ],
    Base = #{
        <<"id">> => integer_to_binary(GuildId),
        <<"properties">> => maps:get(<<"guild">>, Data, #{}),
        <<"roles">> => Roles,
        <<"channels">> => Channels,
        <<"emojis">> => maps:get(<<"emojis">>, Data, []),
        <<"stickers">> => maps:get(<<"stickers">>, Data, []),
        <<"members">> => [],
        <<"member_count">> => maps:get(
            member_count, State, guild_data_index_members:member_count(Data)
        ),
        <<"online_count">> => guild_member_list_read:get_online_count(State),
        <<"presences">> => [],
        <<"voice_states">> => [],
        <<"joined_at">> => null,
        <<"visiting">> => true
    },
    case Viewer andalso guild_thread_gate:active(State) of
        true -> Base#{<<"threads">> => visitor_threads(View, State)};
        false -> Base
    end.

-spec visitor_threads(#{integer() => true}, guild_data:guild_state()) -> [map()].
visitor_threads(View, State) ->
    Threads =
        case guild_thread_gate:store(State) of
            undefined -> [];
            Tab -> guild_thread_store:threads(Tab)
        end,
    Visible = [
        guild_thread_view:thread_payload(Thread)
     || Thread <- Threads, public_thread_in_view(Thread, View)
    ],
    Sorted = lists:sort(fun(A, B) -> channel_id(A) >= channel_id(B) end, Visible),
    lists:sublist(Sorted, ?MAX_SNAPSHOT_THREADS).

-spec public_thread_in_view(map(), #{integer() => true}) -> boolean().
public_thread_in_view(Thread, View) ->
    channel_types:is_public_thread(map_utils:get_integer(Thread, <<"type">>, undefined)) andalso
        maps:is_key(parent_id(Thread), View).

-spec mask_channel(visibility(), map(), guild_data:guild_state()) -> map().
mask_channel(#{thread_viewer := true}, Channel, _State) ->
    Channel;
mask_channel(#{thread_viewer := false}, Channel, State) ->
    case guild_thread_gate:needs_variant(State) of
        true ->
            guild_thread_payload:mask_overwrites(
                guild_thread_payload:strip_channel_surface(Channel)
            );
        false ->
            Channel
    end.

-spec mask_role(visibility(), term(), guild_data:guild_state()) -> term().
mask_role(#{thread_viewer := false}, Role, State) when is_map(Role) ->
    case guild_thread_gate:needs_variant(State) of
        true -> guild_thread_payload:mask_role(Role);
        false -> Role
    end;
mask_role(_Visibility, Role, _State) ->
    Role.

-spec with_parents([map()], [map()]) -> [map()].
with_parents(Visible, All) ->
    Ids = maps:from_keys([channel_id(C) || C <- Visible], true),
    Missing = lists:usort([
        P
     || C <- Visible,
        P <- [parent_id(C)],
        is_integer(P),
        not maps:is_key(P, Ids)
    ]),
    Extra = [C || C <- All, lists:member(channel_id(C), Missing)],
    Visible ++ Extra.

-spec channel_id(term()) -> integer() | undefined.
channel_id(Channel) when is_map(Channel) ->
    snowflake_id:parse_maybe(maps:get(<<"id">>, Channel, undefined));
channel_id(_) ->
    undefined.

-spec parent_id(map()) -> integer() | undefined.
parent_id(Channel) ->
    snowflake_id:parse_maybe(maps:get(<<"parent_id">>, Channel, undefined)).
