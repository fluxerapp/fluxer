%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_thread_view).
-typing([eqwalizer]).

-export([
    guild_threads/4,
    list_sync/5,
    can_view/4,
    thread_payload/1,
    hide_for_non_viewer/4
]).

-type guild_state() :: map().

-spec guild_threads(integer(), map(), map() | undefined, guild_state()) -> [map()].
guild_threads(UserId, Opts, Member, State) ->
    case guild_thread_gate:store(State) of
        undefined ->
            [];
        Tab ->
            case maps:get(bot, Opts, false) of
                true -> bot_threads(UserId, Member, Tab, State);
                false -> joined_threads(UserId, Member, Tab, State)
            end
    end.

-spec bot_threads(integer(), map() | undefined, ets:table(), guild_state()) -> [map()].
bot_threads(UserId, Member, Tab, State) ->
    [
        with_own_member(UserId, Thread, Tab)
     || Thread <- guild_thread_store:threads(Tab), can_view(UserId, Thread, Member, State)
    ].

-spec joined_threads(integer(), map() | undefined, ets:table(), guild_state()) -> [map()].
joined_threads(UserId, Member, Tab, State) ->
    lists:filtermap(
        fun(ThreadId) ->
            case guild_thread_store:get_thread(Tab, ThreadId) of
                undefined ->
                    false;
                Thread ->
                    case can_view(UserId, Thread, Member, State) of
                        true -> {true, with_own_member(UserId, Thread, Tab)};
                        false -> false
                    end
            end
        end,
        guild_thread_store:user_thread_ids(Tab, UserId)
    ).

-spec with_own_member(integer(), map(), ets:table()) -> map().
with_own_member(UserId, Thread, Tab) ->
    Payload = thread_payload(Thread),
    case guild_thread_store:get_member(Tab, thread_id(Thread), UserId) of
        undefined -> Payload;
        ThreadMember -> Payload#{<<"member">> => own_member_payload(ThreadMember)}
    end.

-spec own_member_payload(map()) -> map().
own_member_payload(ThreadMember) ->
    maps:without([<<"id">>, <<"user_id">>, <<"member">>, <<"presence">>], ThreadMember).

-spec thread_payload(map()) -> map().
thread_payload(Thread) ->
    maps:without([<<"member_ids_preview">>, <<"member">>], Thread).

-spec thread_id(map()) -> integer().
thread_id(Thread) ->
    snowflake_id:parse(maps:get(<<"id">>, Thread)).

-spec can_view(integer(), map(), map() | undefined, guild_state()) -> boolean().
can_view(UserId, Thread, Member, State) ->
    Perms = guild_thread_permissions:resolve(UserId, Thread, Member, State),
    permission_bits:has(Perms, constants:view_channel_permission()).

-spec viewable_thread_ids(
    integer(), [integer()] | all | joined, map() | undefined, guild_state()
) ->
    [{integer(), map()}].
viewable_thread_ids(UserId, Parents, Member, State) ->
    case guild_thread_gate:store(State) of
        undefined ->
            [];
        Tab ->
            [
                {thread_id(Thread), Thread}
             || Thread <- candidate_threads(Parents, UserId, Tab),
                can_view(UserId, Thread, Member, State)
            ]
    end.

-spec candidate_threads([integer()] | all | joined, integer(), ets:table()) -> [map()].
candidate_threads(all, _UserId, Tab) ->
    guild_thread_store:threads(Tab);
candidate_threads(joined, UserId, Tab) ->
    stored_threads(guild_thread_store:user_thread_ids(Tab, UserId), Tab);
candidate_threads(Parents, _UserId, Tab) ->
    stored_threads(
        lists:append([guild_thread_store:parent_thread_ids(Tab, P) || P <- Parents]), Tab
    ).

-spec stored_threads([integer()], ets:table()) -> [map()].
stored_threads(ThreadIds, Tab) ->
    lists:filtermap(
        fun(ThreadId) ->
            case guild_thread_store:get_thread(Tab, ThreadId) of
                undefined -> false;
                Thread -> {true, Thread}
            end
        end,
        ThreadIds
    ).

-spec list_sync(
    integer(), integer(), [integer()] | all | joined, map() | undefined, guild_state()
) ->
    map().
list_sync(GuildId, UserId, Parents, Member, State) ->
    Viewable = viewable_thread_ids(UserId, Parents, Member, State),
    Members =
        case guild_thread_gate:store(State) of
            undefined ->
                [];
            Tab ->
                [
                    M
                 || {ThreadId, _Thread} <- Viewable,
                    M <- [guild_thread_store:get_member(Tab, ThreadId, UserId)],
                    M =/= undefined
                ]
        end,
    Base = #{
        <<"guild_id">> => integer_to_binary(GuildId),
        <<"threads">> => [thread_payload(Thread) || {_Id, Thread} <- Viewable],
        <<"members">> => Members
    },
    case Parents of
        all -> Base;
        joined -> Base;
        _ -> Base#{<<"channel_ids">> => [integer_to_binary(P) || P <- Parents]}
    end.

-spec hide_for_non_viewer(integer(), map() | undefined, guild_state(), [map()]) -> [map()].
hide_for_non_viewer(UserId, Member, State, Channels) ->
    Kept = [C || C <- Channels, not guild_thread_gate:is_thread_only_type(channel_type(C))],
    Visible =
        case length(Kept) =:= length(Channels) of
            true -> Kept;
            false -> drop_forum_only_categories(UserId, Member, State, Kept)
        end,
    [
        guild_thread_gate:mask_overwrites(guild_thread_gate:strip_channel_surface(C))
     || C <- Visible
    ].

-spec drop_forum_only_categories(integer(), map() | undefined, guild_state(), [map()]) ->
    [map()].
drop_forum_only_categories(UserId, Member, State, Channels) ->
    Parents = sets:from_list([
        P
     || C <- Channels,
        channel_type(C) =/= 4,
        P <- [snowflake_id:parse_maybe(maps:get(<<"parent_id">>, C, undefined))],
        is_integer(P)
    ]),
    lists:filter(
        fun(C) ->
            case
                {channel_type(C), snowflake_id:parse_maybe(maps:get(<<"id">>, C, undefined))}
            of
                {4, Id} when is_integer(Id) ->
                    sets:is_element(Id, Parents) orelse
                        guild_permissions:can_view_channel_by_permissions(
                            UserId, Id, Member, State
                        ) orelse
                        guild_virtual_channel_access:has_virtual_access(UserId, Id, State);
                _ ->
                    true
            end
        end,
        Channels
    ).

-spec channel_type(map()) -> integer() | undefined.
channel_type(Channel) ->
    guild_data_normalize_schema:int(maps:get(<<"type">>, Channel, undefined)).
