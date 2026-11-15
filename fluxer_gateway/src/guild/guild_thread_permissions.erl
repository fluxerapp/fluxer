%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_thread_permissions).
-typing([eqwalizer]).

-export([
    resolve/4,
    parent_id/1,
    unknown_parent_count/0,
    init_counters/0
]).

-define(ALL_PERMISSIONS, 16#FFFFFFFFFFFFFFFF).
-define(SEND_MESSAGES, 2048).
-define(UNKNOWN_PARENT_KEY, {?MODULE, unknown_parent}).

-type permission() :: non_neg_integer().
-type guild_state() :: map().

-export_type([permission/0, guild_state/0]).

-spec resolve(integer(), map(), map() | undefined, guild_state()) -> permission().
resolve(UserId, Thread, Member, State) ->
    case parent_channel(Thread, State) of
        undefined ->
            note_unknown_parent(),
            0;
        ParentId ->
            parent_permissions(UserId, ParentId, Thread, Member, State)
    end.

-spec parent_permissions(integer(), integer(), map(), map() | undefined, guild_state()) ->
    permission().
parent_permissions(UserId, ParentId, Thread, Member, State) ->
    ParentPerms = guild_permissions:compute_member_permissions(UserId, ParentId, Member, State),
    Perms = alias_permissions(ParentPerms),
    case can_view_thread_from_parent(UserId, Thread, Perms, Member, State) of
        true -> Perms;
        false -> Perms band bnot constants:view_channel_permission()
    end.

-spec alias_permissions(permission()) -> permission().
alias_permissions(?ALL_PERMISSIONS) ->
    ?ALL_PERMISSIONS;
alias_permissions(Perms) ->
    case bitset:has(Perms, constants:send_messages_in_threads_permission()) of
        true -> Perms bor ?SEND_MESSAGES;
        false -> Perms band bnot ?SEND_MESSAGES
    end.

-spec is_moderator(permission(), map() | undefined) -> boolean().
is_moderator(?ALL_PERMISSIONS, _Member) ->
    true;
is_moderator(Perms, Member) ->
    bitset:has(Perms, constants:administrator_permission()) orelse
        (bitset:has(Perms, constants:manage_threads_permission()) andalso
            not timed_out(Member)).

-spec can_view_thread_from_parent(
    integer(), map(), permission(), map() | undefined, guild_state()
) ->
    boolean().
can_view_thread_from_parent(UserId, Thread, Perms, Member, State) ->
    bitset:has(Perms, constants:view_channel_permission()) andalso
        (not is_private(Thread) orelse is_moderator(Perms, Member) orelse
            is_thread_member(UserId, Thread, State)).

-spec is_private(map()) -> boolean().
is_private(#{<<"type">> := 12}) -> true;
is_private(_) -> false.

-spec parent_id(map()) -> integer() | undefined.
parent_id(Thread) ->
    snowflake_id:parse_optional(maps:get(<<"parent_id">>, Thread, undefined)).

-spec parent_channel(map(), guild_state()) -> integer() | undefined.
parent_channel(Thread, State) ->
    case parent_id(Thread) of
        ParentId when is_integer(ParentId) ->
            existing_channel(ParentId, State);
        _ ->
            undefined
    end.

-spec existing_channel(integer(), guild_state()) -> integer() | undefined.
existing_channel(ChannelId, State) ->
    case guild_permissions_check:find_channel_by_id(ChannelId, State) of
        undefined -> undefined;
        _Channel -> ChannelId
    end.

-spec is_thread_member(integer(), map(), guild_state()) -> boolean().
is_thread_member(UserId, Thread, State) ->
    case
        {
            guild_thread_gate:store(State),
            snowflake_id:parse_optional(maps:get(<<"id">>, Thread, undefined))
        }
    of
        {Tab, ThreadId} when Tab =/= undefined, is_integer(ThreadId) ->
            stored_or_listed_member(Tab, ThreadId, UserId, Thread);
        _ ->
            listed_member(UserId, Thread)
    end.

-spec stored_or_listed_member(guild_thread_store:tab(), integer(), integer(), map()) ->
    boolean().
stored_or_listed_member(Tab, ThreadId, UserId, Thread) ->
    case guild_thread_store:get_thread(Tab, ThreadId) of
        undefined -> listed_member(UserId, Thread);
        _ -> guild_thread_store:is_member(Tab, ThreadId, UserId)
    end.

-spec listed_member(integer(), map()) -> boolean().
listed_member(UserId, Thread) ->
    lists:member(
        UserId, snowflake_id:parse_list(maps:get(<<"_fluxer_member_ids">>, Thread, []))
    ).

-spec timed_out(map() | undefined) -> boolean().
timed_out(#{<<"communication_disabled_until">> := Until}) when is_binary(Until) ->
    try calendar:rfc3339_to_system_time(binary_to_list(Until), [{unit, millisecond}]) of
        UntilMs -> UntilMs > erlang:system_time(millisecond)
    catch
        _:_ -> false
    end;
timed_out(_) ->
    false.

-spec note_unknown_parent() -> ok.
note_unknown_parent() ->
    guild_thread_store:add_counter(?UNKNOWN_PARENT_KEY, 1, 1).

-spec init_counters() -> ok.
init_counters() ->
    guild_thread_store:ensure_counter(?UNKNOWN_PARENT_KEY, 1).

-spec unknown_parent_count() -> non_neg_integer().
unknown_parent_count() ->
    case persistent_term:get(?UNKNOWN_PARENT_KEY, undefined) of
        undefined -> 0;
        Counter -> counters:get(Counter, 1)
    end.
