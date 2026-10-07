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

-spec resolve(integer(), map(), map() | undefined, guild_state()) -> permission().
resolve(UserId, Thread, Member, State) ->
    case parent_channel(Thread, State) of
        undefined ->
            note_unknown_parent(),
            0;
        ParentId ->
            ParentPerms = guild_permissions:compute_member_permissions(
                UserId, ParentId, Member, State
            ),
            Perms = alias_permissions(ParentPerms),
            case can_view_thread_from_parent(UserId, Thread, Perms, Member, State) of
                true -> Perms;
                false -> Perms band bnot constants:view_channel_permission()
            end
    end.

-spec alias_permissions(permission()) -> permission().
alias_permissions(?ALL_PERMISSIONS) ->
    ?ALL_PERMISSIONS;
alias_permissions(Perms) ->
    case permission_bits:has(Perms, constants:send_messages_in_threads_permission()) of
        true -> Perms bor ?SEND_MESSAGES;
        false -> Perms band bnot ?SEND_MESSAGES
    end.

-spec is_moderator(permission(), map() | undefined) -> boolean().
is_moderator(?ALL_PERMISSIONS, _Member) ->
    true;
is_moderator(Perms, Member) ->
    permission_bits:has(Perms, constants:administrator_permission()) orelse
        (permission_bits:has(Perms, constants:manage_threads_permission()) andalso
            not timed_out(Member)).

-spec can_view_thread_from_parent(
    integer(), map(), permission(), map() | undefined, guild_state()
) ->
    boolean().
can_view_thread_from_parent(UserId, Thread, Perms, Member, State) ->
    permission_bits:has(Perms, constants:view_channel_permission()) andalso
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
            case guild_permissions:find_channel_by_id(ParentId, State) of
                undefined -> undefined;
                _Channel -> ParentId
            end;
        _ ->
            undefined
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
            case guild_thread_store:get_thread(Tab, ThreadId) of
                undefined -> listed_member(UserId, Thread);
                _ -> guild_thread_store:is_member(Tab, ThreadId, UserId)
            end;
        _ ->
            listed_member(UserId, Thread)
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

alias_maps_send_messages_to_threads_bit_test() ->
    InThreads = constants:send_messages_in_threads_permission(),
    ?assertEqual(InThreads bor ?SEND_MESSAGES, alias_permissions(InThreads)),
    ?assertEqual(1024, alias_permissions(1024 bor ?SEND_MESSAGES)),
    ?assertEqual(?ALL_PERMISSIONS, alias_permissions(?ALL_PERMISSIONS)).

moderator_rules_test() ->
    Manage = constants:manage_threads_permission(),
    ?assert(is_moderator(Manage, #{})),
    ?assert(is_moderator(constants:administrator_permission(), #{})),
    ?assertNot(is_moderator(1024, #{})),
    ?assertNot(
        is_moderator(Manage, #{<<"communication_disabled_until">> => <<"2999-01-01T00:00:00Z">>})
    ),
    ?assert(
        is_moderator(Manage, #{<<"communication_disabled_until">> => <<"2000-01-01T00:00:00Z">>})
    ).

shared_case_table_test() ->
    {ok, Contents} = file:read_file("test/thread_permission_cases.json"),
    Cases = [
        Case
     || #{<<"fn">> := Fn} = Case <- json:decode(Contents),
        Fn =:= <<"isThreadModerator">> orelse Fn =:= <<"canViewThread">>
    ],
    ?assertEqual(16, length(Cases)),
    lists:foreach(fun run_case/1, Cases).

run_case(
    #{<<"name">> := Name, <<"fn">> := Fn, <<"actor">> := Actor, <<"expect">> := Expect} = Case
) ->
    Perms = case_perms(Actor),
    Member = case_member(Actor),
    Thread = case_thread(Actor, maps:get(<<"thread">>, Case, #{})),
    Actual =
        case Fn of
            <<"isThreadModerator">> -> is_moderator(Perms, Member);
            <<"canViewThread">> -> can_view_thread_from_parent(1, Thread, Perms, Member, #{})
        end,
    ?assertEqual({Name, case_expect(Expect)}, {Name, Actual}).

case_perms(#{<<"isOwner">> := true}) ->
    ?ALL_PERMISSIONS;
case_perms(#{<<"perms">> := Names}) ->
    case lists:member(<<"ADMINISTRATOR">>, Names) of
        true -> ?ALL_PERMISSIONS;
        false -> lists:foldl(fun(N, Acc) -> Acc bor case_bit(N) end, 0, Names)
    end.

case_bit(<<"VIEW_CHANNEL">>) -> constants:view_channel_permission();
case_bit(<<"READ_MESSAGE_HISTORY">>) -> 65536;
case_bit(<<"MANAGE_THREADS">>) -> constants:manage_threads_permission();
case_bit(<<"SEND_MESSAGES_IN_THREADS">>) -> constants:send_messages_in_threads_permission().

case_member(#{<<"timedOut">> := true}) ->
    #{<<"communication_disabled_until">> => <<"2999-01-01T00:00:00Z">>};
case_member(_) ->
    #{}.

case_thread(Actor, Thread) ->
    MemberIds =
        case maps:get(<<"isMember">>, Actor, false) of
            true -> [<<"1">>];
            false -> []
        end,
    #{
        <<"id">> => <<"2">>,
        <<"type">> => maps:get(<<"type">>, Thread, 11),
        <<"_fluxer_member_ids">> => MemberIds
    }.

case_expect(Bool) when is_boolean(Bool) -> Bool;
case_expect(null) -> true;
case_expect(<<"MISSING_ACCESS">>) -> false.

-endif.
