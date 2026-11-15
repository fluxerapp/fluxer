%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_push_scan).
-typing([eqwalizer]).

-export([context/3, members/3]).

-type user_id() :: integer().
-type guild_state() :: map().

-spec context(map(), integer(), map()) -> map().
context(MessageData, ChannelId, State) ->
    MentionRoles = maps:get(<<"mention_roles">>, MessageData, []),
    #{
        mention_everyone => type_conv:is_true(
            maps:get(<<"mention_everyone">>, MessageData, false)
        ),
        direct_mentions => guild_push_candidates:direct_mention_ids(MessageData),
        mention_roles => guild_push_candidates:mention_role_ids(MentionRoles),
        author_id => guild_push_candidates:author_id(MessageData),
        format_members => guild_push_candidates:format_member_ids(MessageData),
        channel_id => ChannelId,
        thread_recipient => guild_thread_gate:recipient_active(ChannelId, State),
        thread_scope => guild_thread_push:scope(ChannelId, State),
        session_eligibility => maps:get(session_eligibility, State),
        grace_sessions => guild_push_engagement:held_sessions(maps:get(grace_hold, State, none))
    }.

-spec members(ets:tid(), map(), map()) ->
    {ok, map()} | {error, table_unavailable}.
members(MembersTab, Context, State) ->
    try
        Context1 = with_thread_role_adds(MembersTab, Context, State),
        {ok,
            fold_push_members(scan_user_ids(MembersTab, Context1), MembersTab, Context1, State)}
    catch
        error:badarg -> {error, table_unavailable}
    end.

-spec initial_scan_acc() -> map().
initial_scan_acc() ->
    #{
        eligible_user_ids => [],
        held_user_ids => [],
        user_roles => #{},
        format_members => #{}
    }.

-spec scan_user_ids(ets:tid(), map()) -> [user_id()].
scan_user_ids(_MembersTab, #{thread_scope := #{members := Members}} = Context) ->
    lists:usort(
        maps:keys(Members) ++
            maps:keys(maps:get(direct_mentions, Context)) ++
            maps:keys(maps:get(format_members, Context)) ++
            maps:keys(maps:get(role_adds, Context))
    );
scan_user_ids(MembersTab, _Context) ->
    member_id_snapshot(MembersTab).

-spec member_id_snapshot(ets:tid()) -> [user_id()].
member_id_snapshot(MembersTab) ->
    ets:select(MembersTab, [{{'$1', '_'}, [], ['$1']}]).

-spec fold_push_members([user_id()], ets:tid(), map(), guild_state()) -> map().
fold_push_members(UserIds, MembersTab, Context, State) ->
    lists:foldl(
        fun(UserId, Acc) -> scan_push_member_id(UserId, MembersTab, Context, State, Acc) end,
        initial_scan_acc(),
        UserIds
    ).

-spec scan_push_member_id(user_id(), ets:tid(), map(), guild_state(), map()) -> map().
scan_push_member_id(UserId, MembersTab, Context, State, Acc) ->
    case ets:lookup(MembersTab, UserId) of
        [Row] -> scan_push_member(Row, Context, State, Acc);
        _ -> Acc
    end.

-spec scan_push_member(term(), map(), guild_state(), map()) -> map().
scan_push_member({UserId, Member}, Context, State, Acc) when
    is_integer(UserId), UserId > 0, is_map(Member)
->
    Acc1 = maybe_collect_format_member(UserId, Member, Context, Acc),
    case scanned_member_route(UserId, Member, Context, State) of
        skip -> Acc1;
        Key -> add_scanned_member(Key, UserId, Member, Acc1)
    end;
scan_push_member(_Row, _Context, _State, Acc) ->
    Acc.

-spec scanned_member_route(user_id(), map(), map(), guild_state()) ->
    eligible_user_ids | held_user_ids | skip.
scanned_member_route(UserId, Member, Context, State) ->
    Route =
        case
            guild_thread_push:is_candidate(
                UserId, Member, maps:get(thread_scope, Context, undefined), Context
            )
        of
            true -> member_push_route(UserId, Member, Context);
            false -> skip
        end,
    member_route(Route, UserId, Member, Context, State).

-spec with_thread_role_adds(ets:tid(), map(), guild_state()) -> map().
with_thread_role_adds(MembersTab, #{thread_scope := Scope} = Context, State) when
    Scope =/= undefined
->
    Context#{
        role_adds => guild_thread_push:role_adds(
            Scope,
            Context,
            {ets, MembersTab},
            guild_thread_push:role_visible(maps:get(channel_id, Context), State)
        )
    };
with_thread_role_adds(_MembersTab, Context, _State) ->
    Context.

-spec member_route(
    eligible_user_ids | held_user_ids | skip, user_id(), map(), map(), guild_state()
) -> eligible_user_ids | held_user_ids | skip.
member_route(skip, _UserId, _Member, _Context, _State) ->
    skip;
member_route(Key, UserId, Member, Context, State) ->
    ThreadRecipient = maps:get(thread_recipient, Context, fun all_recipients/1),
    case
        guild_permissions_check:can_view_channel(
            UserId, maps:get(channel_id, Context), Member, State
        ) andalso ThreadRecipient(UserId)
    of
        true -> Key;
        false -> skip
    end.

-spec all_recipients(user_id()) -> true.
all_recipients(_UserId) ->
    true.

-spec member_push_route(user_id(), map(), map()) ->
    eligible_user_ids | held_user_ids | skip.
member_push_route(UserId, Member, Context) ->
    case guild_push_candidates:is_candidate(UserId, Member, Context) of
        true ->
            eligible_user_ids;
        false ->
            guild_push_engagement:grace_route(UserId, maps:get(grace_sessions, Context, #{}))
    end.

-spec add_scanned_member(eligible_user_ids | held_user_ids, user_id(), map(), map()) -> map().
add_scanned_member(Key, UserId, Member, Acc) ->
    UserIds = maps:get(Key, Acc),
    UserRoles = maps:get(user_roles, Acc),
    Acc#{
        Key := [UserId | UserIds],
        user_roles := UserRoles#{UserId => guild_push_message:role_ids(Member)}
    }.

-spec maybe_collect_format_member(user_id(), map(), map(), map()) -> map().
maybe_collect_format_member(UserId, Member, Context, Acc) ->
    case maps:is_key(UserId, maps:get(format_members, Context)) of
        true ->
            Members = maps:get(format_members, Acc),
            Acc#{format_members := Members#{UserId => Member}};
        false ->
            Acc
    end.
