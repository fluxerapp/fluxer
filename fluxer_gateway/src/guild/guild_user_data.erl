%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_user_data).
-typing([eqwalizer]).

-export([
    maybe_update_cached_user_data/3,
    handle_user_data_update/3,
    check_user_data_differs/2
]).

-export_type([guild_state/0, user_id/0]).

-type guild_state() :: map().
-type user_id() :: integer().
-type member() :: map().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec update_members_user_data(map(), user_id(), map()) -> map().
update_members_user_data(Members, UserId, UserData) ->
    maps:map(
        fun(_MemberUserId, Member) ->
            maybe_update_member_user(Member, UserId, UserData)
        end,
        Members
    ).

-spec maybe_update_member_user(member(), user_id(), map()) -> member().
maybe_update_member_user(Member, UserId, EventData) ->
    MUser = maps:get(<<"user">>, Member, #{}),
    MemberId =
        case is_map(MUser) of
            true -> snowflake_id:parse_optional(maps:get(<<"id">>, MUser, undefined));
            false -> undefined
        end,
    case MemberId =:= UserId of
        true -> Member#{<<"user">> => EventData};
        false -> Member
    end.

-spec handle_user_data_update(user_id(), map(), guild_state()) -> guild_state().
handle_user_data_update(UserId, UserData, State) ->
    case guild_permissions:find_member_by_user_id(UserId, State) of
        undefined ->
            State;
        Member ->
            maybe_apply_changed_user_data(UserId, UserData, Member, State)
    end.

-spec maybe_apply_changed_user_data(user_id(), map(), member(), guild_state()) -> guild_state().
maybe_apply_changed_user_data(UserId, UserData, Member, State) ->
    CurrentUserData = maps:get(<<"user">>, Member, #{}),
    case check_user_data_differs(CurrentUserData, UserData) of
        false -> State;
        true -> apply_user_data_update(UserId, UserData, State)
    end.

-spec apply_user_data_update(user_id(), map(), guild_state()) -> guild_state().
apply_user_data_update(UserId, UserData, State) ->
    Data = maps:get(data, State),
    Members = guild_data_index:member_map(Data),
    NormalizedUserData = ensure_map(guild_data_normalize:member(#{<<"user">> => UserData})),
    #{<<"user">> := UpdatedUserData} = NormalizedUserData,
    UpdatedMembers = update_members_user_data(Members, UserId, ensure_map(UpdatedUserData)),
    UpdatedData = guild_data_index:put_member_map(UpdatedMembers, Data),
    UpdatedState = State#{data => UpdatedData},
    dispatch_guild_member_update(UserId, UpdatedState),
    UpdatedState.

-spec dispatch_guild_member_update(user_id(), guild_state()) -> ok.
dispatch_guild_member_update(UserId, State) ->
    case guild_permissions:find_member_by_user_id(UserId, State) of
        undefined ->
            ok;
        M ->
            GuildId = maps:get(id, State),
            MemberUpdateData = M#{<<"guild_id">> => integer_to_binary(GuildId)},
            gen_server:cast(
                self(),
                {dispatch, #{event => guild_member_update, data => MemberUpdateData}}
            )
    end.

-spec check_user_data_differs(map(), map()) -> boolean().
check_user_data_differs(CurrentUserData, NewUserData) ->
    utils:check_user_data_differs(CurrentUserData, NewUserData).

-spec maybe_update_cached_user_data(atom(), map(), guild_state()) -> guild_state().
maybe_update_cached_user_data(Event, EventData, State) when
    Event =:= message_create; Event =:= message_update
->
    case maps:get(<<"author">>, EventData, undefined) of
        undefined ->
            State;
        AuthorData ->
            maybe_update_author_data(AuthorData, State)
    end;
maybe_update_cached_user_data(_, _, State) ->
    State.

-spec maybe_update_author_data(map(), guild_state()) -> guild_state().
maybe_update_author_data(AuthorData, State) ->
    case snowflake_id:parse_optional(maps:get(<<"id">>, AuthorData, undefined)) of
        undefined ->
            State;
        UserId ->
            maybe_update_known_member(UserId, AuthorData, State)
    end.

-spec maybe_update_known_member(user_id(), map(), guild_state()) -> guild_state().
maybe_update_known_member(UserId, AuthorData, State) ->
    case guild_permissions:find_member_by_user_id(UserId, State) of
        undefined ->
            State;
        Member ->
            maybe_handle_changed_author_data(UserId, AuthorData, Member, State)
    end.

-spec maybe_handle_changed_author_data(user_id(), map(), member(), guild_state()) ->
    guild_state().
maybe_handle_changed_author_data(UserId, AuthorData, Member, State) ->
    CurrentUserData = maps:get(<<"user">>, Member, #{}),
    case check_user_data_differs(CurrentUserData, AuthorData) of
        true -> handle_user_data_update(UserId, AuthorData, State);
        false -> State
    end.

-spec ensure_map(term()) -> map().
ensure_map(M) when is_map(M) -> M;
ensure_map(_) -> #{}.

-ifdef(TEST).

handle_user_data_update_no_change_test() ->
    State = test_state(),
    UserData = #{<<"id">> => <<"100">>, <<"username">> => <<"alice">>},
    NewState = handle_user_data_update(100, UserData, State),
    ?assertEqual(State, NewState).

message_create_equivalent_author_data_does_not_dispatch_member_update_test() ->
    drain_test_mailbox(),
    State = normalized_test_state(),
    Author = #{
        <<"id">> => <<"100">>,
        <<"username">> => <<"alice">>,
        <<"discriminator">> => <<"0001">>,
        <<"global_name">> => null,
        <<"avatar">> => null,
        <<"avatar_color">> => null,
        <<"flags">> => 0
    },
    NewState = maybe_update_cached_user_data(message_create, #{<<"author">> => Author}, State),
    ?assertEqual(State, NewState),
    assert_no_member_update_dispatch().

check_user_data_differs_test() ->
    Current = #{<<"username">> => <<"alice">>},
    Same = #{<<"username">> => <<"alice">>},
    Different = #{<<"username">> => <<"bob">>},
    ?assertEqual(false, check_user_data_differs(Current, Same)),
    ?assertEqual(true, check_user_data_differs(Current, Different)).

test_state() ->
    #{
        id => 42,
        data => #{
            <<"members">> => #{
                100 => #{<<"user">> => #{<<"id">> => <<"100">>, <<"username">> => <<"alice">>}}
            }
        }
    }.

normalized_test_state() ->
    #{
        id => 42,
        data => guild_data_index:put_member(
            #{
                <<"user">> => #{
                    <<"id">> => <<"100">>,
                    <<"username">> => <<"alice">>,
                    <<"discriminator">> => <<"0001">>,
                    <<"global_name">> => null,
                    <<"avatar">> => null,
                    <<"avatar_color">> => null,
                    <<"flags">> => 0
                },
                <<"roles">> => []
            },
            #{}
        )
    }.

drain_test_mailbox() ->
    receive
        _ -> drain_test_mailbox()
    after 0 ->
        ok
    end.

assert_no_member_update_dispatch() ->
    receive
        {'$gen_cast', {dispatch, #{event := guild_member_update}}} ->
            ?assert(false)
    after 50 ->
        ok
    end.

-endif.
