%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_user_data).

-typing([eqwalizer]).

-export([maybe_update_cached_user_data/3, handle_user_data_update/3]).

-export_type([guild_state/0, user_id/0]).

-type guild_state() :: map().
-type user_id() :: integer().
-type member() :: map().

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
    case guild_permissions_check:find_member_by_user_id(UserId, State) of
        undefined ->
            State;
        Member ->
            maybe_apply_changed_user_data(UserId, UserData, Member, State)
    end.

-spec maybe_apply_changed_user_data(user_id(), map(), member(), guild_state()) -> guild_state().
maybe_apply_changed_user_data(UserId, UserData, Member, State) ->
    CurrentUserData = maps:get(<<"user">>, Member, #{}),
    case utils:check_user_data_differs(CurrentUserData, UserData) of
        false -> State;
        true -> apply_user_data_update(UserId, UserData, State)
    end.

-spec apply_user_data_update(user_id(), map(), guild_state()) -> guild_state().
apply_user_data_update(UserId, UserData, State) ->
    Data = maps:get(data, State),
    Members = guild_data_index_members:member_map(Data),
    NormalizedUserData = map_utils:ensure_map(
        guild_data_normalize:member(#{<<"user">> => UserData})
    ),
    #{<<"user">> := UpdatedUserData} = NormalizedUserData,
    UpdatedMembers = update_members_user_data(
        Members, UserId, map_utils:ensure_map(UpdatedUserData)
    ),
    UpdatedData = guild_data_index_members:put_member_map(UpdatedMembers, Data),
    UpdatedState = State#{data => UpdatedData},
    dispatch_guild_member_update(UserId, UpdatedState),
    UpdatedState.

-spec dispatch_guild_member_update(user_id(), guild_state()) -> ok.
dispatch_guild_member_update(UserId, State) ->
    case guild_permissions_check:find_member_by_user_id(UserId, State) of
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
    case snowflake_id:parse_maybe(maps:get(<<"id">>, AuthorData, undefined)) of
        undefined ->
            State;
        UserId ->
            maybe_update_known_member(UserId, AuthorData, State)
    end.

-spec maybe_update_known_member(user_id(), map(), guild_state()) -> guild_state().
maybe_update_known_member(UserId, AuthorData, State) ->
    case guild_permissions_check:find_member_by_user_id(UserId, State) of
        undefined ->
            State;
        Member ->
            maybe_handle_changed_author_data(UserId, AuthorData, Member, State)
    end.

-spec maybe_handle_changed_author_data(user_id(), map(), member(), guild_state()) ->
    guild_state().
maybe_handle_changed_author_data(UserId, AuthorData, Member, State) ->
    CurrentUserData = maps:get(<<"user">>, Member, #{}),
    case utils:check_user_data_differs(CurrentUserData, AuthorData) of
        true -> handle_user_data_update(UserId, AuthorData, State);
        false -> State
    end.
