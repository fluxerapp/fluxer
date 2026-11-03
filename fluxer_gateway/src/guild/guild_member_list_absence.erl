%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_absence).
-typing([eqwalizer]).

-export([of_list/3, sync_body_unchanged/4]).
-export_type([absence/0, guild_state/0, list_id/0, user_id/0]).

-type guild_state() :: map().
-type list_id() :: binary().
-type user_id() :: integer().
-type engine_ref() :: ets:table().
-type absence() :: {absent, engine_ref()} | present.

-spec sync_body_unchanged(user_id(), list_id(), absence(), guild_state()) -> boolean().
sync_body_unchanged(_UserId, _ListId, present, _State) ->
    false;
sync_body_unchanged(UserId, ListId, {absent, Ref}, State) ->
    list_store_ref(ListId, State) =:= Ref andalso member_row_absent(UserId, Ref).

-spec of_list(user_id(), list_id(), guild_state()) -> absence().
of_list(UserId, ListId, State) ->
    absence_for_store(UserId, list_store_ref(ListId, State)).

-spec absence_for_store(user_id(), engine_ref() | undefined) -> absence().
absence_for_store(_UserId, undefined) ->
    present;
absence_for_store(UserId, Ref) ->
    case member_row_absent(UserId, Ref) of
        true -> {absent, Ref};
        false -> present
    end.

-spec member_row_absent(user_id(), engine_ref()) -> boolean().
member_row_absent(UserId, Ref) when is_integer(UserId), UserId > 0 ->
    try guild_member_list_engine:is_member_online(Ref, UserId) of
        not_present -> true;
        _Online -> false
    catch
        _:_ -> false
    end;
member_row_absent(_UserId, _Ref) ->
    false.

-spec list_store_ref(list_id(), guild_state()) -> engine_ref() | undefined.
list_store_ref(<<"0">>, State) ->
    case maps:get(member_list_engine, State, undefined) of
        undefined -> undefined;
        Ref when is_reference(Ref); is_atom(Ref) -> Ref;
        _Other -> undefined
    end;
list_store_ref(ListId, State) ->
    guild_member_list_channel_engine:ref(ListId, State).
