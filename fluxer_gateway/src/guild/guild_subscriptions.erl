%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(guild_subscriptions).

-export([
    init_state/0,
    subscribe/3,
    unsubscribe/3,
    unsubscribe_session/2,
    update_subscriptions/3,
    get_subscribed_sessions/2,
    is_subscribed/3,
    get_user_ids_for_session/2
]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type session_id() :: binary().
-type user_id() :: integer().
-type subscription_state() :: #{user_id() => sets:set(session_id())}.

-spec init_state() -> subscription_state().
init_state() -> #{}.

-spec subscribe(session_id(), user_id(), subscription_state()) -> subscription_state().
subscribe(SessionId, UserId, State) ->
    Subscribers = maps:get(UserId, State, sets:new()),
    NewSubscribers = sets:add_element(SessionId, Subscribers),
    maps:put(UserId, NewSubscribers, State).

-spec unsubscribe(session_id(), user_id(), subscription_state()) -> subscription_state().
unsubscribe(SessionId, UserId, State) ->
    case maps:get(UserId, State, undefined) of
        undefined ->
            State;
        Subscribers ->
            NewSubscribers = sets:del_element(SessionId, Subscribers),
            case sets:size(NewSubscribers) of
                0 -> maps:remove(UserId, State);
                _ -> maps:put(UserId, NewSubscribers, State)
            end
    end.

-spec unsubscribe_session(session_id(), subscription_state()) -> subscription_state().
unsubscribe_session(SessionId, State) ->
    maps:fold(
        fun(UserId, Subscribers, Acc) ->
            NewSubscribers = sets:del_element(SessionId, Subscribers),
            case sets:size(NewSubscribers) of
                0 -> Acc;
                _ -> maps:put(UserId, NewSubscribers, Acc)
            end
        end,
        #{},
        State
    ).

-spec update_subscriptions(session_id(), [user_id()], subscription_state()) ->
    subscription_state().
update_subscriptions(SessionId, NewMemberIds, State) ->
    CurrentlySubscribed = get_user_ids_for_session(SessionId, State),
    NewMemberIdSet = sets:from_list(NewMemberIds),

    ToRemove = sets:subtract(CurrentlySubscribed, NewMemberIdSet),
    ToAdd = sets:subtract(NewMemberIdSet, CurrentlySubscribed),

    State1 = sets:fold(
        fun(UserId, AccState) ->
            unsubscribe(SessionId, UserId, AccState)
        end,
        State,
        ToRemove
    ),

    sets:fold(
        fun(UserId, AccState) ->
            subscribe(SessionId, UserId, AccState)
        end,
        State1,
        ToAdd
    ).

-spec get_subscribed_sessions(user_id(), subscription_state()) -> [session_id()].
get_subscribed_sessions(UserId, State) ->
    case maps:get(UserId, State, undefined) of
        undefined -> [];
        Subscribers -> sets:to_list(Subscribers)
    end.

-spec is_subscribed(session_id(), user_id(), subscription_state()) -> boolean().
is_subscribed(SessionId, UserId, State) ->
    case maps:get(UserId, State, undefined) of
        undefined -> false;
        Subscribers -> sets:is_element(SessionId, Subscribers)
    end.

-spec get_user_ids_for_session(session_id(), subscription_state()) -> sets:set(user_id()).
get_user_ids_for_session(SessionId, State) ->
    maps:fold(
        fun(UserId, Subscribers, Acc) ->
            case sets:is_element(SessionId, Subscribers) of
                true -> sets:add_element(UserId, Acc);
                false -> Acc
            end
        end,
        sets:new(),
        State
    ).

-ifdef(TEST).

init_state_test() ->
    ?assertEqual(#{}, init_state()).

subscribe_test() ->
    State0 = init_state(),
    State1 = subscribe(<<"session1">>, 123, State0),
    ?assert(is_subscribed(<<"session1">>, 123, State1)),
    ?assertNot(is_subscribed(<<"session2">>, 123, State1)).

subscribe_multiple_sessions_test() ->
    State0 = init_state(),
    State1 = subscribe(<<"session1">>, 123, State0),
    State2 = subscribe(<<"session2">>, 123, State1),
    ?assert(is_subscribed(<<"session1">>, 123, State2)),
    ?assert(is_subscribed(<<"session2">>, 123, State2)).

unsubscribe_test() ->
    State0 = init_state(),
    State1 = subscribe(<<"session1">>, 123, State0),
    State2 = unsubscribe(<<"session1">>, 123, State1),
    ?assertNot(is_subscribed(<<"session1">>, 123, State2)).

unsubscribe_one_of_many_test() ->
    State0 = init_state(),
    State1 = subscribe(<<"session1">>, 123, State0),
    State2 = subscribe(<<"session2">>, 123, State1),
    State3 = unsubscribe(<<"session1">>, 123, State2),
    ?assertNot(is_subscribed(<<"session1">>, 123, State3)),
    ?assert(is_subscribed(<<"session2">>, 123, State3)).

unsubscribe_session_test() ->
    State0 = init_state(),
    State1 = subscribe(<<"session1">>, 123, State0),
    State2 = subscribe(<<"session1">>, 456, State1),
    State3 = subscribe(<<"session2">>, 123, State2),
    State4 = unsubscribe_session(<<"session1">>, State3),
    ?assertNot(is_subscribed(<<"session1">>, 123, State4)),
    ?assertNot(is_subscribed(<<"session1">>, 456, State4)),
    ?assert(is_subscribed(<<"session2">>, 123, State4)).

get_subscribed_sessions_test() ->
    State0 = init_state(),
    State1 = subscribe(<<"session1">>, 123, State0),
    State2 = subscribe(<<"session2">>, 123, State1),
    Sessions = lists:sort(get_subscribed_sessions(123, State2)),
    ?assertEqual([<<"session1">>, <<"session2">>], Sessions).

update_subscriptions_test() ->
    State0 = init_state(),
    State1 = subscribe(<<"session1">>, 100, State0),
    State2 = subscribe(<<"session1">>, 200, State1),
    State3 = update_subscriptions(<<"session1">>, [200, 300], State2),
    ?assertNot(is_subscribed(<<"session1">>, 100, State3)),
    ?assert(is_subscribed(<<"session1">>, 200, State3)),
    ?assert(is_subscribed(<<"session1">>, 300, State3)).

-endif.
