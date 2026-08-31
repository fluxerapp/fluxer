%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_dispatch_decorate).
-typing([eqwalizer]).

-export([
    decorate_member_data/3,
    extract_member_for_event/3,
    extract_and_remove_session_id/1,
    parse_snowflake/2,
    require_snowflake/2,
    parse_snowflake_binary/2
]).

-type event() :: atom().
-type event_data() :: map().
-type guild_state() :: map().
-type session_id() :: binary().
-type user_id() :: integer().
-export_type([event/0, event_data/0, guild_state/0, session_id/0]).

-spec extract_and_remove_session_id(event_data()) -> {session_id() | undefined, event_data()}.
extract_and_remove_session_id(Data) ->
    case maps:get(<<"session_id">>, Data, undefined) of
        undefined -> {undefined, Data};
        SessionId -> {SessionId, maps:remove(<<"session_id">>, Data)}
    end.

-spec decorate_member_data(event(), event_data(), guild_state()) -> event_data().
decorate_member_data(Event, Data, State) ->
    case extract_member_for_event(Event, Data, State) of
        undefined -> Data;
        MemberData -> add_member_to_data(Event, Data, MemberData)
    end.

-spec add_member_to_data(event(), event_data(), map()) -> event_data().
add_member_to_data(Event, Data, MemberData) ->
    case member_data_placement(Event) of
        message ->
            CleanMemberData = maps:remove(<<"user">>, MemberData),
            Data#{<<"member">> => CleanMemberData};
        user ->
            Data#{<<"member">> => MemberData};
        none ->
            Data
    end.

-spec member_data_placement(event()) -> message | user | none.
member_data_placement(Event) ->
    case {is_message_event(Event), is_user_event(Event)} of
        {true, _} -> message;
        {false, true} -> user;
        {false, false} -> none
    end.

-spec is_message_event(event()) -> boolean().
is_message_event(message_create) -> true;
is_message_event(message_update) -> true;
is_message_event(message_delete) -> true;
is_message_event(message_delete_bulk) -> true;
is_message_event(_) -> false.

-spec is_user_event(event()) -> boolean().
is_user_event(typing_start) -> true;
is_user_event(message_reaction_add) -> true;
is_user_event(message_reaction_remove) -> true;
is_user_event(message_poll_vote_add) -> true;
is_user_event(message_poll_vote_remove) -> true;
is_user_event(_) -> false.

-spec extract_member_for_event(event(), event_data(), guild_state()) -> map() | undefined.
extract_member_for_event(Event, Data, State) ->
    UserId = extract_user_id_for_event(Event, Data),
    case UserId of
        undefined -> undefined;
        Id -> guild_permissions:find_member_by_user_id(Id, State)
    end.

-spec extract_user_id_for_event(event(), event_data()) -> user_id() | undefined.
extract_user_id_for_event(Event, Data) ->
    case member_data_placement(Event) of
        message -> extract_author_id(Data);
        user -> extract_user_id_field(Data);
        none -> undefined
    end.

-spec extract_author_id(event_data()) -> user_id() | undefined.
extract_author_id(Data) ->
    AuthorId = maps:get(<<"id">>, maps:get(<<"author">>, Data, #{}), undefined),
    RawAuthorId =
        case AuthorId of
            undefined -> maps:get(<<"author_id">>, Data, undefined);
            _ -> AuthorId
        end,
    validate_snowflake_or_undefined(<<"author_id">>, RawAuthorId).

-spec extract_user_id_field(event_data()) -> user_id() | undefined.
extract_user_id_field(Data) ->
    UserId = maps:get(<<"user_id">>, Data, undefined),
    validate_snowflake_or_undefined(<<"user_id">>, UserId).

-spec validate_snowflake_or_undefined(binary(), term()) -> user_id() | undefined.
validate_snowflake_or_undefined(_FieldName, undefined) ->
    undefined;
validate_snowflake_or_undefined(FieldName, Value) ->
    parse_snowflake(FieldName, Value).

-spec parse_snowflake(binary(), term()) -> integer() | undefined.
parse_snowflake(FieldName, Value) ->
    case validation:validate_snowflake(FieldName, Value) of
        {ok, Id} -> Id;
        {error, _, _} -> undefined
    end.

-spec require_snowflake(binary(), term()) -> integer().
require_snowflake(FieldName, Value) ->
    validation:snowflake_or_throw(FieldName, Value).

-spec parse_snowflake_binary(binary(), term()) -> binary().
parse_snowflake_binary(FieldName, Value) ->
    integer_to_binary(require_snowflake(FieldName, Value)).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

extract_and_remove_session_id_present_test() ->
    Data = #{<<"session_id">> => <<"abc123">>, <<"other">> => <<"value">>},
    {SessionId, CleanData} = extract_and_remove_session_id(Data),
    ?assertEqual(<<"abc123">>, SessionId),
    ?assertEqual(#{<<"other">> => <<"value">>}, CleanData).

extract_and_remove_session_id_absent_test() ->
    Data = #{<<"other">> => <<"value">>},
    {SessionId, CleanData} = extract_and_remove_session_id(Data),
    ?assertEqual(undefined, SessionId),
    ?assertEqual(Data, CleanData).

is_message_event_test() ->
    ?assertEqual(true, is_message_event(message_create)),
    ?assertEqual(true, is_message_event(message_update)),
    ?assertEqual(true, is_message_event(message_delete)),
    ?assertEqual(true, is_message_event(message_delete_bulk)),
    ?assertEqual(false, is_message_event(typing_start)).

is_user_event_test() ->
    ?assertEqual(true, is_user_event(typing_start)),
    ?assertEqual(true, is_user_event(message_reaction_add)),
    ?assertEqual(true, is_user_event(message_reaction_remove)),
    ?assertEqual(true, is_user_event(message_poll_vote_add)),
    ?assertEqual(true, is_user_event(message_poll_vote_remove)),
    ?assertEqual(false, is_user_event(message_reaction_remove_all)),
    ?assertEqual(false, is_user_event(message_reaction_remove_emoji)),
    ?assertEqual(false, is_user_event(message_create)).

decorate_member_data_message_delete_author_id_test() ->
    Member = #{<<"user">> => #{<<"id">> => <<"123">>}, <<"roles">> => []},
    State = #{data => #{<<"members">> => [Member]}},
    Data = #{<<"author_id">> => <<"123">>},
    Decorated = decorate_member_data(message_delete, Data, State),
    ?assert(maps:is_key(<<"member">>, Decorated)),
    ?assertEqual(false, maps:is_key(<<"user">>, maps:get(<<"member">>, Decorated))).

decorate_member_data_typing_start_test() ->
    Member = #{<<"user">> => #{<<"id">> => <<"456">>}, <<"roles">> => []},
    State = #{data => #{<<"members">> => [Member]}},
    Data = #{<<"user_id">> => <<"456">>},
    Decorated = decorate_member_data(typing_start, Data, State),
    ?assert(maps:is_key(<<"member">>, Decorated)),
    ?assert(maps:is_key(<<"user">>, maps:get(<<"member">>, Decorated))).

decorate_member_data_guild_event_no_decoration_test() ->
    State = #{data => #{<<"members">> => []}},
    Data = #{<<"name">> => <<"test">>},
    Decorated = decorate_member_data(guild_update, Data, State),
    ?assertEqual(false, maps:is_key(<<"member">>, Decorated)).

-endif.
