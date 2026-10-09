%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_dispatch_filter).
-typing([eqwalizer]).

-export([
    filter_sessions_for_event/5,
    is_channel_scoped_event/1,
    is_invite_event/1,
    is_audit_log_event/1,
    is_message_access_filtered_event/1,
    is_bulk_update_event/1,
    extract_channel_id/2,
    extract_message_id/1,
    extract_invite_channel_id/1
]).

-type event() :: atom().
-type event_data() :: map().
-type guild_state() :: map().
-type session_id() :: binary().
-type channel_id() :: integer().
-type session_pair() :: {session_id(), map()}.
-export_type([event/0, event_data/0, guild_state/0, session_id/0, channel_id/0, session_pair/0]).

-spec filter_sessions_for_event(
    event(), event_data(), session_id() | undefined, map(), guild_state()
) ->
    [session_pair()].
filter_sessions_for_event(Event, FinalData, SessionIdOpt, Sessions, UpdatedState) ->
    case is_channel_scoped_event(Event) of
        true ->
            filter_channel_scoped(Event, FinalData, SessionIdOpt, Sessions, UpdatedState);
        false ->
            filter_non_channel_scoped(Event, FinalData, SessionIdOpt, Sessions, UpdatedState)
    end.

-spec filter_channel_scoped(
    event(), event_data(), session_id() | undefined, map(), guild_state()
) -> [session_pair()].
filter_channel_scoped(Event, FinalData, SessionIdOpt, Sessions, UpdatedState) ->
    ChannelId = extract_channel_id(Event, FinalData),
    case is_message_access_filtered_event(Event) of
        true ->
            MessageId = extract_message_id(FinalData),
            guild_sessions:filter_sessions_for_message(
                Sessions, ChannelId, MessageId, SessionIdOpt, UpdatedState
            );
        false ->
            guild_sessions:filter_sessions_for_channel(
                Sessions, ChannelId, SessionIdOpt, UpdatedState
            )
    end.

-spec filter_non_channel_scoped(
    event(), event_data(), session_id() | undefined, map(), guild_state()
) -> [session_pair()].
filter_non_channel_scoped(Event, FinalData, SessionIdOpt, Sessions, UpdatedState) ->
    case is_invite_event(Event) of
        true ->
            filter_invite_event(FinalData, SessionIdOpt, Sessions, UpdatedState);
        false ->
            filter_other_event(Event, SessionIdOpt, Sessions, UpdatedState)
    end.

-spec filter_invite_event(
    event_data(), session_id() | undefined, map(), guild_state()
) -> [session_pair()].
filter_invite_event(FinalData, SessionIdOpt, Sessions, UpdatedState) ->
    case extract_invite_channel_id(FinalData) of
        undefined ->
            [];
        ChannelId ->
            guild_sessions:filter_sessions_for_manage_channels(
                Sessions, ChannelId, SessionIdOpt, UpdatedState
            )
    end.

-spec filter_other_event(
    event(), session_id() | undefined, map(), guild_state()
) -> [session_pair()].
filter_other_event(Event, SessionIdOpt, Sessions, UpdatedState) ->
    case is_audit_log_event(Event) of
        true ->
            filter_sessions_for_view_audit_log(
                Sessions, SessionIdOpt, UpdatedState
            );
        false ->
            guild_sessions:filter_sessions_exclude_session(
                Sessions, SessionIdOpt
            )
    end.

-spec is_channel_scoped_event(event()) -> boolean().
is_channel_scoped_event(channel_create) -> true;
is_channel_scoped_event(channel_update) -> true;
is_channel_scoped_event(channel_delete) -> true;
is_channel_scoped_event(message_create) -> true;
is_channel_scoped_event(message_update) -> true;
is_channel_scoped_event(message_delete) -> true;
is_channel_scoped_event(message_delete_bulk) -> true;
is_channel_scoped_event(message_reaction_add) -> true;
is_channel_scoped_event(message_reaction_remove) -> true;
is_channel_scoped_event(message_reaction_remove_all) -> true;
is_channel_scoped_event(message_reaction_remove_emoji) -> true;
is_channel_scoped_event(message_poll_vote_add) -> true;
is_channel_scoped_event(message_poll_vote_remove) -> true;
is_channel_scoped_event(typing_start) -> true;
is_channel_scoped_event(channel_pins_update) -> true;
is_channel_scoped_event(webhooks_update) -> true;
is_channel_scoped_event(_) -> false.

-spec is_invite_event(event()) -> boolean().
is_invite_event(invite_create) -> true;
is_invite_event(invite_delete) -> true;
is_invite_event(_) -> false.

-spec extract_invite_channel_id(event_data()) -> channel_id() | undefined.
extract_invite_channel_id(FinalData) ->
    case maps:get(<<"channel_id">>, FinalData, undefined) of
        undefined ->
            extract_invite_channel_from_nested(FinalData);
        ChannelIdBin ->
            guild_dispatch_decorate:parse_snowflake(<<"channel_id">>, ChannelIdBin)
    end.

-spec extract_invite_channel_from_nested(event_data()) -> channel_id() | undefined.
extract_invite_channel_from_nested(FinalData) ->
    case maps:get(<<"channel">>, FinalData, undefined) of
        Channel when is_map(Channel) ->
            guild_dispatch_decorate:parse_snowflake(
                <<"channel.id">>, maps:get(<<"id">>, Channel, undefined)
            );
        _ ->
            undefined
    end.

-spec is_audit_log_event(event()) -> boolean().
is_audit_log_event(guild_audit_log_entry_create) -> true;
is_audit_log_event(_) -> false.

-spec filter_sessions_for_view_audit_log(map(), session_id() | undefined, guild_state()) ->
    [session_pair()].
filter_sessions_for_view_audit_log(Sessions, SessionIdOpt, State) ->
    maps:fold(
        fun(Sid, SessionData, Acc) ->
            collect_audit_log_session(Sid, SessionData, SessionIdOpt, State, Acc)
        end,
        [],
        Sessions
    ).

-spec collect_audit_log_session(
    session_id(), map(), session_id() | undefined, guild_state(), [session_pair()]
) -> [session_pair()].
collect_audit_log_session(Sid, SessionData, SessionIdOpt, State, Acc) ->
    case is_audit_log_eligible(Sid, SessionData, SessionIdOpt, State) of
        true -> [{Sid, SessionData} | Acc];
        false -> Acc
    end.

-spec is_audit_log_eligible(session_id(), map(), session_id() | undefined, guild_state()) ->
    boolean().
is_audit_log_eligible(_Sid, #{pending_connect := true}, _SessionIdOpt, _State) ->
    false;
is_audit_log_eligible(Sid, _SessionData, Sid, _State) ->
    false;
is_audit_log_eligible(_Sid, SessionData, _SessionIdOpt, State) ->
    has_view_audit_log_permission(SessionData, State).

-spec has_view_audit_log_permission(map(), guild_state()) -> boolean().
has_view_audit_log_permission(SessionData, State) ->
    case maps:get(user_id, SessionData, undefined) of
        UserId when is_integer(UserId) ->
            Permissions = guild_permissions:get_member_permissions(UserId, undefined, State),
            permission_bits:has(Permissions, constants:view_audit_log_permission());
        _ ->
            false
    end.

-spec is_message_access_filtered_event(event()) -> boolean().
is_message_access_filtered_event(message_update) -> true;
is_message_access_filtered_event(message_delete) -> true;
is_message_access_filtered_event(message_reaction_add) -> true;
is_message_access_filtered_event(message_reaction_remove) -> true;
is_message_access_filtered_event(message_reaction_remove_all) -> true;
is_message_access_filtered_event(message_reaction_remove_emoji) -> true;
is_message_access_filtered_event(message_poll_vote_add) -> true;
is_message_access_filtered_event(message_poll_vote_remove) -> true;
is_message_access_filtered_event(_) -> false.

-spec extract_message_id(event_data()) -> binary().
extract_message_id(EventData) ->
    RawMessageId =
        case maps:get(<<"message_id">>, EventData, undefined) of
            undefined -> maps:get(<<"id">>, EventData, undefined);
            MessageId -> MessageId
        end,
    guild_dispatch_decorate:parse_snowflake_binary(<<"message_id">>, RawMessageId).

-spec is_bulk_update_event(event()) -> boolean().
is_bulk_update_event(channel_update_bulk) -> true;
is_bulk_update_event(_) -> false.

-spec extract_channel_id(event(), event_data()) -> channel_id().
extract_channel_id(Event, FinalData) when
    Event =:= channel_create; Event =:= channel_update; Event =:= channel_delete
->
    ChannelIdBin = maps:get(<<"id">>, FinalData, undefined),
    guild_dispatch_decorate:require_snowflake(<<"id">>, ChannelIdBin);
extract_channel_id(_, FinalData) ->
    ChannelIdBin = maps:get(<<"channel_id">>, FinalData, undefined),
    guild_dispatch_decorate:require_snowflake(<<"channel_id">>, ChannelIdBin).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_channel_scoped_event_test() ->
    ?assertEqual(true, is_channel_scoped_event(message_create)),
    ?assertEqual(true, is_channel_scoped_event(channel_update)),
    ?assertEqual(true, is_channel_scoped_event(typing_start)),
    ?assertEqual(false, is_channel_scoped_event(guild_update)),
    ?assertEqual(false, is_channel_scoped_event(guild_member_add)).

is_invite_event_test() ->
    ?assertEqual(true, is_invite_event(invite_create)),
    ?assertEqual(true, is_invite_event(invite_delete)),
    ?assertEqual(false, is_invite_event(message_create)).

is_bulk_update_event_test() ->
    ?assertEqual(true, is_bulk_update_event(channel_update_bulk)),
    ?assertEqual(false, is_bulk_update_event(channel_update)).

is_message_access_filtered_event_test() ->
    ?assertEqual(true, is_message_access_filtered_event(message_update)),
    ?assertEqual(true, is_message_access_filtered_event(message_delete)),
    ?assertEqual(true, is_message_access_filtered_event(message_reaction_add)),
    ?assertEqual(true, is_message_access_filtered_event(message_reaction_remove)),
    ?assertEqual(true, is_message_access_filtered_event(message_reaction_remove_all)),
    ?assertEqual(true, is_message_access_filtered_event(message_reaction_remove_emoji)),
    ?assertEqual(true, is_message_access_filtered_event(message_poll_vote_add)),
    ?assertEqual(true, is_message_access_filtered_event(message_poll_vote_remove)),
    ?assertEqual(false, is_message_access_filtered_event(message_create)),
    ?assertEqual(false, is_message_access_filtered_event(message_delete_bulk)),
    ?assertEqual(false, is_message_access_filtered_event(typing_start)),
    ?assertEqual(false, is_message_access_filtered_event(channel_create)).

extract_message_id_from_id_field_test() ->
    Data = #{<<"id">> => <<"12345">>, <<"channel_id">> => <<"100">>},
    ?assertEqual(<<"12345">>, extract_message_id(Data)).

extract_message_id_from_message_id_field_test() ->
    Data = #{<<"message_id">> => <<"67890">>, <<"channel_id">> => <<"100">>},
    ?assertEqual(<<"67890">>, extract_message_id(Data)).

extract_message_id_prefers_message_id_test() ->
    Data = #{<<"id">> => <<"12345">>, <<"message_id">> => <<"67890">>},
    ?assertEqual(<<"67890">>, extract_message_id(Data)).

extract_channel_id_channel_delete_uses_id_field_test() ->
    Data = #{<<"id">> => <<"42">>},
    ?assertEqual(42, extract_channel_id(channel_delete, Data)).

extract_channel_id_message_create_uses_channel_id_field_test() ->
    Data = #{<<"channel_id">> => <<"42">>},
    ?assertEqual(42, extract_channel_id(message_create, Data)).

extract_channel_id_channel_create_uses_id_field_test() ->
    Data = #{<<"id">> => <<"42">>},
    ?assertEqual(42, extract_channel_id(channel_create, Data)).

extract_channel_id_channel_update_uses_id_field_test() ->
    Data = #{<<"id">> => <<"42">>},
    ?assertEqual(42, extract_channel_id(channel_update, Data)).

filter_sessions_for_event_guild_wide_goes_to_all_sessions_test() ->
    S1 = #{session_id => <<"s1">>, user_id => 10, pid => self()},
    S2 = #{session_id => <<"s2">>, user_id => 11, pid => self()},
    Sessions = #{<<"s1">> => S1, <<"s2">> => S2},
    State = #{sessions => Sessions, data => #{<<"members">> => #{}}},
    Result = filter_sessions_for_event(guild_member_add, #{}, undefined, Sessions, State),
    ?assertEqual(2, length(Result)).

filter_sessions_for_event_audit_log_requires_view_permission_test() ->
    {Sessions, State, Allowed} = audit_log_test_fixture(),
    Result = filter_sessions_for_event(
        guild_audit_log_entry_create, #{}, undefined, Sessions, State
    ),
    ?assertEqual([{<<"allowed">>, Allowed}], Result).

audit_log_test_fixture() ->
    GuildId = 1,
    GIdBin = integer_to_binary(GuildId),
    AuditPerm = integer_to_binary(constants:view_audit_log_permission()),
    Allowed = #{session_id => <<"allowed">>, user_id => 2001, pid => self()},
    Denied = #{session_id => <<"denied">>, user_id => 2002, pid => self()},
    Sessions = #{<<"allowed">> => Allowed, <<"denied">> => Denied},
    Guild = #{<<"id">> => GIdBin, <<"owner_id">> => <<"999">>, <<"features">> => []},
    State = #{
        id => GuildId,
        sessions => Sessions,
        data => #{
            <<"guild">> => Guild,
            <<"roles">> => [
                #{<<"id">> => GIdBin, <<"permissions">> => <<"0">>},
                #{<<"id">> => <<"300">>, <<"permissions">> => AuditPerm}
            ],
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => <<"2001">>}, <<"roles">> => [<<"300">>]},
                #{<<"user">> => #{<<"id">> => <<"2002">>}, <<"roles">> => []}
            ]
        }
    },
    {Sessions, State, Allowed}.

-endif.
