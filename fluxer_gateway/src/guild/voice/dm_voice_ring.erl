%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(dm_voice_ring).
-typing([eqwalizer]).

-export([fetch_dm_channel_via_rpc/2]).
-export([convert_api_channel_to_gateway_format/2]).
-export([check_recipient/3]).
-export([is_dm_channel_type/1]).
-export([broadcast_voice_state_update/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export_type([
    dm_state/0,
    voice_state/0
]).

-type dm_state() :: map().
-type voice_state() :: map().

-spec fetch_dm_channel_via_rpc(integer(), integer()) -> {ok, map()} | {error, term()}.
fetch_dm_channel_via_rpc(ChannelId, UserId) ->
    Req = #{
        <<"type">> => <<"get_dm_channel">>,
        <<"channel_id">> => ChannelId,
        <<"user_id">> => UserId
    },
    logger:debug(
        "dm_voice_fetch_channel_request: user_id=~p channel_id=~p",
        [UserId, ChannelId]
    ),
    handle_fetch_result(rpc_client:call(Req), ChannelId, UserId).

-spec handle_fetch_result(term(), integer(), integer()) -> {ok, map()} | {error, term()}.
handle_fetch_result({ok, #{<<"channel">> := null}}, ChannelId, UserId) ->
    logger:warning(
        "dm_voice_fetch_channel_not_found: user_id=~p channel_id=~p",
        [UserId, ChannelId]
    ),
    {error, not_found};
handle_fetch_result({ok, #{<<"channel">> := Channel}}, ChannelId, UserId) when
    is_map(Channel)
->
    logger:debug(
        "dm_voice_fetch_channel_ok: user_id=~p channel_id=~p",
        [UserId, ChannelId]
    ),
    {ok, convert_api_channel_to_gateway_format(Channel, UserId)};
handle_fetch_result({ok, _}, ChannelId, UserId) ->
    logger:warning(
        "dm_voice_fetch_channel_invalid_payload: user_id=~p channel_id=~p",
        [UserId, ChannelId]
    ),
    {error, not_found};
handle_fetch_result({error, Reason}, ChannelId, UserId) ->
    logger:warning(
        "dm_voice_fetch_channel_error: user_id=~p channel_id=~p reason=~p",
        [UserId, ChannelId, Reason]
    ),
    {error, Reason}.

-spec convert_api_channel_to_gateway_format(map(), integer()) -> map().
convert_api_channel_to_gateway_format(Channel, CurrentUserId) ->
    ChannelType = map_utils:get_integer(Channel, <<"type">>, undefined),
    Recipients = maps:get(<<"recipients">>, Channel, []),
    RecipientIds = lists:filtermap(
        fun(R) -> extract_recipient_id(R, CurrentUserId) end,
        Recipients
    ),
    #{
        <<"id">> => maps:get(<<"id">>, Channel),
        <<"type">> => ChannelType,
        <<"recipient_ids">> => RecipientIds
    }.

-spec extract_recipient_id(term(), integer()) -> {true, integer()} | false.
extract_recipient_id(Recipient, CurrentUserId) ->
    filter_recipient_id(recipient_entry_id(Recipient), CurrentUserId).

-spec recipient_entry_id(term()) -> integer() | null.
recipient_entry_id(Recipient) when is_map(Recipient) ->
    case maps:get(<<"id">>, Recipient, undefined) of
        undefined -> null;
        Id -> parse_id(Id)
    end;
recipient_entry_id(Id) ->
    parse_id(Id).

-spec parse_id(term()) -> integer() | null.
parse_id(Id) when is_integer(Id), Id > 0 -> Id;
parse_id(Id) when is_binary(Id) ->
    case validation:validate_snowflake(<<"id">>, Id) of
        {ok, IntId} when IntId > 0 -> IntId;
        _ -> null
    end;
parse_id(_) ->
    null.

-spec filter_recipient_id(integer() | null, integer()) -> {true, integer()} | false.
filter_recipient_id(null, _CurrentUserId) -> false;
filter_recipient_id(Id, Id) -> false;
filter_recipient_id(Id, _CurrentUserId) -> {true, Id}.

-spec broadcast_voice_state_update(integer(), voice_state(), dm_state()) -> ok.
broadcast_voice_state_update(ChannelId, VoiceState, State) ->
    Channels = maps:get(channels, State, #{}),
    case maps:get(ChannelId, Channels, undefined) of
        undefined ->
            ok;
        Channel ->
            do_broadcast_voice_state(Channel, VoiceState, State)
    end.

-spec do_broadcast_voice_state(map(), voice_state(), dm_state()) -> ok.
do_broadcast_voice_state(Channel, VoiceState, State) ->
    Recipients = channel_recipient_ids(Channel),
    UserId = maps:get(user_id, State),
    AllRecipients = lists:usort([UserId | Recipients]),
    Event = voice_state_update,
    SanitizedVoiceState = voice_state_utils:sanitize_voice_state_for_broadcast(VoiceState),
    spawn(fun() -> dispatch_voice_state_update(AllRecipients, Event, SanitizedVoiceState) end),
    ok.

-spec channel_recipient_ids(map()) -> [integer()].
channel_recipient_ids(Channel) ->
    Raw =
        case maps:get(<<"recipient_ids">>, Channel, undefined) of
            Ids when is_list(Ids) -> Ids;
            _ -> ensure_list(maps:get(<<"recipients">>, Channel, []))
        end,
    [Id || Entry <- Raw, Id <- [recipient_entry_id(Entry)], is_integer(Id)].

-spec ensure_list(term()) -> [term()].
ensure_list(Value) when is_list(Value) -> Value;
ensure_list(_) -> [].

-spec dispatch_voice_state_update([integer()], atom(), voice_state()) -> ok.
dispatch_voice_state_update(AllRecipients, Event, VoiceState) ->
    lists:foreach(
        fun(RecipientId) ->
            presence_manager:dispatch_to_user(RecipientId, Event, VoiceState)
        end,
        AllRecipients
    ).

-spec check_recipient(integer(), integer(), dm_state()) -> boolean().
check_recipient(UserId, ChannelId, State) ->
    Channels = maps:get(channels, State, #{}),
    case maps:get(ChannelId, Channels, undefined) of
        undefined ->
            false;
        Channel ->
            ChannelType = map_utils:get_integer(Channel, <<"type">>, undefined),
            is_dm_channel_type(ChannelType) andalso
                is_channel_recipient(UserId, State)
    end.

-spec is_dm_channel_type(term()) -> boolean().
is_dm_channel_type(1) -> true;
is_dm_channel_type(3) -> true;
is_dm_channel_type(_) -> false.

-spec is_channel_recipient(integer(), dm_state()) -> boolean().
is_channel_recipient(UserId, State) ->
    UserId =:= maps:get(user_id, State).

-ifdef(TEST).

check_recipient_accepts_session_owner_for_dm_types_test() ->
    State = #{
        user_id => 1,
        channels => #{
            100 => #{<<"id">> => <<"100">>, <<"type">> => 1},
            200 => #{<<"id">> => <<"200">>, <<"type">> => 3},
            300 => #{<<"id">> => <<"300">>, <<"type">> => 0}
        }
    },
    ?assert(check_recipient(1, 100, State)),
    ?assert(check_recipient(1, 200, State)),
    ?assertNot(check_recipient(2, 100, State)),
    ?assertNot(check_recipient(1, 300, State)),
    ?assertNot(check_recipient(1, 999, State)).

-endif.
