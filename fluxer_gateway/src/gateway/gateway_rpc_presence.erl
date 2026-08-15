%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_presence).
-typing([eqwalizer]).

-export([execute_method/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(PRESENCE_LOOKUP_TIMEOUT, 2000).

-spec execute_method(binary(), map()) -> term().
execute_method(<<"presence.dispatch">>, P) -> handle_dispatch(P);
execute_method(<<"presence.join_guild">>, P) -> handle_join_guild(P);
execute_method(<<"presence.leave_guild">>, P) -> handle_leave_guild(P);
execute_method(<<"presence.terminate_sessions">>, P) -> handle_terminate_sessions(P);
execute_method(<<"presence.terminate_all_sessions">>, P) -> handle_terminate_all(P);
execute_method(<<"presence.has_active">>, P) -> handle_has_active(P);
execute_method(<<"presence.add_temporary_guild">>, P) -> handle_add_temp_guild(P);
execute_method(<<"presence.remove_temporary_guild">>, P) -> handle_remove_temp_guild(P);
execute_method(<<"presence.sync_group_dm_recipients">>, P) -> handle_sync_dm_recipients(P).

-spec handle_dispatch(map()) -> true.
handle_dispatch(#{<<"user_id">> := UserIdBin, <<"event">> := Event, <<"data">> := Data}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    EventAtom = dispatch_event_atom_or_error(Event),
    case dispatch_to_owner(UserId, EventAtom, Data) of
        ok -> true;
        {error, not_found} -> handle_offline_dispatch(EventAtom, UserId, Data);
        _ -> gateway_rpc_error:raise(<<"presence_dispatch_error">>)
    end.

-spec dispatch_event_atom_or_error(term()) -> atom().
dispatch_event_atom_or_error(Event) when is_binary(Event) ->
    normalize_dispatch_event_atom(Event);
dispatch_event_atom_or_error(Event) ->
    case is_atom(Event) of
        true -> normalize_dispatch_event_atom(Event);
        false -> gateway_rpc_error:raise(<<"presence_dispatch_error">>)
    end.

-spec normalize_dispatch_event_atom(atom() | binary()) -> atom().
normalize_dispatch_event_atom(Event) ->
    case constants:dispatch_event_atom(Event) of
        EventAtom when is_atom(EventAtom) -> EventAtom;
        _ -> gateway_rpc_error:raise(<<"presence_dispatch_error">>)
    end.

-spec handle_join_guild(map()) -> true.
handle_join_guild(#{<<"user_id">> := UserIdBin, <<"guild_id">> := GuildIdBin}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    forward_presence_async(UserId, {join_guild, GuildId}),
    true.

-spec handle_leave_guild(map()) -> true.
handle_leave_guild(#{<<"user_id">> := UserIdBin, <<"guild_id">> := GuildIdBin}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    forward_presence_async(UserId, {leave_guild, GuildId}),
    true.

-spec handle_terminate_sessions(map()) -> true.
handle_terminate_sessions(#{
    <<"user_id">> := UserIdBin,
    <<"session_id_hashes">> := SessionIdHashes
}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    case forward_presence_sync(UserId, {terminate_session, SessionIdHashes}) of
        ok -> true;
        {error, not_found} -> true;
        _ -> gateway_rpc_error:raise(<<"presence_terminate_sessions_error">>)
    end.

-spec handle_terminate_all(map()) -> true.
handle_terminate_all(#{<<"user_id">> := UserIdBin}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    forward_presence_async(UserId, {terminate_all_sessions}),
    true.

-spec handle_has_active(map()) -> map().
handle_has_active(#{<<"user_id">> := UserIdBin}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    case lookup_owner_presence(UserId) of
        {ok, _Pid} -> #{<<"has_active">> => true};
        {error, not_found} -> #{<<"has_active">> => false};
        _ -> gateway_rpc_error:raise(<<"presence_lookup_error">>)
    end.

-spec handle_add_temp_guild(map()) -> true.
handle_add_temp_guild(#{<<"user_id">> := UserIdBin, <<"guild_id">> := GuildIdBin}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    forward_presence_async(UserId, {add_temporary_guild, GuildId}),
    true.

-spec handle_remove_temp_guild(map()) -> true.
handle_remove_temp_guild(#{<<"user_id">> := UserIdBin, <<"guild_id">> := GuildIdBin}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    forward_presence_async(UserId, {remove_temporary_guild, GuildId}),
    true.

-spec handle_sync_dm_recipients(map()) -> true.
handle_sync_dm_recipients(#{
    <<"user_id">> := UserIdBin,
    <<"recipients_by_channel">> := RecipientsByChannel
}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    _ = normalize_recipients(RecipientsByChannel),
    case lookup_owner_presence(UserId) of
        {ok, Pid} ->
            gen_server:cast(Pid, presence_rejoin),
            true;
        {error, not_found} ->
            true;
        _ ->
            gateway_rpc_error:raise(<<"presence_lookup_error">>)
    end.

-spec normalize_recipients(map()) -> map().
normalize_recipients(RecipientsByChannel) ->
    maps:fold(
        fun(ChannelIdBin, Recipients, Acc) ->
            Acc#{
                validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin) =>
                    [
                        validation:snowflake_or_throw(<<"recipient_id">>, RBin)
                     || RBin <- Recipients
                    ]
            }
        end,
        #{},
        RecipientsByChannel
    ).

-spec dispatch_to_owner(integer(), atom(), map()) -> ok | {error, not_found} | {error, term()}.
dispatch_to_owner(UserId, EventAtom, Data) ->
    case resolve_owner_node(UserId) of
        {ok, OwnerNode} ->
            Request = {dispatch, UserId, EventAtom, Data},
            normalize_dispatch_result(
                call_presence_manager_node(
                    OwnerNode,
                    Request,
                    ?PRESENCE_LOOKUP_TIMEOUT
                )
            );
        unavailable ->
            {error, unavailable}
    end.

-spec normalize_dispatch_result(term()) -> ok | {error, not_found} | {error, term()}.
normalize_dispatch_result(ok) ->
    ok;
normalize_dispatch_result({error, not_found}) ->
    {error, not_found};
normalize_dispatch_result({error, _Reason} = Error) ->
    Error;
normalize_dispatch_result(_Result) ->
    {error, invalid_reply}.

-spec forward_presence_async(integer(), term()) -> ok.
forward_presence_async(UserId, Message) ->
    case lookup_owner_presence(UserId) of
        {ok, Pid} ->
            gen_server:cast(Pid, Message),
            ok;
        {error, not_found} ->
            ok;
        _ ->
            gateway_rpc_error:raise(<<"presence_lookup_error">>)
    end.

-spec forward_presence_sync(integer(), term()) ->
    ok | {error, not_found | timeout | unavailable | term()}.
forward_presence_sync(UserId, Message) ->
    case lookup_owner_presence(UserId) of
        {ok, Pid} ->
            call_presence(Pid, Message);
        {error, not_found} ->
            {error, not_found};
        {error, _Reason} = Error ->
            Error
    end.

-spec call_presence(pid(), term()) -> ok | {error, timeout | unavailable | term()}.
call_presence(Pid, Message) ->
    try gen_server:call(Pid, Message, ?PRESENCE_LOOKUP_TIMEOUT) of
        ok ->
            ok;
        {error, _Reason} = Error ->
            Error;
        Other ->
            {error, {invalid_reply, Other}}
    catch
        throw:ok -> ok;
        throw:{error, _Reason} = Error -> Error;
        throw:Other -> {error, {invalid_reply, Other}};
        exit:{timeout, _} -> {error, timeout};
        exit:{noproc, _} -> {error, unavailable};
        exit:Reason -> {error, Reason};
        error:Reason:Stack -> {error, {Reason, Stack}}
    end.

-spec lookup_owner_presence(integer()) ->
    {ok, pid()} | {error, not_found | timeout | unavailable | term()}.
lookup_owner_presence(UserId) ->
    case resolve_owner_node(UserId) of
        {ok, OwnerNode} -> lookup_presence_on_node(OwnerNode, UserId);
        unavailable -> {error, unavailable}
    end.

-spec call_presence_manager_node(node(), term(), timeout()) -> term().
call_presence_manager_node(TargetNode, Request, Timeout) ->
    try gen_server:call(presence_manager_server_ref(TargetNode), Request, Timeout) of
        Reply -> Reply
    catch
        throw:{'EXIT', {timeout, _}} -> {error, timeout};
        throw:{'EXIT', {nodedown, _}} -> {error, unavailable};
        throw:{'EXIT', {noproc, _}} -> {error, unavailable};
        throw:{'EXIT', _} -> {error, unavailable};
        throw:Reply -> Reply;
        exit:{timeout, _} -> {error, timeout};
        exit:{nodedown, _} -> {error, unavailable};
        exit:{noproc, _} -> {error, unavailable};
        exit:_ -> {error, unavailable};
        error:_ -> {error, unavailable}
    end.

-spec lookup_presence_on_node(node(), integer()) ->
    {ok, pid()} | {error, not_found | timeout | unavailable | term()}.
lookup_presence_on_node(TargetNode, UserId) ->
    case call_presence_manager_node(TargetNode, {lookup, UserId}, ?PRESENCE_LOOKUP_TIMEOUT) of
        {ok, Pid} when is_pid(Pid) ->
            {ok, Pid};
        {error, not_found} ->
            {error, not_found};
        {error, _Reason} = Error ->
            Error;
        _ ->
            {error, invalid_reply}
    end.

-spec presence_manager_server_ref(node()) -> atom() | {atom(), node()}.
presence_manager_server_ref(TargetNode) when TargetNode =:= node() ->
    presence_manager;
presence_manager_server_ref(TargetNode) ->
    {presence_manager, TargetNode}.

-spec resolve_owner_node(integer()) -> {ok, node()} | unavailable.
resolve_owner_node(UserId) ->
    resolve_owner_node(UserId, fun owner_node_result_for_presence/1).

-spec owner_node_result_for_presence(integer()) -> {ok, node()} | unavailable.
owner_node_result_for_presence(UserId) ->
    case gateway_node_router:owner_node_result(UserId, presence) of
        {ok, Node} when is_atom(Node) -> {ok, Node};
        _ -> unavailable
    end.

-spec resolve_owner_node(integer(), fun((integer()) -> term())) -> {ok, node()} | unavailable.
resolve_owner_node(UserId, OwnerResolver) ->
    try OwnerResolver(UserId) of
        {ok, OwnerNode} when is_atom(OwnerNode) ->
            maybe_valid_owner_node(OwnerNode);
        _ ->
            unavailable
    catch
        throw:{ok, OwnerNode} when is_atom(OwnerNode) ->
            maybe_valid_owner_node(OwnerNode);
        throw:_ ->
            unavailable;
        error:_ ->
            unavailable;
        exit:_ ->
            unavailable
    end.

-spec maybe_valid_owner_node(node()) -> {ok, node()} | unavailable.
maybe_valid_owner_node(OwnerNode) ->
    case lists:member($@, atom_to_list(OwnerNode)) of
        true -> {ok, OwnerNode};
        false -> unavailable
    end.

-spec handle_offline_dispatch(atom(), integer(), map()) -> true.
handle_offline_dispatch(message_create, UserId, Data) ->
    case offline_message_author_id(Data) of
        {ok, AuthorId} ->
            push:handle_message_create(#{
                message_data => Data,
                user_ids => [UserId],
                guild_id => 0,
                author_id => AuthorId
            });
        system ->
            ok;
        invalid ->
            logger:debug("Presence offline message dispatch skipped push for invalid author", #{
                user_id => UserId
            })
    end,
    true;
handle_offline_dispatch(relationship_add, UserId, _Data) ->
    sync_blocked_ids_for_user(UserId),
    true;
handle_offline_dispatch(relationship_remove, UserId, _Data) ->
    sync_blocked_ids_for_user(UserId),
    true;
handle_offline_dispatch(_Event, _UserId, _Data) ->
    true.

-spec offline_message_author_id(map()) -> {ok, pos_integer()} | system | invalid.
offline_message_author_id(Data) ->
    AuthorId = maps:get(<<"id">>, maps:get(<<"author">>, Data, #{}), undefined),
    case AuthorId of
        0 -> system;
        <<"0">> -> system;
        _ -> validate_offline_message_author_id(AuthorId)
    end.

-spec validate_offline_message_author_id(term()) -> {ok, pos_integer()} | invalid.
validate_offline_message_author_id(AuthorId) ->
    case validation:validate_snowflake(<<"author_id">>, AuthorId) of
        {ok, ParsedAuthorId} -> {ok, ParsedAuthorId};
        _ -> invalid
    end.

-spec sync_blocked_ids_for_user(integer()) -> ok.
sync_blocked_ids_for_user(_UserId) ->
    ok.

-ifdef(TEST).

normalize_recipients_test() ->
    Input = #{<<"123">> => [<<"456">>, <<"789">>]},
    Result = normalize_recipients(Input),
    ?assert(is_map(Result)),
    ?assertEqual(1, maps:size(Result)).

resolve_owner_node_uses_remote_owner_when_valid_test() ->
    RemoteNode = 'gateway_b@127.0.0.1',
    ?assertEqual(
        {ok, RemoteNode},
        resolve_owner_node(123, fun(_UserId) -> {ok, RemoteNode} end)
    ).

resolve_owner_node_returns_unavailable_when_invalid_owner_test() ->
    ?assertEqual(
        unavailable,
        resolve_owner_node(123, fun(_UserId) -> {ok, bad_owner} end)
    ),
    ?assertEqual(
        unavailable,
        resolve_owner_node(123, fun(_UserId) -> {bad_owner} end)
    ).

presence_manager_server_ref_local_test() ->
    ?assertEqual(presence_manager, presence_manager_server_ref(node())).

offline_message_author_id_accepts_system_user_test() ->
    ?assertEqual(system, offline_message_author_id(#{<<"author">> => #{<<"id">> => <<"0">>}})),
    ?assertEqual(system, offline_message_author_id(#{<<"author">> => #{<<"id">> => 0}})).

offline_message_author_id_accepts_positive_snowflake_test() ->
    ?assertEqual(
        {ok, 123}, offline_message_author_id(#{<<"author">> => #{<<"id">> => <<"123">>}})
    ).

offline_message_create_system_author_is_success_test() ->
    ?assertEqual(
        true,
        handle_offline_dispatch(
            message_create,
            42,
            #{
                <<"id">> => <<"456">>,
                <<"channel_id">> => <<"123">>,
                <<"author">> => #{<<"id">> => <<"0">>}
            }
        )
    ).

-endif.
