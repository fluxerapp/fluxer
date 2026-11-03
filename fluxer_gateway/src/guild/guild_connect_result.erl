%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_connect_result).
-typing([eqwalizer]).

-export([
    compute_and_send_done/3,
    compute_session_connect/3,
    computed_result/1,
    resolve_guild_id/2,
    send_result/4
]).

-spec compute_and_send_done([map()], pid(), map()) -> ok.
compute_and_send_done(Items, GuildPid, Snapshot) ->
    Results = [compute_item(Item, Snapshot) || Item <- Items],
    gen_server:cast(GuildPid, {session_connect_worker_batch_done, Results}),
    ok.

-spec compute_item(map(), map()) -> guild_connect_async:connect_result().
compute_item(Item, Snapshot) ->
    Request = maps:get(request, Item, #{}),
    {Result0, Computed0} = compute_connect_result(Snapshot, Item, Request),
    {
        maps:get(session_id, Request, undefined),
        maps:get(attempt, Item, 0),
        Result0,
        maps:merge(Item, Computed0)
    }.

-spec compute_connect_result(map(), map(), map()) ->
    {{ok, map()} | {ok_unavailable, map()} | {error, term()}, map()}.
compute_connect_result(Snapshot, Item, Request) ->
    case resolve_guild_id(Snapshot, Item) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            safe_compute_connect(GuildId, Request, Snapshot);
        _ ->
            {{error, missing_guild_id}, #{}}
    end.

-spec safe_compute_connect(integer(), map(), map()) ->
    {{ok, map()} | {ok_unavailable, map()} | {error, term()}, map()}.
safe_compute_connect(GuildId, Request, Snapshot) ->
    try compute_session_connect(GuildId, Request, Snapshot) of
        Tmp -> {computed_result(Tmp), Tmp}
    catch
        _:Reason ->
            {{error, {session_connect_async_failed, Reason}}, #{}}
    end.

-spec compute_session_connect(integer(), map(), map()) -> map().
compute_session_connect(GuildId, #{user_id := UserId} = Request, State) when
    is_integer(UserId)
->
    case guild_availability:is_guild_unavailable_for_user(UserId, State) of
        true ->
            #{
                thread_viewer => maps:get(
                    thread_viewer, guild_thread_gate:session_fields(Request, UserId, State)
                ),
                unavailable_response => #{
                    <<"id">> => integer_to_binary(GuildId),
                    <<"unavailable">> => true
                }
            };
        false ->
            compute_available_connect(GuildId, UserId, Request, State)
    end;
compute_session_connect(_GuildId, _Request, _State) ->
    #{}.

-spec compute_available_connect(integer(), integer(), map(), map()) -> map().
compute_available_connect(GuildId, UserId, Request, State) ->
    case guild_permissions_check:find_member_by_user_id(UserId, State) of
        undefined ->
            #{not_member => true};
        _Member ->
            ThreadFields = guild_thread_gate:session_fields(Request, UserId, State),
            GS = guild_data:get_guild_state(
                UserId, State, guild_thread_gate:state_opts(maps:merge(Request, ThreadFields))
            ),
            #{
                guild_state => GS,
                thread_viewer => maps:get(thread_viewer, ThreadFields),
                initial_last_message_ids => guild_sessions_connect:build_initial_last_message_ids(
                    GS
                ),
                initial_channel_versions => build_channel_versions(GS),
                viewable_channels => build_viewable_map(
                    guild_visibility_channels:get_user_viewable_channels(UserId, State)
                ),
                user_roles => session_passive:get_user_roles_for_guild(UserId, State),
                should_mark_guild_synced =>
                    maps:get(initial_guild_id, Request, undefined) =:= GuildId
            }
    end.

-spec computed_result(map()) -> {ok, map()} | {ok_unavailable, map()} | {error, term()}.
computed_result(Computed) ->
    case maps:find(unavailable_response, Computed) of
        {ok, U} when is_map(U) -> {ok_unavailable, U};
        _ -> guild_state_or_not_member(Computed)
    end.

-spec guild_state_or_not_member(map()) -> {ok, map()} | {error, term()}.
guild_state_or_not_member(Computed) ->
    case maps:get(not_member, Computed, false) of
        true -> {error, not_member};
        _ -> extract_guild_state_result(Computed)
    end.

-spec extract_guild_state_result(map()) -> {ok, map()} | {error, term()}.
extract_guild_state_result(Computed) ->
    case maps:find(guild_state, Computed) of
        {ok, GS} when is_map(GS) -> {ok, GS};
        _ -> {error, invalid_guild_state}
    end.

-spec resolve_guild_id(map(), map()) -> integer() | undefined.
resolve_guild_id(State, Fallback) ->
    case snowflake_id:parse_optional(maps:get(id, State, undefined)) of
        GuildId when is_integer(GuildId), GuildId > 0 ->
            GuildId;
        _ ->
            snowflake_id:parse_optional(maps:get(guild_id, Fallback, undefined))
    end.

-spec build_channel_versions(map()) -> #{binary() => integer()}.
build_channel_versions(GuildState) ->
    lists:foldl(fun add_channel_version/2, #{}, channels(GuildState)).

-spec channels(map()) -> [term()].
channels(GuildState) ->
    case maps:get(<<"channels">>, GuildState, []) of
        Channels when is_list(Channels) -> Channels;
        _ -> []
    end.

-spec add_channel_version(term(), #{binary() => integer()}) -> #{binary() => integer()}.
add_channel_version(Channel, Acc) when is_map(Channel) ->
    case maps:find(<<"id">>, Channel) of
        {ok, Id} when is_binary(Id) ->
            Acc#{Id => require_integer(map_utils:get_integer(Channel, <<"version">>, 0))};
        _ ->
            Acc
    end;
add_channel_version(_Channel, Acc) ->
    Acc.

-spec require_integer(term()) -> integer().
require_integer(Value) when is_integer(Value) ->
    Value;
require_integer(_) ->
    error(badarg).

-spec build_viewable_map([integer()]) -> map().
build_viewable_map(Ids) ->
    maps:from_list([{Id, true} || Id <- Ids, is_integer(Id), Id > 0]).

-spec send_result(integer(), non_neg_integer(), term(), pid()) -> ok.
send_result(GuildId, Attempt, Result0, SessionPid) ->
    Reply =
        case Result0 of
            {ok, GS} -> {ok, self(), GS};
            {ok_unavailable, U} -> {ok_unavailable, self(), U};
            {error, R} -> {error, R}
        end,
    SessionPid ! {guild_connect_result, GuildId, Attempt, Reply},
    ok.
