%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_rpc_guild_channels).
-typing([eqwalizer]).

-export([handle/2]).

-define(GUILD_CALL_TIMEOUT, 4000).

-spec handle(binary(), map()) -> term().
handle(<<"guild.get_viewable_channels">>, P) -> handle_get_viewable_channels(P);
handle(<<"guild.resolve_channel_mentions">>, P) -> handle_resolve_channel_mentions(P);
handle(<<"guild.get_vanity_url_channel">>, P) -> handle_get_vanity_url_channel(P);
handle(<<"guild.get_first_viewable_text_channel">>, P) -> handle_get_first_viewable(P);
handle(<<"guild.get_category_channel_count">>, P) -> handle_get_category_count(P);
handle(<<"guild.get_channel_count">>, P) -> handle_get_channel_count(P).

-spec handle_get_viewable_channels(map()) -> map().
handle_get_viewable_channels(#{<<"guild_id">> := GIB, <<"user_id">> := UIB}) ->
    GuildId = snowflake_or_throw(<<"guild_id">>, GIB),
    UserId = snowflake_or_throw(<<"user_id">>, UIB),
    case get_viewable_channels_cached_or_rpc(GuildId, UserId) of
        {ok, ChannelIds} -> #{<<"channel_ids">> => format_snowflake_ids(ChannelIds)};
        error -> gateway_rpc_error:raise(<<"viewable_channels_error">>)
    end.

-spec handle_resolve_channel_mentions(map()) -> map().
handle_resolve_channel_mentions(Params) ->
    case Params of
        #{<<"guild_id">> := GIB, <<"channel_ids">> := CIBs} when
            is_binary(GIB), is_list(CIBs)
        ->
            resolve_channel_mentions_params(GIB, CIBs);
        _ ->
            #{<<"channels">> => []}
    end.

-spec resolve_channel_mentions_params(binary(), [term()]) -> map().
resolve_channel_mentions_params(GuildIdBin, ChannelIdBins) ->
    GuildId = snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    ChannelIds = parse_channel_ids(ChannelIdBins),
    case resolve_channel_mentions_cached_or_rpc(GuildId, ChannelIds) of
        {ok, Channels} -> #{<<"channels">> => format_channel_mentions(Channels)};
        error -> #{<<"channels">> => []}
    end.

-spec parse_channel_ids([term()]) -> [integer()].
parse_channel_ids(ChannelIdBins) ->
    try snowflake_id:parse_list(ChannelIdBins) of
        ChannelIds -> ChannelIds
    catch
        error:{invalid_snowflake, _} -> []
    end.

-spec handle_get_vanity_url_channel(map()) -> term().
handle_get_vanity_url_channel(#{<<"guild_id">> := GIB}) ->
    GuildId = snowflake_or_throw(<<"guild_id">>, GIB),
    gateway_rpc_guild_infra:with_guild(GuildId, fun get_vanity_url_channel/1).

-spec get_vanity_url_channel(pid()) -> map().
get_vanity_url_channel(Pid) ->
    case gen_server:call(Pid, {get_vanity_url_channel}, ?GUILD_CALL_TIMEOUT) of
        #{channel_id := CId} -> format_channel_id_reply(CId, <<"vanity_url_channel_error">>);
        _ -> gateway_rpc_error:raise(<<"vanity_url_channel_error">>)
    end.

-spec handle_get_first_viewable(map()) -> term().
handle_get_first_viewable(#{<<"guild_id">> := GIB}) ->
    GuildId = snowflake_or_throw(<<"guild_id">>, GIB),
    gateway_rpc_guild_infra:with_guild(GuildId, fun get_first_viewable_text_channel/1).

-spec get_first_viewable_text_channel(pid()) -> map().
get_first_viewable_text_channel(Pid) ->
    case gen_server:call(Pid, {get_first_viewable_text_channel}, ?GUILD_CALL_TIMEOUT) of
        #{channel_id := CId} ->
            format_channel_id_reply(CId, <<"first_viewable_text_channel_error">>);
        _ ->
            gateway_rpc_error:raise(<<"first_viewable_text_channel_error">>)
    end.

-spec handle_get_category_count(map()) -> term().
handle_get_category_count(#{<<"guild_id">> := GIB, <<"category_id">> := CatIB}) ->
    GuildId = snowflake_or_throw(<<"guild_id">>, GIB),
    CategoryId = snowflake_or_throw(<<"category_id">>, CatIB),
    gateway_rpc_guild_infra:with_guild(GuildId, fun(Pid) ->
        get_category_channel_count(Pid, CategoryId)
    end).

-spec get_category_channel_count(pid(), integer()) -> map().
get_category_channel_count(Pid, CategoryId) ->
    Request = {get_category_channel_count, #{category_id => CategoryId}},
    case gen_server:call(Pid, Request, ?GUILD_CALL_TIMEOUT) of
        #{count := Count} -> #{<<"count">> => Count};
        _ -> gateway_rpc_error:raise(<<"category_channel_count_error">>)
    end.

-spec handle_get_channel_count(map()) -> term().
handle_get_channel_count(#{<<"guild_id">> := GIB}) ->
    GuildId = snowflake_or_throw(<<"guild_id">>, GIB),
    gateway_rpc_guild_infra:with_guild(GuildId, fun get_channel_count/1).

-spec get_channel_count(pid()) -> map().
get_channel_count(Pid) ->
    case gen_server:call(Pid, {get_channel_count}, ?GUILD_CALL_TIMEOUT) of
        #{count := Count} -> #{<<"count">> => Count};
        _ -> gateway_rpc_error:raise(<<"channel_count_error">>)
    end.

-spec get_viewable_channels_cached_or_rpc(integer(), integer()) -> {ok, [integer()]} | error.
get_viewable_channels_cached_or_rpc(GuildId, UserId) ->
    case guild_permission_cache:get_snapshot(GuildId) of
        {ok, Snapshot} ->
            get_viewable_channels_from_snapshot(GuildId, UserId, Snapshot);
        {error, not_found} ->
            get_viewable_channels_via_rpc(GuildId, UserId)
    end.

-spec get_viewable_channels_from_snapshot(integer(), integer(), map()) ->
    {ok, [integer()]} | error.
get_viewable_channels_from_snapshot(GuildId, UserId, Snapshot) ->
    case should_bypass_viewable_cache(UserId, Snapshot) of
        true -> get_viewable_channels_via_rpc(GuildId, UserId);
        false -> {ok, guild_visibility:get_user_viewable_channels(UserId, Snapshot)}
    end.

-spec should_bypass_viewable_cache(integer(), map()) -> boolean().
should_bypass_viewable_cache(UserId, Snapshot) ->
    Data = map_utils:ensure_map(maps:get(data, Snapshot, #{})),
    Member = guild_permissions:find_member_by_user_id(UserId, Snapshot),
    Channels = maps:get(<<"channels">>, Data, undefined),
    ChannelIndex = guild_data_index:channel_index(Data),
    HasIndexedChannels = is_map(ChannelIndex) andalso map_size(ChannelIndex) > 0,
    Member =/= undefined andalso HasIndexedChannels andalso empty_channels_list(Channels).

-spec empty_channels_list(term()) -> boolean().
empty_channels_list(Channels) ->
    not is_list(Channels) orelse Channels =:= [].

-spec get_viewable_channels_via_rpc(integer(), integer()) -> {ok, [integer()]} | error.
get_viewable_channels_via_rpc(GuildId, UserId) ->
    case gateway_rpc_guild_infra:ensure_guild_pid(GuildId) of
        {ok, Pid} ->
            get_viewable_channels_from_pid(GuildId, Pid, UserId);
        error ->
            error
    end.

-spec get_viewable_channels_from_pid(integer(), pid(), integer()) -> {ok, [integer()]} | error.
get_viewable_channels_from_pid(GuildId, Pid, UserId) ->
    Request = {get_viewable_channels, #{user_id => UserId}},
    case gateway_rpc_guild_infra:safe_guild_call(GuildId, Pid, Request, ?GUILD_CALL_TIMEOUT) of
        {ok, #{channel_ids := CIds}} when is_list(CIds) -> {ok, snowflake_id:parse_list(CIds)};
        _ -> error
    end.

-spec resolve_channel_mentions_cached_or_rpc(integer(), [integer()]) -> {ok, [map()]} | error.
resolve_channel_mentions_cached_or_rpc(_GuildId, []) ->
    {ok, []};
resolve_channel_mentions_cached_or_rpc(GuildId, ChannelIds) ->
    case guild_permission_cache:get_snapshot(GuildId) of
        {ok, Snapshot} ->
            resolve_from_snapshot_or_rpc(GuildId, ChannelIds, Snapshot);
        {error, not_found} ->
            resolve_via_rpc(GuildId, ChannelIds)
    end.

-spec resolve_from_snapshot_or_rpc(integer(), [integer()], map()) -> {ok, [map()]} | error.
resolve_from_snapshot_or_rpc(GuildId, ChannelIds, Snapshot) ->
    case resolve_from_snapshot(ChannelIds, Snapshot, GuildId) of
        {ok, Channels} -> {ok, Channels};
        fallback -> resolve_via_rpc(GuildId, ChannelIds)
    end.

-spec resolve_from_snapshot([integer()], map(), integer()) ->
    {ok, [map()]} | fallback.
resolve_from_snapshot(ChannelIds, Snapshot, GuildId) ->
    Data = map_utils:ensure_map(maps:get(data, Snapshot, #{})),
    Roles = guild_data_index:role_index(Data),
    ChIndex = guild_data_index:channel_index(Data),
    BasePermissions = everyone_role_permissions(GuildId, Roles),
    NeedsFallback = snapshot_needs_fallback(ChannelIds, map_utils:ensure_map(ChIndex)),
    case {BasePermissions, NeedsFallback} of
        {undefined, _} ->
            fallback;
        {_BasePerm, true} ->
            fallback;
        {BasePerm, false} ->
            Channels = channel_mentions_from_snapshot(
                unique_preserve(ChannelIds), ChIndex, GuildId, BasePerm
            ),
            {ok, Channels}
    end.

-spec channel_mentions_from_snapshot([integer()], map(), integer(), integer()) -> [map()].
channel_mentions_from_snapshot(ChannelIds, ChIndex, GuildId, BasePerm) ->
    lists:filtermap(
        fun(CId) ->
            channel_mention_from_snapshot(CId, ChIndex, GuildId, BasePerm)
        end,
        ChannelIds
    ).

-spec channel_mention_from_snapshot(integer(), map(), integer(), integer()) ->
    {true, map()} | false.
channel_mention_from_snapshot(CId, ChIndex, GuildId, BasePerm) ->
    case maps:get(CId, ChIndex, undefined) of
        Ch when is_map(Ch) -> viewable_channel_mention(Ch, GuildId, BasePerm);
        _ -> false
    end.

-spec viewable_channel_mention(map(), integer(), integer()) -> {true, map()} | false.
viewable_channel_mention(#{<<"type">> := Type}, _GuildId, _BasePerm) when
    Type =:= 15; Type =:= 16
->
    false;
viewable_channel_mention(Channel, GuildId, BasePerm) ->
    case everyone_can_view(Channel, GuildId, BasePerm) of
        true -> build_channel_mention(Channel);
        false -> false
    end.

-spec snapshot_needs_fallback([integer()], map()) -> boolean().
snapshot_needs_fallback(CIds, ChIndex) ->
    lists:any(
        fun(CId) ->
            snapshot_channel_needs_fallback(CId, ChIndex)
        end,
        CIds
    ).

-spec snapshot_channel_needs_fallback(integer(), map()) -> boolean().
snapshot_channel_needs_fallback(CId, ChIndex) ->
    case maps:get(CId, ChIndex, undefined) of
        Ch when is_map(Ch) -> not maps:is_key(<<"name">>, Ch);
        _ -> false
    end.

-spec resolve_via_rpc(integer(), [integer()]) -> {ok, [map()]} | error.
resolve_via_rpc(GuildId, ChannelIds) ->
    case gateway_rpc_guild_infra:ensure_guild_pid(GuildId) of
        {ok, Pid} ->
            resolve_via_pid(GuildId, Pid, ChannelIds);
        error ->
            error
    end.

-spec resolve_via_pid(integer(), pid(), [integer()]) -> {ok, [map()]} | error.
resolve_via_pid(GuildId, Pid, ChannelIds) ->
    Request = {resolve_channel_mentions, #{channel_ids => unique_preserve(ChannelIds)}},
    case gateway_rpc_guild_infra:safe_guild_call(GuildId, Pid, Request, ?GUILD_CALL_TIMEOUT) of
        {ok, #{channels := Chs}} when is_list(Chs) -> {ok, map_entries(Chs)};
        _ -> error
    end.

-spec map_entries([term()]) -> [map()].
map_entries(Entries) ->
    [Entry || Entry <- Entries, is_map(Entry)].

-spec everyone_role_permissions(integer(), [map()] | map()) -> integer() | undefined.
everyone_role_permissions(GuildId, Roles) ->
    case guild_permissions:find_role_by_id(GuildId, Roles) of
        undefined ->
            undefined;
        Role ->
            permission_bits:parse_optional(maps:get(<<"permissions">>, Role, undefined))
    end.

-spec everyone_can_view(map(), integer(), integer()) -> boolean().
everyone_can_view(Channel, GuildId, BasePerm) ->
    case permission_bits:has(BasePerm, constants:administrator_permission()) of
        true ->
            true;
        false ->
            P = apply_everyone_overwrites(BasePerm, Channel, GuildId),
            permission_bits:has(P, constants:view_channel_permission())
    end.

-spec build_channel_mention(map()) -> {true, map()} | false.
build_channel_mention(Channel) ->
    Id = snowflake_id:parse_optional(maps:get(<<"id">>, Channel, undefined)),
    Name = maps:get(<<"name">>, Channel, undefined),
    Type = guild_data_normalize_schema:int(maps:get(<<"type">>, Channel, undefined)),
    case {Id, Name, Type} of
        {I, N, T} when is_integer(I), is_binary(N), is_integer(T) ->
            {true, #{id => I, name => N, type => T}};
        _ ->
            false
    end.

-spec format_channel_mentions([map()]) -> [map()].
format_channel_mentions(Channels) ->
    lists:filtermap(fun format_channel_mention/1, Channels).

-spec format_channel_mention(map()) -> {true, map()} | false.
format_channel_mention(#{id := Id, name := Name, type := Type}) ->
    format_channel_mention_fields(Id, Name, Type);
format_channel_mention(#{<<"id">> := Id, <<"name">> := Name, <<"type">> := Type}) ->
    format_channel_mention_fields(Id, Name, Type);
format_channel_mention(_) ->
    false.

-spec format_channel_mention_fields(term(), term(), term()) -> {true, map()} | false.
format_channel_mention_fields(Id, Name, Type) ->
    case {snowflake_id:parse_optional(Id), Name, guild_data_normalize_schema:int(Type)} of
        {IdInt, NameBin, TypeInt} when
            is_integer(IdInt), is_binary(NameBin), is_integer(TypeInt)
        ->
            {true, #{
                <<"id">> => integer_to_binary(IdInt),
                <<"name">> => NameBin,
                <<"type">> => TypeInt
            }};
        _ ->
            false
    end.

-spec format_channel_id_reply(term(), binary()) -> map().
format_channel_id_reply(null, _Error) ->
    #{<<"channel_id">> => null};
format_channel_id_reply(ChannelId, Error) ->
    case parse_rpc_snowflake(ChannelId) of
        Id when is_integer(Id) -> #{<<"channel_id">> => snowflake_id:to_binary(Id)};
        undefined -> gateway_rpc_error:raise(Error)
    end.

-spec apply_everyone_overwrites(integer(), map(), integer()) -> integer().
apply_everyone_overwrites(BasePerm, Channel, GuildId) ->
    Overwrites = map_utils:ensure_list(maps:get(<<"permission_overwrites">>, Channel, [])),
    lists:foldl(
        fun(Overwrite, Acc) -> apply_everyone_overwrite(Overwrite, GuildId, Acc) end,
        BasePerm,
        Overwrites
    ).

-spec apply_everyone_overwrite(term(), integer(), integer()) -> integer().
apply_everyone_overwrite(Overwrite, GuildId, Acc) when is_map(Overwrite) ->
    case
        {
            snowflake_id:parse_optional(maps:get(<<"id">>, Overwrite, undefined)),
            guild_data_normalize_schema:int(maps:get(<<"type">>, Overwrite, undefined)),
            permission_bits:parse_optional(maps:get(<<"allow">>, Overwrite, undefined)),
            permission_bits:parse_optional(maps:get(<<"deny">>, Overwrite, undefined))
        }
    of
        {GuildId, 0, Allow, Deny} when is_integer(Allow), is_integer(Deny) ->
            permission_bits:apply_allow_deny(Acc, Allow, Deny);
        _ ->
            Acc
    end;
apply_everyone_overwrite(_, _, Acc) ->
    Acc.

-spec snowflake_or_throw(binary(), term()) -> integer().
snowflake_or_throw(FieldName, Value) ->
    case parse_rpc_snowflake(Value) of
        Id when is_integer(Id) -> Id;
        undefined -> gateway_rpc_error:raise(<<FieldName/binary, "_invalid">>)
    end.

-spec format_snowflake_ids([term()]) -> [binary()].
format_snowflake_ids(Ids) ->
    [snowflake_id:to_binary(Id) || Id <- snowflake_id:parse_list(Ids)].

-spec parse_rpc_snowflake(term()) -> integer() | undefined.
parse_rpc_snowflake(Value) ->
    try snowflake_id:parse_optional(Value) of
        Id -> Id
    catch
        error:{invalid_snowflake, _} -> undefined
    end.

-spec unique_preserve([integer()]) -> [integer()].
unique_preserve(List) ->
    {Result, _} = lists:foldl(
        fun(V, {Acc, Seen}) ->
            unique_preserve_value(V, Acc, Seen)
        end,
        {[], #{}},
        List
    ),
    lists:reverse(Result).

-spec unique_preserve_value(integer(), [integer()], map()) -> {[integer()], map()}.
unique_preserve_value(Value, Acc, Seen) ->
    case maps:is_key(Value, Seen) of
        true -> {Acc, Seen};
        false -> {[Value | Acc], Seen#{Value => true}}
    end.
