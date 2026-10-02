%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_read_model).
-typing([eqwalizer]).

-export([put_state/1, update/2, delete/1, delete/2, query/2]).

-define(TABLE, guild_read_model).
-define(STATE_KEYS, [id, member_count, member_list_engine, virtual_channel_access]).
-define(DATA_KEYS, [
    <<"guild">>,
    <<"roles">>,
    <<"channels">>,
    <<"channel_index">>,
    <<"emojis">>,
    <<"stickers">>,
    role_perms_cache,
    overwrite_perms_cache,
    members_ets
]).

-spec put_state(map()) -> ok.
put_state(#{id := GuildId, data := Data} = State) when is_integer(GuildId), is_map(Data) ->
    case maps:get(disable_permission_cache_updates, State, false) of
        true -> ok;
        false -> store(GuildId, State, Data)
    end;
put_state(_) ->
    ok.

-spec store(integer(), map(), map()) -> ok.
store(GuildId, State, Data) ->
    ok = guild_ets_utils:ensure_table(?TABLE, [
        named_table, public, set, {read_concurrency, true}
    ]),
    ReadData = maps:with(?DATA_KEYS, Data),
    Members =
        case maps:get(members_ets, Data, undefined) of
            Tab when is_reference(Tab) -> #{};
            _ -> guild_data_index:member_map(Data)
        end,
    Snapshot = (maps:with(?STATE_KEYS, State))#{
        data => ReadData#{<<"members">> => Members, members_normalized => Members}
    },
    true = ets:insert(?TABLE, {GuildId, self(), Snapshot}),
    ok.

-spec update(map(), map()) -> ok.
update(OldState, NewState) ->
    Keys = [data | ?STATE_KEYS],
    case
        lists:all(
            fun(Key) ->
                erts_debug:same(
                    maps:get(Key, OldState, undefined), maps:get(Key, NewState, undefined)
                )
            end,
            Keys
        )
    of
        true -> ok;
        false -> put_state(NewState)
    end.

-spec delete(term()) -> ok.
delete(GuildId) ->
    delete(GuildId, self()).

-spec delete(term(), pid()) -> ok.
delete(GuildId, Owner) ->
    try ets:match_delete(?TABLE, {GuildId, Owner, '_'}) of
        true -> ok
    catch
        error:badarg -> ok
    end.

-spec query(integer(), {atom(), map()}) -> {ok, map()} | miss.
query(GuildId, Request) ->
    try
        case ets:lookup(?TABLE, GuildId) of
            [{GuildId, Owner, Snapshot}] ->
                case is_process_alive(Owner) of
                    true -> read(Request, Snapshot);
                    false -> miss
                end;
            [] ->
                miss
        end
    catch
        error:badarg -> miss;
        error:{badmatch, false} -> miss
    end.

-spec read({atom(), map()}, map()) -> {ok, map()} | miss.
read({Tag, Request}, Snapshot) ->
    State = materialize_member(maps:get(user_id, Request, null), Snapshot),
    case Tag of
        get_guild_data -> reply(guild_data:get_guild_data(Request, State));
        get_guild_auth_context -> reply(guild_data:get_auth_context(Request, State));
        get_guild_member -> reply(guild_data:get_guild_member(Request, State));
        has_member -> reply(guild_data:has_member(Request, State));
        _ -> miss
    end.

-spec materialize_member(integer() | null, map()) -> map().
materialize_member(UserId, #{data := #{members_ets := Tab} = Data} = Snapshot) ->
    MemberCount = ets:info(Tab, size),
    true = is_integer(MemberCount),
    Members =
        case ets:lookup(Tab, UserId) of
            [{UserId, Member}] -> #{UserId => Member};
            [] -> #{}
        end,
    Snapshot#{
        member_count => MemberCount,
        data => Data#{<<"members">> => Members, members_normalized => Members}
    };
materialize_member(_UserId, Snapshot) ->
    Snapshot.

-spec reply({reply, map(), map()}) -> {ok, map()}.
reply({reply, Reply, _State}) ->
    {ok, Reply}.
