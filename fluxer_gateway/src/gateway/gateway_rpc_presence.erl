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

-module(gateway_rpc_presence).

-export([execute_method/2]).

execute_method(<<"presence.dispatch">>, #{
    <<"user_id">> := UserIdBin, <<"event">> := Event, <<"data">> := Data
}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    EventAtom = constants:dispatch_event_atom(Event),
    case presence_manager:dispatch_to_user(UserId, EventAtom, Data) of
        ok ->
            true;
        {error, not_found} ->
            handle_offline_dispatch(EventAtom, UserId, Data)
    end;
execute_method(<<"presence.join_guild">>, #{
    <<"user_id">> := UserIdBin, <<"guild_id">> := GuildIdBin
}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(presence_manager, {lookup, UserId}, 10000) of
        {ok, Pid} ->
            case gen_server:call(Pid, {join_guild, GuildId}, 10000) of
                ok -> true;
                _ -> throw({error, <<"Join guild failed">>})
            end;
        not_found ->
            true;
        {error, _} ->
            true;
        _ ->
            true
    end;
execute_method(<<"presence.leave_guild">>, #{
    <<"user_id">> := UserIdBin, <<"guild_id">> := GuildIdBin
}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(presence_manager, {lookup, UserId}, 10000) of
        {ok, Pid} ->
            case gen_server:call(Pid, {leave_guild, GuildId}, 10000) of
                ok -> true;
                _ -> throw({error, <<"Leave guild failed">>})
            end;
        not_found ->
            true;
        {error, _} ->
            true;
        _ ->
            true
    end;
execute_method(<<"presence.terminate_sessions">>, #{
    <<"user_id">> := UserIdBin, <<"session_id_hashes">> := SessionIdHashes
}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    case gen_server:call(presence_manager, {lookup, UserId}, 10000) of
        {ok, Pid} ->
            case gen_server:call(Pid, {terminate_session, SessionIdHashes}, 10000) of
                ok -> true;
                _ -> throw({error, <<"Terminate session failed">>})
            end;
        not_found ->
            true;
        {error, _} ->
            true;
        _ ->
            true
    end;
execute_method(<<"presence.terminate_all_sessions">>, #{
    <<"user_id">> := UserIdBin
}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    case presence_manager:terminate_all_sessions(UserId) of
        ok -> true;
        _ -> throw({error, <<"Terminate all sessions failed">>})
    end;
execute_method(<<"presence.has_active">>, #{<<"user_id">> := UserIdBin}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    case gen_server:call(presence_manager, {lookup, UserId}, 10000) of
        {ok, _Pid} ->
            #{<<"has_active">> => true};
        _ ->
            #{<<"has_active">> => false}
    end;
execute_method(<<"presence.add_temporary_guild">>, #{
    <<"user_id">> := UserIdBin, <<"guild_id">> := GuildIdBin
}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(presence_manager, {lookup, UserId}, 10000) of
        {ok, Pid} ->
            case gen_server:call(Pid, {add_temporary_guild, GuildId}, 10000) of
                ok -> true;
                _ -> throw({error, <<"Add temporary guild failed">>})
            end;
        not_found ->
            true;
        {error, _} ->
            true;
        _ ->
            true
    end;
execute_method(<<"presence.remove_temporary_guild">>, #{
    <<"user_id">> := UserIdBin, <<"guild_id">> := GuildIdBin
}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    GuildId = validation:snowflake_or_throw(<<"guild_id">>, GuildIdBin),
    case gen_server:call(presence_manager, {lookup, UserId}, 10000) of
        {ok, Pid} ->
            case gen_server:call(Pid, {remove_temporary_guild, GuildId}, 10000) of
                ok -> true;
                _ -> throw({error, <<"Remove temporary guild failed">>})
            end;
        not_found ->
            true;
        {error, _} ->
            true;
        _ ->
            true
    end;
execute_method(<<"presence.sync_group_dm_recipients">>, #{
    <<"user_id">> := UserIdBin, <<"recipients_by_channel">> := RecipientsByChannel
}) ->
    UserId = validation:snowflake_or_throw(<<"user_id">>, UserIdBin),
    NormalizedRecipients =
        maps:from_list([
            {
                validation:snowflake_or_throw(<<"channel_id">>, ChannelIdBin),
                [validation:snowflake_or_throw(<<"recipient_id">>, RBin) || RBin <- Recipients]
            }
         || {ChannelIdBin, Recipients} <- maps:to_list(RecipientsByChannel)
        ]),
    case gen_server:call(presence_manager, {lookup, UserId}, 10000) of
        {ok, Pid} ->
            gen_server:cast(Pid, {sync_group_dm_recipients, NormalizedRecipients}),
            true;
        not_found ->
            true;
        {error, _} ->
            true;
        _ ->
            true
    end.

handle_offline_dispatch(message_create, UserId, Data) ->
    AuthorIdBin = maps:get(<<"id">>, maps:get(<<"author">>, Data, #{}), <<"0">>),
    AuthorId = validation:snowflake_or_throw(<<"author_id">>, AuthorIdBin),
    push:handle_message_create(#{
        message_data => Data,
        user_ids => [UserId],
        guild_id => 0,
        author_id => AuthorId
    }),
    true;
handle_offline_dispatch(relationship_add, UserId, _Data) ->
    sync_blocked_ids_for_user(UserId),
    true;
handle_offline_dispatch(relationship_remove, UserId, _Data) ->
    sync_blocked_ids_for_user(UserId),
    true;
handle_offline_dispatch(_Event, _UserId, _Data) ->
    true.

sync_blocked_ids_for_user(_UserId) ->
    ok.
