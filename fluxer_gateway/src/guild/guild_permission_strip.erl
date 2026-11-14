%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_permission_strip).
-typing([eqwalizer]).

-export([guild/1, roles/1, channels/1, channel_index/1]).

-spec guild(map() | term()) -> map().
guild(Guild) when is_map(Guild) ->
    case snowflake_id:parse_optional(maps:get(<<"owner_id">>, Guild, undefined)) of
        undefined -> #{};
        OwnerId -> #{<<"owner_id">> => OwnerId}
    end;
guild(_) ->
    #{}.

-spec roles(list() | term()) -> list().
roles(Roles) when is_list(Roles) ->
    [strip_role(Role) || Role <- Roles, is_map(Role)];
roles(_) ->
    [].

-spec strip_role(map()) -> map().
strip_role(Role) ->
    Keep = [<<"id">>, <<"permissions">>, <<"position">>],
    maps:with(Keep, normalize_role(Role)).

-spec channels(list() | term()) -> list().
channels(Channels) when is_list(Channels) ->
    [strip_channel(Channel) || Channel <- Channels, is_map(Channel)];
channels(_) ->
    [].

-spec strip_channel(map()) -> map().
strip_channel(Channel) ->
    Keep = [<<"id">>, <<"name">>, <<"type">>, <<"parent_id">>, <<"permission_overwrites">>],
    maps:with(Keep, normalize_channel(Channel)).

-spec channel_index(map() | term()) -> map().
channel_index(ChannelIndex) when is_map(ChannelIndex) ->
    maps:map(
        fun(_Id, Channel) when is_map(Channel) -> strip_channel(Channel) end, ChannelIndex
    );
channel_index(_) ->
    #{}.

-spec normalize_role(map()) -> map().
normalize_role(Role) ->
    case guild_data_normalize:role(Role) of
        Normalized when is_map(Normalized) -> Normalized;
        _ -> Role
    end.

-spec normalize_channel(map()) -> map().
normalize_channel(Channel) ->
    case guild_data_normalize:channel(Channel) of
        Normalized when is_map(Normalized) -> Normalized;
        _ -> Channel
    end.
