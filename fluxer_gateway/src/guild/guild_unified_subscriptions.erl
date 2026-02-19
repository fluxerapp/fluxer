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

-module(guild_unified_subscriptions).

-export([handle_subscriptions/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type session_state() :: map().
-type session_id() :: binary() | undefined.

-spec handle_subscriptions(map(), pid(), session_state()) -> ok.
handle_subscriptions(Data, SocketPid, SessionState) ->
    Subscriptions = maps:get(<<"subscriptions">>, Data, #{}),
    Guilds = maps:get(guilds, SessionState, #{}),
    SessionId = maps:get(id, SessionState, undefined),
    maps:foreach(
        fun(GuildIdBin, GuildSubData) ->
            process_guild_subscription(
                GuildIdBin, GuildSubData, Guilds, SessionId, SocketPid, SessionState
            )
        end,
        Subscriptions
    ),
    ok.

-spec process_guild_subscription(binary(), map(), map(), session_id(), pid(), session_state()) ->
    ok.
process_guild_subscription(GuildIdBin, GuildSubData, Guilds, SessionId, SocketPid, SessionState) ->
    case validation:validate_snowflake(<<"guild_id">>, GuildIdBin) of
        {ok, GuildId} ->
            case maps:get(GuildId, Guilds, undefined) of
                {GuildPid, _Ref} when is_pid(GuildPid) ->
                    process_guild_sub_options(
                        GuildId, GuildPid, GuildSubData, SessionId, SocketPid, SessionState
                    );
                undefined ->
                    ok;
                _ ->
                    ok
            end;
        {error, _, _} ->
            ok
    end.

-spec process_guild_sub_options(integer(), pid(), map(), session_id(), pid(), session_state()) ->
    ok.
process_guild_sub_options(GuildId, GuildPid, GuildSubData, SessionId, SocketPid, SessionState) ->
    WasActive = not session_passive:is_passive(GuildId, SessionState),
    ActiveChanged = process_active_flag(GuildSubData, GuildPid, SessionId, WasActive),
    process_sync_flag(GuildSubData, GuildId, GuildPid, SessionId, ActiveChanged),
    process_member_list_channels(GuildSubData, GuildId, GuildPid, SessionId, SocketPid),
    process_member_subscriptions(GuildSubData, GuildPid, SessionId),
    process_typing_flag(GuildSubData, GuildPid, SessionId),
    ok.

-spec process_active_flag(map(), pid(), session_id(), boolean()) -> boolean().
process_active_flag(GuildSubData, GuildPid, SessionId, WasActive) ->
    case maps:get(<<"active">>, GuildSubData, undefined) of
        undefined ->
            false;
        true ->
            gen_server:cast(GuildPid, {set_session_active, SessionId}),
            not WasActive;
        false ->
            gen_server:cast(GuildPid, {set_session_passive, SessionId}),
            WasActive
    end.

-spec process_sync_flag(map(), integer(), pid(), session_id(), boolean()) -> ok.
process_sync_flag(GuildSubData, _GuildId, GuildPid, SessionId, ActiveChanged) ->
    ShouldSync = maps:get(<<"sync">>, GuildSubData, false) =:= true orelse ActiveChanged,
    case ShouldSync of
        true ->
            gen_server:cast(GuildPid, {send_guild_sync, SessionId});
        false ->
            ok
    end.

-spec process_member_list_channels(map(), integer(), pid(), session_id(), pid()) -> ok.
process_member_list_channels(GuildSubData, GuildId, GuildPid, SessionId, SocketPid) ->
    case maps:get(<<"member_list_channels">>, GuildSubData, undefined) of
        undefined ->
            ok;
        MemberListChannels when is_map(MemberListChannels) ->
            maps:foreach(
                fun(ChannelIdBin, Ranges) ->
                    process_channel_lazy_subscribe(
                        ChannelIdBin, Ranges, GuildId, GuildPid, SessionId, SocketPid
                    )
                end,
                MemberListChannels
            );
        _ ->
            ok
    end.

-spec process_channel_lazy_subscribe(binary(), list(), integer(), pid(), session_id(), pid()) -> ok.
process_channel_lazy_subscribe(ChannelIdBin, Ranges, _GuildId, GuildPid, SessionId, _SocketPid) ->
    case validation:validate_snowflake(<<"channel_id">>, ChannelIdBin) of
        {ok, ChannelId} ->
            ParsedRanges = parse_ranges(Ranges),
            case
                gen_server:call(
                    GuildPid,
                    {lazy_subscribe, #{
                        session_id => SessionId,
                        channel_id => ChannelId,
                        ranges => ParsedRanges
                    }},
                    10000
                )
            of
                ok ->
                    ok;
                _Error ->
                    ok
            end;
        {error, _, _} ->
            ok
    end,
    ok.

-spec parse_ranges(list()) -> [{non_neg_integer(), non_neg_integer()}].
parse_ranges(Ranges) when is_list(Ranges) ->
    lists:filtermap(
        fun(Range) ->
            case Range of
                [Start, End] when is_integer(Start), is_integer(End), Start >= 0, End >= Start ->
                    {true, {Start, End}};
                _ ->
                    false
            end
        end,
        Ranges
    );
parse_ranges(_) ->
    [].

-spec process_member_subscriptions(map(), pid(), session_id()) -> ok.
process_member_subscriptions(GuildSubData, GuildPid, SessionId) ->
    case maps:get(<<"members">>, GuildSubData, undefined) of
        undefined ->
            ok;
        Members when is_list(Members) ->
            MemberIds = parse_member_ids(Members),
            gen_server:cast(GuildPid, {update_member_subscriptions, SessionId, MemberIds});
        _ ->
            ok
    end.

-spec parse_member_ids(list()) -> [integer()].
parse_member_ids(Members) when is_list(Members) ->
    lists:filtermap(
        fun(MemberIdRaw) ->
            case validation:validate_snowflake(<<"member_id">>, MemberIdRaw) of
                {ok, MemberId} -> {true, MemberId};
                {error, _, _} -> false
            end
        end,
        Members
    );
parse_member_ids(_) ->
    [].

-spec process_typing_flag(map(), pid(), session_id()) -> ok.
process_typing_flag(GuildSubData, GuildPid, SessionId) ->
    case maps:get(<<"typing">>, GuildSubData, undefined) of
        undefined ->
            ok;
        TypingFlag when is_boolean(TypingFlag) ->
            gen_server:cast(GuildPid, {set_session_typing_override, SessionId, TypingFlag});
        _ ->
            ok
    end.

-ifdef(TEST).

parse_ranges_valid_test() ->
    ?assertEqual([{0, 99}, {100, 199}], parse_ranges([[0, 99], [100, 199]])).

parse_ranges_invalid_test() ->
    ?assertEqual([], parse_ranges([[100, 50]])),
    ?assertEqual([], parse_ranges([[-1, 99]])),
    ?assertEqual([], parse_ranges([[<<"0">>, 99]])).

parse_ranges_mixed_test() ->
    ?assertEqual([{0, 99}], parse_ranges([[0, 99], [100, 50], <<"invalid">>])).

parse_ranges_non_list_test() ->
    ?assertEqual([], parse_ranges(undefined)),
    ?assertEqual([], parse_ranges(#{})).

parse_member_ids_valid_test() ->
    ?assertEqual([123, 456], parse_member_ids([<<"123">>, <<"456">>])).

parse_member_ids_invalid_test() ->
    ?assertEqual([], parse_member_ids([<<"not_a_number">>])).

parse_member_ids_mixed_test() ->
    ?assertEqual([123], parse_member_ids([<<"123">>, <<"invalid">>])).

parse_member_ids_non_list_test() ->
    ?assertEqual([], parse_member_ids(undefined)),
    ?assertEqual([], parse_member_ids(#{})).

-endif.
