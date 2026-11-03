%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_thread_backstop).
-typing([eqwalizer]).

-export([check/3, capable/1]).

-spec check(session_dispatch:event(), map() | list(), session_dispatch:session_state()) ->
    drop | {ok, map() | list()}.
check(Event, Data, State) ->
    Capable = capable(State),
    case {Capable, is_thread_event(Event), Data} of
        {false, true, _} ->
            drop;
        {_, _, #{<<"__thread_scoped">> := GuildId}} ->
            thread_scoped(Capable, GuildId, Data, State);
        {_, _, #{<<"__thread_unscoped">> := GuildId}} ->
            thread_unscoped(thread_viewer(Capable, GuildId, State), Data);
        _ ->
            {ok, Data}
    end.

-spec capable(session_dispatch:session_state()) -> boolean().
capable(#{bot := true}) -> true;
capable(#{thread_channels_capable := true}) -> true;
capable(_State) -> false.

-spec thread_scoped(boolean(), term(), map(), session_dispatch:session_state()) ->
    drop | {ok, map()}.
thread_scoped(false, _GuildId, _Data, _State) ->
    drop;
thread_scoped(true, GuildId, Data, State) ->
    case thread_viewer(true, GuildId, State) of
        true -> {ok, maps:remove(<<"__thread_scoped">>, Data)};
        false -> drop
    end.

-spec thread_unscoped(boolean(), map()) -> drop | {ok, map()}.
thread_unscoped(true, _Data) ->
    drop;
thread_unscoped(false, Data) ->
    {ok, maps:remove(<<"__thread_unscoped">>, Data)}.

-spec thread_viewer(boolean(), term(), session_dispatch:session_state()) -> boolean().
thread_viewer(false, _GuildId, _State) ->
    false;
thread_viewer(true, GuildId, State) ->
    UserId = maps:get(user_id, State, undefined),
    case {channel_threads_config:loaded(), snowflake_id:parse_maybe(GuildId)} of
        {false, _} ->
            true;
        {true, G} when is_integer(G), is_integer(UserId) ->
            guild_thread_gate:compute_viewer(
                channel_threads_config:guild_active(G),
                type_conv:is_true(maps:get(bot, State, false)),
                true,
                UserId
            );
        _ ->
            false
    end.

-spec is_thread_event(session_dispatch:event()) -> boolean().
is_thread_event(thread_create) -> true;
is_thread_event(thread_update) -> true;
is_thread_event(thread_delete) -> true;
is_thread_event(thread_list_sync) -> true;
is_thread_event(thread_member_update) -> true;
is_thread_event(thread_members_update) -> true;
is_thread_event(thread_member_list_update) -> true;
is_thread_event(forum_unreads) -> true;
is_thread_event(<<"THREAD_", _/binary>>) -> true;
is_thread_event(<<"FORUM_UNREADS">>) -> true;
is_thread_event(_) -> false.
