%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_request_router).
-typing([eqwalizer]).

-export([route_call/4, route_cast/3]).

-spec route_call(term(), term(), gen_server:from(), map()) ->
    {reply, term(), map()} | {noreply, map()} | {stop, term(), term(), map()}.
route_call(Tag, Msg, From, State) ->
    case query_call_handler(Tag) of
        query -> guild_query_handler:handle_call(Msg, From, State);
        voice -> guild_voice_handler:handle_call(Msg, From, State);
        subscription -> guild_subscription_handler:handle_call(Msg, From, State);
        undefined -> {reply, ok, State}
    end.

-spec query_call_handler(term()) -> query | voice | subscription | undefined.
query_call_handler(resolve_mention_sources_page) -> query;
query_call_handler(get_counts) -> query;
query_call_handler(get_viewer_counts) -> query;
query_call_handler(get_channel_member_counts) -> query;
query_call_handler(get_forum_unread_scope) -> query;
query_call_handler(get_large_guild_metadata) -> query;
query_call_handler(resolve_channel_mentions) -> query;
query_call_handler(get_members_with_role) -> query;
query_call_handler(check_permission) -> query;
query_call_handler(get_user_permissions) -> query;
query_call_handler(can_manage_roles) -> query;
query_call_handler(can_manage_role) -> query;
query_call_handler(get_guild_data) -> query;
query_call_handler(get_guild_auth_context) -> query;
query_call_handler(check_target_member) -> query;
query_call_handler(get_viewable_channels) -> query;
query_call_handler(get_guild_member) -> query;
query_call_handler(get_guild_members_batch) -> query;
query_call_handler(Tag) -> query_call_handler_more(Tag).

-spec query_call_handler_more(term()) -> query | voice | subscription | undefined.
query_call_handler_more(has_member) -> query;
query_call_handler_more(list_guild_members) -> query;
query_call_handler_more(search_guild_members) -> query;
query_call_handler_more(list_guild_members_cursor) -> query;
query_call_handler_more(get_first_viewable_text_channel) -> query;
query_call_handler_more(get_category_channel_count) -> query;
query_call_handler_more(get_channel_count) -> query;
query_call_handler_more(get_sessions) -> query;
query_call_handler_more(get_push_base_state) -> query;
query_call_handler_more(get_cluster_merge_state) -> query;
query_call_handler_more(Tag) -> voice_call_handler(Tag).

-spec voice_call_handler(term()) -> voice | subscription | undefined.
voice_call_handler(voice_state_update) -> voice;
voice_call_handler(get_voice_state) -> voice;
voice_call_handler(update_member_voice) -> voice;
voice_call_handler(disconnect_voice_user) -> voice;
voice_call_handler(disconnect_voice_user_if_in_channel) -> voice;
voice_call_handler(disconnect_all_voice_users_in_channel) -> voice;
voice_call_handler(confirm_voice_connection_from_livekit) -> voice;
voice_call_handler(move_member) -> voice;
voice_call_handler(switch_voice_region) -> voice;
voice_call_handler(add_virtual_channel_access) -> voice;
voice_call_handler(store_pending_connection) -> voice;
voice_call_handler(get_voice_states_for_channel) -> voice;
voice_call_handler(Tag) -> subscription_call_handler(Tag).

-spec subscription_call_handler(term()) -> subscription | undefined.
subscription_call_handler(lazy_subscribe) -> subscription;
subscription_call_handler(_) -> undefined.

-spec route_cast(term(), term(), map()) -> {noreply, map()}.
route_cast(Tag, Msg, State) ->
    case cast_handler(Tag) of
        voice -> guild_voice_handler:handle_cast(Msg, State);
        subscription -> guild_subscription_handler:handle_cast(Msg, State);
        dm_partners -> guild_dm_partners:handle_cast(Msg, State);
        undefined -> {noreply, State}
    end.

-spec cast_handler(term()) -> voice | subscription | dm_partners | undefined.
cast_handler(relay_voice_state_update) -> voice;
cast_handler(relay_voice_server_update) -> voice;
cast_handler(store_pending_connection) -> voice;
cast_handler(add_virtual_channel_access) -> voice;
cast_handler(remove_virtual_channel_access) -> voice;
cast_handler(cleanup_virtual_access_for_user) -> voice;
cast_handler(voice_signal) -> voice;
cast_handler(update_member_subscriptions) -> subscription;
cast_handler(update_dm_partners) -> dm_partners;
cast_handler(_) -> undefined.
