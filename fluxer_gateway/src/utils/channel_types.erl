%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(channel_types).
-typing([eqwalizer]).

-export([is_category/1, is_public_thread/1, is_thread_only/1]).

-define(GUILD_CATEGORY, 4).
-define(ANNOUNCEMENT_THREAD, 10).
-define(PUBLIC_THREAD, 11).
-define(GUILD_FORUM, 15).
-define(GUILD_MEDIA, 16).

-spec is_category(term()) -> boolean().
is_category(Type) ->
    Type =:= ?GUILD_CATEGORY.

-spec is_public_thread(term()) -> boolean().
is_public_thread(Type) ->
    Type =:= ?ANNOUNCEMENT_THREAD orelse Type =:= ?PUBLIC_THREAD.

-spec is_thread_only(term()) -> boolean().
is_thread_only(Type) ->
    Type =:= ?GUILD_FORUM orelse Type =:= ?GUILD_MEDIA.
