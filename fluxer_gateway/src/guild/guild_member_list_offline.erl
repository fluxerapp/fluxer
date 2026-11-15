%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_member_list_offline).
-typing([eqwalizer]).

-export([threshold/0]).

-define(OFFLINE_RENDER_THRESHOLD, 1000).

-spec threshold() -> pos_integer().
threshold() ->
    ?OFFLINE_RENDER_THRESHOLD.
