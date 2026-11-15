%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_visitor_state).
-typing([eqwalizer]).

-export([new/0, get/1, put/2]).
-export_type([state/0]).

-type state() :: #{
    sessions := #{session:session_id() => guild_visitors:visitor_session()},
    refs := #{reference() => session:session_id()},
    count := non_neg_integer(),
    by_user := #{session:user_id() => pos_integer()},
    by_ip := #{binary() => pos_integer()},
    focus := #{integer() => #{session:session_id() => true}},
    views := #{{boolean(), boolean()} => #{integer() => true}}
}.

-spec new() -> state().
new() ->
    #{
        sessions => #{},
        refs => #{},
        count => 0,
        by_user => #{},
        by_ip => #{},
        focus => #{},
        views => #{}
    }.

-spec get(guild_data:guild_state()) -> state().
get(#{visitors := Visitors}) when is_map(Visitors) ->
    Visitors.

-spec put(state(), guild_data:guild_state()) -> guild_data:guild_state().
put(Visitors, State) ->
    State#{visitors => Visitors}.
