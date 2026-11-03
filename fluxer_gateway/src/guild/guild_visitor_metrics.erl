%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(guild_visitor_metrics).
-typing([eqwalizer]).

-export([
    init/0,
    admitted/0,
    rejected/1,
    evicted/2,
    converted/0,
    session_added/0,
    session_removed/1,
    counts/0,
    reject_reasons/0,
    evict_reasons/0
]).

-define(KEY, {?MODULE, counters}).
-define(REJECT_REASONS, [disabled, age_restricted, unavailable, busy]).
-define(EVICT_REASONS, [busy, banned, disabled, age_restricted, removed, error]).

-spec init() -> ok.
init() ->
    case persistent_term:get(?KEY, undefined) of
        undefined ->
            persistent_term:put(?KEY, counters:new(length(keys()), [write_concurrency]));
        _ ->
            ok
    end.

-spec admitted() -> ok.
admitted() ->
    add(admitted, 1).

-spec rejected(atom()) -> ok.
rejected(Reason) ->
    add({rejected, Reason}, 1).

-spec evicted(atom(), non_neg_integer()) -> ok.
evicted(_Reason, 0) ->
    ok;
evicted(Reason, Count) ->
    add({evicted, Reason}, Count).

-spec converted() -> ok.
converted() ->
    add(converted, 1).

-spec session_added() -> ok.
session_added() ->
    add(live, 1).

-spec session_removed(non_neg_integer()) -> ok.
session_removed(0) ->
    ok;
session_removed(Count) ->
    add(live, -Count).

-spec reject_reasons() -> [atom()].
reject_reasons() ->
    ?REJECT_REASONS.

-spec evict_reasons() -> [atom()].
evict_reasons() ->
    ?EVICT_REASONS.

-spec counts() -> #{term() => integer()}.
counts() ->
    Keys = keys(),
    case persistent_term:get(?KEY, undefined) of
        undefined ->
            maps:from_list([{Key, 0} || Key <- Keys]);
        Counters ->
            maps:from_list([
                {Key, counters:get(Counters, Index)}
             || {Key, Index} <- lists:zip(Keys, lists:seq(1, length(Keys)))
            ])
    end.

-spec keys() -> [term()].
keys() ->
    [admitted, converted, live] ++
        [{rejected, Reason} || Reason <- ?REJECT_REASONS] ++
        [{evicted, Reason} || Reason <- ?EVICT_REASONS].

-spec add(term(), integer()) -> ok.
add(Key, N) ->
    ok = init(),
    case index(Key, keys(), 1) of
        undefined ->
            ok;
        Index ->
            counters:add(persistent_term:get(?KEY), Index, N)
    end.

-spec index(term(), [term()], pos_integer()) -> pos_integer() | undefined.
index(Key, [Key | _], Index) -> Index;
index(Key, [_ | Rest], Index) -> index(Key, Rest, Index + 1);
index(_Key, [], _Index) -> undefined.
