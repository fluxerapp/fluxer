%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(channel_threads_identify_counts).
-typing([eqwalizer]).

-export([ensure_counters/0, note/2, counts/0]).
-export_type([os_label/0]).

-define(IDENTIFY_COUNTS_KEY, {channel_threads_config, identify_counts}).

-type os_label() :: binary().

-spec ensure_counters() -> ok.
ensure_counters() ->
    case persistent_term:get(?IDENTIFY_COUNTS_KEY, undefined) of
        undefined ->
            Size = 2 * length(os_labels()),
            persistent_term:put(?IDENTIFY_COUNTS_KEY, counters:new(Size, [write_concurrency]));
        _Counters ->
            ok
    end.

-spec note(boolean(), map()) -> ok.
note(Capable, Properties) ->
    case persistent_term:get(?IDENTIFY_COUNTS_KEY, undefined) of
        undefined ->
            ok;
        Counters ->
            Os = os_label(maps:get(<<"os">>, Properties, undefined)),
            counters:add(Counters, identify_index(Capable, Os), 1)
    end.

-spec counts() -> #{{boolean(), os_label()} => non_neg_integer()}.
counts() ->
    case persistent_term:get(?IDENTIFY_COUNTS_KEY, undefined) of
        undefined ->
            #{};
        Counters ->
            maps:from_list([
                {{Capable, Os}, counters:get(Counters, identify_index(Capable, Os))}
             || Capable <- [true, false], Os <- os_labels()
            ])
    end.

-spec os_label(term()) -> os_label().
os_label(Os) when is_binary(Os) ->
    classify_os(string:lowercase(Os));
os_label(_Os) ->
    <<"other">>.

-spec classify_os(unicode:chardata()) -> os_label().
classify_os(Os) ->
    first_os_match(Os, [
        {<<"android">>, [<<"android">>]},
        {<<"ios">>, [<<"ios">>, <<"iphone">>, <<"ipad">>]},
        {<<"macos">>, [<<"mac">>, <<"darwin">>, <<"os x">>]},
        {<<"windows">>, [<<"win">>]},
        {<<"linux">>, [<<"linux">>, <<"bsd">>]}
    ]).

-spec first_os_match(unicode:chardata(), [{os_label(), [binary()]}]) -> os_label().
first_os_match(_Os, []) ->
    <<"other">>;
first_os_match(Os, [{Label, Needles} | Rest]) ->
    case matches_any(Os, Needles) of
        true -> Label;
        false -> first_os_match(Os, Rest)
    end.

-spec matches_any(unicode:chardata(), [binary()]) -> boolean().
matches_any(Os, Needles) ->
    lists:any(fun(Needle) -> string:find(Os, Needle) =/= nomatch end, Needles).

-spec os_labels() -> [os_label()].
os_labels() ->
    [<<"android">>, <<"ios">>, <<"windows">>, <<"macos">>, <<"linux">>, <<"other">>].

-spec identify_index(boolean(), os_label()) -> pos_integer().
identify_index(Capable, Os) ->
    CapableOffset =
        case Capable of
            true -> 0;
            false -> length(os_labels())
        end,
    CapableOffset + os_index(Os, os_labels(), 1).

-spec os_index(os_label(), [os_label()], pos_integer()) -> pos_integer().
os_index(Os, [Os | _Rest], Index) ->
    Index;
os_index(Os, [_Other | Rest], Index) ->
    os_index(Os, Rest, Index + 1);
os_index(_Os, [], Index) ->
    Index - 1.
