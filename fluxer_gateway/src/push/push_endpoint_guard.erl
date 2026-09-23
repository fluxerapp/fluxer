%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_endpoint_guard).
-typing([eqwalizer]).

-export([check/1, check/2]).

-export_type([resolver/0]).

-define(RESOLVE_TIMEOUT_MS, 3000).
-define(MAX_HOST_LENGTH, 253).
-define(MAX_LABEL_LENGTH, 63).

-type resolver() :: fun((string()) -> {ok, [inet:ip_address()]} | {error, term()}).

-spec check(binary()) -> ok | {error, term()}.
check(Endpoint) ->
    check(Endpoint, fun resolve/1).

-spec check(binary(), resolver()) -> ok | {error, term()}.
check(Endpoint, Resolver) ->
    case parse_endpoint(Endpoint) of
        {ok, Host} -> check_host(Host, Resolver);
        {error, Reason} -> {error, Reason}
    end.

-spec parse_endpoint(binary()) -> {ok, string()} | {error, term()}.
parse_endpoint(Endpoint) when is_binary(Endpoint) ->
    case safe_parse(Endpoint) of
        {ok, Parsed} -> validate_parsed(Parsed);
        error -> {error, endpoint_rejected}
    end;
parse_endpoint(_Endpoint) ->
    {error, endpoint_rejected}.

-spec safe_parse(binary()) -> {ok, map()} | error.
safe_parse(Endpoint) ->
    try uri_string:parse(binary_to_list(Endpoint)) of
        Parsed when is_map(Parsed) -> {ok, Parsed};
        _ -> error
    catch
        _:_ -> error
    end.

-spec validate_parsed(map()) -> {ok, string()} | {error, term()}.
validate_parsed(Parsed) ->
    Scheme = to_lower(to_string(maps:get(scheme, Parsed, ""))),
    Host = to_lower(to_string(maps:get(host, Parsed, ""))),
    Port = maps:get(port, Parsed, undefined),
    HasUserinfo = maps:is_key(userinfo, Parsed),
    Allowed =
        Scheme =:= "https" andalso
            not HasUserinfo andalso
            allowed_port(Port) andalso
            Host =/= "",
    case Allowed of
        true -> {ok, Host};
        false -> {error, endpoint_rejected}
    end.

-spec allowed_port(term()) -> boolean().
allowed_port(undefined) -> true;
allowed_port(80) -> true;
allowed_port(443) -> true;
allowed_port(_Port) -> false.

-spec check_host(string(), resolver()) -> ok | {error, term()}.
check_host(Host, Resolver) ->
    case inet:parse_address(Host) of
        {ok, Address} -> check_addresses([Address]);
        {error, _Reason} -> check_hostname(Host, Resolver)
    end.

-spec check_hostname(string(), resolver()) -> ok | {error, term()}.
check_hostname(Host, Resolver) ->
    case is_fqdn(Host) of
        true -> resolve_and_check(Host, Resolver);
        false -> {error, endpoint_rejected}
    end.

-spec resolve_and_check(string(), resolver()) -> ok | {error, term()}.
resolve_and_check(Host, Resolver) ->
    case Resolver(Host) of
        {ok, Addresses} -> check_addresses(Addresses);
        {error, Reason} -> {error, Reason}
    end.

-spec resolve(string()) -> {ok, [inet:ip_address()]} | {error, term()}.
resolve(Host) ->
    merge_addresses(
        inet:getaddrs(Host, inet, ?RESOLVE_TIMEOUT_MS),
        inet:getaddrs(Host, inet6, ?RESOLVE_TIMEOUT_MS)
    ).

-spec merge_addresses(
    {ok, [inet:ip_address()]} | {error, term()},
    {ok, [inet:ip_address()]} | {error, term()}
) -> {ok, [inet:ip_address()]} | {error, term()}.
merge_addresses({ok, V4}, {ok, V6}) -> {ok, V4 ++ V6};
merge_addresses({ok, V4}, {error, _Reason}) -> {ok, V4};
merge_addresses({error, _Reason}, {ok, V6}) -> {ok, V6};
merge_addresses({error, Reason}, {error, _Other}) -> {error, Reason}.

-spec check_addresses([inet:ip_address()]) -> ok | {error, term()}.
check_addresses([]) ->
    {error, nxdomain};
check_addresses(Addresses) ->
    case lists:all(fun address_allowed/1, Addresses) of
        true -> ok;
        false -> {error, endpoint_blocked}
    end.

-spec address_allowed(inet:ip_address()) -> boolean().
address_allowed({_, _, _, _} = Address) ->
    not blocked_v4(Address);
address_allowed({_, _, _, _, _, _, _, _} = Address) ->
    case embedded_v4(Address) of
        {ok, Embedded} -> not blocked_v4(Embedded);
        none -> not blocked_v6(Address)
    end.

-spec blocked_v4(inet:ip4_address()) -> boolean().
blocked_v4(Address) ->
    Value = v4_value(Address),
    lists:any(
        fun({Network, Prefix}) ->
            masked(Value, Prefix, 32) =:= masked(v4_value(Network), Prefix, 32)
        end,
        blocked_v4_ranges()
    ).

-spec blocked_v6(inet:ip6_address()) -> boolean().
blocked_v6(Address) ->
    Value = v6_value(Address),
    lists:any(
        fun({Network, Prefix}) ->
            masked(Value, Prefix, 128) =:= masked(v6_value(Network), Prefix, 128)
        end,
        blocked_v6_ranges()
    ).

-spec blocked_v4_ranges() -> [{inet:ip4_address(), non_neg_integer()}].
blocked_v4_ranges() ->
    [
        {{0, 0, 0, 0}, 8},
        {{10, 0, 0, 0}, 8},
        {{100, 64, 0, 0}, 10},
        {{127, 0, 0, 0}, 8},
        {{169, 254, 0, 0}, 16},
        {{172, 16, 0, 0}, 12},
        {{192, 0, 0, 0}, 24},
        {{192, 0, 2, 0}, 24},
        {{192, 88, 99, 0}, 24},
        {{192, 168, 0, 0}, 16},
        {{198, 18, 0, 0}, 15},
        {{198, 51, 100, 0}, 24},
        {{203, 0, 113, 0}, 24},
        {{224, 0, 0, 0}, 4},
        {{240, 0, 0, 0}, 4}
    ].

-spec blocked_v6_ranges() -> [{inet:ip6_address(), non_neg_integer()}].
blocked_v6_ranges() ->
    [
        {{0, 0, 0, 0, 0, 0, 0, 0}, 128},
        {{0, 0, 0, 0, 0, 0, 0, 1}, 128},
        {{16#2001, 16#0db8, 0, 0, 0, 0, 0, 0}, 32},
        {{16#fc00, 0, 0, 0, 0, 0, 0, 0}, 7},
        {{16#fe80, 0, 0, 0, 0, 0, 0, 0}, 10},
        {{16#ff00, 0, 0, 0, 0, 0, 0, 0}, 8}
    ].

-spec embedded_v4(inet:ip6_address()) -> {ok, inet:ip4_address()} | none.
embedded_v4({0, 0, 0, 0, 0, 16#ffff, High, Low}) -> {ok, quad(High, Low)};
embedded_v4({16#0064, 16#ff9b, 0, 0, 0, 0, High, Low}) -> {ok, quad(High, Low)};
embedded_v4({0, 0, 0, 0, 0, 0, High, Low}) -> {ok, quad(High, Low)};
embedded_v4({16#2002, High, Low, _, _, _, _, _}) -> {ok, quad(High, Low)};
embedded_v4(_Address) -> none.

-spec quad(non_neg_integer(), non_neg_integer()) -> inet:ip4_address().
quad(High, Low) ->
    {High bsr 8, High band 16#ff, Low bsr 8, Low band 16#ff}.

-spec v4_value(inet:ip4_address()) -> non_neg_integer().
v4_value({A, B, C, D}) ->
    (A bsl 24) bor (B bsl 16) bor (C bsl 8) bor D.

-spec v6_value(inet:ip6_address()) -> non_neg_integer().
v6_value({A, B, C, D, E, F, G, H}) ->
    lists:foldl(fun(Word, Acc) -> (Acc bsl 16) bor Word end, 0, [A, B, C, D, E, F, G, H]).

-spec masked(non_neg_integer(), non_neg_integer(), pos_integer()) -> non_neg_integer().
masked(Value, Prefix, Bits) ->
    Value band (((1 bsl Prefix) - 1) bsl (Bits - Prefix)).

-spec is_fqdn(string()) -> boolean().
is_fqdn(Host0) ->
    Host = strip_trailing_dot(Host0),
    Labels = split_labels(Host),
    Host =/= "" andalso
        length(Host) =< ?MAX_HOST_LENGTH andalso
        length(Labels) > 1 andalso
        lists:all(fun is_label/1, Labels) andalso
        not is_all_digits(lists:last(Labels)).

-spec strip_trailing_dot(string()) -> string().
strip_trailing_dot(Host) ->
    case lists:reverse(Host) of
        [$. | Rest] -> lists:reverse(Rest);
        _ -> Host
    end.

-spec split_labels(string()) -> [string()].
split_labels(Host) ->
    split_labels(Host, [], []).

-spec split_labels(string(), string(), [string()]) -> [string()].
split_labels([], Current, Acc) ->
    lists:reverse([lists:reverse(Current) | Acc]);
split_labels([$. | Rest], Current, Acc) ->
    split_labels(Rest, [], [lists:reverse(Current) | Acc]);
split_labels([Char | Rest], Current, Acc) ->
    split_labels(Rest, [Char | Current], Acc).

-spec is_label(string()) -> boolean().
is_label([]) ->
    false;
is_label(Label) when length(Label) > ?MAX_LABEL_LENGTH ->
    false;
is_label(Label) ->
    lists:all(fun is_label_char/1, Label) andalso
        is_alphanumeric(hd(Label)) andalso
        is_alphanumeric(lists:last(Label)).

-spec is_label_char(char()) -> boolean().
is_label_char($-) -> true;
is_label_char(Char) -> is_alphanumeric(Char).

-spec is_alphanumeric(char()) -> boolean().
is_alphanumeric(Char) when Char >= $a, Char =< $z -> true;
is_alphanumeric(Char) when Char >= $0, Char =< $9 -> true;
is_alphanumeric(_Char) -> false.

-spec is_all_digits(string()) -> boolean().
is_all_digits("") -> false;
is_all_digits(Label) -> lists:all(fun(Char) -> Char >= $0 andalso Char =< $9 end, Label).

-spec to_lower(string()) -> string().
to_lower(Value) ->
    [lower_char(Char) || Char <- Value].

-spec lower_char(char()) -> char().
lower_char(Char) when Char >= $A, Char =< $Z -> Char + 32;
lower_char(Char) -> Char.

-spec to_string(term()) -> string().
to_string(Value) when is_list(Value) -> Value;
to_string(Value) when is_binary(Value) -> binary_to_list(Value);
to_string(_Value) -> "".
