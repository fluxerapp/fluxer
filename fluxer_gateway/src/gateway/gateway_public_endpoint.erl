%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_public_endpoint).
-typing([eqwalizer]).

-export([normalize/4]).

-spec normalize(binary(), binary() | undefined, binary() | undefined, integer() | undefined) ->
    binary().
normalize(Url, BaseDomain, _PublicScheme, PublicPort) when
    is_binary(Url), is_binary(BaseDomain), is_integer(PublicPort), PublicPort > 0
->
    case base_domain(BaseDomain) of
        <<>> -> Url;
        Domain -> insert_port(Url, Domain, PublicPort)
    end;
normalize(Url, _BaseDomain, _PublicScheme, _PublicPort) ->
    Url.

-spec base_domain(binary()) -> binary().
base_domain(BaseDomain) ->
    case string:trim(BaseDomain) of
        Trimmed when is_binary(Trimmed) -> trim_root_dot(Trimmed);
        _ -> <<>>
    end.

-spec insert_port(binary(), binary(), integer()) -> binary().
insert_port(Url, Domain, Port) ->
    case binary:split(Url, <<"://">>) of
        [Scheme, Rest] -> insert_authority_port(Url, Scheme, Rest, Domain, Port);
        [_] -> Url
    end.

-spec insert_authority_port(binary(), binary(), binary(), binary(), integer()) -> binary().
insert_authority_port(Url, _Scheme, <<"/", _/binary>>, _Domain, _Port) ->
    Url;
insert_authority_port(Url, Scheme, Rest, Domain, Port) ->
    {Authority, Tail} = split_authority(Rest),
    case insertable(Scheme, Authority, Domain, Port) of
        true -> join_authority_port(Scheme, Authority, Tail, Port);
        false -> Url
    end.

-spec insertable(binary(), binary(), binary(), integer()) -> boolean().
insertable(Scheme, Authority, Domain, Port) ->
    Host = after_last(Authority, <<"@">>),
    Default = default_port(Scheme),
    is_integer(Default) andalso Port =/= Default andalso not has_port(Host) andalso
        same_host(Host, Domain).

-spec join_authority_port(binary(), binary(), binary(), integer()) -> binary().
join_authority_port(Scheme, Authority, Tail, Port) ->
    PortBin = integer_to_binary(Port),
    <<Scheme/binary, "://", Authority/binary, ":", PortBin/binary, Tail/binary>>.

-spec split_authority(binary()) -> {binary(), binary()}.
split_authority(Rest) ->
    case binary:match(Rest, [<<"/">>, <<"\\">>, <<"?">>, <<"#">>]) of
        {Pos, _} -> split_binary(Rest, Pos);
        nomatch -> {Rest, <<>>}
    end.

-spec after_last(binary(), binary()) -> binary().
after_last(Bin, Separator) ->
    case binary:split(Bin, Separator) of
        [_, Rest] -> after_last(Rest, Separator);
        [Tail] -> Tail
    end.

-spec has_port(binary()) -> boolean().
has_port(Host) ->
    binary:match(after_last(Host, <<"]">>), <<":">>) =/= nomatch.

-spec same_host(binary(), binary()) -> boolean().
same_host(<<"[", _/binary>> = Host, Domain) ->
    string:equal(Host, Domain, true);
same_host(Host, Domain) ->
    string:equal(trim_root_dot(Host), Domain, true).

-spec trim_root_dot(binary()) -> binary().
trim_root_dot(<<>>) ->
    <<>>;
trim_root_dot(Host) ->
    case binary:last(Host) of
        $. -> binary:part(Host, 0, byte_size(Host) - 1);
        _ -> Host
    end.

-spec default_port(binary()) -> integer() | undefined.
default_port(Scheme) ->
    case string:lowercase(Scheme) of
        <<"http">> -> 80;
        <<"https">> -> 443;
        <<"ws">> -> 80;
        <<"wss">> -> 443;
        _ -> undefined
    end.
