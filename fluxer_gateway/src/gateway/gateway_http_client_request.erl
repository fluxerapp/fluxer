%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_http_client_request).
-typing([eqwalizer]).

-export([
    safe_do_request/6,
    extract_host_key/1,
    url_metadata/1,
    ensure_binary/1,
    ensure_list/1
]).
-export_type([workload/0, method/0, request_headers/0, request_options/0, response/0]).

-type workload() :: rpc | push.
-type method() :: get | post | put | patch | delete | head | options.
-type request_headers() :: [{binary() | string(), binary() | string()}].
-type request_options() :: #{
    connect_timeout => timeout(),
    recv_timeout => timeout(),
    max_concurrency => pos_integer(),
    failure_threshold => pos_integer(),
    recovery_timeout_ms => pos_integer(),
    content_type => binary() | string(),
    host_key => binary(),
    is_https => boolean()
}.
-type response() :: {ok, non_neg_integer(), [{binary(), binary()}], binary()} | {error, term()}.

-spec safe_do_request(
    workload(), method(), iodata(), request_headers(), iodata() | undefined, request_options()
) -> response().
safe_do_request(Workload, Method, Url, Headers, Body, Opts) ->
    try do_request(Workload, Method, Url, Headers, Body, Opts) of
        Result -> Result
    catch
        Class:Reason:Stacktrace ->
            logger:warning(
                "http client request_exception class=~p reason=~p frame=~p "
                "workload=~p method=~p host=~ts",
                [
                    Class,
                    Reason,
                    first_stack_frame(Stacktrace),
                    Workload,
                    Method,
                    extract_host_key(Url)
                ]
            ),
            {error, request_exception}
    end.

-spec extract_host_key(iodata()) -> binary().
extract_host_key(Url) ->
    UrlString = ensure_list(Url),
    try uri_string:parse(UrlString) of
        Parsed when is_map(Parsed) -> extract_host_from_parsed(Parsed);
        _ -> <<"unknown">>
    catch
        _:_ -> <<"unknown">>
    end.

-spec url_metadata(iodata()) -> {binary(), boolean()}.
url_metadata(Url) ->
    UrlString = ensure_list(Url),
    try uri_string:parse(UrlString) of
        Parsed when is_map(Parsed) ->
            {extract_host_from_parsed(Parsed), parsed_is_https(Parsed)};
        _ ->
            {<<"unknown">>, false}
    catch
        _:_ -> {<<"unknown">>, false}
    end.

-spec ensure_binary(term()) -> binary().
ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) -> iodata_to_binary_or_format(Value).

-spec ensure_list(term()) -> string().
ensure_list(Value) when is_binary(Value) -> binary_to_list(Value);
ensure_list(Value) when is_list(Value) -> characters_to_list_or_format(Value);
ensure_list(Value) when is_atom(Value) -> atom_to_list(Value);
ensure_list(Value) when is_integer(Value) -> integer_to_list(Value);
ensure_list(_Value) -> "".

-spec do_request(
    workload(), method(), iodata(), request_headers(), iodata() | undefined, request_options()
) -> response().
do_request(Workload, Method, Url, Headers, Body, Opts) ->
    HttpMethod = normalize_method(Method),
    UrlString = ensure_list(Url),
    RequestHeaders = normalize_request_headers(Headers),
    RequestTuple = build_request_tuple(UrlString, RequestHeaders, Body, Opts),
    HttpOptions = build_http_options(UrlString, Opts),
    RequestOptions = [{body_format, binary}],
    Profile = gateway_http_client:pick_sharded_profile(Workload),
    case httpc:request(HttpMethod, RequestTuple, HttpOptions, RequestOptions, Profile) of
        {ok, {{_HttpVersion, StatusCode, _ReasonPhrase}, RespHeaders, RespBody}} when
            is_integer(StatusCode)
        ->
            {ok, StatusCode, normalize_response_headers(RespHeaders), ensure_binary(RespBody)};
        {ok, _Other} ->
            {error, invalid_response};
        {error, Reason} ->
            {error, Reason}
    end.

-spec build_http_options(string(), request_options()) -> list().
build_http_options(UrlString, Opts) ->
    ConnectTimeout = maps:get(connect_timeout, Opts),
    RecvTimeout = maps:get(recv_timeout, Opts),
    BaseOptions = [
        {connect_timeout, ConnectTimeout},
        {timeout, RecvTimeout},
        {autoredirect, false}
    ],
    case is_https_request(UrlString, Opts) of
        true -> [{ssl, https_ssl_options()} | BaseOptions];
        false -> BaseOptions
    end.

-spec is_https_request(string(), request_options()) -> boolean().
is_https_request(UrlString, Opts) ->
    case maps:get(is_https, Opts, undefined) of
        IsHttps when is_boolean(IsHttps) -> IsHttps;
        _ -> is_https_url(UrlString)
    end.

-spec is_https_url(string()) -> boolean().
is_https_url(UrlString) ->
    parsed_is_https(uri_string:parse(UrlString)).

-spec parsed_is_https(term()) -> boolean().
parsed_is_https(#{scheme := Scheme}) -> string:lowercase(ensure_list(Scheme)) =:= "https";
parsed_is_https(_) -> false.

-spec https_ssl_options() -> list().
https_ssl_options() ->
    [
        {verify, verify_peer},
        {cacerts, https_cacerts()},
        {depth, 9},
        {customize_hostname_check, [
            {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
        ]}
    ].

-spec https_cacerts() -> list().
https_cacerts() ->
    case public_key:cacerts_get() of
        Certs when is_list(Certs), Certs =/= [] ->
            Certs;
        _ ->
            erlang:error(no_ca_store)
    end.

-spec normalize_method(method() | atom()) -> method().
normalize_method(post) -> post;
normalize_method(get) -> get;
normalize_method(put) -> put;
normalize_method(patch) -> patch;
normalize_method(delete) -> delete;
normalize_method(head) -> head;
normalize_method(options) -> options;
normalize_method(_) -> post.

-spec build_request_tuple(
    string(), [{string(), string()}], iodata() | undefined, request_options()
) ->
    {string(), [{string(), string()}]}
    | {string(), [{string(), string()}], string(), iodata()}.
build_request_tuple(Url, Headers, undefined, _Opts) ->
    {Url, Headers};
build_request_tuple(Url, Headers, Body, Opts) ->
    ContentType = resolve_content_type(Headers, Opts),
    {Url, Headers, ContentType, Body}.

-spec resolve_content_type([{string(), string()}], request_options()) -> string().
resolve_content_type(Headers, Opts) ->
    case maps:get(content_type, Opts, undefined) of
        undefined ->
            default_content_type(Headers);
        Value ->
            ensure_list(Value)
    end.

-spec default_content_type([{string(), string()}]) -> string().
default_content_type(Headers) ->
    case find_content_type_header(Headers) of
        undefined -> "application/json";
        Value -> Value
    end.

-spec find_content_type_header([{string(), string()}]) -> string() | undefined.
find_content_type_header([]) ->
    undefined;
find_content_type_header([{Name, Value} | Rest]) ->
    case string:lowercase(Name) of
        "content-type" -> Value;
        _ -> find_content_type_header(Rest)
    end.

-spec normalize_request_headers(request_headers()) -> [{string(), string()}].
normalize_request_headers(Headers) ->
    [{ensure_list(Name), ensure_list(Value)} || {Name, Value} <- Headers].

-spec normalize_response_headers(term()) -> [{binary(), binary()}].
normalize_response_headers(Headers) when is_list(Headers) ->
    lists:filtermap(fun normalize_response_header/1, Headers);
normalize_response_headers(_Headers) ->
    [].

-spec normalize_response_header(term()) -> {true, {binary(), binary()}} | false.
normalize_response_header({Name, Value}) ->
    {true, {ensure_binary(Name), ensure_binary(Value)}};
normalize_response_header(_Header) ->
    false.

-spec extract_host_from_parsed(map()) -> binary().
extract_host_from_parsed(Parsed) ->
    case maps:get(host, Parsed, undefined) of
        undefined -> <<"unknown">>;
        Host when is_binary(Host) -> normalize_host(Host);
        Host when is_list(Host) -> normalize_host(list_to_binary(Host));
        _ -> <<"unknown">>
    end.

-spec normalize_host(binary()) -> binary().
normalize_host(Host) ->
    list_to_binary(string:lowercase(binary_to_list(Host))).

-spec first_stack_frame(list()) -> term().
first_stack_frame([Frame | _]) -> Frame;
first_stack_frame([]) -> undefined.

-spec iodata_to_binary_or_format(term()) -> binary().
iodata_to_binary_or_format(Value) when is_binary(Value) ->
    Value;
iodata_to_binary_or_format(Value) ->
    iolist_to_binary(io_lib:format("~p", [Value])).

-spec characters_to_list_or_format([term()]) -> string().
characters_to_list_or_format(Value) ->
    case coerce_charlist(Value) of
        {ok, Chardata} ->
            unicode_to_list_or_format(Chardata, Value);
        error ->
            lists:flatten(io_lib:format("~p", [Value]))
    end.

-spec unicode_to_list_or_format(unicode:chardata(), [term()]) -> string().
unicode_to_list_or_format(Chardata, OrigValue) ->
    case unicode:characters_to_list(Chardata) of
        List when is_list(List) -> List;
        _ -> lists:flatten(io_lib:format("~p", [OrigValue]))
    end.

-spec coerce_charlist([term()]) -> {ok, unicode:chardata()} | error.
coerce_charlist([]) ->
    {ok, []};
coerce_charlist([H | _] = List) when is_integer(H); is_binary(H) ->
    coerce_charlist_all(List);
coerce_charlist(_) ->
    error.

-spec coerce_charlist_all([term()]) -> {ok, unicode:chardata()} | error.
coerce_charlist_all(List) ->
    AllValid = lists:all(fun is_char_element/1, List),
    case AllValid of
        true -> {ok, lists:map(fun coerce_char_element/1, List)};
        false -> error
    end.

-spec is_char_element(term()) -> boolean().
is_char_element(E) when is_integer(E) -> true;
is_char_element(E) when is_binary(E) -> true;
is_char_element(_) -> false.

-spec coerce_char_element(term()) -> char() | binary().
coerce_char_element(V) when is_integer(V) -> V;
coerce_char_element(V) when is_binary(V) -> V;
coerce_char_element(_) -> $?.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_https_url_true_for_https_test() ->
    ?assertEqual(true, is_https_url("https://fcm.googleapis.com/v1")),
    ?assertEqual(true, is_https_url("HTTPS://oauth2.googleapis.com/token")).

is_https_url_false_for_http_test() ->
    ?assertEqual(false, is_https_url("http://127.0.0.1:8088/rpc")),
    ?assertEqual(false, is_https_url("not-a-url")).

build_http_options_adds_ssl_for_https_test() ->
    case has_ca_store() of
        true ->
            Opts = #{connect_timeout => 3000, recv_timeout => 5000},
            Options = build_http_options("https://fcm.googleapis.com/v1", Opts),
            ?assertMatch([{ssl, _} | _], Options),
            {ssl, SslOpts} = lists:keyfind(ssl, 1, Options),
            ?assertEqual({verify, verify_peer}, lists:keyfind(verify, 1, SslOpts)),
            ?assertMatch({cacerts, [_ | _]}, lists:keyfind(cacerts, 1, SslOpts)),
            ?assertEqual({depth, 9}, lists:keyfind(depth, 1, SslOpts)),
            ?assertMatch(
                {customize_hostname_check, _},
                lists:keyfind(customize_hostname_check, 1, SslOpts)
            );
        false ->
            ok
    end.

has_ca_store() ->
    try public_key:cacerts_get() of
        Certs when is_list(Certs), Certs =/= [] -> true;
        _ -> false
    catch
        _:_ -> false
    end.

build_http_options_omits_ssl_for_http_test() ->
    Opts = #{connect_timeout => 5000, recv_timeout => 30000},
    Options = build_http_options("http://127.0.0.1:8088/rpc", Opts),
    ?assertEqual(false, lists:keyfind(ssl, 1, Options)),
    ?assertEqual({connect_timeout, 5000}, lists:keyfind(connect_timeout, 1, Options)),
    ?assertEqual({autoredirect, false}, lists:keyfind(autoredirect, 1, Options)).

memoisable_urls() ->
    [
        <<"http://fluxer-api.fluxer.svc.cluster.local/internal/rpc">>,
        <<"https://fcm.googleapis.com/v1/projects/fluxer/messages:send">>,
        <<"http://fluxer-api.fluxer.svc.cluster.local:8080/internal/rpc">>,
        <<"HTTPS://Fluxer-API.Fluxer.SVC.Cluster.Local:8443/internal/rpc">>
    ].

url_metadata_matches_derive_path_test_() ->
    [
        {binary_to_list(Url), fun() ->
            Derived = {extract_host_key(Url), is_https_url(ensure_list(Url))},
            ?assertEqual(Derived, url_metadata(Url))
        end}
     || Url <- memoisable_urls()
    ].

url_metadata_values_test() ->
    ?assertEqual(
        [
            {<<"fluxer-api.fluxer.svc.cluster.local">>, false},
            {<<"fcm.googleapis.com">>, true},
            {<<"fluxer-api.fluxer.svc.cluster.local">>, false},
            {<<"fluxer-api.fluxer.svc.cluster.local">>, true}
        ],
        [url_metadata(Url) || Url <- memoisable_urls()]
    ).

url_metadata_handles_unparsable_url_test() ->
    ?assertEqual({<<"unknown">>, false}, url_metadata(<<"not-a-url">>)).

build_http_options_uses_memoised_scheme_test() ->
    case has_ca_store() of
        true ->
            Opts = #{connect_timeout => 3000, recv_timeout => 5000, is_https => true},
            ?assertMatch([{ssl, _} | _], build_http_options("http://127.0.0.1:8088/rpc", Opts));
        false ->
            ok
    end.

build_http_options_memoised_http_skips_ssl_test() ->
    Opts = #{connect_timeout => 3000, recv_timeout => 5000, is_https => false},
    Options = build_http_options("https://fcm.googleapis.com/v1", Opts),
    ?assertEqual(false, lists:keyfind(ssl, 1, Options)).

-endif.
