%% Copyright (C) 2026 Fluxer Contributors
%%
%% This file is part of Fluxer.
%%
%% Fluxer is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Affero General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%%
%% Fluxer is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Affero General Public License for more details.
%%
%% You should have received a copy of the GNU Affero General Public License
%% along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

-module(metrics_client).

-export([
    counter/1,
    counter/2,
    gauge/2,
    gauge/3,
    histogram/2,
    histogram/3,
    crash/2,
    batch/1,
    is_enabled/0
]).

-spec counter(binary()) -> ok.
counter(Name) ->
    counter(Name, #{}).

-spec counter(binary(), map()) -> ok.
counter(Name, Dimensions) ->
    fire_and_forget(<<"/metrics/counter">>, #{
        <<"name">> => Name,
        <<"dimensions">> => Dimensions,
        <<"value">> => 1
    }).

-spec gauge(binary(), number()) -> ok.
gauge(Name, Value) ->
    gauge(Name, #{}, Value).

-spec gauge(binary(), map(), number()) -> ok.
gauge(Name, Dimensions, Value) ->
    fire_and_forget(<<"/metrics/gauge">>, #{
        <<"name">> => Name,
        <<"dimensions">> => Dimensions,
        <<"value">> => Value
    }).

-spec histogram(binary(), number()) -> ok.
histogram(Name, ValueMs) ->
    histogram(Name, #{}, ValueMs).

-spec histogram(binary(), map(), number()) -> ok.
histogram(Name, Dimensions, ValueMs) ->
    fire_and_forget(<<"/metrics/histogram">>, #{
        <<"name">> => Name,
        <<"dimensions">> => Dimensions,
        <<"value_ms">> => ValueMs
    }).

-spec crash(binary(), binary()) -> ok.
crash(GuildId, Stacktrace) ->
    fire_and_forget(<<"/metrics/crash">>, #{
        <<"guild_id">> => GuildId,
        <<"stacktrace">> => Stacktrace
    }).

-spec is_enabled() -> boolean().
is_enabled() ->
    case metrics_host() of
        Host when is_list(Host), Host =/= "" -> true;
        Host when is_binary(Host), byte_size(Host) > 0 -> true;
        _ -> false
    end.

fire_and_forget(Path, Body) ->
    case metrics_host() of
        Host when is_list(Host), Host =/= "" ->
            spawn(fun() -> do_send(Host, Path, Body) end),
            ok;
        Host when is_binary(Host), byte_size(Host) > 0 ->
            spawn(fun() -> do_send(Host, Path, Body) end),
            ok;
        _ ->
            ok
    end.

do_send(Host, Path, Body) ->
    do_send(Host, Path, Body, 0).

do_send(Host, Path, Body, Attempt) ->
    Url = iolist_to_binary(["http://", Host, Path]),
    Headers = [{<<"Content-Type">>, <<"application/json">>}],
    JsonBody = jsx:encode(Body),
    MaxRetries = 1,

    case
        hackney:request(post, Url, Headers, JsonBody, [
            {recv_timeout, 5000}, {connect_timeout, 2000}
        ])
    of
        {ok, StatusCode, _RespHeaders, ClientRef} when StatusCode >= 200, StatusCode < 300 ->
            hackney:skip_body(ClientRef),
            ok;
        {ok, StatusCode, _RespHeaders, ClientRef} ->
            hackney:skip_body(ClientRef),
            case Attempt < MaxRetries of
                true ->
                    do_send(Host, Path, Body, Attempt + 1);
                false ->
                    logger:warning("Failed to send metric after ~p attempts: ~p ~s", [Attempt + 1, StatusCode, Path]),
                    ok
            end;
        {error, Reason} ->
            case Attempt < MaxRetries of
                true ->
                    do_send(Host, Path, Body, Attempt + 1);
                false ->
                    logger:warning("Failed to send metric after ~p attempts: ~p ~s", [Attempt + 1, Reason, Path]),
                    ok
            end
    end.

-spec batch([map()]) -> ok.
batch(Metrics) when is_list(Metrics) ->
    case metrics_host() of
        Host when is_list(Host), Host =/= "" ->
            spawn(fun() -> do_batch(Host, Metrics) end),
            ok;
        Host when is_binary(Host), byte_size(Host) > 0 ->
            spawn(fun() -> do_batch(Host, Metrics) end),
            ok;
        _ ->
            ok
    end.

do_batch(Host, Metrics) ->
    Gauges = [format_gauge(M) || M <- Metrics, maps:get(type, M, undefined) =:= gauge],
    Counters = [format_counter(M) || M <- Metrics, maps:get(type, M, undefined) =:= counter],
    Histograms = [format_histogram(M) || M <- Metrics, maps:get(type, M, undefined) =:= histogram],
    Body = #{
        <<"gauges">> => Gauges,
        <<"counters">> => Counters,
        <<"histograms">> => Histograms
    },
    do_send(Host, <<"/metrics/batch">>, Body).

format_gauge(#{name := Name, dimensions := Dims, value := Value}) ->
    #{<<"name">> => Name, <<"dimensions">> => Dims, <<"value">> => Value}.

format_counter(#{name := Name, dimensions := Dims, value := Value}) ->
    #{<<"name">> => Name, <<"dimensions">> => Dims, <<"value">> => Value}.

format_histogram(#{name := Name, dimensions := Dims, value := Value}) ->
    #{<<"name">> => Name, <<"dimensions">> => Dims, <<"value_ms">> => Value}.

metrics_host() ->
    fluxer_gateway_env:get(metrics_host).
