%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_build_info).
-typing([eqwalizer]).

-export([set_version_header/1, version/0, version_headers/1]).

-define(VERSION_HEADER, <<"x-fluxer-version">>).

-spec version() -> binary().
version() ->
    case fluxer_gateway_config:env_value("BUILD_VERSION") of
        undefined -> <<"dev">>;
        Value -> ensure_binary(Value)
    end.

-spec version_headers(#{binary() => binary()}) -> #{binary() => binary()}.
version_headers(Headers) ->
    Headers#{?VERSION_HEADER => version()}.

-spec set_version_header(cowboy_req:req()) -> cowboy_req:req().
set_version_header(Req) ->
    cowboy_req:set_resp_header(?VERSION_HEADER, version(), Req).

-spec ensure_binary(string()) -> binary().
ensure_binary(Value) ->
    type_conv:ensure_binary(Value, <<"dev">>).
