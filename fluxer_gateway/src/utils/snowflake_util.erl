%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(snowflake_util).
-typing([eqwalizer]).

-export([extract_timestamp/1]).

-define(FLUXER_EPOCH, 1420070400000).
-define(TIMESTAMP_SHIFT, 22).

-spec extract_timestamp(term()) -> integer() | undefined.
extract_timestamp(SnowflakeValue) ->
    try snowflake_id:parse_optional(SnowflakeValue) of
        Snowflake when is_integer(Snowflake), Snowflake > 0 ->
            (Snowflake bsr ?TIMESTAMP_SHIFT) + ?FLUXER_EPOCH;
        undefined ->
            undefined
    catch
        error:{invalid_snowflake, _} -> undefined
    end.
