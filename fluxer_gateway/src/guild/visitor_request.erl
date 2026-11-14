%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(visitor_request).
-typing([eqwalizer]).

-export([tier/1, ip_key/1]).

-spec tier(term()) -> binary().
tier(<<"verified">>) -> <<"verified">>;
tier(<<"unverified">>) -> <<"unverified">>;
tier(_) -> <<"unclaimed">>.

-spec ip_key(term()) -> binary() | undefined.
ip_key(Key) when is_binary(Key), byte_size(Key) > 0 -> Key;
ip_key(_) -> undefined.
