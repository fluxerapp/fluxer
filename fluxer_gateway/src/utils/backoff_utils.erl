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

-module(backoff_utils).

-export([
    calculate/1,
    calculate/2
]).

-spec calculate(non_neg_integer()) -> non_neg_integer().
calculate(Attempt) ->
    calculate(Attempt, 30000).

-spec calculate(non_neg_integer(), pos_integer()) -> non_neg_integer().
calculate(Attempt, MaxMs) ->
    BackoffMs = round(1000 * math:pow(2, Attempt)),
    min(BackoffMs, MaxMs).
