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

-ifndef(TIMEOUT_CONFIG_HRL).
-define(TIMEOUT_CONFIG_HRL, true).

-define(RELOAD_TIMEOUT, 10000).
-define(STATS_TIMEOUT, 1000).
-define(RPC_TIMEOUT, 15000).
-define(GUILD_CALL_TIMEOUT, 10000).
-define(SESSION_CALL_TIMEOUT, 5000).
-define(SHUTDOWN_TIMEOUT, 10000).
-define(DEFAULT_GEN_SERVER_TIMEOUT, 5000).

-endif.
