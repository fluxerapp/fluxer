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

-module(push_logger_filter).

-export([install_progress_filter/0]).

install_progress_filter() ->
    Filter = {fun logger_filters:progress/2, stop},
    case logger:add_handler_filter(default, push_progress_filter, Filter) of
        ok ->
            ok;
        {error, already_exists} ->
            ok;
        {error, Reason} ->
            logger:error(
                "[push] failed to install progress filter: ~p",
                [Reason]
            ),
            {error, Reason}
    end.
