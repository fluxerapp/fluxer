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

-module(user_utils).

-export([normalize_user/1]).

normalize_user(User) when is_map(User) ->
    AllowedKeys = [
        <<"id">>,
        <<"username">>,
        <<"discriminator">>,
        <<"global_name">>,
        <<"avatar">>,
        <<"avatar_color">>,
        <<"bot">>,
        <<"system">>,
        <<"flags">>,
        <<"banner">>,
        <<"banner_color">>
    ],
    CleanPairs =
        lists:foldl(
            fun(Key, Acc) ->
                Value = maps:get(Key, User, undefined),
                case is_undefined(Value) of
                    true -> Acc;
                    false -> [{Key, Value} | Acc]
                end
            end,
            [],
            AllowedKeys
        ),
    maps:from_list(lists:reverse(CleanPairs));
normalize_user(_) ->
    #{}.

is_undefined(undefined) -> true;
is_undefined(_) -> false.
