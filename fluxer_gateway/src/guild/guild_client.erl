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

-module(guild_client).

-export([
    voice_state_update/3
]).

-export_type([
    voice_state_update_success/0,
    voice_state_update_error/0,
    voice_state_update_result/0
]).

-define(DEFAULT_TIMEOUT, 12000).

-type voice_state_update_success() :: #{
    success := true,
    token => binary(),
    endpoint => binary(),
    connection_id => binary(),
    voice_state => map(),
    needs_token => boolean()
}.

-type voice_state_update_error() :: {error, atom(), atom()}.

-type voice_state_update_result() ::
    {ok, voice_state_update_success()}
    | {error, timeout}
    | {error, noproc}
    | {error, atom(), atom()}.

-spec voice_state_update(pid(), map(), timeout()) -> voice_state_update_result().
voice_state_update(GuildPid, Request, Timeout) ->
    try gen_server:call(GuildPid, {voice_state_update, Request}, Timeout) of
        Response when is_map(Response) ->
            case maps:get(success, Response, false) of
                true -> {ok, Response};
                false -> {error, unknown, internal_error}
            end;
        {error, Category, ErrorAtom} when is_atom(Category), is_atom(ErrorAtom) ->
            {error, Category, ErrorAtom}
    catch
        exit:{timeout, _} ->
            {error, timeout};
        exit:{noproc, _} ->
            {error, noproc};
        exit:{normal, _} ->
            {error, noproc}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

module_exports_test() ->
    Exports = guild_client:module_info(exports),
    ?assert(lists:member({voice_state_update, 3}, Exports)).

-endif.
