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

-module(presence_utils).

-export([
    collect_guild_member_presences/1,
    collect_guild_member_ids/1,
    filter_self_presence/2,
    is_visible_presence/1,
    batch_presences/1,
    send_presence_bulk/4
]).

-define(PRESENCE_BATCH_SIZE, 500).

-spec collect_guild_member_presences(map()) -> [map()].
collect_guild_member_presences(GuildState) ->
    MemberIds = collect_guild_member_ids(GuildState),
    case MemberIds of
        [] ->
            [];
        _ ->
            Presences = presence_cache:bulk_get(MemberIds),
            [P || P <- Presences, is_visible_presence(P)]
    end.

-spec collect_guild_member_ids(map()) -> [integer()].
collect_guild_member_ids(GuildState) ->
    Members = get_members_from_guild_state(GuildState),
    MemberIds = [member_user_id(M) || M <- Members],
    [Id || Id <- MemberIds, Id =/= undefined].

-spec filter_self_presence(integer(), [map()]) -> [map()].
filter_self_presence(UserId, Presences) ->
    [P || P <- Presences, presence_user_id(P) =/= UserId].

-spec is_visible_presence(map()) -> boolean().
is_visible_presence(Presence) ->
    Status = maps:get(<<"status">>, Presence, <<"offline">>),
    Status =/= <<"offline">> andalso Status =/= <<"invisible">>.

-spec batch_presences([map()]) -> [[map()]].
batch_presences([]) ->
    [];
batch_presences(Presences) ->
    batch_presences(Presences, []).

batch_presences([], Acc) ->
    lists:reverse(Acc);
batch_presences(Presences, Acc) ->
    {Batch, Rest} = take_batch(Presences, ?PRESENCE_BATCH_SIZE),
    batch_presences(Rest, [Batch | Acc]).

-spec send_presence_bulk(pid(), integer(), integer(), [map()]) -> ok.
send_presence_bulk(_Pid, _GuildId, _UserId, []) ->
    ok;
send_presence_bulk(Pid, GuildId, UserId, Presences) ->
    FilteredPresences = filter_self_presence(UserId, Presences),
    case FilteredPresences of
        [] ->
            ok;
        _ ->
            Batches = batch_presences(FilteredPresences),
            lists:foreach(
                fun(Batch) ->
                    BulkPayload = #{
                        <<"guild_id">> => integer_to_binary(GuildId),
                        <<"presences">> => Batch
                    },
                    gen_server:cast(Pid, {dispatch, presence_update_bulk, BulkPayload})
                end,
                Batches
            )
    end.

get_members_from_guild_state(GuildState) ->
    case maps:get(data, GuildState, undefined) of
        undefined ->
            map_utils:ensure_list(maps:get(<<"members">>, GuildState, []));
        Data ->
            map_utils:ensure_list(maps:get(<<"members">>, Data, []))
    end.

member_user_id(Member) ->
    User = maps:get(<<"user">>, Member, #{}),
    map_utils:get_integer(User, <<"id">>, undefined).

presence_user_id(P) when is_map(P) ->
    User = maps:get(<<"user">>, P, #{}),
    map_utils:get_integer(User, <<"id">>, undefined);
presence_user_id(_) ->
    undefined.

take_batch(List, N) when length(List) =< N ->
    {List, []};
take_batch(List, N) ->
    {lists:sublist(List, N), lists:nthtail(N, List)}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

batch_presences_empty_test() ->
    ?assertEqual([], batch_presences([])).

batch_presences_small_list_test() ->
    Presences = [#{<<"user">> => #{<<"id">> => I}} || I <- lists:seq(1, 10)],
    Batches = batch_presences(Presences),
    ?assertEqual(1, length(Batches)),
    ?assertEqual(10, length(hd(Batches))).

batch_presences_exact_batch_size_test() ->
    Presences = [#{<<"user">> => #{<<"id">> => I}} || I <- lists:seq(1, 500)],
    Batches = batch_presences(Presences),
    ?assertEqual(1, length(Batches)),
    ?assertEqual(500, length(hd(Batches))).

batch_presences_multiple_batches_test() ->
    Presences = [#{<<"user">> => #{<<"id">> => I}} || I <- lists:seq(1, 1250)],
    Batches = batch_presences(Presences),
    ?assertEqual(3, length(Batches)),
    ?assertEqual(500, length(lists:nth(1, Batches))),
    ?assertEqual(500, length(lists:nth(2, Batches))),
    ?assertEqual(250, length(lists:nth(3, Batches))).

filter_self_presence_test() ->
    Presences = [
        #{<<"user">> => #{<<"id">> => <<"1">>}},
        #{<<"user">> => #{<<"id">> => <<"2">>}},
        #{<<"user">> => #{<<"id">> => <<"3">>}}
    ],
    Filtered = filter_self_presence(2, Presences),
    ?assertEqual(2, length(Filtered)),
    ?assert(
        not lists:any(
            fun(P) -> presence_user_id(P) =:= 2 end,
            Filtered
        )
    ).

is_visible_presence_online_test() ->
    ?assert(is_visible_presence(#{<<"status">> => <<"online">>})),
    ?assert(is_visible_presence(#{<<"status">> => <<"idle">>})),
    ?assert(is_visible_presence(#{<<"status">> => <<"dnd">>})).

is_visible_presence_offline_test() ->
    ?assertNot(is_visible_presence(#{<<"status">> => <<"offline">>})),
    ?assertNot(is_visible_presence(#{<<"status">> => <<"invisible">>})),
    ?assertNot(is_visible_presence(#{})).

collect_guild_member_ids_internal_format_test() ->
    GuildState = #{
        data => #{
            <<"members">> => [
                #{<<"user">> => #{<<"id">> => <<"100">>}},
                #{<<"user">> => #{<<"id">> => <<"200">>}}
            ]
        }
    },
    Ids = collect_guild_member_ids(GuildState),
    ?assertEqual([100, 200], lists:sort(Ids)).

collect_guild_member_ids_external_format_test() ->
    GuildState = #{
        <<"members">> => [
            #{<<"user">> => #{<<"id">> => <<"100">>}},
            #{<<"user">> => #{<<"id">> => <<"200">>}}
        ]
    },
    Ids = collect_guild_member_ids(GuildState),
    ?assertEqual([100, 200], lists:sort(Ids)).

-endif.
