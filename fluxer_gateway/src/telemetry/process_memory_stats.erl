%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(process_memory_stats).
-typing([eqwalizer]).

-export([get_guild_memory_stats/1]).

-export_type([guild_stats/0]).

-define(DEFAULT_LIMIT, 20).
-define(MAX_LIMIT, 100).
-define(MAX_STATE_FETCH, 200).
-define(MAX_PROCESS_SCAN, 5000).
-define(MAX_CACHED_GUILD_SCAN, 5000).

-type candidate_info() :: #{pid := pid(), memory := non_neg_integer()}.
-type guild_stats() :: #{
    guild_id := binary() | null,
    guild_name := binary(),
    guild_icon := binary() | null,
    memory := non_neg_integer(),
    member_count := non_neg_integer(),
    session_count := non_neg_integer(),
    presence_count := non_neg_integer()
}.

-spec get_guild_memory_stats(pos_integer() | term()) -> [guild_stats()].
get_guild_memory_stats(Limit0) ->
    Limit = clamp_limit(Limit0),
    Candidates = ranked_guild_process_candidates(),
    LimitedCandidates = limit_candidates_for_state_fetch(Candidates, Limit),
    GuildProcessInfos = lists:filtermap(fun get_guild_process_info/1, LimitedCandidates),
    Sorted = lists:sort(
        fun(#{memory := M1}, #{memory := M2}) -> M1 >= M2 end,
        GuildProcessInfos
    ),
    lists:sublist(Sorted, Limit).

-spec ranked_guild_process_candidates() -> [candidate_info()].
ranked_guild_process_candidates() ->
    case cached_guild_processes() of
        [] -> scan_guild_process_candidates();
        Pids -> cached_guild_process_candidates(Pids)
    end.

-spec cached_guild_processes() -> [pid()].
cached_guild_processes() ->
    try bounded_ets_rows(guild_pid_cache, ?MAX_CACHED_GUILD_SCAN) of
        Rows ->
            lists:usort([Pid || {_GuildId, Pid} <- Rows, is_pid(Pid), node(Pid) =:= node()])
    catch
        error:badarg -> []
    end.

-spec cached_guild_process_candidates([pid()]) -> [candidate_info()].
cached_guild_process_candidates(Pids) ->
    sort_candidates(lists:filtermap(fun cached_guild_process_candidate/1, Pids)).

-spec cached_guild_process_candidate(pid()) -> {true, candidate_info()} | false.
cached_guild_process_candidate(Pid) ->
    case erlang:process_info(Pid, memory) of
        {memory, Memory} when is_integer(Memory), Memory >= 0 ->
            {true, #{pid => Pid, memory => Memory}};
        _ ->
            false
    end.

-spec scan_guild_process_candidates() -> [candidate_info()].
scan_guild_process_candidates() ->
    sort_candidates(
        lists:filtermap(fun scanned_guild_process_candidate/1, process_scan_pids())
    ).

-spec process_scan_pids() -> [pid()].
process_scan_pids() ->
    lists:sublist(erlang:processes(), ?MAX_PROCESS_SCAN).

-spec bounded_ets_rows(ets:table(), pos_integer()) -> [term()].
bounded_ets_rows(Table, Limit) ->
    MatchSpec = [{{'$1', '$2'}, [], [{{'$1', '$2'}}]}],
    case ets:select(Table, MatchSpec, Limit) of
        {Rows, _Continuation} -> Rows;
        '$end_of_table' -> []
    end.

-spec scanned_guild_process_candidate(pid()) -> {true, candidate_info()} | false.
scanned_guild_process_candidate(Pid) ->
    case erlang:process_info(Pid, [memory, initial_call, dictionary]) of
        undefined ->
            false;
        InfoList ->
            scanned_guild_process_info(Pid, InfoList)
    end.

-spec scanned_guild_process_info(pid(), list()) -> {true, candidate_info()} | false.
scanned_guild_process_info(Pid, InfoList) ->
    Memory = proplists:get_value(memory, InfoList, 0),
    InitialCall = proplists:get_value(initial_call, InfoList),
    Dictionary = proplists:get_value(dictionary, InfoList, []),
    scanned_guild_module_candidate(Pid, Memory, extract_module(InitialCall, Dictionary)).

-spec scanned_guild_module_candidate(pid(), term(), atom() | undefined) ->
    {true, candidate_info()} | false.
scanned_guild_module_candidate(Pid, Memory, guild) when is_integer(Memory), Memory >= 0 ->
    {true, #{pid => Pid, memory => Memory}};
scanned_guild_module_candidate(_Pid, _Memory, _Module) ->
    false.

-spec sort_candidates([candidate_info()]) -> [candidate_info()].
sort_candidates(Candidates) ->
    lists:sort(
        fun(#{memory := Left}, #{memory := Right}) -> Left >= Right end,
        Candidates
    ).

-spec limit_candidates_for_state_fetch([candidate_info()], pos_integer()) -> [candidate_info()].
limit_candidates_for_state_fetch(Candidates, Limit) ->
    lists:sublist(Candidates, state_fetch_limit(Limit)).

-spec state_fetch_limit(term()) -> pos_integer().
state_fetch_limit(Limit0) ->
    Limit = clamp_limit(Limit0),
    min(
        ?MAX_STATE_FETCH,
        case Limit > 50 of
            true -> Limit + 50;
            false -> Limit + Limit
        end
    ).

-spec clamp_limit(term()) -> pos_integer().
clamp_limit(Limit) when is_integer(Limit), Limit > 0, Limit =< ?MAX_LIMIT ->
    Limit;
clamp_limit(Limit) when is_integer(Limit), Limit > ?MAX_LIMIT ->
    ?MAX_LIMIT;
clamp_limit(_) ->
    ?DEFAULT_LIMIT.

-spec get_guild_process_info(candidate_info()) -> {true, guild_stats()} | false.
get_guild_process_info(#{pid := Pid, memory := Memory}) ->
    case extract_guild_stats(Pid, Memory) of
        undefined -> false;
        Info -> {true, Info}
    end.

-spec extract_module(mfa() | undefined, list()) -> atom() | undefined.
extract_module(InitialCall, Dictionary) ->
    case lists:keyfind('$initial_call', 1, Dictionary) of
        {'$initial_call', {M, _, _}} -> eqwalizer:dynamic_cast(M);
        _ -> initial_call_module(InitialCall)
    end.

-spec initial_call_module(mfa() | undefined) -> atom() | undefined.
initial_call_module({M, _, _}) ->
    M;
initial_call_module(_) ->
    undefined.

-spec extract_guild_stats(pid(), non_neg_integer()) -> guild_stats() | undefined.
extract_guild_stats(Pid, Memory) ->
    try sys:get_state(Pid, 100) of
        State when is_map(State) ->
            guild_stats_from_state(State, Memory);
        _ ->
            undefined
    catch
        throw:State when is_map(State) -> guild_stats_from_state(State, Memory);
        throw:_ -> undefined;
        error:_ -> undefined;
        exit:_ -> undefined
    end.

-spec guild_stats_from_state(map(), non_neg_integer()) -> guild_stats().
guild_stats_from_state(State, Memory) ->
    GuildId = maps:get(id, State, undefined),
    Data = maps:get(data, State, #{}),
    Guild = maps:get(<<"guild">>, Data, #{}),
    GuildName = maps:get(<<"name">>, Guild, <<"Unknown">>),
    GuildIcon = maps:get(<<"icon">>, Guild, null),
    MemberCount = guild_data_index_members:member_count(Data),
    SessionCount = map_size(maps:get(sessions, State, #{})),
    PresenceCount = map_size(maps:get(presences, State, #{})),
    #{
        guild_id =>
            case GuildId of
                undefined -> null;
                Id -> integer_to_binary(Id)
            end,
        guild_name => GuildName,
        guild_icon => GuildIcon,
        memory => Memory,
        member_count => MemberCount,
        session_count => SessionCount,
        presence_count => PresenceCount
    }.
