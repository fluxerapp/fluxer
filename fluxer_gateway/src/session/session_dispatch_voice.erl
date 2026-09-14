%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_dispatch_voice).
-typing([eqwalizer]).

-export([
    should_buffer_reaction/2,
    buffer_reaction/2,
    maybe_cancel_buffered_reaction/3,
    flush_reaction_buffer/2
]).

-export_type([session_state/0, event/0]).

-define(REACTION_BUFFER_INTERVAL_MS, 650).
-define(MAX_REACTION_BUFFER_SIZE, 512).

-type session_state() :: session:session_state().
-type event() :: atom() | binary().

-spec should_buffer_reaction(event(), session_state()) -> boolean().
should_buffer_reaction(message_reaction_add, State) ->
    maps:get(debounce_reactions, State, false);
should_buffer_reaction(_, _) ->
    false.

-spec buffer_reaction(map(), session_state()) -> session_state().
buffer_reaction(Data, State) ->
    Buffer = ensure_queue(maps:get(reaction_buffer, State, [])),
    Trimmed = trim_queue(Buffer, ?MAX_REACTION_BUFFER_SIZE - 1),
    NewBuffer = queue:in(Data, Trimmed),
    Timer = maps:get(reaction_buffer_timer, State, undefined),
    NewTimer =
        case Timer of
            undefined ->
                erlang:send_after(?REACTION_BUFFER_INTERVAL_MS, self(), flush_reaction_buffer);
            Existing ->
                Existing
        end,
    State#{reaction_buffer => NewBuffer, reaction_buffer_timer => NewTimer}.

-spec maybe_cancel_buffered_reaction(event(), map() | list(), session_state()) ->
    {cancelled, session_state()} | not_applicable.
maybe_cancel_buffered_reaction(message_reaction_remove, Data, State) ->
    BufferQ = ensure_queue(maps:get(reaction_buffer, State, [])),
    case queue:is_empty(BufferQ) of
        true ->
            not_applicable;
        false ->
            try_cancel_from_buffer(BufferQ, Data, State)
    end;
maybe_cancel_buffered_reaction(_, _, _) ->
    not_applicable.

-spec try_cancel_from_buffer(queue:queue(map()), map(), session_state()) ->
    {cancelled, session_state()} | not_applicable.
try_cancel_from_buffer(BufferQ, Data, State) ->
    BufferList = queue:to_list(BufferQ),
    MessageId = maps:get(<<"message_id">>, Data, undefined),
    UserId = maps:get(<<"user_id">>, Data, undefined),
    Emoji = maps:get(<<"emoji">>, Data, #{}),
    case remove_matching_reaction(BufferList, MessageId, UserId, Emoji) of
        {found, NewBufferList} ->
            {cancelled, State#{reaction_buffer => queue:from_list(NewBufferList)}};
        not_found ->
            not_applicable
    end.

-spec remove_matching_reaction([map()], term(), term(), map()) ->
    {found, [map()]} | not_found.
remove_matching_reaction(Buffer, MessageId, UserId, Emoji) ->
    EmojiId = maps:get(<<"id">>, Emoji, undefined),
    EmojiName = maps:get(<<"name">>, Emoji, undefined),
    remove_matching_reaction_loop(Buffer, MessageId, UserId, EmojiId, EmojiName, []).

-spec remove_matching_reaction_loop([map()], term(), term(), term(), term(), [map()]) ->
    {found, [map()]} | not_found.
remove_matching_reaction_loop([], _MessageId, _UserId, _EmojiId, _EmojiName, _Acc) ->
    not_found;
remove_matching_reaction_loop([Entry | Rest], MessageId, UserId, EmojiId, EmojiName, Acc) ->
    case matches_reaction(Entry, MessageId, UserId, EmojiId, EmojiName) of
        true ->
            {found, lists:reverse(Acc) ++ Rest};
        false ->
            remove_matching_reaction_loop(
                Rest,
                MessageId,
                UserId,
                EmojiId,
                EmojiName,
                [Entry | Acc]
            )
    end.

-spec matches_reaction(map(), term(), term(), term(), term()) -> boolean().
matches_reaction(Entry, MessageId, UserId, EmojiId, EmojiName) ->
    EntryEmoji = maps:get(<<"emoji">>, Entry, #{}),
    maps:get(<<"message_id">>, Entry, undefined) =:= MessageId andalso
        maps:get(<<"user_id">>, Entry, undefined) =:= UserId andalso
        maps:get(<<"id">>, EntryEmoji, undefined) =:= EmojiId andalso
        maps:get(<<"name">>, EntryEmoji, undefined) =:= EmojiName.

-spec flush_reaction_buffer(
    fun((atom(), map(), session_state()) -> {noreply, session_state()}),
    session_state()
) -> session_state().
flush_reaction_buffer(DispatchFun, State) ->
    BufferQ = ensure_queue(maps:get(reaction_buffer, State, [])),
    BufferList = queue:to_list(BufferQ),
    Timer = maps:get(reaction_buffer_timer, State, undefined),
    _ =
        case Timer of
            undefined -> ok;
            _ -> _ = erlang:cancel_timer(Timer)
        end,
    StateCleared = State#{reaction_buffer => queue:new(), reaction_buffer_timer => undefined},
    lists:foldl(
        fun(Group, Acc) -> dispatch_reaction_group(Group, DispatchFun, Acc) end,
        StateCleared,
        group_reactions_by_message(BufferList)
    ).

-spec group_reactions_by_message([map()]) -> [[map()]].
group_reactions_by_message(Buffer) ->
    {Keys, Groups} = lists:foldl(fun collect_reaction_group/2, {[], #{}}, Buffer),
    [lists:reverse(maps:get(Key, Groups)) || Key <- lists:reverse(Keys)].

-spec collect_reaction_group(map(), {[term()], #{term() => [map()]}}) ->
    {[term()], #{term() => [map()]}}.
collect_reaction_group(Entry, {Keys, Groups}) ->
    Key = reaction_group_key(Entry),
    case maps:is_key(Key, Groups) of
        true -> {Keys, Groups#{Key := [Entry | maps:get(Key, Groups)]}};
        false -> {[Key | Keys], Groups#{Key => [Entry]}}
    end.

-spec reaction_group_key(map()) -> term().
reaction_group_key(Entry) ->
    {
        maps:get(<<"guild_id">>, Entry, undefined),
        maps:get(<<"channel_id">>, Entry, undefined),
        maps:get(<<"message_id">>, Entry, undefined)
    }.

-spec dispatch_reaction_group(
    [map()],
    fun((atom(), map(), session_state()) -> {noreply, session_state()}),
    session_state()
) -> session_state().
dispatch_reaction_group([Single], DispatchFun, State) ->
    {noreply, FinalState} = DispatchFun(message_reaction_add, Single, State),
    FinalState;
dispatch_reaction_group(Group, DispatchFun, State) ->
    dispatch_reaction_add_many(Group, DispatchFun, State).

-spec dispatch_reaction_add_many(
    [map()],
    fun((atom(), map(), session_state()) -> {noreply, session_state()}),
    session_state()
) -> session_state().
dispatch_reaction_add_many(Buffer, DispatchFun, State) ->
    First = hd(Buffer),
    ChannelId = maps:get(<<"channel_id">>, First, undefined),
    MessageId = maps:get(<<"message_id">>, First, undefined),
    GuildId = maps:get(<<"guild_id">>, First, undefined),
    Reactions = build_reactions(Buffer),
    Data0 = #{
        <<"channel_id">> => ChannelId,
        <<"message_id">> => MessageId,
        <<"reactions">> => Reactions
    },
    Data = add_guild_id(GuildId, Data0),
    {noreply, FinalState} = DispatchFun(message_reaction_add_many, Data, State),
    FinalState.

-spec build_reactions([map()]) -> [map()].
build_reactions(Buffer) ->
    lists:map(fun build_single_reaction/1, Buffer).

-spec build_single_reaction(map()) -> map().
build_single_reaction(Entry) ->
    Base = #{
        <<"user_id">> => maps:get(<<"user_id">>, Entry, undefined),
        <<"emoji">> => maps:get(<<"emoji">>, Entry, #{})
    },
    case maps:get(<<"member">>, Entry, undefined) of
        undefined -> Base;
        null -> Base;
        Member -> Base#{<<"member">> => Member}
    end.

-spec add_guild_id(term(), map()) -> map().
add_guild_id(undefined, Data) -> Data;
add_guild_id(null, Data) -> Data;
add_guild_id(GuildId, Data) -> Data#{<<"guild_id">> => GuildId}.

-spec ensure_queue(queue:queue(T) | [T]) -> queue:queue(T).
ensure_queue(List) when is_list(List) -> queue:from_list(List);
ensure_queue(Q) -> Q.

-spec trim_queue(queue:queue(T), non_neg_integer()) -> queue:queue(T).
trim_queue(Q, MaxLen) ->
    case queue:len(Q) > MaxLen of
        true -> trim_queue(queue:drop(Q), MaxLen);
        false -> Q
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

reaction_entry(ChannelId, MessageId, UserId) ->
    #{
        <<"channel_id">> => ChannelId,
        <<"message_id">> => MessageId,
        <<"user_id">> => UserId,
        <<"emoji">> => #{<<"id">> => null, <<"name">> => <<"thumbsup">>}
    }.

record_reaction_dispatch(Event, Data, State) ->
    {noreply, State#{dispatched => [{Event, Data} | maps:get(dispatched, State, [])]}}.

summarize_reaction_dispatch({message_reaction_add_many, Data}) ->
    {message_reaction_add_many, maps:get(<<"message_id">>, Data), [
        maps:get(<<"user_id">>, Reaction)
     || Reaction <- maps:get(<<"reactions">>, Data)
    ]};
summarize_reaction_dispatch({Event, Data}) ->
    {Event, maps:get(<<"message_id">>, Data), [maps:get(<<"user_id">>, Data)]}.

flush_reaction_buffer_groups_by_message_test() ->
    Buffer = [
        reaction_entry(<<"10">>, <<"100">>, <<"1">>),
        reaction_entry(<<"10">>, <<"200">>, <<"2">>),
        reaction_entry(<<"10">>, <<"100">>, <<"3">>)
    ],
    State = #{reaction_buffer => queue:from_list(Buffer), reaction_buffer_timer => undefined},
    Final = flush_reaction_buffer(fun record_reaction_dispatch/3, State),
    Dispatched = lists:reverse(maps:get(dispatched, Final, [])),
    ?assertEqual(
        [
            {message_reaction_add_many, <<"100">>, [<<"1">>, <<"3">>]},
            {message_reaction_add, <<"200">>, [<<"2">>]}
        ],
        [summarize_reaction_dispatch(Entry) || Entry <- Dispatched]
    ).

flush_reaction_buffer_single_entry_dispatches_add_test() ->
    Entry = reaction_entry(<<"10">>, <<"100">>, <<"1">>),
    State = #{reaction_buffer => queue:from_list([Entry]), reaction_buffer_timer => undefined},
    Final = flush_reaction_buffer(fun record_reaction_dispatch/3, State),
    ?assertEqual([{message_reaction_add, Entry}], maps:get(dispatched, Final, [])).

-endif.
