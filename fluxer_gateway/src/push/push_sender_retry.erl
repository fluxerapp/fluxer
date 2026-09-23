%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_sender_retry).
-typing([eqwalizer]).

-export([
    maybe_retry_with_smaller_record_size/3,
    initial_record_size/0
]).

-export_type([push_response/0]).

-define(PUSH_RECORD_SIZE, 2816).
-define(CONSTRAINED_PUSH_RECORD_SIZE, 2048).
-define(MIN_PUSH_RECORD_SIZE, 1024).
-define(MAX_PAYLOAD_RETRY_ATTEMPTS, 2).

-type push_response() :: {ok, integer(), term(), binary()} | {error, term()}.

-spec initial_record_size() -> pos_integer().
initial_record_size() ->
    ?PUSH_RECORD_SIZE.

-spec maybe_retry_with_smaller_record_size(
    push_response(), pos_integer(), non_neg_integer()
) ->
    no_retry | {retry, pos_integer()}.
maybe_retry_with_smaller_record_size(_Response, _CurrentRecordSize, Attempt) when
    Attempt >= ?MAX_PAYLOAD_RETRY_ATTEMPTS
->
    no_retry;
maybe_retry_with_smaller_record_size(
    {ok, 413, _ResponseHeaders, ResponseBody}, CurrentRecordSize, _Attempt
) ->
    case next_record_size_for_payload_too_large(CurrentRecordSize, ResponseBody) of
        undefined -> no_retry;
        NextRecordSize -> {retry, NextRecordSize}
    end;
maybe_retry_with_smaller_record_size(_Response, _CurrentRecordSize, _Attempt) ->
    no_retry.

-spec next_record_size_for_payload_too_large(pos_integer(), binary()) ->
    pos_integer() | undefined.
next_record_size_for_payload_too_large(CurrentRecordSize, ResponseBody) ->
    case parse_constrained_overage_bytes(ResponseBody) of
        OverageBytes when is_integer(OverageBytes), OverageBytes > 0 ->
            sanitize_next_record_size(CurrentRecordSize - OverageBytes, CurrentRecordSize);
        _ ->
            sanitize_next_record_size(?CONSTRAINED_PUSH_RECORD_SIZE, CurrentRecordSize)
    end.

-spec sanitize_next_record_size(integer(), pos_integer()) ->
    pos_integer() | undefined.
sanitize_next_record_size(CandidateRecordSize, CurrentRecordSize) when
    is_integer(CandidateRecordSize)
->
    ClampedRecordSize = erlang:max(?MIN_PUSH_RECORD_SIZE, CandidateRecordSize),
    case ClampedRecordSize < CurrentRecordSize of
        true -> ClampedRecordSize;
        false -> undefined
    end.

-spec parse_constrained_overage_bytes(binary()) -> non_neg_integer() | undefined.
parse_constrained_overage_bytes(ResponseBody) ->
    case decode_push_error_body(ResponseBody) of
        #{<<"message">> := Message} -> parse_constrained_overage_from_message(Message);
        _ -> undefined
    end.

-spec parse_constrained_overage_from_message(binary() | list()) ->
    non_neg_integer() | undefined.
parse_constrained_overage_from_message(Message) when is_list(Message) ->
    parse_constrained_overage_from_message(list_to_binary(Message));
parse_constrained_overage_from_message(Message) when is_binary(Message) ->
    case
        re:run(Message, <<"too long by ([0-9]+) bytes">>, [caseless, {capture, [1], binary}])
    of
        {match, [OverageBytesBin]} -> parse_non_neg_integer(OverageBytesBin);
        _ -> undefined
    end.

-spec decode_push_error_body(binary()) -> map() | undefined.
decode_push_error_body(ResponseBody) when
    is_binary(ResponseBody), byte_size(ResponseBody) > 0
->
    try json:decode(ResponseBody) of
        ParsedBody when is_map(ParsedBody) -> ParsedBody;
        _ -> undefined
    catch
        error:_ -> undefined;
        throw:_ -> undefined;
        exit:_ -> undefined
    end;
decode_push_error_body(_ResponseBody) ->
    undefined.

-spec parse_non_neg_integer(binary()) -> non_neg_integer() | undefined.
parse_non_neg_integer(Value) ->
    case guild_data_normalize_schema:int(Value) of
        ParsedValue when ParsedValue >= 0 -> ParsedValue;
        _ -> undefined
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

initial_record_size_is_shared_by_every_endpoint_test() ->
    ?assertEqual(2816, initial_record_size()).

next_record_size_for_payload_too_large_overage_test() ->
    ResponseBody = <<
        "{\"code\":413,\"errno\":104,\"error\":\"Payload Too Large\","
        "\"message\":\"This message is intended for a constrained device and is limited in size. "
        "Converted buffer is too long by 441 bytes\"}"
    >>,
    ?assertEqual(
        2375,
        next_record_size_for_payload_too_large(?PUSH_RECORD_SIZE, ResponseBody)
    ).

next_record_size_for_payload_too_large_fallback_test() ->
    ResponseBody = <<"{\"code\":413,\"errno\":104,\"error\":\"Payload Too Large\"}">>,
    ?assertEqual(
        ?CONSTRAINED_PUSH_RECORD_SIZE,
        next_record_size_for_payload_too_large(?PUSH_RECORD_SIZE, ResponseBody)
    ),
    ?assertEqual(
        undefined,
        next_record_size_for_payload_too_large(
            ?CONSTRAINED_PUSH_RECORD_SIZE, ResponseBody
        )
    ).

next_record_size_is_clamped_to_the_minimum_test() ->
    ResponseBody = <<
        "{\"code\":413,\"message\":\"Converted buffer is too long by 2000 bytes\"}"
    >>,
    ?assertEqual(
        ?MIN_PUSH_RECORD_SIZE,
        next_record_size_for_payload_too_large(?PUSH_RECORD_SIZE, ResponseBody)
    ).

maybe_retry_stops_after_the_attempt_cap_test() ->
    Response = {ok, 413, [], <<>>},
    ?assertEqual(
        {retry, ?CONSTRAINED_PUSH_RECORD_SIZE},
        maybe_retry_with_smaller_record_size(Response, ?PUSH_RECORD_SIZE, 0)
    ),
    ?assertEqual(
        no_retry,
        maybe_retry_with_smaller_record_size(
            Response, ?PUSH_RECORD_SIZE, ?MAX_PAYLOAD_RETRY_ATTEMPTS
        )
    ).

maybe_retry_ignores_non_413_responses_test() ->
    ?assertEqual(
        no_retry,
        maybe_retry_with_smaller_record_size({ok, 400, [], <<>>}, ?PUSH_RECORD_SIZE, 0)
    ),
    ?assertEqual(
        no_retry,
        maybe_retry_with_smaller_record_size({error, timeout}, ?PUSH_RECORD_SIZE, 0)
    ).

-endif.
