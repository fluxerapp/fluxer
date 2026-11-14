%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(session_replay_buffer).
-typing([eqwalizer]).

-export([transfer_buffer/2, entry_payload_bytes/1]).

-define(MAX_TRANSFER_PRE_ENCODED_BYTES, 4194304).

-spec transfer_buffer(
    eqwalizer:dynamic(limited_deque:deque() | [map()]), session:seq()
) -> {[term()], session:seq()}.
transfer_buffer(Buffer, AckSeq) when is_list(Buffer) ->
    trim_transfer_list(Buffer, transfer_payload_bytes(Buffer), AckSeq);
transfer_buffer(Buffer, AckSeq) ->
    ListBuffer = limited_deque:to_list(Buffer),
    trim_transfer_list(ListBuffer, transfer_payload_bytes(ListBuffer), AckSeq).

-spec trim_transfer_list([term()], non_neg_integer(), session:seq()) ->
    {[term()], session:seq()}.
trim_transfer_list(Buffer, Bytes, AckSeq) when Bytes =< ?MAX_TRANSFER_PRE_ENCODED_BYTES ->
    {Buffer, AckSeq};
trim_transfer_list([], _Bytes, AckSeq) ->
    {[], AckSeq};
trim_transfer_list([Dropped | Rest], Bytes, AckSeq) ->
    trim_transfer_list(
        Rest,
        max(0, Bytes - entry_payload_bytes(Dropped)),
        dropped_ack_seq(Dropped, AckSeq)
    ).

-spec transfer_payload_bytes([term()]) -> non_neg_integer().
transfer_payload_bytes(Entries) ->
    lists:foldl(fun(Entry, Acc) -> trunc(Acc + entry_payload_bytes(Entry)) end, 0, Entries).

-spec entry_payload_bytes(term()) -> non_neg_integer().
entry_payload_bytes(#{data := {pre_encoded, Payload}}) when is_binary(Payload) ->
    byte_size(Payload);
entry_payload_bytes(_Entry) ->
    0.

-spec dropped_ack_seq(term(), session:seq()) -> session:seq().
dropped_ack_seq(#{seq := Seq}, AckSeq) when is_integer(Seq), Seq > AckSeq ->
    Seq;
dropped_ack_seq(_Dropped, AckSeq) ->
    AckSeq.
