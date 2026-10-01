%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(gateway_broadcast).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(NATS_SUBJECT, <<"gateway.broadcast">>).
-define(NATS_SUBSCRIBE_RETRY_MS, 2000).

-type state() :: #{
    nats_subscription := term(),
    nats_monitor := reference() | undefined
}.

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, state()}.
init([]) ->
    self() ! subscribe_nats,
    {ok, #{nats_subscription => undefined, nats_monitor => undefined}}.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, ok, state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(subscribe_nats, State) ->
    {noreply, subscribe_to_nats(State)};
handle_info({nats_msg, ?NATS_SUBJECT, Payload, _ReplyTo}, State) when is_binary(Payload) ->
    broadcast_payload(Payload),
    {noreply, State};
handle_info({'DOWN', MonRef, process, _Pid, _Reason}, #{nats_monitor := MonRef} = State) ->
    erlang:send_after(?NATS_SUBSCRIBE_RETRY_MS, self(), subscribe_nats),
    {noreply, State#{nats_subscription => undefined, nats_monitor => undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec broadcast_payload(binary()) -> ok.
broadcast_payload(Payload) ->
    try json:decode(Payload) of
        #{<<"event">> := EventName, <<"data">> := Data} when
            is_binary(EventName), is_map(Data)
        ->
            broadcast_event(event_atoms:normalize(EventName), Data);
        _Other ->
            logger:warning("Gateway broadcast: unexpected NATS payload format")
    catch
        Class:Reason ->
            logger:warning("Gateway broadcast failed to decode NATS payload", #{
                class => Class, reason => Reason
            })
    end.

-spec broadcast_event(atom() | binary(), map()) -> ok.
broadcast_event(badges_update = Event, Data) ->
    session_manager:broadcast_dispatch(
        Event, {pre_encoded, iolist_to_binary(json:encode(Data))}
    );
broadcast_event(Event, _Data) ->
    logger:warning("Gateway broadcast rejected event", #{event => Event}).

-spec subscribe_to_nats(state()) -> state().
subscribe_to_nats(#{nats_subscription := Sid} = State) when Sid =/= undefined ->
    State;
subscribe_to_nats(State) ->
    try gateway_nats_rpc:subscribe(?NATS_SUBJECT, <<>>) of
        {ok, Sid} ->
            State#{nats_subscription => Sid, nats_monitor => monitor_nats_rpc()};
        {error, _Reason} ->
            retry_subscribe(State)
    catch
        _:_ -> retry_subscribe(State)
    end.

-spec retry_subscribe(state()) -> state().
retry_subscribe(State) ->
    erlang:send_after(?NATS_SUBSCRIBE_RETRY_MS, self(), subscribe_nats),
    State.

-spec monitor_nats_rpc() -> reference() | undefined.
monitor_nats_rpc() ->
    case whereis(gateway_nats_rpc) of
        Pid when is_pid(Pid) -> erlang:monitor(process, Pid);
        _ -> undefined
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

broadcast_payload_ignores_unknown_events_test() ->
    ?assertEqual(ok, broadcast_payload(<<"{\"event\":\"MESSAGE_CREATE\",\"data\":{}}">>)).

broadcast_payload_ignores_malformed_payloads_test() ->
    ?assertEqual(ok, broadcast_payload(<<"{\"event\":1}">>)),
    ?assertEqual(ok, broadcast_payload(<<"not json">>)).

-endif.
