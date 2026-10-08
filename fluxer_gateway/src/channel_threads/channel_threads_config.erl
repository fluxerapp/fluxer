%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(channel_threads_config).
-typing([eqwalizer]).
-behaviour(gen_server).

-export([
    start_link/0,
    config/0,
    version/0,
    enabled/0,
    loaded/0,
    ever_enabled/0,
    guild_active/1,
    guild_active/2,
    user_active/1,
    user_active/2,
    user_excluded/1,
    user_excluded/2,
    guild_fields_changed/2,
    user_fields_changed/2,
    update_counts/0,
    note_identify/2,
    identify_counts/0,
    identify_os_label/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-export([validate_config/1, store_validated_config/3, default_config/0, apply_nats_payload/1]).
-endif.

-define(PERSISTENT_TERM_KEY, channel_threads_config).
-define(PULLED_KEY, {channel_threads_config, pulled}).
-define(UPDATE_COUNTS_KEY, {channel_threads_config, update_counts}).
-define(IDENTIFY_COUNTS_KEY, {channel_threads_config, identify_counts}).
-define(NATS_SUBJECT, <<"config.channel.threads">>).
-define(FETCH_DELAY_MS, 2000).
-define(RECONCILE_INTERVAL_MS, 30000).
-define(NATS_SUBSCRIBE_RETRY_MS, 2000).
-define(MAX_TARGETED_IDS, 1000).
-define(MAX_SALT_BYTES, 64).
-define(MAX_ID_DIGITS, 20).
-define(DEFAULT_GUILD_SALT, <<"channel-threads-guild-v1">>).
-define(DEFAULT_USER_SALT, <<"channel-threads-user-v1">>).

-type id_set() :: #{binary() => true}.
-type config() :: #{
    enabled := boolean(),
    config_version := non_neg_integer(),
    ever_enabled := boolean(),
    guild_basis_points := non_neg_integer(),
    guild_salt := binary(),
    enabled_guilds := id_set(),
    disabled_guilds := id_set(),
    user_basis_points := non_neg_integer(),
    user_salt := binary(),
    included_users := id_set(),
    excluded_users := id_set()
}.
-type state() :: #{
    nats_subscription := term(),
    nats_monitor := reference() | undefined,
    fetch_timer := reference() | undefined
}.
-type store_result() :: updated | unchanged | stale | rejected.
-type os_label() :: binary().

-export_type([config/0]).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec config() -> config().
config() ->
    case persistent_term:get(?PERSISTENT_TERM_KEY, undefined) of
        #{config_version := _} = Config -> Config;
        _ -> default_config()
    end.

-spec version() -> non_neg_integer().
version() ->
    maps:get(config_version, config()).

-spec enabled() -> boolean().
enabled() ->
    maps:get(enabled, config()).

-spec loaded() -> boolean().
loaded() ->
    persistent_term:get(?PULLED_KEY, false) orelse
        case config() of
            #{enabled := false, config_version := 0} -> false;
            _ -> true
        end.

-spec ever_enabled() -> boolean().
ever_enabled() ->
    maps:get(ever_enabled, config()).

-spec guild_active(integer()) -> boolean().
guild_active(GuildId) ->
    guild_active(config(), GuildId).

-spec guild_active(config(), integer()) -> boolean().
guild_active(#{enabled := true} = Config, GuildId) when is_integer(GuildId) ->
    Id = integer_to_binary(GuildId),
    #{
        enabled_guilds := Included,
        disabled_guilds := Excluded,
        guild_basis_points := BasisPoints,
        guild_salt := Salt
    } = Config,
    active(Id, Included, Excluded, BasisPoints, Salt);
guild_active(_Config, _GuildId) ->
    false.

-spec user_active(integer()) -> boolean().
user_active(UserId) ->
    user_active(config(), UserId).

-spec user_active(config(), integer()) -> boolean().
user_active(#{enabled := true} = Config, UserId) when is_integer(UserId) ->
    Id = integer_to_binary(UserId),
    #{
        included_users := Included,
        excluded_users := Excluded,
        user_basis_points := BasisPoints,
        user_salt := Salt
    } = Config,
    active(Id, Included, Excluded, BasisPoints, Salt);
user_active(_Config, _UserId) ->
    false.

-spec user_excluded(integer()) -> boolean().
user_excluded(UserId) ->
    user_excluded(config(), UserId).

-spec user_excluded(config(), integer()) -> boolean().
user_excluded(#{excluded_users := Excluded}, UserId) when is_integer(UserId) ->
    maps:is_key(integer_to_binary(UserId), Excluded);
user_excluded(_Config, _UserId) ->
    false.

-spec active(binary(), id_set(), id_set(), non_neg_integer(), binary()) -> boolean().
active(Id, Included, Excluded, BasisPoints, Salt) ->
    case maps:is_key(Id, Excluded) of
        true ->
            false;
        false ->
            maps:is_key(Id, Included) orelse
                (BasisPoints > 0 andalso experiment_bucket:bucket(Id, Salt) < BasisPoints)
    end.

-spec guild_fields_changed(config(), config()) -> boolean().
guild_fields_changed(Previous, Current) ->
    fields_changed(
        [enabled, guild_basis_points, guild_salt, enabled_guilds, disabled_guilds],
        Previous,
        Current
    ).

-spec user_fields_changed(config(), config()) -> boolean().
user_fields_changed(Previous, Current) ->
    fields_changed(
        [enabled, user_basis_points, user_salt, included_users, excluded_users],
        Previous,
        Current
    ).

-spec fields_changed([atom()], config(), config()) -> boolean().
fields_changed(Keys, Previous, Current) ->
    maps:with(Keys, Previous) =/= maps:with(Keys, Current).

-spec update_counts() -> #{store_result() => non_neg_integer()}.
update_counts() ->
    Counters = persistent_term:get(?UPDATE_COUNTS_KEY, undefined),
    maps:from_list([{Result, read_count(Counters, Result)} || Result <- store_results()]).

-spec note_identify(boolean(), map()) -> ok.
note_identify(Capable, Properties) ->
    case persistent_term:get(?IDENTIFY_COUNTS_KEY, undefined) of
        undefined ->
            ok;
        Counters ->
            Os = identify_os_label(maps:get(<<"os">>, Properties, undefined)),
            counters:add(Counters, identify_index(Capable, Os), 1)
    end.

-spec identify_counts() -> #{{boolean(), os_label()} => non_neg_integer()}.
identify_counts() ->
    case persistent_term:get(?IDENTIFY_COUNTS_KEY, undefined) of
        undefined ->
            #{};
        Counters ->
            maps:from_list([
                {{Capable, Os}, counters:get(Counters, identify_index(Capable, Os))}
             || Capable <- [true, false], Os <- os_labels()
            ])
    end.

-spec identify_os_label(term()) -> os_label().
identify_os_label(Os) when is_binary(Os) ->
    classify_os(string:lowercase(Os));
identify_os_label(_Os) ->
    <<"other">>.

-spec classify_os(unicode:chardata()) -> os_label().
classify_os(Os) ->
    first_os_match(Os, [
        {<<"android">>, [<<"android">>]},
        {<<"ios">>, [<<"ios">>, <<"iphone">>, <<"ipad">>]},
        {<<"macos">>, [<<"mac">>, <<"darwin">>, <<"os x">>]},
        {<<"windows">>, [<<"win">>]},
        {<<"linux">>, [<<"linux">>, <<"bsd">>]}
    ]).

-spec first_os_match(unicode:chardata(), [{os_label(), [binary()]}]) -> os_label().
first_os_match(_Os, []) ->
    <<"other">>;
first_os_match(Os, [{Label, Needles} | Rest]) ->
    case lists:any(fun(Needle) -> string:find(Os, Needle) =/= nomatch end, Needles) of
        true -> Label;
        false -> first_os_match(Os, Rest)
    end.

-spec os_labels() -> [os_label()].
os_labels() ->
    [<<"android">>, <<"ios">>, <<"windows">>, <<"macos">>, <<"linux">>, <<"other">>].

-spec identify_index(boolean(), os_label()) -> pos_integer().
identify_index(Capable, Os) ->
    CapableOffset =
        case Capable of
            true -> 0;
            false -> length(os_labels())
        end,
    CapableOffset + os_index(Os, os_labels(), 1).

-spec os_index(os_label(), [os_label()], pos_integer()) -> pos_integer().
os_index(Os, [Os | _Rest], Index) ->
    Index;
os_index(Os, [_Other | Rest], Index) ->
    os_index(Os, Rest, Index + 1);
os_index(_Os, [], Index) ->
    Index - 1.

-spec init([]) -> {ok, state()}.
init([]) ->
    erlang:process_flag(fullsweep_after, 50),
    persistent_term:put(?PERSISTENT_TERM_KEY, config()),
    ensure_identify_counters(),
    self() ! subscribe_nats,
    {ok, #{
        nats_subscription => undefined,
        nats_monitor => undefined,
        fetch_timer => erlang:send_after(?FETCH_DELAY_MS, self(), fetch_config)
    }}.

-spec ensure_identify_counters() -> ok.
ensure_identify_counters() ->
    case persistent_term:get(?IDENTIFY_COUNTS_KEY, undefined) of
        undefined ->
            Size = 2 * length(os_labels()),
            persistent_term:put(?IDENTIFY_COUNTS_KEY, counters:new(Size, [write_concurrency]));
        _Counters ->
            ok
    end.

-spec handle_call(term(), gen_server:from(), state()) -> {reply, term(), state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(subscribe_nats, State) ->
    {noreply, subscribe_to_nats(State)};
handle_info(fetch_config, State) ->
    count(fetch_config_from_api()),
    {noreply, State#{
        fetch_timer => erlang:send_after(?RECONCILE_INTERVAL_MS, self(), fetch_config)
    }};
handle_info({nats_resubscribed, ?NATS_SUBJECT}, State) ->
    count(fetch_config_from_api()),
    {noreply, State};
handle_info({nats_msg, ?NATS_SUBJECT, Payload, _ReplyTo}, State) when is_binary(Payload) ->
    count(apply_nats_payload(Payload)),
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

-spec default_config() -> config().
default_config() ->
    #{
        enabled => false,
        config_version => 0,
        ever_enabled => false,
        guild_basis_points => 0,
        guild_salt => ?DEFAULT_GUILD_SALT,
        enabled_guilds => #{},
        disabled_guilds => #{},
        user_basis_points => 0,
        user_salt => ?DEFAULT_USER_SALT,
        included_users => #{},
        excluded_users => #{}
    }.

-spec fetch_config_from_api() -> store_result().
fetch_config_from_api() ->
    RpcRequest = #{<<"type">> => <<"get_channel_threads_config">>},
    case api_rpc_client:call(RpcRequest) of
        {ok, #{<<"config">> := Config}} when is_map(Config) ->
            store_valid_config(Config, api);
        {ok, _Other} ->
            logger:warning("Channel threads config: unexpected API response format"),
            rejected;
        {error, Reason} ->
            logger:warning("Channel threads config failed to fetch from API", #{
                reason => Reason
            }),
            rejected
    end.

-spec apply_nats_payload(binary()) -> store_result().
apply_nats_payload(Payload) ->
    try json:decode(Payload) of
        #{<<"type">> := <<"channel_threads_config">>, <<"config">> := Config} when
            is_map(Config)
        ->
            store_valid_config(Config, nats);
        #{<<"config">> := Config} when is_map(Config) ->
            store_valid_config(Config, nats);
        _Other ->
            logger:warning("Channel threads config: unexpected NATS payload format"),
            rejected
    catch
        Class:Reason ->
            logger:warning("Channel threads config failed to decode NATS payload", #{
                class => Class, reason => Reason
            }),
            rejected
    end.

-spec store_valid_config(map(), api | nats) -> store_result().
store_valid_config(Wire, Source) ->
    case validate_config(Wire) of
        {ok, Validated} ->
            Result = store_validated_config(config(), Validated, Source),
            persistent_term:put(?PULLED_KEY, true),
            Result;
        {error, {invalid_field, WireKey, Length}} ->
            logger:warning("Channel threads config rejected invalid config", #{
                source => Source, field => WireKey, length => Length
            }),
            rejected
    end.

-spec store_validated_config(config(), config(), api | nats) -> store_result().
store_validated_config(Previous, Previous, _Source) ->
    unchanged;
store_validated_config(#{config_version := Held}, #{config_version := Offered}, Source) when
    Offered < Held
->
    logger:warning("Channel threads config ignored a lower config_version", #{
        source => Source, held => Held, offered => Offered
    }),
    stale;
store_validated_config(Previous, Offered, Source) ->
    Current = Offered#{
        ever_enabled := maps:get(ever_enabled, Previous) orelse maps:get(ever_enabled, Offered)
    },
    store_changed_config(Previous, Current, Source).

-spec store_changed_config(config(), config(), api | nats) -> store_result().
store_changed_config(Previous, Previous, _Source) ->
    unchanged;
store_changed_config(Previous, Current, Source) ->
    persistent_term:put(?PERSISTENT_TERM_KEY, Current),
    logger:notice("Channel threads config updated", #{
        source => Source,
        config_version => maps:get(config_version, Current),
        enabled => maps:get(enabled, Current),
        guild_basis_points => maps:get(guild_basis_points, Current),
        user_basis_points => maps:get(user_basis_points, Current),
        enabled_guilds => map_size(maps:get(enabled_guilds, Current)),
        disabled_guilds => map_size(maps:get(disabled_guilds, Current)),
        included_users => map_size(maps:get(included_users, Current)),
        excluded_users => map_size(maps:get(excluded_users, Current))
    }),
    ok = channel_threads_flip:config_changed(Previous, Current),
    updated.

-spec validate_config(map()) ->
    {ok, config()} | {error, {invalid_field, binary(), non_neg_integer() | undefined}}.
validate_config(Wire) ->
    lists:foldl(
        fun(Field, Acc) -> validate_field(Field, Wire, Acc) end,
        {ok, default_config()},
        config_fields()
    ).

-spec config_fields() -> [{atom(), binary(), fun((term()) -> {ok, term()} | error)}].
config_fields() ->
    [
        {enabled, <<"enabled">>, fun validate_boolean/1},
        {config_version, <<"config_version">>, fun validate_config_version/1},
        {ever_enabled, <<"ever_enabled">>, fun validate_boolean/1},
        {guild_basis_points, <<"guild_basis_points">>, fun validate_basis_points/1},
        {guild_salt, <<"guild_salt">>, fun validate_salt/1},
        {enabled_guilds, <<"enabled_guild_ids">>, fun validate_ids/1},
        {disabled_guilds, <<"disabled_guild_ids">>, fun validate_ids/1},
        {user_basis_points, <<"user_basis_points">>, fun validate_basis_points/1},
        {user_salt, <<"user_salt">>, fun validate_salt/1},
        {included_users, <<"included_user_ids">>, fun validate_ids/1},
        {excluded_users, <<"excluded_user_ids">>, fun validate_ids/1}
    ].

-spec validate_field(
    {atom(), binary(), fun((term()) -> {ok, term()} | error)},
    map(),
    {ok, config()} | {error, term()}
) -> {ok, config()} | {error, term()}.
validate_field(_Field, _Wire, {error, _} = Error) ->
    Error;
validate_field({Key, WireKey, Validate}, Wire, {ok, Acc}) ->
    case maps:find(WireKey, Wire) of
        error -> {ok, Acc};
        {ok, Value} -> store_validated_field(Key, WireKey, Value, Validate(Value), Acc)
    end.

-spec store_validated_field(atom(), binary(), term(), {ok, term()} | error, config()) ->
    {ok, config()} | {error, term()}.
store_validated_field(Key, _WireKey, _Value, {ok, Normalised}, Acc) ->
    {ok, Acc#{Key => Normalised}};
store_validated_field(_Key, WireKey, Value, error, _Acc) when is_list(Value) ->
    {error, {invalid_field, WireKey, length(Value)}};
store_validated_field(_Key, WireKey, _Value, error, _Acc) ->
    {error, {invalid_field, WireKey, undefined}}.

-spec validate_boolean(term()) -> {ok, boolean()} | error.
validate_boolean(Value) when is_boolean(Value) ->
    {ok, Value};
validate_boolean(_Value) ->
    error.

-spec validate_config_version(term()) -> {ok, non_neg_integer()} | error.
validate_config_version(Value) when is_integer(Value), Value >= 0 ->
    {ok, Value};
validate_config_version(_Value) ->
    error.

-spec validate_basis_points(term()) -> {ok, non_neg_integer()} | error.
validate_basis_points(Value) when is_integer(Value), Value >= 0 ->
    case Value =< experiment_bucket:resolution() of
        true -> {ok, Value};
        false -> error
    end;
validate_basis_points(_Value) ->
    error.

-spec validate_salt(term()) -> {ok, binary()} | error.
validate_salt(Value) when is_binary(Value) ->
    validate_salt_bytes(Value, byte_size(Value));
validate_salt(_Value) ->
    error.

-spec validate_salt_bytes(binary(), non_neg_integer()) -> {ok, binary()} | error.
validate_salt_bytes(Value, Size) when Size >= 1, Size =< ?MAX_SALT_BYTES ->
    validate_printable_ascii(Value, Value);
validate_salt_bytes(_Value, _Size) ->
    error.

-spec validate_printable_ascii(binary(), binary()) -> {ok, binary()} | error.
validate_printable_ascii(<<>>, Value) ->
    {ok, Value};
validate_printable_ascii(<<Byte:8, Rest/binary>>, Value) when Byte >= 16#20, Byte =< 16#7e ->
    validate_printable_ascii(Rest, Value);
validate_printable_ascii(_Remaining, _Value) ->
    error.

-spec validate_ids(term()) -> {ok, id_set()} | error.
validate_ids(Value) when is_list(Value), length(Value) =< ?MAX_TARGETED_IDS ->
    collect_ids(Value, #{});
validate_ids(_Value) ->
    error.

-spec collect_ids([term()], id_set()) -> {ok, id_set()} | error.
collect_ids([], Acc) ->
    {ok, Acc};
collect_ids([Id | Rest], Acc) when is_binary(Id) ->
    case is_id(Id, byte_size(Id)) of
        true -> collect_ids(Rest, Acc#{Id => true});
        false -> error
    end;
collect_ids(_Ids, _Acc) ->
    error.

-spec is_id(binary(), non_neg_integer()) -> boolean().
is_id(Id, Size) when Size >= 1, Size =< ?MAX_ID_DIGITS ->
    is_all_digits(Id);
is_id(_Id, _Size) ->
    false.

-spec is_all_digits(binary()) -> boolean().
is_all_digits(<<>>) ->
    true;
is_all_digits(<<Byte:8, Rest/binary>>) when Byte >= $0, Byte =< $9 ->
    is_all_digits(Rest);
is_all_digits(_Remaining) ->
    false.

-spec subscribe_to_nats(state()) -> state().
subscribe_to_nats(#{nats_subscription := Sid} = State) when Sid =/= undefined ->
    State;
subscribe_to_nats(State) ->
    case subscribe_to_config_subject() of
        {ok, Sid} ->
            MonRef = monitor_nats_rpc(),
            logger:info("Channel threads config subscribed to NATS", #{subject => ?NATS_SUBJECT}),
            count(fetch_config_from_api()),
            State#{nats_subscription => Sid, nats_monitor => MonRef};
        {error, Reason} ->
            logger:debug("Channel threads config waiting for NATS subscription", #{
                subject => ?NATS_SUBJECT, reason => Reason
            }),
            erlang:send_after(?NATS_SUBSCRIBE_RETRY_MS, self(), subscribe_nats),
            State
    end.

-spec subscribe_to_config_subject() -> {ok, term()} | {error, term()}.
subscribe_to_config_subject() ->
    try gateway_nats_rpc:subscribe(?NATS_SUBJECT, <<>>) of
        {ok, Sid} -> {ok, Sid};
        {error, Reason} -> {error, Reason}
    catch
        Class:Reason -> {error, {Class, Reason}}
    end.

-spec monitor_nats_rpc() -> reference() | undefined.
monitor_nats_rpc() ->
    case whereis(gateway_nats_rpc) of
        Pid when is_pid(Pid) -> erlang:monitor(process, Pid);
        _ -> undefined
    end.

-spec count(store_result()) -> ok.
count(Result) ->
    counters:add(update_counters(), result_index(Result), 1).

-spec update_counters() -> counters:counters_ref().
update_counters() ->
    case persistent_term:get(?UPDATE_COUNTS_KEY, undefined) of
        undefined ->
            Counters = counters:new(length(store_results()), [atomics]),
            persistent_term:put(?UPDATE_COUNTS_KEY, Counters),
            Counters;
        Counters ->
            Counters
    end.

-spec read_count(counters:counters_ref() | undefined, store_result()) -> non_neg_integer().
read_count(undefined, _Result) ->
    0;
read_count(Counters, Result) ->
    counters:get(Counters, result_index(Result)).

-spec store_results() -> [store_result()].
store_results() ->
    [updated, unchanged, stale, rejected].

-spec result_index(store_result()) -> pos_integer().
result_index(updated) -> 1;
result_index(unchanged) -> 2;
result_index(stale) -> 3;
result_index(rejected) -> 4.
