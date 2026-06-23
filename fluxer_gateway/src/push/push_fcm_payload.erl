%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_fcm_payload).
-typing([eqwalizer]).

-export([
    build_message/2,
    handle_response/3,
    delete_payload/2
]).
-export_type([fcm_response/0]).

-type fcm_response() :: {ok, integer(), term(), binary()} | {error, term()}.

-spec build_message(binary(), map()) -> map().
build_message(DeviceToken, #{<<"type">> := <<"notification_clear">>} = Payload) ->
    build_clear_message(DeviceToken, Payload);
build_message(DeviceToken, Payload) ->
    build_notification_message(DeviceToken, Payload).

-spec build_clear_message(binary(), map()) -> map().
build_clear_message(DeviceToken, Payload) ->
    Data0 = data_as_strings(maps:get(<<"data">>, Payload, #{})),
    Tag = push_utils:normalize_binary(
        maps:get(
            <<"notification_tag">>, Payload, maps:get(<<"tag">>, Payload, <<"fluxer-message">>)
        ),
        <<"fluxer-message">>
    ),
    Data = maps:merge(
        Data0,
        data_as_strings(#{
            <<"type">> => <<"notification_clear">>,
            <<"action">> => <<"clear_channel">>,
            <<"notification_tag">> => Tag
        })
    ),
    #{
        <<"message">> => #{
            <<"token">> => DeviceToken,
            <<"data">> => Data,
            <<"android">> => #{
                <<"priority">> => <<"HIGH">>,
                <<"ttl">> => <<"300s">>,
                <<"collapse_key">> => Tag
            },
            <<"fcm_options">> => #{
                <<"analytics_label">> => <<"notification_clear">>
            }
        }
    }.

-spec build_notification_message(binary(), map()) -> map().
build_notification_message(DeviceToken, Payload) ->
    Notification = maps:get(<<"notification">>, Payload, #{}),
    Title = resolve_title(Notification, Payload),
    Body = resolve_body(Notification, Payload),
    Tag = push_utils:normalize_binary(
        maps:get(<<"tag">>, Payload, <<"fluxer-message">>), <<"fluxer-message">>
    ),
    ImageUrl = resolve_image_url(Notification, Payload),
    NotificationBody = maybe_put(
        <<"image">>, ImageUrl, #{<<"title">> => Title, <<"body">> => Body}
    ),
    Data = build_notification_data(Payload, Title, Body, Tag, ImageUrl),
    Group = resolve_notification_group(maps:get(<<"data">>, Payload, #{}), Tag),
    AndroidNotification = build_android_notification(Tag, ImageUrl, Group),
    wrap_notification_message(DeviceToken, NotificationBody, Data, AndroidNotification, Group).

-spec build_android_notification(binary(), binary() | undefined, binary()) -> map().
build_android_notification(Tag, ImageUrl, Group) ->
    maybe_put(<<"image">>, ImageUrl, #{
        <<"channel_id">> => <<"fluxer_default_push">>,
        <<"tag">> => Tag,
        <<"group">> => Group,
        <<"click_action">> => <<"FLUXER_MESSAGE">>
    }).

-spec wrap_notification_message(binary(), map(), map(), map(), binary()) -> map().
wrap_notification_message(DeviceToken, NotificationBody, Data, AndroidNotification, Group) ->
    #{
        <<"message">> => #{
            <<"token">> => DeviceToken,
            <<"notification">> => NotificationBody,
            <<"data">> => Data,
            <<"android">> => #{
                <<"priority">> => <<"HIGH">>,
                <<"ttl">> => <<"86400s">>,
                <<"collapse_key">> => Group,
                <<"notification">> => AndroidNotification
            },
            <<"fcm_options">> => #{
                <<"analytics_label">> => <<"message_create">>
            }
        }
    }.

-spec resolve_notification_group(map(), binary()) -> binary().
resolve_notification_group(Data, MessageTag) ->
    case
        push_utils:normalize_binary(
            maps:get(<<"notification_tag">>, Data, undefined), undefined
        )
    of
        Group when is_binary(Group), byte_size(Group) > 0 ->
            Group;
        _ ->
            case
                push_utils:normalize_binary(
                    maps:get(<<"channel_id">>, Data, undefined), undefined
                )
            of
                ChannelId when is_binary(ChannelId), byte_size(ChannelId) > 0 ->
                    <<"channel:", ChannelId/binary>>;
                _ ->
                    derive_channel_tag_from_message_tag(MessageTag)
            end
    end.

-spec derive_channel_tag_from_message_tag(binary()) -> binary().
derive_channel_tag_from_message_tag(MessageTag) ->
    case binary:split(MessageTag, <<":">>, [global]) of
        [<<"channel">>, ChannelId, _MessageId] when byte_size(ChannelId) > 0 ->
            <<"channel:", ChannelId/binary>>;
        _ ->
            MessageTag
    end.

-spec build_notification_data(map(), binary(), binary(), binary(), binary() | undefined) ->
    map().
build_notification_data(Payload, Title, Body, Tag, ImageUrl) ->
    Data0 = data_as_strings(maps:get(<<"data">>, Payload, #{})),
    DataExtra = maybe_put(<<"image_url">>, ImageUrl, #{
        <<"title">> => Title,
        <<"body">> => Body,
        <<"tag">> => Tag
    }),
    maps:merge(Data0, data_as_strings(DataExtra)).

-spec resolve_title(map(), map()) -> binary().
resolve_title(Notification, Payload) ->
    sanitize_text(
        push_utils:normalize_binary(
            maps:get(<<"title">>, Notification, maps:get(<<"title">>, Payload, <<"Fluxer">>)),
            <<"Fluxer">>
        )
    ).

-spec resolve_body(map(), map()) -> binary().
resolve_body(Notification, Payload) ->
    sanitize_text(
        push_utils:normalize_binary(
            maps:get(<<"body">>, Notification, maps:get(<<"body">>, Payload, <<"">>)),
            <<"">>
        )
    ).

-spec sanitize_text(binary()) -> binary().
sanitize_text(Bin) when is_binary(Bin) ->
    case unicode:characters_to_list(Bin) of
        Codepoints when is_list(Codepoints) ->
            safe_codepoints_to_binary(Codepoints, Bin);
        _ ->
            Bin
    end.

-spec safe_codepoints_to_binary([integer()], binary()) -> binary().
safe_codepoints_to_binary(Codepoints, Fallback) ->
    Filtered = [C || C <- Codepoints, is_safe_codepoint(C)],
    case unicode:characters_to_binary(Filtered) of
        Result when is_binary(Result) -> Result;
        _ -> Fallback
    end.

-spec is_safe_codepoint(integer()) -> boolean().
is_safe_codepoint(C) when C =:= $\n; C =:= $\t -> true;
is_safe_codepoint(C) when C >= 0, C =< 16#1F -> false;
is_safe_codepoint(C) when C >= 16#7F, C =< 16#9F -> false;
is_safe_codepoint(C) when C >= 16#200E, C =< 16#200F -> false;
is_safe_codepoint(C) when C >= 16#202A, C =< 16#202E -> false;
is_safe_codepoint(C) when C >= 16#2066, C =< 16#2069 -> false;
is_safe_codepoint(_) -> true.

-spec resolve_image_url(map(), map()) -> binary() | undefined.
resolve_image_url(Notification, Payload) ->
    first_binary([
        maps:get(<<"image_url">>, Payload, undefined),
        maps:get(<<"image">>, Notification, undefined),
        maps:get(<<"image_url">>, Notification, undefined)
    ]).

-spec handle_response(integer(), binary(), fcm_response()) -> false | {true, map()}.
handle_response(_UserId, _SubscriptionId, {ok, Status, _, _}) when
    Status >= 200, Status < 300
->
    false;
handle_response(UserId, SubscriptionId, {ok, _Status, _, Body}) ->
    Reason = fcm_error_code(Body),
    case is_permanent_fcm_error(Reason) of
        true -> {true, delete_payload(UserId, SubscriptionId)};
        false -> false
    end;
handle_response(UserId, _SubscriptionId, {error, Reason}) ->
    logger:debug("Push: FCM network error", #{user_id => UserId, reason => Reason}),
    false.

-spec delete_payload(integer(), binary()) -> map().
delete_payload(UserId, SubscriptionId) ->
    #{
        <<"user_id">> => integer_to_binary(UserId),
        <<"subscription_id">> => SubscriptionId
    }.

-spec data_as_strings(term()) -> map().
data_as_strings(Data) when is_map(Data) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            Acc#{push_utils:normalize_binary(Key, <<>>) => stringify_value(Value)}
        end,
        #{},
        Data
    );
data_as_strings(_) ->
    #{}.

-spec maybe_put(binary(), binary() | undefined, map()) -> map().
maybe_put(_Key, undefined, Map) ->
    Map;
maybe_put(Key, Value, Map) when is_binary(Value), byte_size(Value) > 0 ->
    Map#{Key => Value};
maybe_put(_Key, _Value, Map) ->
    Map.

-spec stringify_value(term()) -> binary().
stringify_value(Value) when is_binary(Value) -> Value;
stringify_value(Value) when is_integer(Value) -> integer_to_binary(Value);
stringify_value(Value) when is_float(Value) -> list_to_binary(io_lib:format("~p", [Value]));
stringify_value(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
stringify_value(Value) when is_list(Value) -> stringify_list_value(Value);
stringify_value(Value) -> stringify_json_value(Value).

-spec stringify_list_value([term()]) -> binary().
stringify_list_value(Value) ->
    case type_conv:to_binary(Value) of
        Bin when is_binary(Bin) -> Bin;
        undefined -> stringify_json_value(Value)
    end.

-spec stringify_json_value(term()) -> binary().
stringify_json_value(Value) ->
    iolist_to_binary(json:encode(json_compatible_value(Value))).

-spec json_compatible_value(term()) -> json:encode_value().
json_compatible_value(Value) when is_binary(Value) -> Value;
json_compatible_value(Value) when is_integer(Value) -> Value;
json_compatible_value(Value) when is_float(Value) -> Value;
json_compatible_value(Value) when is_atom(Value) -> Value;
json_compatible_value(Value) when is_list(Value) ->
    [json_compatible_value(Item) || Item <- Value];
json_compatible_value(Value) when is_map(Value) ->
    maps:fold(
        fun(Key, Item, Acc) ->
            Acc#{push_utils:normalize_binary(Key, <<>>) => json_compatible_value(Item)}
        end,
        #{},
        Value
    );
json_compatible_value(Value) ->
    iolist_to_binary(io_lib:format("~p", [Value])).

-spec first_binary(list()) -> binary() | undefined.
first_binary([]) ->
    undefined;
first_binary([Value | Rest]) ->
    case push_utils:normalize_binary(Value, undefined) of
        Bin when is_binary(Bin), byte_size(Bin) > 0 -> Bin;
        _ -> first_binary(Rest)
    end.

-spec fcm_error_code(binary()) -> binary().
fcm_error_code(Body) ->
    case decode_json_map(Body) of
        #{<<"error">> := #{<<"details">> := Details}} when is_list(Details) ->
            fcm_details_error_code(Details);
        #{<<"error">> := #{<<"status">> := Status}} when is_binary(Status) ->
            Status;
        _ ->
            <<"http_error">>
    end.

-spec fcm_details_error_code(list()) -> binary().
fcm_details_error_code(Details) ->
    case find_fcm_error_code(Details) of
        undefined -> <<"http_error">>;
        Code -> Code
    end.

-spec find_fcm_error_code(list()) -> binary() | undefined.
find_fcm_error_code([]) -> undefined;
find_fcm_error_code([#{<<"errorCode">> := Code} | _]) when is_binary(Code) -> Code;
find_fcm_error_code([_ | Rest]) -> find_fcm_error_code(Rest).

-spec is_permanent_fcm_error(binary()) -> boolean().
is_permanent_fcm_error(<<"UNREGISTERED">>) -> true;
is_permanent_fcm_error(<<"INVALID_ARGUMENT">>) -> true;
is_permanent_fcm_error(_) -> false.

-spec decode_json_map(binary()) -> map() | undefined.
decode_json_map(Body) when is_binary(Body), byte_size(Body) > 0 ->
    try json:decode(Body) of
        Map when is_map(Map) -> Map;
        _ -> undefined
    catch
        error:_ -> undefined;
        throw:_ -> undefined;
        exit:_ -> undefined
    end;
decode_json_map(_) ->
    undefined.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

build_message_includes_android_chat_notification_fields_test() ->
    Payload = #{
        <<"title">> => <<"Alice">>,
        <<"body">> => <<"Hello">>,
        <<"tag">> => <<"channel:123:456">>,
        <<"image_url">> => <<"https://cdn.example/image.png">>,
        <<"data">> => #{
            <<"channel_id">> => <<"123">>,
            <<"message_id">> => <<"456">>,
            <<"notification_tag">> => <<"channel:123">>,
            <<"badge_count">> => 4
        },
        <<"notification">> => #{
            <<"title">> => <<"Alice">>,
            <<"body">> => <<"Hello">>
        }
    },
    #{<<"message">> := Message} = build_message(<<"device-token">>, Payload),
    ?assertEqual(<<"device-token">>, maps:get(<<"token">>, Message)),
    ?assertEqual(<<"HIGH">>, maps:get(<<"priority">>, maps:get(<<"android">>, Message))),
    ?assertEqual(<<"86400s">>, maps:get(<<"ttl">>, maps:get(<<"android">>, Message))),
    AndroidNotification = maps:get(<<"notification">>, maps:get(<<"android">>, Message)),
    ?assertEqual(<<"fluxer_default_push">>, maps:get(<<"channel_id">>, AndroidNotification)),
    ?assertEqual(<<"channel:123:456">>, maps:get(<<"tag">>, AndroidNotification)),
    ?assertEqual(<<"channel:123">>, maps:get(<<"group">>, AndroidNotification)),
    Android = maps:get(<<"android">>, Message),
    ?assertEqual(<<"channel:123">>, maps:get(<<"collapse_key">>, Android)),
    ?assertEqual(
        <<"https://cdn.example/image.png">>, maps:get(<<"image">>, AndroidNotification)
    ),
    Data = maps:get(<<"data">>, Message),
    ?assertEqual(<<"4">>, maps:get(<<"badge_count">>, Data)),
    ?assertEqual(<<"https://cdn.example/image.png">>, maps:get(<<"image_url">>, Data)).

build_clear_message_is_data_only_and_collapsible_test() ->
    Payload = #{
        <<"type">> => <<"notification_clear">>,
        <<"tag">> => <<"channel:123">>,
        <<"data">> => #{
            <<"channel_id">> => <<"123">>,
            <<"message_id">> => <<"456">>,
            <<"badge_count">> => 0
        }
    },
    #{<<"message">> := Message} = build_message(<<"device-token">>, Payload),
    ?assertEqual(false, maps:is_key(<<"notification">>, Message)),
    Android = maps:get(<<"android">>, Message),
    ?assertEqual(<<"HIGH">>, maps:get(<<"priority">>, Android)),
    ?assertEqual(<<"300s">>, maps:get(<<"ttl">>, Android)),
    ?assertEqual(<<"channel:123">>, maps:get(<<"collapse_key">>, Android)),
    Data = maps:get(<<"data">>, Message),
    ?assertEqual(<<"notification_clear">>, maps:get(<<"type">>, Data)),
    ?assertEqual(<<"clear_channel">>, maps:get(<<"action">>, Data)),
    ?assertEqual(<<"channel:123">>, maps:get(<<"notification_tag">>, Data)),
    ?assertEqual(<<"0">>, maps:get(<<"badge_count">>, Data)).

-endif.
