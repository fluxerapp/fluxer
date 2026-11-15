%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_spoiler_preview_tests).

-include_lib("eunit/include/eunit.hrl").

inline_spoiler_is_hidden_test() ->
    ?assertEqual(
        <<"a █████ b"/utf8>>,
        push_notification_format:build_content_preview(
            #{<<"content">> => <<"a ||secret|| b">>}, #{}
        )
    ).

block_spoiler_is_hidden_test() ->
    Preview = push_notification_format:build_content_preview(
        #{<<"content">> => <<"||\nsecret\n||">>}, #{}
    ),
    ?assertEqual(nomatch, binary:match(Preview, <<"secret">>)).
