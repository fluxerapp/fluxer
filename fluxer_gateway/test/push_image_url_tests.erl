%% SPDX-License-Identifier: AGPL-3.0-or-later

-module(push_image_url_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MEDIA_URL, <<"https://media.example.test/attachments/11/22/cat.png?ex=1&is=2&hm=3">>).
-define(API_URL, <<"https://api.example.test/api/channels/11/attachments/22/cat.png?mid=33">>).
-define(PLAIN_API_URL, <<"http://api.example.test/channels/11/attachments/22/cat.png?mid=33">>).

image(Url) ->
    #{
        <<"url">> => Url,
        <<"proxy_url">> => Url,
        <<"content_type">> => <<"image/png">>,
        <<"flags">> => 0
    }.

message(Fields) ->
    maps:merge(#{<<"channel_id">> => <<"11">>, <<"id">> => <<"33">>}, Fields).

media_proxy_attachment_test() ->
    ?assertEqual(
        ?MEDIA_URL, push_image_url:extract(message(#{<<"attachments">> => [image(?MEDIA_URL)]}))
    ).

own_api_attachment_has_no_image_test() ->
    ?assertEqual(
        undefined, push_image_url:extract(message(#{<<"attachments">> => [image(?API_URL)]}))
    ),
    ?assertEqual(
        undefined,
        push_image_url:extract(message(#{<<"attachments">> => [image(?PLAIN_API_URL)]}))
    ),
    ?assertEqual(
        undefined,
        push_image_url:extract(message(#{<<"embeds">> => [#{<<"image">> => image(?API_URL)}]}))
    ).

own_api_attachment_falls_back_to_the_next_image_test() ->
    ?assertEqual(
        ?MEDIA_URL,
        push_image_url:extract(
            message(#{
                <<"attachments">> => [image(?API_URL)],
                <<"embeds">> => [#{<<"image">> => image(?MEDIA_URL)}]
            })
        )
    ).

links_of_other_messages_keep_their_image_test() ->
    ?assertEqual(
        ?API_URL,
        push_image_url:extract(
            message(#{<<"id">> => <<"34">>, <<"attachments">> => [image(?API_URL)]})
        )
    ),
    ?assertEqual(
        ?API_URL,
        push_image_url:extract(
            message(#{<<"channel_id">> => <<"12">>, <<"attachments">> => [image(?API_URL)]})
        )
    ),
    ?assertEqual(?API_URL, push_image_url:extract(#{<<"attachments">> => [image(?API_URL)]})),
    Url = <<"https://example.test/channels/11/attachments/22/cat.png">>,
    ?assertEqual(Url, push_image_url:extract(message(#{<<"attachments">> => [image(Url)]}))).
