// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/image_url
import fluxer_gateway/push/message.{type MessageData}
import gleam/option.{None, Some}

const media_url =
  "https://media.example.test/attachments/11/22/cat.png?ex=1&is=2&hm=3"

const api_url =
  "https://api.example.test/api/channels/11/attachments/22/cat.png?mid=33"

const plain_api_url =
  "http://api.example.test/channels/11/attachments/22/cat.png?mid=33"

fn image(url: String) -> message.Media {
  message.Media(
    url: Some(url),
    content_type: Some("image/png"),
    flags: Some(0),
    nsfw: False,
    filename: None,
  )
}

fn data() -> MessageData {
  message.MessageData(
    ..message.empty_data(),
    channel_id: Some(11),
    id: Some(33),
  )
}

fn attached(data: MessageData, url: String) -> MessageData {
  message.MessageData(..data, attachments: [Some(image(url))])
}

fn embedded(data: MessageData, url: String) -> MessageData {
  message.MessageData(..data, embeds: [
    Some(
      message.Embed(
        image: Some(image(url)),
        thumbnail: None,
        title: None,
        description: None,
        fields: [],
      ),
    ),
  ])
}

pub fn media_proxy_attachment_test() {
  assert image_url.extract(attached(data(), media_url)) == Some(media_url)
}

pub fn own_api_attachment_has_no_image_test() {
  assert image_url.extract(attached(data(), api_url)) == None
  assert image_url.extract(attached(data(), plain_api_url)) == None
  assert image_url.extract(embedded(data(), api_url)) == None
  let shouted =
    "HTTPS://API.EXAMPLE.TEST/CHANNELS/11/ATTACHMENTS/22/CAT.PNG?a=1&MID=33#x"
  assert image_url.extract(attached(data(), shouted)) == None
}

pub fn own_api_attachment_falls_back_to_the_next_image_test() {
  let both = embedded(attached(data(), api_url), media_url)
  assert image_url.extract(both) == Some(media_url)
}

pub fn links_of_other_messages_keep_their_image_test() {
  let other_message = message.MessageData(..data(), id: Some(34))
  assert image_url.extract(attached(other_message, api_url)) == Some(api_url)
  let other_channel = message.MessageData(..data(), channel_id: Some(12))
  assert image_url.extract(attached(other_channel, api_url)) == Some(api_url)
  let unknown = message.MessageData(..message.empty_data(), id: None)
  assert image_url.extract(attached(unknown, api_url)) == Some(api_url)
  let bare = "https://example.test/channels/11/attachments/22/cat.png"
  assert image_url.extract(attached(data(), bare)) == Some(bare)
  let prefixed =
    "https://api.example.test/channels/11/attachments/22/cat.png?xmid=33"
  assert image_url.extract(attached(data(), prefixed)) == Some(prefixed)
  let longer =
    "https://api.example.test/channels/11/attachments/22/cat.png?mid=330"
  assert image_url.extract(attached(data(), longer)) == Some(longer)
}
