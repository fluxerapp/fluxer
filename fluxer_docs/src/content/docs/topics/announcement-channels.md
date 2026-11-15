---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Announcement channels
description: Publishing messages from an announcement channel into the channels that follow it.
---

An announcement channel is a guild channel of [type](/http-api/channels/#channel-types) `5`. When you publish a message in it, Fluxer copies the message into every channel that follows it. Each copy is posted by the [channel follower webhook](/http-api/webhooks/#webhook-types) in the following channel.

## Converting a channel

You can create one with [Create guild channel](/http-api/guild-channels/#create-guild-channel) and `type` set to `5`, or switch a text channel to an announcement channel and back with [Modify channel](/http-api/channels/#modify-channel). Both need `MANAGE_CHANNELS`.

Converting a text channel into an announcement channel returns 400 `CHANNEL_HAS_FOLLOWED_CHANNELS` while the text channel follows another announcement channel, and 400 `CHANNEL_HAS_THREADS` while it has a private thread. Delete its channel follower webhooks or private threads first.

## Publishing

[Crosspost message](/http-api/messages/#crosspost-message) publishes one message and sets its `CROSSPOSTED` [message flag](/http-api/messages/#message-flags). You need `SEND_MESSAGES`, and `MANAGE_MESSAGES` to publish someone else's message. You can publish a message only once, and only a `DEFAULT` message that isn't a reply, a forward or a copy from another announcement channel.

Fluxer creates the copies in the background. A channel that starts following after a message was published never gets that message.

## Following

[Follow announcement channel](/http-api/channels/#follow-announcement-channel) creates a channel follower webhook in a text channel of any guild. You need `VIEW_CHANNEL` on the announcement channel, and `VIEW_CHANNEL` and `MANAGE_WEBHOOKS` on the target channel. Fluxer posts a `CHANNEL_FOLLOW_ADD` [system message](/http-api/messages/#message-types) in the target channel.

[Get channel follower stats](/http-api/channels/#get-channel-follower-stats) counts the followers, and deleting the follower webhook with [Delete webhook](/http-api/webhooks/#delete-webhook) unfollows.

## What a copy has

A copy is a message with the `IS_CROSSPOST` flag and a `message_reference` to the published message.

| Part | In the copy |
| --- | --- |
| Author | The follower webhook, with a name like `Fluxer #updates`, with the source guild icon at delivery time |
| Content | The published content |
| Embeds | The same embeds |
| Attachments | The same attachments, with the same IDs and URLs |
| Stickers | The same stickers |
| Flags | `IS_CROSSPOST` plus `SUPPRESS_EMBEDS`, `SUPPRESS_NOTIFICATIONS` and `VOICE_MESSAGE` from the source |
| Mentions | None, and `@everyone` notifies nobody in the following guild |

When the published message is deleted or can't be shown, a copy becomes a placeholder with the content `[Original message deleted]` and the `SOURCE_MESSAGE_DELETED` flag.

[Get message crosspost source](/http-api/messages/#get-message-crosspost-source) returns the public profile of the source guild of a copy.

## Content rules

- An age-restricted announcement channel can only be followed into an age-restricted channel.
- An announcement channel with a content warning can only be followed into a channel with a content warning or an age restriction.
- Explicit media is left out of a copy when the following channel doesn't allow it.

Fluxer checks the age-restriction and content-warning rules when a follow is created, at every delivery and at every edit. Age restrictions and content warnings come from the channel, then its category, then its guild.

When a following channel stops meeting the age-restriction or content-warning rule, it gets no new copies, and its existing copies become placeholders the next time the published message is edited.

## Delivery

Fluxer retries a failed delivery 8 times over about 80 minutes and then drops it.

A follow is paused while the person who created it can't view the announcement channel, and messages published during the pause are never delivered. Editing a published message during the pause turns its copy in that channel into a placeholder.

Nothing is delivered from a guild with [ANNOUNCEMENT_CHANNELS_DISABLED](/http-api/guilds/#guild-features), or to or from a guild with message sending disabled. With `ANNOUNCEMENT_CHANNELS_DISABLED`, edits of published messages stop updating existing copies, except that attachments removed from a published message are also removed from its copies.

## Edits and deletions

Edits to a published message are applied to every copy, and each updated copy sends [Message Update](/gateway/events/#message-update). Deleting a published message turns every copy into a placeholder, and deleting a copy removes only that copy.

When an instance admin removes a published message or one of its copies, the published message and every copy are deleted.

## Cleanup

| Event | Follows | Copies |
| --- | --- | --- |
| Announcement channel deleted | Removed | Become placeholders |
| Announcement channel converted to text | Removed | Stay, and keep receiving edits and deletions |
| Source guild deleted by its owner | Removed | Become placeholders |
| Source guild deleted by an administrator | Removed | Deleted |
| Following channel deleted | Removed with the channel | Deleted with the channel |
| Follower webhook deleted | Removed | Stay, and keep receiving edits and deletions |
| Follower webhook moved | Kept | Stay in the previous channel |

Each removed follow sends [Webhooks Update](/gateway/events/#webhooks-update). Following or unfollowing through the API records `WEBHOOK_CREATE` or `WEBHOOK_DELETE` in the following guild's [audit log](/http-api/guild-audit-logs/).

## Limits

| Limit | Value |
| --- | --- |
| Publishes from one announcement channel | 10 in a row, then one every 6 minutes |
| Edits of one published message | 3 in a row, then one every 20 minutes |
| Webhooks in the following channel | 15 by default (`max_webhooks_per_channel`) |
| Webhooks in the following guild | 1000 by default (`max_webhooks_per_guild`) |

Follower webhooks count toward both webhook limits. Any number of channels can follow one announcement channel. The publish and edit limits return 429 with their own error codes, which are listed in [Rate limits](/topics/rate-limits/#announcement-channel-limits).
