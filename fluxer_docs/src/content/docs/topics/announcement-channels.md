---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Announcement channels
description: Publishing messages from an announcement channel into the channels that follow it.
---

An announcement channel is a guild text channel, [type](/http-api/channels/#channel-types) `5`, whose messages can be published. Other channels follow it, and each published message is copied into every following channel. The copy is posted by a channel follower [webhook](/http-api/webhooks/#webhook-types) that lives in the following channel.

Every guild can create announcement channels. [Create guild channel](/http-api/guild-channels/#create-guild-channel) accepts `type` `5`, and an announcement channel has the fields, permissions, and slowmode of a text channel.

## Converting a channel

[Modify channel](/http-api/channels/#modify-channel) converts a guild text channel into an announcement channel and back through its `type` field. No other conversion exists. The caller needs `MANAGE_CHANNELS`, the same as for any other channel change.

A text channel that receives copies from a followed announcement channel cannot become an announcement channel, because an announcement channel cannot follow another one. Delete its channel follower webhooks first.

Converting an announcement channel into a text channel removes every follow of it. The copies already delivered stay, and they keep receiving later edits and deletions of the messages they came from.

## Publishing

[Crosspost message](/http-api/messages/#crosspost-message) publishes one message. The author needs `SEND_MESSAGES`. Publishing another member's message, or a webhook's, also needs `MANAGE_MESSAGES`. A message is published once, and only a `DEFAULT` message that is not a reply, a forward, or a copy can be published.

Publishing sets the `CROSSPOSTED` [message flag](/http-api/messages/#message-flags) and returns at once. The copies are created in the background, usually within seconds. A channel that follows the announcement channel after a message was published never receives that message.

## Following

[Follow announcement channel](/http-api/channels/#follow-announcement-channel) creates the channel follower webhook in a guild text channel of any guild, including the announcement channel's own guild. The caller needs `VIEW_CHANNEL` on the announcement channel and `MANAGE_WEBHOOKS` in the target, so a guild receives copies only in a channel one of its webhook managers chose. The follow posts a `CHANNEL_FOLLOW_ADD` [system message](/http-api/messages/#message-types) in the target channel.

[Get channel follower stats](/http-api/channels/#get-channel-follower-stats) counts the following channels and their guilds. Deleting the channel follower webhook with [Delete webhook](/http-api/webhooks/#delete-webhook) unfollows.

## What a copy has

A copy is an ordinary message in the following channel with the `IS_CROSSPOST` flag. Its `message_reference` has type `DEFAULT` and names the published message, its channel, and its guild. It never has `referenced_message`.

| Part | In the copy |
| --- | --- |
| Author | The channel follower webhook, with its name and the source guild icon at the time of delivery |
| Content | The published content, character for character |
| Embeds | Every embed, with the same URLs as in the published message |
| Attachments | The attachments of the published message, in order, with the same IDs, file names, sizes, and metadata |
| Stickers | The same stickers |
| Flags | `IS_CROSSPOST` plus `SUPPRESS_EMBEDS`, `SUPPRESS_NOTIFICATIONS`, and `VOICE_MESSAGE` from the source |
| Mentions | None, so an `@everyone` in the published message notifies nobody in a following guild |

A copy has no files of its own. The `url` and `proxy_url` of each attachment in a copy point at the file of the published message, in the announcement channel, so every copy serves the same file. An attachment of a copy expires when the attachment of the published message does. An attachment removed from the published message by an edit leaves every copy, and one added by an edit appears in every copy.

The follower webhook is named after the source guild and the announcement channel, as in `Fluxer #updates`, and its avatar is a copy of the source guild icon at the time of the follow. The following guild can rename it, and later copies use the new name. Its avatar cannot be changed and does not follow later icon changes.

A new copy uses the source guild icon at the time of delivery as its author avatar, not the webhook avatar. When the source guild has no icon, the copy has a null avatar and shows the default avatar for the webhook ID. Copies already delivered keep their name and avatar, edits included.

[Get message crosspost source](/http-api/messages/#get-message-crosspost-source) returns the public profile of the source guild of a copy, so a reader who is not a member of it can see where the copy came from.

## Content rules

An age restriction and a content warning each resolve through the channel, then its parent category, and then its guild.

- An age-restricted announcement channel can only be followed into an age-restricted channel.
- An announcement channel with a content warning can only be followed into a channel that has a content warning or an age restriction.
- An attachment or embed flagged as explicit media is left out of a copy when the following channel does not allow explicit media.

Fluxer checks the first two rules when a follow is created and when a channel follower webhook is moved, and again at every delivery and update. When a following channel stops meeting them, new copies are skipped, and an update turns an existing copy into a deleted-source placeholder.

A published message whose content, rich embed text, or attachment hash matches an instance blocklist cannot be published. A later edit that matches one turns every copy into a placeholder.

## Delivery

Delivery is asynchronous. Fluxer retries a failed delivery to one channel 8 times over about 80 minutes and then drops it. A channel that already has its copy is never given a second one.

A follow is paused while the account that created it cannot view the announcement channel, for example after leaving the source guild or losing `VIEW_CHANNEL` there. A paused follow receives no copies, and its webhook has no `source_guild` or `source_channel`. Messages published after access returns are delivered again. Messages published during the pause are not. An edit of a published message during the pause turns its copy in that channel into a deleted-source placeholder.

Nothing is delivered from a guild that has [ANNOUNCEMENT_CHANNELS_DISABLED](/http-api/guilds/#guild-features), from an unavailable guild, or from a guild whose message sending is disabled. A following guild that is unavailable or has message sending disabled receives nothing either.

## Edits and deletions

An edit of a published message reaches every copy that still exists, including copies in channels that have since unfollowed. Fluxer batches the edits of one message into 10-second windows and copies the latest state once per window, so a copy can briefly lag the published message. Each copy is announced by [Message Update](/gateway/events/#message-update).

Deleting a published message keeps each copy and edits it: the content becomes `[Original message deleted]`, attachments, embeds, and stickers are removed, `edited_timestamp` is set, and the `SOURCE_MESSAGE_DELETED` flag is added. The files of the published message are deleted with it. Deleting a copy removes that copy alone, and the files it showed stay with the published message.

When an instance administrator removes a published message, every copy is deleted outright. When an instance administrator removes a copy, the published message and every other copy are deleted too, so identical content leaves every guild at once. A published message that link preview moderation deletes takes its copies with it the same way.

## Cleanup

| Event | Follows | Copies |
| --- | --- | --- |
| Announcement channel deleted | Removed | Become deleted-source placeholders |
| Announcement channel converted to text | Removed | Stay, and keep receiving edits and deletions |
| Source guild deleted by its owner | Removed | Become deleted-source placeholders |
| Source guild deleted by an administrator | Removed | Deleted |
| Following channel deleted | Removed with the channel | Deleted with the channel |
| Follower webhook deleted | Removed | Stay, and keep receiving edits and deletions |
| Follower webhook moved | Kept | Stay in the previous channel |

The removal runs in the background after the triggering request. Each removed follow emits [Webhooks Update](/gateway/events/#webhooks-update) for its channel and writes no audit log entry. A follow or unfollow made through the API records `WEBHOOK_CREATE` or `WEBHOOK_DELETE` in the following guild's [audit log](/http-api/guild-audit-logs/).

## Limits

| Limit | Value |
| --- | --- |
| Publishes from one announcement channel | 10 in a row, then one every 6 minutes |
| Edits of one published message | 3 in a row, then one every 20 minutes |
| Webhooks in the following channel | 15, the `max_webhooks_per_channel` default, follower webhooks included |
| Webhooks in the following guild | 1000, the `max_webhooks_per_guild` default, follower webhooks included |

An announcement channel has no cap on its follows, and a published message has no cap on its attachment size beyond the upload limits. The webhook limits are resolved against the following guild. The allowances are [enforced inside the handler](/topics/rate-limits/#limits-enforced-inside-a-handler) and answer 429 with their own codes. An edit by another member who holds `MANAGE_MESSAGES` draws on no edit allowance and still reaches the copies.
