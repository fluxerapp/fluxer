---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Event filtering
description: Which sessions receive each Dispatch, and how to receive fewer of them.
---

Fluxer has no gateway intents, and you limit [Dispatch](/gateway/events/) traffic with [Lazy Request](/gateway/commands/#lazy-request) subscriptions, the [Identify](/gateway/commands/#identify) `ignored_events` list and the `CHANNEL_THREADS` [session flag](/gateway/threads/#session-flag).

## Permission and visibility

Fluxer sends each guild event to the sessions in the Recipients column. A session that is still connecting to the guild gets none of them.

| Event class | Recipients |
| --- | --- |
| Channel create, update and delete, [Message Create](/gateway/events/#message-create), [Message Delete Bulk](/gateway/events/#message-delete-bulk), [Typing Start](/gateway/events/#typing-start), [Channel Pins Update](/gateway/events/#channel-pins-update), [Webhooks Update](/gateway/events/#webhooks-update) | Sessions that can view the channel |
| [Message Update](/gateway/events/#message-update), [Message Delete](/gateway/events/#message-delete) and the reaction events | Sessions that can view the channel and access the message |
| [Invite Create](/gateway/events/#invite-create) and [Invite Delete](/gateway/events/#invite-delete) | Sessions with `MANAGE_CHANNELS` on the invite's channel |
| [Guild Audit Log Entry Create](/gateway/events/#guild-audit-log-entry-create) | Sessions with `VIEW_AUDIT_LOG`. Entries about threads need [thread access](/gateway/threads/#session-flag). |
| [Voice State Update](/gateway/events/#voice-state-update) | Sessions that can view the voice state's channel or the channel the user left |
| Thread events | Listed for each event on [Gateway threads](/gateway/threads/#dispatch-events) |
| Everything else | Every session connected to the guild |

A session can view a channel when it has `VIEW_CHANNEL`, and it can view a category when it can view one of the category's channels. A user in a voice channel can still view it until they leave, even after a permission change that would hide it.

A session can access a message when it has `READ_MESSAGE_HISTORY` or when the message is newer than the guild's [message history cutoff](/http-api/guilds/#guild-object).

[Channel Update Bulk](/gateway/events/#channel-update-bulk) has only the channels the session can view.

When you pass the `session_id` query parameter to [Add own reaction](/http-api/messages/#add-own-reaction), [Remove own reaction](/http-api/messages/#remove-own-reaction) or [Remove another user's reaction](/http-api/messages/#remove-another-users-reaction), that session gets no [Message Reaction Add](/gateway/events/#message-reaction-add) or [Message Reaction Remove](/gateway/events/#message-reaction-remove) for the change.

A guild with the `UNAVAILABLE_FOR_EVERYONE` or `UNAVAILABLE_FOR_EVERYONE_BUT_STAFF` feature sends only [Guild Update](/gateway/events/#guild-update).

## Active and passive guilds

A user session is passive in every guild until a [Lazy Request](/gateway/commands/#lazy-request) sets `active: true` for that guild. The guild in [Identify](/gateway/commands/#identify) `initial_guild_id` starts active. Bot sessions are never passive, and every session is active in a guild with 250 members or fewer.

A passive session in a larger guild gets only these events.

- [Guild Update](/gateway/events/#guild-update) and [Guild Delete](/gateway/events/#guild-delete)
- [Guild Role Update](/gateway/events/#guild-role-update) and [Guild Role Update Bulk](/gateway/events/#guild-role-update-bulk)
- [Channel Create](/gateway/events/#channel-create), [Channel Update](/gateway/events/#channel-update), [Channel Update Bulk](/gateway/events/#channel-update-bulk) and [Channel Delete](/gateway/events/#channel-delete)
- [Guild Audit Log Entry Create](/gateway/events/#guild-audit-log-entry-create)
- [Message Create](/gateway/events/#message-create) and [Message Update](/gateway/events/#message-update) for a message that mentions the session's user, and Message Create for the user's own messages
- [Guild Member Update](/gateway/events/#guild-member-update) and [Guild Member Remove](/gateway/events/#guild-member-remove) for the session's own user
- [Voice State Update](/gateway/events/#voice-state-update) for channels it can view
- Thread events that [Gateway threads](/gateway/threads/#dispatch-events) lists for passive sessions
- [Passive Updates](/gateway/events/#passive-updates)

A message mentions the user when `mentions` has the user, `mention_roles` has one of the user's roles, or `mention_here` or `mention_everyone` is set.

[Passive Updates](/gateway/events/#passive-updates) comes every 30 seconds with the changed `last_message_id` values and voice states for channels the session can view, and Fluxer skips it when nothing changed.

### Typing is decided separately

When you set `typing` for a guild in [Lazy Request](/gateway/commands/#lazy-request), that value alone decides whether the session gets [Typing Start](/gateway/events/#typing-start), including for bots. Without it the event follows the active state.

### Events in a thread

A message, reaction, typing or pin event in a thread also has to meet one of the conditions on [Gateway threads](/gateway/threads/#dispatch-events). The [`threads`](/gateway/threads/#lazy-request-thread-options) condition applies only while the guild is active for the session.

### Member lists

[Guild Member List Update](/gateway/events/#guild-member-list-update) goes to a session only for a channel in its `member_list_channels` that it can view with `VIEW_CHANNEL_MEMBERS`. A session has one member list subscription per guild.

## Visited guilds

A session [visiting](/http-api/guilds/#visiting-guilds) a guild gets only these events for it.

- [Guild Update](/gateway/events/#guild-update) and the role, emoji and sticker events
- Channel create, update and delete events for channels a visitor can read, with only the @everyone overwrite
- [Channel Pins Update](/gateway/events/#channel-pins-update) for channels a visitor can read
- Message and reaction events for the channel set as `focused_channel_id` in [Lazy Request](/gateway/commands/#lazy-request)
- [Thread create, update and delete events](/gateway/threads/#dispatch-events) for public threads in channels a visitor can read

Visitors never appear in member lists, presences, typing, voice states or member counts. Fluxer ignores member list ranges, [Request Guild Members](/gateway/commands/#request-guild-members), read acknowledgments and typing from a session for a visited guild.

## Presence subscriptions

A session gets a [Presence Update](/gateway/events/#presence-update) for a guild member only if it listed that user in the `members` array of a [Lazy Request](/gateway/commands/#lazy-request) and can view a channel the user can view. Each `members` array replaces the previous set for that guild. If the session loses access to every channel it shares with a user, send that user in `members` again once access comes back.

## Ignored events

`ignored_events` in Identify is an array of up to 256 Dispatch event names. Fluxer drops each listed event for the session, and the list can only change with a new Identify. An invalid value closes the connection with `4002`.

Fluxer still sends [Message Create](/gateway/events/#message-create) when `MESSAGE_CREATE` is ignored if the message mentions the user directly or uses `@here` or `@everyone`, but not for a role mention. A dropped event uses no sequence number.

```json
{
  "op": 2,
  "d": {
    "token": "...",
    "properties": {"os": "Linux", "browser": "bot", "device": "bot"},
    "ignored_events": ["TYPING_START", "PRESENCE_UPDATE"]
  }
}
```

## The shard filter

A session that identified with a `shard_id` other than 0 drops every Dispatch that has no guild. This filter does not drop [Rate Limited](/gateway/events/#rate-limited), [Guild Counts Update](/gateway/events/#guild-counts-update) or [Channel Member Counts Update](/gateway/events/#channel-member-counts-update), which are replies to a command the session sent. Account, direct message, relationship and call events arrive only on shard 0 or on a session that sent no `shard` pair (see [Sharding](/gateway/overview/#sharding)).

## What a bot should send

A bot needs no Lazy Request, and if it doesn't use typing or reactions, add `TYPING_START`, `MESSAGE_REACTION_ADD` and `MESSAGE_REACTION_REMOVE` to `ignored_events`.
