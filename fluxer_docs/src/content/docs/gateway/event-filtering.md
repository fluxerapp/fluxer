---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Event filtering
description: The four gates a Dispatch passes before it reaches a socket, and the two client controls.
---

A [Dispatch](/gateway/events/) is one event Fluxer sends to a connected client. Each one passes four independent gates on its way to a socket, and a client shapes its traffic with [Lazy Request](/gateway/commands/#lazy-request) subscriptions and the [Identify](/gateway/commands/#identify) `ignored_events` list.

Fluxer has no intent bitfield. A client ported from a protocol that uses intents replaces its intent mask with those two mechanisms. There is no `intents` field, no intent close code, and no privileged-intent approval.

## The four gates

Fluxer evaluates a guild-scoped Dispatch against these gates in order.

| Gate | Name | What it decides |
| --- | --- | --- |
| 1 | Guild availability | Whether the guild dispatches anything except [Guild Update](/gateway/events/#guild-update) |
| 2 | Permission and visibility | Which sessions may see the event at all |
| 3 | Guild subscription state | Whether a passive session in a large guild falls inside the fixed subset that still receives it |
| 4 | Session-level filters | Whether the shard filter, and then the `ignored_events` list, drops it inside the session after the guild has already chosen the recipients |

A guild with the `UNAVAILABLE_FOR_EVERYONE` or `UNAVAILABLE_FOR_EVERYONE_BUT_STAFF` feature fails gate 1. `UNAVAILABLE_FOR_EVERYONE_BUT_STAFF` decides whether the guild hands a session its full state or an `unavailable` stub when the session connects. Gate 1 has no staff exemption, so a staff session also receives nothing but Guild Update while the feature is set.

An account-scoped Dispatch skips gates 1 through 3 and is subject only to gate 4. Direct message traffic, relationship changes, and account record changes arrive that way.

## Permission and visibility

The guild resolves each event to one of five recipient sets.

| Event class | Events | Recipients |
| --- | --- | --- |
| Channel-scoped | [Channel Create](/gateway/events/#channel-create), [Channel Update](/gateway/events/#channel-update), [Channel Delete](/gateway/events/#channel-delete)<sup>1</sup>, [Message Create](/gateway/events/#message-create), [Message Delete Bulk](/gateway/events/#message-delete-bulk), [Typing Start](/gateway/events/#typing-start), [Channel Pins Update](/gateway/events/#channel-pins-update), [Webhooks Update](/gateway/events/#webhooks-update) | Sessions that can view the channel |
| Message-access filtered | [Message Update](/gateway/events/#message-update), [Message Delete](/gateway/events/#message-delete), [Message Reaction Add](/gateway/events/#message-reaction-add), [Message Reaction Remove](/gateway/events/#message-reaction-remove), [Message Reaction Remove All](/gateway/events/#message-reaction-remove-all), [Message Reaction Remove Emoji](/gateway/events/#message-reaction-remove-emoji) | Sessions that can view the channel and can access that message |
| Invite | [Invite Create](/gateway/events/#invite-create), [Invite Delete](/gateway/events/#invite-delete) | Sessions holding `MANAGE_CHANNELS` on the invite's channel<sup>2</sup> |
| Audit log | [Guild Audit Log Entry Create](/gateway/events/#guild-audit-log-entry-create) | Sessions holding `VIEW_AUDIT_LOG` in the guild |
| Guild-wide | Everything else | Every session connected to the guild |

<sup>1</sup> Channel Delete is filtered against the guild state as it was before the deletion, so the session that could see the channel is the session that learns it is gone

<sup>2</sup> The channel is read from the payload's `channel_id`, and from a nested `channel.id` when that field is absent. An invite payload with neither field reaches no session

Every one of those sets also excludes a session whose connection to the guild is still in flight, so a session receives none of the events above until the guild has given it that initial state.

Channel visibility is `VIEW_CHANNEL` on the channel, plus two extensions. A category is visible when at least one of its children is visible. A user with a live voice connection in a channel keeps virtual access to it whenever the channel would otherwise stop being visible. That covers a role or overwrite change removing `VIEW_CHANNEL`, and a move into a channel the user cannot view. Virtual access is keyed by user, so it applies to every session of that user. It is dropped when the user's voice connection to the channel ends.

Message access is `READ_MESSAGE_HISTORY` on the channel. Without that permission a session still receives events for messages newer than the guild's message history cutoff. A guild that sets no cutoff offers no such fallback, so a session without `READ_MESSAGE_HISTORY` receives none of the message-access filtered events there.

[Channel Update Bulk](/gateway/events/#channel-update-bulk) is filtered twice. The guild chooses the recipient set guild-wide, and then each recipient's copy of the payload keeps only the channels that recipient can view. A recipient left with an empty `channels` array receives no Dispatch at all.

[Voice State Update](/gateway/events/#voice-state-update) takes its own path. The guild sends it straight to the sessions that can view the channel the voice state names, or, when the state names no channel, the channel the user just left. A voice state with no connection ID is not broadcast at all.

### Excluding the acting session

[Message Reaction Add](/gateway/events/#message-reaction-add) and [Message Reaction Remove](/gateway/events/#message-reaction-remove) accept a `session_id` on the originating HTTP request. Fluxer excludes that session from the resulting Dispatch and strips the field before it sends the payload. Every other mutation delivers its Dispatch to the acting session like any other eligible session.

## Active and passive guilds

A user session is passive in every guild until [Lazy Request](/gateway/commands/#lazy-request) marks that guild `active: true`. A bot session is never passive. A guild with 250 members or fewer is active for every session, so the rule below applies only to a passive user session in a guild with more than 250 members.

Such a session receives exactly this set:

- [Guild Update](/gateway/events/#guild-update)
- [Guild Delete](/gateway/events/#guild-delete)
- [Guild Role Update](/gateway/events/#guild-role-update) and [Guild Role Update Bulk](/gateway/events/#guild-role-update-bulk)
- [Channel Create](/gateway/events/#channel-create), [Channel Update](/gateway/events/#channel-update), [Channel Update Bulk](/gateway/events/#channel-update-bulk), and [Channel Delete](/gateway/events/#channel-delete)
- [Guild Audit Log Entry Create](/gateway/events/#guild-audit-log-entry-create)
- [Passive Updates](/gateway/events/#passive-updates)
- [Message Create](/gateway/events/#message-create) when the message mentions the session's user
- [Guild Member Update](/gateway/events/#guild-member-update) and [Guild Member Remove](/gateway/events/#guild-member-remove) when the subject is the session's own user

Every other Dispatch the guild produces is suppressed:

- [Message Update](/gateway/events/#message-update), [Message Delete](/gateway/events/#message-delete), and [Message Delete Bulk](/gateway/events/#message-delete-bulk)
- Every reaction event
- [Invite Create](/gateway/events/#invite-create) and [Invite Delete](/gateway/events/#invite-delete)
- [Channel Pins Update](/gateway/events/#channel-pins-update)
- [Webhooks Update](/gateway/events/#webhooks-update)
- [Guild Member Add](/gateway/events/#guild-member-add)
- [Guild Role Create](/gateway/events/#guild-role-create) and [Guild Role Delete](/gateway/events/#guild-role-delete)
- [Guild Ban Add](/gateway/events/#guild-ban-add) and [Guild Ban Remove](/gateway/events/#guild-ban-remove)
- [Guild Emojis Update](/gateway/events/#guild-emojis-update) and [Guild Stickers Update](/gateway/events/#guild-stickers-update)

[Voice State Update](/gateway/events/#voice-state-update) never reaches this gate. It takes the separate path described above, so a passive session in a large guild still receives it for every channel it can view.

A message mentions the session's user when the payload names that user in `mentions`, names one of that user's roles in `mention_roles`, sets `mention_here`, or sets `mention_everyone`. The guild reads only the first 100 entries of `mention_roles`, so a message that mentions more roles than that can miss a passive recipient.

Every 30 seconds a passive session receives [Passive Updates](/gateway/events/#passive-updates). The payload has the changed per-channel `last_message_id` watermarks and the changed voice states for the channels it can view, so the session keeps unread state and voice rosters correct without the suppressed message events. A cycle that finds nothing changed sends nothing.

### Typing is decided separately

[Typing Start](/gateway/events/#typing-start) never follows the rule above. When the session set `typing` for the guild through [Lazy Request](/gateway/commands/#lazy-request), that value alone decides delivery. Without an override the event follows the active state, so a passive session in a large guild does not receive it.

The override applies to every session, including a bot session. A bot suppresses Typing Start in one guild through that override alone.

### Member lists

[Guild Member List Update](/gateway/events/#guild-member-list-update) has its own subscription. A session receives it only for a channel it named in `member_list_channels`, and only while it can view that channel and holds `VIEW_CHANNEL_MEMBERS` on it. One session holds at most one member list subscription per guild.

## Presence subscriptions

[Presence Update](/gateway/events/#presence-update) skips gates 1 through 3 and is subject only to gate 4. A guild dispatches it only to a session that named the subject in the `members` array of a [Lazy Request](/gateway/commands/#lazy-request), that can view at least one channel the subject can view, and that does not belong to the subject.

A session that no longer shares a viewable channel with the subject is dropped from that subject's subscriber set, so a client that regains access MUST resend `members` to restore delivery. Each `members` array replaces the session's previous subscription set for that guild.

The session holds a presence back in two cases. Every presence that arrives before [Ready](/gateway/events/#ready) is held. Fluxer releases the queue once it has dispatched Ready. A held presence whose subject already appears in the Ready `presences` array is dropped, and the session sends the rest in one burst. When Ready has not been dispatched within 10,000 milliseconds of session start, a fallback timer releases the queue. After that the session buffers a guild presence whose `guild_id` names a guild it is not connected to, and an account-scoped presence for a user that is neither a friend nor a recipient of a group direct message it belongs to.

A bot session holds no friend or group direct message presence subscriptions, so a bot receives a presence through this guild path alone.

## Ignored events

Identify accepts `ignored_events`, an array of up to 256 Dispatch event names. Fluxer upper-cases and deduplicates the names at Identify. An absent field and a JSON `null` both mean the empty list. Any other value that is not an array of strings, and any array holding more than 256 entries, close the connection with `4002` and reason `Invalid identify payload`. A Dispatch whose `t` appears in the list is dropped and never enters the replay buffer.

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

One exception overrides the list. [Message Create](/gateway/events/#message-create) is delivered even when `MESSAGE_CREATE` is ignored, if the message names the session's user in `mentions`, sets `mention_here`, or sets `mention_everyone`. A role mention does not defeat the list.

A suppressed Dispatch consumes no sequence number, so a client MUST NOT expect a gap in the sequence where the list dropped one.

The list is fixed for the lifetime of the session. Changing it requires a new Identify.

:::note[The guild computes an ignored event anyway]
`ignored_events` reduces socket traffic and replay pressure.
:::

## The shard filter

A session that identified with a `shard` pair whose `shard_id` is not 0 drops every Dispatch that does not name a guild. A Dispatch names a guild through a non-empty `guild_id`, or through a non-empty `id` on a [Guild Create](/gateway/events/#guild-create), [Guild Update](/gateway/events/#guild-update), [Guild Delete](/gateway/events/#guild-delete), or [Guild Sync](/gateway/events/#guild-sync) payload. [Rate Limited](/gateway/events/#rate-limited), [Guild Counts Update](/gateway/events/#guild-counts-update), and [Channel Member Counts Update](/gateway/events/#channel-member-counts-update) answer a command the session sent, and each passes the gate whatever its payload holds.

Account-level traffic, direct message traffic, relationship changes, and calls therefore never reach a session on a shard other than 0.

A session on shard 0, and a session that identified without a `shard` pair, filter nothing at this gate. Fluxer still applies guild ownership at Identify, as [Sharding](/gateway/overview/#sharding) describes, so a shard 0 session is only ever connected to the guilds its shard owns.

## What a bot should send

A bot session is never passive, so `active` changes nothing for it and a typical bot needs no [Lazy Request](/gateway/commands/#lazy-request) at all. It shapes traffic with `ignored_events` and pulls members with [Request Guild Members](/gateway/commands/#request-guild-members).

A bot that does not process typing or reactions saves the most by ignoring `TYPING_START`, `MESSAGE_REACTION_ADD`, and `MESSAGE_REACTION_REMOVE`. Ignoring `PRESENCE_UPDATE` saves a bot nothing, because a bot receives no presence until it subscribes to one.
