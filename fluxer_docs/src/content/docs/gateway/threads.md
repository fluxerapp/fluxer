---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Gateway threads
description: Thread Dispatch events, thread subscriptions, forum unread counts, and the thread session flag.
---

A session receives [threads](/http-api/threads/) through the Dispatch events below. A [guild ready object](/gateway/events/#guild-ready-object) in [Ready](/gateway/events/#ready), [Guild Create](/gateway/events/#guild-create), and [Guild Sync](/gateway/events/#guild-sync) has `threads` only for a session with thread access.

:::note[User sessions need the `CHANNEL_THREADS` session flag]
A user session has thread access only when it sets `CHANNEL_THREADS` in Identify. A bot session always has thread access and needs no flag. HTTP routes read a separate client capability, as [Client capability](/http-api/threads/#client-capability) describes.
:::

## Session flag

| Value | Name | Description |
| --- | --- | --- |
| 1 &lt;&lt; 2 | CHANNEL_THREADS | The client can parse thread, forum, and media channels |

The flag joins the [session flags](/gateway/commands/#session-flags) of Identify. A user session without it receives no channel of type 10, 11, 12, 15, or 16 and no event from this page. Its guild ready objects leave out a category that is visible only through a forum or media channel inside it. Its channels have no `flags` and no thread or forum fields. Its roles and permission overwrites have the thread permission bits cleared. [Ready](/gateway/events/#ready) leaves out every read state that has `flags`. It receives no [Message ACK](/gateway/events/#message-ack) for an entry that has `flags`. It receives no [Channel Pins ACK](/gateway/events/#channel-pins-ack) for a thread and no [Saved Message Create](/gateway/events/#saved-message-create) for a message in a thread.

The same session receives no message of type 18 (`THREAD_CREATED`) or 21 (`THREAD_STARTER_MESSAGE`). Its messages have no `thread` field. The message flags `HAS_THREAD` (`1 << 5`) and `FAILED_TO_MENTION_SOME_ROLES_IN_THREAD` (`1 << 8`) are cleared.

## Guild threads

`threads` on a guild ready object is an array of [thread objects](/http-api/threads/#thread-object). A user session receives the active threads it joined and can view, each with `member`. A bot session receives every active thread it can view, with `member` on the threads it joined.

The `member` of each entry has no `id`, `user_id`, `member`, or `presence`. A thread object in a guild ready object or in any event on this page has no `member_ids_preview`.

## Dispatch events

| Event | Description | Scope |
| --- | --- | --- |
| [Thread Create](#thread-create) | A thread is created or the session joins one | Thread visibility |
| [Thread Update](#thread-update) | A visible thread changes | Thread visibility |
| [Thread Delete](#thread-delete) | A thread is deleted | Thread visibility |
| [Thread List Sync](#thread-list-sync) | The session's active thread set is replaced | Guild connection |
| [Thread Member Update](#thread-member-update) | The session's own thread membership changes | Current user |
| [Thread Members Update](#thread-members-update) | Users join or leave a thread | Thread visibility |
| [Thread Member List Update](#thread-member-list-update) | A subscribed thread member list resyncs | Member list subscription |
| [Forum Unreads](#forum-unreads) | Unread counts for forum posts are returned | Command response |

A passive session in a guild with more than 250 members receives fewer thread events, as each event states.

Messages, reactions, typing, and pins in a thread reach a session that can view the thread and meets one of these conditions. An event in the message access set of [Event filtering](/gateway/event-filtering/#permission-and-visibility) also needs message access in the thread. These events also follow [Active and passive guilds](/gateway/event-filtering/#active-and-passive-guilds).

- The session is a bot session.
- The user is a member of the thread.
- The session lists the thread in `thread_member_lists`.
- The session set [`threads`](#lazy-request-thread-options) and the guild is active for it, through [Lazy Request](/gateway/commands/#lazy-request) `active` or the [Identify](/gateway/commands/#identify) `initial_guild_id`, whatever the guild size.

[Message Create](/gateway/events/#message-create) in a thread has `channel_type` set to the thread type.

### <span id="thread-create"></span>THREAD_CREATE

A thread was created, or the session was added to one. The payload is the [thread object](/http-api/threads/#thread-object) with the fields below.

| Field | Type | Description |
| --- | --- | --- |
| newly_created? | boolean | Always `true`, present only when the thread was just created |
| member? | [thread member](/http-api/thread-members/#thread-member-object) object | The session's own membership, present only for a member |

Recipients are the sessions that can view the thread and are a bot session, a member, or a session that set `threads` through [Lazy Request](#lazy-request-thread-options). A passive session in a guild with more than 250 members receives it only when its user is a member.

A user added to an existing thread receives a Thread Create with `member` and no `newly_created`. It arrives before the matching [Thread Members Update](#thread-members-update).

### <span id="thread-update"></span>THREAD_UPDATE

A thread changed. The payload is the [thread object](/http-api/threads/#thread-object).

Recipients are the sessions that can view the thread and are a bot session, a member, or a session that set `threads`. Fluxer checks them before and after the change and sends to both sets. A passive session in a guild with more than 250 members receives it only for a thread it joined.

A message sent to an archived thread unarchives it and emits Thread Update. A message sent to an active thread emits none. When a thread is unarchived, each session of a member that can view it also receives a [Thread Member Update](#thread-member-update).

### <span id="thread-delete"></span>THREAD_DELETE

A thread was deleted.

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | The ID of the thread |
| guild_id | snowflake | The ID of the guild |
| parent_id | snowflake | The ID of the parent channel |
| type | integer | The [thread type](/http-api/threads/#thread-types) |

Recipients are the sessions that can view the thread as it stood before the deletion and are a bot session, a member, or a session that set `threads`. A member is a user the guild holds as a member of the thread, or one of up to 1,000 thread members Fluxer reads at deletion. A passive session in a guild with more than 250 members receives it only when its user is a member.

### <span id="thread-list-sync"></span>THREAD_LIST_SYNC

The session's active threads were replaced for a guild or for some of its channels.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | The ID of the guild |
| channel_ids?<sup>1</sup> | array[snowflake] | The parent channels whose threads are replaced |
| threads<sup>2</sup> | array[[thread](/http-api/threads/#thread-object) object] | The active threads the session can view |
| members | array[[thread member](/http-api/thread-members/#thread-member-object) object] | The session's memberships in those threads |

<sup>1</sup> Absent when the event replaces every thread in the guild

<sup>2</sup> With `channel_ids`, every active thread in those channels. Without it, every active thread in the guild for a bot session and for a session that set `threads`

Without `channel_ids`, a user session that did not set `threads` receives the active threads it joined.

- Fluxer sends it when [Lazy Request](#lazy-request-thread-options) sets `threads` to true for a session that had not set it. The event follows after a random delay below 5,000 ms.
- Fluxer sends it when the guild reloads its thread state. A bot session, a session that set `threads`, and a user session that joined an active thread each receive one.
- Fluxer sends it when the session gains view access to channels that have active threads. `channel_ids` lists those channels.

A guild sends a burst of up to 20 full syncs, then 20 more each second, and queues the rest. Thread List Sync is delivered live and never retained for Resume.

### <span id="thread-member-update"></span>THREAD_MEMBER_UPDATE

The session's own membership in a thread changed. The payload is the [thread member object](/http-api/thread-members/#thread-member-object) with `guild_id`. It has `muted` and `mute_config`.

Every session of the user that can view the thread receives it, passive or not.

### <span id="thread-members-update"></span>THREAD_MEMBERS_UPDATE

Users joined or left a thread.

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | The ID of the thread |
| guild_id | snowflake | The ID of the guild |
| member_count | integer | The number of thread members, capped at 50 |
| added_members?<sup>1</sup> | array[[thread member](/http-api/thread-members/#thread-member-object) object] | The users who joined |
| removed_member_ids? | array[snowflake] | The IDs of the users who left |

<sup>1</sup> Each entry has no `muted` or `mute_config`, and adds `member` and `presence`

`member` is the guild member, or null when the guild has none for the user. `presence` is the [presence object](/gateway/events/#presence-object) with `user` reduced to `id`, or null when the user is not visibly online to the guild.

Every session of an added or removed user receives it. Any other session receives it when it can view the thread and is a bot session, a member, or a session that set `threads`. A passive session in a guild with more than 250 members receives it only when its user was added or removed.

### <span id="thread-member-list-update"></span>THREAD_MEMBER_LIST_UPDATE

A thread member list the session subscribed to through `thread_member_lists` resynced.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | The ID of the guild |
| thread_id | snowflake | The ID of the thread |
| members<sup>1</sup> | array[thread member list item object] | The members of the thread, at most 1,000 |

<sup>1</sup> Each item is `user_id`, `join_timestamp`, `flags`, `member`, and `presence`

`member` is the guild member or null. `presence` is the guild's [presence object](/gateway/events/#presence-object) when the user is visibly online to the guild, and otherwise the placeholder `{"status": "offline", "mobile": false, "afk": false}`.

The event is delivered live and never retained for Resume.

### <span id="forum-unreads"></span>FORUM_UNREADS

The answer to [Request Forum Unreads](#request-forum-unreads).

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | The ID of the guild |
| channel_id | snowflake | The ID of the forum or media channel |
| threads | array[forum unread object] | One entry for each requested post found in the channel |

Each forum unread object is `thread_id` with either `count` or `missing`. `count` is the number of messages after the sent `ack_message_id`, capped at 25. `missing` is true when the request sent no valid `ack_message_id` for that post.

## <span id="lazy-request-thread-options"></span>Lazy Request thread options

The [guild subscription object](/gateway/commands/#guild-subscription-object) of [Lazy Request](/gateway/commands/#lazy-request) takes two more options. Fluxer applies them after `typing`.

| Field | Type | Description |
| --- | --- | --- |
| threads? | boolean | Whether the session receives events and messages for every thread it can view in the guild |
| thread_member_lists? | array[snowflake] | The threads whose member lists the session subscribes to, at most 10 |

Fluxer ignores both options for a user session that did not set `CHANNEL_THREADS`. `threads` set to false clears the subscription, and any value that is not a Boolean is ignored. Fluxer also clears it when the session marks the guild passive.

`thread_member_lists` replaces the previous list, and an empty array clears it. Fluxer keeps the first 10 entries that are Snowflakes and removes duplicates. It then drops each thread the session cannot view or that is not active. A value that is not an array is ignored.

Each newly listed thread produces a [Thread Member List Update](#thread-member-list-update) at once. A membership change in the thread, or a presence change of one of its listed members, produces another after a 250 ms batching delay.

## Request Forum Unreads

Opcode `28` requests unread counts for posts in one forum or media channel.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | The ID of the guild |
| channel_id | snowflake | The ID of the forum or media channel |
| threads | array[forum unread request object] | The posts to count, deduplicated and truncated to 40 |

Each forum unread request object is `thread_id` and an optional `ack_message_id`, the newest message the client has read in that post. An entry with no valid `thread_id` is dropped. An `ack_message_id` that is not a valid Snowflake counts as absent.

```json
{
  "op": 28,
  "d": {
    "guild_id": "1189375284394692608",
    "channel_id": "1189375284394692700",
    "threads": [{"thread_id": "1189375284394692800", "ack_message_id": "1189375284394692900"}]
  }
}
```

Results arrive in one [Forum Unreads](#forum-unreads). A post that does not exist or is outside the channel has no entry. An archived post, and an active post that has a message after the sent `ack_message_id`, has no entry when Fluxer fails to count its messages.

No Dispatch follows in these cases.

- The guild or channel ID is not a valid Snowflake.
- The guild is not connected to the session.
- The channel is not a forum or media channel.
- The session cannot view the channel.
- The request has no valid `thread_id`.
- The connection has too many requests in progress.

A user session that did not set `CHANNEL_THREADS` closes with `4001` and reason `Unknown opcode`. A `d` that is not an object closes the same way.

Request Forum Unreads accepts five commands per WebSocket in a rolling 5-second window. A further command is discarded without closing the connection.
