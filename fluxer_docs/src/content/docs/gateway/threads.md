---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Gateway threads
description: Thread Dispatch events, thread subscriptions, forum unread counts, and the thread session flag.
draft: true
---

A session receives [threads](/http-api/threads/) through the Dispatch events below. A guild ready object in [Ready](/gateway/events/#ready), [Guild Create](/gateway/events/#guild-create), and [Guild Sync](/gateway/events/#guild-sync) has `threads` only when the session can view threads in that guild. A client treats the presence of that key as the signal that threads, forums, and media channels exist in the guild.

:::note[Threads need the `channel_threads` experiment]
A user session receives thread data only when it sets `CHANNEL_THREADS` in Identify and the experiment is active for the guild and the account. A bot session needs no flag.
:::

## Session flag

| Value | Name | Description |
| --- | --- | --- |
| 1 &lt;&lt; 2 | CHANNEL_THREADS | The client can parse thread, forum, and media channels |

The flag joins the [session flags](/gateway/commands/#session-flags) of Identify. A user session without it sees no channel of type 11, 12, 15, or 16, and no thread event.

## Guild threads

`threads` on a guild ready object is an array of [thread objects](/http-api/threads/#thread-object). A user session receives the active threads it joined, each with `member`. A bot session receives every active thread it can view.

The `member` of each entry has no `id`, `user_id`, or `member`.

## Dispatch events

| Event | Description | Scope |
| --- | --- | --- |
| [Thread Create](#thread-create) | A thread was created or became visible | Thread |
| [Thread Update](#thread-update) | A thread changed | Thread |
| [Thread Delete](#thread-delete) | A thread was deleted | Thread |
| [Thread List Sync](#thread-list-sync) | The session's active thread set was replaced | Guild |
| [Thread Member Update](#thread-member-update) | The session's own membership changed | Current user |
| [Thread Members Update](#thread-members-update) | Users joined or left a thread | Thread |
| [Thread Member List Update](#thread-member-list-update) | A subscribed thread member list resynced | Command response |
| [Forum Unreads](#forum-unreads) | Unread counts for forum posts | Command response |

A session receives a thread event only when it can view the thread. Messages, reactions, typing, and pins in a thread reach a session that is a member of the thread, a bot session, or an active session that subscribed through `threads`. A message in a thread has `channel_type` set to the thread type.

### <span id="thread-create"></span>THREAD_CREATE

A thread was created, or the session was added to one. The payload is the [thread object](/http-api/threads/#thread-object) with the fields below.

| Field | Type | Description |
| --- | --- | --- |
| newly_created? | boolean | Whether the thread was just created, present only then |
| member? | [thread member](/http-api/thread-members/#thread-member-object) object | The session's own membership, present only for a member |

A user session receives it for a thread it joined, a thread it was added to, or any thread while subscribed through `threads`. A passive session receives it only when it was added.

### <span id="thread-update"></span>THREAD_UPDATE

A thread changed. The payload is the [thread object](/http-api/threads/#thread-object).

A new message and a changed counter emit no Thread Update. A passive session receives it only for a thread it joined.

### <span id="thread-delete"></span>THREAD_DELETE

A thread was deleted.

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | The ID of the thread |
| guild_id | snowflake | The ID of the guild |
| parent_id | snowflake | The ID of the parent channel |
| type | integer | The [thread type](/http-api/threads/#thread-types) |

Recipients are the sessions that could view the thread before the deletion.

### <span id="thread-list-sync"></span>THREAD_LIST_SYNC

The session's active threads were replaced for a guild or for some of its channels.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | The ID of the guild |
| channel_ids?<sup>1</sup> | array[snowflake] | The parent channels whose threads are replaced |
| threads | array[[thread](/http-api/threads/#thread-object) object] | The active threads the session can view |
| members | array[[thread member](/http-api/thread-members/#thread-member-object) object] | The session's memberships in those threads |

<sup>1</sup> Absent when the event replaces every thread in the guild

Fluxer sends it after `threads` is set to true in [Lazy Request](#lazy-request-thread-options), within 5,000 ms, and when the session gains access to a channel that has threads.

### <span id="thread-member-update"></span>THREAD_MEMBER_UPDATE

The session's own membership in a thread changed. The payload is the [thread member object](/http-api/thread-members/#thread-member-object) with `guild_id`.

### <span id="thread-members-update"></span>THREAD_MEMBERS_UPDATE

Users joined or left a thread.

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | The ID of the thread |
| guild_id | snowflake | The ID of the guild |
| member_count | integer | The number of thread members, capped at 50 |
| added_members?<sup>1</sup> | array[[thread member](/http-api/thread-members/#thread-member-object) object] | The users who joined |
| removed_member_ids? | array[snowflake] | The IDs of the users who left |

<sup>1</sup> Each entry adds `member`, the guild member, and `presence`, the [presence object](/gateway/events/#presence-object). Either is null when Fluxer has none

A bot session receives every Thread Members Update it can view. A user session receives it when it was added or removed, is a member, or subscribed through `threads`. A passive session receives it only when it was added or removed.

### <span id="thread-member-list-update"></span>THREAD_MEMBER_LIST_UPDATE

A thread member list the session subscribed to through `thread_member_lists` resynced.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | The ID of the guild |
| thread_id | snowflake | The ID of the thread |
| members<sup>1</sup> | array[thread member list item object] | The members of the thread, at most 1,000 |

<sup>1</sup> Each item is `user_id`, `join_timestamp`, `flags`, `member`, and `presence`, where `member` is the guild member or null

The event is delivered live and never retained for Resume.

### <span id="forum-unreads"></span>FORUM_UNREADS

The answer to [Request Forum Unreads](#request-forum-unreads).

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | The ID of the guild |
| channel_id | snowflake | The ID of the forum or media channel |
| threads | array[forum unread object] | One entry for each requested post the session can view |

Each forum unread object is `thread_id` with either `count` or `missing`. `count` is the number of unread messages after the sent `ack_message_id`, capped at 25. `missing` is true when the request sent no `ack_message_id` for that post.

## <span id="lazy-request-thread-options"></span>Lazy Request thread options

The [guild subscription object](/gateway/commands/#guild-subscription-object) of [Lazy Request](/gateway/commands/#lazy-request) takes two more options.

| Field | Type | Description |
| --- | --- | --- |
| threads? | boolean | Whether the session receives every thread event and thread message in the guild |
| thread_member_lists? | array[snowflake] | The threads whose member lists the session subscribes to, at most 10 |

Fluxer ignores both options for a user session that did not set `CHANNEL_THREADS`. A thread the session cannot view is dropped from `thread_member_lists`. Each newly listed thread produces a [Thread Member List Update](#thread-member-list-update), and a membership change in it produces another.

## Request Forum Unreads

Opcode `28` requests unread counts for posts in one forum or media channel.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | The ID of the guild |
| channel_id | snowflake | The ID of the forum or media channel |
| threads | array[forum unread request object] | The posts to count, deduplicated and truncated to 40 |

Each forum unread request object is `thread_id` and an optional `ack_message_id`, the newest message the client has read in that post.

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

Results arrive in one [Forum Unreads](#forum-unreads), where a post outside the channel has no entry. A channel the session cannot view, and a request with no valid `thread_id`, produce no Dispatch. A session that cannot view threads closes with `4001` and reason `Unknown opcode`.

A session sends at most five Request Forum Unreads commands in a rolling 5-second window. A further command is discarded without closing the connection.
