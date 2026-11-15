---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Gateway threads
description: Thread events, thread subscriptions, forum unread counts and the thread session flag.
---

Fluxer sends [thread](/http-api/threads/) changes, thread member lists and forum unread counts as the Dispatch events below. A user session needs the `CHANNEL_THREADS` [session flag](#session-flag), and a bot session always gets them.

## Session flag

| Value | Name | Description |
| --- | --- | --- |
| 1 &lt;&lt; 2 | CHANNEL_THREADS | The client can parse thread, forum and media channels |

Set it in the [session flags](/gateway/commands/#session-flags) of Identify. A user session without it gets no thread, forum or media channels, no thread messages and none of the events on this page. Its channels and messages have no thread fields, and its permission values have the [thread permissions](/http-api/permissions/#feature-gated-permission-bits) cleared.

## Guild threads

`threads` on a [guild ready object](/gateway/events/#guild-ready-object) is an array of [thread objects](/http-api/threads/#thread-object). A user session receives the active threads it joined and can view, and a bot session receives every active thread it can view. Each entry has `member` for a thread the session joined.

## Dispatch events

| Event | Description |
| --- | --- |
| [Thread Create](#thread-create) | A thread is created or the session is added to one |
| [Thread Update](#thread-update) | A thread changes |
| [Thread Delete](#thread-delete) | A thread is deleted |
| [Thread List Sync](#thread-list-sync) | The session's active thread set is replaced |
| [Thread Member Update](#thread-member-update) | The session's own thread membership changes |
| [Thread Members Update](#thread-members-update) | Users join or leave a thread |
| [Thread Member List Update](#thread-member-list-update) | The full member list of a subscribed thread |
| [Forum Unreads](#forum-unreads) | Unread counts for forum posts |

A session that can view a thread gets its thread events when it is a bot session, when the user is a member or when the session turned on [`threads`](#lazy-request-thread-options). A [passive](/gateway/event-filtering/#active-and-passive-guilds) session gets fewer of them, as noted under each event.

Messages, reactions, typing and pins in a thread go to a session that can view the thread when it is a bot session, the user is a member, the session lists the thread in [`thread_member_lists`](#lazy-request-thread-options), or the session set `threads` and the guild is active for it, whatever the guild size. They also follow [Event filtering](/gateway/event-filtering/#permission-and-visibility).

### <span id="thread-create"></span>THREAD_CREATE

A thread is created or the session is added to one. The payload is the [thread object](/http-api/threads/#thread-object) with these fields.

| Field | Type | Description |
| --- | --- | --- |
| newly_created? | boolean | `true` when the thread was just created, absent when the session was added to an existing thread |
| member? | [thread member](/http-api/thread-members/#thread-member-object) object | The session's own membership |

A passive session gets it only when its user is a member.

### <span id="thread-update"></span>THREAD_UPDATE

A thread changes, and the payload is the [thread object](/http-api/threads/#thread-object). A message sent to an archived thread unarchives it and sends this event. A passive session gets it only for a thread it joined.

### <span id="thread-delete"></span>THREAD_DELETE

A thread is deleted.

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | The ID of the thread |
| guild_id | snowflake | The ID of the guild |
| parent_id | snowflake | The ID of the parent channel |
| type | integer | The [thread type](/http-api/threads/#thread-types) |

A passive session gets it only when its user is a member.

### <span id="thread-list-sync"></span>THREAD_LIST_SYNC

The session's active threads are replaced for a guild or for some of its channels.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | The ID of the guild |
| channel_ids? | array[snowflake] | The parent channels whose threads are replaced, absent for the whole guild |
| threads | array[[thread](/http-api/threads/#thread-object) object] | The active threads the session can view |
| members | array[[thread member](/http-api/thread-members/#thread-member-object) object] | The session's memberships in those threads |

A user session that did not set `threads` gets only the threads it joined unless `channel_ids` is present. Fluxer sends it after [Lazy Request](#lazy-request-thread-options) sets `threads` to true and when the session gets view access to channels with active threads. Resume does not replay it.

### <span id="thread-member-update"></span>THREAD_MEMBER_UPDATE

The session's own membership in a thread changes. The payload is the [thread member object](/http-api/thread-members/#thread-member-object) plus `guild_id`. Every session of the user that can view the thread receives it.

### <span id="thread-members-update"></span>THREAD_MEMBERS_UPDATE

Users join or leave a thread.

| Field | Type | Description |
| --- | --- | --- |
| id | snowflake | The ID of the thread |
| guild_id | snowflake | The ID of the guild |
| member_count | integer | The number of thread members, capped at 50 |
| added_members? | array[[thread member](/http-api/thread-members/#thread-member-object) object] | The users who joined, each with `member` and `presence` |
| removed_member_ids? | array[snowflake] | The IDs of the users who left |

`member` is the guild member or null. `presence` is the [presence object](/gateway/events/#presence-object) or null when the user is not visibly online. A passive session gets it only when its user was added or removed.

### <span id="thread-member-list-update"></span>THREAD_MEMBER_LIST_UPDATE

The full member list of a thread the session subscribed to with [`thread_member_lists`](#lazy-request-thread-options). Resume does not replay it.

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | The ID of the guild |
| thread_id | snowflake | The ID of the thread |
| members | array[thread member list item object] | The members of the thread, at most 1,000 |

Each item has `user_id`, `join_timestamp`, `flags`, `member` and `presence`. `presence` is a [presence object](/gateway/events/#presence-object) with status `offline` when the user is not visibly online.

### <span id="forum-unreads"></span>FORUM_UNREADS

The answer to [Request Forum Unreads](#request-forum-unreads).

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | The ID of the guild |
| channel_id | snowflake | The ID of the forum or media channel |
| threads | array[forum unread object] | One entry for each requested post found in the channel |

Each forum unread object has `thread_id` and either `count` or `missing`. `count` is the number of messages after the `ack_message_id` in the request, capped at 25. `missing` is true when the request had no valid `ack_message_id` for that post.

## <span id="lazy-request-thread-options"></span>Lazy Request thread options

The [guild subscription object](/gateway/commands/#guild-subscription-object) of [Lazy Request](/gateway/commands/#lazy-request) takes two more options. Fluxer ignores them for a user session without `CHANNEL_THREADS`.

| Field | Type | Description |
| --- | --- | --- |
| threads? | boolean | Whether the session gets events and messages for every thread it can view in the guild |
| thread_member_lists? | array[snowflake] | The threads whose member lists the session subscribes to, at most 10 |

`threads` set to false, or marking the guild passive, clears the subscription.

`thread_member_lists` replaces the previous list, and an empty array clears it. Fluxer sends a [Thread Member List Update](#thread-member-list-update) right away for each newly listed thread, and sends another 250 ms after that thread's members change.

## Request Forum Unreads

Opcode `28` requests unread counts for posts in one forum or media channel. Fluxer closes a user session without `CHANNEL_THREADS` with [`4001`](/gateway/opcodes-and-close-codes/#close-codes).

| Field | Type | Description |
| --- | --- | --- |
| guild_id | snowflake | The ID of the guild |
| channel_id | snowflake | The ID of the forum or media channel |
| threads | array[forum unread request object] | The posts to count, at most 40 |

Each forum unread request object has `thread_id` and an optional `ack_message_id`, the newest message the client has read in that post.

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

Fluxer responds with one [Forum Unreads](#forum-unreads) and sends nothing when the guild is unavailable to the session, the channel is not a forum or media channel or the session cannot view it. A connection can send five of these commands in any 5 seconds, and Fluxer drops the rest.
