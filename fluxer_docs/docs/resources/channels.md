# Channels

*We're still writing this page. More documentation is landing over the coming days.*

## Threads

Threads are sub-channels (channel type `11`) that live inside a text channel. They inherit the permissions and adult-content settings of their parent channel, and they behave like channels for messaging: the same message endpoints work inside a thread.

- A thread started from a message shares that message's ID; a standalone thread gets its own ID.
- Posting inside a thread requires the `SEND_MESSAGES_IN_THREADS` permission; `SEND_MESSAGES` has no effect inside threads.
- Creating threads requires `CREATE_PUBLIC_THREADS`; archiving, locking, renaming other people's threads, and deleting threads require `MANAGE_THREADS` (the thread creator can rename and archive their own thread).
- Threads auto-archive after their `auto_archive_duration` (60, 1440, 4320, or 10080 minutes, default 10080) of inactivity. Sending a message reopens an archived thread unless it is locked.
- Thread state lives in the `thread_metadata` object on the channel (`archived`, `locked`, `auto_archive_duration`, `archive_timestamp`), alongside `member_count`, `message_count`, and `total_message_sent`.

The full endpoint reference (create from message, create standalone, active/archived listings, search, and thread member management) is available in the [OpenAPI specification](../api-reference.md), tagged `Threads`. Bots receive `THREAD_CREATE`, `THREAD_UPDATE`, `THREAD_DELETE`, `THREAD_MEMBER_UPDATE`, and `THREAD_MEMBERS_UPDATE` gateway events.
