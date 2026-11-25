---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Snowflakes
description: The 64-bit IDs Fluxer gives every resource, how to get their creation time and how to send and page with them.
---

A snowflake is the 64-bit ID of a Fluxer resource, such as a user, guild, channel, message or role. The HTTP API, the Admin API and the Gateway all use the same IDs, and an ID is unique only within the instance that issued it.

:::caution[Never parse a snowflake as a JSON number]
Fluxer sends every snowflake as a string of decimal digits, because a JSON number can't represent every one exactly. Parse each one into a 64-bit integer before you compare two snowflakes or sort a list of snowflakes.
:::

## Format

A snowflake packs a timestamp, a worker ID and a sequence into 64 bits. Bit 63 is always 0.

| Field | Bits | Description |
| --- | --- | --- |
| Timestamp | 62 to 22 | Milliseconds since the Fluxer epoch, `2015-01-01T00:00:00.000Z` |
| Worker ID | 21 to 12 | The worker that issued the ID, from `0` through `1023` |
| Sequence | 11 to 0 | A per-worker counter, from `0` through `4095` |

Every instance uses the same epoch. Don't rely on the worker ID or sequence.

## Timestamps

To get the creation time in Unix milliseconds, shift the snowflake right by 22 bits and add the epoch. To build a cursor for a given time, reverse that to get its smallest snowflake.

```text
timestamp_ms = (snowflake >> 22) + 1420070400000
snowflake = (timestamp_ms - 1420070400000) << 22
```

Snowflakes cover `2015-01-01T00:00:00.000Z` through `2084-09-06T15:47:35.551Z`.

A thread started from a message has that message's ID, so its timestamp is the message's. The thread's own creation time is `create_timestamp` in its [thread metadata](/http-api/threads/#thread-metadata-object).

## Wire format

Send snowflakes as strings too. The HTTP API and Gateway also take a JSON integer.

| Surface | Accepts |
| --- | --- |
| HTTP request | A string or JSON integer from `0` through `9223372036854775807`, with no leading zero |
| Gateway command | The same, except `0` is rejected |

The HTTP API reports a malformed value as [`INVALID_SNOWFLAKE_FORMAT`](/http-api/errors/#validation-error-codes) and a value that is too large as [`SNOWFLAKE_OUT_OF_RANGE`](/http-api/errors/#validation-error-codes).

## Pagination

Snowflakes sorted as integers are in creation order, and list endpoints use them as cursors. You can also [build a cursor from a timestamp](#timestamps).

| Cursor | Selects |
| --- | --- |
| `before` | IDs lower than the cursor |
| `after` | IDs higher than the cursor |
| `around` | A window centred on the cursor, including it |

Only [List channel messages](/http-api/messages/#list-channel-messages) and [List messages from multiple channels](/http-api/messages/#list-messages-from-multiple-channels) accept `around`. Each endpoint lists the cursors it accepts, and a few use cursors that aren't snowflakes.
