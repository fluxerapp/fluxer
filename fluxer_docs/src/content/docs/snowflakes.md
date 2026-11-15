---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Snowflakes
description: The 64-bit IDs Fluxer gives every resource, how to get their creation time and how to send and page with them.
---

A snowflake is the 64-bit ID of a Fluxer resource, such as a user, guild, channel, message or role. The HTTP API, the Admin API and the Gateway all use the same IDs, and an ID is unique within the instance that issued it.

:::caution[Never parse a snowflake as a JSON number]
Fluxer sends every snowflake as a string of decimal digits, because a JSON number can't represent one exactly. Parse the string into a 64-bit integer and compare snowflakes as integers.
:::

## Format

A snowflake packs a timestamp, a worker ID and a sequence into 64 bits. Bit 63 is always zero.

| Field | Bits | Description |
| --- | --- | --- |
| Timestamp | 62 to 22 | Milliseconds since the Fluxer epoch, `2015-01-01T00:00:00.000Z` |
| Worker ID | 21 to 12 | The worker that issued the ID, from `0` through `1023` |
| Sequence | 11 to 0 | A per-worker counter, from `0` through `4095` |

Every instance uses the same epoch. Don't rely on the worker ID or sequence.

## Timestamps

To get the creation time in Unix milliseconds, shift the snowflake right by 22 bits and add the epoch. To build a cursor for a given time, reverse the calculation to get the smallest snowflake for that time.

```text
timestamp_ms = (snowflake >> 22) + 1420070400000
snowflake = (timestamp_ms - 1420070400000) << 22
```

Snowflakes cover times from `2015-01-01T00:00:00.000Z` through `2084-09-06T15:47:35.551Z`.

A thread started from a message has that message's ID, so its timestamp is when the message was sent. The thread's own creation time is `create_timestamp` in its [thread metadata](/http-api/threads/#thread-metadata-object).

## Wire format

Send snowflakes as strings too. The HTTP API and the Gateway also accept a JSON integer.

| Surface | Accepts |
| --- | --- |
| HTTP request | A string or JSON integer from `0` through `9223372036854775807`, with no leading zero |
| Gateway command | The same, except `0` is rejected |

The HTTP API reports a malformed value as [`INVALID_SNOWFLAKE_FORMAT`](/http-api/errors/#validation-error-codes) and a value that is too large as [`SNOWFLAKE_OUT_OF_RANGE`](/http-api/errors/#validation-error-codes).

## Pagination

Snowflakes sorted as integers are in creation order, and list endpoints use them as cursors. A cursor doesn't have to be the ID of a real resource, and a snowflake [built from a timestamp](#timestamps) works too.

| Cursor | Selects |
| --- | --- |
| `before` | IDs lower than the cursor |
| `after` | IDs higher than the cursor |
| `around` | A window centered on the cursor, including it |

Only [List channel messages](/http-api/messages/#list-channel-messages) and [List messages from multiple channels](/http-api/messages/#list-messages-from-multiple-channels) accept `around`. Each endpoint lists the cursors it accepts, and a few endpoints use something other than a snowflake as the cursor.
