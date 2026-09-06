---
# SPDX-License-Identifier: AGPL-3.0-or-later
title: Snowflakes
description: Fluxer's 64-bit identifiers, their bit layout, wire grammar, and pagination behaviour.
---

A snowflake is a 64-bit identifier for a Fluxer resource such as a user, guild, channel, message, role, or attachment. It is the type written as `snowflake` in every wire table of this reference, and the HTTP API, the Admin API, and the Gateway all share this identifier space.

A snowflake is unique within the deployment that issued it. The value stays stable, and Fluxer does not reuse it after the resource is deleted. Separate deployments can issue the same numeric value, so a client MUST scope every snowflake to its deployment.

:::caution[Never parse a snowflake as a JSON number]
Snowflake values exceed the exact integer range of a JSON double, so parsing one as a number can silently change it. A client MUST parse the string into a 64-bit integer and MUST compare snowflakes as integers, because unequal-length decimal strings do not sort as text.
:::

## Format

A snowflake packs three fields into 64 bits. The worker and sequence fields distinguish identifiers minted during the same millisecond. Bit 63 is always zero, so an issued snowflake fits a signed 64-bit integer.

| Field | Bits | Description |
| --- | --- | --- |
| Timestamp<sup>1</sup> | 62 to 22 | The number of milliseconds since `2015-01-01T00:00:00.000Z` |
| Worker ID | 21 to 12 | The unsigned allocator worker identifier, from `0` through `1023` |
| Sequence<sup>2</sup> | 11 to 0 | The unsigned per-worker sequence, from `0` through `4095` |

<sup>1</sup> The timestamp records the instant the identifier was minted, which can fall shortly before the resource exists

<sup>2</sup> Sequence order applies only within one worker and one millisecond

The epoch is 1420070400000 milliseconds after the Unix epoch. Every Fluxer instance uses the same value. The lower 22 bits expose allocator details, so a client MUST NOT use them for routing or resource semantics.

One worker issues strictly increasing values. It advances the sequence for each identifier minted during the same millisecond, and it waits for the next millisecond once the sequence passes 4095.

A worker whose clock moves backwards keeps minting against the highest millisecond it has already used. An extracted timestamp can therefore fall later than the wall clock at the moment of minting.

## Extracting a timestamp

To obtain the creation time in Unix milliseconds, shift the snowflake right by 22 bits and add the epoch.

```text
timestamp_ms = (snowflake >> 22) + 1420070400000
```

The inverse expression produces the smallest possible snowflake for a Unix millisecond timestamp. It is useful as a pagination boundary.

```text
snowflake = (timestamp_ms - 1420070400000) << 22
```

The representable range runs from `2015-01-01T00:00:00.000Z` through `2084-09-06T15:47:35.551Z`.

## Wire representation

Fluxer emits a snowflake as an unsigned decimal string in every JSON body, path parameter, query string parameter, and Gateway payload. An emitted value contains only ASCII digits. Inbound traffic is more permissive, and a client SHOULD send the emitted form everywhere.

### Accepted input

| Surface | Accepted input |
| --- | --- |
| HTTP request<sup>1</sup> | `0`, or a non-zero digit followed by further digits, sent as a JSON string or as a JSON integer |
| Gateway command<sup>2</sup> | A non-zero digit followed by further digits, sent as a string of ASCII digits or as a positive JSON integer |

<sup>1</sup> Fluxer removes leading and trailing whitespace before it reads the text. In that text a leading zero, a sign, a decimal point, and any other non-digit character are rejected with the validation code [`INVALID_SNOWFLAKE_FORMAT`](/http-api/errors/#validation-error-code-registry), and a well-formed value above 9223372036854775807 is rejected with [`SNOWFLAKE_OUT_OF_RANGE`](/http-api/errors/#validation-error-code-registry)

<sup>2</sup> The Gateway rejects `0`, a leading zero, a signed value, a negative integer, and a value above 9223372036854775807

Both HTTP validation codes are element codes inside a 400 [`INVALID_FORM_BODY`](/http-api/errors/#api-error-code-registry) response. Each Gateway [command](/gateway/commands/) states what its own rejection does, from discarding the field to abandoning the whole command.

A snowflake sent as a JSON number keeps its exact value at any size.

A fractional JSON number is an ordinary type failure and reports [`INVALID_FORMAT`](/http-api/errors/#validation-error-code-registry). An exponent form is read as the number it denotes, and one above 9007199254740991 reports [`INVALID_SNOWFLAKE_FORMAT`](/http-api/errors/#validation-error-code-registry). A path parameter and a query string parameter always arrive as text.

:::note[`0` is the beginning of snowflake time]
An HTTP pagination cursor accepts `0`. An HTTP path parameter or field that identifies a real resource also accepts `0`, and the request then receives the ordinary not-found result for that resource.
:::

## Pagination

A collection endpoint that pages over a snowflake-ordered resource accepts a cursor such as `before`, `after`, or `around`. The endpoint defines which cursors it supports, whether they are mutually exclusive, the result order, and how `limit` is applied.

The `before` cursor selects identifiers lower than the cursor value, and the `after` cursor selects identifiers higher than it. Both exclude the cursor value itself. The `around` cursor selects a window centred on the cursor value and includes that value. [List channel messages](/http-api/messages/#list-channel-messages) accepts it as a query string parameter and [List messages from multiple channels](/http-api/messages/#list-messages-from-multiple-channels) accepts it on each entry of its request body. No other operation accepts it.

The type of a `before` or `after` cursor follows the operation. [List pinned messages](/http-api/messages/#list-pinned-messages) pages on the pin time, so its `before` is an ISO 8601 timestamp rather than a message ID. [List blocklist entries](/admin-api/blocklists/#list-blocklist-entries) pages on the entry value, and its `after` is the stored value of the last entry on the previous page. Each operation states the type of its own cursors.

Because the timestamp occupies the high bits, numeric snowflake order is creation time order at millisecond resolution.

:::note[A cursor is a numeric boundary]
A cursor does not need to identify a resource, so a snowflake derived from a timestamp can delimit a time range without a dedicated timestamp parameter.
:::
