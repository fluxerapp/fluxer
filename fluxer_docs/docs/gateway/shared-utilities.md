# Shared Utilities

This document covers the shared utility modules under `fluxer_gateway/src/utils/` and the
`limited_deque` module at `fluxer_gateway/src/limited_deque.erl`. These modules are imported
across every gateway subsystem and provide primitives that the 14 other documents reference.

---

## `constants.erl`

`constants.erl` is the single source of truth for all numeric constants used in the gateway
protocol and internal timers. Every value is exported as a typed function so callers are not
tied to raw integer literals.

### Opcode table

`gateway_opcode/1` maps an integer to an atom. `opcode_to_num/1` is the reverse direction.
Both are used in `gateway_handler` (see [websocket-handler.md](websocket-handler.md)).

| Number | Atom | Direction |
|--------|------|-----------|
| 0 | `dispatch` | S→C |
| 1 | `heartbeat` | C→S |
| 2 | `identify` | C→S |
| 3 | `presence_update` | C→S |
| 4 | `voice_state_update` | C→S |
| 5 | `voice_server_ping` | C→S |
| 6 | `resume` | C→S |
| 7 | `reconnect` | S→C |
| 8 | `request_guild_members` | C→S |
| 9 | `invalid_session` | S→C |
| 10 | `hello` | S→C |
| 11 | `heartbeat_ack` | S→C |
| 12 | `gateway_error` | S→C |
| 14 | `lazy_request` | C→S |
| 15 | `request_guild_counts` | C→S |
| 16 | `request_channel_member_counts` | C→S |

Opcode 13 is not defined. `gateway_opcode/1` returns the atom `unknown` for any unrecognised
integer.

### Close code table

`close_code_to_num/1` maps an atom to the corresponding WebSocket close code integer.
The full table is used in [websocket-handler.md](websocket-handler.md).


| Code | Atom | Meaning |
|------|------|---------|
| 4000 | `unknown_error` | An unknown error occurred |
| 4001 | `unknown_opcode` | An unrecognised opcode was sent |
| 4002 | `decode_error` | Payload could not be decoded |
| 4003 | `not_authenticated` | A payload arrived before Identify |
| 4004 | `authentication_failed` | Token is invalid |
| 4005 | `already_authenticated` | Identify sent more than once |
| 4007 | `invalid_seq` | Invalid sequence number on Resume |
| 4008 | `rate_limited` | Too many connections or messages |
| 4009 | `session_timeout` | Session timed out waiting to resume |
| 4010 | `invalid_shard` | Shard info on Identify was invalid |
| 4011 | `sharding_required` | Session would hold too many guilds |
| 4012 | `invalid_api_version` | `?v=` query param was not `1` |
| 4013 | `ack_backpressure` | Heartbeat acknowledgement backpressure |

Code 4006 is not defined.

### Permission bit constants

Each function returns the bit value as a `pos_integer()`. These values are the `Bit` arguments
passed to `permission_bits:has/2` and `bitset:has/2`. Full descriptions of how they compose are
in [permissions.md](permissions.md).

| Function | Bit value |
|---|---|
| `kick_members_permission/0` | 2 |
| `ban_members_permission/0` | 4 |
| `administrator_permission/0` | 8 |
| `manage_channels_permission/0` | 16 |
| `view_audit_log_permission/0` | 128 |
| `stream_permission/0` | 512 |
| `view_channel_permission/0` | 1 024 |
| `read_message_history_permission/0` | 65 536 |
| `connect_permission/0` | 1 048 576 |
| `speak_permission/0` | 2 097 152 |
| `use_vad_permission/0` | 33 554 432 |
| `manage_roles_permission/0` | 268 435 456 |
| `view_channel_members_permission/0` | 18 014 398 509 481 984 |

`administrator_permission/0` (bit 8) triggers the short-circuit in
`guild_permissions:compute_member_permissions/4` that returns `?ALL_PERMISSIONS = 0xFFFFFFFFFFFFFFFF`.

### Timing constants

| Function | Value | Where used |
|---|---|---|
| `heartbeat_interval/0` | 41 250 ms | Sent in Hello payload; see [websocket-handler.md](websocket-handler.md) |
| `heartbeat_timeout/0` | 45 000 ms | Zombie-detection window in the handler |
| `resume_timeout/0` | 60 000 ms | Resume timer in session state; see [session-lifecycle.md](session-lifecycle.md) |
| `max_payload_size/0` | 4 096 bytes | Oversized frame guard in `websocket_handle/2`; see [websocket-handler.md](websocket-handler.md) |
| `random_session_bytes/0` | 16 | Random bytes used to generate the 32-char hex session ID; see [session-lifecycle.md](session-lifecycle.md) |

An additional constant not exported as a function is `voice_channel_camera_user_limit/0` which
returns `25` and caps the effective user limit when any camera is active in a voice channel
(see [voice.md](voice.md)).

### Status type conversion

`status_type_atom/1` converts between binary status strings and atoms in both directions:

- Binary → atom: `<<"online">>` → `online`, `<<"dnd">>` → `dnd`, `<<"idle">>` → `idle`,
  `<<"invisible">>` → `invisible`, `<<"offline">>` → `offline`, empty binary → `online`.
- Atom → binary: `online` → `<<"online">>`, and so on. `undefined` and `null` return `<<"online">>`.

This is used by `utils:parse_status/1` and `constants:dispatch_event_atom/1`.

`dispatch_event_atom/1` converts event names: atoms are uppercased and returned as binaries;
binaries are normalised via `event_atoms:normalize/1`.


---

## `bitset.erl`

`bitset` is the core bit-manipulation module. All permission values are non-negative integers;
the type alias `t() :: non_neg_integer()` and `bit() :: pos_integer()` are exported for
Eqwalizer.

### `parse/1`

Accepts three input forms and returns a `non_neg_integer()`:

| Input | Behaviour |
|---|---|
| `non_neg_integer()` | returned as-is |
| `binary()` | ASCII decimal digits only; empty binary or any non-digit byte raises `{invalid_bitset, Value}` |
| `char_list()` | same decimal parsing over a list of digit codepoints |

```erlang
bitset:parse(1024)        %% → 1024
bitset:parse(<<"1024">>) %% → 1024
bitset:parse("1024")     %% → 1024
bitset:parse(<<"-1">>)   %% raises {invalid_bitset, <<"-1">>}
```

Variant functions:

- `parse_optional/1` — returns `undefined` for `undefined` and `null`; otherwise calls `parse/1`.
- `parse_maybe/1` — wraps `parse_optional/1` in a `try/catch`; returns `undefined` on any error
  instead of raising. Safe to call on untrusted input.
- `normalize/1` and `require/1` — aliases for `parse/1`.

### Operations

| Function | Erlang expression | Description |
|---|---|---|
| `none/0` | `0` | Zero bitset |
| `has(Bits, Bit)` | `(Bits band Bit) =:= Bit` | True when all bits in `Bit` are set in `Bits` |
| `any(Bits, Mask)` | `(Bits band Mask) =/= 0` | True when at least one bit in `Mask` is set |
| `add(Bits, Mask)` | `Bits bor Mask` | Sets all bits in `Mask` |
| `remove(Bits, Mask)` | `Bits band bnot Mask` | Clears all bits in `Mask` |
| `apply_allow_deny(Bits, Allow, Deny)` | `add(remove(Bits, Deny), Allow)` | Deny cleared first, allow set after |

`apply_allow_deny/3` is the fundamental operation for channel overwrite merging. Deny bits are
cleared before allow bits are set, so an explicit allow always wins when the same bit appears in
both. Used in `guild_permissions_overwrites:apply_channel_overwrites/5` (see
[permissions.md](permissions.md)) and in the event dispatch filter (see
[event-dispatch-pipeline.md](event-dispatch-pipeline.md)).

Conversion helpers: `to_integer/1` returns the backing integer; `to_binary/1` calls
`integer_to_binary/1` on the backing integer.


---

## `permission_bits.erl`

`permission_bits` is a thin wrapper module over `bitset`. Every exported function delegates
directly to the corresponding `bitset` function without adding logic:

| `permission_bits` function | Delegates to |
|---|---|
| `none/0` | `bitset:none/0` |
| `parse/1` | `bitset:parse/1` |
| `parse_optional/1` | `bitset:parse_optional/1` |
| `parse_maybe/1` | `bitset:parse_maybe/1` |
| `normalize/1` | `bitset:normalize/1` |
| `require/1` | `bitset:require/1` |
| `to_integer/1` | `bitset:to_integer/1` |
| `to_binary/1` | `bitset:to_binary/1` |
| `has/2` | `bitset:has/2` |
| `any/2` | `bitset:any/2` |
| `add/2` | `bitset:add/2` |
| `remove/2` | `bitset:remove/2` |
| `apply_allow_deny/3` | `bitset:apply_allow_deny/3` |

The type aliases `-type t() :: bitset:t()` and `-type bit() :: bitset:bit()` mean Eqwalizer
treats `permission_bits:t()` and `bitset:t()` as the same concrete type. Callers in the
permissions subsystem import `permission_bits` to make intent clear; callers that work with
arbitrary bitsets import `bitset` directly.

`permission_bits` is used in [permissions.md](permissions.md) for all role and overwrite
computation, in [event-dispatch-pipeline.md](event-dispatch-pipeline.md) for the session
visibility filter, and in [voice.md](voice.md) for voice permission checks.

---

## `snowflake_id.erl`

`snowflake_id` parses and validates Snowflake IDs. The type `t() :: pos_integer()` is the
canonical in-memory representation. Named aliases (`user_id()`, `guild_id()`, `channel_id()`,
`role_id()`, `message_id()`) are all aliases for `t()`.

### `parse/1`

Accepts three input forms:

| Input | Accepted | Rejected |
|---|---|---|
| positive integer | any `> 0` | `0`, negative |
| binary | ASCII decimal with no leading zeros, starting with `1`–`9` | `<<"0">>`, `<<"001">>`, signs |
| char list | same rules as binary | — |

Any rejected input raises `{invalid_snowflake, Value}`. Leading zeros are rejected to match the
canonical wire format where Snowflakes are transmitted as decimal strings without padding.

```erlang
snowflake_id:parse(123)       %% → 123
snowflake_id:parse(<<"456">>) %% → 456
snowflake_id:parse(<<"001">>) %% raises {invalid_snowflake, <<"001">>}
snowflake_id:parse(0)         %% raises {invalid_snowflake, 0}
```

Variant functions:

- `parse_optional/1` — returns `undefined` for `null` and `undefined`.
- `parse_maybe/1` — wraps `parse_optional/1` in a `try/catch`; returns `undefined` on error.
- `parse_list/1` — calls `parse/1` on each element of a list; `undefined` and `null` return `[]`.
- `filter/1` — returns `{true, Id}` or `false`; suitable for `lists:filtermap`.
- `first/1` — scans a list and returns the first valid Snowflake or `undefined`.

Utility functions:

- `is_valid/1` — boolean; wraps `parse/1` in a try/catch.
- `equal/2` — compares a known integer Snowflake against an arbitrary term by parsing the term.
- `member/2` — checks whether a Snowflake is in a list of mixed-type values.
- `get/3` — looks up a Snowflake key in a map, trying the integer key first and the binary key
  as a fallback. Used when maps may have been keyed before or after binary-to-integer conversion.

`snowflake_id` is used in [calls.md](calls.md) and [voice.md](voice.md) for
connection ID and user ID handling throughout the call and voice pipelines.


---

## `snowflake_util.erl`

`snowflake_util` extracts the creation timestamp from a Snowflake ID.

### `extract_timestamp/1`

```erlang
-spec extract_timestamp(term()) -> integer() | undefined.
```

Uses two compile-time constants:

- `?FLUXER_EPOCH = 1_420_070_400_000` — custom epoch in milliseconds (2015-01-01T00:00:00Z).
- `?TIMESTAMP_SHIFT = 22` — the number of bits the timestamp occupies above the worker ID and
  sequence number fields.

```erlang
(Snowflake bsr 22) + 1_420_070_400_000
```

The result is a Unix timestamp in milliseconds. Returns `undefined` for any input that
`snowflake_id:parse_optional/1` cannot parse (zero, negative values, non-digit binaries, or
`null`/`undefined`).

The Snowflake layout places the timestamp in the most significant 42 bits:

```
| 42 bits: ms since epoch | 5 bits: worker ID | 5 bits: process ID | 12 bits: sequence |
```

Shifting right by 22 discards the lower 22 bits (worker ID + process ID + sequence), leaving
only the timestamp component.


---

## `event_atoms.erl`

`event_atoms` normalises gateway event name binaries to atoms so dispatch code can pattern
match efficiently.

### `normalize/1`

```erlang
-spec normalize(binary() | atom()) -> atom() | binary().
```

- Atom input: returned unchanged.
- Binary input: checked against a static `known_event_map()` first. If found, the associated
  atom is returned. If not found, the binary is lowercased and `binary_to_existing_atom/2` is
  attempted. If that also fails (atom was never created), the original binary is returned
  unchanged.

The `known_event_map()` is built by merging five sub-maps at call time:

| Sub-map | Sample entries |
|---|---|
| `core_event_map` | `READY`, `RESUMED`, `PRESENCE_UPDATE`, `TYPING_START`, `SESSIONS_REPLACE` |
| `channel_event_map` | `CHANNEL_CREATE`, `CHANNEL_DELETE`, `CHANNEL_UPDATE`, `CHANNEL_UPDATE_BULK` |
| `guild_event_map` | `GUILD_CREATE`, `GUILD_DELETE`, `GUILD_UPDATE`, `GUILD_MEMBER_ADD`, `GUILD_ROLE_UPDATE_BULK` |
| `message_event_map` | `MESSAGE_CREATE`, `MESSAGE_DELETE`, `MESSAGE_REACTION_ADD`, `MESSAGE_REACTION_REMOVE` |
| `user_voice_event_map` | `VOICE_STATE_UPDATE`, `VOICE_SERVER_UPDATE`, `USER_SETTINGS_UPDATE`, `USER_UPDATE` |

The map covers all events dispatched by `guild_dispatch` and emitted by the session subsystem.
Events not in the map that also do not correspond to an existing atom are returned as the
original binary, so unknown future events pass through without crashing.

`event_atoms:normalize/1` is called from `constants:dispatch_event_atom/1` and throughout the
session and guild dispatch pipeline; see [session-lifecycle.md](session-lifecycle.md) and
[guild-gen-server.md](guild-gen-server.md).

---

## `utils.erl`

`utils` provides miscellaneous helpers used across session and handler code.

### `generate_session_id/0`

```erlang
-spec generate_session_id() -> binary().
```

Generates `constants:random_session_bytes()` (16) random bytes via
`crypto:strong_rand_bytes/1` and returns them hex-encoded as a 32-character binary. Used when
creating a new Session on Identify; see [session-lifecycle.md](session-lifecycle.md).

### `generate_resume_token/0`

Generates 32 random bytes and returns them URL-safe base64-encoded. Used for resume token
creation.

### `hash_token/1`

```erlang
-spec hash_token(binary()) -> binary().
```

Returns `crypto:hash(sha256, Token)` — a 32-byte binary. Stored in session state as
`token_hash` and compared during Resume to verify the client has not switched tokens.

### `parse_status/1`

```erlang
-spec parse_status(binary() | atom() | term()) -> atom().
```

Wraps `constants:status_type_atom/1`. Binaries and atoms are converted via the constants table.
Any other input returns the atom `online`. Used when processing presence updates from the
client; see [presence-subsystem.md](presence-subsystem.md).

### `safe_json_decode/1`

```erlang
-spec safe_json_decode(binary()) -> map().
```

Decodes a JSON binary with `json:decode/1`. Returns `#{}` on any error or if the result is not
a map. Used where decoding failures should silently produce an empty map.

### `binary_to_integer_safe/1`

```erlang
-spec binary_to_integer_safe(binary() | integer() | term()) -> integer() | undefined.
```

Integer input is returned as-is. Binary input is converted via `type_conv:to_integer/1`.
Any other type returns `undefined`. Useful where the source may be either a pre-parsed integer
or a wire-format decimal binary.

### `check_user_data_differs/2`

```erlang
-spec check_user_data_differs(map(), map()) -> boolean().
```

Normalises both user data maps via `user_utils:normalize_user/1` and compares the fields
listed in `user_utils:partial_user_fields/0`. Returns `true` if any checked field differs in
the new map. Fields absent from the new map are not considered a change. Used in session code
to detect whether a `USER_UPDATE` event carries relevant changes.

### `parse_iso8601_to_unix_ms/1`

```erlang
-spec parse_iso8601_to_unix_ms(binary() | term()) -> integer() | undefined.
```

Parses an ISO 8601 UTC timestamp of the form `YYYY-MM-DDTHH:MM:SS[.fraction]Z` into a Unix
millisecond timestamp. Fractional seconds up to nanosecond resolution are supported; only the
first three digits (milliseconds) are used. Returns `undefined` for any malformed input.
Used when parsing custom status `expires_at` fields.


---

## `backoff_utils.erl`

`backoff_utils` computes exponential backoff delays for retry logic.

### `calculate/1` and `calculate/2`

```erlang
-spec calculate(non_neg_integer()) -> non_neg_integer().
-spec calculate(non_neg_integer(), pos_integer()) -> non_neg_integer().
```

Returns `min(round(1000 * 2^Attempt), MaxMs)`. The exponent is capped at
`?MAX_BACKOFF_EXPONENT = 32` before the power is computed so no integer overflow can occur for
arbitrarily large attempt numbers. Default `MaxMs` is 30 000 ms.

```
attempt 0 → 1 000 ms
attempt 1 → 2 000 ms
attempt 2 → 4 000 ms
attempt 3 → 8 000 ms
attempt 4 → 16 000 ms
attempt 5+ → 30 000 ms (capped)
```

A negative or non-integer attempt is treated as 0 via `cap_exponent/1`.

### `calculate_with_jitter/1` and `calculate_with_jitter/2`

```erlang
-spec calculate_with_jitter(non_neg_integer()) -> non_neg_integer().
-spec calculate_with_jitter(non_neg_integer(), pos_integer()) -> non_neg_integer().
```

Computes a base delay with `calculate/2` then applies uniform jitter over a range of
`±(Base div 4)`. The jitter range is `max(1, Base div 2)` and the offset is `JitterRange div 2`,
producing a value in roughly `[Base * 0.75, Base * 1.25]`. The result is always at least 1 and
at most `MaxMs`.

`backoff_utils` is used in [calls.md](calls.md) for connection retry timing and in
[clustering-nats-rpc.md](clustering-nats-rpc.md) for NATS reconnect scheduling.

---

## `gateway_retry_timer.erl`

`gateway_retry_timer` provides blocking wait primitives for retry loops inside gen_server
init sequences.

### `wait/1`

```erlang
-spec wait(term()) -> ok | {error, invalid_delay}.
```

Starts an Erlang timer for `DelayMs` milliseconds, blocks until it fires, and returns `ok`.
The maximum wait is `?MAX_RETRY_WAIT_MS = 60 000` ms regardless of the requested delay. A
non-integer or negative `DelayMs` returns `{error, invalid_delay}` without blocking.

### `wait_until/2`

```erlang
-spec wait_until(DelayMs :: term(), DeadlineMs :: integer()) ->
    ok | expired | {error, invalid_delay}.
```

Checks whether the monotonic clock has already passed `DeadlineMs`. If so it returns `expired`
immediately without sleeping. Otherwise it sleeps for `min(DelayMs, DeadlineMs - NowMs)` via
the same one-shot timer mechanism used by `wait/1`, then returns `ok`.

The caller is responsible for checking whether to retry after receiving `ok`. The typical
pattern from `gateway_hotpatch_reconciler` is:

```erlang
case reconcile_once(State) of
    ok -> put_ready(true);
    _Err ->
        case gateway_retry_timer:wait_until(?RETRY_MS, DeadlineMs) of
            ok      -> startup_reconcile(State, DeadlineMs);
            expired -> handle_timeout(State)
        end
end
```

`wait_until/2` is used in the hotpatch startup reconcile loop (see
[hot-patching.md](hot-patching.md)) to block the init callback until all hotpatch events
have been applied or the deadline has passed. It is also referenced in
[clustering-nats-rpc.md](clustering-nats-rpc.md).


---

## `limited_deque.erl`

`limited_deque` is a bounded double-ended queue used as the session event replay buffer.
The module header suppresses the auto-import of `size/1` so the module-level `size/1` shadows
the BIF without a module prefix requirement at call sites.

### Internal structure

The opaque type `deque()` is a map:

```erlang
-opaque deque() :: #{
    front    := [term()],
    rear     := [term()],
    count    := non_neg_integer(),
    max_count := pos_integer(),
    bytes    := non_neg_integer(),
    max_bytes := non_neg_integer()
}.
```

Elements at the front of the logical sequence are prepended to the `front` list; elements
pushed at the rear are prepended to the `rear` list. The logical order is
`front ++ lists:reverse(rear)`. This two-list design means both `push` (rear append) and
`pop_front` (front dequeue) are O(1) in the common case; a `pop_front` from an empty `front`
costs O(n) once to reverse the `rear`, after which subsequent pops are O(1) again.

Setting `max_bytes` to `0` disables byte-based trimming; only `max_count` is enforced.

### `new/2`

```erlang
-spec new(MaxCount :: pos_integer(), MaxBytes :: non_neg_integer()) -> deque().
```

Creates an empty deque. Both lists are `[]`, both counters are `0`.

### `push/2`

```erlang
-spec push(Item :: term(), Deque :: deque()) -> deque().
```

Prepends `Item` to `rear`, increments `count`, adds `entry_bytes(Item)` to `bytes`, then calls
`trim_front/1`. `entry_bytes/1` computes `erts_debug:flat_size(Term) * erlang:system_info(wordsize)`.

`trim_front/1` pops entries from the `front` until both `count <= max_count` and
`bytes <= max_bytes`. This evicts the oldest items when either limit is exceeded.

### `pop/1` and `pop_front/1`

```erlang
-spec pop(deque()) -> {term(), deque()} | empty.
-spec pop_front(deque()) -> {term(), deque()} | empty.
```

`pop/1` removes from the rear (LIFO end). `pop_front/1` removes from the front (FIFO end).
Both return `empty` on an empty deque. When the target list is empty but the other is not, the
other list is reversed to become the new target list in O(n), then the head is taken.

### `filter/2`

```erlang
-spec filter(fun((term()) -> boolean()), deque()) -> deque().
```

Converts the deque to a flat list via `to_list/1`, applies `lists:filter/2`, then rebuilds the
deque via `from_list/3`. `from_list/3` calls `trim_front/1` after loading so the count and byte
limits are re-enforced on the result. Byte counts are recomputed from scratch by `from_list/3`.

### `drop_while_front/2`

```erlang
-spec drop_while_front(fun((term()) -> boolean()), deque()) -> deque().
```

Repeatedly calls `pop_front/1` as long as the predicate returns `true` for the front item.
When the predicate returns `false`, the item is pushed back to the front via `push_front/2` and
the function returns. This is the function used in session state to drop acknowledged events:

```erlang
limited_deque:drop_while_front(fun(E) -> maps:get(seq, E) =< AckSeq end, Buffer)
```

See [session-lifecycle.md](session-lifecycle.md) for how `ack_seq` drives buffer trimming.

### `size/1`

```erlang
-spec size(deque()) -> non_neg_integer().
size(#{count := Count}) -> Count.
```

O(1). Reads `count` directly from the map without traversing either list. The `count` field is
maintained incrementally by all mutation functions.

### `bytes/1`

```erlang
-spec bytes(deque()) -> non_neg_integer().
bytes(#{bytes := Bytes}) -> Bytes.
```

O(1). Reads the tracked `bytes` field directly. The field is updated on every `push` and `pop`.

### `recompute_bytes/2`

```erlang
-spec recompute_bytes(fun((term()) -> non_neg_integer()), deque()) -> deque().
```

Recomputes the `bytes` field from scratch using a caller-supplied byte function. Used when the
default `entry_bytes/1` heuristic is not appropriate for a particular item type.

### Byte tracking

`entry_bytes(Term)` uses `erts_debug:flat_size/1` which counts the number of words needed to
store the term on the heap (excluding sub-binaries referenced by off-heap pointers).
Multiplying by `erlang:system_info(wordsize)` (8 on 64-bit systems) gives an approximation of
the heap cost in bytes. This is a lower bound; binaries larger than 64 bytes are stored off-heap
and the reference itself is counted, not the full binary.

The session event buffer sets `max_bytes = 16_777_216` (16 MiB) alongside
`max_count = 4_096`; see [session-lifecycle.md](session-lifecycle.md).


---

## `type_conv.erl`

`type_conv` converts values between Erlang types. All functions are pure; none raise exceptions.

### `to_integer/1`

```erlang
-spec to_integer(term()) -> integer() | undefined.
```

Accepts `integer()` (returned as-is), `binary()` (via `binary_to_integer/1`), `list()` (via
byte-list-to-binary conversion), and `atom()` (via `atom_to_list` then `list_to_integer`).
Returns `undefined` for any non-convertible input including empty binaries and malformed values.

Used by `utils:binary_to_integer_safe/1`, `map_utils:get_integer/3`, and throughout the session
and telemetry pipelines; see [telemetry.md](telemetry.md).

### `to_binary/1`

```erlang
-spec to_binary(term()) -> binary() | undefined.
```

Accepts `binary()` (returned as-is), `integer()` (via `integer_to_binary/1`), `list()` (via
byte-list conversion), and `atom()` (via `atom_to_binary/2` with UTF-8). Returns `undefined`
for `undefined` input or non-convertible types.

### `to_list/1`

```erlang
-spec to_list(term()) -> list() | undefined.
```

Accepts `list()` (returned as-is), `binary()` (via `binary_to_list/1`), and `atom()` (via
`atom_to_list/1`). Returns `undefined` for `undefined` or non-convertible types.

### `ensure_binary/1` and `ensure_binary/2`

```erlang
-spec ensure_binary(term()) -> binary().
-spec ensure_binary(term(), Default :: binary()) -> binary().
```

Returns the value as a binary or the default (default `<<>>`) when conversion is not possible.
Unlike `to_binary/1`, never returns `undefined`. Handles iolist-style char lists by walking the
list and concatenating bytes.

### `unicode_to_binary/1`

Similar to `ensure_binary/1` but operates on Unicode codepoint lists. Returns `undefined` when
the input is not a binary or a list of valid codepoints.

### `extract_id/2` and `extract_id_required/2`

```erlang
-spec extract_id(map() | term(), atom() | binary()) -> pos_integer() | undefined.
```

Fetches a field from a map and passes it to `snowflake_id:parse_maybe/1`. Returns `undefined`
if the map is not a map, the field is absent, or the value is not a valid Snowflake.
`extract_id_required/2` is an alias with identical behaviour.


---

## `map_utils.erl`

`map_utils` provides safe map access and list-of-map search helpers.

### `get_safe/3`

```erlang
-spec get_safe(map() | term(), key(), default()) -> term().
```

Wraps `maps:get/3`. Returns `Default` when the first argument is not a map.

### `get_nested/3`

```erlang
-spec get_nested(map() | term(), [key()], default()) -> term().
```

Walks a path of keys into a nested map structure. Returns `Default` as soon as any key is not
found or an intermediate value is not a map. An empty path returns the map itself.

### `ensure_map/1`

Returns the argument unchanged if it is a map; returns `#{}` otherwise.

### `ensure_list/1`

Returns the argument unchanged if it is a list; returns `[]` otherwise.

### `get_integer/3`

```erlang
-spec get_integer(map() | term(), key(), default()) -> integer() | term().
```

Fetches a field from a map and passes it through `type_conv:to_integer/1`. Returns `Default`
when the field is absent, the argument is not a map, or the value cannot be converted.

### `get_binary/3`

Same pattern as `get_integer/3` but uses `type_conv:to_binary/1`.

### `filter_by_field/3`

```erlang
-spec filter_by_field(list() | term(), key(), term()) -> [map()].
```

Scans a list of maps and returns those where `maps:find(Field, Item) =:= {ok, Value}`.
Non-map items are silently skipped. Returns `[]` when the first argument is not a list.

### `find_by_field/3`

```erlang
-spec find_by_field(list() | term(), key(), term()) -> {ok, map()} | error.
```

Like `filter_by_field/3` but stops at the first match and returns `{ok, Item}` or `error`.

`map_utils` is used in [push-notifications.md](push-notifications.md) for eligibility
data extraction and in [telemetry.md](telemetry.md) for probe data assembly.

---

## `list_ops.erl`

`list_ops` provides update and removal operations over lists of maps keyed by `<<"id">>` or
`user.id`.

### `replace_by_id/3`

```erlang
-spec replace_by_id(term(), id(), item()) -> item_list().
```

Maps over the list replacing any item where `maps:get(<<"id">>, Item)` equals `Id` with
`NewItem`. Non-map items are left unchanged. Returns `[]` when the first argument is not a list.

### `remove_by_id/2`

Filters out all map items where `<<"id">>` equals `Id`. Non-map items are retained.

### `replace_by_user_id/3` and `remove_by_user_id/2`

Same as the `_by_id` variants but use `extract_user_id/1` to read the `user.id` field nested
inside each item map.

### `bulk_update/2`

```erlang
-spec bulk_update(term(), term()) -> item_list().
```

Builds an `UpdateMap` keyed by `<<"id">>` from the updates list, then maps over the items list
replacing any item whose `<<"id">>` is in `UpdateMap` with the corresponding update entry.
Items with no matching update pass through unchanged. Returns `[]` when either argument is not
a list.

### `extract_user_id/1`

```erlang
-spec extract_user_id(map() | term()) -> pos_integer() | undefined.
```

Extracts `user.id` from a map item using `map_utils:get_safe/3` and `type_conv:extract_id/2`.
Returns `undefined` when the item is not a map or the field is absent.

`list_ops` is used in [telemetry.md](telemetry.md) for processing guild member and role
list updates and throughout the guild dispatch pipeline.


---

## `validation.erl`

`validation` provides input validation helpers for gateway protocol payloads. All functions
follow a `{ok, Value} | {error, Category, Reason}` return convention. Error tuples are produced
by `gateway_errors:error/1`.

### Snowflake validation

**`validate_snowflake/1`**

```erlang
-spec validate_snowflake(term()) -> {ok, pos_integer()} | {error, atom(), atom()}.
```

Accepts an integer or binary. Calls `snowflake_id:parse_optional/1`. Returns
`{ok, Snowflake}` on success. Returns `{error, _, validation_null_snowflake}` for `null` and
`{error, _, validation_invalid_snowflake}` for any other invalid value.

**`validate_snowflake/2`**

Takes a field name and a value; delegates to `validate_snowflake/1`. The field name is unused
in the current implementation but is preserved in the API for call-site documentation.

**`validate_optional_snowflake/1`**

Returns `{ok, null}` for `null`; otherwise delegates to `validate_snowflake/1`.

**`validate_snowflake_list/1`** and **`validate_snowflake_list/2`**

Validates each element of a list. Returns `{ok, [pos_integer()]}` on full success or
`{error, _, validation_invalid_snowflake_list}` on the first invalid element. Returns
`{error, _, validation_expected_list}` when the argument is not a list.

**`snowflake_or_throw/2`** and **`snowflake_list_or_throw/2`**

Convenience wrappers that call `erlang:error({validation, Reason})` on failure instead of
returning an error tuple. Used where callers handle errors via `catch`.

**`extract_snowflake/2`**

```erlang
-spec extract_snowflake(binary(), map()) -> {ok, pos_integer()} | {error, atom(), atom()}.
```

Fetches a field from a map via `get_field/2` then validates it with `validate_snowflake/2`.

**`extract_snowflakes/2`**

```erlang
-spec extract_snowflakes([{atom(), binary()}], map()) ->
    {ok, #{atom() => pos_integer()}} | {error, atom(), atom()}.
```

Iterates a list of `{AtomKey, FieldName}` pairs, calling `extract_snowflake/2` for each.
Returns a map of atom keys to parsed Snowflakes or the first error encountered.

### Map field access

**`get_field/2`**

```erlang
-spec get_field(term(), term()) -> {ok, term()} | {error, atom(), atom()}.
```

`maps:get(Key, Map, undefined)`. Returns `{ok, Value}` when the field is present and not
`undefined`. Returns `{error, _, validation_missing_field}` when absent and
`{error, _, validation_expected_map}` when the second argument is not a map.

**`get_field/3`**

Three-argument variant returning `Default` instead of an error tuple when the field is absent
or the argument is not a map.

**`get_required_field/3`**

```erlang
-spec get_required_field(binary(), term(), fun((term()) -> {ok, term()} | {error, _, _})) ->
    {ok, term()} | {error, atom(), atom()}.
```

Fetches a field then passes its value through a validator function.

**`get_optional_field/3`**

Same pattern as `get_required_field/3` but returns `{ok, undefined}` when the field is absent.

### Error mapping

**`error_category_to_close_code/1`**

```erlang
-spec error_category_to_close_code(atom()) -> integer().
```

Maps error category atoms to WebSocket close codes using `constants:close_code_to_num/1`:

| Category | Close code |
|---|---|
| `rate_limited` | 4008 |
| `auth_failed` | 4004 |
| any other | 4000 |

Used in [presence-subsystem.md](presence-subsystem.md) and
[push-notifications.md](push-notifications.md) when converting validation errors to
protocol close codes.


---

## `custom_status_validation.erl`

`custom_status_validation` validates a user's custom status object by delegating to an external
API service via `rpc_client:call/1`.

### `validate/2`

```erlang
-spec validate(UserId :: integer(), CustomStatus :: map() | null) ->
    {ok, map() | null} | {error, term()}.
```

Two clauses:

- `null` input: returns `{ok, null}` immediately without a network call.
- Map input: builds a validation RPC request and calls `rpc_client:call/1`.

The RPC request has the shape:

```erlang
#{
    <<"type">>          => <<"validate_custom_status">>,
    <<"user_id">>       => type_conv:to_binary(UserId),
    <<"custom_status">> => build_custom_status_payload(CustomStatus)
}
```

`build_custom_status_payload/1` extracts up to four fields from the input map, omitting any
that are `undefined`:

- `<<"text">>` — status text.
- `<<"expires_at">>` — ISO 8601 expiry timestamp.
- `<<"emoji_id">>` — Snowflake of a custom emoji.
- `<<"emoji_name">>` — name of a Unicode or custom emoji.

Fields not present in the input are absent from the payload map rather than sent as `null`.
This lets the API distinguish "field not provided" from "field explicitly cleared".

`custom_status_validation` is called from the presence subsystem when a client sends a
`presence_update` opcode with a custom status; see [presence-subsystem.md](presence-subsystem.md).


---

## Cross-reference summary

The table below maps each utility module to the documents that use it directly.

| Module | Documents |
|---|---|
| `constants` | [architecture-overview.md](architecture-overview.md) (shard limits), [otp-supervision-tree.md](otp-supervision-tree.md) (timing constants), [websocket-handler.md](websocket-handler.md) (opcodes, close codes, `max_payload_size`), [session-lifecycle.md](session-lifecycle.md) (`resume_timeout`, `random_session_bytes`), [permissions.md](permissions.md) (permission bit values) |
| `bitset` | [event-dispatch-pipeline.md](event-dispatch-pipeline.md) (visibility checks), [voice.md](voice.md) (permission checks), [permissions.md](permissions.md) (full API) |
| `permission_bits` | [guild-gen-server.md](guild-gen-server.md) (permission cache), [event-dispatch-pipeline.md](event-dispatch-pipeline.md) (filter), [permissions.md](permissions.md) (full API) |
| `snowflake_id` | [voice.md](voice.md) (connection IDs), [calls.md](calls.md) (user and channel IDs) |
| `snowflake_util` | [session-lifecycle.md](session-lifecycle.md) (timestamp extraction from IDs) |
| `event_atoms` | [session-lifecycle.md](session-lifecycle.md) (event normalisation in dispatch), [guild-gen-server.md](guild-gen-server.md) (dispatch routing) |
| `utils` | [session-lifecycle.md](session-lifecycle.md) (`generate_session_id`, `hash_token`), [presence-subsystem.md](presence-subsystem.md) (`parse_status`) |
| `backoff_utils` | [calls.md](calls.md) (connection retry), [clustering-nats-rpc.md](clustering-nats-rpc.md) (NATS reconnect) |
| `gateway_retry_timer` | [clustering-nats-rpc.md](clustering-nats-rpc.md) (cluster handoff retry), [hot-patching.md](hot-patching.md) (`wait_until/2` in startup reconcile) |
| `limited_deque` | [session-lifecycle.md](session-lifecycle.md) (session event buffer, `push/2`, `pop_front/1`, `drop_while_front/2`, `size/1`) |
| `type_conv` | [telemetry.md](telemetry.md) (probe data conversion), session and guild dispatch pipelines |
| `map_utils` | [push-notifications.md](push-notifications.md) (eligibility data), [telemetry.md](telemetry.md) (probe assembly) |
| `list_ops` | [telemetry.md](telemetry.md) (member/role list processing) |
| `validation` | [presence-subsystem.md](presence-subsystem.md) (presence input validation), [push-notifications.md](push-notifications.md) (notification payload validation) |
| `custom_status_validation` | [presence-subsystem.md](presence-subsystem.md) (custom status API validation) |
