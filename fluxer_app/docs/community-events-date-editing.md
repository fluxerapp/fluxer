# Preserve event timestamps on text-only edits

The event editor displays minute-precision local dates. Re-parsing an unchanged
field discarded seconds/milliseconds and selected the earlier instant during a
repeated daylight-saving hour. The editor now captures the original ISO string
and its displayed input together. An unchanged field reuses its own original
instant; changed fields and new events retain the existing local-date conversion.
Clearing the optional end still sends null. Resetting the editor clears the snapshots.

## Source and focused execution (4 October 2026)

Baseline: `035011b5b1ee1069342502067b666722218bed20`, exact tab blob
`46a6c5793377f83c25d841e1c4d761afaec8274c`.
Candidate tab blob: `28bdcd4d6e9ba188819849e258567046716d8fb2`.
Date-helper blob: `cf3301e4c4ef33f93dfd663bc8f3946fc38a4ce3`.

The same ten assertions ran against the original conversion functions extracted
unchanged from that tab, then the new helper: **3 pass / 7 fail -> 10 pass / 0 fail**.
The baseline adapter only exposed the old functions under the new test interface.
Execution used Node 22.16.0, TypeScript 5.8.3, ICU 77.1 and tzdata 2025b. TypeScript
transpilation replaced only Vitest's test registration with `node:test` and resolved
the helper import locally; native Date and node:assert were unchanged. Vitest and
application dependencies were unavailable in this environment.

Cases cover lost sub-minute precision; the later repeated hour in New York and
Berlin; Lord Howe's repeated half-hour; separately preserving the unchanged end;
intentional edits; new events; device-timezone changes during editing; original
ISO offset spelling; and invalid-date errors. The helper's standalone strict
TypeScript check and the updated tab's syntax transpilation passed.

Repository regression command (for an installed workspace):

```sh
pnpm --dir fluxer_app exec vitest run src/features/guild/utils/GuildEventDateUtils.test.ts --pool=forks --maxWorkers=1
```

This is focused date-conversion execution, not a mounted React interaction,
Cassandra lifecycle, full application typecheck/build, or new browser acceptance.
Previous application-build results stay attached to their original source.
