# Keep image reads attached to the current event draft

The previous editor started an independent FileReader promise for every selection.
A slower old selection could replace newer artwork, including after the editor was
reset or another event was selected. Read failures were not handled by the editor,
and Save could run before the selected image bytes arrived.

GuildEventImageReader now owns one current reader, aborts and detaches superseded
readers, ignores stale callbacks, and reports completion or errors to the editor.
Reset, edit-switch, image removal and unmount cancel the active read. Save is
disabled while reading; its submit handler also checks that state. The file input
resets after selection so the same file can be retried. No upload, scan, media
proxy, schema, permission or server persistence behavior changes.

## Source and focused execution (4 October 2026)

Baseline is the existing events branch at
`0926e0240cfb426694c5b663991cc00079d84689`; its tab blob is
`28bdcd4d6e9ba188819849e258567046716d8fb2`.
Candidate tab: `aa6cdde8a423d36d0be1da5a5b2532c6a1a54101`.
Reader helper: `a3398d0943ca6b5b5142967816dfd042b0e24d89`.
Regression: `5c40864dabbbf115f376a460a30a2f50e463d0fc`.

A bounded comparison executed the exact original fileAsDataUrl function extracted
from that tab, then the production reader helper, with the same controlled
FileReader completion order. Finishing the newer selection before the older one
left `obsolete` as the old editor's final image; the repaired helper retained
`newest`. Twenty held selections produced 20 outstanding readers and zero aborts
before, versus one outstanding reader and 19 aborts after. These are deterministic
adapter counts, not browser memory or throughput measurements.

Eight focused maintained regression cases pass: pending state, latest selection,
cancellation, read failure and retry, synchronous start failure, silent unmount
cleanup, invalid result, and bounded outstanding reads. The test exercises the
production helper with a controlled FileReader adapter, not a native browser.
Execution used Node 22.16.0 and TypeScript 5.8.3; only Vitest's test registration was
replaced with node:test and the helper import resolved locally. Assertions and
production helper bodies were unchanged. The helper's strict standalone TypeScript
check and the updated tab's syntax transpilation passed. The tab wiring was
inspected, not mounted in React. Earlier full application results remain pinned
to their earlier source; no current-source whole-app or live-media pass is claimed.

For an installed repository workspace:

```sh
pnpm --dir fluxer_app exec vitest run src/features/guild/utils/GuildEventImageReader.test.ts --pool=forks --maxWorkers=1
```
