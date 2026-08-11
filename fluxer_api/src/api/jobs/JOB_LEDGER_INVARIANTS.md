# Job ledger lifecycle invariants

This ledger is a recoverable distributed state machine. `jobs_by_id` is authoritative. `jobs_active`,
`jobs_by_day_bucket`, and the JetStream delivery are projections or recovery mechanisms; none may
silently override authoritative state.

The table below is the required review surface. Test names are behavioral contracts rather than
implementation details.

## Authoritative state and projections

| Invariant / failure boundary | Required result | Deterministic regression |
| --- | --- | --- |
| Create authority before publishing broker work | Publication never precedes a complete authoritative row; failed publication terminalizes the row | `WorkerService.test.ts`: “persists a complete ledger record before publishing the queue message”, “marks a precreated ledger job failed when queue publication fails”, “does not publish when authoritative ledger creation fails” |
| Non-terminal mutation wins | Authoritative, active, and history rows converge to the same non-terminal state without a terminal TTL | `JobLedgerRepository.test.ts`: “atomically releases running work to a claimable queued retry”, “repairs a speculative terminal bucket when the authoritative CAS errors” |
| Non-terminal mutation loses a race | The winner is re-read; neither active nor history may be overwritten from the loser | `JobLedgerRepository.test.ts`: “does not overwrite a newer running active index while releasing a retry”, “does not resurrect an active row when a newer claimant terminalizes during retry repair” |
| Terminal transition before authoritative CAS | History is written with its final retention deadline while active remains as a durable repair index | `JobLedgerRepository.test.ts`: “writes terminal bucket retention before the authoritative terminal CAS” |
| Terminal CAS loses to a non-terminal winner | Both secondary projections are restored from authority and speculative TTL is removed | `JobLedgerRepository.test.ts`: “repairs the history bucket when reconciliation terminalization loses to lease renewal” |
| Terminal CAS loses to a terminal winner | The winning terminal status and original retention deadline replace the speculative loser | `JobLedgerRepository.test.ts`: “repairs the winning terminal bucket when a conflicting terminal CAS loses” |
| Terminal CAS errors before application | Best-effort immediate repair restores projections; the primary error remains visible | `JobLedgerRepository.test.ts`: “repairs a speculative terminal bucket when the authoritative CAS errors” |
| Immediate conflict repair also errors | Active state remains discoverable and bounded reconciliation restores non-terminal history and removes speculative TTL | `JobLedgerRepository.test.ts`: “uses reconciliation to repair a speculative bucket after immediate conflict repair also fails” |
| Authoritative CAS succeeds but active deletion errors | Authority and terminal history remain valid; retrying the same terminal outcome idempotently repairs active state | `JobLedgerRepository.test.ts`: “repairs terminal indexes when retrying after active-index deletion fails” |
| Authority is terminal but active remains | Reconciliation rewrites history with remaining—not restarted—retention and removes active | `JobLedgerRepository.test.ts`: “repairs a terminal day bucket using only the authoritative remaining retention” |
| Authority has expired or is missing | Reconciliation deletes both derivable secondary rows | `JobLedgerRepository.test.ts`: “removes orphaned and terminal active-index rows” |
| Retention deadline passes | Complete authoritative and history records expire together | `JobLedgerRepository.test.ts`: “expires complete terminal history rows after the retention window” |

## Lease ownership and lifecycle fencing

| Invariant / interleaving | Required result | Deterministic regression |
| --- | --- | --- |
| Expired-lease takeover races owner renewal | Takeover conditions on observed token and expiry; renewal wins | `JobLedgerRepository.test.ts`: “does not steal an expired lease that is concurrently renewed by its owner” |
| Reconciliation races renewal | Reconciliation conditions on the complete observed lease generation | `JobLedgerRepository.test.ts`: “does not fail a running job that renews while reconciliation is terminalizing it” |
| Legacy null-lease reconciliation races claim | Null token and null expiry are fenced; claimant wins | `JobLedgerRepository.test.ts`: “does not fail a legacy row that is concurrently claimed during reconciliation” |
| Stale owner mutates after takeover | Completion and every non-terminal mutation are rejected by lease CAS | `JobLedgerRepository.test.ts`: “reclaims an expired running lease while rejecting a fresh lease and stale owner completion”, “does not regress or resurrect terminal jobs through late worker mutations” |
| Cancellation loses terminal CAS | The caller receives `false`; the broker message is retained | `WorkerService.test.ts`: “returns false when cancellation loses its terminalization compare-and-set race”; `WorkerRunnerPayload.test.ts`: “keeps the broker message when cancellation cannot be terminalized” |
| Retry release | Running work atomically becomes queued, clears lease ownership, increments attempts once, and remains claimable | `JobLedgerRepository.test.ts`: “atomically releases running work to a claimable queued retry”; `WorkerRunnerPayload.test.ts`: “releases a failed attempt so the broker redelivery can claim and execute the retry” |
| Stale queued observation races a claim/retry ABA cycle | Reconciliation conditions on the observed transition generation and attempt count; it cannot overwrite or regress the released retry | `JobLedgerRepository.test.ts`: “does not terminalize a queued retry released after stale reconciliation observation” |
| Dead-letter publication | Durable pending state precedes publication; one renewable publication lease owns each generation; contenders NAK without publishing; reconciliation fences the complete observed generation | `JobLedgerRepository.test.ts`: pending transition and publication/reconciliation race tests; `WorkerRunnerPayload.test.ts`: publication-owner, pending-DLQ retry, and retained-ledger tests |

## Scheduling and recovery age

| Invariant | Required result | Deterministic regression |
| --- | --- | --- |
| Maximum accepted schedule versus source retention | Source `max_age` is at least maximum schedule delay plus the full recovery window; existing streams are upgraded | `JetStreamWorkerQueue.test.ts`: “retains permitted 30-day schedules plus the queue recovery window”, “updates an existing source stream to the schedule-safe retained lifetime” |
| Scheduled-message authority versus source retention | The authoritative scheduled-message row survives for the same complete schedule-plus-recovery horizon as its source delivery | `JobSchedulingPolicy.test.ts`: “retains scheduled-message authority for the complete queue schedule and recovery horizon” |
| Future delivery handling | The original durable delivery receives one native delayed NAK; it is not acknowledged and republished | `WorkerRunnerPayload.test.ts`: “preserves the ledger identity through the real future-message processing path” |
| Newly scheduled queued age | Recovery starts at the later of due time and queued transition time | `JobLedgerRepository.test.ts`: scheduled pending/due/future tests |
| Scheduled retry age | A retry after the original due date ages from its new queued transition | `JobLedgerRepository.test.ts`: “ages a scheduled retry from its latest queued transition after the original due time” |
| Dead-letter recovery age | Age is measured from the dead-letter transition, not creation | `JobLedgerRepository.test.ts`: “fails dead-letter-pending work only after the source stream recovery window expires” |

The shared policy in `JobSchedulingPolicy.ts` is the single source for maximum scheduling delay, queue
recovery window, and source-stream lifetime. The API validator, stream configuration, and reconciler
must import it rather than duplicate durations.

## Bounded reconciliation and pagination

| Invariant | Required result | Deterministic regression |
| --- | --- | --- |
| Bounded active scan | Physical query carries a limit and a continuation predicate | PostgreSQL query-shape and integration tests; `JobLedgerRepository.test.ts`: bounded rotation tests |
| Fair wraparound | Cursor ordering and comparison describe the same traversal; shuffled insertion `[93, 91, 92]` with limit one visits every row | `JobLedgerRepository.test.ts`: “rotates fairly across shuffled active-row insertion order” |
| Stable history cursor | Cursor identifies the last returned `(created_at, job_id)` and strict tuple continuation cannot skip equal timestamps | `JobLedgerRepository.test.ts`: stable cursor and equal-timestamp tests |
| Sparse filters | Bounded chunks continue until a page is filled or lookback ends | `JobLedgerRepository.test.ts`: “continues scanning a bucket until sparse filters fill the page” |
| Backend parity | Cassandra metadata, emitted PostgreSQL, and compatibility executor apply the same predicates, deterministic order, null conditions, and limit | `CassandraTableDsl.test.ts`, `PostgresKvQueryExecutor.test.ts`, `PostgresKvQueryExecutor.Integration.test.ts` |
| Compatibility paging under mutation | PostgreSQL uses a physical `LIMIT` and a typed, order-bound keyset cursor; deletion before the cursor cannot skip later rows; declared mixed clustering order is preserved | `PostgresKvQueryExecutor.test.ts`: bounded page and limited-IN query shapes; `PostgresKvQueryExecutor.Integration.test.ts`: mixed-direction keyset deletion regression |

## Worker and broker lifecycle

| Invariant / interleaving | Required result | Deterministic regression |
| --- | --- | --- |
| Stop with active plus prefetched work | Active work drains; prefetched work never starts and is released | `WorkerRunnerPayload.test.ts`: “drains active work and does not start prefetched work after stop begins” |
| Stop during consumer acquisition | A consumer acquired after stop starts is closed before `start()` returns | `WorkerRunnerPayload.test.ts`: “closes a consumer acquired after stop begins before start can return” |
| Consumer close fails | Close error is preserved only after already-started work drains | `WorkerRunnerPayload.test.ts`: “drains active work even when closing the consumer fails” |
| Concurrent stop calls overlap a close failure | Every caller joins one stop promise; no caller waits separately on a stuck iterator | `WorkerRunnerPayload.test.ts`: “joins concurrent stop callers when consumer close fails instead of awaiting a stuck iterator” |
| Fatal and graceful shutdown overlap | All callers join one complete backend cleanup; all runners are awaited even when one stop rejects | `WorkerMainLifecycle.test.ts`: joinable-shutdown and all-runner-settlement tests |
| Lease heartbeat | Durable lease and broker acknowledgement timer renew for the complete active execution | `WorkerRunnerPayload.test.ts`: “renews the durable lease and broker ack timer while task execution is active” |
| Terminal CAS loses | Worker retains the broker message rather than acknowledging uncommitted work | `WorkerRunnerPayload.test.ts`: successful/cancellation terminalization-loss tests |
| DLQ deduplication | Stable message ID and duplicate window cover the retained DLQ lifetime | `JetStreamWorkerQueue.test.ts`: DLQ duplicate-window and stable-message-ID tests |

## Review gate

A candidate is eligible for approval only when all of the following bind to the same staged Git tree:

1. focused deterministic regressions pass;
2. TypeScript and Biome pass;
3. real PostgreSQL integration passes;
4. Cassandra-specific conditional/schema semantics are checked;
5. the complete API suite passes; and
6. two independent read-only reviewers return explicit `PASS` for that exact tree.

Any source, test, schema, or documentation edit invalidates earlier full-suite and review evidence.
