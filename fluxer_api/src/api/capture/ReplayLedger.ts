// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	IntentRecord,
	LifeRecord,
	LwtIntentRecord,
	OutcomeRecord,
	ProducerState,
	SettleReason,
	SettleRecord,
} from '@app/api/database/capture/CaptureRecords';

const ORPHAN_AFTER_MS = 60_000;

type Intent = IntentRecord | LwtIntentRecord;

export type LedgerAction<H> =
	| {kind: 'apply'; intent: IntentRecord; handle: H}
	| {kind: 'settle'; keys: Array<SettleRecord>; handle: H}
	| {kind: 'drop'; handle: H; reason: 'failed' | 'not_applied'}
	| {kind: 'broken'; reason: string};

interface Pending<H> {
	intent: Intent;
	streamMs: number;
	handle: H;
}

function settleReason(intent: Intent, applied: boolean): SettleReason {
	if (intent.kind === 'i') return 'unknown';
	return applied ? 'lwt' : 'lwt_unknown';
}

function settleKeys(intent: Intent, reason: SettleReason): Array<SettleRecord> | null {
	if (intent.kind === 'l') {
		if (intent.rows.length === 0) return null;
		return intent.rows.map(({t, c, insert}) => ({
			kind: 'settle',
			t,
			s: 'row',
			c,
			reason,
			ts: intent.ts,
			...(insert ? {insert} : {}),
		}));
	}
	const keys: Array<SettleRecord> = [];
	for (const statement of intent.statements) {
		if (!statement.keys?.length) return null;
		for (const key of statement.keys) {
			keys.push({kind: 'settle', t: statement.t, s: key.s, c: key.c, reason, ts: intent.ts});
		}
	}
	return keys;
}

export class ReplayLedger<H> {
	private readonly producers = new Map<string, ProducerState>();
	private readonly pending = new Map<string, Pending<H>>();

	life(record: LifeRecord): Array<LedgerAction<H>> {
		this.producers.set(record.producer, record.state);
		if (record.state === 'disabled') {
			return [{kind: 'broken', reason: `producer ${record.producer} turned capture off`}];
		}
		if (record.state === 'open') return [];
		const actions: Array<LedgerAction<H>> = [];
		for (const [id, {intent, handle}] of this.pending) {
			if (intent.producer !== record.producer) continue;
			this.pending.delete(id);
			actions.push(this.settle(intent, handle, settleReason(intent, false)));
		}
		return actions;
	}

	intent(intent: Intent, streamMs: number, handle: H): Array<LedgerAction<H>> {
		if (this.producers.get(intent.producer) === 'sealed') {
			return [{kind: 'broken', reason: `sealed producer ${intent.producer} wrote seq ${intent.seq}`}];
		}
		this.pending.set(`${intent.producer}:${intent.seq}`, {intent, streamMs, handle});
		return [];
	}

	outcome(record: OutcomeRecord): Array<LedgerAction<H>> {
		if (this.producers.get(record.producer) === 'sealed') {
			return [{kind: 'broken', reason: `sealed producer ${record.producer} sent outcomes`}];
		}
		const actions: Array<LedgerAction<H>> = [];
		for (const [seq, outcome] of record.outcomes) {
			const id = `${record.producer}:${seq}`;
			const pending = this.pending.get(id);
			if (!pending) continue;
			this.pending.delete(id);
			const {intent, handle} = pending;
			if (outcome === 'failed' || outcome === 'not_applied') {
				actions.push({kind: 'drop', handle, reason: outcome});
			} else if (intent.kind === 'i' && outcome === 'applied') {
				actions.push({kind: 'apply', intent, handle});
			} else {
				actions.push(this.settle(intent, handle, settleReason(intent, outcome === 'applied')));
			}
		}
		return actions;
	}

	orphans(streamMs: number): Array<LedgerAction<H>> {
		const actions: Array<LedgerAction<H>> = [];
		for (const [id, {intent, streamMs: seenMs, handle}] of this.pending) {
			if (streamMs - seenMs <= ORPHAN_AFTER_MS) break;
			this.pending.delete(id);
			actions.push(this.settle(intent, handle, settleReason(intent, false)));
		}
		return actions;
	}

	awaitingOutcome(): number {
		return this.pending.size;
	}

	producerStates(): ReadonlyMap<string, ProducerState> {
		return this.producers;
	}

	allProducersFinished(): boolean {
		if (this.producers.size === 0) return false;
		for (const state of this.producers.values()) {
			if (state !== 'closed' && state !== 'sealed') return false;
		}
		return true;
	}

	private settle(intent: Intent, handle: H, reason: SettleReason): LedgerAction<H> {
		const keys = settleKeys(intent, reason);
		if (keys === null) {
			return {kind: 'broken', reason: `${intent.producer} seq ${intent.seq} has an unknown outcome and no key`};
		}
		return {kind: 'settle', keys, handle};
	}
}
