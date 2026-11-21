// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CdcLifeRecord} from '@app/api/database/capture/CdcRecords';

const GAP_WAIT_MS = 5 * 60_000;
const CLOSED_WAIT_MS = 5 * 60_000;
const SILENT_MS = 15 * 60_000;

type LossKind = 'gap' | 'lossy' | 'unclosed';

export interface Loss {
	kind: LossKind;
	producer: string;
	detail: string;
}

export interface ProducerProgress {
	pod: string;
	applied: number;
	above: Set<number>;
	highest: number;
	gapSinceMs: number | null;
	lastMs: number;
	closed: {lastSeq: number; atMs: number} | null;
}

export class LossTracker {
	private readonly producers: Map<string, ProducerProgress>;
	private readonly changed = new Set<string>();

	constructor(restored: ReadonlyMap<string, ProducerProgress> = new Map()) {
		this.producers = new Map(
			[...restored].map(([id, progress]) => [id, {...progress, above: new Set(progress.above)}]),
		);
	}

	key(producer: string, seq: number, atMs: number): void {
		const progress = this.progress(producer, atMs);
		if (seq > progress.applied) {
			progress.above.add(seq);
			while (progress.above.delete(progress.applied + 1)) progress.applied++;
		}
		this.raiseHighest(progress, seq, atMs);
	}

	life(record: CdcLifeRecord, atMs: number): Array<Loss> {
		const losses: Array<Loss> = [];
		if (record.state === 'disabled') return losses;
		if (record.state === 'open') {
			for (const [id, other] of this.producers) {
				if (id === record.producer || other.pod !== record.pod || other.closed !== null) continue;
				losses.push({kind: 'unclosed', producer: id, detail: `${record.pod} opened again without a closed record`});
				this.forget(id);
			}
			this.progress(record.producer, atMs).pod = record.pod;
			return losses;
		}
		const progress = this.progress(record.producer, atMs);
		switch (record.state) {
			case 'alive':
				this.raiseHighest(progress, record.last_seq, atMs);
				break;
			case 'lossy':
				losses.push({kind: 'lossy', producer: record.producer, detail: `lost seq ${record.from_seq}-${record.to_seq}`});
				break;
			case 'closed':
				progress.closed = {lastSeq: record.last_seq, atMs};
				this.raiseHighest(progress, record.last_seq, atMs);
				if (record.dropped > 0) {
					losses.push({kind: 'lossy', producer: record.producer, detail: `closed with ${record.dropped} dropped`});
				}
				break;
		}
		return losses;
	}

	check(nowMs: number): Array<Loss> {
		const losses: Array<Loss> = [];
		for (const [id, progress] of this.producers) {
			if (progress.gapSinceMs !== null && nowMs - progress.gapSinceMs >= GAP_WAIT_MS) {
				const detail = `seq ${progress.applied + 1} missing below ${progress.highest}`;
				losses.push({kind: 'gap', producer: id, detail});
				progress.applied = progress.highest;
				progress.above.clear();
				progress.gapSinceMs = null;
				this.changed.add(id);
			}
			if (progress.closed !== null && nowMs - progress.closed.atMs >= CLOSED_WAIT_MS) {
				this.forget(id);
			} else if (progress.closed === null && nowMs - progress.lastMs >= SILENT_MS) {
				losses.push({kind: 'unclosed', producer: id, detail: 'no record for 15 minutes and no closed record'});
				this.forget(id);
			}
		}
		return losses;
	}

	unclosed(): number {
		return [...this.producers.values()].filter((progress) => progress.closed === null).length;
	}

	takeChanges(): Map<string, ProducerProgress | null> {
		const changes = new Map([...this.changed].map((id) => [id, this.producers.get(id) ?? null]));
		this.changed.clear();
		return changes;
	}

	private progress(producer: string, atMs: number): ProducerProgress {
		let progress = this.producers.get(producer);
		if (progress === undefined) {
			progress = {
				pod: producer.split(':')[0] ?? producer,
				applied: 0,
				above: new Set(),
				highest: 0,
				gapSinceMs: null,
				lastMs: atMs,
				closed: null,
			};
			this.producers.set(producer, progress);
		}
		progress.lastMs = Math.max(progress.lastMs, atMs);
		this.changed.add(producer);
		return progress;
	}

	private raiseHighest(progress: ProducerProgress, seq: number, atMs: number): void {
		progress.highest = Math.max(progress.highest, seq, progress.applied);
		if (progress.highest <= progress.applied) progress.gapSinceMs = null;
		else progress.gapSinceMs ??= atMs;
	}

	private forget(id: string): void {
		this.producers.delete(id);
		this.changed.add(id);
	}
}
