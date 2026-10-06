// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	assertEventShape,
	assertStateShape,
	createInitialActiveState,
	type SourceLifecycleAction,
	type SourceLifecycleClock,
	SourceLifecycleError,
	type SourceLifecycleEvent,
	type SourceLifecycleState,
	transitionSourceLifecycle,
} from '@fluxer/voice_engine_v2/src/source_isolation/SourceLifecycleState';

export const MAX_TRACKED_SOURCES = 256;

export interface SourceLifecycleSnapshotEntry {
	sourceId: string;
	state: SourceLifecycleState;
}

export type SourceLifecycleDispatchResult =
	| {ok: true; action: SourceLifecycleAction; state: SourceLifecycleState}
	| {ok: false; error: 'unknownSource' | 'invalidSourceId'};

export class SourceLifecycleRegistry {
	private readonly states: Map<string, SourceLifecycleState>;
	private readonly clock: SourceLifecycleClock;
	private readonly cap: number;

	constructor(clock: SourceLifecycleClock, cap: number = MAX_TRACKED_SOURCES) {
		assertClockObject(clock);
		assertCap(cap);
		this.clock = clock;
		this.cap = cap;
		this.states = new Map<string, SourceLifecycleState>();
	}

	register(
		sourceId: string,
	):
		| {ok: true; state: SourceLifecycleState}
		| {ok: false; error: 'capExceeded' | 'duplicateSource' | 'invalidSourceId'} {
		if (!isValidSourceId(sourceId)) {
			return {ok: false, error: 'invalidSourceId'};
		}
		if (this.states.has(sourceId)) {
			return {ok: false, error: 'duplicateSource'};
		}
		if (this.states.size >= this.cap) {
			return {ok: false, error: 'capExceeded'};
		}
		const state = createInitialActiveState(this.clock);
		assertStateShape(state);
		this.states.set(sourceId, state);
		this.assertCapInvariant();
		return {ok: true, state};
	}

	dispatch(sourceId: string, event: SourceLifecycleEvent): SourceLifecycleDispatchResult {
		if (!isValidSourceId(sourceId)) {
			return {ok: false, error: 'invalidSourceId'};
		}
		assertEventShape(event);
		const current = this.states.get(sourceId);
		if (current === undefined) {
			return {ok: false, error: 'unknownSource'};
		}
		assertStateShape(current);
		const result = transitionSourceLifecycle(current, event, this.clock);
		this.states.set(sourceId, result.state);
		this.assertCapInvariant();
		return {ok: true, action: result.action, state: result.state};
	}

	remove(sourceId: string): boolean {
		if (!isValidSourceId(sourceId)) {
			return false;
		}
		const removed = this.states.delete(sourceId);
		this.assertCapInvariant();
		return removed;
	}

	get(sourceId: string): SourceLifecycleState | undefined {
		if (!isValidSourceId(sourceId)) {
			return undefined;
		}
		return this.states.get(sourceId);
	}

	size(): number {
		const size = this.states.size;
		if (size > this.cap) {
			throw new SourceLifecycleError('invariantViolated', `registry size exceeds cap (${size} > ${this.cap})`);
		}
		return size;
	}

	snapshot(): ReadonlyArray<SourceLifecycleSnapshotEntry> {
		this.assertCapInvariant();
		const entries: Array<SourceLifecycleSnapshotEntry> = [];
		const sortedIds = Array.from(this.states.keys()).sort();
		for (const sourceId of sortedIds) {
			const state = this.states.get(sourceId);
			if (state === undefined) {
				throw new SourceLifecycleError('invariantViolated', `snapshot missing state for ${sourceId}`);
			}
			assertStateShape(state);
			entries.push({sourceId, state});
		}
		if (entries.length !== this.states.size) {
			throw new SourceLifecycleError('invariantViolated', 'snapshot length mismatch');
		}
		return entries;
	}

	private assertCapInvariant(): void {
		if (this.states.size > this.cap) {
			throw new SourceLifecycleError('invariantViolated', `registry size ${this.states.size} exceeds cap ${this.cap}`);
		}
		if (this.states.size < 0) {
			throw new SourceLifecycleError('invariantViolated', `registry size ${this.states.size} negative`);
		}
	}
}

function isValidSourceId(sourceId: string): boolean {
	if (typeof sourceId !== 'string') return false;
	if (sourceId.length === 0) return false;
	if (sourceId.length > 256) return false;
	return true;
}

function assertCap(cap: number): void {
	if (!Number.isInteger(cap)) {
		throw new SourceLifecycleError('invariantViolated', `cap must be integer (got ${cap})`);
	}
	if (cap < 1) {
		throw new SourceLifecycleError('invariantViolated', `cap must be >= 1 (got ${cap})`);
	}
	if (cap > MAX_TRACKED_SOURCES) {
		throw new SourceLifecycleError('invariantViolated', `cap must be <= ${MAX_TRACKED_SOURCES} (got ${cap})`);
	}
}

function assertClockObject(clock: SourceLifecycleClock): void {
	if (clock === null || typeof clock !== 'object') {
		throw new SourceLifecycleError('invariantViolated', 'clock must be an object');
	}
	if (typeof clock.nowNs !== 'function') {
		throw new SourceLifecycleError('invariantViolated', 'clock.nowNs must be a function');
	}
}
