// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {VoiceEngineV2Command} from '@fluxer/voice_engine_v2/src/protocol/commands';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';

export const VOICE_ENGINE_V2_EVENT_LOG_CAP = 4096;

export interface VoiceEngineV2EventLogEntry {
	sequence: number;
	atMs: number;
	event: VoiceEngineV2Event;
	commands: Array<VoiceEngineV2Command>;
}

export interface VoiceEngineV2EventLogSpillSink {
	write(entry: VoiceEngineV2EventLogEntry): Promise<void>;
}

export const VOICE_ENGINE_V2_MEMORY_EVENT_LOG_SPILL_SINK_CAP = 8192;

export interface VoiceEngineV2MemoryEventLogSpillSink extends VoiceEngineV2EventLogSpillSink {
	readonly entries: ReadonlyArray<VoiceEngineV2EventLogEntry>;
	readonly droppedEntriesCount: number;
}

export function createVoiceEngineV2MemoryEventLogSpillSink(
	capacity: number = VOICE_ENGINE_V2_MEMORY_EVENT_LOG_SPILL_SINK_CAP,
): VoiceEngineV2MemoryEventLogSpillSink {
	assert.ok(Number.isInteger(capacity), 'memory event log spill sink capacity must be an integer');
	assert.ok(capacity >= 1, 'memory event log spill sink capacity must be at least 1');
	assert.ok(capacity <= 1_048_576, 'memory event log spill sink capacity must fit within a sane upper bound');
	const entries: Array<VoiceEngineV2EventLogEntry> = [];
	let droppedEntriesCount = 0;
	return {
		get entries(): ReadonlyArray<VoiceEngineV2EventLogEntry> {
			return entries.slice();
		},
		get droppedEntriesCount(): number {
			assert.ok(droppedEntriesCount >= 0, 'memory event log spill sink dropped count must be non-negative');
			return droppedEntriesCount;
		},
		write(entry: VoiceEngineV2EventLogEntry): Promise<void> {
			assert.ok(entry !== null && typeof entry === 'object', 'memory event log spill entry must be an object');
			assert.ok(Number.isInteger(entry.sequence), 'memory event log spill entry sequence must be an integer');
			assert.ok(entry.sequence >= 1, 'memory event log spill entry sequence must be >= 1');
			if (entries.length === capacity) {
				entries.shift();
				droppedEntriesCount += 1;
			}
			entries.push(entry);
			assert.ok(entries.length <= capacity, 'memory event log spill sink entries must stay bounded');
			return Promise.resolve();
		},
	};
}

export class VoiceEngineV2EventLogRing {
	private readonly capacity: number;
	private readonly slots: Array<VoiceEngineV2EventLogEntry | null>;
	private headSlot = 0;
	private storedCount = 0;
	private nextSequenceValue = 1;
	private droppedCount = 0;
	private evictedSequenceMinValue: number | null = null;

	constructor(capacity: number = VOICE_ENGINE_V2_EVENT_LOG_CAP) {
		assert.ok(Number.isInteger(capacity), 'event log capacity must be an integer');
		assert.ok(capacity >= 1, 'event log capacity must be at least 1');
		assert.ok(capacity <= 1_048_576, 'event log capacity must fit within a sane upper bound');
		this.capacity = capacity;
		this.slots = new Array<VoiceEngineV2EventLogEntry | null>(capacity).fill(null);
	}

	get size(): number {
		assert.ok(this.storedCount >= 0, 'stored count cannot be negative');
		assert.ok(this.storedCount <= this.capacity, 'stored count cannot exceed capacity');
		return this.storedCount;
	}

	get cap(): number {
		assert.ok(this.capacity >= 1, 'capacity must remain >= 1');
		return this.capacity;
	}

	get nextSequence(): number {
		assert.ok(this.nextSequenceValue >= 1, 'next sequence must remain >= 1');
		return this.nextSequenceValue;
	}

	get droppedEventsCount(): number {
		assert.ok(this.droppedCount >= 0, 'dropped count cannot be negative');
		return this.droppedCount;
	}

	get evictedSequenceMin(): number | null {
		if (this.droppedCount === 0)
			assert.equal(this.evictedSequenceMinValue, null, 'eviction min must be null when nothing dropped');
		if (this.droppedCount > 0)
			assert.ok(this.evictedSequenceMinValue !== null, 'eviction min must be set when drops occurred');
		return this.evictedSequenceMinValue;
	}

	allocateSequence(): number {
		const sequence = this.nextSequenceValue;
		assert.ok(sequence >= 1, 'allocated sequence must be >= 1');
		this.nextSequenceValue = sequence + 1;
		return sequence;
	}

	push(entry: VoiceEngineV2EventLogEntry): VoiceEngineV2EventLogEntry | null {
		assert.ok(Number.isInteger(entry.sequence), 'entry sequence must be an integer');
		assert.ok(entry.sequence >= 1, 'entry sequence must be >= 1');
		assert.equal(entry.sequence + 1, this.nextSequenceValue, 'entry sequence must match allocated sequence');
		let evicted: VoiceEngineV2EventLogEntry | null = null;
		if (this.storedCount === this.capacity) {
			evicted = this.evictOldest();
		}
		const insertIndex = (this.headSlot + this.storedCount) % this.capacity;
		assert.equal(this.slots[insertIndex], null, 'insert slot must be empty before writing');
		this.slots[insertIndex] = entry;
		this.storedCount += 1;
		assert.ok(this.storedCount <= this.capacity, 'stored count must remain bounded by capacity');
		return evicted;
	}

	get tailEntry(): VoiceEngineV2EventLogEntry | null {
		assert.ok(this.storedCount >= 0, 'stored count cannot be negative');
		if (this.storedCount === 0) return null;
		const tailIndex = (this.headSlot + this.storedCount - 1) % this.capacity;
		const entry = this.slots[tailIndex];
		assert.ok(entry !== null, 'tail slot must contain an entry when entries are stored');
		assert.ok(entry !== undefined, 'tail slot index must stay within the slot array');
		return entry;
	}

	replaceTail(entry: VoiceEngineV2EventLogEntry): void {
		assert.ok(this.storedCount >= 1, 'replaceTail requires at least one stored entry');
		const tailIndex = (this.headSlot + this.storedCount - 1) % this.capacity;
		const current = this.slots[tailIndex];
		assert.ok(current !== null, 'tail slot must contain an entry before replacement');
		assert.ok(current !== undefined, 'tail slot index must stay within the slot array');
		assert.equal(entry.sequence, current.sequence, 'replaceTail must preserve the tail sequence');
		this.slots[tailIndex] = entry;
	}

	snapshotEntries(): ReadonlyArray<VoiceEngineV2EventLogEntry> {
		assert.ok(this.storedCount <= this.capacity, 'stored count must remain bounded by capacity');
		const result: Array<VoiceEngineV2EventLogEntry> = new Array<VoiceEngineV2EventLogEntry>(this.storedCount);
		const limit = this.storedCount;
		for (let cursor = 0; cursor < limit; cursor += 1) {
			const slotIndex = (this.headSlot + cursor) % this.capacity;
			const entry = this.slots[slotIndex];
			assert.ok(entry !== null, 'snapshotEntries must not encounter empty slots within stored range');
			result[cursor] = entry;
		}
		return result;
	}

	clear(): void {
		assert.ok(this.capacity >= 1, 'capacity must remain >= 1 before clearing');
		const limit = this.capacity;
		for (let index = 0; index < limit; index += 1) {
			this.slots[index] = null;
		}
		this.headSlot = 0;
		this.storedCount = 0;
		this.nextSequenceValue = 1;
		this.droppedCount = 0;
		this.evictedSequenceMinValue = null;
		assert.equal(this.storedCount, 0, 'clear must leave stored count at zero');
		assert.equal(this.nextSequenceValue, 1, 'clear must reset the sequence allocator to one');
	}

	private evictOldest(): VoiceEngineV2EventLogEntry {
		assert.equal(this.storedCount, this.capacity, 'evictOldest must only run when full');
		const head = this.slots[this.headSlot];
		assert.ok(head !== null, 'head slot must contain an entry when evicting');
		this.slots[this.headSlot] = null;
		this.headSlot = (this.headSlot + 1) % this.capacity;
		this.storedCount -= 1;
		this.droppedCount += 1;
		if (this.evictedSequenceMinValue === null) {
			this.evictedSequenceMinValue = head.sequence;
		}
		assert.ok(this.droppedCount >= 1, 'dropped count must be >= 1 after eviction');
		return head;
	}
}

export function assertEventLogRingInvariants(ring: VoiceEngineV2EventLogRing): void {
	const entries = ring.snapshotEntries();
	const observedSize = entries.length;
	assert.equal(observedSize, ring.size, 'ring size must match snapshot length');
	assert.ok(observedSize <= ring.cap, 'ring size must remain bounded by cap');
	const droppedCount = ring.droppedEventsCount;
	const evictedMin = ring.evictedSequenceMin;
	if (droppedCount === 0) {
		assert.equal(evictedMin, null, 'no drops implies evicted min is null');
	}
	if (droppedCount > 0) {
		assert.ok(evictedMin !== null, 'drops imply evicted min is set');
		assert.equal(evictedMin, 1, 'oldest evicted sequence must be 1 because sequences start at 1 and drop oldest');
	}
	if (observedSize >= 1) {
		const firstSequence = entries[0]?.sequence ?? 0;
		const lastSequence = entries[observedSize - 1]?.sequence ?? 0;
		assert.equal(lastSequence - firstSequence + 1, observedSize, 'sequences must form a contiguous window');
		const expectedFirstSequence = droppedCount + 1;
		assert.equal(firstSequence, expectedFirstSequence, 'first sequence must equal dropped count + 1');
		const cursorLimit = observedSize;
		for (let cursor = 1; cursor < cursorLimit; cursor += 1) {
			const previousSequence = entries[cursor - 1]?.sequence ?? 0;
			const currentSequence = entries[cursor]?.sequence ?? 0;
			assert.equal(currentSequence, previousSequence + 1, 'sequences must increase by exactly one across the window');
		}
	}
}
