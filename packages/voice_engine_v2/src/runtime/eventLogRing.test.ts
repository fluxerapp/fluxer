// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {transitionVoiceEngineV2} from '@fluxer/voice_engine_v2/src/core/reducer';
import {
	availableVoiceEngineV2Capabilities,
	createVoiceEngineV2InitialSnapshot,
	type VoiceEngineV2Snapshot,
} from '@fluxer/voice_engine_v2/src/core/state';
import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import type {VoiceEngineV2InboundVideoFrame} from '@fluxer/voice_engine_v2/src/protocol/types';
import {
	assertEventLogRingInvariants,
	createVoiceEngineV2MemoryEventLogSpillSink,
	VOICE_ENGINE_V2_EVENT_LOG_CAP,
	type VoiceEngineV2EventLogEntry,
	VoiceEngineV2EventLogRing,
	type VoiceEngineV2EventLogSpillSink,
} from '@fluxer/voice_engine_v2/src/runtime/eventLogRing';
import {
	assertEventLogInvariants,
	type VoiceEngineV2EventLogEntry as RuntimeEventLogEntry,
	VoiceEngineV2Runtime,
} from '@fluxer/voice_engine_v2/src/runtime/VoiceEngineV2Runtime';
import {FakeVoiceEngineV2Driver, VoiceEngineV2TestImplementation} from '@fluxer/voice_engine_v2/src/testing';
import {describe, expect, it} from 'vitest';

function makeInboundVideoFrameEvent(timestampUs: number): VoiceEngineV2Event {
	const frame: VoiceEngineV2InboundVideoFrame = {
		participantSid: 'participant-1',
		trackSid: `track-unknown-${timestampUs}`,
		width: 1280,
		height: 720,
		timestampUs,
	};
	return {type: 'inboundVideo.frameReceived', frame};
}

function makeRuntime(options?: {
	eventLogCap?: number;
	spillSink?: VoiceEngineV2EventLogSpillSink;
}): VoiceEngineV2Runtime {
	const driver = new FakeVoiceEngineV2Driver();
	return new VoiceEngineV2Runtime(new VoiceEngineV2TestImplementation(driver), {
		eventLogCap: options?.eventLogCap,
		eventLogSpillSink: options?.spillSink ?? createVoiceEngineV2MemoryEventLogSpillSink(),
		clock: {now: () => 0},
		verifyEventLogInvariantsOnDispatch: true,
	});
}

function replayBounded(
	initialSnapshot: VoiceEngineV2Snapshot,
	events: ReadonlyArray<VoiceEngineV2Event>,
	bound?: number,
): {snapshot: VoiceEngineV2Snapshot; ring: VoiceEngineV2EventLogRing} {
	const capacity = bound ?? VOICE_ENGINE_V2_EVENT_LOG_CAP;
	assert.ok(capacity >= 1, 'replayBounded requires positive capacity');
	const ring = new VoiceEngineV2EventLogRing(capacity);
	let snapshot = initialSnapshot;
	const limit = events.length;
	for (let index = 0; index < limit; index += 1) {
		const event = events[index];
		assert.ok(event !== undefined, 'replayBounded must receive defined events');
		const transition = transitionVoiceEngineV2(snapshot, event);
		snapshot = transition.snapshot;
		const sequence = ring.allocateSequence();
		const entry: VoiceEngineV2EventLogEntry = {
			sequence,
			atMs: 0,
			event,
			commands: transition.commands,
		};
		ring.push(entry);
	}
	assertEventLogRingInvariants(ring);
	return {snapshot, ring};
}

describe('VoiceEngineV2EventLogRing', () => {
	it('enforces the event log cap by dropping the oldest entries', () => {
		const capacity = 8;
		const runtime = makeRuntime({eventLogCap: capacity});
		const total = capacity * 3;
		for (let index = 0; index < total; index += 1) {
			runtime.dispatch(makeInboundVideoFrameEvent(index));
		}
		expect(runtime.eventLog).toHaveLength(capacity);
		expect(runtime.eventLogCap).toBe(capacity);
		expect(runtime.eventLog[0]?.sequence).toBe(total - capacity + 1);
		expect(runtime.eventLog[capacity - 1]?.sequence).toBe(total);
	});

	it('tracks dropped count and evicted sequence min exactly', () => {
		const capacity = 4;
		const runtime = makeRuntime({eventLogCap: capacity});
		expect(runtime.droppedEventsCount).toBe(0);
		expect(runtime.evictedSequenceMin).toBeNull();
		for (let index = 0; index < capacity; index += 1) {
			runtime.dispatch(makeInboundVideoFrameEvent(index));
		}
		expect(runtime.droppedEventsCount).toBe(0);
		expect(runtime.evictedSequenceMin).toBeNull();
		const overflow = 5;
		for (let index = 0; index < overflow; index += 1) {
			runtime.dispatch(makeInboundVideoFrameEvent(capacity + index));
		}
		expect(runtime.droppedEventsCount).toBe(overflow);
		expect(runtime.evictedSequenceMin).toBe(1);
	});

	it('preserves sequence monotonicity across eviction', () => {
		const capacity = 16;
		const runtime = makeRuntime({eventLogCap: capacity});
		const total = capacity * 5 + 3;
		for (let index = 0; index < total; index += 1) {
			runtime.dispatch(makeInboundVideoFrameEvent(index));
		}
		const log = runtime.eventLog;
		expect(log.length).toBe(capacity);
		const limit = log.length;
		for (let cursor = 1; cursor < limit; cursor += 1) {
			const previous = log[cursor - 1]?.sequence ?? 0;
			const current = log[cursor]?.sequence ?? 0;
			expect(current).toBe(previous + 1);
		}
		expect(log[0]?.sequence).toBe(runtime.droppedEventsCount + 1);
		assertEventLogInvariants(runtime);
	});

	it('produces an identical final snapshot when the bound is loose enough to keep every event', () => {
		const events: Array<VoiceEngineV2Event> = [];
		const eventCount = 64;
		for (let index = 0; index < eventCount; index += 1) {
			events.push(makeInboundVideoFrameEvent(index));
		}
		const initialSnapshot = createVoiceEngineV2InitialSnapshot(availableVoiceEngineV2Capabilities());
		const fullReplay = replayBounded(initialSnapshot, events, eventCount * 2);
		const boundedReplay = replayBounded(initialSnapshot, events, eventCount);
		const looseEnoughReplay = replayBounded(initialSnapshot, events, eventCount + 5);
		expect(fullReplay.snapshot).toEqual(boundedReplay.snapshot);
		expect(fullReplay.snapshot).toEqual(looseEnoughReplay.snapshot);
		expect(fullReplay.ring.droppedEventsCount).toBe(0);
		expect(looseEnoughReplay.ring.droppedEventsCount).toBe(0);
		expect(boundedReplay.ring.droppedEventsCount).toBe(0);
	});

	it('fires assertEventLogInvariants when the in-memory state is tampered with', () => {
		const runtime = makeRuntime({eventLogCap: 8});
		const eventCount = 6;
		for (let index = 0; index < eventCount; index += 1) {
			runtime.dispatch(makeInboundVideoFrameEvent(index));
		}
		assertEventLogInvariants(runtime);
		const internal = runtime as unknown as {
			eventLogRing: {droppedCount: number; evictedSequenceMinValue: number | null};
		};
		const originalDroppedCount = internal.eventLogRing.droppedCount;
		const originalEvictedSequenceMinValue = internal.eventLogRing.evictedSequenceMinValue;
		internal.eventLogRing.droppedCount = 3;
		expect(() => assertEventLogInvariants(runtime)).toThrow();
		internal.eventLogRing.droppedCount = originalDroppedCount;
		internal.eventLogRing.evictedSequenceMinValue = originalEvictedSequenceMinValue;
		assertEventLogInvariants(runtime);
	});

	it('runs the full-window invariant scan on dispatch only when the runtime opts in', () => {
		const verifyingRuntime = makeRuntime({eventLogCap: 8});
		verifyingRuntime.dispatch(makeInboundVideoFrameEvent(0));
		const verifyingInternal = verifyingRuntime as unknown as {
			eventLogRing: {droppedCount: number};
		};
		verifyingInternal.eventLogRing.droppedCount = 3;
		expect(() => verifyingRuntime.dispatch(makeInboundVideoFrameEvent(1))).toThrow();

		const driver = new FakeVoiceEngineV2Driver();
		const productionRuntime = new VoiceEngineV2Runtime(new VoiceEngineV2TestImplementation(driver), {
			eventLogCap: 8,
			eventLogSpillSink: createVoiceEngineV2MemoryEventLogSpillSink(),
			clock: {now: () => 0},
		});
		productionRuntime.dispatch(makeInboundVideoFrameEvent(0));
		const productionInternal = productionRuntime as unknown as {
			eventLogRing: {droppedCount: number};
		};
		productionInternal.eventLogRing.droppedCount = 3;
		expect(() => productionRuntime.dispatch(makeInboundVideoFrameEvent(1))).not.toThrow();
		expect(() => assertEventLogInvariants(productionRuntime)).toThrow();
	});

	it('invokes the spill sink with each evicted entry in eviction order', async () => {
		const capacity = 4;
		const evictions: Array<RuntimeEventLogEntry> = [];
		const sink: VoiceEngineV2EventLogSpillSink = {
			write(entry: VoiceEngineV2EventLogEntry): Promise<void> {
				evictions.push(entry);
				return Promise.resolve();
			},
		};
		const runtime = makeRuntime({eventLogCap: capacity, spillSink: sink});
		const total = capacity + 3;
		for (let index = 0; index < total; index += 1) {
			runtime.dispatch(makeInboundVideoFrameEvent(index));
		}
		await Promise.resolve();
		expect(evictions.length).toBe(total - capacity);
		const limit = evictions.length;
		for (let cursor = 0; cursor < limit; cursor += 1) {
			expect(evictions[cursor]?.sequence).toBe(cursor + 1);
		}
	});

	it('still satisfies the runtime cap default at the package constant', () => {
		expect(VOICE_ENGINE_V2_EVENT_LOG_CAP).toBeGreaterThanOrEqual(4096);
		const runtime = makeRuntime();
		expect(runtime.eventLogCap).toBe(VOICE_ENGINE_V2_EVENT_LOG_CAP);
	});
});
