// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceEngineV2Event} from '@fluxer/voice_engine_v2/src/protocol/events';
import {createVoiceEngineV2MemoryEventLogSpillSink} from '@fluxer/voice_engine_v2/src/runtime/eventLogRing';
import {
	coalesceVoiceEngineV2EventSequence,
	VOICE_ENGINE_V2_COALESCED_TRACKS_CAP,
} from '@fluxer/voice_engine_v2/src/runtime/frameCoalescing';
import {VoiceEngineV2Runtime} from '@fluxer/voice_engine_v2/src/runtime/VoiceEngineV2Runtime';
import {FakeVoiceEngineV2Driver, VoiceEngineV2TestImplementation} from '@fluxer/voice_engine_v2/src/testing';
import {describe, expect, it} from 'vitest';

function makeRuntime(options?: {eventLogCap?: number}): VoiceEngineV2Runtime {
	const driver = new FakeVoiceEngineV2Driver();
	return new VoiceEngineV2Runtime(new VoiceEngineV2TestImplementation(driver), {
		eventLogCap: options?.eventLogCap,
		eventLogSpillSink: createVoiceEngineV2MemoryEventLogSpillSink(),
		clock: {now: () => 0},
		verifyEventLogInvariantsOnDispatch: true,
	});
}

function subscribeEvent(trackSid: string): VoiceEngineV2Event {
	return {
		type: 'inboundVideo.trackSubscribed',
		track: {
			participantSid: 'PA_alice',
			participantIdentity: 'alice',
			trackSid,
			source: 'camera',
		},
	};
}

function frameEvent(trackSid: string, timestampUs: number): VoiceEngineV2Event {
	return {
		type: 'inboundVideo.frameReceived',
		frame: {
			participantSid: 'PA_alice',
			participantIdentity: 'alice',
			trackSid,
			width: 1280,
			height: 720,
			timestampUs,
			byteLength: 345_600,
		},
	};
}

describe('voice engine v2 frame-received coalescing', () => {
	it('collapses consecutive same-track frames into the retained tail entry, latest wins', () => {
		const runtime = makeRuntime();
		runtime.dispatch(subscribeEvent('TR_1'));
		runtime.dispatch(frameEvent('TR_1', 1000));
		runtime.dispatch(frameEvent('TR_1', 2000));
		runtime.dispatch(frameEvent('TR_1', 3000));

		expect(runtime.eventLog).toHaveLength(2);
		const tail = runtime.eventLog[1];
		if (tail === undefined) throw new Error('expected a retained tail entry');
		expect(tail.event.type).toBe('inboundVideo.frameReceived');
		if (tail.event.type !== 'inboundVideo.frameReceived') throw new Error('expected a frame event');
		expect(tail.event.frame.timestampUs).toBe(3000);
		expect(runtime.coalescedEventsCount).toBe(2);

		const track = runtime.snapshot.inboundVideo.tracks.TR_1;
		expect(track?.lastFrameTimestampUs).toBe(3000);
		expect(track?.frameCount).toBe(1);
	});

	it('keeps the event log bounded to one tail entry under a sustained same-track burst', () => {
		const runtime = makeRuntime({eventLogCap: 8});
		runtime.dispatch(subscribeEvent('TR_1'));
		const burst = 100;
		for (let index = 1; index <= burst; index += 1) {
			runtime.dispatch(frameEvent('TR_1', index * 1000));
		}

		expect(runtime.eventLog).toHaveLength(2);
		expect(runtime.droppedEventsCount).toBe(0);
		expect(runtime.coalescedEventsCount).toBe(burst - 1);
		expect(runtime.snapshot.inboundVideo.tracks.TR_1?.lastFrameTimestampUs).toBe(burst * 1000);
	});

	it('does not coalesce frames across interleaved tracks', () => {
		const runtime = makeRuntime();
		runtime.dispatch(subscribeEvent('TR_a'));
		runtime.dispatch(subscribeEvent('TR_b'));
		runtime.dispatch(frameEvent('TR_a', 1000));
		runtime.dispatch(frameEvent('TR_b', 1100));
		runtime.dispatch(frameEvent('TR_a', 2000));
		runtime.dispatch(frameEvent('TR_b', 2100));

		expect(runtime.eventLog).toHaveLength(6);
		expect(runtime.coalescedEventsCount).toBe(0);
		expect(runtime.snapshot.inboundVideo.tracks.TR_a?.frameCount).toBe(2);
		expect(runtime.snapshot.inboundVideo.tracks.TR_b?.frameCount).toBe(2);
	});

	it('does not coalesce across an intervening non-frame event', () => {
		const runtime = makeRuntime();
		runtime.dispatch(subscribeEvent('TR_1'));
		runtime.dispatch(frameEvent('TR_1', 1000));
		runtime.dispatch(subscribeEvent('TR_2'));
		runtime.dispatch(frameEvent('TR_1', 2000));

		expect(runtime.eventLog).toHaveLength(4);
		expect(runtime.coalescedEventsCount).toBe(0);
		expect(runtime.snapshot.inboundVideo.tracks.TR_1?.frameCount).toBe(2);
	});

	it('coalesces frames for unknown tracks so the drop counter reflects retained events only', () => {
		const runtime = makeRuntime();
		runtime.dispatch(frameEvent('TR_unknown', 1000));
		runtime.dispatch(frameEvent('TR_unknown', 2000));
		runtime.dispatch(frameEvent('TR_unknown', 3000));

		expect(runtime.eventLog).toHaveLength(1);
		expect(runtime.coalescedEventsCount).toBe(2);
		expect(runtime.snapshot.inboundVideo.droppedFrameCount).toBe(1);
	});

	it('matches the pure event-sequence coalescing function for a mixed sequence', () => {
		const rawEvents: Array<VoiceEngineV2Event> = [
			subscribeEvent('TR_1'),
			frameEvent('TR_1', 1000),
			frameEvent('TR_1', 2000),
			subscribeEvent('TR_2'),
			frameEvent('TR_2', 2100),
			frameEvent('TR_2', 2200),
			frameEvent('TR_1', 3000),
			frameEvent('TR_1', 4000),
		];
		const runtime = makeRuntime();
		for (const event of rawEvents) {
			runtime.dispatch(event);
		}

		const retainedEvents = runtime.eventLog.map((entry) => entry.event);
		expect(retainedEvents).toEqual(coalesceVoiceEngineV2EventSequence(rawEvents));
		expect(runtime.coalescedEventsCount).toBe(rawEvents.length - retainedEvents.length);
	});

	it('bounds per-track bookkeeping at the named cap', () => {
		const runtime = makeRuntime();
		const total = VOICE_ENGINE_V2_COALESCED_TRACKS_CAP + 8;
		for (let index = 0; index < total; index += 1) {
			runtime.dispatch(frameEvent(`TR_${index}`, (index + 1) * 1000));
		}

		expect(runtime.coalesceTrackedTracksCount).toBe(VOICE_ENGINE_V2_COALESCED_TRACKS_CAP);
		expect(runtime.eventLog.length).toBeLessThanOrEqual(runtime.eventLogCap);
	});

	it('is deterministic across two runtimes fed the same event sequence', () => {
		const rawEvents: Array<VoiceEngineV2Event> = [
			subscribeEvent('TR_1'),
			frameEvent('TR_1', 1000),
			frameEvent('TR_1', 2000),
			frameEvent('TR_1', 3000),
			subscribeEvent('TR_2'),
			frameEvent('TR_2', 3100),
		];
		const first = makeRuntime();
		const second = makeRuntime();
		for (const event of rawEvents) {
			first.dispatch(event);
			second.dispatch(event);
		}

		expect(second.eventLog.map((entry) => entry.event)).toEqual(first.eventLog.map((entry) => entry.event));
		expect(second.snapshot).toEqual(first.snapshot);
		expect(second.coalescedEventsCount).toBe(first.coalescedEventsCount);
	});
});
