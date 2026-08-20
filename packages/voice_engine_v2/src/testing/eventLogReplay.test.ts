// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import appVoiceSessionFixtureJson from '../../fixtures/event_logs/app_voice_session.json';
import type {VoiceEngineV2Event} from '../protocol/events';
import {createVoiceEngineV2MemoryEventLogSpillSink} from '../runtime/eventLogRing';
import {coalesceVoiceEngineV2EventSequence} from '../runtime/frameCoalescing';
import {VoiceEngineV2Runtime} from '../runtime/VoiceEngineV2Runtime';
import {
	replayVoiceEngineV2EventLogFixture,
	VOICE_ENGINE_V2_EVENT_LOG_FIXTURE_VERSION,
	type VoiceEngineV2EventLogFixture,
} from './eventLogReplay';
import {FakeVoiceEngineV2Driver} from './FakeVoiceEngineV2Driver';
import {VoiceEngineV2TestImplementation} from './VoiceEngineV2TestImplementation';

const appVoiceSessionFixture = appVoiceSessionFixtureJson as unknown as VoiceEngineV2EventLogFixture;

function makeCoalescingRuntime(): VoiceEngineV2Runtime {
	return new VoiceEngineV2Runtime(new VoiceEngineV2TestImplementation(new FakeVoiceEngineV2Driver()), {
		eventLogSpillSink: createVoiceEngineV2MemoryEventLogSpillSink(),
		clock: {now: () => 0},
		verifyEventLogInvariantsOnDispatch: true,
	});
}

function inboundVideoBurstEvents(): Array<VoiceEngineV2Event> {
	const events: Array<VoiceEngineV2Event> = [
		{
			type: 'inboundVideo.trackSubscribed',
			track: {participantSid: 'PA_alice', participantIdentity: 'alice', trackSid: 'TR_screen', source: 'screen'},
		},
	];
	const burst = 12;
	for (let index = 1; index <= burst; index += 1) {
		events.push({
			type: 'inboundVideo.frameReceived',
			frame: {
				participantSid: 'PA_alice',
				participantIdentity: 'alice',
				trackSid: 'TR_screen',
				width: 1280,
				height: 720,
				timestampUs: index * 1000,
				byteLength: 1_382_400,
			},
		});
	}
	events.push({
		type: 'inboundVideo.trackSubscribed',
		track: {participantSid: 'PA_bob', participantIdentity: 'bob', trackSid: 'TR_camera', source: 'camera'},
	});
	events.push({
		type: 'inboundVideo.frameReceived',
		frame: {
			participantSid: 'PA_bob',
			participantIdentity: 'bob',
			trackSid: 'TR_camera',
			width: 640,
			height: 360,
			timestampUs: 99_000,
			byteLength: 345_600,
		},
	});
	return events;
}

describe('voice engine v2 event-log fixture replay', () => {
	it('replays command batches and final immutable snapshot goldens', () => {
		const replay = replayVoiceEngineV2EventLogFixture(appVoiceSessionFixture);

		expect(replay.fixtureName).toBe('app_voice_session');
		expect(replay.commandBatches).toEqual(appVoiceSessionFixture.steps.map((step) => step.commands));
		expect(replay.finalSnapshot).toEqual(appVoiceSessionFixture.expected.finalSnapshot);
		expect(replay.finalModel).toEqual(appVoiceSessionFixture.expected.finalModel);
	});

	it('is deterministic across repeated replays', () => {
		const first = replayVoiceEngineV2EventLogFixture(appVoiceSessionFixture);
		const second = replayVoiceEngineV2EventLogFixture(appVoiceSessionFixture);

		expect(second.commandBatches).toEqual(first.commandBatches);
		expect(second.finalSnapshot).toEqual(first.finalSnapshot);
		expect(second.finalModel).toEqual(first.finalModel);
	});

	it('preserves previous snapshots when later events transition state', () => {
		const replay = replayVoiceEngineV2EventLogFixture(appVoiceSessionFixture);
		const connectStep = replay.steps.find((step) => step.name === 'request voice connection');
		const connectedStep = replay.steps.find(
			(step) => step.name === 'voice connection succeeds and queued media publishes',
		);

		expect(connectStep?.previousSnapshot.connection.status).toBe('idle');
		expect(connectStep?.snapshot.connection.status).toBe('connecting');
		expect(connectStep?.previousSnapshot.microphone.status).toBe('idle');
		expect(connectStep?.snapshot.microphone.status).toBe('idle');

		expect(connectedStep?.previousSnapshot.connection.status).toBe('connecting');
		expect(connectedStep?.previousSnapshot.microphone.status).toBe('idle');
		expect(connectedStep?.snapshot.connection.status).toBe('connected');
		expect(connectedStep?.snapshot.microphone.status).toBe('publishing');
	});

	it('replays a runtime-coalesced frame log to the identical snapshot', () => {
		const rawEvents = inboundVideoBurstEvents();
		const runtime = makeCoalescingRuntime();
		for (const event of rawEvents) {
			runtime.dispatch(event);
		}

		expect(runtime.coalescedEventsCount).toBe(11);
		const retainedEvents = runtime.eventLog.map((entry) => entry.event);
		expect(retainedEvents).toEqual(coalesceVoiceEngineV2EventSequence(rawEvents));

		const fixture: VoiceEngineV2EventLogFixture = {
			version: VOICE_ENGINE_V2_EVENT_LOG_FIXTURE_VERSION,
			name: 'coalesced_inbound_video_burst',
			steps: runtime.eventLog.map((entry) => ({event: entry.event, commands: entry.commands})),
			expected: {finalSnapshot: runtime.snapshot},
		};
		const replay = replayVoiceEngineV2EventLogFixture(fixture);

		expect(replay.finalSnapshot).toEqual(runtime.snapshot);
		expect(replay.finalSnapshot.inboundVideo.tracks.TR_screen?.lastFrameTimestampUs).toBe(12_000);
		expect(replay.finalSnapshot.inboundVideo.tracks.TR_screen?.frameCount).toBe(1);
	});

	it('replays the same coalesced log deterministically across runtimes', () => {
		const rawEvents = inboundVideoBurstEvents();
		const first = makeCoalescingRuntime();
		const second = makeCoalescingRuntime();
		for (const event of rawEvents) {
			first.dispatch(event);
			second.dispatch(event);
		}

		expect(second.eventLog.map((entry) => entry.event)).toEqual(first.eventLog.map((entry) => entry.event));
		expect(second.snapshot).toEqual(first.snapshot);
	});
});
