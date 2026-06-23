// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import appVoiceSessionFixtureJson from '../../fixtures/event_logs/app_voice_session.json';
import {coalesceVoiceEngineV2OutboundStats} from '../policies/voiceStats';
import type {VoiceEngineV2OutboundStats} from '../protocol/types';
import {
	createVoiceEngineV2DeterministicPlatformPort,
	createVoiceEngineV2SeededRandomPort,
	createVoiceEngineV2SystemClockPort,
} from '../runtime';
import {replayVoiceEngineV2EventLogFixture, type VoiceEngineV2EventLogFixture} from './eventLogReplay';

const fixture = appVoiceSessionFixtureJson as unknown as VoiceEngineV2EventLogFixture;

describe('voice engine v2 determinism guarantees', () => {
	it('produces byte-identical snapshot output across fresh replay runs', () => {
		const first = replayVoiceEngineV2EventLogFixture(fixture);
		const second = replayVoiceEngineV2EventLogFixture(fixture);

		const firstBytes = JSON.stringify(first.finalSnapshot);
		const secondBytes = JSON.stringify(second.finalSnapshot);

		expect(secondBytes).toBe(firstBytes);
		expect(second.finalSnapshot).toEqual(first.finalSnapshot);
		expect(second.commandBatches).toEqual(first.commandBatches);
	});

	it('keeps Map-style grouping iteration order stable across runs', () => {
		const tracks: Array<VoiceEngineV2OutboundStats> = buildGroupedOutboundTracks();

		const firstRun = coalesceVoiceEngineV2OutboundStats(tracks);
		const secondRun = coalesceVoiceEngineV2OutboundStats(tracks);
		const firstKeys = firstRun.map((track) => `${track.kind}:${track.trackSid}`);
		const secondKeys = secondRun.map((track) => `${track.kind}:${track.trackSid}`);

		expect(secondKeys).toEqual(firstKeys);
		expect(firstKeys.length).toBe(2);
		expect(firstKeys[0]).toBe('audio:micA');
		expect(firstKeys[1]).toBe('video:camA');
	});

	it('produces stable seeded random sequences for replay-friendly entropy', () => {
		const firstSequence = drawSeededValues(7, 8);
		const secondSequence = drawSeededValues(7, 8);

		expect(secondSequence).toEqual(firstSequence);
		expect(new Set(firstSequence).size).toBeGreaterThan(1);
	});

	it('isolates the system clock port against retrograde wall clock motion', () => {
		let readingIndex = 0;
		const readings = [500, 200, 800, Number.NaN, -10];
		const clock = createVoiceEngineV2SystemClockPort({
			read(): number {
				const value = readings[readingIndex] ?? 0;
				readingIndex += 1;
				return value;
			},
		});

		const observed = [clock.now(), clock.now(), clock.now(), clock.now(), clock.now()];
		const monotonic = observed.every((value, index) => index === 0 || value >= (observed[index - 1] ?? 0));

		expect(monotonic).toBe(true);
		expect(observed[0]).toBe(500);
	});

	it('exposes injectable Clock and Random ports through the runtime options surface', () => {
		const platform = createVoiceEngineV2DeterministicPlatformPort(42);

		const first = platform.clock.now();
		const second = platform.clock.now();
		const randomValue = platform.random.next();

		expect(second).toBeGreaterThan(first);
		expect(randomValue).toBeGreaterThanOrEqual(0);
		expect(randomValue).toBeLessThan(1);
	});
});

function buildGroupedOutboundTracks(): Array<VoiceEngineV2OutboundStats> {
	return [
		{trackSid: 'micA', source: 'microphone', kind: 'audio', codec: 'opus', bitrateKbps: 32, packetsLost: 0},
		{trackSid: 'micA', source: 'microphone', kind: 'audio', codec: 'opus', bitrateKbps: 30, packetsLost: 1},
		{trackSid: 'camA', source: 'camera', kind: 'video', codec: 'h264', bitrateKbps: 1500, packetsLost: 2, fps: 30},
		{trackSid: 'camA', source: 'camera', kind: 'video', codec: 'h264', bitrateKbps: 1600, packetsLost: 3, fps: 31},
	];
}

function drawSeededValues(seed: number, count: number): Array<number> {
	const random = createVoiceEngineV2SeededRandomPort(seed);
	const values: Array<number> = [];
	for (let index = 0; index < count; index += 1) {
		values.push(random.next());
	}
	return values;
}
