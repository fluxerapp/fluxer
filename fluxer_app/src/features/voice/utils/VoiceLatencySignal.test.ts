// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	getLatencySignalBaseline,
	getLatencySignalState,
	type LatencySignalSample,
} from '@app/features/voice/utils/VoiceLatencySignal';
import {describe, expect, it} from 'vitest';

function samples(...latencies: Array<number>): Array<LatencySignalSample> {
	return latencies.map((latency) => ({latency}));
}

describe('VoiceLatencySignal', () => {
	it('keeps a stable high cross-region latency green', () => {
		expect(getLatencySignalState(170, samples(168, 170, 171, 169, 170))).toMatchObject({
			kind: 'value',
			baselineLatency: 170,
			excessLatency: 0,
			filledCount: 4,
			tone: 'green',
		});
	});
	it('uses the recent median as the baseline', () => {
		expect(getLatencySignalBaseline(samples(168, 171, 500, 170, 169))).toBe(170);
	});
	it('degrades from the baseline only when latency rises meaningfully', () => {
		const baseline = samples(48, 50, 51, 49, 50, 50);
		expect(getLatencySignalState(115, baseline)).toMatchObject({filledCount: 4, tone: 'green'});
		expect(getLatencySignalState(140, baseline)).toMatchObject({filledCount: 3, tone: 'yellow'});
		expect(getLatencySignalState(250, baseline)).toMatchObject({filledCount: 2, tone: 'orange'});
		expect(getLatencySignalState(390, baseline)).toMatchObject({filledCount: 1, tone: 'red'});
	});
	it('does not punish the first measured sample before a baseline exists', () => {
		expect(getLatencySignalState(270)).toMatchObject({
			kind: 'value',
			baselineLatency: 270,
			excessLatency: 0,
			filledCount: 4,
			tone: 'green',
		});
	});
	it('returns loading while latency is unavailable', () => {
		expect(getLatencySignalState(null, samples(50, 51, 49))).toEqual({kind: 'loading'});
	});
});
