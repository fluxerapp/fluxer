// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it} from 'vitest';
import {candidateTtlSecondsFor} from './VoiceReconciliationWorker';

const INTERVAL_MS = 15000;
const GATEWAY_ONLY_GRACE_MS = 10000;

function ttlFor(observedSweepSpacingMs: number): number {
	return candidateTtlSecondsFor({
		intervalMs: INTERVAL_MS,
		observedSweepSpacingMs,
		graceMs: GATEWAY_ONLY_GRACE_MS,
	});
}

describe('candidateTtlSecondsFor', () => {
	it('outlives the gap between two consecutive observations of the same key', () => {
		for (const observedSweepSpacingMs of [0, 45_000, 136_000, 300_000, 596_000, 900_000]) {
			expect(ttlFor(observedSweepSpacingMs) * 1000).toBeGreaterThan(observedSweepSpacingMs);
		}
	});

	it('outlives a sweep gap far longer than the tick interval', () => {
		expect(ttlFor(596_000) * 1000).toBeGreaterThan(596_000);
	});

	it('grows with the observed sweep spacing rather than the tick interval', () => {
		expect(ttlFor(596_000)).toBeGreaterThan(ttlFor(136_000));
		expect(ttlFor(136_000)).toBeGreaterThan(ttlFor(0));
	});

	it('keeps a floor that survives a single long sweep before any spacing is observed', () => {
		expect(ttlFor(0)).toBeGreaterThanOrEqual(300);
	});

	it('stays bounded so a stale candidate cannot outlive its connection indefinitely', () => {
		expect(ttlFor(Number.MAX_SAFE_INTEGER)).toBeLessThanOrEqual(3600);
	});
});
