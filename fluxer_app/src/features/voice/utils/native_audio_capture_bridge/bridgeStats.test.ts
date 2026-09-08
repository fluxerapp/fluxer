// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it, vi} from 'vitest';
import {
	endBridgeStats,
	getBridgeStats,
	getEndedBridgeCaptures,
	recordBridgeFrame,
	startBridgeStats,
} from './bridgeStats';

function recordFrames(captureId: string, count: number, peak: number, rms: number): void {
	for (let index = 0; index < count; index++) {
		recordBridgeFrame(captureId, {timestampUs: index * 10_000, durationUs: 10_000, peak, rms});
	}
}

function findEndedCapture(captureId: string) {
	return getEndedBridgeCaptures().find((capture) => capture.captureId === captureId);
}

describe('bridgeStats', () => {
	it('retains the counters of a capture that a mid-share restart supersedes', () => {
		startBridgeStats('generator', 'native-audio:failed', {prebufferTargetUs: 60_000, frameDurationUs: 10_000});
		recordFrames('native-audio:failed', 1124, 0.42, 0.11);

		startBridgeStats('generator', 'native-audio:replacement', {prebufferTargetUs: 60_000, frameDurationUs: 10_000});

		expect(getBridgeStats().captureId).toBe('native-audio:replacement');
		expect(getBridgeStats().framesReceived).toBe(0);
		const retained = findEndedCapture('native-audio:failed');
		expect(retained?.bridgeMode).toBe('generator');
		expect(retained?.framesReceived).toBe(1124);
		expect(retained?.nonSilentFrameCount).toBe(1124);
		expect(retained?.lastFramePeak).toBe(0.42);
		expect(retained?.endReason).toBe('superseded');
	});

	it('records the end reason of a capture the newer capture already superseded', () => {
		startBridgeStats('generator', 'native-audio:ends-late');
		recordFrames('native-audio:ends-late', 3, 0.2, 0.05);
		startBridgeStats('generator', 'native-audio:ends-late-replacement');

		endBridgeStats('native-audio:ends-late', 'ended', 'source-disappeared');

		const retained = findEndedCapture('native-audio:ends-late');
		expect(retained?.endReason).toBe('ended');
		expect(retained?.endDetail).toBe('source-disappeared');
		expect(retained?.endedAt).not.toBeNull();
		expect(getBridgeStats().captureId).toBe('native-audio:ends-late-replacement');
		expect(getBridgeStats().active).toBe(true);
	});

	it('still ends the active capture in place', () => {
		startBridgeStats('script-processor', 'native-audio:active-end');

		endBridgeStats('native-audio:active-end', 'cleanup', 'caller-stopped');

		const stats = getBridgeStats();
		expect(stats.active).toBe(false);
		expect(stats.endReason).toBe('cleanup');
		expect(stats.endDetail).toBe('caller-stopped');
		expect(stats.endedAt).not.toBeNull();
	});

	it('keeps the first end reason of the active capture', () => {
		startBridgeStats('generator', 'native-audio:double-end');
		endBridgeStats('native-audio:double-end', 'ended', 'source-disappeared');

		endBridgeStats('native-audio:double-end', 'cleanup', 'caller-stopped');

		expect(getBridgeStats().endReason).toBe('ended');
		expect(getBridgeStats().endDetail).toBe('source-disappeared');
	});

	it('flags a capture whose frames keep arriving while every one of them is silence', () => {
		const startMs = Date.UTC(2026, 8, 8, 12, 0, 0);
		vi.useFakeTimers();
		try {
			vi.setSystemTime(startMs);
			startBridgeStats('generator', 'native-audio:all-silent', {frameDurationUs: 10_000});
			for (let index = 0; index < 400; index++) {
				vi.setSystemTime(startMs + index * 100);
				recordBridgeFrame('native-audio:all-silent', {
					timestampUs: index * 10_000,
					durationUs: 10_000,
					peak: 0,
					rms: 0,
				});
			}

			const stats = getBridgeStats();
			expect(stats.framesReceived).toBe(400);
			expect(stats.nonSilentFrameCount).toBe(0);
			expect(stats.lastNonSilentFrameAt).toBeNull();
			expect(stats.silentFrameStreak).toBe(400);
			expect(stats.maxSilentRunMs).toBe(39_900);
			expect(stats.sustainedSilenceWarned).toBe(true);
		} finally {
			vi.useRealTimers();
		}
	});

	it('does not flag a capture that keeps producing audible frames', () => {
		const startMs = Date.UTC(2026, 8, 8, 13, 0, 0);
		vi.useFakeTimers();
		try {
			vi.setSystemTime(startMs);
			startBridgeStats('generator', 'native-audio:audible', {frameDurationUs: 10_000});
			for (let index = 0; index < 400; index++) {
				vi.setSystemTime(startMs + index * 100);
				const audible = index % 4 === 0;
				recordBridgeFrame('native-audio:audible', {
					timestampUs: index * 10_000,
					durationUs: 10_000,
					peak: audible ? 0.3 : 0,
					rms: audible ? 0.1 : 0,
				});
			}

			const stats = getBridgeStats();
			expect(stats.nonSilentFrameCount).toBe(100);
			expect(stats.lastNonSilentFrameAt).toBe(startMs + 396 * 100);
			expect(stats.silentFrameStreak).toBe(3);
			expect(stats.maxSilentRunMs).toBe(300);
			expect(stats.sustainedSilenceWarned).toBe(false);
		} finally {
			vi.useRealTimers();
		}
	});

	it('bounds the retained capture history', () => {
		for (let index = 0; index < 12; index++) {
			startBridgeStats('generator', `native-audio:bounded-${index}`);
		}

		expect(getEndedBridgeCaptures().length).toBeLessThanOrEqual(8);
		expect(findEndedCapture('native-audio:bounded-11')).toBeUndefined();
		expect(findEndedCapture('native-audio:bounded-10')).toBeDefined();
	});
});
