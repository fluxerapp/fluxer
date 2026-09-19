// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	PROOF_LAYERING_TRACES,
	PROOF_MEMORY_SEQUENCES,
	PROOF_MEMORY_SHARES,
	PROOF_PROFILES,
	PROOF_RUNTIME_KEY_COUNT,
	PROOF_VIEWER_SCRIPTS,
	PROOF_VIEWER_SHARE_TICKS,
	PROOF_VIEWER_TRACES,
	reportProofStage,
	runProofCaptureStage,
	runProofLayeringStage,
	runProofMemoryStage,
	runProofViewerStage,
} from '@app/features/voice/engine/ScreenShareDeliveryProofHarness';
import {describe, expect, it} from 'vitest';

describe('screen share delivery proof, viewers, memory and captures', () => {
	it('keeps every advertised viewer able to decode the share', () => {
		expect(PROOF_RUNTIME_KEY_COUNT * PROOF_PROFILES.length * PROOF_VIEWER_SCRIPTS.length).toBe(PROOF_VIEWER_TRACES);
		const outcome = runProofViewerStage();
		reportProofStage('stage F', outcome);
		expect(outcome.violations).toEqual([]);
		expect(outcome.traces).toBe(PROOF_VIEWER_TRACES);
		expect(outcome.ticks).toBeLessThanOrEqual(PROOF_VIEWER_TRACES * PROOF_VIEWER_SHARE_TICKS);
		expect(Object.keys(outcome.applied)).toEqual(
			expect.arrayContaining([
				'H grace boundary',
				'R14 decode',
				'R14 F4',
				'R14 F4 republish',
				'R14 F5',
				'R14 F5 republish',
				'R14 F8',
				'R14 F9',
				'R14 VP8 fallback with a silent viewer',
				'R16 viewers',
			]),
		);
	}, 30_000);

	it('turns extra layers on for a second viewer and off when they leave', () => {
		expect(PROOF_LAYERING_TRACES).toBe(20_400);
		const outcome = runProofLayeringStage();
		reportProofStage('stage L', outcome);
		expect(outcome.violations).toEqual([]);
		expect(outcome.traces).toBe(20_400);
		expect(Object.keys(outcome.applied)).toEqual(
			expect.arrayContaining([
				'R17 a flapping count does not cross',
				'R17 a second viewer turns it on',
				'R17 one viewer stays single',
				'R17 the last viewer leaving turns it off',
			]),
		);
	}, 60_000);

	it('converges across shares through persisted memory', () => {
		expect(PROOF_RUNTIME_KEY_COUNT * PROOF_PROFILES.length * 2).toBe(PROOF_MEMORY_SEQUENCES);
		const outcome = runProofMemoryStage();
		reportProofStage('stage G', outcome);
		expect(outcome.violations).toEqual([]);
		expect(outcome.traces).toBe(PROOF_MEMORY_SEQUENCES * PROOF_MEMORY_SHARES);
		expect(outcome.applied['R15 memory cap']).toBe(PROOF_MEMORY_SEQUENCES);
		expect(Object.keys(outcome.applied)).toEqual(
			expect.arrayContaining([
				'K full quality reset',
				'K memory expiry',
				'R15 best recorded ratio',
				'R15 converges on the second codec',
				'R15 first codec holds',
				'R15 header change',
				'R15 memory full',
				'R15 probe block',
				'R15 starts at a settled level',
			]),
		);
	}, 30_000);

	it('classifies every captured share the way the capture was read', () => {
		const outcome = runProofCaptureStage();
		reportProofStage('stage U0', outcome);
		expect(outcome.violations).toEqual([]);
		expect(outcome.traces).toBe(7);
		expect(Object.keys(outcome.applied)).toEqual([
			'D sample deltas',
			'D encode time and header bytes',
			'D paused encoding',
			'D counters reset',
			'CL b2 processor load with the uplink capped at 1 Mbps',
			'CL c1 uplink capped at 500 kbps at 1080p30',
			'U0 c1 software H.264 at 4K15',
			'U0 c1 pinned to software H.264 at 4K15',
			'U0 c4 uplink capped at 2 Mbps',
			'U0 c6 uplink capped at 600 kbps',
			'U0 c5 viewerless share',
		]);
	});
});
