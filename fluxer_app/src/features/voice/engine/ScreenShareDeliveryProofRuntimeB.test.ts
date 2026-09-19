// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	PROOF_CODEC_POLICIES,
	PROOF_CONDITIONS,
	PROOF_PROFILES,
	PROOF_RUNTIME_KEY_COUNT,
	PROOF_RUNTIME_RULES,
	PROOF_RUNTIME_TRACES,
	PROOF_RUNTIME_TRACES_PER_FILE,
	PROOF_SHARE_TICKS,
	reportProofStage,
	runProofRuntimeStage,
} from '@app/features/voice/engine/ScreenShareDeliveryProofHarness';
import {describe, expect, it} from 'vitest';

describe('screen share delivery proof, runtime for P4 to P6', () => {
	it('runs every key, condition and codec policy', () => {
		expect(
			PROOF_RUNTIME_KEY_COUNT * PROOF_PROFILES.length * PROOF_CONDITIONS.length * PROOF_CODEC_POLICIES.length,
		).toBe(PROOF_RUNTIME_TRACES);
		const outcome = runProofRuntimeStage(['P4', 'P5', 'P6']);
		reportProofStage('stage E P4 to P6', outcome);
		expect(outcome.violations).toEqual([]);
		expect(outcome.traces).toBe(PROOF_RUNTIME_TRACES_PER_FILE);
		expect(outcome.ticks).toBeLessThanOrEqual(PROOF_RUNTIME_TRACES_PER_FILE * PROOF_SHARE_TICKS);
		expect(Object.keys(outcome.applied)).toEqual(
			expect.arrayContaining([
				...PROOF_RUNTIME_RULES,
				'E3 ramp cap exit',
				'E4 ratio-unmeasured',
				'R1 stall without source stats',
				'K write rule without a target',
			]),
		);
	}, 30_000);
});
