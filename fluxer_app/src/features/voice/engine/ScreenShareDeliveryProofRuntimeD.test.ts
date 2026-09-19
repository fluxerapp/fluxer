// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	PROOF_CODEC_POLICIES,
	PROOF_CONDITIONS,
	PROOF_CONTROL_TRACES,
	PROOF_PROFILES,
	PROOF_RUNTIME_KEY_COUNT,
	PROOF_RUNTIME_RULES,
	PROOF_RUNTIME_TRACES,
	PROOF_RUNTIME_TRACES_PER_FILE,
	PROOF_SHARE_TICKS,
	reportProofStage,
	runProofControlStage,
	runProofRuntimeStage,
} from '@app/features/voice/engine/ScreenShareDeliveryProofHarness';
import {describe, expect, it} from 'vitest';

describe('screen share delivery proof, runtime for P10 to P12 and the experiment arms', () => {
	it('runs every key, condition and codec policy', () => {
		expect(
			PROOF_RUNTIME_KEY_COUNT * PROOF_PROFILES.length * PROOF_CONDITIONS.length * PROOF_CODEC_POLICIES.length,
		).toBe(PROOF_RUNTIME_TRACES);
		const outcome = runProofRuntimeStage(['P10', 'P11', 'P12']);
		reportProofStage('stage E P10 to P12', outcome);
		expect(outcome.violations).toEqual([]);
		expect(outcome.traces).toBe(PROOF_RUNTIME_TRACES_PER_FILE);
		expect(outcome.ticks).toBeLessThanOrEqual(PROOF_RUNTIME_TRACES_PER_FILE * PROOF_SHARE_TICKS);
		expect(Object.keys(outcome.applied)).toEqual(expect.arrayContaining([...PROOF_RUNTIME_RULES]));
	}, 30_000);

	it('holds the control arm at the published target and keeps its stall recovery', () => {
		expect(PROOF_CONTROL_TRACES).toBe(10_880);
		const outcome = runProofControlStage();
		reportProofStage('stage X control arm', outcome);
		expect(outcome.violations).toEqual([]);
		expect(outcome.traces).toBe(10_880);
	}, 60_000);
});
