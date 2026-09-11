// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT,
	type VoiceNoiseSuppressionAssignmentResponse,
} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';
import {
	type ExperimentAssignmentsResponse,
	INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE,
} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {runInAction} from 'mobx';
import {afterEach, describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		debug = vi.fn();
		info = vi.fn();
		warn = vi.fn();
		error = vi.fn();
	},
}));

vi.mock('@app/features/platform/transport/RestTransport', () => ({
	http: {get: vi.fn(), post: vi.fn()},
}));

const {ExperimentAssignments} = await import('@app/features/experiment/state/ExperimentAssignments');
const {VoiceNoiseSuppressionRollout} = await import('@app/features/voice/state/VoiceNoiseSuppressionRollout');

const GUILD_ID = '1485064866382176262';
const OTHER_GUILD_ID = '1485064866382176263';

const CANARY_ASSIGNMENT: VoiceNoiseSuppressionAssignmentResponse = {
	enabled: true,
	config_version: 7,
	user_targeted: true,
	backend: 'rnnoise',
	source: 'canary',
	guild_overrides: [{guild_id: GUILD_ID, backend: 'speex'}],
	enabled_backends: ['none', 'speex', 'rnnoise', 'gtcrn'],
	allow_user_override: true,
	stereo_enabled: false,
	suppression_strength: 80,
};

function publish(assignment: VoiceNoiseSuppressionAssignmentResponse): void {
	const response: ExperimentAssignmentsResponse = {
		poll_interval_seconds: 300,
		poll_jitter_percent: 15,
		assignments: {voice_noise_suppression: assignment},
	};
	runInAction(() => {
		ExperimentAssignments.response = response;
	});
}

afterEach(() => {
	runInAction(() => {
		ExperimentAssignments.response = INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE;
	});
});

describe('VoiceNoiseSuppressionRollout assignment', () => {
	it('reads the inert assignment out of the inert envelope', () => {
		expect(VoiceNoiseSuppressionRollout.assignment).toBe(INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT);
	});

	it('reads the assignment out of a populated envelope', () => {
		publish(CANARY_ASSIGNMENT);
		expect(VoiceNoiseSuppressionRollout.assignment).toEqual(CANARY_ASSIGNMENT);
	});

	it('follows the envelope back to inert when the store is reset', () => {
		publish(CANARY_ASSIGNMENT);
		ExperimentAssignments.reset();
		expect(VoiceNoiseSuppressionRollout.assignment).toBe(INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT);
	});
});

describe('VoiceNoiseSuppressionRollout resolveForCall', () => {
	it('returns null while the inert envelope is in place', () => {
		expect(VoiceNoiseSuppressionRollout.resolveForCall(null, null)).toBeNull();
		expect(VoiceNoiseSuppressionRollout.resolveForCall(GUILD_ID, 'gtcrn')).toBeNull();
	});

	it('delegates to the shared resolver for the canary, guild and override cases', () => {
		publish(CANARY_ASSIGNMENT);
		expect(VoiceNoiseSuppressionRollout.resolveForCall(null, null)).toEqual({
			backend: 'rnnoise',
			source: 'canary',
			stereoEnabled: false,
			suppressionStrength: 80,
			configVersion: 7,
		});
		expect(VoiceNoiseSuppressionRollout.resolveForCall(OTHER_GUILD_ID, null)?.source).toBe('canary');
		expect(VoiceNoiseSuppressionRollout.resolveForCall(GUILD_ID, null)).toEqual({
			backend: 'speex',
			source: 'guild_rule',
			stereoEnabled: false,
			suppressionStrength: 80,
			configVersion: 7,
		});
		expect(VoiceNoiseSuppressionRollout.resolveForCall(GUILD_ID, 'gtcrn')).toEqual({
			backend: 'gtcrn',
			source: 'user_override',
			stereoEnabled: false,
			suppressionStrength: 80,
			configVersion: 7,
		});
		expect(VoiceNoiseSuppressionRollout.resolveForCall(null, 'deep_filter')?.source).toBe('canary');
	});

	it('lets a user rule win over a guild override', () => {
		publish({...CANARY_ASSIGNMENT, backend: 'gtcrn', source: 'user_rule', allow_user_override: false});
		expect(VoiceNoiseSuppressionRollout.resolveForCall(GUILD_ID, 'speex')).toEqual({
			backend: 'gtcrn',
			source: 'user_rule',
			stereoEnabled: false,
			suppressionStrength: 80,
			configVersion: 7,
		});
	});

	it('returns null for a user the rollout does not target', () => {
		publish({...CANARY_ASSIGNMENT, user_targeted: false, backend: null, source: null, guild_overrides: []});
		expect(VoiceNoiseSuppressionRollout.resolveForCall(null, 'rnnoise')).toBeNull();
	});
});
