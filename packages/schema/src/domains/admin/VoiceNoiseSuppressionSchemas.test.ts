// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
	INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT,
	resolveVoiceNoiseSuppressionAssignment,
	resolveVoiceNoiseSuppressionForCall,
	VOICE_NOISE_SUPPRESSION_BACKENDS,
	type VoiceNoiseSuppressionAssignmentResponse,
	type VoiceNoiseSuppressionBackend,
	type VoiceNoiseSuppressionConfig,
} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';
import {experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {describe, expect, test} from 'vitest';

const TARGETED_USER_ID = '1000000000000000001';
const OTHER_USER_ID = '1000000000000000002';
const GUILD_ID = '2000000000000000001';
const OTHER_GUILD_ID = '2000000000000000002';

function createConfig(overrides: Partial<VoiceNoiseSuppressionConfig> = {}): VoiceNoiseSuppressionConfig {
	return {
		...DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
		enabled_backends: [...DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG.enabled_backends],
		included_user_ids: [],
		excluded_user_ids: [],
		guild_overrides: [],
		...overrides,
	};
}

function createAssignment(
	overrides: Partial<VoiceNoiseSuppressionAssignmentResponse> = {},
): VoiceNoiseSuppressionAssignmentResponse {
	return {
		enabled: true,
		config_version: 7,
		user_targeted: true,
		backend: 'rnnoise',
		source: 'canary',
		guild_overrides: [],
		enabled_backends: [...VOICE_NOISE_SUPPRESSION_BACKENDS],
		allow_user_override: false,
		stereo_enabled: false,
		suppression_strength: 80,
		...overrides,
	};
}

function syntheticUserIds(count: number): Array<string> {
	const ids: Array<string> = [];
	for (let index = 0; index < count; index++) {
		ids.push((1400000000000000000n + BigInt(index)).toString());
	}
	return ids;
}

function targetedUserIds(config: VoiceNoiseSuppressionConfig, userIds: ReadonlyArray<string>): Set<string> {
	const targeted = new Set<string>();
	for (const userId of userIds) {
		if (resolveVoiceNoiseSuppressionAssignment(config, userId).user_targeted) {
			targeted.add(userId);
		}
	}
	return targeted;
}

describe('experimentBucket', () => {
	test('stays inside the basis point range for every synthetic id', () => {
		for (const userId of syntheticUserIds(500)) {
			const bucket = experimentBucket(userId, 'voice-ns-v1');
			expect(Number.isInteger(bucket)).toBe(true);
			expect(bucket).toBeGreaterThanOrEqual(0);
			expect(bucket).toBeLessThan(10000);
		}
	});

	test('is stable for the same user id and salt', () => {
		expect(experimentBucket(TARGETED_USER_ID, 'voice-ns-v1')).toBe(experimentBucket(TARGETED_USER_ID, 'voice-ns-v1'));
	});

	test('changes with the salt for at least most user ids', () => {
		const userIds = syntheticUserIds(200);
		const changed = userIds.filter(
			(userId) => experimentBucket(userId, 'voice-ns-v1') !== experimentBucket(userId, 'voice-ns-v2'),
		);
		expect(changed.length).toBeGreaterThan(userIds.length - 5);
	});
});

describe('resolveVoiceNoiseSuppressionAssignment', () => {
	test('returns the inert assignment when the master switch is off', () => {
		const config = createConfig({
			enabled: false,
			config_version: 4,
			rollout_basis_points: 10000,
			included_user_ids: [TARGETED_USER_ID],
			guild_overrides: [{guild_id: GUILD_ID, backend: 'rnnoise'}],
		});
		expect(resolveVoiceNoiseSuppressionAssignment(config, TARGETED_USER_ID)).toEqual({
			...INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT,
			config_version: 4,
		});
	});

	test('returns the inert assignment for the default config', () => {
		expect(resolveVoiceNoiseSuppressionAssignment(createConfig(), TARGETED_USER_ID)).toEqual(
			INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT,
		);
	});

	test('denylist beats allowlist', () => {
		const config = createConfig({
			enabled: true,
			included_user_ids: [TARGETED_USER_ID],
			excluded_user_ids: [TARGETED_USER_ID],
		});
		const assignment = resolveVoiceNoiseSuppressionAssignment(config, TARGETED_USER_ID);
		expect(assignment.user_targeted).toBe(false);
		expect(assignment.backend).toBeNull();
		expect(assignment.source).toBeNull();
	});

	test('denylist beats canary', () => {
		const config = createConfig({
			enabled: true,
			rollout_basis_points: 10000,
			excluded_user_ids: [TARGETED_USER_ID],
		});
		const assignment = resolveVoiceNoiseSuppressionAssignment(config, TARGETED_USER_ID);
		expect(assignment.user_targeted).toBe(false);
		expect(assignment.source).toBeNull();
		expect(resolveVoiceNoiseSuppressionAssignment(config, OTHER_USER_ID).user_targeted).toBe(true);
	});

	test('denylist strips guild overrides and the client-side knobs', () => {
		const config = createConfig({
			enabled: true,
			allow_user_override: true,
			stereo_enabled: true,
			rollout_basis_points: 10000,
			excluded_user_ids: [TARGETED_USER_ID],
			guild_overrides: [{guild_id: GUILD_ID, backend: 'rnnoise'}],
		});
		const assignment = resolveVoiceNoiseSuppressionAssignment(config, TARGETED_USER_ID);
		expect(assignment.guild_overrides).toEqual([]);
		expect(assignment.enabled_backends).toEqual([]);
		expect(assignment.allow_user_override).toBe(false);
		expect(assignment.stereo_enabled).toBe(false);
	});

	test('allowlist gives source user_rule outside the canary', () => {
		const config = createConfig({
			enabled: true,
			default_backend: 'gtcrn',
			rollout_basis_points: 0,
			included_user_ids: [TARGETED_USER_ID],
		});
		const assignment = resolveVoiceNoiseSuppressionAssignment(config, TARGETED_USER_ID);
		expect(assignment.user_targeted).toBe(true);
		expect(assignment.backend).toBe('gtcrn');
		expect(assignment.source).toBe('user_rule');
	});

	test.each([
		{basisPoints: 0, targeted: false},
		{basisPoints: 10000, targeted: true},
	])('canary at $basisPoints basis points targets $targeted', ({basisPoints, targeted}) => {
		const config = createConfig({enabled: true, rollout_basis_points: basisPoints});
		for (const userId of syntheticUserIds(200)) {
			const assignment = resolveVoiceNoiseSuppressionAssignment(config, userId);
			expect(assignment.user_targeted).toBe(targeted);
			expect(assignment.source).toBe(targeted ? 'canary' : null);
		}
	});

	test('raising the rollout basis points only ever adds users', () => {
		const userIds = syntheticUserIds(2000);
		const atOneThousand = targetedUserIds(createConfig({enabled: true, rollout_basis_points: 1000}), userIds);
		const atTwoThousand = targetedUserIds(createConfig({enabled: true, rollout_basis_points: 2000}), userIds);
		expect(atOneThousand.size).toBeGreaterThan(0);
		for (const userId of atOneThousand) {
			expect(atTwoThousand.has(userId)).toBe(true);
		}
		expect(atTwoThousand.size).toBeGreaterThan(atOneThousand.size);
	});

	test('the canary set follows the salt', () => {
		const userIds = syntheticUserIds(2000);
		const first = targetedUserIds(
			createConfig({enabled: true, rollout_basis_points: 5000, rollout_salt: 'voice-ns-v1'}),
			userIds,
		);
		const second = targetedUserIds(
			createConfig({enabled: true, rollout_basis_points: 5000, rollout_salt: 'voice-ns-v2'}),
			userIds,
		);
		expect(first).not.toEqual(second);
	});

	test('a default backend missing from enabled_backends yields no backend and no source', () => {
		const config = createConfig({
			enabled: true,
			default_backend: 'deep_filter',
			enabled_backends: ['none', 'standard'],
			rollout_basis_points: 10000,
			included_user_ids: [OTHER_USER_ID],
		});
		for (const userId of [TARGETED_USER_ID, OTHER_USER_ID]) {
			const assignment = resolveVoiceNoiseSuppressionAssignment(config, userId);
			expect(assignment.user_targeted).toBe(false);
			expect(assignment.backend).toBeNull();
			expect(assignment.source).toBeNull();
		}
	});

	test('guild overrides naming a disabled backend are filtered out', () => {
		const config = createConfig({
			enabled: true,
			default_backend: 'standard',
			enabled_backends: ['none', 'standard', 'rnnoise'],
			rollout_basis_points: 10000,
			guild_overrides: [
				{guild_id: GUILD_ID, backend: 'rnnoise'},
				{guild_id: OTHER_GUILD_ID, backend: 'deep_filter'},
			],
		});
		expect(resolveVoiceNoiseSuppressionAssignment(config, TARGETED_USER_ID).guild_overrides).toEqual([
			{guild_id: GUILD_ID, backend: 'rnnoise'},
		]);
	});

	test('guild overrides survive the allowlist path too', () => {
		const config = createConfig({
			enabled: true,
			enabled_backends: ['none', 'standard', 'rnnoise'],
			included_user_ids: [TARGETED_USER_ID],
			guild_overrides: [
				{guild_id: GUILD_ID, backend: 'rnnoise'},
				{guild_id: OTHER_GUILD_ID, backend: 'gtcrn'},
			],
		});
		expect(resolveVoiceNoiseSuppressionAssignment(config, TARGETED_USER_ID).guild_overrides).toEqual([
			{guild_id: GUILD_ID, backend: 'rnnoise'},
		]);
	});

	test('echoes the config version on every path', () => {
		const config = createConfig({enabled: true, config_version: 11, rollout_basis_points: 10000});
		expect(resolveVoiceNoiseSuppressionAssignment(config, TARGETED_USER_ID).config_version).toBe(11);
	});
});

describe('resolveVoiceNoiseSuppressionForCall', () => {
	test('returns null when the master switch is off', () => {
		const assignment = createAssignment({
			enabled: false,
			user_targeted: true,
			backend: 'rnnoise',
			source: 'user_rule',
			allow_user_override: true,
			guild_overrides: [{guild_id: GUILD_ID, backend: 'gtcrn'}],
		});
		expect(resolveVoiceNoiseSuppressionForCall(assignment, GUILD_ID, 'speex')).toBeNull();
		expect(resolveVoiceNoiseSuppressionForCall(assignment, null, null)).toBeNull();
	});

	test('returns null when nothing targets the user', () => {
		const assignment = createAssignment({user_targeted: false, backend: null, source: null});
		expect(resolveVoiceNoiseSuppressionForCall(assignment, GUILD_ID, null)).toBeNull();
		expect(resolveVoiceNoiseSuppressionForCall(assignment, null, null)).toBeNull();
	});

	test('user_rule beats guild_rule', () => {
		const assignment = createAssignment({
			backend: 'rnnoise',
			source: 'user_rule',
			guild_overrides: [{guild_id: GUILD_ID, backend: 'gtcrn'}],
		});
		expect(resolveVoiceNoiseSuppressionForCall(assignment, GUILD_ID, null)).toMatchObject({
			backend: 'rnnoise',
			source: 'user_rule',
		});
	});

	test('guild_rule beats canary', () => {
		const assignment = createAssignment({
			backend: 'rnnoise',
			source: 'canary',
			guild_overrides: [{guild_id: GUILD_ID, backend: 'gtcrn'}],
		});
		expect(resolveVoiceNoiseSuppressionForCall(assignment, GUILD_ID, null)).toMatchObject({
			backend: 'gtcrn',
			source: 'guild_rule',
		});
	});

	test('a guild override targets an otherwise untargeted user', () => {
		const assignment = createAssignment({
			user_targeted: false,
			backend: null,
			source: null,
			guild_overrides: [{guild_id: GUILD_ID, backend: 'speex'}],
		});
		expect(resolveVoiceNoiseSuppressionForCall(assignment, GUILD_ID, null)).toMatchObject({
			backend: 'speex',
			source: 'guild_rule',
		});
		expect(resolveVoiceNoiseSuppressionForCall(assignment, OTHER_GUILD_ID, null)).toBeNull();
	});

	test('a DM falls through to the user level result', () => {
		const assignment = createAssignment({
			backend: 'rnnoise',
			source: 'canary',
			guild_overrides: [{guild_id: GUILD_ID, backend: 'gtcrn'}],
		});
		expect(resolveVoiceNoiseSuppressionForCall(assignment, null, null)).toMatchObject({
			backend: 'rnnoise',
			source: 'canary',
		});
	});

	test.each([
		{allowUserOverride: true, preference: 'speex', expectedBackend: 'speex', expectedSource: 'user_override'},
		{allowUserOverride: true, preference: 'gtcrn', expectedBackend: 'rnnoise', expectedSource: 'canary'},
		{allowUserOverride: false, preference: 'speex', expectedBackend: 'rnnoise', expectedSource: 'canary'},
		{allowUserOverride: true, preference: null, expectedBackend: 'rnnoise', expectedSource: 'canary'},
	])('allow_user_override $allowUserOverride with preference $preference resolves to $expectedSource', ({
		allowUserOverride,
		preference,
		expectedBackend,
		expectedSource,
	}) => {
		const assignment = createAssignment({
			backend: 'rnnoise',
			source: 'canary',
			allow_user_override: allowUserOverride,
			enabled_backends: ['none', 'standard', 'speex', 'rnnoise'],
		});
		expect(
			resolveVoiceNoiseSuppressionForCall(assignment, null, preference as VoiceNoiseSuppressionBackend | null),
		).toMatchObject({backend: expectedBackend, source: expectedSource});
	});

	test('a user override also outranks a guild rule', () => {
		const assignment = createAssignment({
			user_targeted: false,
			backend: null,
			source: null,
			allow_user_override: true,
			enabled_backends: ['none', 'standard', 'speex'],
			guild_overrides: [{guild_id: GUILD_ID, backend: 'standard'}],
		});
		expect(resolveVoiceNoiseSuppressionForCall(assignment, GUILD_ID, 'speex')).toMatchObject({
			backend: 'speex',
			source: 'user_override',
		});
	});

	test('carries the client knobs through', () => {
		const assignment = createAssignment({
			backend: 'rnnoise',
			source: 'user_rule',
			stereo_enabled: true,
			suppression_strength: 42,
			config_version: 19,
		});
		expect(resolveVoiceNoiseSuppressionForCall(assignment, null, null)).toEqual({
			backend: 'rnnoise',
			source: 'user_rule',
			stereoEnabled: true,
			suppressionStrength: 42,
			configVersion: 19,
		});
	});
});
