// SPDX-License-Identifier: AGPL-3.0-or-later

import {EXPERIMENT_BUCKET_RESOLUTION, experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {
	DEFAULT_MESSAGE_KEYBOARD_FOCUS_CONFIG,
	MessageKeyboardFocusConfigSchema,
	resolveMessageKeyboardFocusAssignment,
} from '@fluxer/schema/src/domains/experiment/MessageKeyboardFocusSchemas';
import {describe, expect, it} from 'vitest';

const USER_ID = '1485064866382176262';
const OTHER_USER_ID = '1485064866382176263';

function config(overrides: Partial<ReturnType<typeof MessageKeyboardFocusConfigSchema.parse>> = {}) {
	return MessageKeyboardFocusConfigSchema.parse({...overrides});
}

describe('message keyboard focus configuration', () => {
	it('defaults to a disabled rollout that targets nobody', () => {
		expect(DEFAULT_MESSAGE_KEYBOARD_FOCUS_CONFIG).toEqual({
			enabled: false,
			config_version: 0,
			rollout_basis_points: 0,
			rollout_salt: 'message-keyboard-focus-v1',
			included_user_ids: [],
			excluded_user_ids: [],
		});
	});

	it('refuses a rollout wider than the bucket resolution', () => {
		expect(() => config({rollout_basis_points: EXPERIMENT_BUCKET_RESOLUTION + 1})).toThrow();
	});

	it('refuses a target that is not a snowflake', () => {
		expect(() => config({included_user_ids: ['not-a-snowflake']})).toThrow();
	});
});

describe('message keyboard focus resolution', () => {
	it('reports the stored revision while the rollout is disabled', () => {
		const assignment = resolveMessageKeyboardFocusAssignment(config({config_version: 3}), USER_ID);
		expect(assignment).toEqual({enabled: false, config_version: 3, user_targeted: false, source: null});
	});

	it('targets an allowlisted account regardless of its bucket', () => {
		const assignment = resolveMessageKeyboardFocusAssignment(
			config({enabled: true, included_user_ids: [USER_ID]}),
			USER_ID,
		);
		expect(assignment).toEqual({enabled: true, config_version: 0, user_targeted: true, source: 'user_rule'});
	});

	it('keeps a blocklisted account out of a full rollout', () => {
		const assignment = resolveMessageKeyboardFocusAssignment(
			config({
				enabled: true,
				rollout_basis_points: EXPERIMENT_BUCKET_RESOLUTION,
				excluded_user_ids: [USER_ID],
			}),
			USER_ID,
		);
		expect(assignment).toEqual({enabled: true, config_version: 0, user_targeted: false, source: null});
	});

	it('prefers the blocklist over the allowlist', () => {
		const assignment = resolveMessageKeyboardFocusAssignment(
			config({enabled: true, included_user_ids: [USER_ID], excluded_user_ids: [USER_ID]}),
			USER_ID,
		);
		expect(assignment.user_targeted).toBe(false);
	});

	it('targets an account whose bucket falls inside the rollout', () => {
		const salt = DEFAULT_MESSAGE_KEYBOARD_FOCUS_CONFIG.rollout_salt;
		const bucket = experimentBucket(USER_ID, salt);
		const inside = resolveMessageKeyboardFocusAssignment(
			config({enabled: true, rollout_basis_points: bucket + 1}),
			USER_ID,
		);
		const outside = resolveMessageKeyboardFocusAssignment(
			config({enabled: true, rollout_basis_points: bucket}),
			USER_ID,
		);
		expect(inside).toEqual({enabled: true, config_version: 0, user_targeted: true, source: 'canary'});
		expect(outside).toEqual({enabled: true, config_version: 0, user_targeted: false, source: null});
	});

	it('separates two accounts under the same salt', () => {
		const salt = DEFAULT_MESSAGE_KEYBOARD_FOCUS_CONFIG.rollout_salt;
		expect(experimentBucket(USER_ID, salt)).not.toBe(experimentBucket(OTHER_USER_ID, salt));
	});

	it('moves an account when the salt changes', () => {
		expect(experimentBucket(USER_ID, 'message-keyboard-focus-v1')).not.toBe(
			experimentBucket(USER_ID, 'message-keyboard-focus-v2'),
		);
	});
});
