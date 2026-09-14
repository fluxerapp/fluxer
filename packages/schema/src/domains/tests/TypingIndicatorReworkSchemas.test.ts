// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DEFAULT_TYPING_INDICATOR_REWORK_CONFIG,
	INERT_TYPING_INDICATOR_REWORK_ASSIGNMENT,
	isTypingIndicatorReworkTargeted,
	resolveTypingIndicatorReworkAssignment,
	type TypingIndicatorReworkConfig,
	TypingIndicatorReworkConfigSchema,
} from '@fluxer/schema/src/domains/experiment/TypingIndicatorReworkSchemas';
import {describe, expect, it} from 'vitest';

const USER_ID = '1485064866382176262';
const OTHER_USER_ID = '1485064866382176263';

function config(overrides: Partial<TypingIndicatorReworkConfig> = {}): TypingIndicatorReworkConfig {
	return {...DEFAULT_TYPING_INDICATOR_REWORK_CONFIG, ...overrides};
}

describe('TypingIndicatorReworkConfigSchema', () => {
	it('defaults to a disabled rollout that targets nobody', () => {
		expect(DEFAULT_TYPING_INDICATOR_REWORK_CONFIG).toEqual({
			enabled: false,
			config_version: 0,
			rollout_basis_points: 0,
			rollout_salt: 'typing-indicator-rework-v1',
			included_user_ids: [],
			excluded_user_ids: [],
		});
	});

	it('rejects a rollout share above the bucket resolution', () => {
		expect(() => TypingIndicatorReworkConfigSchema.parse({rollout_basis_points: 10001})).toThrow();
	});

	it('rejects a target that is not a snowflake', () => {
		expect(() => TypingIndicatorReworkConfigSchema.parse({included_user_ids: ['not-a-snowflake']})).toThrow();
	});
});

describe('resolveTypingIndicatorReworkAssignment', () => {
	it('reports the inert assignment with the stored revision while the rollout is off', () => {
		const assignment = resolveTypingIndicatorReworkAssignment(
			config({enabled: false, config_version: 4, rollout_basis_points: 10000}),
			USER_ID,
		);

		expect(assignment).toEqual({...INERT_TYPING_INDICATOR_REWORK_ASSIGNMENT, config_version: 4});
		expect(isTypingIndicatorReworkTargeted(assignment)).toBe(false);
	});

	it('targets every account at the full rollout share', () => {
		const assignment = resolveTypingIndicatorReworkAssignment(
			config({enabled: true, config_version: 2, rollout_basis_points: 10000}),
			USER_ID,
		);

		expect(assignment).toEqual({enabled: true, config_version: 2, user_targeted: true, source: 'canary'});
		expect(isTypingIndicatorReworkTargeted(assignment)).toBe(true);
	});

	it('targets no account at a zero rollout share', () => {
		const assignment = resolveTypingIndicatorReworkAssignment(config({enabled: true}), USER_ID);

		expect(assignment).toEqual({enabled: true, config_version: 0, user_targeted: false, source: null});
		expect(isTypingIndicatorReworkTargeted(assignment)).toBe(false);
	});

	it('targets an allowlisted account outside the sampled share', () => {
		const assignment = resolveTypingIndicatorReworkAssignment(
			config({enabled: true, included_user_ids: [USER_ID]}),
			USER_ID,
		);

		expect(assignment).toEqual({enabled: true, config_version: 0, user_targeted: true, source: 'user_rule'});
	});

	it('excludes an account even when it is allowlisted and inside the share', () => {
		const assignment = resolveTypingIndicatorReworkAssignment(
			config({
				enabled: true,
				rollout_basis_points: 10000,
				included_user_ids: [USER_ID],
				excluded_user_ids: [USER_ID],
			}),
			USER_ID,
		);

		expect(assignment).toEqual({enabled: true, config_version: 0, user_targeted: false, source: null});
		expect(
			resolveTypingIndicatorReworkAssignment(config({enabled: true, rollout_basis_points: 10000}), OTHER_USER_ID),
		).toEqual({enabled: true, config_version: 0, user_targeted: true, source: 'canary'});
	});

	it('resolves the same account the same way on every call', () => {
		const rollout = config({enabled: true, rollout_basis_points: 5000});
		const first = resolveTypingIndicatorReworkAssignment(rollout, USER_ID);
		const second = resolveTypingIndicatorReworkAssignment(rollout, USER_ID);

		expect(first).toEqual(second);
	});
});
