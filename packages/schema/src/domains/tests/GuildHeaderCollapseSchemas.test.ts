// SPDX-License-Identifier: AGPL-3.0-or-later

import {experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {
	DEFAULT_GUILD_HEADER_COLLAPSE_CONFIG,
	type GuildHeaderCollapseConfig,
	GuildHeaderCollapseConfigSchema,
	INERT_GUILD_HEADER_COLLAPSE_ASSIGNMENT,
	isGuildHeaderCollapseTargeted,
	resolveGuildHeaderCollapseAssignment,
} from '@fluxer/schema/src/domains/experiment/GuildHeaderCollapseSchemas';
import {describe, expect, it} from 'vitest';

const USER_ID = '1485064866382176262';
const OTHER_USER_ID = '1485064866382176263';

function config(overrides: Partial<GuildHeaderCollapseConfig> = {}): GuildHeaderCollapseConfig {
	return {...DEFAULT_GUILD_HEADER_COLLAPSE_CONFIG, ...overrides};
}

describe('GuildHeaderCollapseConfigSchema', () => {
	it('defaults to a disabled rollout that targets nobody', () => {
		expect(DEFAULT_GUILD_HEADER_COLLAPSE_CONFIG).toEqual({
			enabled: false,
			config_version: 0,
			rollout_basis_points: 0,
			rollout_salt: 'guild-header-collapse-v1',
			included_user_ids: [],
			excluded_user_ids: [],
		});
	});

	it('rejects a rollout share above the bucket resolution', () => {
		expect(() => GuildHeaderCollapseConfigSchema.parse({rollout_basis_points: 10001})).toThrow();
	});

	it('rejects a target that is not a snowflake', () => {
		expect(() => GuildHeaderCollapseConfigSchema.parse({included_user_ids: ['not-a-snowflake']})).toThrow();
	});
});

describe('resolveGuildHeaderCollapseAssignment', () => {
	it('reports the inert assignment with the stored revision while the rollout is off', () => {
		const assignment = resolveGuildHeaderCollapseAssignment(
			config({enabled: false, config_version: 4, rollout_basis_points: 10000}),
			USER_ID,
		);

		expect(assignment).toEqual({...INERT_GUILD_HEADER_COLLAPSE_ASSIGNMENT, config_version: 4});
		expect(isGuildHeaderCollapseTargeted(assignment)).toBe(false);
	});

	it('targets every account at the full rollout share', () => {
		const assignment = resolveGuildHeaderCollapseAssignment(
			config({enabled: true, config_version: 2, rollout_basis_points: 10000}),
			USER_ID,
		);

		expect(assignment).toEqual({enabled: true, config_version: 2, user_targeted: true, source: 'canary'});
		expect(isGuildHeaderCollapseTargeted(assignment)).toBe(true);
	});

	it('targets no account at a zero rollout share', () => {
		const assignment = resolveGuildHeaderCollapseAssignment(config({enabled: true}), USER_ID);

		expect(assignment).toEqual({enabled: true, config_version: 0, user_targeted: false, source: null});
		expect(isGuildHeaderCollapseTargeted(assignment)).toBe(false);
	});

	it('targets an allowlisted account outside the sampled share', () => {
		const assignment = resolveGuildHeaderCollapseAssignment(
			config({enabled: true, included_user_ids: [USER_ID]}),
			USER_ID,
		);

		expect(assignment).toEqual({enabled: true, config_version: 0, user_targeted: true, source: 'user_rule'});
	});

	it('excludes an account even when it is allowlisted and inside the share', () => {
		const assignment = resolveGuildHeaderCollapseAssignment(
			config({
				enabled: true,
				rollout_basis_points: 10000,
				included_user_ids: [USER_ID],
				excluded_user_ids: [USER_ID],
			}),
			USER_ID,
		);

		expect(assignment).toEqual({enabled: true, config_version: 0, user_targeted: false, source: null});
	});

	it('resolves the same account the same way on every call', () => {
		const rollout = config({enabled: true, rollout_basis_points: 5000});
		const first = resolveGuildHeaderCollapseAssignment(rollout, USER_ID);
		const second = resolveGuildHeaderCollapseAssignment(rollout, USER_ID);

		expect(first).toEqual(second);
	});

	it('draws a caller inside the share and not one exactly at the boundary', () => {
		const bucket = experimentBucket(USER_ID, DEFAULT_GUILD_HEADER_COLLAPSE_CONFIG.rollout_salt);
		const inside = resolveGuildHeaderCollapseAssignment(
			config({enabled: true, rollout_basis_points: bucket + 1}),
			USER_ID,
		);
		const atBoundary = resolveGuildHeaderCollapseAssignment(
			config({enabled: true, rollout_basis_points: bucket}),
			USER_ID,
		);

		expect([inside.user_targeted, inside.source]).toEqual([true, 'canary']);
		expect([atBoundary.user_targeted, atBoundary.source]).toEqual([false, null]);
	});

	it('excludes only the named account', () => {
		const rollout = config({enabled: true, rollout_basis_points: 10000, excluded_user_ids: [USER_ID]});

		expect(resolveGuildHeaderCollapseAssignment(rollout, USER_ID).user_targeted).toBe(false);
		expect(resolveGuildHeaderCollapseAssignment(rollout, OTHER_USER_ID).user_targeted).toBe(true);
	});
});
