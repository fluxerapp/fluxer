// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DEFAULT_DOMAIN_MIGRATION_CONFIG,
	type DomainMigrationConfig,
	DomainMigrationConfigSchema,
	DomainMigrationConfigUpdateRequest,
	INERT_DOMAIN_MIGRATION_ASSIGNMENT,
	resolveDomainMigrationAssignment,
	toDomainMigrationDiscovery,
} from '@fluxer/schema/src/domains/admin/DomainMigrationSchemas';
import {experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {describe, expect, test} from 'vitest';

const TARGETED_USER_ID = '1000000000000000001';
const OTHER_USER_ID = '1000000000000000002';

function createConfig(overrides: Partial<DomainMigrationConfig> = {}): DomainMigrationConfig {
	return {
		...DEFAULT_DOMAIN_MIGRATION_CONFIG,
		included_user_ids: [],
		excluded_user_ids: [],
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

function targetedUserIds(config: DomainMigrationConfig, userIds: ReadonlyArray<string>): Set<string> {
	const targeted = new Set<string>();
	for (const userId of userIds) {
		if (resolveDomainMigrationAssignment(config, userId).enabled) {
			targeted.add(userId);
		}
	}
	return targeted;
}

describe('domain migration configuration', () => {
	test('derives defaults from the schema with independently owned arrays', () => {
		const first = DomainMigrationConfigSchema.parse({});
		const second = DomainMigrationConfigSchema.parse({});
		expect(first).toEqual(DEFAULT_DOMAIN_MIGRATION_CONFIG);
		first.included_user_ids.push(TARGETED_USER_ID);
		first.excluded_user_ids.push(OTHER_USER_ID);
		expect(second).toEqual(DEFAULT_DOMAIN_MIGRATION_CONFIG);
	});

	test('defaults to disabled with an empty rollout', () => {
		expect(DEFAULT_DOMAIN_MIGRATION_CONFIG.enabled).toBe(false);
		expect(DEFAULT_DOMAIN_MIGRATION_CONFIG.rollout_basis_points).toBe(0);
		expect(DEFAULT_DOMAIN_MIGRATION_CONFIG.anonymous_rollout_basis_points).toBe(0);
		expect(DEFAULT_DOMAIN_MIGRATION_CONFIG.rollout_salt).toBe('domain-migration-v1');
		expect(DEFAULT_DOMAIN_MIGRATION_CONFIG.standalone_forwarding).toBe(false);
	});

	test('fills standalone forwarding in for configs stored before it existed', () => {
		const stored = DomainMigrationConfigSchema.parse({enabled: true, config_version: 4, rollout_basis_points: 100});
		expect(stored.standalone_forwarding).toBe(false);
	});

	test.each([{}, {enabled: false}, {enabled: undefined}, {rollout_basis_points: 2500}, {standalone_forwarding: true}])(
		'keeps partial updates free of configuration defaults: %j',
		(patch) => {
			expect(DomainMigrationConfigUpdateRequest.parse(patch)).toEqual(patch);
		},
	);

	test('does not accept a client-provided configuration version', () => {
		expect(DomainMigrationConfigUpdateRequest.parse({config_version: 12})).toEqual({});
	});

	test.each([
		{rollout_basis_points: -1},
		{rollout_basis_points: 10001},
		{anonymous_rollout_basis_points: -1},
		{anonymous_rollout_basis_points: 10001},
		{anonymous_rollout_basis_points: 1.5},
		{rollout_salt: ' '},
		{rollout_salt: 'domain-migration-\u00e9'},
		{included_user_ids: ['not-an-id']},
		{excluded_user_ids: ['not-an-id']},
		{standalone_forwarding: 'yes'},
	])('applies the same validation to stored configuration and updates: %j', (value) => {
		expect(DomainMigrationConfigSchema.safeParse(value).success).toBe(false);
		expect(DomainMigrationConfigUpdateRequest.safeParse(value).success).toBe(false);
	});

	test('rejects more than a thousand targeted user ids', () => {
		const ids = syntheticUserIds(1001);
		expect(DomainMigrationConfigSchema.safeParse({included_user_ids: ids}).success).toBe(false);
		expect(DomainMigrationConfigSchema.safeParse({included_user_ids: ids.slice(0, 1000)}).success).toBe(true);
	});
});

describe('resolveDomainMigrationAssignment', () => {
	test('the inert assignment is disabled', () => {
		expect(INERT_DOMAIN_MIGRATION_ASSIGNMENT.enabled).toBe(false);
	});

	test('returns the inert assignment when the master switch is off', () => {
		const config = createConfig({
			enabled: false,
			rollout_basis_points: 10000,
			included_user_ids: [TARGETED_USER_ID],
		});
		expect(resolveDomainMigrationAssignment(config, TARGETED_USER_ID)).toEqual(INERT_DOMAIN_MIGRATION_ASSIGNMENT);
	});

	test('returns the inert assignment for the default config', () => {
		expect(resolveDomainMigrationAssignment(createConfig(), TARGETED_USER_ID)).toEqual(
			INERT_DOMAIN_MIGRATION_ASSIGNMENT,
		);
	});

	test('never hands back the shared inert object', () => {
		const assignment = resolveDomainMigrationAssignment(createConfig(), TARGETED_USER_ID);
		expect(assignment).not.toBe(INERT_DOMAIN_MIGRATION_ASSIGNMENT);
	});

	test('denylist beats allowlist', () => {
		const config = createConfig({
			enabled: true,
			included_user_ids: [TARGETED_USER_ID],
			excluded_user_ids: [TARGETED_USER_ID],
		});
		expect(resolveDomainMigrationAssignment(config, TARGETED_USER_ID).enabled).toBe(false);
	});

	test('denylist beats the bucket', () => {
		const config = createConfig({
			enabled: true,
			rollout_basis_points: 10000,
			excluded_user_ids: [TARGETED_USER_ID],
		});
		expect(resolveDomainMigrationAssignment(config, TARGETED_USER_ID).enabled).toBe(false);
		expect(resolveDomainMigrationAssignment(config, OTHER_USER_ID).enabled).toBe(true);
	});

	test('allowlist beats the bucket', () => {
		const config = createConfig({
			enabled: true,
			rollout_basis_points: 0,
			included_user_ids: [TARGETED_USER_ID],
		});
		expect(resolveDomainMigrationAssignment(config, TARGETED_USER_ID).enabled).toBe(true);
		expect(resolveDomainMigrationAssignment(config, OTHER_USER_ID).enabled).toBe(false);
	});

	test.each([
		{basisPoints: 0, enabled: false},
		{basisPoints: 10000, enabled: true},
	])('a rollout of $basisPoints basis points targets $enabled', ({basisPoints, enabled}) => {
		const config = createConfig({enabled: true, rollout_basis_points: basisPoints});
		for (const userId of syntheticUserIds(200)) {
			expect(resolveDomainMigrationAssignment(config, userId).enabled).toBe(enabled);
		}
	});

	test('the bucket boundary is exclusive at the low end and inclusive one point above', () => {
		const salt = DEFAULT_DOMAIN_MIGRATION_CONFIG.rollout_salt;
		const bucket = experimentBucket(TARGETED_USER_ID, salt);
		expect(
			resolveDomainMigrationAssignment(createConfig({enabled: true, rollout_basis_points: bucket}), TARGETED_USER_ID)
				.enabled,
		).toBe(false);
		expect(
			resolveDomainMigrationAssignment(
				createConfig({enabled: true, rollout_basis_points: bucket + 1}),
				TARGETED_USER_ID,
			).enabled,
		).toBe(true);
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

	test('the targeted set follows the salt', () => {
		const userIds = syntheticUserIds(2000);
		const first = targetedUserIds(
			createConfig({enabled: true, rollout_basis_points: 5000, rollout_salt: 'domain-migration-v1'}),
			userIds,
		);
		const second = targetedUserIds(
			createConfig({enabled: true, rollout_basis_points: 5000, rollout_salt: 'domain-migration-v2'}),
			userIds,
		);
		expect(first).not.toEqual(second);
	});
});

describe('toDomainMigrationDiscovery', () => {
	test('projects only the public fields of the config', () => {
		const config = createConfig({
			enabled: true,
			config_version: 3,
			rollout_basis_points: 500,
			rollout_salt: 'domain-migration-v2',
			anonymous_rollout_basis_points: 250,
			included_user_ids: [TARGETED_USER_ID],
			excluded_user_ids: [OTHER_USER_ID],
			standalone_forwarding: true,
		});
		expect(toDomainMigrationDiscovery(config)).toEqual({
			enabled: true,
			anonymous_rollout_basis_points: 250,
			rollout_salt: 'domain-migration-v2',
			standalone_forwarding: true,
		});
	});

	test('reports the kill switch for the default config', () => {
		expect(toDomainMigrationDiscovery(DEFAULT_DOMAIN_MIGRATION_CONFIG)).toEqual({
			enabled: false,
			anonymous_rollout_basis_points: 0,
			rollout_salt: 'domain-migration-v1',
			standalone_forwarding: false,
		});
	});
});
