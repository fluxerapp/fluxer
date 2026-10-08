// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type ChannelThreadsConfig,
	ChannelThreadsConfigSchema,
	channelThreadsGuildActive,
	channelThreadsUserActive,
	channelThreadsUserExcluded,
	compileChannelThreadsConfig,
	DEFAULT_CHANNEL_THREADS_CONFIG,
	everyoneChannelThreadsConfig,
	resolveChannelThreadsAssignment,
} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import {experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import vectors from '@fluxer/schema/src/domains/experiment/ExperimentBucketVectors.json' with {type: 'json'};
import {describe, expect, test} from 'vitest';

const GUILD_ID = '1000000000000000001';
const OTHER_GUILD_ID = '1000000000000000002';
const USER_ID = '1100000000000000001';
const OTHER_USER_ID = '1100000000000000002';

function config(overrides: Partial<ChannelThreadsConfig> = {}): ChannelThreadsConfig {
	return {...DEFAULT_CHANNEL_THREADS_CONFIG, ...overrides};
}

function compiled(overrides: Partial<ChannelThreadsConfig> = {}) {
	return compileChannelThreadsConfig(config(overrides));
}

function syntheticIds(count: number): Array<string> {
	return Array.from({length: count}, (_, index) => (1400000000000000000n + BigInt(index)).toString());
}

describe('channel threads configuration', () => {
	test('defaults to fully off', () => {
		expect(DEFAULT_CHANNEL_THREADS_CONFIG).toEqual({
			enabled: false,
			config_version: 0,
			ever_enabled: false,
			guild_basis_points: 0,
			guild_salt: 'channel-threads-guild-v1',
			enabled_guild_ids: [],
			disabled_guild_ids: [],
			user_basis_points: 0,
			user_salt: 'channel-threads-user-v1',
			included_user_ids: [],
			excluded_user_ids: [],
		});
		expect(channelThreadsGuildActive(compiled(), GUILD_ID)).toBe(false);
		expect(channelThreadsUserActive(compiled(), USER_ID)).toBe(false);
	});

	test.each([
		{guild_basis_points: -1},
		{guild_basis_points: 10001},
		{user_basis_points: 1.5},
		{guild_salt: ' '},
		{user_salt: 'channel-threads-é'},
		{guild_salt: 'x'.repeat(65)},
		{enabled_guild_ids: ['guild']},
		{excluded_user_ids: ['123456789012345678901']},
	])('rejects invalid values: %j', (value) => {
		expect(ChannelThreadsConfigSchema.safeParse(value).success).toBe(false);
	});

	test('caps every targeted id list at a thousand', () => {
		const ids = syntheticIds(1001);
		for (const key of ['enabled_guild_ids', 'disabled_guild_ids', 'included_user_ids', 'excluded_user_ids']) {
			expect(ChannelThreadsConfigSchema.safeParse({[key]: ids}).success).toBe(false);
			expect(ChannelThreadsConfigSchema.safeParse({[key]: ids.slice(0, 1000)}).success).toBe(true);
		}
	});

	test('the everyone config serves every guild and user and keeps the given version', () => {
		const everyone = everyoneChannelThreadsConfig(42);
		expect(everyone).toEqual({
			...DEFAULT_CHANNEL_THREADS_CONFIG,
			enabled: true,
			config_version: 42,
			ever_enabled: true,
			guild_basis_points: 10000,
			user_basis_points: 10000,
		});
		expect(ChannelThreadsConfigSchema.parse(everyone)).toEqual(everyone);
		const cfg = compileChannelThreadsConfig(everyone);
		for (const id of syntheticIds(200)) {
			expect(channelThreadsGuildActive(cfg, id)).toBe(true);
			expect(channelThreadsUserActive(cfg, id)).toBe(true);
		}
		expect(resolveChannelThreadsAssignment(cfg, USER_ID)).toEqual({active: true, config_version: 42});
	});
});

describe('channel threads predicates', () => {
	test('the master switch overrides every list', () => {
		const cfg = compiled({
			enabled_guild_ids: [GUILD_ID],
			included_user_ids: [USER_ID],
			guild_basis_points: 10000,
			user_basis_points: 10000,
		});
		expect(channelThreadsGuildActive(cfg, GUILD_ID)).toBe(false);
		expect(channelThreadsUserActive(cfg, USER_ID)).toBe(false);
		expect(resolveChannelThreadsAssignment(cfg, USER_ID)).toBeUndefined();
	});

	test('exclusion beats inclusion and inclusion beats the bucket', () => {
		const cfg = compiled({
			enabled: true,
			enabled_guild_ids: [GUILD_ID],
			disabled_guild_ids: [GUILD_ID],
			included_user_ids: [USER_ID, OTHER_USER_ID],
			excluded_user_ids: [USER_ID],
		});
		expect(channelThreadsGuildActive(cfg, GUILD_ID)).toBe(false);
		expect(channelThreadsGuildActive(cfg, OTHER_GUILD_ID)).toBe(false);
		expect(channelThreadsUserActive(cfg, USER_ID)).toBe(false);
		expect(channelThreadsUserActive(cfg, OTHER_USER_ID)).toBe(true);
		expect(channelThreadsUserExcluded(cfg, USER_ID)).toBe(true);
		expect(channelThreadsUserExcluded(cfg, OTHER_USER_ID)).toBe(false);
	});

	test('the guild and user buckets use their own salts', () => {
		const guildBucket = experimentBucket(GUILD_ID, 'channel-threads-guild-v1');
		const userBucket = experimentBucket(USER_ID, 'channel-threads-user-v1');
		expect(channelThreadsGuildActive(compiled({enabled: true, guild_basis_points: guildBucket}), GUILD_ID)).toBe(false);
		expect(channelThreadsGuildActive(compiled({enabled: true, guild_basis_points: guildBucket + 1}), GUILD_ID)).toBe(
			true,
		);
		expect(channelThreadsUserActive(compiled({enabled: true, user_basis_points: userBucket}), USER_ID)).toBe(false);
		expect(channelThreadsUserActive(compiled({enabled: true, user_basis_points: userBucket + 1}), USER_ID)).toBe(true);
	});

	test('a full rollout covers everyone who is not excluded', () => {
		const cfg = compiled({enabled: true, guild_basis_points: 10000, user_basis_points: 10000});
		for (const id of syntheticIds(200)) {
			expect(channelThreadsGuildActive(cfg, id)).toBe(true);
			expect(channelThreadsUserActive(cfg, id)).toBe(true);
		}
	});

	test('the assignment is present only for active users and reports the version', () => {
		const cfg = compiled({enabled: true, config_version: 7, included_user_ids: [USER_ID]});
		expect(resolveChannelThreadsAssignment(cfg, USER_ID)).toEqual({active: true, config_version: 7});
		expect(resolveChannelThreadsAssignment(cfg, OTHER_USER_ID)).toBeUndefined();
	});
});

describe('shared experiment bucket vectors', () => {
	test.each(vectors as Array<[string, string, number]>)('bucket(%s, %s) is %i', (salt, id, bucket) => {
		expect(experimentBucket(id, salt)).toBe(bucket);
	});
});
