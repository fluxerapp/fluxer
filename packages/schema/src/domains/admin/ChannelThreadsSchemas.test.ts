// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	applyChannelThreadsConfigUpdate,
	type ChannelThreadsConfig,
	ChannelThreadsConfigSchema,
	ChannelThreadsConfigUpdateRequest,
	channelThreadsGuildActive,
	channelThreadsGuildFieldsChanged,
	channelThreadsUserActive,
	channelThreadsUserExcluded,
	channelThreadsUserFieldsChanged,
	compileChannelThreadsConfig,
	DEFAULT_CHANNEL_THREADS_CONFIG,
	DEFAULT_COMPILED_CHANNEL_THREADS_CONFIG,
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
		expect(channelThreadsGuildActive(DEFAULT_COMPILED_CHANNEL_THREADS_CONFIG, GUILD_ID)).toBe(false);
		expect(channelThreadsUserActive(DEFAULT_COMPILED_CHANNEL_THREADS_CONFIG, USER_ID)).toBe(false);
	});

	test('keeps partial updates free of defaults and server-maintained fields', () => {
		expect(ChannelThreadsConfigUpdateRequest.parse({enabled: true})).toEqual({enabled: true});
		expect(ChannelThreadsConfigUpdateRequest.parse({config_version: 9, ever_enabled: false})).toEqual({});
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
	])('rejects invalid values in stored config and updates: %j', (value) => {
		expect(ChannelThreadsConfigSchema.safeParse(value).success).toBe(false);
		expect(ChannelThreadsConfigUpdateRequest.safeParse(value).success).toBe(false);
	});

	test('caps every targeted id list at a thousand', () => {
		const ids = syntheticIds(1001);
		for (const key of ['enabled_guild_ids', 'disabled_guild_ids', 'included_user_ids', 'excluded_user_ids']) {
			expect(ChannelThreadsConfigSchema.safeParse({[key]: ids}).success).toBe(false);
			expect(ChannelThreadsConfigSchema.safeParse({[key]: ids.slice(0, 1000)}).success).toBe(true);
		}
	});

	test('bumps the version on every update and keeps ever_enabled sticky', () => {
		const first = applyChannelThreadsConfigUpdate(DEFAULT_CHANNEL_THREADS_CONFIG, {user_basis_points: 100});
		expect(first.config_version).toBe(1);
		expect(first.ever_enabled).toBe(false);
		const enabled = applyChannelThreadsConfigUpdate(first, {enabled: true});
		expect(enabled.config_version).toBe(2);
		expect(enabled.ever_enabled).toBe(true);
		const disabled = applyChannelThreadsConfigUpdate(enabled, {enabled: false});
		expect(disabled.config_version).toBe(3);
		expect(disabled.enabled).toBe(false);
		expect(disabled.ever_enabled).toBe(true);
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

	test('the assignment is present only for active users and carries the version', () => {
		const cfg = compiled({enabled: true, config_version: 7, included_user_ids: [USER_ID]});
		expect(resolveChannelThreadsAssignment(cfg, USER_ID)).toEqual({active: true, config_version: 7});
		expect(resolveChannelThreadsAssignment(cfg, OTHER_USER_ID)).toBeUndefined();
	});

	test('field change detection separates the guild and user dimensions', () => {
		const base = config({enabled: true, enabled_guild_ids: [GUILD_ID], included_user_ids: [USER_ID]});
		expect(channelThreadsGuildFieldsChanged(base, {...base, config_version: 5})).toBe(false);
		expect(channelThreadsUserFieldsChanged(base, {...base, config_version: 5})).toBe(false);
		expect(channelThreadsGuildFieldsChanged(base, {...base, enabled_guild_ids: [OTHER_GUILD_ID]})).toBe(true);
		expect(channelThreadsUserFieldsChanged(base, {...base, enabled_guild_ids: [OTHER_GUILD_ID]})).toBe(false);
		expect(channelThreadsGuildFieldsChanged(base, {...base, excluded_user_ids: [USER_ID]})).toBe(false);
		expect(channelThreadsUserFieldsChanged(base, {...base, excluded_user_ids: [USER_ID]})).toBe(true);
		expect(channelThreadsGuildFieldsChanged(base, {...base, enabled_guild_ids: [GUILD_ID, GUILD_ID]})).toBe(false);
		expect(channelThreadsGuildFieldsChanged(base, {...base, enabled: false})).toBe(true);
		expect(channelThreadsUserFieldsChanged(base, {...base, enabled: false})).toBe(true);
	});
});

describe('shared experiment bucket vectors', () => {
	test.each(vectors as Array<[string, string, number]>)('bucket(%s, %s) is %i', (salt, id, bucket) => {
		expect(experimentBucket(id, salt)).toBe(bucket);
	});
});
