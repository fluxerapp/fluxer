// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ChannelThreadsAssignmentResponse,
	ChannelThreadsConfigSchema,
	everyoneChannelThreadsConfig,
} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import {describe, expect, test} from 'vitest';

describe('channel threads configuration', () => {
	test('the everyone config serves every guild and user and keeps the given version', () => {
		const everyone = everyoneChannelThreadsConfig(42);
		expect(everyone).toEqual({
			enabled: true,
			config_version: 42,
			ever_enabled: true,
			guild_basis_points: 10000,
			guild_salt: 'channel-threads-guild-v1',
			enabled_guild_ids: [],
			disabled_guild_ids: [],
			user_basis_points: 10000,
			user_salt: 'channel-threads-user-v1',
			included_user_ids: [],
			excluded_user_ids: [],
		});
		expect(ChannelThreadsConfigSchema.parse(everyone)).toEqual(everyone);
		expect(ChannelThreadsAssignmentResponse.parse({active: true, config_version: everyone.config_version})).toEqual({
			active: true,
			config_version: 42,
		});
	});
});
