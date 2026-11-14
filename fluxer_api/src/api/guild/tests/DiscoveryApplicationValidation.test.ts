// SPDX-License-Identifier: AGPL-3.0-or-later

import {setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {DiscoveryCategories} from '@fluxer/constants/src/DiscoveryConstants';
import type {DiscoveryApplicationResponse} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import {afterEach, beforeEach, describe, test} from 'vitest';

async function setGuildMemberCount(harness: ApiTestHarness, guildId: string, memberCount: number): Promise<void> {
	await createBuilder(harness, '')
		.post(`/test/guilds/${guildId}/member-count`)
		.body({member_count: memberCount})
		.execute();
}

describe('Discovery Application Validation', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('permission requirements', () => {
		test('should require MANAGE_GUILD permission to apply', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await setGuildMemberCount(harness, guild.id, 1);
			await createBuilder(harness, member.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Should not be allowed', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
		});
		test('should require MANAGE_GUILD permission to edit', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await setGuildMemberCount(harness, guild.id, 1);
			await createBuilder<DiscoveryApplicationResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Owner applied for discovery', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, member.token)
				.patch(`/guilds/${guild.id}/discovery`)
				.body({description: 'Member tries to edit'})
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
		});
		test('should require MANAGE_GUILD permission to withdraw', async () => {
			const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await setGuildMemberCount(harness, guild.id, 1);
			await createBuilder<DiscoveryApplicationResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/discovery`)
				.body({description: 'Owner applied for discovery', category_type: DiscoveryCategories.GAMING})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, member.token)
				.delete(`/guilds/${guild.id}/discovery`)
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
		});
		test('should require MANAGE_GUILD permission to get status', async () => {
			const {members, guild} = await setupTestGuildWithMembers(harness, 1);
			const member = members[0];
			await createBuilder(harness, member.token)
				.get(`/guilds/${guild.id}/discovery`)
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
				.execute();
		});
	});
});
