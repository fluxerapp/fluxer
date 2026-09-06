// SPDX-License-Identifier: AGPL-3.0-or-later

import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';
import {createTestAccount, createUniqueEmail, createUniqueUsername, setUserACLs} from '../../auth/tests/AuthTestUtils';
import {setupTestGuildWithMembers} from '../../guild/tests/GuildTestUtils';
import {getInstanceConfigRepository} from '../../middleware/ServiceSingletons';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '../../test/TestRequestBuilder';

interface PendingRegistrationResponse {
	user_id: string;
}

describe('pending registration approval and the stock community', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	it('keeps a banned user out of the stock community on approval', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		await getInstanceConfigRepository().setInstancePolicyConfig({
			single_community_enabled: true,
			single_community_guild_id: guild.id,
		});
		await getInstanceConfigRepository().setRegistrationConfig({mode: 'approval'});

		const pending = await createBuilderWithoutAuth<PendingRegistrationResponse>(harness)
			.post('/auth/register')
			.body({
				email: createUniqueEmail('bannedapproval'),
				username: createUniqueUsername('bannedapproval'),
				global_name: 'The banned man',
				password: 'approving-since-1999',
				date_of_birth: '2000-01-01',
				consent: true,
			})
			.execute();

		await createBuilder(harness, owner.token)
			.put(`/guilds/${guild.id}/bans/${pending.user_id}`)
			.body({})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();

		await createBuilder(harness, admin.token)
			.patch(`/admin/instance/pending-registrations/${pending.user_id}`)
			.body({status: 'approved'})
			.expect(HTTP_STATUS.OK)
			.execute();

		await createBuilder(harness, owner.token)
			.get(`/guilds/${guild.id}/members/${pending.user_id}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});

	it('does not add a user who was never pending to the stock community', async () => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
		]);
		const {owner, guild} = await setupTestGuildWithMembers(harness, 0);
		const outsider = await createTestAccount(harness);
		await getInstanceConfigRepository().setInstancePolicyConfig({
			single_community_enabled: true,
			single_community_guild_id: guild.id,
		});

		await createBuilder(harness, admin.token)
			.patch(`/admin/instance/pending-registrations/${outsider.userId}`)
			.body({status: 'approved'})
			.expect(HTTP_STATUS.OK)
			.execute();

		await createBuilder(harness, owner.token)
			.get(`/guilds/${guild.id}/members/${outsider.userId}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
});
