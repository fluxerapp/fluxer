// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {acceptInvite, createChannelInvite, createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

const STAFF_TEST_FLAGS = UserFlags.HAS_SESSION_STARTED | UserFlags.STAFF;

async function addGuildFeaturesForTesting(
	harness: ApiTestHarness,
	guildId: string,
	features: Array<string>,
): Promise<void> {
	await createBuilder<{
		success: boolean;
	}>(harness, '')
		.post(`/test/guilds/${guildId}/features`)
		.body({add_features: features})
		.execute();
}

async function setUserFlagsForTesting(harness: ApiTestHarness, userId: string, flags: bigint): Promise<void> {
	await createBuilder<{
		success: boolean;
	}>(harness, '')
		.patch(`/test/users/${userId}/flags`)
		.body({flags: flags.toString()})
		.execute();
}

describe('Guild unavailable feature access checks', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	it('blocks /guilds/* and /channels/* when UNAVAILABLE_FOR_EVERYONE is enabled', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Unavailable for everyone');
		if (!guild.system_channel_id) {
			throw new Error('Guild system channel is missing');
		}
		await setUserFlagsForTesting(harness, owner.userId, STAFF_TEST_FLAGS);
		await addGuildFeaturesForTesting(harness, guild.id, [GuildFeatures.UNAVAILABLE_FOR_EVERYONE]);
		await createBuilder(harness, owner.token)
			.get(`/guilds/${guild.id}`)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_ACCESS)
			.execute();
		await createBuilder(harness, owner.token)
			.get(`/channels/${guild.system_channel_id}`)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_ACCESS)
			.execute();
	});
	it('blocks non-staff and allows staff for UNAVAILABLE_FOR_EVERYONE_BUT_STAFF', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Unavailable for everyone but staff');
		if (!guild.system_channel_id) {
			throw new Error('Guild system channel is missing');
		}
		const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id);
		await acceptInvite(harness, member.token, invite.code);
		await addGuildFeaturesForTesting(harness, guild.id, [GuildFeatures.UNAVAILABLE_FOR_EVERYONE_BUT_STAFF]);
		await createBuilder(harness, member.token)
			.get(`/guilds/${guild.id}`)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_ACCESS)
			.execute();
		await createBuilder(harness, member.token)
			.get(`/channels/${guild.system_channel_id}`)
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_ACCESS)
			.execute();
		await setUserFlagsForTesting(harness, owner.userId, STAFF_TEST_FLAGS);
		await createBuilder(harness, owner.token).get(`/guilds/${guild.id}`).expect(HTTP_STATUS.OK).execute();
		await createBuilder(harness, owner.token)
			.get(`/channels/${guild.system_channel_id}`)
			.expect(HTTP_STATUS.OK)
			.execute();
	});
});
