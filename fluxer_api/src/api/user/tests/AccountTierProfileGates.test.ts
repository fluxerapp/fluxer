// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {acceptInvite, createChannelInvite, createGuild} from '@app/api/message/tests/MessageTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {type InteractionTierMode, InteractionTierModes} from '@fluxer/constants/src/AccountConfigConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface ErrorBody {
	code: string;
	errors?: Array<{path: string; code: string}>;
}

describe('profile and password gates', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function useInteractionTier(mode: InteractionTierMode): Promise<void> {
		await getInstanceConfigRepository().updateAccountsConfig({interaction_tier: mode});
	}

	async function joinCommunity(member: TestAccount): Promise<string> {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Profile Gates');
		const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
		await acceptInvite(harness, member.token, invite.code);
		return guild.id;
	}

	async function profileStatuses(member: TestAccount, guildId: string): Promise<[number, number]> {
		const account = await createBuilder<ErrorBody>(harness, member.token)
			.patch('/users/@me')
			.body({global_name: 'Gate Tester'})
			.executeRaw();
		const community = await createBuilder<ErrorBody>(harness, member.token)
			.patch(`/guilds/${guildId}/members/@me`)
			.body({nick: 'Gate Tester'})
			.executeRaw();
		return [account.response.status, community.response.status];
	}

	it('refuses an unconfirmed email on both profile routes in legacy mode', async () => {
		await useInteractionTier(InteractionTierModes.LEGACY);
		const member = await createTestAccount(harness, {skipEmailVerification: true});
		const guildId = await joinCommunity(member);
		await createBuilder(harness, member.token)
			.patch('/users/@me')
			.body({global_name: 'Gate Tester'})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.PROFILE_EMAIL_VERIFICATION_REQUIRED)
			.execute();
		await createBuilder(harness, member.token)
			.patch(`/guilds/${guildId}/members/@me`)
			.body({nick: 'Gate Tester'})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.PROFILE_EMAIL_VERIFICATION_REQUIRED)
			.execute();
	});

	it('lets an unconfirmed email edit both profiles under the enforced tier', async () => {
		await useInteractionTier(InteractionTierModes.ENFORCED);
		const member = await createTestAccount(harness, {skipEmailVerification: true});
		const guildId = await joinCommunity(member);
		expect(await profileStatuses(member, guildId)).toEqual([HTTP_STATUS.OK, HTTP_STATUS.OK]);
	});

	it.each([InteractionTierModes.LEGACY, InteractionTierModes.ENFORCED])(
		'asks for the current password before changing it in %s mode',
		async (mode) => {
			await useInteractionTier(mode);
			const account = await createTestAccount(harness);
			const {json} = await createBuilder<ErrorBody>(harness, account.token)
				.patch('/users/@me')
				.body({new_password: 'An0ther-Str0ng-Passw0rd!'})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.executeWithResponse();
			expect(json.errors?.[0]).toMatchObject({path: 'password', code: ValidationErrorCodes.PASSWORD_NOT_SET});
		},
	);
});
