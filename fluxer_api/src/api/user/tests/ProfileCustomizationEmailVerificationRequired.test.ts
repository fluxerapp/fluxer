// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {acceptInvite, createChannelInvite, createGuild, getChannel} from '@app/api/channel/tests/ChannelTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface ErrorResponse {
	code: string;
	message: string;
}

async function setEmailVerified(harness: ApiTestHarness, userId: string, emailVerified: boolean): Promise<void> {
	await createBuilder(harness, '')
		.post(`/test/users/${userId}/security-flags`)
		.body({
			email_verified: emailVerified,
			suspicious_activity_flags: 0,
		})
		.execute();
}

describe('Profile customization requires verified email', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});
	it('rejects global profile customization for unverified accounts', async () => {
		const account = await createTestAccount(harness);
		await setEmailVerified(harness, account.userId, false);
		const {json} = await createBuilder<ErrorResponse>(harness, account.token)
			.patch('/users/@me')
			.body({bio: 'blocked'})
			.expect(403)
			.executeWithResponse();
		expect(json.code).toBe(APIErrorCodes.PROFILE_EMAIL_VERIFICATION_REQUIRED);
	});
	it('still allows non-profile account flags for unverified accounts', async () => {
		const account = await createTestAccount(harness);
		await setEmailVerified(harness, account.userId, false);
		const updated = await createBuilder<{
			has_dismissed_premium_onboarding: boolean;
		}>(harness, account.token)
			.patch('/users/@me')
			.body({has_dismissed_premium_onboarding: true})
			.expect(200)
			.execute();
		expect(updated.has_dismissed_premium_onboarding).toBeDefined();
	});
	it('rejects community profile customization for unverified accounts', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Profile Lock Guild');
		const channel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);
		await setEmailVerified(harness, member.userId, false);
		const {json} = await createBuilder<ErrorResponse>(harness, member.token)
			.patch(`/guilds/${guild.id}/members/@me`)
			.body({nick: 'blocked'})
			.expect(403)
			.executeWithResponse();
		expect(json.code).toBe(APIErrorCodes.PROFILE_EMAIL_VERIFICATION_REQUIRED);
	});
});
