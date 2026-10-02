// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness, createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	createChannelInvite,
	createGuild,
	sendChannelMessage,
} from '@app/api/channel/tests/ChannelTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface ErrorResponse {
	code: string;
	message: string;
}

async function setFlags(harness: ApiTestHarness, userId: string, flags: Array<string>): Promise<void> {
	await createBuilder(harness, '').post(`/test/users/${userId}/security-flags`).body({set_flags: flags}).execute();
}

async function clearFlags(harness: ApiTestHarness, userId: string, flags: Array<string>): Promise<void> {
	await createBuilder(harness, '').post(`/test/users/${userId}/security-flags`).body({clear_flags: flags}).execute();
}

async function expectLimited(request: Promise<{json: ErrorResponse}>): Promise<void> {
	const {json} = await request;
	expect(json.code).toBe(APIErrorCodes.ACCOUNT_LIMITED);
	expect(json.message).toBe('Your account is limited. Check your email for how to lift it.');
}

describe('Account limitation', () => {
	let harness: ApiTestHarness;
	let owner: TestAccount;
	let member: TestAccount;
	let guild: GuildResponse;
	let channelId: string;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		owner = await createTestAccount(harness);
		member = await createTestAccount(harness);
		guild = await createGuild(harness, owner.token, 'Limit Guild');
		channelId = guild.system_channel_id!;
		const invite = await createChannelInvite(harness, owner.token, channelId);
		await acceptInvite(harness, member.token, invite.code);
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	it('reports the limitation on the private user', async () => {
		await setFlags(harness, member.userId, ['ACCOUNT_LIMITED']);
		const me = await createBuilder<{account_limited?: boolean}>(harness, member.token).get('/users/@me').execute();
		expect(me.account_limited).toBe(true);
		await clearFlags(harness, member.userId, ['ACCOUNT_LIMITED']);
		const lifted = await createBuilder<{account_limited?: boolean}>(harness, member.token).get('/users/@me').execute();
		expect(lifted.account_limited).toBe(false);
	});

	it('blocks sending messages', async () => {
		await setFlags(harness, member.userId, ['ACCOUNT_LIMITED']);
		await expectLimited(
			createBuilder<ErrorResponse>(harness, member.token)
				.post(`/channels/${channelId}/messages`)
				.body({content: 'hello'})
				.expect(403)
				.executeWithResponse(),
		);
	});

	it('blocks adding reactions', async () => {
		const message = await sendChannelMessage(harness, owner.token, channelId, 'react to me');
		await setFlags(harness, member.userId, ['ACCOUNT_LIMITED']);
		await expectLimited(
			createBuilder<ErrorResponse>(harness, member.token)
				.put(`/channels/${channelId}/messages/${message.id}/reactions/%F0%9F%91%8D/@me`)
				.expect(403)
				.executeWithResponse(),
		);
	});

	it('blocks joining a guild through an invite but not rejoining one it is in', async () => {
		const outsider = await createTestAccount(harness);
		await setFlags(harness, outsider.userId, ['ACCOUNT_LIMITED']);
		await setFlags(harness, member.userId, ['ACCOUNT_LIMITED']);
		const invite = await createChannelInvite(harness, owner.token, channelId);
		await expectLimited(
			createBuilder<ErrorResponse>(harness, outsider.token)
				.post(`/invites/${invite.code}`)
				.body(null)
				.expect(403)
				.executeWithResponse(),
		);
		await createBuilder(harness, member.token).post(`/invites/${invite.code}`).body(null).expect(200).execute();
	});

	it('blocks profile changes others see but allows private account fields', async () => {
		await setFlags(harness, member.userId, ['ACCOUNT_LIMITED']);
		await expectLimited(
			createBuilder<ErrorResponse>(harness, member.token)
				.patch('/users/@me')
				.body({bio: 'blocked'})
				.expect(403)
				.executeWithResponse(),
		);
		await expectLimited(
			createBuilder<ErrorResponse>(harness, member.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({nick: 'blocked'})
				.expect(403)
				.executeWithResponse(),
		);
		await createBuilder(harness, member.token)
			.patch('/users/@me')
			.body({has_dismissed_premium_onboarding: true})
			.expect(200)
			.execute();
	});

	it('never limits exempt accounts', async () => {
		await setFlags(harness, member.userId, ['ACCOUNT_LIMITED', 'LIMIT_EXEMPT']);
		const me = await createBuilder<{account_limited?: boolean}>(harness, member.token).get('/users/@me').execute();
		expect(me.account_limited).toBe(false);
		await sendChannelMessage(harness, member.token, channelId, 'still here');
		await createBuilder(harness, member.token).patch('/users/@me').body({bio: 'allowed'}).expect(200).execute();
	});
});
