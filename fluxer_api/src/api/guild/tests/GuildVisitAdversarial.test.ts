// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {sendChannelMessage} from '@app/api/channel/tests/ChannelTestUtils';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface Registered {
	token: string;
	user_id: string;
}

interface Scene {
	owner: TestAccount;
	guildId: string;
	channelId: string;
	code: string;
}

const SHARED_IP = '198.51.100.77';

describe('Guild visits under adversarial use', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		await getInstanceConfigRepository().updateGuildVisitsConfig({enabled: true, user_basis_points: 10000});
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function createScene(inviteBody: Record<string, unknown> = {}): Promise<Scene> {
		const owner = await createTestAccount(harness);
		const guild = await createBuilder<GuildResponse>(harness, owner.token)
			.post('/guilds')
			.body({name: 'Adversarial'})
			.execute();
		const channelId = guild.system_channel_id!;
		await sendChannelMessage(harness, owner.token, channelId, 'members only chatter');
		const invite = await createBuilder<{code: string}>(harness, owner.token)
			.post(`/channels/${channelId}/invites`)
			.body(inviteBody)
			.execute();
		return {owner, guildId: guild.id, channelId, code: invite.code};
	}

	async function createUnclaimed(username: string, ip?: string): Promise<Registered> {
		const builder = createBuilderWithoutAuth<Registered>(harness)
			.post('/auth/register')
			.body({username, consent: true, date_of_birth: '1990-01-01'});
		if (ip) builder.header('x-forwarded-for', ip);
		return await builder.execute();
	}

	it('keeps a limited invite usable when one account visits and leaves in a loop', async () => {
		const {guildId, code} = await createScene({max_uses: 3});
		const looper = await createTestAccount(harness);
		for (let round = 0; round < 3; round++) {
			await createBuilder(harness, looper.token).post(`/invites/${code}/visit`).body(null).execute();
			await createBuilder(harness, looper.token)
				.delete(`/users/@me/guild-visits/${guildId}`)
				.expect(HTTP_STATUS.NO_CONTENT)
				.execute();
		}
		const newcomer = await createTestAccount(harness);
		await createBuilder(harness, newcomer.token).post(`/invites/${code}`).body(null).execute();
	});

	it('refuses unclaimed accounts a full join on the enforced tier whatever the client claims', async () => {
		await getInstanceConfigRepository().updateAccountsConfig({interaction_tier: 'enforced'});
		const {owner, guildId, code} = await createScene();
		const before = await createBuilder<GuildResponse>(harness, owner.token).get(`/guilds/${guildId}`).execute();
		const unclaimed = await createUnclaimed('squatter');
		const joined = await createBuilder(harness, unclaimed.token).post(`/invites/${code}`).body(null).executeRaw();
		expect(joined.response.status).toBe(HTTP_STATUS.FORBIDDEN);
		const after = await createBuilder<GuildResponse>(harness, owner.token).get(`/guilds/${guildId}`).execute();
		expect(after.member_count).toBe(before.member_count);
	});

	it('stops REST reads by a visitor whose address was banned through another account', async () => {
		const {owner, guildId, channelId, code} = await createScene();
		const alt = await createUnclaimed('lurkalt', SHARED_IP);
		await createBuilder(harness, alt.token)
			.post(`/invites/${code}/visit`)
			.header('x-forwarded-for', SHARED_IP)
			.body(null)
			.execute();
		const main = await createUnclaimed('lurkmain', SHARED_IP);
		await createBuilder(harness, owner.token)
			.put(`/guilds/${guildId}/bans/${main.user_id}`)
			.body({delete_message_days: 0})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await createBuilder(harness, alt.token)
			.post(`/invites/${code}/visit`)
			.header('x-forwarded-for', SHARED_IP)
			.body(null)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
		const read = await createBuilder(harness, alt.token)
			.get(`/channels/${channelId}/messages`)
			.header('x-forwarded-for', SHARED_IP)
			.executeRaw();
		expect(read.response.status).not.toBe(HTTP_STATUS.OK);
	});
});
