// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {authorizeBot, createTestBotAccount} from '@app/api/bot/tests/BotTestUtils';
import {
	acceptInvite,
	createChannelInvite,
	createDmChannel,
	createFriendship,
	createGuild,
	getChannel,
	type MinimalChannelResponse,
	sendChannelMessage,
} from '@app/api/channel/tests/ChannelTestUtils';
import {updateGuild} from '@app/api/guild/tests/GuildTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, type TestRequestBuilder} from '@app/api/test/TestRequestBuilder';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Community member DMs setting', () => {
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

	async function createSharedGuild(owner: TestAccount, members: Array<TestAccount>, name: string) {
		const guild = await createGuild(harness, owner.token, name);
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		for (const member of members) {
			await acceptInvite(harness, member.token, invite.code);
		}
		return guild;
	}

	async function turnOffMemberDms(owner: TestAccount, guild: GuildResponse): Promise<void> {
		const updated = await updateGuild(harness, owner.token, guild.id, {
			features: [...guild.features, GuildFeatures.MEMBER_DMS_DISABLED],
		});
		expect(updated.features).toContain(GuildFeatures.MEMBER_DMS_DISABLED);
	}

	async function createMembers(): Promise<{owner: TestAccount; sender: TestAccount; target: TestAccount}> {
		const owner = await createTestAccount(harness);
		const sender = await createTestAccount(harness);
		const target = await createTestAccount(harness);
		await ensureSessionStarted(harness, sender.token);
		await ensureSessionStarted(harness, target.token);
		return {owner, sender, target};
	}

	it('refuses a DM between non-friends whose only shared community turned member DMs off', async () => {
		const {owner, sender, target} = await createMembers();
		const guild = await createSharedGuild(owner, [sender, target], 'Cohort');
		await turnOffMemberDms(owner, guild);
		const channel = await createDmChannel(harness, sender.token, target.userId);
		await createBuilder(harness, sender.token)
			.post(`/channels/${channel.id}/messages`)
			.body({content: 'hello'})
			.expect(HTTP_STATUS.BAD_REQUEST, 'COMMUNITY_MEMBER_DMS_DISABLED')
			.execute();
		await createBuilder(harness, target.token)
			.post(`/channels/${channel.id}/messages`)
			.body({content: 'hello back'})
			.expect(HTTP_STATUS.BAD_REQUEST, 'COMMUNITY_MEMBER_DMS_DISABLED')
			.execute();
	});

	it('stops an existing DM once the community turns member DMs off', async () => {
		const {owner, sender, target} = await createMembers();
		const guild = await createSharedGuild(owner, [sender, target], 'Cohort');
		const channel = await createDmChannel(harness, sender.token, target.userId);
		const before = await sendChannelMessage(harness, sender.token, channel.id, 'before');
		await turnOffMemberDms(owner, guild);
		const actions: Array<[string, () => TestRequestBuilder<{code: string}>]> = [
			[
				'send',
				() =>
					createBuilder<{code: string}>(harness, sender.token)
						.post(`/channels/${channel.id}/messages`)
						.body({content: 'after'}),
			],
			[
				'pin',
				() => createBuilder<{code: string}>(harness, sender.token).put(`/channels/${channel.id}/pins/${before.id}`),
			],
			[
				'ring',
				() =>
					createBuilder<{code: string}>(harness, sender.token)
						.post(`/channels/${channel.id}/call/ring`)
						.body({recipients: [target.userId]}),
			],
		];
		for (const [name, request] of actions) {
			const {json} = await request().expect(HTTP_STATUS.BAD_REQUEST).executeWithResponse();
			expect(json.code, name).toBe('COMMUNITY_MEMBER_DMS_DISABLED');
		}
	});

	it('allows the DM when the members also share a community that allows it', async () => {
		const {owner, sender, target} = await createMembers();
		const guild = await createSharedGuild(owner, [sender, target], 'Cohort');
		await turnOffMemberDms(owner, guild);
		await createSharedGuild(sender, [target], 'Book club');
		const channel = await createDmChannel(harness, sender.token, target.userId);
		await sendChannelMessage(harness, sender.token, channel.id, 'from the book club');
	});

	it('allows the DM between friends', async () => {
		const {owner, sender, target} = await createMembers();
		const guild = await createSharedGuild(owner, [sender, target], 'Cohort');
		await turnOffMemberDms(owner, guild);
		await createFriendship(harness, sender, target);
		const channel = await createDmChannel(harness, sender.token, target.userId);
		await sendChannelMessage(harness, sender.token, channel.id, 'hi friend');
	});

	it('lets a bot in the community keep messaging members', async () => {
		const botAccount = await createTestBotAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, botAccount.ownerToken, 'Bot community');
		const systemChannel = await getChannel(harness, botAccount.ownerToken, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, botAccount.ownerToken, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);
		await authorizeBot(harness, botAccount.ownerToken, botAccount.appId, ['bot'], guild.id, '0');
		await updateGuild(harness, botAccount.ownerToken, guild.id, {
			features: [...guild.features, GuildFeatures.MEMBER_DMS_DISABLED],
		});
		const channel = await createBuilder<MinimalChannelResponse>(harness, `Bot ${botAccount.botToken}`)
			.post('/users/@me/channels')
			.body({recipient_id: member.userId})
			.execute();
		await sendChannelMessage(harness, `Bot ${botAccount.botToken}`, channel.id, 'welcome');
	});

	it('restores member DMs when the community turns the setting back off', async () => {
		const {owner, sender, target} = await createMembers();
		const guild = await createSharedGuild(owner, [sender, target], 'Cohort');
		await turnOffMemberDms(owner, guild);
		await updateGuild(harness, owner.token, guild.id, {features: guild.features});
		const channel = await createDmChannel(harness, sender.token, target.userId);
		await sendChannelMessage(harness, sender.token, channel.id, 'allowed again');
	});
});
