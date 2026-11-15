// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import {setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import {threadsRequest} from '@app/api/channel/tests/ThreadTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

describe.each(['mentions', 'saved-messages'])('%s pagination', (collection) => {
	let harness: ApiTestHarness;
	let users: UserRepository;

	beforeEach(async () => {
		harness = await createApiTestHarness();
		users = new UserRepository();
	});

	afterEach(async () => {
		await harness.shutdown();
	});

	async function world() {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const [member] = members;
		assert(member, 'Expected a guild member');
		return {owner, member, guild, channel: systemChannel};
	}

	async function send(account: TestAccount, channelId: string) {
		return threadsRequest<MessageResponse>(harness, account.token)
			.post(`/channels/${channelId}/messages`)
			.body({content: 'Stored message'})
			.expect(HttpStatus.OK)
			.execute();
	}

	async function thread(account: TestAccount, channelId: string) {
		return threadsRequest<ThreadChannelResponse>(harness, account.token)
			.post(`/channels/${channelId}/threads`)
			.body({name: 'Saved thread', type: ChannelTypes.PUBLIC_THREAD})
			.expect(HttpStatus.CREATED)
			.execute();
	}

	async function seed(account: TestAccount, guildId: string, messages: Array<MessageResponse>) {
		const userId = createUserID(BigInt(account.userId));
		if (collection === 'mentions') {
			await users.createRecentMentions(
				messages.map((message) => ({
					user_id: userId,
					channel_id: createChannelID(BigInt(message.channel_id)),
					message_id: createMessageID(BigInt(message.id)),
					guild_id: createGuildID(BigInt(guildId)),
					is_everyone: false,
					is_role: false,
				})),
			);
			return;
		}
		for (const message of messages) {
			await users.createSavedMessage(
				userId,
				createChannelID(BigInt(message.channel_id)),
				createMessageID(BigInt(message.id)),
			);
		}
	}

	async function list(account: TestAccount, options: {limit: number; before?: string; capable?: boolean}) {
		const query = new URLSearchParams({limit: String(options.limit)});
		if (options.before !== undefined) {
			query.set('before', options.before);
		}
		const entries = await threadsRequest<Array<{id: string}>>(harness, account.token, options)
			.get(`/users/@me/${collection}?${query}`)
			.expect(HttpStatus.OK)
			.execute();
		return entries.map((entry) => entry.id);
	}

	it('returns the newest page and continues strictly before its cursor', async () => {
		const w = await world();
		const oldest = await send(w.owner, w.channel.id);
		const middle = await send(w.owner, w.channel.id);
		const newest = await send(w.owner, w.channel.id);
		await seed(w.member, w.guild.id, [middle, newest, oldest]);
		expect(await list(w.member, {limit: 2})).toEqual([newest.id, middle.id]);
		expect(await list(w.member, {limit: 2, before: middle.id})).toEqual([oldest.id]);
	});

	it('stops after four hidden pages and can reach older content with an explicit cursor', async () => {
		const w = await world();
		const t = await thread(w.owner, w.channel.id);
		const visible = await send(w.owner, w.channel.id);
		const firstHidden = await send(w.owner, t.id);
		const secondHidden = await send(w.owner, t.id);
		const thirdHidden = await send(w.owner, t.id);
		const fourthHidden = await send(w.owner, t.id);
		await seed(w.member, w.guild.id, [visible, thirdHidden, firstHidden, fourthHidden, secondHidden]);
		expect(await list(w.member, {limit: 1, capable: false})).toEqual([]);
		expect(await list(w.member, {limit: 1, before: firstHidden.id, capable: false})).toEqual([visible.id]);
	});
});
