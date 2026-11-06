// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createEmojiID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import {sendChannelMessage, setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import {createEmoji, getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import {getGatewayService} from '@app/api/middleware/ServiceRegistry';
import {getChannelRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, type MockInstance, vi} from 'vitest';

interface ReactionUsersPage {
	items: Array<{id: string}>;
}

interface Setup {
	owner: TestAccount;
	guildId: string;
	reactors: [TestAccount, TestAccount];
	channelId: string;
	messageId: string;
	emoji: {id: string; name: string};
}

const CUSTOM_FORMS: Array<[string, (id: string) => string]> = [
	['coin:id', (id) => `coin:${id}`],
	[':coin:id', (id) => `:coin:${id}`],
	['a:coin:id', (id) => `a:coin:${id}`],
	['<:coin:id>', (id) => `<:coin:${id}>`],
	['<a:coin:id>', (id) => `<a:coin:${id}>`],
	[':id', (id) => `:${id}`],
];

describe('Reaction emoji matching', () => {
	let harness: ApiTestHarness;
	let dispatchGuild: MockInstance<IGatewayService['dispatchGuild']>;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		dispatchGuild = vi.spyOn(getGatewayService(), 'dispatchGuild');
	});

	afterEach(() => {
		dispatchGuild.mockRestore();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function setup(): Promise<Setup> {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 2);
		const emoji = await createEmoji(harness, owner.token, guild.id, {name: 'coin', image: getPngDataUrl()});
		const message = await sendChannelMessage(harness, owner.token, systemChannel.id, 'react to me');
		return {
			owner,
			guildId: guild.id,
			reactors: members as [TestAccount, TestAccount],
			channelId: systemChannel.id,
			messageId: message.id,
			emoji: {id: emoji.id, name: emoji.name},
		};
	}

	function reactionsPath(s: Setup, emoji: string): string {
		return `/channels/${s.channelId}/messages/${s.messageId}/reactions/${encodeURIComponent(emoji)}`;
	}

	async function react(s: Setup, account: TestAccount, emoji: string): Promise<void> {
		await createBuilder(harness, account.token)
			.put(`${reactionsPath(s, emoji)}/@me`)
			.body(null)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
	}

	async function remove(account: TestAccount, path: string): Promise<void> {
		await createBuilder(harness, account.token).delete(path).expect(HTTP_STATUS.NO_CONTENT).execute();
	}

	async function messageReactions(s: Setup): Promise<MessageResponse['reactions']> {
		const message = await createBuilder<MessageResponse>(harness, s.owner.token)
			.get(`/channels/${s.channelId}/messages/${s.messageId}`)
			.execute();
		return message.reactions ?? [];
	}

	async function reactorIds(s: Setup, emoji: string): Promise<Array<string>> {
		const page = await createBuilder<ReactionUsersPage>(harness, s.owner.token)
			.get(`${reactionsPath(s, emoji)}/users`)
			.execute();
		return page.items.map((user) => user.id).sort();
	}

	function dispatchedEmoji(event: string): unknown {
		const call = dispatchGuild.mock.calls.find(([params]) => params.event === event);
		return (call?.[0].data as {emoji?: unknown} | undefined)?.emoji;
	}

	async function renameEmojiToBean(s: Setup): Promise<void> {
		await createBuilder(harness, s.owner.token)
			.patch(`/guilds/${s.guildId}/emojis/${s.emoji.id}`)
			.body({name: 'bean'})
			.execute();
	}

	it.each(CUSTOM_FORMS)('clears every reaction for a custom emoji requested as %s', async (_label, form) => {
		const s = await setup();
		const [first, second] = s.reactors;
		await react(s, first, `coin:${s.emoji.id}`);
		await react(s, second, `coin:${s.emoji.id}`);

		await remove(s.owner, reactionsPath(s, form(s.emoji.id)));

		expect(await messageReactions(s)).toEqual([]);
		expect(await reactorIds(s, `coin:${s.emoji.id}`)).toEqual([]);
		expect(dispatchedEmoji('MESSAGE_REACTION_REMOVE_EMOJI')).toEqual({id: s.emoji.id, name: 'coin', animated: false});
	});

	it.each(CUSTOM_FORMS)('removes one user reaction for a custom emoji requested as %s', async (_label, form) => {
		const s = await setup();
		const [first, second] = s.reactors;
		const requested = form(s.emoji.id);
		await react(s, first, `coin:${s.emoji.id}`);
		await react(s, second, `coin:${s.emoji.id}`);

		await remove(first, `${reactionsPath(s, requested)}/@me`);
		expect(dispatchedEmoji('MESSAGE_REACTION_REMOVE')).toEqual({id: s.emoji.id, name: 'coin', animated: false});
		await remove(s.owner, `${reactionsPath(s, requested)}/${second.userId}`);

		expect(await messageReactions(s)).toEqual([]);
	});

	it.each(CUSTOM_FORMS)('lists and joins the existing custom emoji reaction requested as %s', async (_label, form) => {
		const s = await setup();
		const [first, second] = s.reactors;
		const requested = form(s.emoji.id);
		await react(s, first, `coin:${s.emoji.id}`);

		await react(s, second, requested);
		await react(s, second, requested);

		expect(await messageReactions(s)).toEqual([{emoji: {id: s.emoji.id, name: 'coin'}, count: 2}]);
		expect(await reactorIds(s, requested)).toEqual([first.userId, second.userId].sort());
		expect(dispatchedEmoji('MESSAGE_REACTION_ADD')).toEqual({id: s.emoji.id, name: 'coin', animated: false});
	});

	it('names a new custom emoji reaction after the emoji, not the request', async () => {
		const s = await setup();
		const [first, second] = s.reactors;
		await react(s, first, `:coin:${s.emoji.id}`);
		await react(s, second, `coin:${s.emoji.id}`);

		expect(await messageReactions(s)).toEqual([{emoji: {id: s.emoji.id, name: 'coin'}, count: 2}]);
	});

	it('keeps one reaction per custom emoji across a rename', async () => {
		const s = await setup();
		const [first, second] = s.reactors;
		await react(s, first, `coin:${s.emoji.id}`);
		await renameEmojiToBean(s);
		await react(s, second, `bean:${s.emoji.id}`);

		expect(await messageReactions(s)).toEqual([{emoji: {id: s.emoji.id, name: 'coin'}, count: 2}]);
		await remove(s.owner, reactionsPath(s, `bean:${s.emoji.id}`));

		expect(await messageReactions(s)).toEqual([]);
	});

	it('treats rows stored under an old emoji name as the same reaction', async () => {
		const s = await setup();
		const [first, second] = s.reactors;
		await react(s, first, `coin:${s.emoji.id}`);
		await getChannelRepository().messageInteractions.addReaction(
			createChannelID(BigInt(s.channelId)),
			createMessageID(BigInt(s.messageId)),
			createUserID(BigInt(second.userId)),
			'bean',
			createEmojiID(BigInt(s.emoji.id)),
		);

		expect(await messageReactions(s)).toEqual([{emoji: {id: s.emoji.id, name: 'bean'}, count: 2}]);
		expect(await reactorIds(s, `coin:${s.emoji.id}`)).toEqual([first.userId, second.userId].sort());
		await remove(second, `${reactionsPath(s, `coin:${s.emoji.id}`)}/@me`);
		expect(await reactorIds(s, `coin:${s.emoji.id}`)).toEqual([first.userId]);
		await react(s, second, `coin:${s.emoji.id}`);
		await remove(s.owner, reactionsPath(s, `coin:${s.emoji.id}`));

		expect(await messageReactions(s)).toEqual([]);
	});

	it('counts each distinct emoji once, up to the given limit', async () => {
		const s = await setup();
		const [first, second] = s.reactors;
		const other = await createEmoji(harness, s.owner.token, s.guildId, {name: 'bean', image: getPngDataUrl()});
		await react(s, first, '\u{1F525}');
		await react(s, second, '\u{1F44D}');
		await react(s, first, `coin:${s.emoji.id}`);
		await react(s, second, `bean:${other.id}`);
		const repository = getChannelRepository().messageInteractions;
		const channelId = createChannelID(BigInt(s.channelId));
		const messageId = createMessageID(BigInt(s.messageId));
		await repository.addReaction(
			channelId,
			messageId,
			createUserID(BigInt(second.userId)),
			'old',
			createEmojiID(BigInt(s.emoji.id)),
		);

		expect(await repository.countUniqueReactions(channelId, messageId, 30)).toBe(4);
		expect(await repository.countUniqueReactions(channelId, messageId, 3)).toBe(3);
	});

	it.each([
		['with U+FE0F', '\u2764\uFE0F'],
		['with a skin tone', '\u{1F44D}\u{1F3FD}'],
	])('removes a unicode reaction %s', async (_label, emoji) => {
		const s = await setup();
		const [first, second] = s.reactors;
		await react(s, first, emoji);
		await react(s, second, emoji);

		await remove(second, `${reactionsPath(s, emoji)}/@me`);
		expect(await reactorIds(s, emoji)).toEqual([first.userId]);
		await remove(s.owner, reactionsPath(s, emoji));

		expect(await messageReactions(s)).toEqual([]);
	});

	it('keeps unicode reactions with and without U+FE0F apart', async () => {
		const s = await setup();
		const [first, second] = s.reactors;
		await react(s, first, '\u2764\uFE0F');
		await react(s, second, '\u2764');

		await remove(s.owner, reactionsPath(s, '\u2764'));

		expect(await messageReactions(s)).toEqual([{emoji: {id: null, name: '\u2764\uFE0F'}, count: 1}]);
	});
});
