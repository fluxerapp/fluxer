// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import {resetApiServicesForTesting} from '@app/api/CreateApiContext';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {
	createChannel,
	createDmChannel,
	createFriendship,
	createPermissionOverwrite,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {threadsRequest} from '@app/api/channel/tests/ThreadTestUtils';
import type {MessageRow} from '@app/api/database/types/MessageTypes';
import {setInjectedGatewayService} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {ChannelTypes, MessageReferenceTypes, MessageTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {ServerMessageFlags} from '@fluxer/constants/src/ThreadConstants';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {MessageRequestSchemaType} from '@fluxer/schema/src/domains/message/MessageRequestSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {SavedMessageEntryListResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';

type PresenceDispatch = Parameters<NoopGatewayService['dispatchPresence']>[0];

class RecordingGateway extends NoopGatewayService {
	readonly dispatches: Array<PresenceDispatch> = [];

	override async dispatchPresence(params: PresenceDispatch): Promise<void> {
		this.dispatches.push(params);
	}

	savedPayloads(): Array<unknown> {
		return this.dispatches.filter((entry) => entry.event === 'SAVED_MESSAGE_CREATE').map((entry) => entry.data);
	}
}

describe('user content routes', () => {
	let harness: ApiTestHarness;
	let gateway: RecordingGateway;
	let users: UserRepository;
	let channels: ChannelRepository;

	beforeEach(async () => {
		harness = await createApiTestHarness();
		gateway = new RecordingGateway();
		setInjectedGatewayService(gateway);
		resetApiServicesForTesting();
		users = new UserRepository();
		channels = new ChannelRepository();
	});

	afterEach(async () => {
		await harness.shutdown();
	});

	async function world() {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const [member] = members;
		assert(member, 'Expected a guild member');
		const other = await createChannel(harness, owner.token, guild.id, 'other');
		return {owner, member, guild, channel: systemChannel, other};
	}

	async function send(account: TestAccount, channelId: string, body: MessageRequestSchemaType = {}) {
		return threadsRequest<MessageResponse>(harness, account.token)
			.post(`/channels/${channelId}/messages`)
			.body({content: 'Stored message', ...body})
			.expect(HttpStatus.OK)
			.execute();
	}

	async function thread(account: TestAccount, channelId: string, messageId?: string) {
		const path = `/channels/${channelId}${messageId ? `/messages/${messageId}` : ''}/threads`;
		return threadsRequest<ThreadChannelResponse>(harness, account.token)
			.post(path)
			.body({name: 'Saved thread', type: ChannelTypes.PUBLIC_THREAD})
			.expect(HttpStatus.CREATED)
			.execute();
	}

	async function mentionRows(account: TestAccount, guildId: string, messages: Array<MessageResponse>) {
		await users.createRecentMentions(
			messages.map((message) => ({
				user_id: createUserID(BigInt(account.userId)),
				channel_id: createChannelID(BigInt(message.channel_id)),
				message_id: createMessageID(BigInt(message.id)),
				guild_id: createGuildID(BigInt(guildId)),
				is_everyone: false,
				is_role: false,
			})),
		);
	}

	async function savedRows(account: TestAccount, messages: Array<{id: string; channel_id: string}>) {
		for (const message of messages) {
			await users.createSavedMessage(
				createUserID(BigInt(account.userId)),
				createChannelID(BigInt(message.channel_id)),
				createMessageID(BigInt(message.id)),
			);
		}
	}

	function save(account: TestAccount, message: MessageResponse) {
		return threadsRequest(harness, account.token)
			.post('/users/@me/saved-messages')
			.body({channel_id: message.channel_id, message_id: message.id});
	}

	function listPath(path: string, options: {limit?: number}) {
		const query = new URLSearchParams();
		if (options.limit !== undefined) {
			query.set('limit', String(options.limit));
		}
		return `${path}?${query.toString()}`;
	}

	async function mentions(account: TestAccount, options: {limit?: number; capable?: boolean} = {}) {
		return threadsRequest<Array<MessageResponse>>(harness, account.token, options)
			.get(listPath('/users/@me/mentions', options))
			.expect(HttpStatus.OK)
			.execute();
	}

	async function saved(account: TestAccount, options: {limit?: number; capable?: boolean} = {}) {
		return threadsRequest<SavedMessageEntryListResponse>(harness, account.token, options)
			.get(listPath('/users/@me/saved-messages', options))
			.expect(HttpStatus.OK)
			.execute();
	}

	async function storedSavedIds(account: TestAccount) {
		return (await users.listSavedMessages(createUserID(BigInt(account.userId)))).map((entry) =>
			String(entry.messageId),
		);
	}

	async function deny(
		account: TestAccount,
		channelId: string,
		member: TestAccount,
		permission = Permissions.VIEW_CHANNEL,
	) {
		await createPermissionOverwrite(harness, account.token, channelId, member.userId, {
			type: 1,
			allow: '0',
			deny: permission.toString(),
		});
	}

	async function updateStoredMessage(
		message: MessageResponse,
		patch: Partial<Pick<MessageRow, 'flags' | 'type' | 'message_reference'>>,
	) {
		const stored = await channels.messages.getMessage(
			createChannelID(BigInt(message.channel_id)),
			createMessageID(BigInt(message.id)),
		);
		assert(stored, 'Expected a stored message');
		await channels.messages.upsertMessage({...stored.toRow(), ...patch}, stored.toRow());
	}

	it('omits mentions from denied and deleted channels while retaining accessible mentions', async () => {
		const w = await world();
		const gone = await createChannel(harness, w.owner.token, w.guild.id, 'removed');
		const visible = await send(w.owner, w.channel.id);
		const denied = await send(w.owner, w.other.id);
		const deleted = await send(w.owner, gone.id);
		await deny(w.owner, w.other.id, w.member);
		await threadsRequest(harness, w.owner.token).delete(`/channels/${gone.id}`).expect(HttpStatus.NO_CONTENT).execute();
		await mentionRows(w.member, w.guild.id, [visible, denied, deleted]);
		expect((await mentions(w.member)).map((message) => message.id)).toEqual([visible.id]);
	});

	it('refills hidden thread mentions without replacing ordinary denied mentions', async () => {
		const w = await world();
		const t = await thread(w.owner, w.channel.id);
		const first = await send(w.owner, w.channel.id);
		const second = await send(w.owner, w.channel.id);
		const denied = await send(w.owner, w.other.id);
		const third = await send(w.owner, w.channel.id);
		const hidden1 = await send(w.owner, t.id);
		const hidden2 = await send(w.owner, t.id);
		await deny(w.owner, w.other.id, w.member);
		await mentionRows(w.member, w.guild.id, [first, second, denied, third, hidden1, hidden2]);
		expect((await mentions(w.member, {limit: 4, capable: false})).map((message) => message.id)).toEqual([
			third.id,
			second.id,
			first.id,
		]);
	});

	it('keeps every saved reference to an unreachable channel', async () => {
		const w = await world();
		const visible = await send(w.owner, w.channel.id);
		const first = await send(w.owner, w.other.id);
		const second = await send(w.owner, w.other.id);
		await savedRows(w.member, [visible, first, second]);
		await deny(w.owner, w.other.id, w.member);
		const result = await saved(w.member);
		expect(result.map((entry) => [entry.message_id, entry.status, entry.message !== null])).toEqual([
			[second.id, 'missing_permissions', false],
			[first.id, 'missing_permissions', false],
			[visible.id, 'available', true],
		]);
		expect(await storedSavedIds(w.member)).toEqual([second.id, first.id, visible.id]);
	});

	it('refills saved thread entries and includes ordinary missing-permission entries', async () => {
		const w = await world();
		const t = await thread(w.owner, w.channel.id);
		const first = await send(w.owner, w.channel.id);
		const denied = await send(w.owner, w.other.id);
		const second = await send(w.owner, w.channel.id);
		const hidden1 = await send(w.owner, t.id);
		const hidden2 = await send(w.owner, t.id);
		const third = await send(w.owner, w.channel.id);
		await savedRows(w.member, [first, denied, second, hidden1, hidden2, third]);
		await deny(w.owner, w.other.id, w.member);
		expect(
			(await saved(w.member, {limit: 4, capable: false})).map((entry) => [entry.message_id, entry.status]),
		).toEqual([
			[third.id, 'available'],
			[second.id, 'available'],
			[denied.id, 'missing_permissions'],
			[first.id, 'available'],
		]);
	});

	it('publishes a plain saved guild message without thread scoping', async () => {
		const w = await world();
		const message = await send(w.owner, w.channel.id);
		gateway.dispatches.length = 0;
		await save(w.owner, message).expect(HttpStatus.NO_CONTENT).execute();
		expect(gateway.savedPayloads()).toEqual([expect.objectContaining({id: message.id, flags: 0})]);
		const [payload] = gateway.savedPayloads();
		expect(payload).not.toHaveProperty('__thread_scoped');
		expect(payload).not.toHaveProperty('__thread_unscoped');
	});

	it('publishes a saved thread message only to thread viewers', async () => {
		const w = await world();
		const t = await thread(w.owner, w.channel.id);
		const message = await send(w.owner, t.id);
		gateway.dispatches.length = 0;
		await save(w.owner, message).expect(HttpStatus.NO_CONTENT).execute();
		expect(gateway.savedPayloads()).toEqual([
			expect.objectContaining({id: message.id, channel_id: t.id, __thread_scoped: w.guild.id}),
		]);
	});

	it('publishes a thread-created message without an unscoped copy', async () => {
		const w = await world();
		await thread(w.owner, w.channel.id);
		const parentMessages = await channels.messages.listMessages(createChannelID(BigInt(w.channel.id)));
		const created = parentMessages.find((message) => message.type === MessageTypes.THREAD_CREATED);
		assert(created, 'Expected the thread-created parent message');
		const message = await threadsRequest<MessageResponse>(harness, w.owner.token)
			.get(`/channels/${w.channel.id}/messages/${created.id}`)
			.execute();
		gateway.dispatches.length = 0;
		await save(w.owner, message).expect(HttpStatus.NO_CONTENT).execute();
		expect(gateway.savedPayloads()).toEqual([
			expect.objectContaining({id: message.id, type: MessageTypes.THREAD_CREATED, __thread_scoped: w.guild.id}),
		]);
	});

	it('publishes a thread parent flag to viewers and a masked copy to other sessions', async () => {
		const w = await world();
		const message = await send(w.owner, w.channel.id, {flags: 4});
		await thread(w.owner, w.channel.id, message.id);
		gateway.dispatches.length = 0;
		await save(w.owner, message).expect(HttpStatus.NO_CONTENT).execute();
		expect(gateway.savedPayloads()).toEqual([
			expect.objectContaining({id: message.id, flags: ServerMessageFlags.HAS_THREAD | 4, __thread_scoped: w.guild.id}),
			expect.objectContaining({id: message.id, flags: 4, __thread_unscoped: w.guild.id}),
		]);
	});

	async function maskingWorld() {
		const w = await world();
		const parent = await send(w.owner, w.channel.id, {flags: 4});
		await thread(w.owner, w.channel.id, parent.id);
		await thread(w.owner, w.other.id);
		const created = (await channels.messages.listMessages(createChannelID(BigInt(w.other.id)))).find(
			(message) => message.type === MessageTypes.THREAD_CREATED,
		);
		assert(created, 'Expected a thread-created message to reference');
		const reply = await send(w.owner, w.other.id);
		await updateStoredMessage(reply, {
			type: MessageTypes.REPLY,
			message_reference: {
				message_id: created.id,
				channel_id: createChannelID(BigInt(w.other.id)),
				guild_id: createGuildID(BigInt(w.guild.id)),
				type: MessageReferenceTypes.DEFAULT,
			},
		});
		const friend = await createTestAccount(harness);
		await createFriendship(harness, w.member, friend);
		const dm = await createDmChannel(harness, w.member.token, friend.userId);
		const direct = await send(w.member, dm.id);
		await updateStoredMessage(direct, {flags: ServerMessageFlags.HAS_THREAD});
		await savedRows(w.member, [parent, reply, direct]);
		return {w, parent, reply, direct};
	}

	it('masks a parent thread flag and thread-created reply for a non-viewer', async () => {
		const {w, parent, reply, direct} = await maskingWorld();
		const result = await saved(w.member, {capable: false});
		expect(result.map((entry) => [entry.message_id, entry.message?.flags])).toEqual([
			[direct.id, 0],
			[reply.id, 0],
			[parent.id, 4],
		]);
		expect(result.find((entry) => entry.message_id === reply.id)?.message?.referenced_message).toBeNull();
	});

	it('preserves guild thread artifacts for a capable viewer and still masks a direct message', async () => {
		const {w, parent, reply, direct} = await maskingWorld();
		const result = await saved(w.member);
		expect(result.map((entry) => [entry.message_id, entry.message?.flags])).toEqual([
			[direct.id, 0],
			[reply.id, 0],
			[parent.id, ServerMessageFlags.HAS_THREAD | 4],
		]);
		expect(result.find((entry) => entry.message_id === reply.id)?.message?.referenced_message).toMatchObject({
			type: MessageTypes.THREAD_CREATED,
		});
	});
});
