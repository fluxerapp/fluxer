// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';
import type {NatsConnection} from 'nats';
import {describe, expect, it} from 'vitest';
import {createChannelID, createGuildID, createMessageID, createUserID} from '../../../BrandedTypes';
import {Message} from '../../../models/Message';
import {MESSAGE_BUILD_BATCH_MAX_BYTES, MessageResponseDataService} from './MessageResponseDataService';

const encoder = new TextEncoder();
const decoder = new TextDecoder();
const ROUTER_SHARD_REQUEST_TIMEOUT_MS = 5000;

class FakeConnectionManager implements INatsConnectionManager {
	readonly payloads: Array<Record<string, unknown>> = [];
	readonly timeouts: Array<number | undefined> = [];
	readonly orphanedMessageIds = new Set<string>();

	async connect(): Promise<void> {}

	async drain(): Promise<void> {}

	isClosed(): boolean {
		return false;
	}

	getConnection(): NatsConnection {
		return {
			request: async (_subject: string, data: Uint8Array, options?: {timeout?: number}) => {
				const payload = JSON.parse(decoder.decode(data)) as Record<string, unknown>;
				this.payloads.push(payload);
				this.timeouts.push(options?.timeout);
				if (payload.op === 'BuildResponses') {
					const messages = (payload.messages as Array<{message_id: string; channel_id: string}>).filter(
						(message) => !this.orphanedMessageIds.has(message.message_id),
					);
					return {
						data: encoder.encode(
							JSON.stringify({
								FoundApiMany: messages.map((message) => fakeMessageResponse(message.message_id, message.channel_id)),
							}),
						),
					};
				}
				return {
					data: encoder.encode(JSON.stringify({FoundApi: fakeMessageResponse('2')})),
				};
			},
		} as unknown as NatsConnection;
	}
}

function fakeMessageResponse(messageId: string, channelId: string = '1'): Record<string, unknown> {
	return {
		id: messageId,
		channel_id: channelId,
		author: {id: '3', username: 'author', discriminator: '0001', avatar: null, flags: 0},
		type: MessageTypes.DEFAULT,
		flags: 0,
		content: '',
		timestamp: '2026-01-01T00:00:00.000Z',
		edited_timestamp: null,
		pinned: false,
		mention_everyone: false,
		tts: false,
		mentions: [],
		mention_roles: [],
		embeds: [],
		attachments: [],
		stickers: [],
	};
}

function makeMessage(messageId: bigint = 2n, content: string = '', channelId: bigint = 1n): Message {
	return new Message({
		channel_id: createChannelID(channelId),
		bucket: 0,
		message_id: createMessageID(messageId),
		author_id: createUserID(3n),
		type: MessageTypes.DEFAULT,
		webhook_id: null,
		webhook_name: null,
		webhook_avatar_hash: null,
		content,
		edited_timestamp: null,
		pinned_timestamp: null,
		flags: 0,
		mention_everyone: false,
		mention_users: null,
		mention_roles: null,
		mention_channels: null,
		attachments: null,
		embeds: null,
		sticker_items: null,
		message_reference: null,
		message_snapshots: null,
		call: null,
		nsfw_emojis: null,
		has_reaction: null,
		version: 1,
	});
}

const BASE_MESSAGE_ID = 1000000000000000000n;
const VIEWER_ID = createUserID(3n);
const ACCESS = {sourceGuildId: null, messageHistoryCutoff: null, canReadMessageHistory: true};

async function measureSerializedMessageBytes(): Promise<number> {
	const connectionManager = new FakeConnectionManager();
	const service = new MessageResponseDataService(connectionManager);
	await service.buildMessages({userId: VIEWER_ID, messages: [makeMessage(BASE_MESSAGE_ID)], access: ACCESS});
	const {messages} = connectionManager.payloads[0] as {messages: Array<unknown>};
	return Buffer.byteLength(JSON.stringify(messages[0]));
}

function makeSizedMessage(index: number, totalBytes: number, baseBytes: number, channelId: bigint = 1n): Message {
	return makeMessage(BASE_MESSAGE_ID + BigInt(index), 'a'.repeat(totalBytes - baseBytes), channelId);
}

function messageId(index: number): string {
	return (BASE_MESSAGE_ID + BigInt(index)).toString();
}

function batchSizes(connectionManager: FakeConnectionManager): Array<number> {
	return connectionManager.payloads.map((payload) => (payload as {messages: Array<unknown>}).messages.length);
}

describe('MessageResponseDataService', () => {
	it('omits reactions from broadcast message response requests', async () => {
		const connectionManager = new FakeConnectionManager();
		const service = new MessageResponseDataService(connectionManager);

		await service.buildBroadcastMessage({
			channel: {guildId: null},
			message: makeMessage(),
		});

		expect(connectionManager.payloads[0]).toMatchObject({
			op: 'BuildResponse',
			include_reactions: false,
			viewer_user_id: '3',
		});
	});

	it('keeps regular channel message responses reaction-aware by default', async () => {
		const connectionManager = new FakeConnectionManager();
		const service = new MessageResponseDataService(connectionManager);

		await service.buildMessageForChannel({
			channel: {guildId: null},
			message: makeMessage(),
		});

		expect(connectionManager.payloads[0]).toMatchObject({
			op: 'BuildResponse',
			include_reactions: true,
			viewer_user_id: '3',
		});
	});

	it('waits longer than the router shard timeout so the inner hop expires first', async () => {
		const connectionManager = new FakeConnectionManager();
		const service = new MessageResponseDataService(connectionManager);

		await service.buildMessageForChannel({
			channel: {guildId: null},
			message: makeMessage(),
		});

		expect(connectionManager.timeouts[0]).toBe(6000);
		expect(connectionManager.timeouts[0]).toBeGreaterThan(ROUTER_SHARD_REQUEST_TIMEOUT_MS);
	});

	it('sends no request when there are no messages to build', async () => {
		const connectionManager = new FakeConnectionManager();
		const service = new MessageResponseDataService(connectionManager);

		const responses = await service.buildMessages({userId: VIEWER_ID, messages: [], access: ACCESS});

		expect(responses).toEqual([]);
		expect(connectionManager.payloads).toEqual([]);
	});

	it('keeps a batch that exactly fills the byte budget in one request', async () => {
		const baseBytes = await measureSerializedMessageBytes();
		const connectionManager = new FakeConnectionManager();
		const service = new MessageResponseDataService(connectionManager);
		const messages = [
			makeSizedMessage(0, MESSAGE_BUILD_BATCH_MAX_BYTES / 2, baseBytes),
			makeSizedMessage(1, MESSAGE_BUILD_BATCH_MAX_BYTES / 2, baseBytes),
		];

		const responses = await service.buildMessages({userId: VIEWER_ID, messages, access: ACCESS});

		expect(batchSizes(connectionManager)).toEqual([2]);
		expect(responses.map((response) => response.id)).toEqual(messages.map((message) => message.id.toString()));
	});

	it('splits into ordered batches once one more byte would cross the budget', async () => {
		const baseBytes = await measureSerializedMessageBytes();
		const connectionManager = new FakeConnectionManager();
		const service = new MessageResponseDataService(connectionManager);
		const messages = [
			makeSizedMessage(0, MESSAGE_BUILD_BATCH_MAX_BYTES / 2, baseBytes),
			makeSizedMessage(1, MESSAGE_BUILD_BATCH_MAX_BYTES / 2 + 1, baseBytes),
			makeSizedMessage(2, baseBytes, baseBytes),
		];

		const responses = await service.buildMessages({userId: VIEWER_ID, messages, access: ACCESS});

		expect(batchSizes(connectionManager)).toEqual([1, 2]);
		expect(responses.map((response) => response.id)).toEqual(messages.map((message) => message.id.toString()));
	});

	it('sends a single message that exceeds the budget on its own', async () => {
		const baseBytes = await measureSerializedMessageBytes();
		const connectionManager = new FakeConnectionManager();
		const service = new MessageResponseDataService(connectionManager);
		const messages = [
			makeSizedMessage(0, MESSAGE_BUILD_BATCH_MAX_BYTES + 1, baseBytes),
			makeSizedMessage(1, baseBytes, baseBytes),
		];

		const responses = await service.buildMessages({userId: VIEWER_ID, messages, access: ACCESS});

		expect(batchSizes(connectionManager)).toEqual([1, 1]);
		expect(responses.map((response) => response.id)).toEqual(messages.map((message) => message.id.toString()));
	});

	it('drops a message the service leaves out instead of shifting later responses', async () => {
		const connectionManager = new FakeConnectionManager();
		connectionManager.orphanedMessageIds.add(messageId(1));
		const service = new MessageResponseDataService(connectionManager);
		const messages = [0, 1, 2].map((index) => makeMessage(BASE_MESSAGE_ID + BigInt(index)));

		const responses = await service.buildMessagesForChannels({
			userId: VIEWER_ID,
			messages,
			channelById: new Map([['1', {guildId: null}]]),
		});

		expect(responses.map((response) => response.id)).toEqual([messageId(0), messageId(2)]);
	});

	it('keeps responses in input order when channels resolve to different guilds', async () => {
		const connectionManager = new FakeConnectionManager();
		connectionManager.orphanedMessageIds.add(messageId(2));
		const service = new MessageResponseDataService(connectionManager);
		const messages = [
			makeMessage(BASE_MESSAGE_ID, '', 10n),
			makeMessage(BASE_MESSAGE_ID + 1n, '', 20n),
			makeMessage(BASE_MESSAGE_ID + 2n, '', 10n),
			makeMessage(BASE_MESSAGE_ID + 3n, '', 20n),
		];

		const responses = await service.buildMessagesForChannels({
			userId: VIEWER_ID,
			messages,
			channelById: new Map([
				['10', {guildId: createGuildID(100n)}],
				['20', {guildId: createGuildID(200n)}],
			]),
		});

		expect(batchSizes(connectionManager)).toEqual([2, 2]);
		expect(responses.map((response) => response.id)).toEqual([messageId(0), messageId(1), messageId(3)]);
	});

	it('keeps other guilds aligned when an earlier batch drops a message', async () => {
		const baseBytes = await measureSerializedMessageBytes();
		const connectionManager = new FakeConnectionManager();
		connectionManager.orphanedMessageIds.add(messageId(0));
		const service = new MessageResponseDataService(connectionManager);
		const messages = [
			makeSizedMessage(0, MESSAGE_BUILD_BATCH_MAX_BYTES / 2, baseBytes, 10n),
			makeMessage(BASE_MESSAGE_ID + 1n, '', 20n),
			makeSizedMessage(2, MESSAGE_BUILD_BATCH_MAX_BYTES / 2 + 1, baseBytes, 10n),
			makeSizedMessage(3, baseBytes, baseBytes, 10n),
		];

		const responses = await service.buildMessagesForChannels({
			userId: VIEWER_ID,
			messages,
			channelById: new Map([
				['10', {guildId: createGuildID(100n)}],
				['20', {guildId: createGuildID(200n)}],
			]),
		});

		expect(batchSizes(connectionManager).sort()).toEqual([1, 1, 2]);
		expect(responses.map((response) => response.id)).toEqual([messageId(1), messageId(2), messageId(3)]);
		expect(responses.map((response) => response.channel_id)).toEqual(['20', '10', '10']);
	});

	it('keeps a full page of ordinary pins in a single request', async () => {
		const connectionManager = new FakeConnectionManager();
		const service = new MessageResponseDataService(connectionManager);
		const messages = Array.from({length: 50}, (_unused, index) =>
			makeMessage(BASE_MESSAGE_ID + BigInt(index), 'a'.repeat(120)),
		);

		const responses = await service.buildMessages({userId: VIEWER_ID, messages, access: ACCESS});

		expect(batchSizes(connectionManager)).toEqual([50]);
		expect(responses.map((response) => response.id)).toEqual(messages.map((message) => message.id.toString()));
	});
});
