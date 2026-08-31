// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';
import type {NatsConnection} from 'nats';
import {describe, expect, it} from 'vitest';
import {createChannelID, createMessageID, createUserID} from '../../../BrandedTypes';
import {Message} from '../../../models/Message';
import {MessageResponseDataService} from './MessageResponseDataService';

const encoder = new TextEncoder();
const decoder = new TextDecoder();
const ROUTER_SHARD_REQUEST_TIMEOUT_MS = 5000;

class FakeConnectionManager implements INatsConnectionManager {
	readonly payloads: Array<Record<string, unknown>> = [];
	readonly timeouts: Array<number | undefined> = [];

	async connect(): Promise<void> {}

	async drain(): Promise<void> {}

	isClosed(): boolean {
		return false;
	}

	getConnection(): NatsConnection {
		return {
			request: async (_subject: string, data: Uint8Array, options?: {timeout?: number}) => {
				this.payloads.push(JSON.parse(decoder.decode(data)) as Record<string, unknown>);
				this.timeouts.push(options?.timeout);
				return {
					data: encoder.encode(
						JSON.stringify({
							FoundApi: {
								id: '2',
								channel_id: '1',
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
							},
						}),
					),
				};
			},
		} as unknown as NatsConnection;
	}
}

function makeMessage(): Message {
	return new Message({
		channel_id: createChannelID(1n),
		bucket: 0,
		message_id: createMessageID(2n),
		author_id: createUserID(3n),
		type: MessageTypes.DEFAULT,
		webhook_id: null,
		webhook_name: null,
		webhook_avatar_hash: null,
		content: '',
		edited_timestamp: null,
		pinned_timestamp: null,
		flags: 0,
		mention_everyone: false,
		mention_users: null,
		mention_roles: null,
		mention_channels: null,
		attachments: null,
		embeds: null,
		poll: null,
		sticker_items: null,
		message_reference: null,
		message_snapshots: null,
		call: null,
		nsfw_emojis: null,
		has_reaction: null,
		version: 1,
	});
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
});
