// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes, MessageEmbedTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';
import type {NatsConnection} from 'nats';
import {afterEach, describe, expect, it} from 'vitest';
import {createChannelID, createGuildID, createMessageID, createUserID} from '../../BrandedTypes';
import {
	MessageResponseDataService,
	setInjectedMessageResponseDataService,
} from '../../channel/services/message/MessageResponseDataService';
import type {MessageEmbed} from '../../database/types/MessageTypes';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import {Channel} from '../../models/Channel';
import {Message} from '../../models/Message';
import {dispatchEmbedUpdate} from './ExtractEmbeds';

const decoder = new TextDecoder();
const encoder = new TextEncoder();

// Captures the serialized broadcast request the worker hands to the
// out-of-process message-response service. messages[0].embeds is exactly the
// embed array the realtime MESSAGE_UPDATE will carry to connected clients.
class CapturingConnectionManager implements INatsConnectionManager {
	readonly payloads: Array<Record<string, unknown>> = [];

	async connect(): Promise<void> {}

	async drain(): Promise<void> {}

	isClosed(): boolean {
		return false;
	}

	getConnection(): NatsConnection {
		return {
			request: async (_subject: string, data: Uint8Array) => {
				this.payloads.push(JSON.parse(decoder.decode(data)) as Record<string, unknown>);
				return {
					data: encoder.encode(
						JSON.stringify({
							FoundApi: {
								id: '2',
								channel_id: '1',
								author: {id: '3', username: 'author', discriminator: '0001', avatar: null, flags: 0},
								type: MessageTypes.DEFAULT,
								flags: 0,
								content: 'see https://example.com',
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

function richEmbed(title: string): MessageEmbed {
	return {
		type: MessageEmbedTypes.RICH,
		title,
		description: null,
		url: null,
		timestamp: null,
		color: null,
		author: null,
		provider: null,
		thumbnail: null,
		image: null,
		video: null,
		audio: null,
		footer: null,
		fields: null,
		html: null,
		html_width: null,
		html_height: null,
		nsfw: null,
		children: null,
	};
}

function urlEmbed(url: string): MessageEmbed {
	return {
		type: MessageEmbedTypes.LINK,
		title: null,
		description: null,
		url,
		timestamp: null,
		color: null,
		author: null,
		provider: null,
		thumbnail: null,
		image: null,
		video: null,
		audio: null,
		footer: null,
		fields: null,
		html: null,
		html_width: null,
		html_height: null,
		nsfw: null,
		children: null,
	};
}

function makeMessage(embeds: Array<MessageEmbed> | null): Message {
	return new Message({
		channel_id: createChannelID(1n),
		bucket: 0,
		message_id: createMessageID(2n),
		author_id: createUserID(3n),
		type: MessageTypes.DEFAULT,
		webhook_id: null,
		webhook_name: null,
		webhook_avatar_hash: null,
		content: 'see https://example.com',
		edited_timestamp: null,
		pinned_timestamp: null,
		flags: 0,
		mention_everyone: false,
		mention_users: null,
		mention_roles: null,
		mention_channels: null,
		attachments: null,
		embeds,
		sticker_items: null,
		message_reference: null,
		message_snapshots: null,
		call: null,
		nsfw_emojis: null,
		has_reaction: null,
		version: 1,
	});
}

function makeChannel(): Channel {
	return new Channel({
		channel_id: createChannelID(1n),
		guild_id: null,
		type: ChannelTypes.DM,
		name: null,
		topic: null,
		icon_hash: null,
		url: null,
		parent_id: null,
		position: 0,
		owner_id: null,
		recipient_ids: new Set([createUserID(3n), createUserID(4n)]),
		nsfw: false,
		content_warning_level: 0,
		content_warning_text: null,
		rate_limit_per_user: 0,
		bitrate: 0,
		user_limit: 0,
		voice_connection_limit: null,
		rtc_region: null,
		last_message_id: null,
		last_pin_timestamp: null,
		permission_overwrites: null,
		nicks: null,
		soft_deleted: false,
		indexed_at: null,
		version: 1,
	});
}

function capturingGatewayService(): {gatewayService: IGatewayService; captured: () => Array<unknown>} {
	const captured: Array<unknown> = [];
	const gatewayService = {
		async dispatchGuild(params: {data: unknown}) {
			captured.push(params.data);
		},
		async dispatchChannel(params: {data: unknown}) {
			captured.push(params.data);
		},
	} as never as IGatewayService;
	return {gatewayService, captured: () => captured};
}

describe('ExtractEmbeds.dispatchEmbedUpdate', () => {
	afterEach(() => {
		setInjectedMessageResponseDataService(undefined);
	});

	it('broadcasts the merged rich-first embeds, not the url-only unfurl set', async () => {
		const connectionManager = new CapturingConnectionManager();
		setInjectedMessageResponseDataService(new MessageResponseDataService(connectionManager));

		// latestMessage carries exactly the merged set updateMessageEmbeds
		// persisted: the author rich embed first, then the freshly unfurled URL
		// embed. dispatchEmbedUpdate must broadcast this verbatim.
		const latestMessage = makeMessage([richEmbed('kept'), urlEmbed('https://example.com/')]);
		const {gatewayService} = capturingGatewayService();

		// guildId set + channel.guildId null drives the dispatchGuild branch,
		// which is the real broadcast path used for guild-channel embed updates.
		await dispatchEmbedUpdate({
			latestMessage,
			channel: makeChannel(),
			guildId: createGuildID(10n),
			gatewayService,
		});

		// The serialized request handed to the response service is the realtime
		// MESSAGE_UPDATE payload source. Its embeds MUST carry the rich embed
		// rich-first, identical to what updateMessageEmbeds persisted.
		const request = connectionManager.payloads[0];
		const broadcastMessage = request?.message as {embeds?: Array<{type: string; title?: string | null}>};
		const broadcastEmbeds = broadcastMessage?.embeds ?? [];
		const broadcastTypes = broadcastEmbeds.map((e) => e.type);
		expect(broadcastTypes).toContain(MessageEmbedTypes.RICH);
		expect(broadcastTypes).toContain(MessageEmbedTypes.LINK);
		expect(broadcastTypes.indexOf(MessageEmbedTypes.RICH)).toBeLessThan(broadcastTypes.indexOf(MessageEmbedTypes.LINK));
		const richTitle = broadcastEmbeds.find((e) => e.type === MessageEmbedTypes.RICH)?.title;
		expect(richTitle).toBe('kept');
	});
});
