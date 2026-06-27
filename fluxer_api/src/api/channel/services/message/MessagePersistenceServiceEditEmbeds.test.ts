// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes, MessageEmbedTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';
import {createChannelID, createMessageID, createUserID} from '../../../BrandedTypes';
import type {MessageEmbed} from '../../../database/types/MessageTypes';
import type {EmbedService} from '../../../infrastructure/EmbedService';
import {Channel} from '../../../models/Channel';
import {Message} from '../../../models/Message';
import type {MessageUpdateRequest} from '../../MessageTypes';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {MessageContentService} from './MessageContentService';
import {MessagePersistenceService} from './MessagePersistenceService';

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

function makeMessage(params: {content: string; embeds: Array<MessageEmbed> | null}): Message {
	return new Message({
		channel_id: createChannelID(1n),
		bucket: 0,
		message_id: createMessageID(2n),
		author_id: createUserID(3n),
		type: MessageTypes.DEFAULT,
		webhook_id: null,
		webhook_name: null,
		webhook_avatar_hash: null,
		content: params.content,
		edited_timestamp: null,
		pinned_timestamp: null,
		flags: 0,
		mention_everyone: false,
		mention_users: null,
		mention_roles: null,
		mention_channels: null,
		attachments: null,
		embeds: params.embeds,
		sticker_items: null,
		message_reference: null,
		message_snapshots: null,
		call: null,
		nsfw_emojis: null,
		has_reaction: null,
		version: 1,
	});
}

function makeDmChannel(): Channel {
	return new Channel({
		channel_id: createChannelID(1n),
		type: ChannelTypes.DM,
		guild_id: null,
		name: null,
		topic: null,
		nsfw: false,
		position: null,
		parent_id: null,
		permission_overwrites: null,
		bitrate: null,
		user_limit: null,
		rate_limit_per_user: null,
		icon_hash: null,
		owner_id: null,
		application_id: null,
		flags: 0,
		default_auto_archive_duration: null,
		rtc_region: null,
		video_quality_mode: null,
		default_thread_rate_limit_per_user: null,
		default_reaction_emoji: null,
		default_sort_order: null,
		default_forum_layout: null,
		available_tags: null,
		applied_tags: null,
		member_count: null,
		message_count: null,
		total_message_sent: null,
		version: 1,
	} as never);
}

// Faithful stand-in for EmbedService.getInitialEmbeds: mirrors the real
// decision order -- explicit custom embeds win; otherwise derive URL embeds
// from the content; otherwise return null.
function fakeEmbedService(): EmbedService {
	return {
		async getInitialEmbeds(params: {content: string | null; customEmbeds?: Array<unknown>}) {
			if (params.customEmbeds?.length) {
				return {
					embeds: params.customEmbeds.map((c) => richEmbed((c as {title?: string}).title ?? 'custom')),
					hasUncachedUrls: false,
				};
			}
			if (params.content && /https?:\/\//.test(params.content)) {
				const url = params.content.match(/https?:\/\/\S+/)?.[0] ?? '';
				return {embeds: [urlEmbed(url)], hasUncachedUrls: false};
			}
			return {embeds: null, hasUncachedUrls: false};
		},
	} as never;
}

// channelRepository.messages.upsertMessage simply persists the new row and
// returns the resulting Message -- exactly what the real repository does.
function fakeChannelRepository(): IChannelRepositoryAggregate {
	return {
		messages: {
			async upsertMessage(row: ConstructorParameters<typeof Message>[0]) {
				return new Message(row);
			},
		},
	} as never;
}

function createService(): MessagePersistenceService {
	const service = new MessagePersistenceService(
		fakeChannelRepository(),
		{} as never,
		{} as never,
		{} as never,
		fakeEmbedService(),
		{} as never,
		{} as never,
		{} as never,
		{} as never,
		{} as never,
		{} as never,
		{} as never,
	);
	// Override the internally-constructed content service so the embed-merge
	// logic under test runs without reaching emoji/pack infrastructure.
	(service as unknown as {contentService: Partial<MessageContentService>}).contentService = {
		isNSFWContentAllowed: () => false,
		async sanitizeCustomEmojis(params: {content: string}) {
			return params.content;
		},
	};
	return service;
}

async function runUpdate(message: Message, data: MessageUpdateRequest): Promise<Message> {
	const service = createService();
	const result = await service.updateMessage({
		message,
		messageId: message.id,
		data,
		channel: makeDmChannel(),
		guild: null,
	});
	return result.message;
}

describe('MessagePersistenceService.updateMessage embeds', () => {
	it('preserves existing custom embeds on a content-only edit', async () => {
		const message = makeMessage({content: 'before', embeds: [richEmbed('kept')]});
		const updated = await runUpdate(message, {content: 'after'});
		const types = updated.embeds.map((e) => e.type);
		expect(types).toContain(MessageEmbedTypes.RICH);
		expect(updated.embeds.find((e) => e.type === MessageEmbedTypes.RICH)?.title).toBe('kept');
	});

	it('clears embeds when an explicit empty embeds array is supplied', async () => {
		const message = makeMessage({content: 'before', embeds: [richEmbed('kept')]});
		const updated = await runUpdate(message, {content: 'after', embeds: []});
		expect(updated.embeds).toHaveLength(0);
	});

	it('preserves content when only embeds are edited', async () => {
		const message = makeMessage({content: 'keep me', embeds: null});
		const updated = await runUpdate(message, {embeds: [{title: 'new'} as never]});
		expect(updated.content).toBe('keep me');
		expect(updated.embeds.map((e) => e.type)).toContain(MessageEmbedTypes.RICH);
	});

	it('re-derives the URL auto-embed when a content edit changes a link', async () => {
		const message = makeMessage({content: 'see https://old.example', embeds: [urlEmbed('https://old.example')]});
		const updated = await runUpdate(message, {content: 'see https://new.example'});
		const urlEmbeds = updated.embeds.filter((e) => e.type === MessageEmbedTypes.LINK);
		expect(urlEmbeds).toHaveLength(1);
		expect(urlEmbeds[0]?.url).toBe('https://new.example/');
	});
});
