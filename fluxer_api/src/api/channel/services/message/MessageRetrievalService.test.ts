// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {createSnowflakeFromTimestamp} from '@fluxer/snowflake/src/Snowflake';
import {describe, expect, it} from 'vitest';
import type {AttachmentDecayService} from '../../../attachment/AttachmentDecayService';
import type {ChannelID, MessageID} from '../../../BrandedTypes';
import {createChannelID, createMessageID, createUserID} from '../../../BrandedTypes';
import type {UserCacheService} from '../../../infrastructure/UserCacheService';
import {Message} from '../../../models/Message';
import type {IUserRepository} from '../../../user/IUserRepository';
import type {IChannelRepositoryAggregate} from '../../repositories/IChannelRepositoryAggregate';
import type {AuthenticatedChannel} from '../AuthenticatedChannel';
import type {MessageChannelAuthService} from './MessageChannelAuthService';
import type {MessageProcessingService} from './MessageProcessingService';
import {MessageRetrievalService} from './MessageRetrievalService';
import type {MessageSearchService} from './MessageSearchService';

const CHANNEL_ID = createChannelID(10n);
const VIEWER_ID = createUserID(7n);
const CUTOFF = new Date('2026-01-10T00:00:00.000Z');

function makeMessageId(timestamp: Date, sequence: number): MessageID {
	return createMessageID(createSnowflakeFromTimestamp(timestamp.getTime()) + BigInt(sequence));
}

function makeMessage(channelId: ChannelID, messageId: MessageID): Message {
	return new Message({
		channel_id: channelId,
		bucket: 0,
		message_id: messageId,
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
		sticker_items: null,
		message_reference: null,
		message_snapshots: null,
		call: null,
		nsfw_emojis: null,
		has_reaction: null,
		version: 1,
	});
}

function createRetrievalService({
	guilded,
	canReadHistory,
	messageHistoryCutoff = CUTOFF.toISOString(),
	stored,
}: {
	guilded: boolean;
	canReadHistory: boolean;
	messageHistoryCutoff?: string | null;
	stored: Array<Message>;
}) {
	const permissionChecks: Array<bigint> = [];
	const authenticationCalls: Array<string> = [];
	const storedById = new Map(stored.map((message) => [message.id.toString(), message] as const));
	const channel = {id: CHANNEL_ID, guildId: guilded ? 20n : null};
	const authChannel: AuthenticatedChannel = {
		channel: channel as AuthenticatedChannel['channel'],
		guild: guilded ? ({message_history_cutoff: messageHistoryCutoff} as AuthenticatedChannel['guild']) : null,
		member: null,
		hasPermission: async (permission: bigint) => {
			permissionChecks.push(permission);
			return canReadHistory;
		},
		checkPermission: async () => {},
	};
	const channelRepository = {
		messages: {
			getMessage: async (_channelId: ChannelID, messageId: MessageID) => storedById.get(messageId.toString()) ?? null,
		},
	} as unknown as IChannelRepositoryAggregate;
	const channelAuthService = {
		getChannelAuthenticated: async ({channelId}: {channelId: ChannelID}) => {
			authenticationCalls.push(channelId.toString());
			return authChannel;
		},
	} as unknown as MessageChannelAuthService;
	const processingService = {
		repairMentionsOnRead: async (message: Message) => message,
	} as unknown as MessageProcessingService;
	const attachmentDecayService = {
		extendForAttachments: async () => {},
	} as unknown as AttachmentDecayService;
	const service = new MessageRetrievalService(
		channelRepository,
		{} as unknown as UserCacheService,
		channelAuthService,
		processingService,
		{} as unknown as MessageSearchService,
		{} as unknown as IUserRepository,
		attachmentDecayService,
	);
	return {service, permissionChecks, authenticationCalls};
}

describe('MessageRetrievalService.getMessagesByIds', () => {
	it('authenticates the channel and checks message history once for the whole batch', async () => {
		const messages = [
			makeMessage(CHANNEL_ID, makeMessageId(new Date('2026-02-01T00:00:00.000Z'), 1)),
			makeMessage(CHANNEL_ID, makeMessageId(new Date('2026-02-02T00:00:00.000Z'), 2)),
			makeMessage(CHANNEL_ID, makeMessageId(new Date('2026-02-03T00:00:00.000Z'), 3)),
		];
		const {service, permissionChecks, authenticationCalls} = createRetrievalService({
			guilded: true,
			canReadHistory: true,
			stored: messages,
		});

		const result = await service.getMessagesByIds({
			userId: VIEWER_ID,
			channelId: CHANNEL_ID,
			messageIds: messages.map((message) => message.id),
		});

		expect(Array.from(result.keys())).toEqual(messages.map((message) => message.id.toString()));
		expect(authenticationCalls).toEqual([CHANNEL_ID.toString()]);
		expect(permissionChecks).toHaveLength(1);
	});

	it('drops messages older than the history cutoff when history is not readable', async () => {
		const beforeCutoff = makeMessage(CHANNEL_ID, makeMessageId(new Date('2026-01-05T00:00:00.000Z'), 1));
		const afterCutoff = makeMessage(CHANNEL_ID, makeMessageId(new Date('2026-01-20T00:00:00.000Z'), 2));
		const {service, permissionChecks} = createRetrievalService({
			guilded: true,
			canReadHistory: false,
			stored: [beforeCutoff, afterCutoff],
		});

		const result = await service.getMessagesByIds({
			userId: VIEWER_ID,
			channelId: CHANNEL_ID,
			messageIds: [beforeCutoff.id, afterCutoff.id],
		});

		expect(Array.from(result.keys())).toEqual([afterCutoff.id.toString()]);
		expect(permissionChecks).toHaveLength(1);
	});

	it('drops every message when history is not readable and no cutoff is configured', async () => {
		const message = makeMessage(CHANNEL_ID, makeMessageId(new Date('2026-02-01T00:00:00.000Z'), 1));
		const {service} = createRetrievalService({
			guilded: true,
			canReadHistory: false,
			messageHistoryCutoff: null,
			stored: [message],
		});

		const result = await service.getMessagesByIds({
			userId: VIEWER_ID,
			channelId: CHANNEL_ID,
			messageIds: [message.id],
		});

		expect(result.size).toBe(0);
	});

	it('skips the history permission check entirely for direct messages', async () => {
		const message = makeMessage(CHANNEL_ID, makeMessageId(new Date('2026-02-01T00:00:00.000Z'), 1));
		const {service, permissionChecks} = createRetrievalService({
			guilded: false,
			canReadHistory: false,
			stored: [message],
		});

		const result = await service.getMessagesByIds({
			userId: VIEWER_ID,
			channelId: CHANNEL_ID,
			messageIds: [message.id],
		});

		expect(Array.from(result.keys())).toEqual([message.id.toString()]);
		expect(permissionChecks).toEqual([]);
	});

	it('omits messages that no longer exist instead of failing the batch', async () => {
		const present = makeMessage(CHANNEL_ID, makeMessageId(new Date('2026-02-01T00:00:00.000Z'), 1));
		const deleted = makeMessage(CHANNEL_ID, makeMessageId(new Date('2026-02-02T00:00:00.000Z'), 2));
		const {service} = createRetrievalService({
			guilded: true,
			canReadHistory: true,
			stored: [present],
		});

		const result = await service.getMessagesByIds({
			userId: VIEWER_ID,
			channelId: CHANNEL_ID,
			messageIds: [present.id, deleted.id],
		});

		expect(Array.from(result.keys())).toEqual([present.id.toString()]);
	});
});
