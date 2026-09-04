// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {describe, expect, it} from 'vitest';
import type {ApiContext} from '../../ApiContext';
import type {ChannelID, MessageID, UserID} from '../../BrandedTypes';
import {createChannelID, createMessageID, createUserID} from '../../BrandedTypes';
import type {IChannelRepository} from '../../channel/IChannelRepository';
import type {ChannelService} from '../../channel/services/ChannelService';
import type {KVBulkMessageDeletionQueueService} from '../../infrastructure/KVBulkMessageDeletionQueueService';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import type {LimitConfigService} from '../../limits/LimitConfigService';
import {Message} from '../../models/Message';
import {UserContentService, UserContentServiceTestHooks} from './UserContentService';

const {isUnreachableEntityError} = UserContentServiceTestHooks;

describe('isUnreachableEntityError', () => {
	it('treats a deleted or left community as unreachable rather than fatal', () => {
		expect(isUnreachableEntityError(new UnknownGuildError())).toBe(true);
	});

	it('treats a gone channel and a lost permission as unreachable', () => {
		expect(isUnreachableEntityError(new UnknownChannelError())).toBe(true);
		expect(isUnreachableEntityError(new MissingPermissionsError())).toBe(true);
	});

	it('leaves a deleted message to the delete path instead of marking it unavailable', () => {
		expect(isUnreachableEntityError(new UnknownMessageError())).toBe(false);
	});

	it('still lets unexpected failures surface', () => {
		expect(isUnreachableEntityError(new Error('database is on fire'))).toBe(false);
		expect(isUnreachableEntityError(null)).toBe(false);
	});
});

const VIEWER_ID = createUserID(7n);

interface ChannelBatchCall {
	channelId: string;
	messageIds: Array<string>;
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
		has_reaction: null,
		version: 1,
	});
}

function createUserContentService({
	entries,
	readable,
	failures = new Map<string, Error>(),
}: {
	entries: Array<{channelId: ChannelID; messageId: MessageID}>;
	readable: Array<{channelId: ChannelID; messageId: MessageID}>;
	failures?: Map<string, Error>;
}) {
	const batchCalls: Array<ChannelBatchCall> = [];
	const deletedSavedMessageIds: Array<string> = [];
	const readableByChannel = new Map<string, Map<string, Message>>();
	for (const entry of readable) {
		const key = entry.channelId.toString();
		const messages = readableByChannel.get(key) ?? new Map<string, Message>();
		messages.set(entry.messageId.toString(), makeMessage(entry.channelId, entry.messageId));
		readableByChannel.set(key, messages);
	}
	const userRepository = {
		listRecentMentions: async () => entries,
		listSavedMessages: async () => entries,
		deleteSavedMessage: async (_userId: UserID, messageId: MessageID) => {
			deletedSavedMessageIds.push(messageId.toString());
		},
	};
	const channelService = {
		messages: {
			retrieval: {
				getMessagesByIds: async ({channelId, messageIds}: {channelId: ChannelID; messageIds: Array<MessageID>}) => {
					batchCalls.push({
						channelId: channelId.toString(),
						messageIds: messageIds.map((messageId) => messageId.toString()),
					});
					const failure = failures.get(channelId.toString());
					if (failure) throw failure;
					return readableByChannel.get(channelId.toString()) ?? new Map<string, Message>();
				},
			},
		},
	};
	const service = new UserContentService(
		{services: {users: userRepository, gateway: {}, worker: {}, snowflake: {}}} as unknown as ApiContext,
		{} as unknown as UserCacheService,
		channelService as unknown as ChannelService,
		{} as unknown as IChannelRepository,
		{} as unknown as KVBulkMessageDeletionQueueService,
		{} as unknown as LimitConfigService,
	);
	return {service, batchCalls, deletedSavedMessageIds};
}

const CHANNEL_A = createChannelID(100n);
const CHANNEL_B = createChannelID(200n);
const CHANNEL_C = createChannelID(300n);

describe('getRecentMentions', () => {
	it('authenticates each distinct channel once no matter how many mentions it holds', async () => {
		const entries = [
			{channelId: CHANNEL_A, messageId: createMessageID(11n)},
			{channelId: CHANNEL_B, messageId: createMessageID(12n)},
			{channelId: CHANNEL_A, messageId: createMessageID(13n)},
			{channelId: CHANNEL_A, messageId: createMessageID(14n)},
		];
		const {service, batchCalls} = createUserContentService({entries, readable: entries});

		const messages = await service.getRecentMentions({
			userId: VIEWER_ID,
			limit: 50,
			everyone: true,
			roles: true,
			guilds: true,
		});

		expect(messages.map((message) => message.id.toString())).toEqual(['14', '13', '12', '11']);
		expect(batchCalls).toEqual([
			{channelId: '100', messageIds: ['11', '13', '14']},
			{channelId: '200', messageIds: ['12']},
		]);
	});

	it('drops only the mentions from the channels the viewer can no longer reach', async () => {
		const entries = [
			{channelId: CHANNEL_A, messageId: createMessageID(11n)},
			{channelId: CHANNEL_B, messageId: createMessageID(12n)},
			{channelId: CHANNEL_C, messageId: createMessageID(13n)},
			{channelId: CHANNEL_B, messageId: createMessageID(14n)},
		];
		const {service} = createUserContentService({
			entries,
			readable: [entries[0], entries[2]],
			failures: new Map<string, Error>([
				[CHANNEL_B.toString(), new MissingPermissionsError()],
				[CHANNEL_C.toString(), new UnknownChannelError()],
			]),
		});

		const messages = await service.getRecentMentions({
			userId: VIEWER_ID,
			limit: 50,
			everyone: true,
			roles: true,
			guilds: true,
		});

		expect(messages.map((message) => message.id.toString())).toEqual(['11']);
	});

	it('drops a mention whose message the batch read did not return', async () => {
		const entries = [
			{channelId: CHANNEL_A, messageId: createMessageID(11n)},
			{channelId: CHANNEL_A, messageId: createMessageID(12n)},
		];
		const {service} = createUserContentService({entries, readable: [entries[1]]});

		const messages = await service.getRecentMentions({
			userId: VIEWER_ID,
			limit: 50,
			everyone: true,
			roles: true,
			guilds: true,
		});

		expect(messages.map((message) => message.id.toString())).toEqual(['12']);
	});

	it('still lets an unexpected channel failure surface', async () => {
		const entries = [{channelId: CHANNEL_A, messageId: createMessageID(11n)}];
		const {service} = createUserContentService({
			entries,
			readable: [],
			failures: new Map<string, Error>([[CHANNEL_A.toString(), new Error('database is on fire')]]),
		});

		await expect(
			service.getRecentMentions({userId: VIEWER_ID, limit: 50, everyone: true, roles: true, guilds: true}),
		).rejects.toThrow('database is on fire');
	});
});

describe('getSavedMessages', () => {
	it('marks every entry of an unreachable channel as missing permissions without deleting it', async () => {
		const entries = [
			{channelId: CHANNEL_A, messageId: createMessageID(11n)},
			{channelId: CHANNEL_B, messageId: createMessageID(12n)},
			{channelId: CHANNEL_B, messageId: createMessageID(13n)},
		];
		const {service, batchCalls, deletedSavedMessageIds} = createUserContentService({
			entries,
			readable: [entries[0]],
			failures: new Map<string, Error>([[CHANNEL_B.toString(), new UnknownGuildError()]]),
		});

		const saved = await service.getSavedMessages({userId: VIEWER_ID, limit: 50});

		expect(
			saved.map((entry) => ({id: entry.messageId.toString(), status: entry.status, hasMessage: entry.message != null})),
		).toEqual([
			{id: '13', status: 'missing_permissions', hasMessage: false},
			{id: '12', status: 'missing_permissions', hasMessage: false},
			{id: '11', status: 'available', hasMessage: true},
		]);
		expect(deletedSavedMessageIds).toEqual([]);
		expect(batchCalls).toEqual([
			{channelId: '100', messageIds: ['11']},
			{channelId: '200', messageIds: ['12', '13']},
		]);
	});

	it('deletes and drops a saved message the batch read could not return', async () => {
		const entries = [
			{channelId: CHANNEL_A, messageId: createMessageID(11n)},
			{channelId: CHANNEL_A, messageId: createMessageID(12n)},
		];
		const {service, deletedSavedMessageIds} = createUserContentService({entries, readable: [entries[0]]});

		const saved = await service.getSavedMessages({userId: VIEWER_ID, limit: 50});

		expect(saved.map((entry) => entry.messageId.toString())).toEqual(['11']);
		expect(deletedSavedMessageIds).toEqual(['12']);
	});
});
