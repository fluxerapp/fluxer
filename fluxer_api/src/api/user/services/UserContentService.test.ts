// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {AccessDeniedError} from '@fluxer/errors/src/domains/core/AccessDeniedError';
import {BadGatewayError} from '@fluxer/errors/src/domains/core/BadGatewayError';
import {MaxBookmarksError} from '@fluxer/errors/src/domains/core/MaxBookmarksError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {NsfwContentRequiresAgeVerificationError} from '@fluxer/errors/src/domains/moderation/NsfwContentRequiresAgeVerificationError';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';
import {describe, expect, it, vi} from 'vitest';
import type {ApiContext} from '../../ApiContext';
import type {ChannelID, MessageID, UserID} from '../../BrandedTypes';
import {createChannelID, createMessageID, createUserID} from '../../BrandedTypes';
import type {IChannelRepository} from '../../channel/IChannelRepository';
import type {ChannelService} from '../../channel/services/ChannelService';
import type {KVBulkMessageDeletionQueueService} from '../../infrastructure/KVBulkMessageDeletionQueueService';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import type {LimitConfigService} from '../../limits/LimitConfigService';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
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

	it('treats an age gate and an unresolved membership as unreachable', () => {
		expect(isUnreachableEntityError(new AccessDeniedError())).toBe(true);
		expect(isUnreachableEntityError(new NsfwContentRequiresAgeVerificationError())).toBe(true);
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
	stored,
	failures = new Map<string, Error>(),
}: {
	entries: Array<{channelId: ChannelID; messageId: MessageID}>;
	readable: Array<{channelId: ChannelID; messageId: MessageID}>;
	stored?: Array<{channelId: ChannelID; messageId: MessageID}>;
	failures?: Map<string, Error>;
}) {
	const batchCalls: Array<ChannelBatchCall> = [];
	const deletedSavedMessageIds: Array<string> = [];
	const savedMessageListCalls: Array<{limit?: number; before?: MessageID}> = [];
	const readableByChannel = new Map<string, Map<string, Message>>();
	for (const entry of readable) {
		const key = entry.channelId.toString();
		const messages = readableByChannel.get(key) ?? new Map<string, Message>();
		messages.set(entry.messageId.toString(), makeMessage(entry.channelId, entry.messageId));
		readableByChannel.set(key, messages);
	}
	const userRepository = {
		listRecentMentions: async () => entries,
		listSavedMessages: async (_userId: UserID, limit?: number, before?: MessageID) => {
			savedMessageListCalls.push({limit, before});
			return entries;
		},
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
	const storedKeys = new Set(
		(stored ?? readable).map((entry) => `${entry.channelId.toString()}:${entry.messageId.toString()}`),
	);
	const channelRepository = {
		messages: {
			getMessage: async (channelId: ChannelID, messageId: MessageID) =>
				storedKeys.has(`${channelId.toString()}:${messageId.toString()}`) ? makeMessage(channelId, messageId) : null,
		},
	};
	const service = new UserContentService(
		{services: {users: userRepository, gateway: {}, worker: {}, snowflake: {}}} as unknown as ApiContext,
		{} as unknown as UserCacheService,
		channelService as unknown as ChannelService,
		channelRepository as unknown as IChannelRepository,
		{} as unknown as KVBulkMessageDeletionQueueService,
		{} as unknown as LimitConfigService,
	);
	return {service, batchCalls, deletedSavedMessageIds, savedMessageListCalls};
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
	it('passes the page cursor to the repository', async () => {
		const entries = [{channelId: CHANNEL_A, messageId: createMessageID(11n)}];
		const {service, savedMessageListCalls} = createUserContentService({entries, readable: entries});

		await service.getSavedMessages({userId: VIEWER_ID, limit: 50, before: createMessageID(20n)});

		expect(savedMessageListCalls).toEqual([{limit: 50, before: createMessageID(20n)}]);
	});

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

	it('keeps a saved message the batch read could not return while the message still exists', async () => {
		const entries = [
			{channelId: CHANNEL_A, messageId: createMessageID(11n)},
			{channelId: CHANNEL_A, messageId: createMessageID(12n)},
		];
		const {service, deletedSavedMessageIds} = createUserContentService({
			entries,
			readable: [entries[0]],
			stored: entries,
		});

		const saved = await service.getSavedMessages({userId: VIEWER_ID, limit: 50});

		expect(
			saved.map((entry) => ({id: entry.messageId.toString(), status: entry.status, hasMessage: entry.message != null})),
		).toEqual([
			{id: '12', status: 'missing_permissions', hasMessage: false},
			{id: '11', status: 'available', hasMessage: true},
		]);
		expect(deletedSavedMessageIds).toEqual([]);
	});

	it('deletes a saved message the repository no longer holds', async () => {
		const entries = [
			{channelId: CHANNEL_A, messageId: createMessageID(11n)},
			{channelId: CHANNEL_A, messageId: createMessageID(12n)},
		];
		const {service, deletedSavedMessageIds} = createUserContentService({
			entries,
			readable: [entries[0]],
			stored: [entries[0]],
		});

		const saved = await service.getSavedMessages({userId: VIEWER_ID, limit: 50});

		expect(saved.map((entry) => entry.messageId.toString())).toEqual(['11']);
		expect(deletedSavedMessageIds).toEqual(['12']);
	});
});

describe('gateway dispatches after the write', () => {
	function createDispatchingService(dispatchPresence: () => Promise<void>) {
		const createdSavedMessageIds: Array<string> = [];
		const deletedSavedMessageIds: Array<string> = [];
		const deletedRecentMentionIds: Array<string> = [];
		const message = makeMessage(CHANNEL_A, createMessageID(31n));
		const userRepository = {
			findUnique: async () => ({
				isBot: false,
				premiumType: 0,
				premiumUntil: null,
				premiumGiftExtensionEndsAt: null,
				premiumWillCancel: false,
				premiumGraceEndsAt: null,
				flags: 0n,
				premiumFlags: 0,
				traits: null,
			}),
			listSavedMessages: async () => [],
			countSavedMessages: async () => 0,
			createSavedMessage: async (_userId: UserID, _channelId: ChannelID, messageId: MessageID) => {
				createdSavedMessageIds.push(messageId.toString());
			},
			deleteSavedMessage: async (_userId: UserID, messageId: MessageID) => {
				deletedSavedMessageIds.push(messageId.toString());
			},
			getRecentMention: async (_userId: UserID, messageId: MessageID) => ({messageId}),
			deleteRecentMention: async (mention: {messageId: MessageID}) => {
				deletedRecentMentionIds.push(mention.messageId.toString());
			},
		};
		const channelService = {
			channelData: {auth: {getChannelAuthenticated: async () => ({})}},
			messages: {retrieval: {getMessage: async () => message}},
		};
		const service = new UserContentService(
			{
				services: {users: userRepository, gateway: {dispatchPresence}, worker: {}, snowflake: {}},
			} as unknown as ApiContext,
			{} as unknown as UserCacheService,
			channelService as unknown as ChannelService,
			{} as unknown as IChannelRepository,
			{} as unknown as KVBulkMessageDeletionQueueService,
			{getConfigSnapshot: () => null} as unknown as LimitConfigService,
		);
		return {service, message, createdSavedMessageIds, deletedSavedMessageIds, deletedRecentMentionIds};
	}

	function saveMessageArgs(messageId: MessageID) {
		return {
			userId: VIEWER_ID,
			channelId: CHANNEL_A,
			messageId,
			userCacheService: {} as unknown as UserCacheService,
			requestCache: {} as unknown as RequestCache,
		};
	}

	it('keeps the saved message when SAVED_MESSAGE_CREATE fails to publish', async () => {
		const {service, message, createdSavedMessageIds} = createDispatchingService(async () => {
			throw new BadGatewayError();
		});
		vi.spyOn(service, 'buildMessageResponsesForUser').mockResolvedValue([]);

		await expect(service.saveMessage(saveMessageArgs(message.id))).resolves.toBeUndefined();

		expect(createdSavedMessageIds).toEqual([message.id.toString()]);
	});

	it('still surfaces a failure of the message response build', async () => {
		const {service, message} = createDispatchingService(async () => {});
		vi.spyOn(service, 'buildMessageResponsesForUser').mockRejectedValue(new Error('database is on fire'));

		await expect(service.saveMessage(saveMessageArgs(message.id))).rejects.toThrow('database is on fire');
	});

	it('keeps the deletion when SAVED_MESSAGE_DELETE fails to publish', async () => {
		const {service, deletedSavedMessageIds} = createDispatchingService(async () => {
			throw new BadGatewayError();
		});

		await expect(service.unsaveMessage({userId: VIEWER_ID, messageId: createMessageID(31n)})).resolves.toBeUndefined();

		expect(deletedSavedMessageIds).toEqual(['31']);
	});

	it('keeps the deletion when RECENT_MENTION_DELETE fails to publish', async () => {
		const {service, deletedRecentMentionIds} = createDispatchingService(async () => {
			throw new BadGatewayError();
		});

		await expect(
			service.deleteRecentMention({userId: VIEWER_ID, messageId: createMessageID(31n)}),
		).resolves.toBeUndefined();

		expect(deletedRecentMentionIds).toEqual(['31']);
	});
});

describe('bookmark ceiling', () => {
	function createBookmarkLimitedService({
		savedMessageCount,
		maxBookmarks,
	}: {
		savedMessageCount: number;
		maxBookmarks: number;
	}) {
		const createdSavedMessageIds: Array<string> = [];
		const message = makeMessage(CHANNEL_A, createMessageID(41n));
		const userRepository = {
			findUnique: async () => ({
				isBot: false,
				premiumType: 0,
				premiumUntil: null,
				premiumGiftExtensionEndsAt: null,
				premiumWillCancel: false,
				premiumGraceEndsAt: null,
				flags: 0n,
				premiumFlags: 0,
				traits: null,
			}),
			countSavedMessages: async () => savedMessageCount,
			listSavedMessages: async () => {
				throw new Error('the ceiling check must not page through saved messages');
			},
			createSavedMessage: async (_userId: UserID, _channelId: ChannelID, messageId: MessageID) => {
				createdSavedMessageIds.push(messageId.toString());
			},
		};
		const channelService = {
			channelData: {auth: {getChannelAuthenticated: async () => ({})}},
			messages: {retrieval: {getMessage: async () => message}},
		};
		const snapshot: LimitConfigSnapshot = {
			traitDefinitions: [],
			rules: [{id: 'default', limits: {max_bookmarks: maxBookmarks}}],
		};
		const service = new UserContentService(
			{
				services: {users: userRepository, gateway: {dispatchPresence: async () => {}}, worker: {}, snowflake: {}},
			} as unknown as ApiContext,
			{} as unknown as UserCacheService,
			channelService as unknown as ChannelService,
			{} as unknown as IChannelRepository,
			{} as unknown as KVBulkMessageDeletionQueueService,
			{getConfigSnapshot: () => snapshot} as unknown as LimitConfigService,
		);
		return {service, message, createdSavedMessageIds};
	}

	function saveMessageArgs(messageId: MessageID) {
		return {
			userId: VIEWER_ID,
			channelId: CHANNEL_A,
			messageId,
			userCacheService: {} as unknown as UserCacheService,
			requestCache: {} as unknown as RequestCache,
		};
	}

	it('enforces a configured max_bookmarks above 1000', async () => {
		const {service, message, createdSavedMessageIds} = createBookmarkLimitedService({
			savedMessageCount: 1002,
			maxBookmarks: 1002,
		});
		vi.spyOn(service, 'buildMessageResponsesForUser').mockResolvedValue([]);

		const error = await service.saveMessage(saveMessageArgs(message.id)).catch((thrown: unknown) => thrown);

		expect(error).toBeInstanceOf(MaxBookmarksError);
		expect((error as MaxBookmarksError).status).toBe(400);
		expect((error as MaxBookmarksError).code).toBe(APIErrorCodes.MAX_BOOKMARKS);
		expect((error as MaxBookmarksError).data?.max_bookmarks).toBe(1002);
		expect(createdSavedMessageIds).toEqual([]);
	});

	it('still saves below a configured max_bookmarks above 1000', async () => {
		const {service, message, createdSavedMessageIds} = createBookmarkLimitedService({
			savedMessageCount: 1001,
			maxBookmarks: 1002,
		});
		vi.spyOn(service, 'buildMessageResponsesForUser').mockResolvedValue([]);

		await expect(service.saveMessage(saveMessageArgs(message.id))).resolves.toBeUndefined();

		expect(createdSavedMessageIds).toEqual([message.id.toString()]);
	});
});
