// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ApiContext} from '@app/api/ApiContext';
import type {ChannelID, GuildID, MessageID, UserID} from '@app/api/BrandedTypes';
import {createChannelID, createGuildID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import {SYSTEM_THREAD_VIEWER, syncChannelThreadsConfig} from '@app/api/experiment/ChannelThreadsGate';
import type {KVBulkMessageDeletionQueueService} from '@app/api/infrastructure/KVBulkMessageDeletionQueueService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import {Message} from '@app/api/models/Message';
import {UserContentService} from '@app/api/user/services/UserContentService';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {ServerMessageFlags} from '@fluxer/constants/src/ThreadConstants';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {BadGatewayError} from '@fluxer/errors/src/domains/core/BadGatewayError';
import {MaxBookmarksError} from '@fluxer/errors/src/domains/core/MaxBookmarksError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';
import {ChannelThreadsConfigSchema} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

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
	channels = [],
}: {
	entries: Array<{channelId: ChannelID; messageId: MessageID}>;
	readable: Array<{channelId: ChannelID; messageId: MessageID}>;
	stored?: Array<{channelId: ChannelID; messageId: MessageID}>;
	failures?: Map<string, Error>;
	channels?: Array<{id: ChannelID; type: number; guildId: GuildID | null}>;
}) {
	const batchCalls: Array<ChannelBatchCall> = [];
	const deletedSavedMessageIds: Array<string> = [];
	const savedMessageListCalls: Array<{limit?: number; before?: MessageID}> = [];
	const recentMentionListCalls: Array<{limit: number; before?: MessageID}> = [];
	const readableByChannel = new Map<string, Map<string, Message>>();
	for (const entry of readable) {
		const key = entry.channelId.toString();
		const messages = readableByChannel.get(key) ?? new Map<string, Message>();
		messages.set(entry.messageId.toString(), makeMessage(entry.channelId, entry.messageId));
		readableByChannel.set(key, messages);
	}
	const userRepository = {
		listRecentMentions: async (
			_userId: UserID,
			_everyone: boolean,
			_roles: boolean,
			_guilds: boolean,
			limit: number,
			before?: MessageID,
		) => {
			recentMentionListCalls.push({limit, before});
			return entries.filter((entry) => before === undefined || entry.messageId < before).slice(0, limit);
		},
		listSavedMessages: async (_userId: UserID, limit?: number, before?: MessageID) => {
			savedMessageListCalls.push({limit, before});
			return entries.filter((entry) => before === undefined || entry.messageId < before).slice(0, limit);
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
		channelData: {
			listChannels: async (channelIds: Array<ChannelID>) =>
				channels.filter((channel) => channelIds.some((channelId) => channelId === channel.id)),
		},
		messages: {
			getMessage: async (channelId: ChannelID, messageId: MessageID) =>
				storedKeys.has(`${channelId.toString()}:${messageId.toString()}`) ? makeMessage(channelId, messageId) : null,
		},
	};
	const service = new UserContentService(
		{services: {users: userRepository, gateway: {}, worker: {}, snowflake: {}}} as unknown as ApiContext,
		{} as unknown as UserCacheService,
		channelService as unknown as ChannelService,
		channelRepository as unknown as ChannelRepository,
		{} as unknown as KVBulkMessageDeletionQueueService,
		{} as unknown as LimitConfigService,
	);
	return {service, batchCalls, deletedSavedMessageIds, savedMessageListCalls, recentMentionListCalls};
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
			viewer: SYSTEM_THREAD_VIEWER,
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
			viewer: SYSTEM_THREAD_VIEWER,
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
			viewer: SYSTEM_THREAD_VIEWER,
			userId: VIEWER_ID,
			limit: 50,
			everyone: true,
			roles: true,
			guilds: true,
		});

		expect(messages.map((message) => message.id.toString())).toEqual(['12']);
	});

	describe('thread channels', () => {
		const GUILD = createGuildID(900n);
		const THREAD = createChannelID(400n);
		const viewer = {kind: 'user', userId: VIEWER_ID, bot: false, capable: false} as const;

		beforeEach(() => {
			syncChannelThreadsConfig(null, (raw) => ChannelThreadsConfigSchema.parse(raw ? JSON.parse(raw) : {}));
		});

		const entries = [
			{channelId: THREAD, messageId: createMessageID(15n)},
			{channelId: THREAD, messageId: createMessageID(14n)},
			{channelId: CHANNEL_A, messageId: createMessageID(13n)},
			{channelId: CHANNEL_B, messageId: createMessageID(12n)},
			{channelId: CHANNEL_A, messageId: createMessageID(11n)},
			{channelId: CHANNEL_A, messageId: createMessageID(10n)},
		];

		function build() {
			return createUserContentService({
				entries,
				readable: entries.filter((entry) => entry.channelId === CHANNEL_A),
				failures: new Map<string, Error>([
					[THREAD.toString(), new UnknownChannelError()],
					[CHANNEL_B.toString(), new UnknownChannelError()],
				]),
				channels: [
					{id: THREAD, type: ChannelTypes.PUBLIC_THREAD, guildId: GUILD},
					{id: CHANNEL_B, type: ChannelTypes.GUILD_TEXT, guildId: GUILD},
				],
			});
		}

		it('refills the page when the newest mentions sit in threads a non-viewer cannot see', async () => {
			syncChannelThreadsConfig(JSON.stringify({enabled: false, ever_enabled: true}), (raw) =>
				ChannelThreadsConfigSchema.parse(JSON.parse(raw!)),
			);
			const {service, recentMentionListCalls} = build();

			const messages = await service.getRecentMentions({
				viewer,
				userId: VIEWER_ID,
				limit: 4,
				everyone: true,
				roles: true,
				guilds: true,
			});

			expect(messages.map((message) => message.id.toString())).toEqual(['13', '11', '10']);
			expect(recentMentionListCalls).toEqual([
				{limit: 4, before: undefined},
				{limit: 2, before: createMessageID(12n)},
			]);
		});

		it('stops refilling after a bounded number of pages of hidden mentions', async () => {
			syncChannelThreadsConfig(JSON.stringify({enabled: false, ever_enabled: true}), (raw) =>
				ChannelThreadsConfigSchema.parse(JSON.parse(raw!)),
			);
			const hiddenEntries = Array.from({length: 50}, (_, index) => ({
				channelId: THREAD,
				messageId: createMessageID(BigInt(100 - index)),
			}));
			const {service, recentMentionListCalls} = createUserContentService({
				entries: hiddenEntries,
				readable: [],
				failures: new Map<string, Error>([[THREAD.toString(), new UnknownChannelError()]]),
				channels: [{id: THREAD, type: ChannelTypes.PUBLIC_THREAD, guildId: GUILD}],
			});

			const messages = await service.getRecentMentions({
				viewer,
				userId: VIEWER_ID,
				limit: 4,
				everyone: true,
				roles: true,
				guilds: true,
			});

			expect(messages).toEqual([]);
			expect(recentMentionListCalls).toHaveLength(4);
		});

		it('reads one page when the experiment was never enabled', async () => {
			const {service, recentMentionListCalls} = build();

			const messages = await service.getRecentMentions({
				viewer,
				userId: VIEWER_ID,
				limit: 4,
				everyone: true,
				roles: true,
				guilds: true,
			});

			expect(messages.map((message) => message.id.toString())).toEqual(['13']);
			expect(recentMentionListCalls).toHaveLength(1);
		});
	});

	it('still lets an unexpected channel failure surface', async () => {
		const entries = [{channelId: CHANNEL_A, messageId: createMessageID(11n)}];
		const {service} = createUserContentService({
			entries,
			readable: [],
			failures: new Map<string, Error>([[CHANNEL_A.toString(), new Error('database is on fire')]]),
		});

		await expect(
			service.getRecentMentions({
				viewer: SYSTEM_THREAD_VIEWER,
				userId: VIEWER_ID,
				limit: 50,
				everyone: true,
				roles: true,
				guilds: true,
			}),
		).rejects.toThrow('database is on fire');
	});
});

describe('getSavedMessages', () => {
	it('passes the page cursor to the repository', async () => {
		const entries = [{channelId: CHANNEL_A, messageId: createMessageID(11n)}];
		const {service, savedMessageListCalls} = createUserContentService({entries, readable: entries});

		await service.getSavedMessages({
			viewer: SYSTEM_THREAD_VIEWER,
			userId: VIEWER_ID,
			limit: 50,
			before: createMessageID(20n),
		});

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

		const saved = await service.getSavedMessages({viewer: SYSTEM_THREAD_VIEWER, userId: VIEWER_ID, limit: 50});

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

		const saved = await service.getSavedMessages({viewer: SYSTEM_THREAD_VIEWER, userId: VIEWER_ID, limit: 50});

		expect(
			saved.map((entry) => ({id: entry.messageId.toString(), status: entry.status, hasMessage: entry.message != null})),
		).toEqual([
			{id: '12', status: 'missing_permissions', hasMessage: false},
			{id: '11', status: 'available', hasMessage: true},
		]);
		expect(deletedSavedMessageIds).toEqual([]);
	});

	describe('thread channels', () => {
		const GUILD = createGuildID(900n);
		const THREAD = createChannelID(400n);
		const viewer = {kind: 'user', userId: VIEWER_ID, bot: false, capable: false} as const;

		function everEnabled() {
			syncChannelThreadsConfig(JSON.stringify({enabled: false, ever_enabled: true}), (raw) =>
				ChannelThreadsConfigSchema.parse(JSON.parse(raw!)),
			);
		}

		beforeEach(() => {
			syncChannelThreadsConfig(null, (raw) => ChannelThreadsConfigSchema.parse(raw ? JSON.parse(raw) : {}));
		});

		it('stops refilling saved messages after a bounded number of hidden pages', async () => {
			everEnabled();
			const {service, savedMessageListCalls} = createUserContentService({
				entries: Array.from({length: 50}, (_, index) => ({
					channelId: THREAD,
					messageId: createMessageID(BigInt(100 - index)),
				})),
				readable: [],
				failures: new Map<string, Error>([[THREAD.toString(), new UnknownChannelError()]]),
				channels: [{id: THREAD, type: ChannelTypes.PUBLIC_THREAD, guildId: GUILD}],
			});

			expect(await service.getSavedMessages({viewer, userId: VIEWER_ID, limit: 4})).toEqual([]);
			expect(savedMessageListCalls).toHaveLength(4);
		});

		it('drops saved thread messages for a non-viewer and refills the page', async () => {
			everEnabled();
			const entries = [
				{channelId: CHANNEL_A, messageId: createMessageID(15n)},
				{channelId: THREAD, messageId: createMessageID(14n)},
				{channelId: THREAD, messageId: createMessageID(13n)},
				{channelId: CHANNEL_A, messageId: createMessageID(12n)},
				{channelId: CHANNEL_B, messageId: createMessageID(11n)},
				{channelId: CHANNEL_A, messageId: createMessageID(10n)},
			];
			const {service, savedMessageListCalls} = createUserContentService({
				entries,
				readable: entries.filter((entry) => entry.channelId === CHANNEL_A),
				failures: new Map<string, Error>([
					[THREAD.toString(), new UnknownChannelError()],
					[CHANNEL_B.toString(), new UnknownChannelError()],
				]),
				channels: [
					{id: THREAD, type: ChannelTypes.PUBLIC_THREAD, guildId: GUILD},
					{id: CHANNEL_B, type: ChannelTypes.GUILD_TEXT, guildId: GUILD},
				],
			});

			const saved = await service.getSavedMessages({viewer, userId: VIEWER_ID, limit: 4});

			expect(saved.map((entry) => [entry.messageId.toString(), entry.status])).toEqual([
				['15', 'available'],
				['12', 'available'],
				['11', 'missing_permissions'],
				['10', 'available'],
			]);
			expect(savedMessageListCalls).toEqual([
				{limit: 4, before: undefined},
				{limit: 2, before: createMessageID(12n)},
			]);
		});

		it('keeps the unreachable stub when the experiment was never enabled', async () => {
			const entries = [{channelId: THREAD, messageId: createMessageID(14n)}];
			const {service, savedMessageListCalls} = createUserContentService({
				entries,
				readable: [],
				failures: new Map<string, Error>([[THREAD.toString(), new UnknownChannelError()]]),
				channels: [{id: THREAD, type: ChannelTypes.PUBLIC_THREAD, guildId: GUILD}],
			});

			const saved = await service.getSavedMessages({viewer, userId: VIEWER_ID, limit: 4});

			expect(saved.map((entry) => entry.status)).toEqual(['missing_permissions']);
			expect(savedMessageListCalls).toHaveLength(1);
		});
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

		const saved = await service.getSavedMessages({viewer: SYSTEM_THREAD_VIEWER, userId: VIEWER_ID, limit: 50});

		expect(saved.map((entry) => entry.messageId.toString())).toEqual(['11']);
		expect(deletedSavedMessageIds).toEqual(['12']);
	});
});

describe('gateway dispatches after the write', () => {
	function createDispatchingService(
		dispatchPresence: (params: {event: string; data: unknown}) => Promise<void>,
		channel: {type: number; guildId: GuildID | null} = {type: ChannelTypes.GUILD_TEXT, guildId: null},
	) {
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
			channelData: {auth: {getChannelAuthenticated: async () => ({channel})}},
			messages: {retrieval: {getMessage: async () => message}},
		};
		const service = new UserContentService(
			{
				services: {users: userRepository, gateway: {dispatchPresence}, worker: {}, snowflake: {}},
			} as unknown as ApiContext,
			{} as unknown as UserCacheService,
			channelService as unknown as ChannelService,
			{} as unknown as ChannelRepository,
			{} as unknown as KVBulkMessageDeletionQueueService,
			{getConfigSnapshot: () => null} as unknown as LimitConfigService,
		);
		return {service, message, createdSavedMessageIds, deletedSavedMessageIds, deletedRecentMentionIds};
	}

	function saveMessageArgs(messageId: MessageID) {
		return {
			userId: VIEWER_ID,
			viewer: SYSTEM_THREAD_VIEWER,
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
		vi.spyOn(service, 'buildUnmaskedResponses').mockResolvedValue({responses: [], channelById: new Map()});

		await expect(service.saveMessage(saveMessageArgs(message.id))).resolves.toBeUndefined();

		expect(createdSavedMessageIds).toEqual([message.id.toString()]);
	});

	it('still surfaces a failure of the message response build', async () => {
		const {service, message} = createDispatchingService(async () => {});
		vi.spyOn(service, 'buildUnmaskedResponses').mockRejectedValue(new Error('database is on fire'));

		await expect(service.saveMessage(saveMessageArgs(message.id))).rejects.toThrow('database is on fire');
	});

	describe('thread scoping', () => {
		const GUILD = createGuildID(900n);

		function capture() {
			const sent: Array<unknown> = [];
			return {
				sent,
				dispatch: async (params: {data: unknown}) => {
					sent.push(params.data);
				},
			};
		}

		function response(overrides: Partial<MessageResponse>): MessageResponse {
			return {id: '31', channel_id: '100', type: MessageTypes.DEFAULT, flags: 0, ...overrides} as MessageResponse;
		}

		it('sends a plain guild message unchanged', async () => {
			const {sent, dispatch} = capture();
			const {service, message} = createDispatchingService(dispatch, {type: ChannelTypes.GUILD_TEXT, guildId: GUILD});
			const data = response({});
			vi.spyOn(service, 'buildUnmaskedResponses').mockResolvedValue({responses: [data], channelById: new Map()});

			await service.saveMessage(saveMessageArgs(message.id));

			expect(sent).toEqual([data]);
		});

		it('scopes a thread message to thread viewers only', async () => {
			const {sent, dispatch} = capture();
			const {service, message} = createDispatchingService(dispatch, {
				type: ChannelTypes.PUBLIC_THREAD,
				guildId: GUILD,
			});
			const data = response({channel_id: '400'});
			vi.spyOn(service, 'buildUnmaskedResponses').mockResolvedValue({responses: [data], channelById: new Map()});

			await service.saveMessage(saveMessageArgs(message.id));

			expect(sent).toEqual([{...data, __thread_scoped: '900'}]);
		});

		it('scopes a thread created message without an unscoped copy', async () => {
			const {sent, dispatch} = capture();
			const {service, message} = createDispatchingService(dispatch, {type: ChannelTypes.GUILD_TEXT, guildId: GUILD});
			const data = response({type: MessageTypes.THREAD_CREATED});
			vi.spyOn(service, 'buildUnmaskedResponses').mockResolvedValue({responses: [data], channelById: new Map()});

			await service.saveMessage(saveMessageArgs(message.id));

			expect(sent).toEqual([{...data, __thread_scoped: '900'}]);
		});

		it('sends viewers the thread flag and everyone else a masked copy', async () => {
			const {sent, dispatch} = capture();
			const {service, message} = createDispatchingService(dispatch, {type: ChannelTypes.GUILD_TEXT, guildId: GUILD});
			const data = response({flags: ServerMessageFlags.HAS_THREAD | 4});
			vi.spyOn(service, 'buildUnmaskedResponses').mockResolvedValue({responses: [data], channelById: new Map()});

			await service.saveMessage(saveMessageArgs(message.id));

			expect(sent).toEqual([
				{...data, __thread_scoped: '900'},
				{...data, flags: 4, __thread_unscoped: '900'},
			]);
		});
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

describe('buildMessageResponsesForUser thread masking', () => {
	const GUILD = createGuildID(900n);
	const HAS_THREAD = ServerMessageFlags.HAS_THREAD;

	afterEach(() => {
		syncChannelThreadsConfig(null, (raw) => ChannelThreadsConfigSchema.parse(raw ? JSON.parse(raw) : {}));
	});

	function build(config: Record<string, unknown>) {
		syncChannelThreadsConfig(JSON.stringify(config), (raw) => ChannelThreadsConfigSchema.parse(JSON.parse(raw!)));
		const {service} = createUserContentService({entries: [], readable: []});
		const threadCreated = {id: '40', channel_id: '100', type: MessageTypes.THREAD_CREATED, flags: 0};
		const responses = [
			{id: '41', channel_id: '100', type: MessageTypes.DEFAULT, flags: HAS_THREAD | 4},
			{id: '42', channel_id: '100', type: MessageTypes.REPLY, flags: 0, referenced_message: threadCreated},
			{id: '43', channel_id: '300', type: MessageTypes.DEFAULT, flags: HAS_THREAD},
		] as Array<MessageResponse>;
		const channelById = new Map([
			['100', {id: CHANNEL_A, guildId: GUILD}],
			['300', {id: CHANNEL_C, guildId: null}],
		]) as unknown as Map<string, Channel>;
		vi.spyOn(service, 'buildUnmaskedResponses').mockResolvedValue({responses, channelById});
		return {service, responses};
	}

	const viewer = {kind: 'user', userId: VIEWER_ID, bot: false, capable: true} as const;

	it('masks the parent flag and a thread created reply for a non-viewer', async () => {
		const {service} = build({enabled: true, ever_enabled: true, enabled_guild_ids: ['900']});

		const out = await service.buildMessageResponsesForUser(VIEWER_ID, viewer, []);

		expect(out.map((response) => [response.id, response.flags, response.referenced_message])).toEqual([
			['41', 4, undefined],
			['42', 0, null],
			['43', 0, undefined],
		]);
	});

	it('masks a killed tainted guild for everyone', async () => {
		const {service} = build({enabled: false, ever_enabled: true});

		const out = await service.buildMessageResponsesForUser(VIEWER_ID, viewer, []);

		expect(out.map((response) => response.flags)).toEqual([4, 0, 0]);
	});

	it('leaves guild responses untouched for an active viewer and still masks a DM', async () => {
		const {service, responses} = build({
			enabled: true,
			ever_enabled: true,
			enabled_guild_ids: ['900'],
			included_user_ids: [VIEWER_ID.toString()],
		});

		const out = await service.buildMessageResponsesForUser(VIEWER_ID, viewer, []);

		expect(out.slice(0, 2)).toEqual(responses.slice(0, 2));
		expect(out[2]?.flags).toBe(0);
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
			channelData: {
				auth: {getChannelAuthenticated: async () => ({channel: {type: ChannelTypes.GUILD_TEXT, guildId: null}})},
			},
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
			{} as unknown as ChannelRepository,
			{} as unknown as KVBulkMessageDeletionQueueService,
			{getConfigSnapshot: () => snapshot} as unknown as LimitConfigService,
		);
		return {service, message, createdSavedMessageIds};
	}

	function saveMessageArgs(messageId: MessageID) {
		return {
			userId: VIEWER_ID,
			viewer: SYSTEM_THREAD_VIEWER,
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
		vi.spyOn(service, 'buildUnmaskedResponses').mockResolvedValue({responses: [], channelById: new Map()});

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
		vi.spyOn(service, 'buildUnmaskedResponses').mockResolvedValue({responses: [], channelById: new Map()});

		await expect(service.saveMessage(saveMessageArgs(message.id))).resolves.toBeUndefined();

		expect(createdSavedMessageIds).toEqual([message.id.toString()]);
	});
});
