// SPDX-License-Identifier: AGPL-3.0-or-later

import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import type {ChannelID, GuildID, MessageID, UserID} from '../../../BrandedTypes';
import {createChannelID} from '../../../BrandedTypes';
import type {IChannelRepository} from '../../../channel/IChannelRepository';
import type {IPurgeQueue} from '../../../infrastructure/BunnyPurgeQueue';
import type {IGatewayService} from '../../../infrastructure/IGatewayService';
import type {IStorageService} from '../../../infrastructure/IStorageService';
import {Logger} from '../../../Logger';
import type {Message} from '../../../models/Message';
import {deleteMessageSearchDocuments} from '../../../search/MessageSearchIndexCleanup';
import {chunkArray} from '../../../utils/ArrayUtils';
import {ChannelEventDispatcher} from '../../../worker/services/ChannelEventDispatcher';
import {purgeMessageAttachments} from './MessageHelpers';
import {
	isChannelEligible,
	isTimestampInWindow,
	type SelfMessageEligibilityContext,
	type SelfMessageFilter,
} from './SelfMessageFilter';

interface UserMessageDeletionServiceDeps {
	channelRepository: IChannelRepository;
	gatewayService: IGatewayService;
	storageService: IStorageService;
	purgeQueue: IPurgeQueue;
}

interface DeleteUserMessagesScope {
	channelIds?: ReadonlyArray<ChannelID>;
	guildId?: GuildID;
}

interface BulkDeleteUserMessagesOptions {
	beforeTimestamp?: number;
	channelIdAllowlist?: ReadonlySet<string>;
	onProgress?: (deleted: number) => void;
}
type DeleteSelfMessagesFilter = SelfMessageFilter;
type DeleteSelfMessagesEligibilityContext = SelfMessageEligibilityContext;

interface DeleteSelfMessagesResult {
	totalDeleted: number;
	channelCount: number;
}

interface MessageWithChannel {
	channelId: ChannelID;
	messageId: MessageID;
	message: Message;
}

export class UserMessageDeletionService {
	private readonly eventDispatcher: ChannelEventDispatcher;
	private readonly FETCH_BATCH_SIZE = 100;
	private readonly DELETE_BATCH_SIZE = 100;

	constructor(private readonly deps: UserMessageDeletionServiceDeps) {
		this.eventDispatcher = new ChannelEventDispatcher({gatewayService: deps.gatewayService});
	}

	async deleteUserMessagesInScope(
		userId: UserID,
		scope: DeleteUserMessagesScope,
		options: Omit<BulkDeleteUserMessagesOptions, 'channelIdAllowlist'> = {},
	): Promise<number> {
		const channelIdAllowlist = await this.resolveChannelAllowlist(scope);
		if (channelIdAllowlist.size === 0) {
			Logger.debug({userId: userId.toString()}, 'No channels in scope for bulk user message deletion');
			return 0;
		}
		return this.deleteUserMessagesBulk(userId, {...options, channelIdAllowlist});
	}

	async deleteUserMessagesBulk(userId: UserID, options: BulkDeleteUserMessagesOptions = {}): Promise<number> {
		const {beforeTimestamp = Number.POSITIVE_INFINITY, channelIdAllowlist, onProgress} = options;
		Logger.debug({userId, beforeTimestamp}, 'Starting bulk user message deletion');
		const messagesByChannel = await this.collectUserMessages(userId, beforeTimestamp, channelIdAllowlist);
		let totalDeleted = 0;
		for (const [channelIdStr, messages] of messagesByChannel.entries()) {
			const deleted = await this.deleteMessagesInChannel(channelIdStr, messages);
			totalDeleted += deleted;
			onProgress?.(totalDeleted);
		}
		Logger.debug({userId, totalDeleted}, 'Bulk user message deletion completed');
		return totalDeleted;
	}

	async deleteUserMessagesFiltered(
		userId: UserID,
		filter: DeleteSelfMessagesFilter,
		context: DeleteSelfMessagesEligibilityContext,
		onProgress?: (deleted: number) => void,
	): Promise<DeleteSelfMessagesResult> {
		Logger.debug({userId, filter}, 'Starting filtered user message deletion');
		const messagesByChannel = await this.collectMessagesMatchingFilter(userId, filter, context);
		let totalDeleted = 0;
		let channelCount = 0;
		for (const [channelIdStr, messages] of messagesByChannel.entries()) {
			const deleted = await this.deleteMessagesInChannel(channelIdStr, messages);
			if (deleted > 0) {
				channelCount += 1;
			}
			totalDeleted += deleted;
			onProgress?.(totalDeleted);
		}
		Logger.debug({userId, totalDeleted, channelCount}, 'Filtered user message deletion completed');
		return {totalDeleted, channelCount};
	}

	private async collectMessagesMatchingFilter(
		userId: UserID,
		filter: DeleteSelfMessagesFilter,
		context: DeleteSelfMessagesEligibilityContext,
	): Promise<Map<string, Array<MessageWithChannel>>> {
		const messagesByChannel = new Map<string, Array<MessageWithChannel>>();
		const channelEligibility = new Map<string, boolean>();
		let lastMessageId: MessageID | undefined;
		while (true) {
			const messageRefs = await this.deps.channelRepository.listMessagesByAuthor(
				userId,
				this.FETCH_BATCH_SIZE,
				lastMessageId,
			);
			if (messageRefs.length === 0) {
				break;
			}
			for (const {channelId, messageId} of messageRefs) {
				const ts = snowflakeToDate(messageId).getTime();
				if (!isTimestampInWindow(ts, filter)) {
					continue;
				}
				const channelIdStr = channelId.toString();
				let eligible = channelEligibility.get(channelIdStr);
				if (eligible === undefined) {
					const channel = await this.deps.channelRepository.findUnique(channelId);
					eligible = channel ? isChannelEligible(channel, userId, filter, context) : false;
					channelEligibility.set(channelIdStr, eligible);
				}
				if (!eligible) {
					continue;
				}
				const message = await this.deps.channelRepository.getMessage(channelId, messageId);
				if (message && message.authorId === userId) {
					if (!messagesByChannel.has(channelIdStr)) {
						messagesByChannel.set(channelIdStr, []);
					}
					messagesByChannel.get(channelIdStr)!.push({channelId, messageId, message});
				}
			}
			lastMessageId = messageRefs[messageRefs.length - 1]!.messageId;
		}
		return messagesByChannel;
	}

	private async resolveChannelAllowlist(scope: DeleteUserMessagesScope): Promise<ReadonlySet<string>> {
		const allowlist = new Set<string>();
		if (scope.channelIds) {
			for (const channelId of scope.channelIds) {
				allowlist.add(channelId.toString());
			}
		}
		if (scope.guildId) {
			const channels = await this.deps.channelRepository.channelData.listGuildChannels(scope.guildId);
			for (const channel of channels) {
				allowlist.add(channel.id.toString());
			}
		}
		return allowlist;
	}

	private async collectUserMessages(
		userId: UserID,
		beforeTimestamp: number,
		channelIdAllowlist?: ReadonlySet<string>,
	): Promise<Map<string, Array<MessageWithChannel>>> {
		const messagesByChannel = new Map<string, Array<MessageWithChannel>>();
		let lastMessageId: MessageID | undefined;
		while (true) {
			const messageRefs = await this.deps.channelRepository.listMessagesByAuthor(
				userId,
				this.FETCH_BATCH_SIZE,
				lastMessageId,
			);
			if (messageRefs.length === 0) {
				break;
			}
			for (const {channelId, messageId} of messageRefs) {
				if (channelIdAllowlist && !channelIdAllowlist.has(channelId.toString())) {
					continue;
				}
				const messageTimestamp = snowflakeToDate(messageId).getTime();
				if (messageTimestamp > beforeTimestamp) {
					continue;
				}
				const message = await this.deps.channelRepository.getMessage(channelId, messageId);
				if (message && message.authorId === userId) {
					const channelIdStr = channelId.toString();
					if (!messagesByChannel.has(channelIdStr)) {
						messagesByChannel.set(channelIdStr, []);
					}
					messagesByChannel.get(channelIdStr)!.push({channelId, messageId, message});
				}
			}
			lastMessageId = messageRefs[messageRefs.length - 1]!.messageId;
		}
		return messagesByChannel;
	}

	private async deleteMessagesInChannel(channelIdStr: string, messages: Array<MessageWithChannel>): Promise<number> {
		if (messages.length === 0) {
			return 0;
		}
		const channelId = createChannelID(BigInt(channelIdStr));
		const channel = await this.deps.channelRepository.findUnique(channelId);
		if (!channel) {
			Logger.debug({channelId: channelIdStr}, 'Channel not found, skipping messages');
			return 0;
		}
		let deleted = 0;
		const batches = chunkArray(messages, this.DELETE_BATCH_SIZE);
		for (const batch of batches) {
			const messageIds = batch.map((m: MessageWithChannel) => m.messageId);
			const messageObjects = batch.map((m: MessageWithChannel) => m.message);
			await Promise.all(
				messageObjects.map((message: Message) =>
					purgeMessageAttachments(message, this.deps.storageService, this.deps.purgeQueue),
				),
			);
			await this.deps.channelRepository.bulkDeleteMessages(channelId, messageIds);
			await this.eventDispatcher.dispatchBulkDelete(channel, messageIds);
			await deleteMessageSearchDocuments(messageIds, {context: {source: 'bulk_user_message_delete'}});
			deleted += batch.length;
		}
		return deleted;
	}
}
