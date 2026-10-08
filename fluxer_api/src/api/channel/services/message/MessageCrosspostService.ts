// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, MessageID, UserID} from '@app/api/BrandedTypes';
import type {IChannelRepositoryAggregate} from '@app/api/channel/repositories/IChannelRepositoryAggregate';
import type {AuthenticatedChannel} from '@app/api/channel/services/AuthenticatedChannel';
import {collectEmbedContentHashes} from '@app/api/channel/services/message/CrosspostEmbedObjects';
import {
	type CrosspostPropagation,
	createCrosspostRateLimitError,
	isCrosspostedMessage,
	withPeekRetryAfter,
} from '@app/api/channel/services/message/CrosspostPropagation';
import type {MessageChannelAuthService} from '@app/api/channel/services/message/MessageChannelAuthService';
import type {MessageDispatchService} from '@app/api/channel/services/message/MessageDispatchService';
import {isCrosspostCopy, isOperationDisabled} from '@app/api/channel/services/message/MessageHelpers';
import {assertMessageWithinHistoryCutoff} from '@app/api/channel/services/message/MessageHistoryCutoff';
import type {MessageWriteLock} from '@app/api/channel/services/message/MessageWriteLock';
import type {ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import {contentModerationService} from '@app/api/infrastructure/ContentModerationService';
import {Logger} from '@app/api/Logger';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Message} from '@app/api/models/Message';
import {assertGuildMemberCanCommunicate} from '@app/api/utils/GuildCommunicationUtils';
import {WorkerQueueOverflowError} from '@app/api/worker/WorkerQueueOverflowError';
import {CROSSPOST_CHANNEL_RATE_LIMIT} from '@fluxer/constants/src/AnnouncementConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, MessageFlags, MessageTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures, GuildOperations} from '@fluxer/constants/src/GuildConstants';
import {AnnouncementChannelRequiredError} from '@fluxer/errors/src/domains/channel/AnnouncementChannelRequiredError';
import {MessageAlreadyCrosspostedError} from '@fluxer/errors/src/domains/channel/MessageAlreadyCrosspostedError';
import {MessageNotCrosspostableError} from '@fluxer/errors/src/domains/channel/MessageNotCrosspostableError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import {ServiceUnavailableError} from '@fluxer/errors/src/HttpErrors';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';

interface MessageCrosspostServiceDeps {
	channelRepository: IChannelRepositoryAggregate;
	channelAuthService: MessageChannelAuthService;
	dispatchService: MessageDispatchService;
	rateLimitService: IRateLimitService;
	messageWriteLock: MessageWriteLock;
	crosspostPropagation: CrosspostPropagation;
}

interface CrosspostMessageResult {
	message: Message;
	authChannel: AuthenticatedChannel;
}

export function crosspostChannelRateLimitIdentifier(channelId: ChannelID): string {
	return `crosspost:channel:${channelId}`;
}

export class MessageCrosspostService {
	constructor(private readonly deps: MessageCrosspostServiceDeps) {}

	async crosspostMessage({
		userId,
		viewer,
		channelId,
		messageId,
		requestCache,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		messageId: MessageID;
		requestCache: RequestCache;
	}): Promise<CrosspostMessageResult> {
		const authChannel = await this.deps.channelAuthService.getChannelAuthenticated({userId, channelId, viewer});
		const {channel, guild, member, hasPermission, checkPermission} = authChannel;
		if (channel.type !== ChannelTypes.GUILD_ANNOUNCEMENT) {
			throw new AnnouncementChannelRequiredError();
		}
		if (!guild) {
			throw new UnknownChannelError();
		}
		if (
			isOperationDisabled(guild, GuildOperations.SEND_MESSAGE) ||
			guild.features.includes(GuildFeatures.ANNOUNCEMENT_CHANNELS_DISABLED)
		) {
			throw new FeatureTemporarilyDisabledError();
		}
		const message = await this.deps.channelRepository.messages.getMessage(channelId, messageId);
		if (!message) {
			throw new UnknownMessageError();
		}
		await checkPermission(Permissions.SEND_MESSAGES);
		if (message.authorId === userId) {
			assertGuildMemberCanCommunicate(member);
		} else {
			await checkPermission(Permissions.MANAGE_MESSAGES);
			if (!(await hasPermission(Permissions.READ_MESSAGE_HISTORY))) {
				assertMessageWithinHistoryCutoff({message, guild});
			}
		}
		this.assertCrosspostable(message);
		this.assertNotBlocked(message, guild);
		await this.assertBudgetAvailable(channelId);
		const published = await this.deps.messageWriteLock.withFreshMessage(channelId, messageId, async (fresh) => {
			if (!fresh) {
				throw new UnknownMessageError();
			}
			this.assertCrosspostable(fresh);
			await this.assertBudgetAvailable(channelId);
			const updated = await this.deps.channelRepository.messages.upsertMessage(
				{...fresh.toRow(), flags: fresh.flags | MessageFlags.CROSSPOSTED},
				fresh.toRow(),
			);
			await this.deps.channelRepository.crossposts.addSource({sourceChannelId: channelId, sourceMessageId: messageId});
			return updated;
		});
		Logger.info(
			{
				actorId: userId.toString(),
				guildId: guild.id,
				channelId: channelId.toString(),
				messageId: messageId.toString(),
			},
			'message published',
		);
		await this.deps.dispatchService.dispatchMessageUpdate({channel, message: published, requestCache});
		try {
			await this.deps.crosspostPropagation.enqueueCrosspostFanout({channelId, messageId});
		} catch (error) {
			Logger.error(
				{error, channelId: channelId.toString(), messageId: messageId.toString()},
				'Failed to enqueue crosspost fan-out',
			);
			const reverted = await this.revertPublish({channelId, messageId});
			if (reverted) {
				await this.deps.dispatchService.dispatchMessageUpdate({channel, message: reverted, requestCache});
			}
			if (error instanceof WorkerQueueOverflowError) {
				throw new ServiceUnavailableError();
			}
			throw error;
		}
		await this.deps.rateLimitService.checkLimit(this.channelBudgetConfig(channelId));
		return {message: published, authChannel};
	}

	private assertCrosspostable(message: Message): void {
		if (isCrosspostCopy(message) || message.type !== MessageTypes.DEFAULT || message.messageSnapshots.length > 0) {
			throw new MessageNotCrosspostableError();
		}
		if (isCrosspostedMessage(message)) {
			throw new MessageAlreadyCrosspostedError();
		}
	}

	private assertNotBlocked(message: Message, guild: GuildResponse): void {
		const context = {
			userId: message.authorId,
			guildId: BigInt(guild.id),
			channelId: message.channelId,
			messageId: message.id,
		};
		const textContext = {...context, surface: 'message_content' as const};
		contentModerationService.scanText(message.content, textContext);
		for (const embed of message.embeds) {
			if (embed.type !== 'rich') continue;
			contentModerationService.scanText(embed.title, textContext);
			contentModerationService.scanText(embed.description, textContext);
			for (const field of embed.fields) {
				contentModerationService.scanText(field.name, textContext);
				contentModerationService.scanText(field.value, textContext);
			}
			contentModerationService.scanText(embed.footer?.text, textContext);
			contentModerationService.scanText(embed.author?.name, textContext);
		}
		for (const attachment of message.attachments) {
			if (attachment.contentHash) {
				contentModerationService.scanSha256(attachment.contentHash, {...context, surface: 'message_attachment'});
			}
		}
		for (const contentHash of collectEmbedContentHashes(message)) {
			contentModerationService.scanSha256(contentHash, {...context, surface: 'message_attachment'});
		}
	}

	private channelBudgetConfig(channelId: ChannelID) {
		return {identifier: crosspostChannelRateLimitIdentifier(channelId), ...CROSSPOST_CHANNEL_RATE_LIMIT};
	}

	private async assertBudgetAvailable(channelId: ChannelID): Promise<void> {
		const config = this.channelBudgetConfig(channelId);
		const peek = await this.deps.rateLimitService.peekLimit(config);
		if (peek.remaining < 1) {
			throw createCrosspostRateLimitError(
				APIErrorCodes.MESSAGE_CROSSPOST_RATE_LIMITED,
				withPeekRetryAfter(peek, config),
			);
		}
	}

	private async revertPublish({
		channelId,
		messageId,
	}: {
		channelId: ChannelID;
		messageId: MessageID;
	}): Promise<Message | null> {
		return this.deps.messageWriteLock.withFreshMessage(channelId, messageId, async (fresh) => {
			await this.deps.channelRepository.crossposts.deleteSource({
				sourceChannelId: channelId,
				sourceMessageId: messageId,
			});
			if (!fresh || !isCrosspostedMessage(fresh)) {
				return null;
			}
			return this.deps.channelRepository.messages.upsertMessage(
				{...fresh.toRow(), flags: fresh.flags & ~MessageFlags.CROSSPOSTED},
				fresh.toRow(),
			);
		});
	}
}
