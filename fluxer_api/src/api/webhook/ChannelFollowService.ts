// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, GuildID, UserID, WebhookID} from '@app/api/BrandedTypes';
import {createMessageID, createWebhookID, createWebhookToken} from '@app/api/BrandedTypes';
import type {IChannelRepository} from '@app/api/channel/IChannelRepository';
import {withChannelFollowLock} from '@app/api/channel/services/ChannelFollowers';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import type {ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import type {GuildService} from '@app/api/guild/services/GuildService';
import type {AvatarService} from '@app/api/infrastructure/AvatarService';
import {contentModerationService} from '@app/api/infrastructure/ContentModerationService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {Logger} from '@app/api/Logger';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {Webhook} from '@app/api/models/Webhook';
import * as RandomUtils from '@app/api/utils/RandomUtils';
import type {IWebhookRepository} from '@app/api/webhook/IWebhookRepository';
import type {WebhookService} from '@app/api/webhook/WebhookService';
import {
	CHANNEL_FOLLOWER_STATS_CACHE_SECONDS,
	FOLLOWER_WEBHOOK_NAME_MAX_LENGTH,
} from '@fluxer/constants/src/AnnouncementConstants';
import {
	CHANNEL_FOLLOW_TARGET_TYPES,
	ChannelTypes,
	MessageTypes,
	Permissions,
	WebhookTypes,
} from '@fluxer/constants/src/ChannelConstants';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {AnnouncementChannelRequiredError} from '@fluxer/errors/src/domains/channel/AnnouncementChannelRequiredError';
import {InvalidFollowTargetChannelError} from '@fluxer/errors/src/domains/channel/InvalidFollowTargetChannelError';
import {FeatureTemporarilyDisabledError} from '@fluxer/errors/src/domains/core/FeatureTemporarilyDisabledError';
import type {ChannelFollowerStatsResponse} from '@fluxer/schema/src/domains/channel/ChannelFollowSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {WebhookNameType} from '@fluxer/schema/src/primitives/UserValidators';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

interface FollowChannelParams {
	userId: UserID;
	viewer: ThreadViewer;
	channelId: ChannelID;
	webhookChannelId: ChannelID;
	requestCache: RequestCache;
	auditLogReason?: string | null;
}

interface FollowedChannel {
	channelId: ChannelID;
	webhookId: WebhookID;
}

type GuildChannel = Channel & {guildId: GuildID};

function truncateToLength(value: string, maxLength: number): string {
	let result = '';
	for (const codePoint of value) {
		if (result.length + codePoint.length > maxLength) break;
		result += codePoint;
	}
	return result;
}

export class ChannelFollowService {
	constructor(
		private readonly webhookService: WebhookService,
		private readonly webhookRepository: IWebhookRepository,
		private readonly channelService: ChannelService,
		private readonly channelRepository: IChannelRepository,
		private readonly guildService: GuildService,
		private readonly avatarService: AvatarService,
		private readonly cacheService: ICacheService,
		private readonly snowflakeService: ISnowflakeService,
	) {}

	async followChannel(params: FollowChannelParams): Promise<FollowedChannel> {
		const {userId, viewer, channelId, webhookChannelId, requestCache, auditLogReason} = params;
		const sourceAuth = await this.channelService.channelData.auth.getChannelAuthenticated({userId, channelId, viewer});
		const source = sourceAuth.channel;
		const sourceGuild = sourceAuth.guild;
		if (source.type !== ChannelTypes.GUILD_ANNOUNCEMENT || !source.guildId || !sourceGuild) {
			throw new AnnouncementChannelRequiredError();
		}
		if (sourceGuild.features.includes(GuildFeatures.ANNOUNCEMENT_CHANNELS_DISABLED)) {
			throw new FeatureTemporarilyDisabledError();
		}
		const targetAuth = await this.channelService.channelData.auth.getChannelAuthenticated({
			userId,
			channelId: webhookChannelId,
			viewer,
		});
		const target = targetAuth.channel;
		if (!target.guildId || !CHANNEL_FOLLOW_TARGET_TYPES.has(target.type)) {
			throw new InvalidFollowTargetChannelError();
		}
		const targetChannel = target as GuildChannel;
		const {checkPermission, guildData: targetGuild} = await this.guildService.getGuildAuthenticated({
			userId,
			guildId: targetChannel.guildId,
		});
		await checkPermission(Permissions.MANAGE_WEBHOOKS);
		await this.webhookService.assertChannelWebhookPermission({
			userId,
			guildId: targetChannel.guildId,
			channelId: targetChannel.id,
		});
		await this.webhookService.assertFollowContentRules({
			sourceChannel: source,
			sourceGuild,
			targetChannel,
			targetGuild,
		});
		const webhook = await withChannelFollowLock(this.cacheService, targetChannel.id, async () => {
			const current = await this.channelRepository.findUnique(targetChannel.id);
			if (!current?.guildId || !CHANNEL_FOLLOW_TARGET_TYPES.has(current.type)) {
				throw new InvalidFollowTargetChannelError();
			}
			await this.webhookService.assertChannelNotFollowing({
				channelId: targetChannel.id,
				sourceChannelId: source.id,
			});
			await this.webhookService.assertWebhookCapacity({
				guildId: targetChannel.guildId,
				channelId: targetChannel.id,
				guildFeatures: targetGuild.features,
			});
			const name = this.resolveFollowerWebhookName({
				sourceGuild,
				source,
				userId,
				targetChannel,
			});
			const webhookId = createWebhookID(await this.snowflakeService.generate());
			const avatarHash = await this.avatarService.copyGuildIconToWebhookAvatar({
				guildId: source.guildId!,
				iconHash: sourceGuild.icon ?? null,
				webhookId,
			});
			return this.webhookRepository.create({
				webhookId,
				token: createWebhookToken(RandomUtils.randomString(64)),
				type: WebhookTypes.CHANNEL_FOLLOWER,
				guildId: targetChannel.guildId,
				channelId: targetChannel.id,
				creatorId: userId,
				name,
				avatarHash,
				sourceGuildId: source.guildId,
				sourceChannelId: source.id,
			});
		});
		const currentSource = await this.channelRepository.findUnique(source.id);
		if (!currentSource || currentSource.type !== ChannelTypes.GUILD_ANNOUNCEMENT) {
			await this.webhookRepository.delete(webhook.id);
			throw new AnnouncementChannelRequiredError();
		}
		await this.cacheService.delete(this.followerStatsCacheKey(source.id));
		await this.webhookService.dispatchWebhooksUpdate({guildId: targetChannel.guildId, channelId: targetChannel.id});
		await this.webhookService.recordWebhookAuditLog({
			guildId: targetChannel.guildId,
			userId,
			action: 'create',
			webhook,
			auditLogReason,
		});
		await this.sendFollowSystemMessage({
			webhook,
			userId,
			source: currentSource,
			targetChannel,
			requestCache,
		});
		return {channelId: source.id, webhookId: webhook.id};
	}

	async getFollowerStats({
		userId,
		viewer,
		channelId,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
	}): Promise<ChannelFollowerStatsResponse> {
		const channel = await this.channelService.channelData.operations.getChannel({userId, viewer, channelId});
		if (channel.type !== ChannelTypes.GUILD_ANNOUNCEMENT) {
			throw new AnnouncementChannelRequiredError();
		}
		const cacheKey = this.followerStatsCacheKey(channelId);
		const cached = await this.cacheService.get<ChannelFollowerStatsResponse>(cacheKey);
		if (cached) return cached;
		const {channelCount, guildCount} = await this.webhookRepository.countBySourceChannel(channelId);
		const stats: ChannelFollowerStatsResponse = {channel_count: channelCount, guild_count: guildCount};
		await this.cacheService.set(cacheKey, stats, CHANNEL_FOLLOWER_STATS_CACHE_SECONDS);
		return stats;
	}

	private followerStatsCacheKey(channelId: ChannelID): string {
		return `channel-follower-stats:${channelId}`;
	}

	private resolveFollowerWebhookName({
		sourceGuild,
		source,
		userId,
		targetChannel,
	}: {
		sourceGuild: GuildResponse;
		source: Channel;
		userId: UserID;
		targetChannel: GuildChannel;
	}): string {
		const channelName = source.name ?? '';
		const candidates = [`${sourceGuild.name} #${channelName}`, `#${channelName}`];
		let lastError: unknown = null;
		for (const candidate of candidates) {
			const truncated = truncateToLength(candidate, FOLLOWER_WEBHOOK_NAME_MAX_LENGTH);
			try {
				const name = WebhookNameType.parse(truncated);
				contentModerationService.scanText(name, {
					userId,
					guildId: targetChannel.guildId,
					channelId: targetChannel.id,
					messageId: null,
					surface: 'webhook',
				});
				return name;
			} catch (error) {
				lastError = error;
			}
		}
		throw lastError;
	}

	private async sendFollowSystemMessage({
		webhook,
		userId,
		source,
		targetChannel,
		requestCache,
	}: {
		webhook: Webhook;
		userId: UserID;
		source: Channel;
		targetChannel: GuildChannel;
		requestCache: RequestCache;
	}): Promise<void> {
		try {
			const messageId = createMessageID(await this.snowflakeService.generateForChannel(targetChannel.id));
			const {message} = await this.channelService.messages.persistence.createMessage({
				messageId,
				channelId: targetChannel.id,
				userId,
				type: MessageTypes.CHANNEL_FOLLOW_ADD,
				content: webhook.name,
				flags: 0,
				messageReference: {
					channel_id: source.id,
					guild_id: source.guildId,
					message_id: null,
					type: 0,
				},
				guildId: targetChannel.guildId,
				channel: targetChannel,
				allowEmbeds: false,
			});
			await this.channelService.messages.dispatch.dispatchMessageCreate({
				channel: targetChannel,
				message,
				requestCache,
			});
		} catch (error) {
			Logger.error(
				{
					error,
					webhookId: webhook.id.toString(),
					channelId: targetChannel.id.toString(),
				},
				'Failed to send channel follow system message',
			);
		}
	}
}
