// SPDX-License-Identifier: AGPL-3.0-or-later

import fs from 'node:fs/promises';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {GUILD_TEXT_BASED_CHANNEL_TYPES, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {MAX_WEBHOOKS_PER_CHANNEL, MAX_WEBHOOKS_PER_GUILD} from '@fluxer/constants/src/LimitConstants';
import {MaxWebhooksPerChannelError} from '@fluxer/errors/src/domains/channel/MaxWebhooksPerChannelError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {MaxWebhooksPerGuildError} from '@fluxer/errors/src/domains/guild/MaxWebhooksPerGuildError';
import {UnknownWebhookError} from '@fluxer/errors/src/domains/webhook/UnknownWebhookError';
import type {AllowedMentionsRequest} from '@fluxer/schema/src/domains/message/SharedMessageSchemas';
import type {GitHubWebhook} from '@fluxer/schema/src/domains/webhook/GitHubWebhookSchemas';
import type {InstatusWebhook} from '@fluxer/schema/src/domains/webhook/InstatusWebhookSchemas';
import type {
	WebhookCreateRequest,
	WebhookMessageRequest,
	WebhookTokenUpdateRequest,
	WebhookUpdateRequest,
} from '@fluxer/schema/src/domains/webhook/WebhookRequestSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {seconds} from 'itty-time';
import type {ChannelID, GuildID, MessageID, UserID, WebhookID, WebhookToken} from '../BrandedTypes';
import {createChannelID, createGuildID, createWebhookID, createWebhookToken} from '../BrandedTypes';
import type {IChannelRepository} from '../channel/IChannelRepository';
import type {MessageRequest, MessageUpdateRequest} from '../channel/MessageTypes';
import type {ChannelService} from '../channel/services/ChannelService';
import type {GuildAuditLogService} from '../guild/GuildAuditLogService';
import type {GuildService} from '../guild/services/GuildService';
import type {AvatarService} from '../infrastructure/AvatarService';
import {contentModerationService} from '../infrastructure/ContentModerationService';
import type {IGatewayService} from '../infrastructure/IGatewayService';
import type {IMediaService} from '../infrastructure/IMediaService';
import type {ISnowflakeService} from '../infrastructure/ISnowflakeService';
import {Logger} from '../Logger';
import type {LimitConfigService} from '../limits/LimitConfigService';
import {resolveLimitSafe} from '../limits/LimitConfigUtils';
import {createLimitMatchContext} from '../limits/LimitMatchContextBuilder';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {Channel} from '../models/Channel';
import type {Message} from '../models/Message';
import type {Webhook} from '../models/Webhook';
import * as RandomUtils from '../utils/RandomUtils';
import type {IWebhookRepository} from './IWebhookRepository';
import {transform as GitHubTransform} from './transformers/GitHubTransformer';
import {transformInstatusWebhook} from './transformers/InstatusTransformer';

export interface WebhookExecuteMessageData extends Omit<WebhookMessageRequest, 'attachments'> {
	attachments?: WebhookMessageRequest['attachments'] | MessageRequest['attachments'];
	username?: string | null;
	avatar_url?: string | null;
}

interface WebhookUserParams {
	userId: UserID;
	webhookId: WebhookID;
}

interface WebhookTokenParams {
	webhookId: WebhookID;
	token: WebhookToken;
}

interface WebhookTokenUpdateParams extends WebhookTokenParams {
	data: WebhookTokenUpdateRequest;
}

interface WebhookExecuteParams extends WebhookTokenParams {
	data: WebhookExecuteMessageData;
	requestCache: RequestCache;
}

interface WebhookMessageLookupParams extends WebhookTokenParams {
	messageId: MessageID;
}

interface WebhookMessageParams extends WebhookMessageLookupParams {
	requestCache: RequestCache;
}

interface WebhookMessageUpdateParams extends WebhookMessageParams {
	data: MessageUpdateRequest;
}

interface WebhookExecuteGitHubParams extends WebhookTokenParams {
	event: string;
	delivery: string;
	data: GitHubWebhook;
	requestCache: RequestCache;
}

interface WebhookExecuteInstatusParams extends WebhookTokenParams {
	data: InstatusWebhook;
	requestCache: RequestCache;
}

const WEBHOOK_AVATAR_MISSING_CACHE_VALUE = '__fluxer_webhook_avatar_missing__';

export class WebhookService {
	private static readonly NO_ALLOWED_MENTIONS: AllowedMentionsRequest = {parse: []};

	private isUploadedAttachmentData(
		attachment: NonNullable<WebhookExecuteMessageData['attachments']>[number],
	): attachment is Extract<
		NonNullable<MessageRequest['attachments']>[number],
		{
			upload_filename: string;
		}
	> {
		return (
			typeof attachment === 'object' &&
			attachment !== null &&
			'upload_filename' in attachment &&
			typeof attachment.upload_filename === 'string'
		);
	}

	constructor(
		private repository: IWebhookRepository,
		private guildService: GuildService,
		private channelService: ChannelService,
		private channelRepository: IChannelRepository,
		private cacheService: ICacheService,
		private gatewayService: IGatewayService,
		private avatarService: AvatarService,
		private mediaService: IMediaService,
		private snowflakeService: ISnowflakeService,
		private readonly guildAuditLogService: GuildAuditLogService,
		private readonly limitConfigService: LimitConfigService,
	) {}

	async getWebhook({userId, webhookId}: WebhookUserParams): Promise<Webhook> {
		return this.getAuthenticatedWebhook({userId, webhookId});
	}

	async getWebhookByToken(params: WebhookTokenParams): Promise<Webhook> {
		return this.getTokenAuthenticatedWebhook(params);
	}

	async getGuildWebhooks({userId, guildId}: {userId: UserID; guildId: GuildID}): Promise<Array<Webhook>> {
		const {checkPermission} = await this.guildService.getGuildAuthenticated({userId, guildId});
		await checkPermission(Permissions.MANAGE_WEBHOOKS);
		return await this.repository.listByGuild(guildId);
	}

	async getChannelWebhooks({userId, channelId}: {userId: UserID; channelId: ChannelID}): Promise<Array<Webhook>> {
		const channel = await this.channelService.channelData.operations.getChannel({userId, channelId});
		this.assertWebhookTargetChannel(channel);
		const {checkPermission} = await this.guildService.getGuildAuthenticated({
			userId,
			guildId: channel.guildId,
		});
		await checkPermission(Permissions.MANAGE_WEBHOOKS);
		return await this.repository.listByChannel(channelId);
	}

	async createWebhook(
		params: {
			userId: UserID;
			channelId: ChannelID;
			data: WebhookCreateRequest;
		},
		auditLogReason?: string | null,
	): Promise<Webhook> {
		const {userId, channelId, data} = params;
		const channel = await this.channelService.channelData.operations.getChannel({userId, channelId});
		this.assertWebhookTargetChannel(channel);
		const {checkPermission, guildData} = await this.guildService.getGuildAuthenticated({
			userId,
			guildId: channel.guildId,
		});
		await checkPermission(Permissions.MANAGE_WEBHOOKS);
		const guildLimit = this.resolveWebhookLimit(guildData.features, 'max_webhooks_per_guild', MAX_WEBHOOKS_PER_GUILD);
		const guildWebhookCount = await this.repository.countByGuild(channel.guildId);
		if (guildWebhookCount >= guildLimit) {
			throw new MaxWebhooksPerGuildError(guildLimit);
		}
		const channelLimit = this.resolveWebhookLimit(
			guildData.features,
			'max_webhooks_per_channel',
			MAX_WEBHOOKS_PER_CHANNEL,
		);
		const channelWebhookCount = await this.repository.countByChannel(channelId);
		if (channelWebhookCount >= channelLimit) {
			throw new MaxWebhooksPerChannelError(channelLimit);
		}
		contentModerationService.scanText(data.name, {
			userId,
			guildId: channel.guildId,
			channelId,
			messageId: null,
			surface: 'webhook',
		});
		const webhookId = createWebhookID(await this.snowflakeService.generate());
		const webhook = await this.repository.create({
			webhookId,
			token: createWebhookToken(RandomUtils.randomString(64)),
			type: 1,
			guildId: channel.guildId,
			channelId,
			creatorId: userId,
			name: data.name,
			avatarHash: data.avatar ? await this.updateAvatar({webhookId, avatar: data.avatar}) : null,
		});
		await this.dispatchWebhooksUpdate({guildId: channel.guildId, channelId});
		await this.recordWebhookAuditLog({
			guildId: channel.guildId,
			userId,
			action: 'create',
			webhook,
			auditLogReason,
		});
		return webhook;
	}

	async updateWebhook(
		params: {
			userId: UserID;
			webhookId: WebhookID;
			data: WebhookUpdateRequest;
		},
		auditLogReason?: string | null,
	): Promise<Webhook> {
		const {userId, webhookId, data} = params;
		const webhook = await this.getAuthenticatedWebhook({userId, webhookId});
		const {checkPermission, guildData} = await this.guildService.getGuildAuthenticated({
			userId,
			guildId: webhook.guildId ? webhook.guildId : createGuildID(0n),
		});
		await checkPermission(Permissions.MANAGE_WEBHOOKS);
		if (data.channel_id && data.channel_id !== webhook.channelId) {
			const targetChannel = await this.channelService.channelData.operations.getChannel({
				userId,
				channelId: createChannelID(data.channel_id),
			});
			this.assertWebhookTargetChannel(targetChannel);
			if (targetChannel.guildId !== webhook.guildId) {
				throw new UnknownChannelError();
			}
			const canManageTargetChannel = await this.gatewayService.checkPermission({
				guildId: targetChannel.guildId,
				userId,
				permission: Permissions.MANAGE_WEBHOOKS,
				channelId: createChannelID(data.channel_id),
			});
			if (!canManageTargetChannel) {
				throw new MissingPermissionsError();
			}
			const channelLimit = this.resolveWebhookLimit(
				guildData.features,
				'max_webhooks_per_channel',
				MAX_WEBHOOKS_PER_CHANNEL,
			);
			const channelWebhookCount = await this.repository.countByChannel(createChannelID(data.channel_id));
			if (channelWebhookCount >= channelLimit) {
				throw new MaxWebhooksPerChannelError(channelLimit);
			}
		}
		const updatedData = await this.updateWebhookData({webhook, data});
		const updatedWebhook = await this.repository.update(webhookId, {
			name: updatedData.name,
			avatarHash: updatedData.avatarHash,
			channelId: updatedData.channelId,
		});
		if (!updatedWebhook) throw new UnknownWebhookError();
		await this.dispatchWebhooksUpdate({
			guildId: webhook.guildId,
			channelId: webhook.channelId,
		});
		if (webhook.guildId) {
			const previousSnapshot = this.serializeWebhookForAudit(webhook);
			await this.recordWebhookAuditLog({
				guildId: webhook.guildId,
				userId,
				action: 'update',
				webhook: updatedWebhook,
				previousSnapshot,
				auditLogReason,
			});
		}
		return updatedWebhook;
	}

	async updateWebhookByToken({webhookId, token, data}: WebhookTokenUpdateParams): Promise<Webhook> {
		const webhook = await this.getTokenAuthenticatedWebhook({webhookId, token});
		const updatedData = await this.updateWebhookData({webhook, data});
		const updatedWebhook = await this.repository.update(webhookId, {
			name: updatedData.name,
			avatarHash: updatedData.avatarHash,
			channelId: updatedData.channelId,
		});
		if (!updatedWebhook) throw new UnknownWebhookError();
		await this.dispatchWebhooksUpdate({
			guildId: webhook.guildId,
			channelId: webhook.channelId,
		});
		return updatedWebhook;
	}

	async deleteWebhook(
		{
			userId,
			webhookId,
		}: {
			userId: UserID;
			webhookId: WebhookID;
		},
		auditLogReason?: string | null,
	): Promise<void> {
		const webhook = await this.getAuthenticatedWebhook({userId, webhookId});
		const {checkPermission} = await this.guildService.getGuildAuthenticated({userId, guildId: webhook.guildId!});
		await checkPermission(Permissions.MANAGE_WEBHOOKS);
		await this.repository.delete(webhookId);
		await this.dispatchWebhooksUpdate({
			guildId: webhook.guildId,
			channelId: webhook.channelId,
		});
		if (webhook.guildId) {
			await this.recordWebhookAuditLog({
				guildId: webhook.guildId,
				userId,
				action: 'delete',
				webhook,
				auditLogReason,
			});
		}
	}

	async deleteWebhookByToken({webhookId, token}: WebhookTokenParams): Promise<void> {
		const webhook = await this.getTokenAuthenticatedWebhook({webhookId, token});
		await this.repository.delete(webhookId);
		await this.dispatchWebhooksUpdate({
			guildId: webhook.guildId,
			channelId: webhook.channelId,
		});
	}

	async executeWebhook({webhookId, token, data, requestCache}: WebhookExecuteParams): Promise<Message> {
		const webhook = await this.getTokenAuthenticatedWebhook({webhookId, token});
		await this.assertWebhookGuildChannel(webhook);
		const attachments = data.attachments?.filter((attachment) => this.isUploadedAttachmentData(attachment));
		return this.channelService.messages.send.sendWebhookMessage({
			webhook,
			data: {
				content: data.content,
				embeds: data.embeds,
				attachments,
				message_reference: data.message_reference,
				allowed_mentions: data.allowed_mentions ?? WebhookService.NO_ALLOWED_MENTIONS,
				flags: data.flags,
				nonce: data.nonce,
				favorite_meme_id: data.favorite_meme_id,
				sticker_ids: data.sticker_ids,
				tts: data.tts,
			},
			username: data.username,
			avatar: data.avatar_url ? await this.getWebhookAvatar({webhookId: webhook.id, avatarUrl: data.avatar_url}) : null,
			requestCache,
		});
	}

	async editWebhookMessage({
		webhookId,
		token,
		messageId,
		data,
		requestCache,
	}: WebhookMessageUpdateParams): Promise<Message> {
		const webhook = await this.getTokenAuthenticatedWebhook({webhookId, token});
		return this.channelService.messages.send.editWebhookMessage({
			webhook,
			messageId,
			data,
			requestCache,
		});
	}

	async deleteWebhookMessage({webhookId, token, messageId, requestCache}: WebhookMessageParams): Promise<void> {
		const webhook = await this.getTokenAuthenticatedWebhook({webhookId, token});
		await this.channelService.messages.deletion.deleteWebhookMessage({
			webhook,
			messageId,
			requestCache,
		});
	}

	async getWebhookMessage({webhookId, token, messageId}: WebhookMessageLookupParams): Promise<Message> {
		const webhook = await this.getTokenAuthenticatedWebhook({webhookId, token});
		if (!webhook.channelId) throw new UnknownChannelError();
		const message = await this.channelRepository.getMessage(webhook.channelId, messageId);
		if (!message) throw new UnknownMessageError();
		if (message.webhookId !== webhook.id) throw new MissingPermissionsError();
		return message;
	}

	async executeGitHubWebhook(params: WebhookExecuteGitHubParams): Promise<void> {
		const {webhookId, token, event, delivery, data, requestCache} = params;
		const webhook = await this.getTokenAuthenticatedWebhook({webhookId, token});
		await this.assertWebhookGuildChannel(webhook);
		if (delivery) {
			const isCached = await this.cacheService.get<number>(`github:${webhookId}:${delivery}`);
			if (isCached) return;
		}
		const embed = await GitHubTransform(event, data);
		if (!embed) return;
		await this.channelService.messages.send.sendWebhookMessage({
			webhook,
			data: {embeds: [embed], allowed_mentions: WebhookService.NO_ALLOWED_MENTIONS},
			username: 'GitHub',
			avatar: await this.getGitHubWebhookAvatar(webhook.id),
			requestCache,
		});
		if (delivery) await this.cacheService.set(`github:${webhookId}:${delivery}`, 1, seconds('1 day'));
	}

	async executeInstatusWebhook(params: WebhookExecuteInstatusParams): Promise<void> {
		const {webhookId, token, data, requestCache} = params;
		const webhook = await this.getTokenAuthenticatedWebhook({webhookId, token});
		await this.assertWebhookGuildChannel(webhook);
		const embed = transformInstatusWebhook(data);
		if (!embed) return;
		await this.channelService.messages.send.sendWebhookMessage({
			webhook,
			data: {embeds: [embed], allowed_mentions: WebhookService.NO_ALLOWED_MENTIONS},
			username: 'Instatus',
			avatar: await this.getInstatusWebhookAvatar(webhook.id),
			requestCache,
		});
	}

	async dispatchWebhooksUpdate({
		guildId,
		channelId,
	}: {
		guildId: GuildID | null;
		channelId: ChannelID | null;
	}): Promise<void> {
		if (guildId && channelId) {
			await this.gatewayService.dispatchGuild({
				guildId: guildId,
				event: 'WEBHOOKS_UPDATE',
				data: {channel_id: channelId.toString()},
			});
		}
	}

	private async getAuthenticatedWebhook({userId, webhookId}: WebhookUserParams): Promise<Webhook> {
		const webhook = await this.repository.findUnique(webhookId);
		if (!webhook) throw new UnknownWebhookError();
		const {checkPermission} = await this.guildService.getGuildAuthenticated({userId, guildId: webhook.guildId!});
		await checkPermission(Permissions.MANAGE_WEBHOOKS);
		return webhook;
	}

	private async getTokenAuthenticatedWebhook({webhookId, token}: WebhookTokenParams): Promise<Webhook> {
		const webhook = await this.repository.findByToken(webhookId, token);
		if (!webhook) throw new UnknownWebhookError();
		return webhook;
	}

	private async assertWebhookGuildChannel(webhook: Webhook): Promise<void> {
		if (!webhook.channelId) throw new UnknownChannelError();
		const channel = await this.channelRepository.findUnique(webhook.channelId);
		if (!channel) throw new UnknownChannelError();
		this.assertWebhookTargetChannel(channel);
	}

	private assertWebhookTargetChannel(channel: Channel): asserts channel is Channel & {guildId: GuildID} {
		if (!channel.guildId || !GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type)) {
			throw new UnknownChannelError();
		}
	}

	private async updateWebhookData({webhook, data}: {webhook: Webhook; data: WebhookUpdateRequest}): Promise<{
		name: string;
		avatarHash: string | null;
		channelId: ChannelID | null;
	}> {
		contentModerationService.scanText(data.name ?? null, {
			userId: webhook.creatorId,
			guildId: webhook.guildId,
			channelId: webhook.channelId,
			messageId: null,
			surface: 'webhook',
		});
		const name = data.name !== undefined ? data.name : webhook.name;
		const avatarHash =
			data.avatar !== undefined
				? await this.updateAvatar({webhookId: webhook.id, avatar: data.avatar})
				: webhook.avatarHash;
		let channelId = webhook.channelId;
		if (data.channel_id !== undefined && data.channel_id !== webhook.channelId) {
			const channel = await this.channelRepository.findUnique(createChannelID(data.channel_id));
			if (!channel) {
				throw new UnknownChannelError();
			}
			this.assertWebhookTargetChannel(channel);
			if (channel.guildId !== webhook.guildId) {
				throw new UnknownChannelError();
			}
			channelId = channel.id;
		}
		return {name: name!, avatarHash, channelId};
	}

	private async updateAvatar({
		webhookId,
		avatar,
	}: {
		webhookId: WebhookID;
		avatar: string | null;
	}): Promise<string | null> {
		return this.avatarService.uploadAvatar({
			prefix: 'avatars',
			entityId: webhookId,
			errorPath: 'avatar',
			base64Image: avatar,
		});
	}

	private async getWebhookAvatar({
		webhookId,
		avatarUrl,
	}: {
		webhookId: WebhookID;
		avatarUrl: string | null;
	}): Promise<string | null> {
		if (!avatarUrl) return null;
		try {
			const cacheKey = `webhook:${webhookId}:avatar:${avatarUrl}`;
			const avatarCache = await this.cacheService.get<string>(cacheKey);
			if (avatarCache === WEBHOOK_AVATAR_MISSING_CACHE_VALUE) return null;
			if (avatarCache) return avatarCache;
			const metadata = await this.mediaService.getMetadata({
				type: 'external',
				url: avatarUrl,
				with_base64: true,
				nsfw: 'block',
			});
			if (!metadata?.base64) {
				await this.cacheService.set(cacheKey, WEBHOOK_AVATAR_MISSING_CACHE_VALUE, seconds('5 minutes'));
				return null;
			}
			const avatar = await this.avatarService.uploadAvatar({
				prefix: 'avatars',
				entityId: webhookId,
				errorPath: 'avatar',
				base64Image: metadata.base64,
			});
			await this.cacheService.set(cacheKey, avatar, seconds('1 day'));
			return avatar;
		} catch (error) {
			Logger.warn(
				{error, webhookId: webhookId.toString(), avatarUrl},
				'Failed to fetch webhook avatar, proceeding without custom avatar',
			);
			return null;
		}
	}

	private async getGitHubWebhookAvatar(webhookId: WebhookID): Promise<string | null> {
		return this.getStaticWebhookAvatar({webhookId, provider: 'github'});
	}

	private async getInstatusWebhookAvatar(webhookId: WebhookID): Promise<string | null> {
		return this.getStaticWebhookAvatar({webhookId, provider: 'instatus'});
	}

	private async getStaticWebhookAvatar({
		webhookId,
		provider,
	}: {
		webhookId: WebhookID;
		provider: 'github' | 'instatus';
	}): Promise<string | null> {
		const cacheKey = `webhook:${webhookId}:avatar:${provider}`;
		const avatarCache = await this.cacheService.get<string | null>(cacheKey);
		if (avatarCache) return avatarCache;
		const avatarFile = await fs.readFile(new URL(`../assets/${provider}.webp`, import.meta.url));
		const avatar = await this.avatarService.uploadAvatar({
			prefix: 'avatars',
			entityId: webhookId,
			errorPath: 'avatar',
			base64Image: avatarFile.toString('base64'),
		});
		await this.cacheService.set(cacheKey, avatar, seconds('1 day'));
		return avatar;
	}

	private getWebhookMetadata(webhook: Webhook): Record<string, string> | undefined {
		if (!webhook.channelId) {
			return undefined;
		}
		return {channel_id: webhook.channelId.toString()};
	}

	private serializeWebhookForAudit(webhook: Webhook): Record<string, unknown> {
		return {
			id: webhook.id.toString(),
			guild_id: webhook.guildId?.toString() ?? null,
			channel_id: webhook.channelId?.toString() ?? null,
			name: webhook.name,
			creator_id: webhook.creatorId?.toString() ?? null,
			avatar_hash: webhook.avatarHash,
			type: webhook.type,
		};
	}

	private async recordWebhookAuditLog(params: {
		guildId: GuildID;
		userId: UserID;
		action: 'create' | 'update' | 'delete';
		webhook: Webhook;
		previousSnapshot?: Record<string, unknown> | null;
		auditLogReason?: string | null;
	}): Promise<void> {
		const actionName =
			params.action === 'create'
				? 'guild_webhook_create'
				: params.action === 'update'
					? 'guild_webhook_update'
					: 'guild_webhook_delete';
		const previousSnapshot =
			params.action === 'create' ? null : (params.previousSnapshot ?? this.serializeWebhookForAudit(params.webhook));
		const nextSnapshot = params.action === 'delete' ? null : this.serializeWebhookForAudit(params.webhook);
		const changes = this.guildAuditLogService.computeChanges(previousSnapshot, nextSnapshot);
		const actionType =
			params.action === 'create'
				? AuditLogActionType.WEBHOOK_CREATE
				: params.action === 'update'
					? AuditLogActionType.WEBHOOK_UPDATE
					: AuditLogActionType.WEBHOOK_DELETE;
		try {
			await this.guildAuditLogService
				.createBuilder(params.guildId, params.userId)
				.withAction(actionType, params.webhook.id.toString())
				.withReason(params.auditLogReason ?? null)
				.withMetadata(this.getWebhookMetadata(params.webhook))
				.withChanges(changes)
				.commit();
		} catch (error) {
			Logger.error(
				{
					error,
					guildId: params.guildId.toString(),
					userId: params.userId.toString(),
					action: actionName,
					targetId: params.webhook.id.toString(),
				},
				'Failed to record guild webhook audit log',
			);
		}
	}

	private resolveWebhookLimit(guildFeatures: Iterable<string> | null, key: LimitKey, fallback: number): number {
		const ctx = createLimitMatchContext({guildFeatures});
		return resolveLimitSafe(this.limitConfigService.getConfigSnapshot(), ctx, key, fallback);
	}
}
