/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import fs from 'node:fs/promises';
import {
	type ChannelID,
	createChannelID,
	createGuildID,
	createWebhookID,
	createWebhookToken,
	type GuildID,
	type UserID,
	type WebhookID,
	type WebhookToken,
} from '~/BrandedTypes';
import {MAX_WEBHOOKS_PER_CHANNEL, MAX_WEBHOOKS_PER_GUILD, Permissions} from '~/Constants';
import type {AllowedMentionsRequest, MessageRequest} from '~/channel/ChannelModel';
import type {IChannelRepository} from '~/channel/IChannelRepository';
import type {ChannelService} from '~/channel/services/ChannelService';
import {AuditLogActionType} from '~/constants/AuditLogActionType';
import {MaxWebhooksPerChannelError, MaxWebhooksPerGuildError, UnknownChannelError, UnknownWebhookError} from '~/Errors';
import type {GuildAuditLogService} from '~/guild/GuildAuditLogService';
import type {GuildService} from '~/guild/services/GuildService';
import type {AvatarService} from '~/infrastructure/AvatarService';
import type {ICacheService} from '~/infrastructure/ICacheService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import {Logger} from '~/Logger';
import type {Message, Webhook} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import * as RandomUtils from '~/utils/RandomUtils';
import * as GitHubTransformer from '~/webhook/transformers/GitHubTransformer';
import type {WebhookCreateRequest, WebhookMessageRequest, WebhookUpdateRequest} from '~/webhook/WebhookModel';
import type {IWebhookRepository} from './IWebhookRepository';

export class WebhookService {
	private static readonly NO_ALLOWED_MENTIONS: AllowedMentionsRequest = {parse: []};
	constructor(
		private repository: IWebhookRepository,
		private guildService: GuildService,
		private channelService: ChannelService,
		private channelRepository: IChannelRepository,
		private cacheService: ICacheService,
		private gatewayService: IGatewayService,
		private avatarService: AvatarService,
		private mediaService: IMediaService,
		private snowflakeService: SnowflakeService,
		private readonly guildAuditLogService: GuildAuditLogService,
	) {}

	async getWebhook({userId, webhookId}: {userId: UserID; webhookId: WebhookID}): Promise<Webhook> {
		return await this.getAuthenticatedWebhook({userId, webhookId});
	}

	async getWebhookByToken({webhookId, token}: {webhookId: WebhookID; token: WebhookToken}): Promise<Webhook> {
		const webhook = await this.repository.findByToken(webhookId, token);
		if (!webhook) throw new UnknownWebhookError();
		return webhook;
	}

	async getGuildWebhooks({userId, guildId}: {userId: UserID; guildId: GuildID}): Promise<Array<Webhook>> {
		const {checkPermission} = await this.guildService.getGuildAuthenticated({userId, guildId});
		await checkPermission(Permissions.MANAGE_WEBHOOKS);
		return await this.repository.listByGuild(guildId);
	}

	async getChannelWebhooks({userId, channelId}: {userId: UserID; channelId: ChannelID}): Promise<Array<Webhook>> {
		const channel = await this.channelService.getChannel({userId, channelId});
		if (!channel.guildId) throw new UnknownChannelError();
		const {checkPermission} = await this.guildService.getGuildAuthenticated({userId, guildId: channel.guildId});
		await checkPermission(Permissions.MANAGE_WEBHOOKS);
		return await this.repository.listByChannel(channelId);
	}

	async createWebhook(
		params: {userId: UserID; channelId: ChannelID; data: WebhookCreateRequest},
		auditLogReason?: string | null,
	): Promise<Webhook> {
		const {userId, channelId, data} = params;
		const channel = await this.channelService.getChannel({userId, channelId});
		if (!channel.guildId) throw new UnknownChannelError();
		const {checkPermission} = await this.guildService.getGuildAuthenticated({userId, guildId: channel.guildId});
		await checkPermission(Permissions.MANAGE_WEBHOOKS);
		const guildWebhookCount = await this.repository.countByGuild(channel.guildId);
		if (guildWebhookCount >= MAX_WEBHOOKS_PER_GUILD) {
			throw new MaxWebhooksPerGuildError();
		}
		const channelWebhookCount = await this.repository.countByChannel(channelId);
		if (channelWebhookCount >= MAX_WEBHOOKS_PER_CHANNEL) {
			throw new MaxWebhooksPerChannelError();
		}
		const webhookId = createWebhookID(this.snowflakeService.generate());
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
		params: {userId: UserID; webhookId: WebhookID; data: WebhookUpdateRequest},
		auditLogReason?: string | null,
	): Promise<Webhook> {
		const {userId, webhookId, data} = params;
		const webhook = await this.getAuthenticatedWebhook({userId, webhookId});
		const {checkPermission} = await this.guildService.getGuildAuthenticated({
			userId,
			guildId: webhook.guildId ? webhook.guildId : createGuildID(0n),
		});
		await checkPermission(Permissions.MANAGE_WEBHOOKS);
		if (data.channel_id && data.channel_id !== webhook.channelId) {
			const targetChannel = await this.channelService.getChannel({userId, channelId: createChannelID(data.channel_id)});
			if (!targetChannel.guildId || targetChannel.guildId !== webhook.guildId) {
				throw new UnknownChannelError();
			}
			const channelWebhookCount = await this.repository.countByChannel(createChannelID(data.channel_id));
			if (channelWebhookCount >= MAX_WEBHOOKS_PER_CHANNEL) {
				throw new MaxWebhooksPerChannelError();
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

	async updateWebhookByToken({
		webhookId,
		token,
		data,
	}: {
		webhookId: WebhookID;
		token: WebhookToken;
		data: WebhookUpdateRequest;
	}): Promise<Webhook> {
		const webhook = await this.repository.findByToken(webhookId, token);
		if (!webhook) throw new UnknownWebhookError();
		if (data.channel_id && data.channel_id !== webhook.channelId) {
			const targetChannel = await this.channelRepository.findUnique(createChannelID(data.channel_id));
			if (!targetChannel || !targetChannel.guildId || targetChannel.guildId !== webhook.guildId) {
				throw new UnknownChannelError();
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
		return updatedWebhook;
	}

	async deleteWebhook(
		{userId, webhookId}: {userId: UserID; webhookId: WebhookID},
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

	async deleteWebhookByToken({webhookId, token}: {webhookId: WebhookID; token: WebhookToken}): Promise<void> {
		const webhook = await this.repository.findByToken(webhookId, token);
		if (!webhook) throw new UnknownWebhookError();
		await this.repository.delete(webhookId);
		await this.dispatchWebhooksUpdate({
			guildId: webhook.guildId,
			channelId: webhook.channelId,
		});
	}

	async executeWebhook({
		webhookId,
		token,
		data,
		requestCache,
	}: {
		webhookId: WebhookID;
		token: WebhookToken;
		data: WebhookMessageRequest;
		requestCache: RequestCache;
	}): Promise<Message> {
		const webhook = await this.repository.findByToken(webhookId, token);
		if (!webhook) throw new UnknownWebhookError();
		const channel = await this.channelRepository.findUnique(webhook.channelId!);
		if (!channel || !channel.guildId) throw new UnknownChannelError();
		const message = await this.channelService.sendWebhookMessage({
			webhook,
			data: {
				...data,
				allowed_mentions: WebhookService.NO_ALLOWED_MENTIONS,
			} as MessageRequest,
			username: data.username,
			avatar: data.avatar_url ? await this.getWebhookAvatar({webhookId: webhook.id, avatarUrl: data.avatar_url}) : null,
			requestCache,
		});
		return message;
	}

	async executeGitHubWebhook(params: {
		webhookId: WebhookID;
		token: WebhookToken;
		event: string;
		delivery: string;
		data: GitHubTransformer.GitHubWebhook;
		requestCache: RequestCache;
	}): Promise<void> {
		const {webhookId, token, event, delivery, data, requestCache} = params;
		const webhook = await this.repository.findByToken(webhookId, token);
		if (!webhook) throw new UnknownWebhookError();
		const channel = await this.channelRepository.findUnique(webhook.channelId!);
		if (!channel || !channel.guildId) throw new UnknownChannelError();
		if (delivery) {
			const isCached = await this.cacheService.get<number>(`github:${webhookId}:${delivery}`);
			if (isCached) return;
		}
		const embed = await GitHubTransformer.transform(event, data);
		if (!embed) return;
		await this.channelService.sendWebhookMessage({
			webhook,
			data: {embeds: [embed], allowed_mentions: WebhookService.NO_ALLOWED_MENTIONS},
			username: 'GitHub',
			avatar: await this.getGitHubWebhookAvatar(webhook.id),
			requestCache,
		});
		if (delivery) await this.cacheService.set(`github:${webhookId}:${delivery}`, 1, 60 * 60 * 24);
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

	private async getAuthenticatedWebhook({userId, webhookId}: {userId: UserID; webhookId: WebhookID}): Promise<Webhook> {
		const webhook = await this.repository.findUnique(webhookId);
		if (!webhook) throw new UnknownWebhookError();
		const {checkPermission} = await this.guildService.getGuildAuthenticated({userId, guildId: webhook.guildId!});
		await checkPermission(Permissions.MANAGE_WEBHOOKS);
		return webhook;
	}

	private async updateWebhookData({
		webhook,
		data,
	}: {
		webhook: Webhook;
		data: WebhookUpdateRequest;
	}): Promise<{name: string; avatarHash: string | null; channelId: ChannelID | null}> {
		const name = data.name !== undefined ? data.name : webhook.name;
		const avatarHash =
			data.avatar !== undefined
				? await this.updateAvatar({webhookId: webhook.id, avatar: data.avatar})
				: webhook.avatarHash;
		let channelId = webhook.channelId;
		if (data.channel_id !== undefined && data.channel_id !== webhook.channelId) {
			const channel = await this.channelRepository.findUnique(createChannelID(data.channel_id));
			if (!channel || !channel.guildId || channel.guildId !== webhook.guildId) {
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
		const avatarCache = await this.cacheService.get<string | null>(`webhook:${webhookId}:avatar:${avatarUrl}`);
		if (avatarCache) return avatarCache;
		const metadata = await this.mediaService.getMetadata({
			type: 'external',
			url: avatarUrl,
			with_base64: true,
			isNSFWAllowed: false,
		});
		if (!metadata?.base64) {
			await this.cacheService.set(`webhook:${webhookId}:avatar:${avatarUrl}`, null, 60 * 5);
			return null;
		}
		const avatar = await this.avatarService.uploadAvatar({
			prefix: 'avatars',
			entityId: webhookId,
			errorPath: 'avatar',
			base64Image: metadata.base64,
		});
		await this.cacheService.set(`webhook:${webhookId}:avatar:${avatarUrl}`, avatar, 60 * 60 * 24);
		return avatar;
	}

	private async getGitHubWebhookAvatar(webhookId: WebhookID): Promise<string | null> {
		const avatarCache = await this.cacheService.get<string | null>(`webhook:${webhookId}:avatar:github`);
		if (avatarCache) return avatarCache;
		const avatarFile = await fs.readFile('./src/assets/github.webp');
		const avatar = await this.avatarService.uploadAvatar({
			prefix: 'avatars',
			entityId: webhookId,
			errorPath: 'avatar',
			base64Image: avatarFile.toString('base64'),
		});
		await this.cacheService.set(`webhook:${webhookId}:avatar:github`, avatar, 60 * 60 * 24);
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
}
