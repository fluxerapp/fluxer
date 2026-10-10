// SPDX-License-Identifier: AGPL-3.0-or-later

import {stripOwnAttachmentSignature} from '@app/api/attachment/AttachmentUrls';
import type {ChannelID, GuildID, MessageID, UserID, WebhookID, WebhookToken} from '@app/api/BrandedTypes';
import {createChannelID, createGuildID, createUserID, createWebhookID, createWebhookToken} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {MessageRequest, MessageUpdateRequest} from '@app/api/channel/MessageTypes';
import {withChannelFollowLock} from '@app/api/channel/services/ChannelFollowers';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import type {ThreadService} from '@app/api/channel/services/thread/ThreadService';
import {webhookThreadIdRefusedTotal} from '@app/api/channel/threads/ThreadMetrics';
import {assertCrosspostContentRules} from '@app/api/channel/utils/CrosspostContentRules';
import {
	type ContentWarningChannelLike,
	channelToContentWarningView,
	guildResponseToContentWarningView,
} from '@app/api/channel/utils/EffectiveContentWarning';
import {
	everEnabled,
	guildActive,
	isTainted,
	type ThreadViewer,
	viewerActive,
} from '@app/api/experiment/ChannelThreadsGate';
import type {GuildAuditLogService} from '@app/api/guild/GuildAuditLogService';
import type {GuildService} from '@app/api/guild/services/GuildService';
import type {AvatarService} from '@app/api/infrastructure/AvatarService';
import {contentModerationService} from '@app/api/infrastructure/ContentModerationService';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {IMediaService} from '@app/api/infrastructure/IMediaService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import {Logger} from '@app/api/Logger';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';
import type {Webhook} from '@app/api/models/Webhook';
import * as RandomUtils from '@app/api/utils/RandomUtils';
import {inputValidationErrorFromZodIssues} from '@app/api/Validator';
import {transform as GitHubTransform} from '@app/api/webhook/transformers/GitHubTransformer';
import {instatusDeliveryKey, transformInstatusWebhook} from '@app/api/webhook/transformers/InstatusTransformer';
import type {WebhookRepository} from '@app/api/webhook/WebhookRepository';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {
	CHANNEL_FOLLOW_TARGET_TYPES,
	ChannelTypes,
	GUILD_TEXT_BASED_CHANNEL_TYPES,
	Permissions,
	WebhookTypes,
} from '@fluxer/constants/src/ChannelConstants';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {MAX_WEBHOOKS_PER_CHANNEL, MAX_WEBHOOKS_PER_GUILD} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {ChannelAlreadyFollowedError} from '@fluxer/errors/src/domains/channel/ChannelAlreadyFollowedError';
import {InvalidFollowTargetChannelError} from '@fluxer/errors/src/domains/channel/InvalidFollowTargetChannelError';
import {MaxWebhooksPerChannelError} from '@fluxer/errors/src/domains/channel/MaxWebhooksPerChannelError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {MaxWebhooksPerGuildError} from '@fluxer/errors/src/domains/guild/MaxWebhooksPerGuildError';
import {UnknownWebhookError} from '@fluxer/errors/src/domains/webhook/UnknownWebhookError';
import {WebhookForumTargetConflictError} from '@fluxer/errors/src/domains/webhook/WebhookForumTargetConflictError';
import {WebhookForumTargetRequiredError} from '@fluxer/errors/src/domains/webhook/WebhookForumTargetRequiredError';
import {WebhookServiceForumUnsupportedError} from '@fluxer/errors/src/domains/webhook/WebhookServiceForumUnsupportedError';
import {WebhookThreadNameRequiresForumError} from '@fluxer/errors/src/domains/webhook/WebhookThreadNameRequiresForumError';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {AllowedMentionsRequest} from '@fluxer/schema/src/domains/message/SharedMessageSchemas';
import type {GitHubWebhook} from '@fluxer/schema/src/domains/webhook/GitHubWebhookSchemas';
import type {InstatusWebhook} from '@fluxer/schema/src/domains/webhook/InstatusWebhookSchemas';
import {
	type WebhookCreateRequest,
	WebhookForumPostRequestFields,
	type WebhookMessageRequest,
	type WebhookTokenUpdateRequest,
	type WebhookUpdateRequest,
} from '@fluxer/schema/src/domains/webhook/WebhookRequestSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {seconds} from 'itty-time';
import {z} from 'zod';

export interface WebhookExecuteMessageData
	extends Omit<WebhookMessageRequest, 'attachments' | 'thread_name' | 'applied_tags'> {
	attachments?: WebhookMessageRequest['attachments'] | MessageRequest['attachments'];
	username?: string | null;
	avatar_url?: string | null;
	thread_name?: unknown;
	applied_tags?: unknown;
}

const WebhookForumPostFields = z.object(WebhookForumPostRequestFields);

type WebhookForumPost = z.infer<typeof WebhookForumPostFields>;

interface WebhookUserParams {
	userId: UserID;
	viewer: ThreadViewer;
	webhookId: WebhookID;
}

interface WebhookThreadParams {
	threadId?: string;
}

interface WebhookTokenParams {
	webhookId: WebhookID;
	token: WebhookToken;
}

interface WebhookTokenUpdateParams extends WebhookTokenParams {
	data: WebhookTokenUpdateRequest;
}

interface WebhookExecuteParams extends WebhookTokenParams, WebhookThreadParams {
	data: WebhookExecuteMessageData;
	requestCache: RequestCache;
	service?: boolean;
}

interface WebhookMessageLookupParams extends WebhookTokenParams, WebhookThreadParams {
	messageId: MessageID;
}

interface WebhookMessageParams extends WebhookMessageLookupParams {
	requestCache: RequestCache;
}

interface WebhookMessageUpdateParams extends WebhookMessageParams {
	data: MessageUpdateRequest;
}

interface WebhookExecuteGitHubParams extends WebhookTokenParams, WebhookThreadParams {
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
const HOSTED_WEBHOOK_AVATAR_URLS = {
	github: 'https://fluxer.app/static/img/app-webhook-github.2d0319169a3aee33.webp',
	instatus: 'https://fluxer.app/static/img/app-webhook-instatus.22ad5aee16da872c.webp',
} as const;

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
		private repository: WebhookRepository,
		private guildService: GuildService,
		private channelService: ChannelService,
		private channelRepository: ChannelRepository,
		private cacheService: ICacheService,
		private gatewayService: IGatewayService,
		private avatarService: AvatarService,
		private mediaService: IMediaService,
		private snowflakeService: ISnowflakeService,
		private readonly guildAuditLogService: GuildAuditLogService,
		private readonly limitConfigService: LimitConfigService,
		private readonly threadService?: () => ThreadService,
	) {}

	async getWebhook(params: WebhookUserParams): Promise<Webhook> {
		return this.getAuthenticatedWebhook(params);
	}

	async getWebhookByToken(params: WebhookTokenParams): Promise<Webhook> {
		return this.getTokenAuthenticatedWebhook(params);
	}

	async getGuildWebhooks({
		userId,
		guildId,
		viewer,
	}: {
		userId: UserID;
		guildId: GuildID;
		viewer: ThreadViewer;
	}): Promise<Array<Webhook>> {
		const {checkPermission} = await this.guildService.getGuildAuthenticated({userId, guildId});
		await checkPermission(Permissions.MANAGE_WEBHOOKS);
		const webhooks = await this.repository.listByGuild(guildId);
		const visibility = await Promise.all(
			webhooks.map((webhook) =>
				webhook.channelId
					? this.canManageChannelWebhooks({userId, guildId, channelId: webhook.channelId})
					: Promise.resolve(false),
			),
		);
		return this.withoutHiddenForumWebhooks(
			guildId,
			viewer,
			webhooks.filter((_webhook, index) => visibility[index]),
		);
	}

	async getChannelWebhooks({
		userId,
		channelId,
		viewer,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
	}): Promise<Array<Webhook>> {
		const channel = await this.channelService.channelData.operations.getChannel({userId, viewer, channelId});
		this.assertWebhookTargetChannel(channel);
		const {checkPermission} = await this.guildService.getGuildAuthenticated({
			userId,
			guildId: channel.guildId,
		});
		await checkPermission(Permissions.MANAGE_WEBHOOKS);
		await this.assertChannelWebhookPermission({userId, guildId: channel.guildId, channelId});
		return await this.repository.listByChannel(channelId);
	}

	async createWebhook(
		params: {
			userId: UserID;
			viewer: ThreadViewer;
			channelId: ChannelID;
			data: WebhookCreateRequest;
		},
		auditLogReason?: string | null,
	): Promise<Webhook> {
		const {userId, viewer, channelId, data} = params;
		const channel = await this.channelService.channelData.operations.getChannel({userId, viewer, channelId});
		this.assertWebhookTargetChannel(channel);
		const {checkPermission, guildData} = await this.guildService.getGuildAuthenticated({
			userId,
			guildId: channel.guildId,
		});
		await checkPermission(Permissions.MANAGE_WEBHOOKS);
		await this.assertChannelWebhookPermission({userId, guildId: channel.guildId, channelId});
		await this.assertWebhookCapacity({guildId: channel.guildId, channelId, guildFeatures: guildData.features});
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
			type: WebhookTypes.INCOMING,
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
			viewer: ThreadViewer;
			webhookId: WebhookID;
			data: WebhookUpdateRequest;
		},
		auditLogReason?: string | null,
	): Promise<Webhook> {
		const {userId, viewer, webhookId, data} = params;
		const webhook = await this.getAuthenticatedWebhook({userId, viewer, webhookId});
		const {checkPermission, guildData} = await this.guildService.getGuildAuthenticated({
			userId,
			guildId: webhook.guildId ? webhook.guildId : createGuildID(0n),
		});
		await checkPermission(Permissions.MANAGE_WEBHOOKS);
		const isFollower = webhook.type === WebhookTypes.CHANNEL_FOLLOWER;
		if (isFollower && data.avatar !== undefined) {
			throw InputValidationError.fromCode('avatar', ValidationErrorCodes.INVALID_FORMAT);
		}
		let followerMoveChannelId: ChannelID | null = null;
		if (data.channel_id && data.channel_id !== webhook.channelId) {
			const targetChannel = await this.channelService.channelData.operations.getChannel({
				userId,
				viewer,
				channelId: createChannelID(data.channel_id),
			});
			if (isFollower && !CHANNEL_FOLLOW_TARGET_TYPES.has(targetChannel.type)) {
				throw new InvalidFollowTargetChannelError();
			}
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
			if (isFollower) {
				await this.assertFollowerMoveContentRules({webhook, targetChannel, targetGuild: guildData, userId});
				followerMoveChannelId = targetChannel.id;
			}
		}
		const updatedData = await this.updateWebhookData({webhook, data});
		const writeUpdate = () =>
			this.repository.update(webhookId, {
				name: updatedData.name,
				avatarHash: updatedData.avatarHash,
				channelId: updatedData.channelId,
			});
		const moveChannelId = followerMoveChannelId;
		const updatedWebhook = moveChannelId
			? await withChannelFollowLock(this.cacheService, moveChannelId, async () => {
					const current = await this.channelRepository.channelData.findUnique(moveChannelId);
					if (!current || !CHANNEL_FOLLOW_TARGET_TYPES.has(current.type)) {
						throw new InvalidFollowTargetChannelError();
					}
					await this.assertChannelNotFollowing({
						channelId: moveChannelId,
						sourceChannelId: webhook.sourceChannelId,
						excludeWebhookId: webhook.id,
					});
					return writeUpdate();
				})
			: await writeUpdate();
		if (!updatedWebhook) throw new UnknownWebhookError();
		await this.dispatchWebhooksUpdate({
			guildId: webhook.guildId,
			channelId: webhook.channelId,
		});
		if (updatedWebhook.channelId && updatedWebhook.channelId !== webhook.channelId) {
			await this.dispatchWebhooksUpdate({
				guildId: updatedWebhook.guildId,
				channelId: updatedWebhook.channelId,
			});
		}
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

	async deleteWebhook({userId, viewer, webhookId}: WebhookUserParams, auditLogReason?: string | null): Promise<void> {
		const webhook = await this.getAuthenticatedWebhook({userId, viewer, webhookId});
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

	async executeWebhook({
		webhookId,
		token,
		threadId,
		data,
		requestCache,
		service,
	}: WebhookExecuteParams): Promise<Message> {
		const {webhook, channel} = await this.getTokenAuthenticatedWebhookChannel({webhookId, token});
		const forumPost = this.resolveForumPost(channel, data);
		if (channel.isThreadOnly()) {
			if (service && threadId === undefined) throw new WebhookServiceForumUnsupportedError();
			if (threadId !== undefined && forumPost?.thread_name !== undefined) throw new WebhookForumTargetConflictError();
			if (threadId === undefined && forumPost?.thread_name === undefined) throw new WebhookForumTargetRequiredError();
		}
		const thread = await this.resolveWebhookThread(webhook, threadId);
		const attachments = data.attachments?.filter((attachment) => this.isUploadedAttachmentData(attachment));
		const avatar = data.avatar_url
			? await this.getWebhookAvatar({webhookId: webhook.id, avatarUrl: data.avatar_url})
			: null;
		const messageData: MessageRequest = {
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
		};
		const threadName = forumPost?.thread_name;
		if (threadName !== undefined && !thread) {
			await this.channelService.messages.send.validateWebhookForumStarter({parent: channel, data: messageData});
			return this.createThreadService().creation.createWebhookForumPost({
				webhookId: createUserID(BigInt(webhook.id)),
				parent: channel,
				name: threadName,
				appliedTags: (forumPost?.applied_tags ?? []).map((id) => BigInt(id)),
				sendStarter: (created) =>
					this.channelService.messages.send.sendWebhookMessage({
						webhook,
						thread: created,
						data: messageData,
						username: data.username,
						avatar,
						requestCache,
						forumStarter: true,
					}),
			});
		}
		return this.channelService.messages.send.sendWebhookMessage({
			webhook,
			thread,
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
			avatar,
			requestCache,
		});
	}

	private resolveForumPost(channel: Channel, data: WebhookExecuteMessageData): WebhookForumPost | null {
		if (data.thread_name === undefined && data.applied_tags === undefined) return null;
		if (!guildActive(channel.guildId!)) return null;
		if (!channel.isThreadOnly()) throw new WebhookThreadNameRequiresForumError();
		const parsed = WebhookForumPostFields.safeParse({thread_name: data.thread_name, applied_tags: data.applied_tags});
		if (!parsed.success) throw inputValidationErrorFromZodIssues(parsed.error.issues);
		return parsed.data;
	}

	async editWebhookMessage({
		webhookId,
		token,
		threadId,
		messageId,
		data,
		requestCache,
	}: WebhookMessageUpdateParams): Promise<Message> {
		const webhook = await this.getTokenAuthenticatedWebhook({webhookId, token});
		const thread = await this.resolveWebhookThread(webhook, threadId);
		return this.channelService.messages.send.editWebhookMessage({
			webhook,
			thread,
			messageId,
			data,
			requestCache,
		});
	}

	async deleteWebhookMessage({
		webhookId,
		token,
		threadId,
		messageId,
		requestCache,
	}: WebhookMessageParams): Promise<void> {
		const webhook = await this.getTokenAuthenticatedWebhook({webhookId, token});
		const thread = await this.resolveWebhookThread(webhook, threadId);
		await this.channelService.messages.deletion.deleteWebhookMessage({
			webhook,
			thread,
			messageId,
			requestCache,
		});
	}

	async getWebhookMessage({webhookId, token, threadId, messageId}: WebhookMessageLookupParams): Promise<Message> {
		const webhook = await this.getTokenAuthenticatedWebhook({webhookId, token});
		if (!webhook.channelId) throw new UnknownChannelError();
		const thread = await this.resolveWebhookThread(webhook, threadId);
		const message = await this.channelRepository.messages.getMessage(thread?.id ?? webhook.channelId, messageId);
		if (!message) throw new UnknownMessageError();
		if (message.webhookId !== webhook.id) throw new MissingPermissionsError();
		return message;
	}

	async executeGitHubWebhook(params: WebhookExecuteGitHubParams): Promise<void> {
		const {webhookId, token, threadId, event, delivery, data, requestCache} = params;
		const {webhook, channel} = await this.getTokenAuthenticatedWebhookChannel({webhookId, token});
		if (channel.isThreadOnly() && threadId === undefined) throw new WebhookServiceForumUnsupportedError();
		const thread = await this.resolveWebhookThread(webhook, threadId);
		if (delivery) {
			const isCached = await this.cacheService.get<number>(`github:${webhookId}:${delivery}`);
			if (isCached) return;
		}
		const embed = await GitHubTransform(event, data);
		if (!embed) return;
		await this.channelService.messages.send.sendWebhookMessage({
			webhook,
			thread,
			data: {embeds: [embed], allowed_mentions: WebhookService.NO_ALLOWED_MENTIONS},
			username: 'GitHub',
			avatar: await this.getGitHubWebhookAvatar(webhook.id),
			requestCache,
		});
		if (delivery) await this.cacheService.set(`github:${webhookId}:${delivery}`, 1, seconds('1 day'));
	}

	async executeInstatusWebhook(params: WebhookExecuteInstatusParams): Promise<void> {
		const {webhookId, token, data, requestCache} = params;
		const {webhook, channel} = await this.getTokenAuthenticatedWebhookChannel({webhookId, token});
		if (channel.isThreadOnly()) throw new WebhookServiceForumUnsupportedError();
		const delivery = instatusDeliveryKey(data);
		if (delivery) {
			const isCached = await this.cacheService.get<number>(`instatus:${webhookId}:${delivery}`);
			if (isCached) return;
		}
		const embed = transformInstatusWebhook(data);
		if (!embed) return;
		await this.channelService.messages.send.sendWebhookMessage({
			webhook,
			data: {embeds: [embed], allowed_mentions: WebhookService.NO_ALLOWED_MENTIONS},
			username: 'Instatus',
			avatar: await this.getInstatusWebhookAvatar(webhook.id),
			requestCache,
		});
		if (delivery) await this.cacheService.set(`instatus:${webhookId}:${delivery}`, 1, seconds('1 day'));
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

	private async getAuthenticatedWebhook({userId, viewer, webhookId}: WebhookUserParams): Promise<Webhook> {
		const webhook = await this.repository.findUnique(webhookId);
		if (!webhook) throw new UnknownWebhookError();
		const {checkPermission} = await this.guildService.getGuildAuthenticated({userId, guildId: webhook.guildId!});
		await checkPermission(Permissions.MANAGE_WEBHOOKS);
		if (webhook.guildId && (await this.withoutHiddenForumWebhooks(webhook.guildId, viewer, [webhook])).length === 0) {
			throw new UnknownWebhookError();
		}
		if (webhook.guildId && webhook.channelId) {
			await this.assertChannelWebhookPermission({
				userId,
				guildId: webhook.guildId,
				channelId: webhook.channelId,
			});
		}
		return webhook;
	}

	async assertWebhookCapacity({
		guildId,
		channelId,
		guildFeatures,
	}: {
		guildId: GuildID;
		channelId: ChannelID;
		guildFeatures: Iterable<string> | null;
	}): Promise<void> {
		const guildLimit = this.resolveWebhookLimit(guildFeatures, 'max_webhooks_per_guild', MAX_WEBHOOKS_PER_GUILD);
		const guildWebhookCount = await this.repository.countByGuild(guildId);
		if (guildWebhookCount >= guildLimit) {
			throw new MaxWebhooksPerGuildError(guildLimit);
		}
		const channelLimit = this.resolveWebhookLimit(guildFeatures, 'max_webhooks_per_channel', MAX_WEBHOOKS_PER_CHANNEL);
		const channelWebhookCount = await this.repository.countByChannel(channelId);
		if (channelWebhookCount >= channelLimit) {
			throw new MaxWebhooksPerChannelError(channelLimit);
		}
	}

	async assertChannelNotFollowing({
		channelId,
		sourceChannelId,
		excludeWebhookId,
	}: {
		channelId: ChannelID;
		sourceChannelId: ChannelID | null;
		excludeWebhookId?: WebhookID;
	}): Promise<void> {
		if (!sourceChannelId) return;
		const webhooks = await this.repository.listByChannel(channelId);
		const duplicate = webhooks.some(
			(candidate) =>
				candidate.type === WebhookTypes.CHANNEL_FOLLOWER &&
				candidate.sourceChannelId === sourceChannelId &&
				candidate.id !== excludeWebhookId,
		);
		if (duplicate) throw new ChannelAlreadyFollowedError();
	}

	async assertFollowContentRules({
		sourceChannel,
		sourceGuild,
		targetChannel,
		targetGuild,
	}: {
		sourceChannel: Channel;
		sourceGuild: GuildResponse;
		targetChannel: Channel;
		targetGuild: GuildResponse;
	}): Promise<void> {
		const [sourceParent, targetParent] = await Promise.all([
			this.resolveParentContentView(sourceChannel),
			this.resolveParentContentView(targetChannel),
		]);
		assertCrosspostContentRules({
			source: channelToContentWarningView(sourceChannel),
			sourceParent,
			sourceGuild: guildResponseToContentWarningView(sourceGuild),
			target: channelToContentWarningView(targetChannel),
			targetParent,
			targetGuild: guildResponseToContentWarningView(targetGuild),
		});
	}

	private async resolveParentContentView(channel: Channel): Promise<ContentWarningChannelLike | null> {
		if (!channel.parentId || channel.type === ChannelTypes.GUILD_CATEGORY) return null;
		const parent = await this.channelRepository.channelData.findUnique(channel.parentId);
		return parent ? channelToContentWarningView(parent) : null;
	}

	private async assertFollowerMoveContentRules({
		webhook,
		targetChannel,
		targetGuild,
		userId,
	}: {
		webhook: Webhook;
		targetChannel: Channel;
		targetGuild: GuildResponse;
		userId: UserID;
	}): Promise<void> {
		if (!webhook.sourceChannelId || !webhook.sourceGuildId) return;
		const sourceChannel = await this.channelRepository.channelData.findUnique(webhook.sourceChannelId);
		if (!sourceChannel) return;
		const sourceGuild = await this.gatewayService
			.getGuildData({guildId: webhook.sourceGuildId, userId, skipMembershipCheck: true})
			.catch(() => null);
		if (!sourceGuild) return;
		await this.assertFollowContentRules({sourceChannel, sourceGuild, targetChannel, targetGuild});
	}

	private async canManageChannelWebhooks({
		userId,
		guildId,
		channelId,
	}: {
		userId: UserID;
		guildId: GuildID;
		channelId: ChannelID;
	}): Promise<boolean> {
		const [canView, canManage] = await Promise.all([
			this.gatewayService.checkPermission({guildId, userId, permission: Permissions.VIEW_CHANNEL, channelId}),
			this.gatewayService.checkPermission({guildId, userId, permission: Permissions.MANAGE_WEBHOOKS, channelId}),
		]);
		return canView && canManage;
	}

	async assertChannelWebhookPermission(params: {
		userId: UserID;
		guildId: GuildID;
		channelId: ChannelID;
	}): Promise<void> {
		const allowed = await this.canManageChannelWebhooks(params);
		if (!allowed) throw new MissingPermissionsError();
	}

	private async getTokenAuthenticatedWebhook({webhookId, token}: WebhookTokenParams): Promise<Webhook> {
		const webhook = await this.repository.findByToken(webhookId, token);
		if (!webhook || webhook.type !== WebhookTypes.INCOMING) throw new UnknownWebhookError();
		if (await this.isDormantForumWebhook(webhook)) throw new UnknownWebhookError();
		return webhook;
	}

	private async isDormantForumWebhook(webhook: Webhook): Promise<boolean> {
		if (!everEnabled() || !webhook.guildId || !webhook.channelId || guildActive(webhook.guildId)) return false;
		if (!(await isTainted(webhook.guildId))) return false;
		const channel = await this.channelRepository.channelData.findUnique(webhook.channelId);
		return channel?.isThreadOnly() ?? false;
	}

	private async withoutHiddenForumWebhooks(
		guildId: GuildID,
		viewer: ThreadViewer,
		webhooks: Array<Webhook>,
	): Promise<Array<Webhook>> {
		if (!everEnabled() || viewerActive(viewer, guildId) || !(await isTainted(guildId))) return webhooks;
		const channelIds = [...new Set(webhooks.flatMap((webhook) => (webhook.channelId ? [webhook.channelId] : [])))];
		if (channelIds.length === 0) return webhooks;
		const forumIds = new Set(
			(await this.channelRepository.channelData.listChannels(channelIds))
				.filter((channel) => channel.isThreadOnly())
				.map((channel) => channel.id),
		);
		if (forumIds.size === 0) return webhooks;
		return webhooks.filter((webhook) => !webhook.channelId || !forumIds.has(webhook.channelId));
	}

	private createThreadService(): ThreadService {
		if (!this.threadService) throw new UnknownChannelError();
		return this.threadService();
	}

	private async resolveWebhookThread(webhook: Webhook, threadId: string | undefined): Promise<Channel | null> {
		if (threadId === undefined || !everEnabled() || !webhook.guildId || !webhook.channelId) return null;
		if (!guildActive(webhook.guildId)) {
			if (!(await isTainted(webhook.guildId))) return null;
			webhookThreadIdRefusedTotal.inc();
			throw new UnknownChannelError();
		}
		if (!/^\d{1,20}$/.test(threadId)) throw new UnknownChannelError();
		const thread = await this.channelRepository.channelData.findUnique(createChannelID(BigInt(threadId)));
		if (!thread?.isThread() || thread.parentId !== webhook.channelId || thread.guildId !== webhook.guildId) {
			throw new UnknownChannelError();
		}
		return thread;
	}

	private async getTokenAuthenticatedWebhookChannel({
		webhookId,
		token,
	}: WebhookTokenParams): Promise<{webhook: Webhook; channel: Channel & {guildId: GuildID}}> {
		const webhook = await this.repository.findByToken(webhookId, token);
		if (!webhook || webhook.type !== WebhookTypes.INCOMING) throw new UnknownWebhookError();
		if (!webhook.channelId) throw new UnknownChannelError();
		const channel = await this.channelRepository.channelData.findUnique(webhook.channelId);
		if (!channel) throw new UnknownChannelError();
		if (channel.isThreadOnly() && webhook.guildId && !guildActive(webhook.guildId)) throw new UnknownWebhookError();
		this.assertWebhookTargetChannel(channel);
		return {webhook, channel};
	}

	private assertWebhookTargetChannel(channel: Channel): asserts channel is Channel & {guildId: GuildID} {
		if (!channel.guildId) throw new UnknownChannelError();
		if (GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type)) return;
		if (channel.isThreadOnly() && guildActive(channel.guildId)) return;
		throw new UnknownChannelError();
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
			const channel = await this.channelRepository.channelData.findUnique(createChannelID(data.channel_id));
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
		avatarUrl: requestedAvatarUrl,
	}: {
		webhookId: WebhookID;
		avatarUrl: string | null;
	}): Promise<string | null> {
		if (!requestedAvatarUrl) return null;
		const avatarUrl = stripOwnAttachmentSignature(requestedAvatarUrl);
		try {
			const cacheKey = `webhook:${webhookId}:avatar:${avatarUrl}`;
			const avatarCache = await this.cacheService.get<string>(cacheKey);
			if (avatarCache === WEBHOOK_AVATAR_MISSING_CACHE_VALUE) return null;
			if (avatarCache) return avatarCache;
			const metadata = await this.mediaService.getMetadata({
				type: 'external',
				url: avatarUrl,
				with_base64: true,
				nsfw: 'allow',
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
		return this.getHostedWebhookAvatar({webhookId, provider: 'github'});
	}

	private async getInstatusWebhookAvatar(webhookId: WebhookID): Promise<string | null> {
		return this.getHostedWebhookAvatar({webhookId, provider: 'instatus'});
	}

	private async getHostedWebhookAvatar({
		webhookId,
		provider,
	}: {
		webhookId: WebhookID;
		provider: keyof typeof HOSTED_WEBHOOK_AVATAR_URLS;
	}): Promise<string | null> {
		if (Config.instance.selfHosted) return null;
		return this.getWebhookAvatar({webhookId, avatarUrl: HOSTED_WEBHOOK_AVATAR_URLS[provider]});
	}

	private getWebhookMetadata(webhook: Webhook): Record<string, string> | undefined {
		if (!webhook.channelId) {
			return undefined;
		}
		return {channel_id: webhook.channelId.toString(), type: webhook.type.toString()};
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

	async recordWebhookAuditLog(params: {
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
		return resolveLimitSafe(this.limitConfigService.getConfigSnapshot(), ctx, key, fallback, 'guild');
	}
}
