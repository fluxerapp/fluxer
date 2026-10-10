// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, UserID} from '@app/api/BrandedTypes';
import {mapChannelToResponse} from '@app/api/channel/ChannelMappers';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import type {ChannelTypeConversion} from '@app/api/channel/services/channel_data/ChannelOperationsService';
import {applyForumTagEdit, type ForumTagEdit} from '@app/api/channel/services/thread/ForumTagService';
import {mapThreadToResponse} from '@app/api/channel/services/thread/ThreadMappers';
import {withThreadParentFields} from '@app/api/channel/services/thread/ThreadParentSettings';
import {loadThreadViews} from '@app/api/channel/services/thread/ThreadViews';
import type {ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import {maskChannelResponseThreadBits} from '@app/api/guild/services/ThreadPermissionBits';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {User} from '@app/api/models/User';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {InvalidChannelTypeError} from '@fluxer/errors/src/domains/channel/InvalidChannelTypeError';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {ThrottledError} from '@fluxer/errors/src/domains/core/ThrottledError';
import type {ChannelUpdateGatedRequest} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import type {ChannelResponse, ChannelSlowmodeStateResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

const FORUM_TAGS_LOCK_TTL_SECONDS = 5;
const FORUM_TAGS_LOCK_ACQUIRE_ATTEMPTS = 6;
const FORUM_TAGS_LOCK_RETRY_DELAY_MS = 50;

export class ChannelRequestService {
	constructor(
		private readonly channelService: ChannelService,
		private readonly userCacheService: UserCacheService,
		private readonly cacheService: ICacheService,
	) {}

	async getChannelResponse(params: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		requestCache: RequestCache;
	}): Promise<ChannelResponse> {
		const channel = await this.channelService.channelData.operations.getChannel({
			userId: params.userId,
			viewer: params.viewer,
			channelId: params.channelId,
		});
		return this.maskedChannelResponse(channel, params);
	}

	private async maskedChannelResponse(
		channel: Channel,
		params: {userId: UserID; viewer: ThreadViewer; requestCache: RequestCache},
	): Promise<ChannelResponse> {
		if (channel.isThread()) return this.threadResponse(channel, params.userId);
		const response = await mapChannelToResponse({
			channel,
			currentUserId: params.userId,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
		});
		const [masked] = await maskChannelResponseThreadBits(channel.guildId, params.viewer, [
			await withThreadParentFields(
				this.channelService.channelData.threadModify.repository.threads,
				channel,
				response,
				params.viewer,
			),
		]);
		return masked;
	}

	private async threadResponse(channel: Channel, userId: UserID): Promise<ChannelResponse> {
		const repository = this.channelService.channelData.threadModify.repository;
		const [state, member] = await Promise.all([
			repository.threads.getState(channel.id),
			repository.threads.getMember(channel.id, userId),
		]);
		if (!state) throw new UnknownChannelError();
		const [view] = await loadThreadViews(repository, [state]);
		if (!view) throw new UnknownChannelError();
		return mapThreadToResponse({...view, channel}, member);
	}

	async getSlowmodeState(params: {
		user: User;
		viewer: ThreadViewer;
		channelId: ChannelID;
	}): Promise<ChannelSlowmodeStateResponse> {
		const state = await this.channelService.getSlowmodeState({
			user: params.user,
			viewer: params.viewer,
			channelId: params.channelId,
		});
		return {
			rate_limit_per_user: state.rateLimitPerUser,
			retry_after_ms: state.retryAfterMs,
			next_send_allowed_at: state.nextSendAllowedAt ? state.nextSendAllowedAt.toISOString() : null,
			can_bypass: state.canBypass,
		};
	}

	async listRtcRegions(params: {userId: UserID; viewer: ThreadViewer; channelId: ChannelID}) {
		const regions = await this.channelService.channelData.operations.getAvailableRtcRegions({
			userId: params.userId,
			viewer: params.viewer,
			channelId: params.channelId,
		});
		return regions.map((region) => ({
			id: region.id,
			name: region.name,
			emoji: region.emoji,
		}));
	}

	async updateChannel(params: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		data: ChannelUpdateGatedRequest;
		clientFeatures: ReadonlySet<string>;
		requestCache: RequestCache;
		auditLogReason: string | null;
		typeConversion?: ChannelTypeConversion | null;
		countryCode?: string | null;
	}): Promise<ChannelResponse> {
		if (!('available_tags' in params.data) || params.data.available_tags === undefined) {
			return this.applyChannelUpdate(params);
		}
		return this.withForumTagsLock(params.channelId, () => this.applyChannelUpdate(params));
	}

	private async applyChannelUpdate(params: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		data: ChannelUpdateGatedRequest;
		clientFeatures: ReadonlySet<string>;
		requestCache: RequestCache;
		auditLogReason: string | null;
		typeConversion?: ChannelTypeConversion | null;
		countryCode?: string | null;
	}): Promise<ChannelResponse> {
		const channel = await this.channelService.channelData.editChannel({
			userId: params.userId,
			viewer: params.viewer,
			channelId: params.channelId,
			data: params.data,
			clientFeatures: params.clientFeatures,
			requestCache: params.requestCache,
			auditLogReason: params.auditLogReason,
			typeConversion: params.typeConversion,
			countryCode: params.countryCode,
		});
		return this.maskedChannelResponse(channel, params);
	}

	async editForumTags(params: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		edit: ForumTagEdit;
		clientFeatures: ReadonlySet<string>;
		requestCache: RequestCache;
		auditLogReason: string | null;
	}): Promise<ChannelResponse> {
		const {channel, checkPermission} = await this.channelService.channelData.auth.getChannelAuthenticated({
			userId: params.userId,
			channelId: params.channelId,
			viewer: params.viewer,
			skipNsfwValidation: true,
		});
		if (!channel.isThreadOnly() || channel.guildId === null) throw new InvalidChannelTypeError();
		await checkPermission(Permissions.MANAGE_CHANNELS);
		const guildId = channel.guildId;
		return this.withForumTagsLock(channel.id, async () => {
			const config = await this.channelService.channelData.threadModify.repository.threads.getParentConfig(
				guildId,
				channel.id,
			);
			return this.applyChannelUpdate({
				userId: params.userId,
				viewer: params.viewer,
				channelId: params.channelId,
				data: {type: channel.type, available_tags: applyForumTagEdit(config, params.edit)} as ChannelUpdateGatedRequest,
				clientFeatures: params.clientFeatures,
				requestCache: params.requestCache,
				auditLogReason: params.auditLogReason,
			});
		});
	}

	private async withForumTagsLock<T>(channelId: ChannelID, fn: () => Promise<T>): Promise<T> {
		const lockKey = `channel:${channelId}:forum-tags`;
		let lockToken: string | null = null;
		for (let attempt = 0; attempt < FORUM_TAGS_LOCK_ACQUIRE_ATTEMPTS; attempt++) {
			lockToken = await this.cacheService.acquireLock(lockKey, FORUM_TAGS_LOCK_TTL_SECONDS);
			if (lockToken) break;
			await new Promise((resolve) => setTimeout(resolve, FORUM_TAGS_LOCK_RETRY_DELAY_MS * (attempt + 1)));
		}
		if (!lockToken) {
			throw new ThrottledError({
				code: APIErrorCodes.RESOURCE_LOCKED,
				retryAfterSeconds: 1,
				data: {retry_after: 1},
			});
		}
		try {
			return await fn();
		} finally {
			await this.cacheService.releaseLock(lockKey, lockToken).catch(() => {});
		}
	}

	async deleteChannel(params: {
		userId: UserID;
		viewer: ThreadViewer;
		channelId: ChannelID;
		requestCache: RequestCache;
		silent?: boolean;
		auditLogReason: string | null;
	}): Promise<void> {
		const channel = await this.channelService.channelData.operations.getChannel({
			userId: params.userId,
			viewer: params.viewer,
			channelId: params.channelId,
		});
		if (channel.isThread()) {
			await this.channelService.channelData.deleteThread({
				userId: params.userId,
				viewer: params.viewer,
				channelId: params.channelId,
				requestCache: params.requestCache,
				auditLogReason: params.auditLogReason,
			});
			return;
		}
		if (channel.type === ChannelTypes.GROUP_DM) {
			await this.channelService.groupDms.removeRecipientFromChannel({
				userId: params.userId,
				channelId: params.channelId,
				recipientId: params.userId,
				requestCache: params.requestCache,
				silent: params.silent,
			});
			return;
		}
		await this.channelService.channelData.operations.deleteChannel({
			userId: params.userId,
			viewer: params.viewer,
			channelId: params.channelId,
			requestCache: params.requestCache,
			auditLogReason: params.auditLogReason,
		});
	}
}
