// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ChannelID, UserID} from '@app/api/BrandedTypes';
import {mapChannelToResponse} from '@app/api/channel/ChannelMappers';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {User} from '@app/api/models/User';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelUpdateRequest} from '@fluxer/schema/src/domains/channel/ChannelRequestSchemas';
import type {ChannelResponse, ChannelSlowmodeStateResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';

export class ChannelRequestService {
	constructor(
		private readonly channelService: ChannelService,
		private readonly userCacheService: UserCacheService,
	) {}

	async getChannelResponse(params: {
		userId: UserID;
		channelId: ChannelID;
		requestCache: RequestCache;
	}): Promise<ChannelResponse> {
		const channel = await this.channelService.channelData.operations.getChannel({
			userId: params.userId,
			channelId: params.channelId,
		});
		return mapChannelToResponse({
			channel,
			currentUserId: params.userId,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
		});
	}

	async getSlowmodeState(params: {user: User; channelId: ChannelID}): Promise<ChannelSlowmodeStateResponse> {
		const state = await this.channelService.getSlowmodeState({user: params.user, channelId: params.channelId});
		return {
			rate_limit_per_user: state.rateLimitPerUser,
			retry_after_ms: state.retryAfterMs,
			next_send_allowed_at: state.nextSendAllowedAt ? state.nextSendAllowedAt.toISOString() : null,
			can_bypass: state.canBypass,
		};
	}

	async listRtcRegions(params: {userId: UserID; channelId: ChannelID}) {
		const regions = await this.channelService.channelData.operations.getAvailableRtcRegions({
			userId: params.userId,
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
		channelId: ChannelID;
		data: ChannelUpdateRequest;
		clientFeatures: ReadonlySet<string>;
		requestCache: RequestCache;
		auditLogReason: string | null;
	}): Promise<ChannelResponse> {
		const channel = await this.channelService.channelData.editChannel({
			userId: params.userId,
			channelId: params.channelId,
			data: params.data,
			clientFeatures: params.clientFeatures,
			requestCache: params.requestCache,
			auditLogReason: params.auditLogReason,
		});
		return mapChannelToResponse({
			channel,
			currentUserId: params.userId,
			userCacheService: this.userCacheService,
			requestCache: params.requestCache,
		});
	}

	async deleteChannel(params: {
		userId: UserID;
		channelId: ChannelID;
		requestCache: RequestCache;
		silent?: boolean;
		auditLogReason: string | null;
	}): Promise<void> {
		const channel = await this.channelService.channelData.operations.getChannel({
			userId: params.userId,
			channelId: params.channelId,
		});
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
			channelId: params.channelId,
			requestCache: params.requestCache,
			auditLogReason: params.auditLogReason,
		});
	}
}
