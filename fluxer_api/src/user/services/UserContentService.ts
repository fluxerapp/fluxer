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

import crypto from 'node:crypto';
import {type ChannelID, createBetaCode, type MessageID, type UserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import {MAX_BOOKMARKS_NON_PREMIUM, MAX_BOOKMARKS_PREMIUM} from '~/Constants';
import {mapMessageToResponse} from '~/channel/ChannelModel';
import type {IChannelRepository} from '~/channel/IChannelRepository';
import type {ChannelService} from '~/channel/services/ChannelService';
import type {PushSubscriptionRow} from '~/database/CassandraTypes';
import {
	BetaCodeAllowanceExceededError,
	BetaCodeMaxUnclaimedError,
	HarvestExpiredError,
	HarvestFailedError,
	HarvestNotReadyError,
	HarvestOnCooldownError,
	MaxBookmarksError,
	MissingPermissionsError,
	UnknownChannelError,
	UnknownHarvestError,
	UnknownMessageError,
	UnknownUserError,
} from '~/Errors';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {IStorageService} from '~/infrastructure/IStorageService';
import type {RedisBulkMessageDeletionQueueService} from '~/infrastructure/RedisBulkMessageDeletionQueueService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import {Logger} from '~/Logger';
import type {BetaCode, Message, PushSubscription} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import * as RandomUtils from '~/utils/RandomUtils';
import * as SnowflakeUtils from '~/utils/SnowflakeUtils';
import type {IWorkerService} from '~/worker/IWorkerService';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import type {IUserContentRepository} from '../repositories/IUserContentRepository';
import {UserHarvest, type UserHarvestResponse} from '../UserHarvestModel';
import {UserHarvestRepository} from '../UserHarvestRepository';
import type {SavedMessageStatus} from '../UserTypes';
import {BaseUserUpdatePropagator} from './BaseUserUpdatePropagator';

export class UserContentService {
	private readonly updatePropagator: BaseUserUpdatePropagator;

	constructor(
		private userAccountRepository: IUserAccountRepository,
		private userContentRepository: IUserContentRepository,
		userCacheService: UserCacheService,
		private channelService: ChannelService,
		private channelRepository: IChannelRepository,
		private gatewayService: IGatewayService,
		private mediaService: IMediaService,
		private workerService: IWorkerService,
		private snowflakeService: SnowflakeService,
		private bulkMessageDeletionQueue: RedisBulkMessageDeletionQueueService,
	) {
		this.updatePropagator = new BaseUserUpdatePropagator({
			userCacheService,
			gatewayService: this.gatewayService,
		});
	}

	async getBetaCodes(userId: UserID): Promise<Array<BetaCode>> {
		const betaCodes = await this.userContentRepository.listBetaCodes(userId);
		const now = new Date();
		const oneWeekAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);

		return betaCodes.filter((code) => {
			if (!code.redeemedAt) {
				return true;
			}
			return code.redeemedAt >= oneWeekAgo;
		});
	}

	async getBetaCodeAllowanceInfo(userId: UserID): Promise<{allowance: number; nextResetAt: Date | null}> {
		const user = await this.userAccountRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const now = new Date();
		const oneWeekAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);

		let allowance = user.betaCodeAllowance;
		let lastResetAt = user.betaCodeLastResetAt;

		if (!lastResetAt || lastResetAt < oneWeekAgo) {
			allowance = 3;
			lastResetAt = now;
		}

		const nextResetAt = lastResetAt ? new Date(lastResetAt.getTime() + 7 * 24 * 60 * 60 * 1000) : null;

		return {allowance, nextResetAt};
	}

	async createBetaCode(userId: UserID): Promise<BetaCode> {
		const user = await this.userAccountRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const existingBetaCodes = await this.userContentRepository.listBetaCodes(userId);
		const unclaimedCount = existingBetaCodes.filter((code) => !code.redeemerId).length;

		if (unclaimedCount >= 6) {
			throw new BetaCodeMaxUnclaimedError();
		}

		const now = new Date();
		const oneWeekAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);

		let allowance = user.betaCodeAllowance;
		let lastResetAt = user.betaCodeLastResetAt;

		if (!lastResetAt || lastResetAt < oneWeekAgo) {
			allowance = 3;
			lastResetAt = now;
		}

		if (allowance <= 0) {
			throw new BetaCodeAllowanceExceededError();
		}

		await this.userAccountRepository.patchUpsert(userId, {
			beta_code_allowance: allowance - 1,
			beta_code_last_reset_at: lastResetAt,
		});

		return await this.userContentRepository.upsertBetaCode({
			code: createBetaCode(RandomUtils.randomString(32)),
			created_at: now,
			creator_id: userId,
			redeemed_at: null,
			redeemer_id: null,
			version: 1,
		});
	}

	async deleteBetaCode(params: {userId: UserID; code: string}): Promise<void> {
		const {userId, code} = params;
		const user = await this.userAccountRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const now = new Date();
		const oneWeekAgo = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);

		let allowance = user.betaCodeAllowance;
		let lastResetAt = user.betaCodeLastResetAt;

		if (!lastResetAt || lastResetAt < oneWeekAgo) {
			allowance = 3;
			lastResetAt = now;
		}

		await this.userContentRepository.deleteBetaCode(code, userId);

		await this.userAccountRepository.patchUpsert(userId, {
			beta_code_allowance: Math.min(allowance + 1, 3),
			beta_code_last_reset_at: lastResetAt,
		});
	}

	async getRecentMentions(params: {
		userId: UserID;
		limit: number;
		everyone: boolean;
		roles: boolean;
		guilds: boolean;
		before?: MessageID;
	}): Promise<Array<Message>> {
		const {userId, limit, everyone, roles, guilds, before} = params;
		const mentions = await this.userContentRepository.listRecentMentions(
			userId,
			everyone,
			roles,
			guilds,
			limit,
			before,
		);
		const messagePromises = mentions.map(async (mention) => {
			try {
				return await this.channelService.getMessage({
					userId,
					channelId: mention.channelId,
					messageId: mention.messageId,
				});
			} catch (error) {
				if (error instanceof UnknownMessageError) {
					return null;
				}
				throw error;
			}
		});
		const messageResults = await Promise.all(messagePromises);
		const messages = messageResults.filter((message): message is Message => message != null);
		return messages.sort((a, b) => (b.id > a.id ? 1 : -1));
	}

	async deleteRecentMention({userId, messageId}: {userId: UserID; messageId: MessageID}): Promise<void> {
		const recentMention = await this.userContentRepository.getRecentMention(userId, messageId);
		if (!recentMention) return;
		await this.userContentRepository.deleteRecentMention(recentMention);
		await this.dispatchRecentMentionDelete({userId, messageId});
	}

	async getSavedMessages({userId, limit}: {userId: UserID; limit: number}): Promise<
		Array<{
			channelId: ChannelID;
			messageId: MessageID;
			status: SavedMessageStatus;
			message: Message | null;
		}>
	> {
		const savedMessages = await this.userContentRepository.listSavedMessages(userId, limit);
		const results: Array<{
			channelId: ChannelID;
			messageId: MessageID;
			status: SavedMessageStatus;
			message: Message | null;
		}> = [];

		for (const savedMessage of savedMessages) {
			let message: Message | null = null;
			let status: SavedMessageStatus = 'available';

			try {
				message = await this.channelService.getMessage({
					userId,
					channelId: savedMessage.channelId,
					messageId: savedMessage.messageId,
				});
			} catch (error) {
				if (error instanceof UnknownMessageError) {
					await this.userContentRepository.deleteSavedMessage(userId, savedMessage.messageId);
					continue;
				}
				if (error instanceof MissingPermissionsError || error instanceof UnknownChannelError) {
					status = 'missing_permissions';
				} else {
					throw error;
				}
			}

			results.push({
				channelId: savedMessage.channelId,
				messageId: savedMessage.messageId,
				status,
				message,
			});
		}

		return results.sort((a, b) => (b.messageId > a.messageId ? 1 : a.messageId > b.messageId ? -1 : 0));
	}

	async saveMessage({
		userId,
		channelId,
		messageId,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		channelId: ChannelID;
		messageId: MessageID;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<void> {
		const user = await this.userAccountRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const savedMessages = await this.userContentRepository.listSavedMessages(userId, 1000);
		const maxBookmarks = user.isPremium() ? MAX_BOOKMARKS_PREMIUM : MAX_BOOKMARKS_NON_PREMIUM;

		if (savedMessages.length >= maxBookmarks) {
			throw new MaxBookmarksError(user.isPremium());
		}

		await this.channelService.getChannelAuthenticated({userId, channelId});
		const message = await this.channelService.getMessage({userId, channelId, messageId});
		if (!message) {
			throw new UnknownMessageError();
		}
		await this.userContentRepository.createSavedMessage(userId, channelId, messageId);
		await this.dispatchSavedMessageCreate({userId, message, userCacheService, requestCache});
	}

	async unsaveMessage({userId, messageId}: {userId: UserID; messageId: MessageID}): Promise<void> {
		await this.userContentRepository.deleteSavedMessage(userId, messageId);
		await this.dispatchSavedMessageDelete({userId, messageId});
	}

	async registerPushSubscription(params: {
		userId: UserID;
		endpoint: string;
		keys: {p256dh: string; auth: string};
		userAgent?: string;
	}): Promise<PushSubscription> {
		const {userId, endpoint, keys, userAgent} = params;

		const subscriptionId = crypto.createHash('sha256').update(endpoint).digest('hex').substring(0, 32);

		const data: PushSubscriptionRow = {
			user_id: userId,
			subscription_id: subscriptionId,
			endpoint,
			p256dh_key: keys.p256dh,
			auth_key: keys.auth,
			user_agent: userAgent ?? null,
		};

		return await this.userContentRepository.createPushSubscription(data);
	}

	async listPushSubscriptions(userId: UserID): Promise<Array<PushSubscription>> {
		return await this.userContentRepository.listPushSubscriptions(userId);
	}

	async deletePushSubscription(userId: UserID, subscriptionId: string): Promise<void> {
		await this.userContentRepository.deletePushSubscription(userId, subscriptionId);
	}

	async requestDataHarvest(userId: UserID): Promise<{harvestId: string}> {
		const user = await this.userAccountRepository.findUnique(userId);
		if (!user) throw new UnknownUserError();

		if (!Config.dev.testModeEnabled) {
			const harvestRepository = new UserHarvestRepository();
			const latestHarvest = await harvestRepository.findLatestByUserId(userId);

			if (latestHarvest?.requestedAt) {
				const sevenDaysAgo = new Date(Date.now() - 7 * 24 * 60 * 60 * 1000);
				if (latestHarvest.requestedAt > sevenDaysAgo) {
					const retryAfter = new Date(latestHarvest.requestedAt.getTime() + 7 * 24 * 60 * 60 * 1000);
					throw new HarvestOnCooldownError({retryAfter});
				}
			}
		}

		const harvestId = this.snowflakeService.generate();
		const harvest = new UserHarvest({
			user_id: userId,
			harvest_id: harvestId,
			requested_at: new Date(),
			started_at: null,
			completed_at: null,
			failed_at: null,
			storage_key: null,
			file_size: null,
			progress_percent: 0,
			progress_step: 'Queued',
			error_message: null,
			download_url_expires_at: null,
		});

		const harvestRepository = new UserHarvestRepository();
		await harvestRepository.create(harvest);

		await this.workerService.addJob('harvestUserData', {
			userId: userId.toString(),
			harvestId: harvestId.toString(),
		});

		return {harvestId: harvestId.toString()};
	}

	async getHarvestStatus(userId: UserID, harvestId: bigint): Promise<UserHarvestResponse> {
		const harvestRepository = new UserHarvestRepository();
		const harvest = await harvestRepository.findByUserAndHarvestId(userId, harvestId);
		if (!harvest) {
			throw new UnknownHarvestError();
		}
		return harvest.toResponse();
	}

	async getLatestHarvest(userId: UserID): Promise<UserHarvestResponse | null> {
		const harvestRepository = new UserHarvestRepository();
		const harvest = await harvestRepository.findLatestByUserId(userId);
		return harvest ? harvest.toResponse() : null;
	}

	async getHarvestDownloadUrl(
		userId: UserID,
		harvestId: bigint,
		storageService: IStorageService,
	): Promise<{downloadUrl: string; expiresAt: string}> {
		const harvestRepository = new UserHarvestRepository();
		const harvest = await harvestRepository.findByUserAndHarvestId(userId, harvestId);

		if (!harvest) {
			throw new UnknownHarvestError();
		}

		if (!harvest.completedAt || !harvest.storageKey) {
			throw new HarvestNotReadyError();
		}

		if (harvest.failedAt) {
			throw new HarvestFailedError();
		}

		if (harvest.downloadUrlExpiresAt && harvest.downloadUrlExpiresAt < new Date()) {
			throw new HarvestExpiredError();
		}

		const ZIP_EXPIRY_SECONDS = 7 * 24 * 60 * 60;
		const downloadUrl = await storageService.getPresignedDownloadURL({
			bucket: Config.s3.buckets.harvests,
			key: harvest.storageKey,
			expiresIn: ZIP_EXPIRY_SECONDS,
		});

		const expiresAt = new Date(Date.now() + ZIP_EXPIRY_SECONDS * 1000);

		return {
			downloadUrl,
			expiresAt: expiresAt.toISOString(),
		};
	}

	async requestBulkMessageDeletion(params: {userId: UserID; delayMs?: number}): Promise<void> {
		const {userId, delayMs = 24 * 60 * 60 * 1000} = params;
		const scheduledAt = new Date(Date.now() + delayMs);

		await this.bulkMessageDeletionQueue.removeFromQueue(userId);

		const counts = await this.countBulkDeletionTargets(userId, scheduledAt.getTime());
		Logger.debug(
			{
				userId: userId.toString(),
				channelCount: counts.channelCount,
				messageCount: counts.messageCount,
				scheduledAt: scheduledAt.toISOString(),
			},
			'Scheduling bulk message deletion',
		);

		const updatedUser = await this.userAccountRepository.patchUpsert(userId, {
			pending_bulk_message_deletion_at: scheduledAt,
			pending_bulk_message_deletion_channel_count: counts.channelCount,
			pending_bulk_message_deletion_message_count: counts.messageCount,
		});

		if (!updatedUser) {
			throw new UnknownUserError();
		}

		await this.bulkMessageDeletionQueue.scheduleDeletion(userId, scheduledAt);

		await this.updatePropagator.dispatchUserUpdate(updatedUser);
	}

	async cancelBulkMessageDeletion(userId: UserID): Promise<void> {
		Logger.debug({userId: userId.toString()}, 'Canceling pending bulk message deletion');
		const updatedUser = await this.userAccountRepository.patchUpsert(userId, {
			pending_bulk_message_deletion_at: null,
			pending_bulk_message_deletion_channel_count: null,
			pending_bulk_message_deletion_message_count: null,
		});

		if (!updatedUser) {
			throw new UnknownUserError();
		}

		await this.bulkMessageDeletionQueue.removeFromQueue(userId);

		await this.updatePropagator.dispatchUserUpdate(updatedUser);
	}

	private async countBulkDeletionTargets(
		userId: UserID,
		cutoffMs: number,
	): Promise<{
		channelCount: number;
		messageCount: number;
	}> {
		const CHUNK_SIZE = 200;
		let lastChannelId: ChannelID | undefined;
		let lastMessageId: MessageID | undefined;
		const channels = new Set<string>();
		let messageCount = 0;

		while (true) {
			const messageRefs = await this.channelRepository.listMessagesByAuthor(
				userId,
				CHUNK_SIZE,
				lastChannelId,
				lastMessageId,
			);
			if (messageRefs.length === 0) {
				break;
			}

			for (const {channelId, messageId} of messageRefs) {
				if (SnowflakeUtils.extractTimestamp(messageId) > cutoffMs) {
					continue;
				}
				channels.add(channelId.toString());
				messageCount++;
			}

			lastChannelId = messageRefs[messageRefs.length - 1].channelId;
			lastMessageId = messageRefs[messageRefs.length - 1].messageId;

			if (messageRefs.length < CHUNK_SIZE) {
				break;
			}
		}

		return {
			channelCount: channels.size,
			messageCount,
		};
	}

	async dispatchRecentMentionDelete({userId, messageId}: {userId: UserID; messageId: MessageID}): Promise<void> {
		await this.gatewayService.dispatchPresence({
			userId,
			event: 'RECENT_MENTION_DELETE',
			data: {message_id: messageId.toString()},
		});
	}

	async dispatchSavedMessageCreate({
		userId,
		message,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		message: Message;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<void> {
		await this.gatewayService.dispatchPresence({
			userId,
			event: 'SAVED_MESSAGE_CREATE',
			data: await mapMessageToResponse({
				message,
				currentUserId: userId,
				userCacheService,
				requestCache,
				mediaService: this.mediaService,
			}),
		});
	}

	async dispatchSavedMessageDelete({userId, messageId}: {userId: UserID; messageId: MessageID}): Promise<void> {
		await this.gatewayService.dispatchPresence({
			userId,
			event: 'SAVED_MESSAGE_DELETE',
			data: {message_id: messageId.toString()},
		});
	}
}
