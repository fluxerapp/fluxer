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

import type {AuthService} from '~/auth/AuthService';
import type {SudoVerificationResult} from '~/auth/services/SudoVerificationService';
import type {ChannelID, GuildID, MessageID, UserID} from '~/BrandedTypes';
import type {IChannelRepository} from '~/channel/IChannelRepository';
import type {ChannelService} from '~/channel/services/ChannelService';
import type {GuildMemberResponse} from '~/guild/GuildModel';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {GuildService} from '~/guild/services/GuildService';
import type {IDiscriminatorService} from '~/infrastructure/DiscriminatorService';
import type {EntityAssetService} from '~/infrastructure/EntityAssetService';
import type {IEmailService} from '~/infrastructure/IEmailService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {IRateLimitService} from '~/infrastructure/IRateLimitService';
import type {IStorageService} from '~/infrastructure/IStorageService';
import type {RedisAccountDeletionQueueService} from '~/infrastructure/RedisAccountDeletionQueueService';
import type {RedisBulkMessageDeletionQueueService} from '~/infrastructure/RedisBulkMessageDeletionQueueService';
import type {SnowflakeService} from '~/infrastructure/SnowflakeService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {
	AuthSession,
	BetaCode,
	Channel,
	GuildMember,
	Message,
	MfaBackupCode,
	PushSubscription,
	Relationship,
	User,
	UserGuildSettings,
	UserSettings,
} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {BotMfaMirrorService} from '~/oauth/BotMfaMirrorService';
import type {PackService} from '~/pack/PackService';
import type {
	CreatePrivateChannelRequest,
	FriendRequestByTagRequest,
	UserGuildSettingsUpdateRequest,
	UserSettingsUpdateRequest,
	UserUpdateRequest,
} from '~/user/UserModel';
import type {SavedMessageStatus} from '~/user/UserTypes';
import type {UserPermissionUtils} from '~/utils/UserPermissionUtils';
import type {IWorkerService} from '~/worker/IWorkerService';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import type {IUserAuthRepository} from '../repositories/IUserAuthRepository';
import type {IUserChannelRepository} from '../repositories/IUserChannelRepository';
import type {IUserContentRepository} from '../repositories/IUserContentRepository';
import type {IUserRelationshipRepository} from '../repositories/IUserRelationshipRepository';
import type {IUserSettingsRepository} from '../repositories/IUserSettingsRepository';
import type {UserHarvestResponse} from '../UserHarvestModel';
import {UserAccountService} from './UserAccountService';
import {UserAuthService} from './UserAuthService';
import {UserChannelService} from './UserChannelService';
import type {UserContactChangeLogService} from './UserContactChangeLogService';
import {UserContentService} from './UserContentService';
import {UserRelationshipService} from './UserRelationshipService';

interface UpdateUserParams {
	user: User;
	oldAuthSession: AuthSession;
	data: UserUpdateRequest;
	request: Request;
	sudoContext?: SudoVerificationResult;
	emailVerifiedViaToken?: boolean;
}

export class UserService {
	private accountService: UserAccountService;
	private authService: UserAuthService;
	private relationshipService: UserRelationshipService;
	private channelService: UserChannelService;
	private contentService: UserContentService;

	constructor(
		userAccountRepository: IUserAccountRepository,
		userSettingsRepository: IUserSettingsRepository,
		userAuthRepository: IUserAuthRepository,
		userRelationshipRepository: IUserRelationshipRepository,
		userChannelRepository: IUserChannelRepository,
		userContentRepository: IUserContentRepository,
		authService: AuthService,
		userCacheService: UserCacheService,
		channelService: ChannelService,
		channelRepository: IChannelRepository,
		guildService: GuildService,
		gatewayService: IGatewayService,
		entityAssetService: EntityAssetService,
		mediaService: IMediaService,
		packService: PackService,
		emailService: IEmailService,
		snowflakeService: SnowflakeService,
		discriminatorService: IDiscriminatorService,
		rateLimitService: IRateLimitService,
		guildRepository: IGuildRepository,
		workerService: IWorkerService,
		userPermissionUtils: UserPermissionUtils,
		redisDeletionQueue: RedisAccountDeletionQueueService,
		bulkMessageDeletionQueue: RedisBulkMessageDeletionQueueService,
		botMfaMirrorService: BotMfaMirrorService,
		contactChangeLogService: UserContactChangeLogService,
	) {
		this.accountService = new UserAccountService(
			userAccountRepository,
			userSettingsRepository,
			userRelationshipRepository,
			userChannelRepository,
			authService,
			userCacheService,
			guildService,
			gatewayService,
			entityAssetService,
			mediaService,
			packService,
			emailService,
			rateLimitService,
			guildRepository,
			discriminatorService,
			redisDeletionQueue,
			contactChangeLogService,
		);

		this.authService = new UserAuthService(
			userAccountRepository,
			userAuthRepository,
			authService,
			emailService,
			gatewayService,
			botMfaMirrorService,
		);

		this.relationshipService = new UserRelationshipService(
			userAccountRepository,
			userRelationshipRepository,
			gatewayService,
			userPermissionUtils,
		);

		this.channelService = new UserChannelService(
			userAccountRepository,
			userChannelRepository,
			userRelationshipRepository,
			channelService,
			channelRepository,
			gatewayService,
			mediaService,
			snowflakeService,
			userPermissionUtils,
		);

		this.contentService = new UserContentService(
			userAccountRepository,
			userContentRepository,
			userCacheService,
			channelService,
			channelRepository,
			gatewayService,
			mediaService,
			workerService,
			snowflakeService,
			bulkMessageDeletionQueue,
		);
	}

	async findUnique(userId: UserID): Promise<User | null> {
		return await this.accountService.findUnique(userId);
	}

	async findUniqueAssert(userId: UserID): Promise<User> {
		return await this.accountService.findUniqueAssert(userId);
	}

	async getUserProfile(params: {
		userId: UserID;
		targetId: UserID;
		guildId?: GuildID;
		withMutualFriends?: boolean;
		withMutualGuilds?: boolean;
		requestCache: RequestCache;
	}): Promise<{
		user: User;
		guildMember?: GuildMemberResponse | null;
		guildMemberDomain?: GuildMember | null;
		premiumType?: number;
		premiumSince?: Date;
		premiumLifetimeSequence?: number;
		mutualFriends?: Array<User>;
		mutualGuilds?: Array<{id: string; nick: string | null}>;
	}> {
		return await this.accountService.getUserProfile(params);
	}

	async update(params: UpdateUserParams): Promise<User> {
		return await this.accountService.update(params);
	}

	async generateUniqueDiscriminator(username: string): Promise<number> {
		return await this.accountService.generateUniqueDiscriminator(username);
	}

	async checkUsernameDiscriminatorAvailability(params: {username: string; discriminator: number}): Promise<boolean> {
		return await this.accountService.checkUsernameDiscriminatorAvailability(params);
	}

	async findSettings(userId: UserID): Promise<UserSettings> {
		return await this.accountService.findSettings(userId);
	}

	async updateSettings(params: {userId: UserID; data: UserSettingsUpdateRequest}): Promise<UserSettings> {
		return await this.accountService.updateSettings(params);
	}

	async findGuildSettings(userId: UserID, guildId: GuildID | null): Promise<UserGuildSettings | null> {
		return await this.accountService.findGuildSettings(userId, guildId);
	}

	async updateGuildSettings(params: {
		userId: UserID;
		guildId: GuildID | null;
		data: UserGuildSettingsUpdateRequest;
	}): Promise<UserGuildSettings> {
		return await this.accountService.updateGuildSettings(params);
	}

	async getUserNote(params: {userId: UserID; targetId: UserID}): Promise<{note: string} | null> {
		return await this.accountService.getUserNote(params);
	}

	async getUserNotes(userId: UserID): Promise<Record<string, string>> {
		return await this.accountService.getUserNotes(userId);
	}

	async setUserNote(params: {userId: UserID; targetId: UserID; note: string | null}): Promise<void> {
		return await this.accountService.setUserNote(params);
	}

	async selfDisable(userId: UserID): Promise<void> {
		return await this.accountService.selfDisable(userId);
	}

	async selfDelete(userId: UserID): Promise<void> {
		return await this.accountService.selfDelete(userId);
	}

	async dispatchUserUpdate(user: User): Promise<void> {
		return await this.accountService.dispatchUserUpdate(user);
	}

	async dispatchUserSettingsUpdate({userId, settings}: {userId: UserID; settings: UserSettings}): Promise<void> {
		return await this.accountService.dispatchUserSettingsUpdate({userId, settings});
	}

	async dispatchUserGuildSettingsUpdate({
		userId,
		settings,
	}: {
		userId: UserID;
		settings: UserGuildSettings;
	}): Promise<void> {
		return await this.accountService.dispatchUserGuildSettingsUpdate({userId, settings});
	}

	async dispatchUserNoteUpdate(params: {userId: UserID; targetId: UserID; note: string}): Promise<void> {
		return await this.accountService.dispatchUserNoteUpdate(params);
	}

	async enableMfaTotp(params: {user: User; secret: string; code: string}): Promise<Array<MfaBackupCode>> {
		return await this.authService.enableMfaTotp(params);
	}

	async disableMfaTotp(params: {
		user: User;
		code: string;
		sudoContext: SudoVerificationResult;
		password?: string;
	}): Promise<void> {
		return await this.authService.disableMfaTotp(params);
	}

	async getMfaBackupCodes(params: {
		user: User;
		regenerate: boolean;
		sudoContext: SudoVerificationResult;
		password?: string;
	}): Promise<Array<MfaBackupCode>> {
		return await this.authService.getMfaBackupCodes(params);
	}

	async regenerateMfaBackupCodes(user: User): Promise<Array<MfaBackupCode>> {
		return await this.authService.regenerateMfaBackupCodes(user);
	}

	async verifyEmail(token: string): Promise<boolean> {
		return await this.authService.verifyEmail(token);
	}

	async resendVerificationEmail(user: User): Promise<boolean> {
		return await this.authService.resendVerificationEmail(user);
	}

	async getRelationships(userId: UserID): Promise<Array<Relationship>> {
		return await this.relationshipService.getRelationships(userId);
	}

	async sendFriendRequestByTag({
		userId,
		data,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		data: FriendRequestByTagRequest;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Relationship> {
		return await this.relationshipService.sendFriendRequestByTag({userId, data, userCacheService, requestCache});
	}

	async sendFriendRequest({
		userId,
		targetId,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		targetId: UserID;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Relationship> {
		return await this.relationshipService.sendFriendRequest({userId, targetId, userCacheService, requestCache});
	}

	async acceptFriendRequest({
		userId,
		targetId,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		targetId: UserID;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Relationship> {
		const relationship = await this.relationshipService.acceptFriendRequest({
			userId,
			targetId,
			userCacheService,
			requestCache,
		});

		await this.channelService.ensureDmOpenForBothUsers({
			userId,
			recipientId: targetId,
			userCacheService,
			requestCache,
		});

		return relationship;
	}

	async blockUser({
		userId,
		targetId,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		targetId: UserID;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Relationship> {
		return await this.relationshipService.blockUser({userId, targetId, userCacheService, requestCache});
	}

	async updateFriendNickname({
		userId,
		targetId,
		nickname,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		targetId: UserID;
		nickname: string | null;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Relationship> {
		return await this.relationshipService.updateFriendNickname({
			userId,
			targetId,
			nickname,
			userCacheService,
			requestCache,
		});
	}

	async removeRelationship({userId, targetId}: {userId: UserID; targetId: UserID}): Promise<void> {
		return await this.relationshipService.removeRelationship({userId, targetId});
	}

	async dispatchRelationshipCreate({
		userId,
		relationship,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		relationship: Relationship;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<void> {
		return await this.relationshipService.dispatchRelationshipCreate({
			userId,
			relationship,
			userCacheService,
			requestCache,
		});
	}

	async dispatchRelationshipUpdate({
		userId,
		relationship,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		relationship: Relationship;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<void> {
		return await this.relationshipService.dispatchRelationshipUpdate({
			userId,
			relationship,
			userCacheService,
			requestCache,
		});
	}

	async dispatchRelationshipRemove({userId, targetId}: {userId: UserID; targetId: string}): Promise<void> {
		return await this.relationshipService.dispatchRelationshipRemove({userId, targetId});
	}

	async getPrivateChannels(userId: UserID): Promise<Array<Channel>> {
		return await this.channelService.getPrivateChannels(userId);
	}

	async createOrOpenDMChannel({
		userId,
		data,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		data: CreatePrivateChannelRequest;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<Channel> {
		return await this.channelService.createOrOpenDMChannel({userId, data, userCacheService, requestCache});
	}

	async pinDmChannel({userId, channelId}: {userId: UserID; channelId: ChannelID}): Promise<void> {
		return await this.channelService.pinDmChannel({userId, channelId});
	}

	async unpinDmChannel({userId, channelId}: {userId: UserID; channelId: ChannelID}): Promise<void> {
		return await this.channelService.unpinDmChannel({userId, channelId});
	}

	async preloadDMMessages(params: {
		userId: UserID;
		channelIds: Array<ChannelID>;
	}): Promise<Record<string, Message | null>> {
		return await this.channelService.preloadDMMessages(params);
	}

	async getBetaCodes(userId: UserID): Promise<Array<BetaCode>> {
		return await this.contentService.getBetaCodes(userId);
	}

	async getBetaCodeAllowanceInfo(userId: UserID): Promise<{allowance: number; nextResetAt: Date | null}> {
		return await this.contentService.getBetaCodeAllowanceInfo(userId);
	}

	async createBetaCode(userId: UserID): Promise<BetaCode> {
		return await this.contentService.createBetaCode(userId);
	}

	async deleteBetaCode(params: {userId: UserID; code: string}): Promise<void> {
		return await this.contentService.deleteBetaCode(params);
	}

	async getRecentMentions(params: {
		userId: UserID;
		limit: number;
		everyone: boolean;
		roles: boolean;
		guilds: boolean;
		before?: MessageID;
	}): Promise<Array<Message>> {
		return await this.contentService.getRecentMentions(params);
	}

	async deleteRecentMention({userId, messageId}: {userId: UserID; messageId: MessageID}): Promise<void> {
		return await this.contentService.deleteRecentMention({userId, messageId});
	}

	async getSavedMessages({userId, limit}: {userId: UserID; limit: number}): Promise<
		Array<{
			channelId: ChannelID;
			messageId: MessageID;
			status: SavedMessageStatus;
			message: Message | null;
		}>
	> {
		return await this.contentService.getSavedMessages({userId, limit});
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
		return await this.contentService.saveMessage({userId, channelId, messageId, userCacheService, requestCache});
	}

	async unsaveMessage({userId, messageId}: {userId: UserID; messageId: MessageID}): Promise<void> {
		return await this.contentService.unsaveMessage({userId, messageId});
	}

	async registerPushSubscription(params: {
		userId: UserID;
		endpoint: string;
		keys: {p256dh: string; auth: string};
		userAgent?: string;
	}): Promise<PushSubscription> {
		return await this.contentService.registerPushSubscription(params);
	}

	async listPushSubscriptions(userId: UserID): Promise<Array<PushSubscription>> {
		return await this.contentService.listPushSubscriptions(userId);
	}

	async deletePushSubscription(userId: UserID, subscriptionId: string): Promise<void> {
		return await this.contentService.deletePushSubscription(userId, subscriptionId);
	}

	async requestDataHarvest(userId: UserID): Promise<{harvestId: string}> {
		return await this.contentService.requestDataHarvest(userId);
	}

	async getHarvestStatus(userId: UserID, harvestId: bigint): Promise<UserHarvestResponse> {
		return await this.contentService.getHarvestStatus(userId, harvestId);
	}

	async getLatestHarvest(userId: UserID): Promise<UserHarvestResponse | null> {
		return await this.contentService.getLatestHarvest(userId);
	}

	async getHarvestDownloadUrl(
		userId: UserID,
		harvestId: bigint,
		storageService: IStorageService,
	): Promise<{downloadUrl: string; expiresAt: string}> {
		return await this.contentService.getHarvestDownloadUrl(userId, harvestId, storageService);
	}

	async requestBulkMessageDeletion(params: {userId: UserID; delayMs?: number}): Promise<void> {
		return await this.contentService.requestBulkMessageDeletion(params);
	}

	async cancelBulkMessageDeletion(userId: UserID): Promise<void> {
		return await this.contentService.cancelBulkMessageDeletion(userId);
	}

	async dispatchRecentMentionDelete({userId, messageId}: {userId: UserID; messageId: MessageID}): Promise<void> {
		return await this.contentService.dispatchRecentMentionDelete({userId, messageId});
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
		return await this.contentService.dispatchSavedMessageCreate({userId, message, userCacheService, requestCache});
	}

	async dispatchSavedMessageDelete({userId, messageId}: {userId: UserID; messageId: MessageID}): Promise<void> {
		return await this.contentService.dispatchSavedMessageDelete({userId, messageId});
	}
}
