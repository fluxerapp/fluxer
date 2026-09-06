// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import type {Readable} from 'node:stream';
import {MAX_BOOKMARKS_NON_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {UnknownMessageError} from '@fluxer/errors/src/domains/channel/UnknownMessageError';
import {AccessDeniedError} from '@fluxer/errors/src/domains/core/AccessDeniedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MaxBookmarksError} from '@fluxer/errors/src/domains/core/MaxBookmarksError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {HarvestExpiredError} from '@fluxer/errors/src/domains/moderation/HarvestExpiredError';
import {HarvestFailedError} from '@fluxer/errors/src/domains/moderation/HarvestFailedError';
import {HarvestNotReadyError} from '@fluxer/errors/src/domains/moderation/HarvestNotReadyError';
import {NsfwContentRequiresAgeVerificationError} from '@fluxer/errors/src/domains/moderation/NsfwContentRequiresAgeVerificationError';
import {UnknownHarvestError} from '@fluxer/errors/src/domains/moderation/UnknownHarvestError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {
	BulkDeleteSelfMessagesFilter,
	HarvestSelfDataRequest,
	RegisterMobileDeviceRequest,
	UnregisterMobileDeviceRequest,
} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import type {SavedMessageStatus} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import {isPubliclyRoutableUrlShape} from '@pkgs/http_client/src/PublicInternetRequestUrlPolicy';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {ms} from 'itty-time';
import type {ApiContext} from '../../ApiContext';
import {type ChannelID, createChannelID, createUserID, type MessageID, type UserID} from '../../BrandedTypes';
import {Config} from '../../Config';
import type {IChannelRepository} from '../../channel/IChannelRepository';
import type {ChannelService} from '../../channel/services/ChannelService';
import {createMessageResponseDataService} from '../../channel/services/message/MessageResponseDataService';
import type {PushSubscriptionRow} from '../../database/types/UserTypes';
import type {IGatewayService} from '../../infrastructure/IGatewayService';
import type {ISnowflakeService} from '../../infrastructure/ISnowflakeService';
import type {IStorageService} from '../../infrastructure/IStorageService';
import type {KVBulkMessageDeletionQueueService} from '../../infrastructure/KVBulkMessageDeletionQueueService';
import type {UserCacheService} from '../../infrastructure/UserCacheService';
import {Logger} from '../../Logger';
import type {LimitConfigService} from '../../limits/LimitConfigService';
import {resolveLimitSafe} from '../../limits/LimitConfigUtils';
import {createLimitMatchContext} from '../../limits/LimitMatchContextBuilder';
import type {RequestCache} from '../../middleware/RequestCacheMiddleware';
import type {Message} from '../../models/Message';
import type {PushSubscription} from '../../models/PushSubscription';
import type {WorkerTaskName} from '../../worker/WorkerLaneConfig';
import type {IUserAccountRepository} from '../repositories/IUserAccountRepository';
import type {IUserContentRepository} from '../repositories/IUserContentRepository';
import {UserHarvest, type UserHarvestResponse} from '../UserHarvestModel';
import {UserHarvestRepository} from '../UserHarvestRepository';
import {BaseUserUpdatePropagator} from './BaseUserUpdatePropagator';
import {verifyHarvestDownloadToken} from './HarvestDownloadToken';
import {buildHarvestDownloadUrl} from './HarvestDownloadUrl';

export interface SavedMessageEntry {
	channelId: ChannelID;
	messageId: MessageID;
	status: SavedMessageStatus;
	message: Message | null;
}

interface RegisterMobileDeviceParams {
	userId: UserID;
	authSessionIdHash?: string | null;
	device: RegisterMobileDeviceRequest;
}

interface UnregisterMobileDeviceParams {
	userId: UserID;
	device: UnregisterMobileDeviceRequest;
}

interface UserContentRepository extends IUserAccountRepository, IUserContentRepository {}

const WEB_PUSH_PLATFORM = 'web_push' as const;
const DEFAULT_MOBILE_APP_ID = 'stable';
const DEFAULT_APNS_PROVIDER_ENVIRONMENT = 'production';

function createPushSubscriptionId(parts: Array<string>): string {
	const stableInput = parts.map((part) => `${part.length}:${part}`).join('|');
	return crypto.createHash('sha256').update(stableInput).digest('hex').substring(0, 32);
}

function createWebPushSubscriptionId(endpoint: string): string {
	return crypto.createHash('sha256').update(endpoint).digest('hex').substring(0, 32);
}

function assertPublicPushEndpoint(endpoint: string, fieldName: string): void {
	let parsedUrl: URL;
	try {
		parsedUrl = new URL(endpoint);
	} catch {
		throw InputValidationError.fromCode(fieldName, ValidationErrorCodes.INVALID_URL_FORMAT);
	}
	if (!isPubliclyRoutableUrlShape(parsedUrl)) {
		throw InputValidationError.fromCode(fieldName, ValidationErrorCodes.URL_NOT_PUBLICLY_ROUTABLE);
	}
}

function normalizeMobileAppId(appId: string | undefined): string {
	const normalized = appId?.trim();
	return normalized && normalized.length > 0 ? normalized : DEFAULT_MOBILE_APP_ID;
}

function normalizeProviderEnvironment(
	platform: RegisterMobileDeviceRequest['platform'],
	environment: RegisterMobileDeviceRequest['provider_environment'],
): string | null {
	if (environment) return environment;
	return platform === 'ios_apns' ? DEFAULT_APNS_PROVIDER_ENVIRONMENT : null;
}

const isUnreachableEntityError = (error: unknown): boolean =>
	error instanceof MissingPermissionsError ||
	error instanceof UnknownChannelError ||
	error instanceof UnknownGuildError ||
	error instanceof AccessDeniedError ||
	error instanceof NsfwContentRequiresAgeVerificationError;

export const UserContentServiceTestHooks = {isUnreachableEntityError};

export class UserContentService {
	private readonly updatePropagator: BaseUserUpdatePropagator;
	private readonly userRepository: UserContentRepository;
	private readonly gatewayService: IGatewayService;
	private readonly workerService: IWorkerService<WorkerTaskName>;
	private readonly snowflakeService: ISnowflakeService;

	constructor(
		apiContext: ApiContext,
		userCacheService: UserCacheService,
		private channelService: ChannelService,
		private channelRepository: IChannelRepository,
		private bulkMessageDeletionQueue: KVBulkMessageDeletionQueueService,
		private limitConfigService: LimitConfigService,
	) {
		const {users, gateway, worker, snowflake} = apiContext.services;
		this.userRepository = users;
		this.gatewayService = gateway;
		this.workerService = worker;
		this.snowflakeService = snowflake;
		this.updatePropagator = new BaseUserUpdatePropagator({
			userCacheService,
			gatewayService: this.gatewayService,
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
		const mentions = await this.userRepository.listRecentMentions(userId, everyone, roles, guilds, limit, before);
		const messagesByChannel = await this.readMessagesByChannel(userId, mentions);
		const messages = mentions
			.map((mention) => this.pickMessage(messagesByChannel, mention))
			.filter((message): message is Message => message != null);
		return messages.sort((a, b) => (b.id > a.id ? 1 : -1));
	}

	private async readMessagesByChannel(
		userId: UserID,
		entries: ReadonlyArray<{channelId: ChannelID; messageId: MessageID}>,
	): Promise<Map<string, Map<string, Message> | null>> {
		const grouped = new Map<string, {channelId: ChannelID; messageIds: Array<MessageID>}>();
		for (const entry of entries) {
			const key = entry.channelId.toString();
			const group = grouped.get(key);
			if (group) {
				group.messageIds.push(entry.messageId);
			} else {
				grouped.set(key, {channelId: entry.channelId, messageIds: [entry.messageId]});
			}
		}
		const messagesByChannel = new Map<string, Map<string, Message> | null>();
		await Promise.all(
			Array.from(grouped, async ([key, group]) => {
				try {
					const messages = await this.channelService.messages.retrieval.getMessagesByIds({
						userId,
						channelId: group.channelId,
						messageIds: group.messageIds,
					});
					messagesByChannel.set(key, messages);
				} catch (error) {
					if (isUnreachableEntityError(error)) {
						messagesByChannel.set(key, null);
						return;
					}
					throw error;
				}
			}),
		);
		return messagesByChannel;
	}

	private pickMessage(
		messagesByChannel: ReadonlyMap<string, Map<string, Message> | null>,
		entry: {channelId: ChannelID; messageId: MessageID},
	): Message | null {
		return messagesByChannel.get(entry.channelId.toString())?.get(entry.messageId.toString()) ?? null;
	}

	async deleteRecentMention({userId, messageId}: {userId: UserID; messageId: MessageID}): Promise<void> {
		const recentMention = await this.userRepository.getRecentMention(userId, messageId);
		if (!recentMention) return;
		await this.userRepository.deleteRecentMention(recentMention);
		await this.dispatchRecentMentionDelete({userId, messageId});
	}

	async deleteRecentMentions({userId, messageIds}: {userId: UserID; messageIds: Array<MessageID>}): Promise<void> {
		if (messageIds.length === 0) return;
		const mentions = (
			await Promise.all(messageIds.map((messageId) => this.userRepository.getRecentMention(userId, messageId)))
		).filter((mention) => mention != null);
		if (mentions.length === 0) return;
		await this.userRepository.deleteRecentMentions(mentions);
		await Promise.all(
			mentions.map((mention) => this.dispatchRecentMentionDelete({userId, messageId: mention.messageId})),
		);
	}

	async getSavedMessages({userId, limit}: {userId: UserID; limit: number}): Promise<Array<SavedMessageEntry>> {
		const savedMessages = await this.userRepository.listSavedMessages(userId, limit);
		const messagesByChannel = await this.readMessagesByChannel(userId, savedMessages);
		const results: Array<SavedMessageEntry> = [];
		const staleMessageIds: Array<MessageID> = [];
		for (const savedMessage of savedMessages) {
			const channelMessages = messagesByChannel.get(savedMessage.channelId.toString());
			if (channelMessages === null) {
				results.push({
					channelId: savedMessage.channelId,
					messageId: savedMessage.messageId,
					status: 'missing_permissions',
					message: null,
				});
				continue;
			}
			const message = this.pickMessage(messagesByChannel, savedMessage);
			if (!message) {
				const stored = await this.channelRepository.messages.getMessage(savedMessage.channelId, savedMessage.messageId);
				if (!stored) {
					staleMessageIds.push(savedMessage.messageId);
					continue;
				}
				results.push({
					channelId: savedMessage.channelId,
					messageId: savedMessage.messageId,
					status: 'missing_permissions',
					message: null,
				});
				continue;
			}
			results.push({
				channelId: savedMessage.channelId,
				messageId: savedMessage.messageId,
				status: 'available',
				message,
			});
		}
		await Promise.all(staleMessageIds.map((messageId) => this.userRepository.deleteSavedMessage(userId, messageId)));
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
		const user = await this.userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		const savedMessageCount = await this.userRepository.countSavedMessages(userId);
		const ctx = createLimitMatchContext({user});
		const maxBookmarks = resolveLimitSafe(
			this.limitConfigService.getConfigSnapshot(),
			ctx,
			'max_bookmarks',
			MAX_BOOKMARKS_NON_PREMIUM,
		);
		if (savedMessageCount >= maxBookmarks) {
			throw new MaxBookmarksError({maxBookmarks});
		}
		await this.channelService.channelData.auth.getChannelAuthenticated({userId, channelId});
		const message = await this.channelService.messages.retrieval.getMessage({userId, channelId, messageId});
		if (!message) {
			throw new UnknownMessageError();
		}
		await this.userRepository.createSavedMessage(userId, channelId, messageId);
		await this.dispatchSavedMessageCreate({userId, message, userCacheService, requestCache});
	}

	async unsaveMessage({userId, messageId}: {userId: UserID; messageId: MessageID}): Promise<void> {
		await this.userRepository.deleteSavedMessage(userId, messageId);
		await this.dispatchSavedMessageDelete({userId, messageId});
	}

	async registerPushSubscription(params: {
		userId: UserID;
		authSessionIdHash?: string | null;
		endpoint: string;
		keys: {
			p256dh: string;
			auth: string;
		};
		userAgent?: string;
	}): Promise<PushSubscription> {
		const {userId, authSessionIdHash, endpoint, keys, userAgent} = params;
		assertPublicPushEndpoint(endpoint, 'endpoint');
		const subscriptionId = createWebPushSubscriptionId(endpoint);
		const data: PushSubscriptionRow = {
			user_id: userId,
			subscription_id: subscriptionId,
			auth_session_id_hash: authSessionIdHash ?? null,
			endpoint,
			p256dh_key: keys.p256dh,
			auth_key: keys.auth,
			user_agent: userAgent ?? null,
			platform: WEB_PUSH_PLATFORM,
			app_id: null,
			provider_environment: null,
		};
		const subscription = await this.userRepository.createPushSubscription(data);
		await this.gatewayService.invalidatePushSubscriptions({userId});
		return subscription;
	}

	async listPushSubscriptions(userId: UserID): Promise<Array<PushSubscription>> {
		const subscriptions = await this.userRepository.listPushSubscriptions(userId);
		return subscriptions.filter((subscription) => subscription.platform === WEB_PUSH_PLATFORM);
	}

	async deletePushSubscription(userId: UserID, subscriptionId: string): Promise<void> {
		await this.userRepository.deletePushSubscription(userId, subscriptionId);
		await this.gatewayService.invalidatePushSubscriptions({userId});
	}

	async rotatePushSubscription(params: {
		userId: UserID;
		authSessionIdHash?: string | null;
		oldEndpoint: string;
		endpoint: string;
		keys: {
			p256dh: string;
			auth: string;
		};
		userAgent?: string;
	}): Promise<PushSubscription> {
		const {userId, authSessionIdHash, oldEndpoint, endpoint, keys, userAgent} = params;
		assertPublicPushEndpoint(endpoint, 'endpoint');
		const oldSubscriptionId = createWebPushSubscriptionId(oldEndpoint);
		const newSubscriptionId = createWebPushSubscriptionId(endpoint);
		if (oldSubscriptionId !== newSubscriptionId) {
			await this.userRepository.deletePushSubscription(userId, oldSubscriptionId);
		}
		const data: PushSubscriptionRow = {
			user_id: userId,
			subscription_id: newSubscriptionId,
			auth_session_id_hash: authSessionIdHash ?? null,
			endpoint,
			p256dh_key: keys.p256dh,
			auth_key: keys.auth,
			user_agent: userAgent ?? null,
			platform: WEB_PUSH_PLATFORM,
			app_id: null,
			provider_environment: null,
		};
		const subscription = await this.userRepository.createPushSubscription(data);
		await this.gatewayService.invalidatePushSubscriptions({userId});
		return subscription;
	}

	async registerMobileDevice(params: RegisterMobileDeviceParams): Promise<PushSubscription> {
		const {userId, authSessionIdHash, device} = params;
		if (device.platform === 'android_unified_push') {
			assertPublicPushEndpoint(device.token, 'token');
		}
		const appId = normalizeMobileAppId(device.app_id);
		const providerEnvironment = normalizeProviderEnvironment(device.platform, device.provider_environment);
		const subscriptionId = createPushSubscriptionId([device.platform, appId, providerEnvironment ?? '', device.token]);
		const data: PushSubscriptionRow = {
			user_id: userId,
			subscription_id: subscriptionId,
			auth_session_id_hash: authSessionIdHash ?? null,
			endpoint: device.token,
			p256dh_key: device.platform === 'android_unified_push' ? (device.encryption_key ?? null) : null,
			auth_key: device.platform === 'android_unified_push' ? (device.auth_secret ?? null) : null,
			user_agent: device.user_agent ?? null,
			platform: device.platform,
			app_id: appId,
			provider_environment: providerEnvironment,
		};
		const subscription = await this.userRepository.createPushSubscription(data);
		await this.gatewayService.invalidatePushSubscriptions({userId});
		return subscription;
	}

	async listMobileDevices(userId: UserID): Promise<Array<PushSubscription>> {
		const subscriptions = await this.userRepository.listPushSubscriptions(userId);
		return subscriptions.filter((subscription) => subscription.platform !== WEB_PUSH_PLATFORM);
	}

	async deleteMobileDevice(userId: UserID, deviceId: string): Promise<void> {
		await this.deletePushSubscription(userId, deviceId);
	}

	async unregisterMobileDevice(params: UnregisterMobileDeviceParams): Promise<void> {
		const {userId, device} = params;
		const appId = normalizeMobileAppId(device.app_id);
		const providerEnvironment = normalizeProviderEnvironment(device.platform, device.provider_environment);
		const deviceId = createPushSubscriptionId([device.platform, appId, providerEnvironment ?? '', device.token]);
		await this.deleteMobileDevice(userId, deviceId);
	}

	async requestDataHarvest(userId: UserID): Promise<{
		harvest_id: string;
		status: 'pending' | 'processing' | 'completed' | 'failed';
		created_at: string;
	}> {
		return this.requestDataHarvestInternal(userId, null);
	}

	async requestFilteredDataHarvest(params: {userId: UserID; filter: HarvestSelfDataRequest}): Promise<{
		harvest_id: string;
		status: 'pending' | 'processing' | 'completed' | 'failed';
		created_at: string;
	}> {
		return this.requestDataHarvestInternal(params.userId, params.filter);
	}

	private async requestDataHarvestInternal(
		userId: UserID,
		filter: HarvestSelfDataRequest | null,
	): Promise<{
		harvest_id: string;
		status: 'pending' | 'processing' | 'completed' | 'failed';
		created_at: string;
	}> {
		const user = await this.userRepository.findUnique(userId);
		if (!user) throw new UnknownUserError();
		const harvestId = await this.snowflakeService.generate();
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
			...(filter
				? {
						filter: {
							scope: filter.scope,
							includeDms: filter.include_dms,
							includeDmsClosed: filter.include_dms_closed,
							includeGroupDms: filter.include_group_dms,
							includeGuilds: filter.include_guilds,
							guildFilterMode: filter.guild_filter_mode,
							excludedGuildIds: filter.excluded_guild_ids.map((id) => id.toString()),
							includedGuildIds: filter.included_guild_ids.map((id) => id.toString()),
							startTimestamp: filter.start_date ? new Date(filter.start_date).getTime() : null,
							endTimestamp: filter.end_date ? new Date(filter.end_date).getTime() : null,
						},
					}
				: {}),
		});
		return {
			harvest_id: harvest.harvestId.toString(),
			status: harvest.getStatus(),
			created_at: harvest.requestedAt.toISOString(),
		};
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
	): Promise<{
		download_url: string;
		expires_at: string;
	}> {
		const harvestRepository = new UserHarvestRepository();
		const harvest = await harvestRepository.findByUserAndHarvestId(userId, harvestId);
		if (!harvest) {
			throw new UnknownHarvestError();
		}
		if (harvest.failedAt) {
			throw new HarvestFailedError();
		}
		if (!harvest.completedAt || !harvest.storageKey) {
			throw new HarvestNotReadyError();
		}
		if (harvest.downloadUrlExpiresAt && harvest.downloadUrlExpiresAt < new Date()) {
			throw new HarvestExpiredError();
		}
		const ZIP_EXPIRY_MS = ms('7 days');
		const downloadUrl = await buildHarvestDownloadUrl({
			userId,
			harvestId,
			storageKey: harvest.storageKey,
			expiresInSeconds: ZIP_EXPIRY_MS / 1000,
			storageService,
		});
		const expiresAt = new Date(Date.now() + ZIP_EXPIRY_MS);
		return {
			download_url: downloadUrl,
			expires_at: expiresAt.toISOString(),
		};
	}

	async streamHarvestDownload(params: {
		harvestId: bigint;
		token: string;
		range?: string;
		storageService: IStorageService;
	}): Promise<{
		body: Readable;
		contentLength: number;
		contentRange?: string | null;
		contentType?: string | null;
		filename: string;
	} | null> {
		if (Config.presignedHarvestDownloadsEnabled) {
			return null;
		}
		const payload = verifyHarvestDownloadToken(params.token, Config.auth.connectionInitiationSecret);
		if (!payload || payload.harvestId !== params.harvestId.toString()) {
			Logger.debug({harvestId: params.harvestId.toString()}, 'Harvest download rejected: invalid or expired token');
			return null;
		}
		let userId: UserID;
		try {
			userId = createUserID(BigInt(payload.userId));
		} catch {
			return null;
		}
		const harvestRepository = new UserHarvestRepository();
		const harvest = await harvestRepository.findByUserAndHarvestId(userId, params.harvestId);
		if (!harvest || !harvest.completedAt || !harvest.storageKey || harvest.failedAt) {
			return null;
		}
		if (harvest.downloadUrlExpiresAt && harvest.downloadUrlExpiresAt < new Date()) {
			return null;
		}
		if (harvest.storageKey !== payload.storageKey) {
			Logger.debug({harvestId: params.harvestId.toString()}, 'Harvest download rejected: storage key mismatch');
			return null;
		}
		const object = await params.storageService.streamObject({
			bucket: Config.s3.buckets.harvests,
			key: harvest.storageKey,
			range: params.range,
		});
		if (!object) {
			return null;
		}
		return {
			body: object.body,
			contentLength: object.contentLength,
			contentRange: object.contentRange,
			contentType: object.contentType ?? 'application/zip',
			filename: `fluxer-data-${params.harvestId}.zip`,
		};
	}

	async requestBulkMessageDeletion(params: {userId: UserID; delayMs?: number}): Promise<void> {
		const {userId, delayMs = ms('1 day')} = params;
		const scheduledAt = new Date(Date.now() + delayMs);
		const user = await this.userRepository.findUniqueAssert(userId);
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
		const updatedUser = await this.userRepository.patchUpsert(
			userId,
			{
				pending_bulk_message_deletion_at: scheduledAt,
				pending_bulk_message_deletion_channel_count: counts.channelCount,
				pending_bulk_message_deletion_message_count: counts.messageCount,
			},
			user.toRow(),
		);
		await this.bulkMessageDeletionQueue.scheduleDeletion(userId, scheduledAt);
		await this.updatePropagator.dispatchUserUpdate(updatedUser);
	}

	async bulkDeleteSelfMessagesImmediate(params: {userId: UserID; filter: BulkDeleteSelfMessagesFilter}): Promise<void> {
		const {userId, filter} = params;
		Logger.debug({userId: userId.toString(), scope: filter.scope}, 'Enqueueing immediate bulk self message deletion');
		await this.workerService.addJob(
			'bulkDeleteSelfMessagesImmediate',
			{
				userId: userId.toString(),
				filter: {
					scope: filter.scope,
					includeDms: filter.include_dms,
					includeDmsClosed: filter.include_dms_closed,
					includeGroupDms: filter.include_group_dms,
					includeGuilds: filter.include_guilds,
					guildFilterMode: filter.guild_filter_mode,
					excludedGuildIds: filter.excluded_guild_ids.map((id) => id.toString()),
					includedGuildIds: filter.included_guild_ids.map((id) => id.toString()),
					startTimestamp: filter.start_date ? new Date(filter.start_date).getTime() : null,
					endTimestamp: filter.end_date ? new Date(filter.end_date).getTime() : null,
				},
			},
			{maxAttempts: 5},
		);
	}

	async cancelBulkMessageDeletion(userId: UserID): Promise<void> {
		Logger.debug({userId: userId.toString()}, 'Canceling pending bulk message deletion');
		const user = await this.userRepository.findUniqueAssert(userId);
		const updatedUser = await this.userRepository.patchUpsert(
			userId,
			{
				pending_bulk_message_deletion_at: null,
				pending_bulk_message_deletion_channel_count: null,
				pending_bulk_message_deletion_message_count: null,
			},
			user.toRow(),
		);
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
		let lastMessageId: MessageID | undefined;
		const channels = new Set<string>();
		let messageCount = 0;
		while (true) {
			const messageRefs = await this.channelRepository.listMessagesByAuthor(userId, CHUNK_SIZE, lastMessageId);
			if (messageRefs.length === 0) {
				break;
			}
			for (const {channelId, messageId} of messageRefs) {
				if (snowflakeToDate(messageId).getTime() > cutoffMs) {
					continue;
				}
				channels.add(channelId.toString());
				messageCount++;
			}
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
		await this.gatewayService
			.dispatchPresence({
				userId,
				event: 'RECENT_MENTION_DELETE',
				data: {message_id: messageId.toString()},
			})
			.catch((error) => {
				Logger.error(
					{userId: userId.toString(), messageId: messageId.toString(), error},
					'Failed to dispatch RECENT_MENTION_DELETE',
				);
				return null;
			});
	}

	async dispatchSavedMessageCreate({
		userId,
		message,
	}: {
		userId: UserID;
		message: Message;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<void> {
		const data = (await this.buildMessageResponsesForUser(userId, [message]))[0];
		await this.gatewayService
			.dispatchPresence({
				userId,
				event: 'SAVED_MESSAGE_CREATE',
				data,
			})
			.catch((error) => {
				Logger.error(
					{userId: userId.toString(), messageId: message.id.toString(), error},
					'Failed to dispatch SAVED_MESSAGE_CREATE',
				);
				return null;
			});
	}

	async buildMessageResponsesForUser(userId: UserID, messages: Array<Message>): Promise<Array<MessageResponse>> {
		if (messages.length === 0) return [];
		const channelIds = Array.from(new Set(messages.map((message) => message.channelId.toString())));
		const channels = await this.channelRepository.listChannels(
			channelIds.map((channelId) => createChannelID(BigInt(channelId))),
		);
		const channelById = new Map(channels.map((channel) => [channel.id.toString(), channel] as const));
		return createMessageResponseDataService().buildMessagesForChannels({
			userId,
			messages,
			channelById,
		});
	}

	async dispatchSavedMessageDelete({userId, messageId}: {userId: UserID; messageId: MessageID}): Promise<void> {
		await this.gatewayService
			.dispatchPresence({
				userId,
				event: 'SAVED_MESSAGE_DELETE',
				data: {message_id: messageId.toString()},
			})
			.catch((error) => {
				Logger.error(
					{userId: userId.toString(), messageId: messageId.toString(), error},
					'Failed to dispatch SAVED_MESSAGE_DELETE',
				);
				return null;
			});
	}
}
