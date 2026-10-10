// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import type {Readable} from 'node:stream';
import type {ApiContext} from '@app/api/ApiContext';
import {type ChannelID, createChannelID, createUserID, type MessageID, type UserID} from '@app/api/BrandedTypes';
import {Config} from '@app/api/Config';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import type {ChannelService} from '@app/api/channel/services/ChannelService';
import {createMessageResponseDataService} from '@app/api/channel/services/message/MessageResponseDataService';
import {
	carriesThreadArtifact,
	maskThreadArtifacts,
	maskThreadArtifactsByChannel,
} from '@app/api/channel/services/message/ThreadMessageResponses';
import type {PushSubscriptionRow} from '@app/api/database/types/UserTypes';
import {
	everEnabled,
	THREAD_FEATURE_CHANNEL_TYPES,
	type ThreadViewer,
	viewerActive,
} from '@app/api/experiment/ChannelThreadsGate';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {ISnowflakeService} from '@app/api/infrastructure/ISnowflakeService';
import type {IStorageService} from '@app/api/infrastructure/IStorageService';
import type {KVBulkMessageDeletionQueueService} from '@app/api/infrastructure/KVBulkMessageDeletionQueueService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import {Logger} from '@app/api/Logger';
import type {LimitConfigService} from '@app/api/limits/LimitConfigService';
import {resolveLimitSafe} from '@app/api/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@app/api/limits/LimitMatchContextBuilder';
import type {RequestCache} from '@app/api/middleware/RequestCacheMiddleware';
import type {Channel} from '@app/api/models/Channel';
import type {Message} from '@app/api/models/Message';
import {PushSubscription} from '@app/api/models/PushSubscription';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {BaseUserUpdatePropagator} from '@app/api/user/services/BaseUserUpdatePropagator';
import {verifyHarvestDownloadToken} from '@app/api/user/services/HarvestDownloadToken';
import {buildHarvestDownloadUrl} from '@app/api/user/services/HarvestDownloadUrl';
import {
	findInstalledLegacyPushSubscriptionIds,
	findTargetPushSubscriptionIds,
	getPushOriginReplacement,
	getPushSessionPredecessor,
	markInstalledLegacyPushSubscription,
	markPushOriginReplaced,
	markTargetPushSubscription,
	sameUserAgentFamily,
	type WebPushOriginKind,
} from '@app/api/user/services/WebPushOriginReplacement';
import {UserHarvest} from '@app/api/user/UserHarvestModel';
import {UserHarvestRepository} from '@app/api/user/UserHarvestRepository';
import {serializeSelfMessageFilter} from '@app/api/worker/utils/SelfMessageFilterPayload';
import type {WorkerTaskName} from '@app/api/worker/WorkerLaneConfig';
import {MAX_BOOKMARKS_NON_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {THREAD_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
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
import type {HarvestCreationResponse, HarvestStatusResponse} from '@fluxer/schema/src/domains/user/UserHarvestSchemas';
import type {
	BulkDeleteSelfMessagesFilter,
	HarvestSelfDataRequest,
	RegisterMobileDeviceRequest,
	UnregisterMobileDeviceRequest,
} from '@fluxer/schema/src/domains/user/UserRequestSchemas';
import type {SavedMessageStatus} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {snowflakeToDate} from '@fluxer/snowflake/src/Snowflake';
import {isPubliclyRoutableUrlShape} from '@pkgs/http_client/src/PublicInternetRequestUrlPolicy';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {ms} from 'itty-time';

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
	threadChannels?: boolean;
}

interface UnregisterMobileDeviceParams {
	userId: UserID;
	device: UnregisterMobileDeviceRequest;
}

const WEB_PUSH_PLATFORM = 'web_push' as const;
const DEFAULT_MOBILE_APP_ID = 'stable';
const DEFAULT_APNS_PROVIDER_ENVIRONMENT = 'production';
const MAX_HIDDEN_REFILL_PAGES = 3;

function savedMessageCreatePayloads(channel: Channel, data: MessageResponse | undefined): Array<unknown> {
	const guildId = channel.guildId?.toString();
	if (!data || !guildId) return [data];
	const inThread = THREAD_CHANNEL_TYPES.has(channel.type);
	if (!inThread && !carriesThreadArtifact(data)) return [data];
	const scoped = {...data, __thread_scoped: guildId};
	if (inThread) return [scoped];
	const [masked] = maskThreadArtifacts([data]);
	return masked ? [scoped, {...masked, __thread_unscoped: guildId}] : [scoped];
}

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

function isPushEndpointUrl(token: string): boolean {
	const normalized = token.trim().toLowerCase();
	return normalized.startsWith('https://') || normalized.startsWith('http://');
}

function resolveMobileWebPushKeys(device: RegisterMobileDeviceRequest): {p256dh: string; auth: string} | null {
	const p256dh = device.encryption_key;
	const auth = device.auth_secret;
	if (p256dh && auth) return {p256dh, auth};
	if (p256dh || auth) {
		throw InputValidationError.create(
			p256dh ? 'auth_secret' : 'encryption_key',
			'Web Push registrations require encryption_key and auth_secret',
		);
	}
	if (device.platform === 'android_unified_push' || device.platform === 'ios_apns_voip') {
		throw InputValidationError.create(
			'encryption_key',
			'Web Push registrations require encryption_key and auth_secret',
		);
	}
	if (isPushEndpointUrl(device.token)) {
		throw InputValidationError.create('token', 'Endpoint URL registrations require encryption_key and auth_secret');
	}
	return null;
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
	return platform === 'ios_apns' || platform === 'ios_apns_voip' ? DEFAULT_APNS_PROVIDER_ENVIRONMENT : null;
}

const isUnreachableEntityError = (error: unknown): boolean =>
	error instanceof MissingPermissionsError ||
	error instanceof UnknownChannelError ||
	error instanceof UnknownGuildError ||
	error instanceof AccessDeniedError ||
	error instanceof NsfwContentRequiresAgeVerificationError;

export class UserContentService {
	private readonly updatePropagator: BaseUserUpdatePropagator;
	private readonly userRepository: UserRepository;
	private readonly gatewayService: IGatewayService;
	private readonly workerService: IWorkerService<WorkerTaskName>;
	private readonly snowflakeService: ISnowflakeService;
	private readonly kv: IKVProvider;

	constructor(
		apiContext: ApiContext,
		userCacheService: UserCacheService,
		private channelService: ChannelService,
		private channelRepository: ChannelRepository,
		private bulkMessageDeletionQueue: KVBulkMessageDeletionQueueService,
		private limitConfigService: LimitConfigService,
	) {
		const {users, gateway, worker, snowflake, kv} = apiContext.services;
		this.userRepository = users;
		this.gatewayService = gateway;
		this.workerService = worker;
		this.snowflakeService = snowflake;
		this.kv = kv;
		this.updatePropagator = new BaseUserUpdatePropagator({
			userCacheService,
			gatewayService: this.gatewayService,
		});
	}

	async getRecentMentions(params: {
		userId: UserID;
		viewer: ThreadViewer;
		limit: number;
		everyone: boolean;
		roles: boolean;
		guilds: boolean;
		before?: MessageID;
	}): Promise<Array<Message>> {
		const {userId, viewer, limit, everyone, roles, guilds, before} = params;
		const messages: Array<Message> = [];
		let cursor = before;
		let pageLimit = limit;
		for (let refills = 0; ; refills++) {
			const mentions = await this.userRepository.listRecentMentions(userId, everyone, roles, guilds, pageLimit, cursor);
			const messagesByChannel = await this.readMessagesByChannel(userId, viewer, mentions);
			const hiddenChannelIds = await this.hiddenThreadChannelIds(viewer, messagesByChannel);
			let hidden = 0;
			for (const mention of mentions) {
				if (hiddenChannelIds.has(mention.channelId.toString())) {
					hidden++;
					continue;
				}
				const message = this.pickMessage(messagesByChannel, mention);
				if (message) messages.push(message);
			}
			if (hidden === 0 || mentions.length < pageLimit || refills === MAX_HIDDEN_REFILL_PAGES) break;
			cursor = mentions.reduce(
				(oldest, mention) => (mention.messageId < oldest ? mention.messageId : oldest),
				mentions[0]!.messageId,
			);
			pageLimit = hidden;
		}
		return messages.sort((a, b) => (b.id > a.id ? 1 : -1));
	}

	private async readMessagesByChannel(
		userId: UserID,
		viewer: ThreadViewer,
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
						viewer,
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

	async getSavedMessages({
		userId,
		viewer,
		limit,
		before,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
		limit: number;
		before?: MessageID;
	}): Promise<Array<SavedMessageEntry>> {
		const results: Array<SavedMessageEntry> = [];
		let cursor = before;
		let pageLimit = limit;
		for (let refills = 0; ; refills++) {
			const savedMessages = await this.userRepository.listSavedMessages(userId, pageLimit, cursor);
			const hidden = await this.collectSavedMessages(userId, viewer, savedMessages, results);
			if (hidden === 0 || savedMessages.length < pageLimit || refills === MAX_HIDDEN_REFILL_PAGES) break;
			cursor = savedMessages.reduce(
				(oldest, savedMessage) => (savedMessage.messageId < oldest ? savedMessage.messageId : oldest),
				savedMessages[0]!.messageId,
			);
			pageLimit = hidden;
		}
		return results.sort((a, b) => (b.messageId > a.messageId ? 1 : a.messageId > b.messageId ? -1 : 0));
	}

	private async collectSavedMessages(
		userId: UserID,
		viewer: ThreadViewer,
		savedMessages: ReadonlyArray<{channelId: ChannelID; messageId: MessageID}>,
		results: Array<SavedMessageEntry>,
	): Promise<number> {
		const messagesByChannel = await this.readMessagesByChannel(userId, viewer, savedMessages);
		const hiddenChannelIds = await this.hiddenThreadChannelIds(viewer, messagesByChannel);
		let hidden = 0;
		const staleMessageIds: Array<MessageID> = [];
		for (const savedMessage of savedMessages) {
			const channelMessages = messagesByChannel.get(savedMessage.channelId.toString());
			if (channelMessages === null) {
				if (hiddenChannelIds.has(savedMessage.channelId.toString())) {
					hidden++;
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
		return hidden;
	}

	private async hiddenThreadChannelIds(
		viewer: ThreadViewer,
		messagesByChannel: ReadonlyMap<string, Map<string, Message> | null>,
	): Promise<Set<string>> {
		if (!everEnabled()) return new Set();
		const unreachable = Array.from(messagesByChannel)
			.filter(([, messages]) => messages === null)
			.map(([channelId]) => createChannelID(BigInt(channelId)));
		if (unreachable.length === 0) return new Set();
		const channels = await this.channelRepository.channelData.listChannels(unreachable);
		return new Set(
			channels
				.filter(
					(channel) =>
						THREAD_FEATURE_CHANNEL_TYPES.has(channel.type) &&
						(channel.guildId === null || !viewerActive(viewer, channel.guildId)),
				)
				.map((channel) => channel.id.toString()),
		);
	}

	async saveMessage({
		userId,
		viewer,
		channelId,
		messageId,
		userCacheService,
		requestCache,
	}: {
		userId: UserID;
		viewer: ThreadViewer;
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
		const {channel} = await this.channelService.channelData.auth.getChannelAuthenticated({userId, channelId, viewer});
		const message = await this.channelService.messages.retrieval.getMessage({userId, viewer, channelId, messageId});
		if (!message) {
			throw new UnknownMessageError();
		}
		await this.userRepository.createSavedMessage(userId, channelId, messageId);
		await this.dispatchSavedMessageCreate({userId, message, channel, userCacheService, requestCache});
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
		originKind?: WebPushOriginKind | null;
		installedApp?: boolean;
		threadChannels?: boolean;
	}): Promise<PushSubscription> {
		const {userId, authSessionIdHash, endpoint, keys, userAgent, originKind, installedApp, threadChannels} = params;
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
			...(threadChannels === true ? {thread_channels: true} : {}),
		};
		const subscription = await this.storeWebPushSubscription(data, originKind ?? null, installedApp === true);
		return subscription;
	}

	private async storeWebPushSubscription(
		data: PushSubscriptionRow,
		originKind: WebPushOriginKind | null,
		installedApp: boolean,
	): Promise<PushSubscription> {
		if (originKind === 'legacy' && (await this.isLegacyWebPushReplaced(data, installedApp))) {
			return new PushSubscription(data);
		}
		const subscription = await this.userRepository.createPushSubscription(data);
		if (originKind === 'legacy' && installedApp) {
			await this.bestEffortPushOriginWrite(() => markInstalledLegacyPushSubscription(this.kv, data.subscription_id));
		}
		if (originKind === 'target') {
			await this.bestEffortPushOriginWrite(() => this.replaceLegacyWebPushSubscriptions(data, installedApp));
		}
		return subscription;
	}

	private async isLegacyWebPushReplaced(data: PushSubscriptionRow, installedApp: boolean): Promise<boolean> {
		const sessionIdHash = data.auth_session_id_hash;
		if (!sessionIdHash) return false;
		try {
			const replacement = await getPushOriginReplacement(this.kv, sessionIdHash);
			return replacement === 'installed' || (replacement === 'browser' && !installedApp);
		} catch (error) {
			Logger.warn({error}, 'Failed to read the web push origin replacement');
			return false;
		}
	}

	private async bestEffortPushOriginWrite(write: () => Promise<void>): Promise<void> {
		try {
			await write();
		} catch (error) {
			Logger.warn({error}, 'Failed to apply the web push origin replacement');
		}
	}

	private async replaceLegacyWebPushSubscriptions(data: PushSubscriptionRow, installedApp: boolean): Promise<void> {
		await markTargetPushSubscription(this.kv, data.subscription_id);
		const sessionIdHash = data.auth_session_id_hash;
		if (!sessionIdHash) return;
		await markPushOriginReplaced(this.kv, sessionIdHash, installedApp ? 'installed' : 'browser');
		const predecessor = await getPushSessionPredecessor(this.kv, sessionIdHash);
		const candidates = (await this.userRepository.listPushSubscriptions(data.user_id)).filter(
			(subscription) =>
				subscription.platform === WEB_PUSH_PLATFORM &&
				subscription.endpoint !== data.endpoint &&
				(subscription.authSessionIdHash === sessionIdHash ||
					(predecessor !== null &&
						subscription.authSessionIdHash === predecessor &&
						sameUserAgentFamily(subscription.userAgent, data.user_agent))),
		);
		const candidateIds = candidates.map((subscription) => subscription.subscriptionId);
		const [targetSubscriptionIds, installedLegacySubscriptionIds] = await Promise.all([
			findTargetPushSubscriptionIds(this.kv, candidateIds),
			installedApp ? Promise.resolve(new Set<string>()) : findInstalledLegacyPushSubscriptionIds(this.kv, candidateIds),
		]);
		for (const subscription of candidates) {
			if (targetSubscriptionIds.has(subscription.subscriptionId)) continue;
			if (installedLegacySubscriptionIds.has(subscription.subscriptionId)) continue;
			await this.userRepository.deletePushSubscription(data.user_id, subscription.subscriptionId);
		}
	}

	async listPushSubscriptions(userId: UserID): Promise<Array<PushSubscription>> {
		const subscriptions = await this.userRepository.listPushSubscriptions(userId);
		return subscriptions.filter((subscription) => subscription.platform === WEB_PUSH_PLATFORM);
	}

	async deletePushSubscription(userId: UserID, subscriptionId: string): Promise<void> {
		await this.userRepository.deletePushSubscription(userId, subscriptionId);
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
		originKind?: WebPushOriginKind | null;
		installedApp?: boolean;
		threadChannels?: boolean;
	}): Promise<PushSubscription> {
		const {
			userId,
			authSessionIdHash,
			oldEndpoint,
			endpoint,
			keys,
			userAgent,
			originKind,
			installedApp,
			threadChannels,
		} = params;
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
			...(threadChannels === true ? {thread_channels: true} : {}),
		};
		const subscription = await this.storeWebPushSubscription(data, originKind ?? null, installedApp === true);
		return subscription;
	}

	async registerMobileDevice(params: RegisterMobileDeviceParams): Promise<PushSubscription> {
		const {userId, authSessionIdHash, device, threadChannels} = params;
		const webPushKeys = resolveMobileWebPushKeys(device);
		if (webPushKeys) {
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
			p256dh_key: webPushKeys?.p256dh ?? null,
			auth_key: webPushKeys?.auth ?? null,
			user_agent: device.user_agent ?? null,
			platform: device.platform,
			app_id: appId,
			provider_environment: providerEnvironment,
			...(threadChannels === true ? {thread_channels: true} : {}),
		};
		const subscription = await this.userRepository.createPushSubscription(data);
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

	async requestDataHarvest(userId: UserID): Promise<HarvestCreationResponse> {
		return this.requestDataHarvestInternal(userId, null);
	}

	async requestFilteredDataHarvest(params: {
		userId: UserID;
		filter: HarvestSelfDataRequest;
	}): Promise<HarvestCreationResponse> {
		return this.requestDataHarvestInternal(params.userId, params.filter);
	}

	private async requestDataHarvestInternal(
		userId: UserID,
		filter: HarvestSelfDataRequest | null,
	): Promise<HarvestCreationResponse> {
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
						filter: serializeSelfMessageFilter(filter),
					}
				: {}),
		});
		return {
			harvest_id: harvest.harvestId.toString(),
			status: harvest.getStatus(),
			created_at: harvest.requestedAt.toISOString(),
		};
	}

	async getHarvestStatus(userId: UserID, harvestId: bigint): Promise<HarvestStatusResponse> {
		const harvestRepository = new UserHarvestRepository();
		const harvest = await harvestRepository.findByUserAndHarvestId(userId, harvestId);
		if (!harvest) {
			throw new UnknownHarvestError();
		}
		return harvest.toResponse();
	}

	async getLatestHarvest(userId: UserID): Promise<HarvestStatusResponse | null> {
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
		if (!harvest?.completedAt || !harvest.storageKey || harvest.failedAt) {
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
				filter: serializeSelfMessageFilter(filter),
			},
			{maxAttempts: 5},
		);
	}

	async cancelBulkMessageDeletion(userId: UserID): Promise<void> {
		Logger.debug({userId: userId.toString()}, 'Canceling pending bulk message deletion');
		const user = await this.userRepository.findUniqueAssert(userId);
		if ((user.flags & UserFlags.SPAMMER) !== 0n) return;
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
			const messageRefs = await this.channelRepository.messages.listMessagesByAuthor(userId, CHUNK_SIZE, lastMessageId);
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
		channel,
	}: {
		userId: UserID;
		message: Message;
		channel: Channel;
		userCacheService: UserCacheService;
		requestCache: RequestCache;
	}): Promise<void> {
		const data = (await this.buildUnmaskedResponses(userId, [message])).responses[0];
		if (!data) return;
		for (const payload of savedMessageCreatePayloads(channel, data)) {
			await this.gatewayService
				.dispatchPresence({
					userId,
					event: 'SAVED_MESSAGE_CREATE',
					data: payload,
				})
				.catch((error) => {
					Logger.error(
						{userId: userId.toString(), messageId: message.id.toString(), error},
						'Failed to dispatch SAVED_MESSAGE_CREATE',
					);
					return null;
				});
		}
	}

	async buildMessageResponsesForUser(
		userId: UserID,
		viewer: ThreadViewer,
		messages: Array<Message>,
	): Promise<Array<MessageResponse>> {
		const {responses, channelById} = await this.buildUnmaskedResponses(userId, messages);
		return maskThreadArtifactsByChannel(viewer, channelById, responses);
	}

	async buildUnmaskedResponses(
		userId: UserID,
		messages: Array<Message>,
	): Promise<{responses: Array<MessageResponse>; channelById: Map<string, Channel>}> {
		if (messages.length === 0) return {responses: [], channelById: new Map()};
		const channelIds = Array.from(new Set(messages.map((message) => message.channelId.toString())));
		const channels = await this.channelRepository.channelData.listChannels(
			channelIds.map((channelId) => createChannelID(BigInt(channelId))),
		);
		const channelById = new Map(channels.map((channel) => [channel.id.toString(), channel] as const));
		const responses = await createMessageResponseDataService().buildMessagesForChannels({
			userId,
			messages,
			channelById,
		});
		return {responses, channelById};
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
