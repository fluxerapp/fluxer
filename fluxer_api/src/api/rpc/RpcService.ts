// SPDX-License-Identifier: AGPL-3.0-or-later

import {createHash} from 'node:crypto';
import {AUTOMATIC_VOICE_REGION_ID, ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {MAX_PRIVATE_CHANNELS_PER_USER} from '@fluxer/constants/src/LimitConstants';
import {GroupDmAddPermissionFlags, IncomingCallFlags, UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {UnauthorizedError} from '@fluxer/errors/src/domains/core/UnauthorizedError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {VoiceStateResponse} from '@fluxer/schema/src/domains/gateway/GatewaySchemas';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {
	RpcGuildCollectionType,
	RpcRequest,
	RpcResponse,
	RpcResponseGuildCollectionData,
	RpcResponseSessionData,
} from '@fluxer/schema/src/domains/rpc/RpcSchemas';
import type {RelationshipResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import type {IRateLimitService} from '@pkgs/rate_limit/src/IRateLimitService';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import {ms} from 'itty-time';
import sharp from 'sharp';
import {uint8ArrayToBase64} from 'uint8array-extras';
import type {ApiContext} from '../ApiContext';
import * as AuthSession from '../auth/AuthSession';
import type {ChannelID, GuildID, UserID} from '../BrandedTypes';
import {
	createChannelID,
	createGuildID,
	createMessageID,
	createUserID,
	userIdToChannelId,
	vanityCodeToInviteCode,
} from '../BrandedTypes';
import {Config} from '../Config';
import {mapChannelToResponse} from '../channel/ChannelMappers';
import type {IChannelRepository} from '../channel/IChannelRepository';
import {buildBroadcastMessageData} from '../channel/services/message/MessageGatewayDispatch';
import {ensurePersonalNotesChannelExists} from '../channel/services/PersonalNotesChannelRepair';
import {mapFavoriteMemeToResponse} from '../favorite_meme/FavoriteMemeModel';
import type {IFavoriteMemeRepository} from '../favorite_meme/IFavoriteMemeRepository';
import {
	mapGuildEmojiToResponse,
	mapGuildMemberToResponse,
	mapGuildRoleToResponse,
	mapGuildStickerToResponse,
	mapGuildToGuildResponse,
} from '../guild/GuildModel';
import type {IGuildRepositoryAggregate} from '../guild/repositories/IGuildRepositoryAggregate';
import type {AvatarService} from '../infrastructure/AvatarService';
import type {IDiscriminatorService} from '../infrastructure/DiscriminatorService';
import type {IGatewayService} from '../infrastructure/IGatewayService';
import type {ListParticipantsResult} from '../infrastructure/ILiveKitService';
import type {IStorageService} from '../infrastructure/IStorageService';
import type {PremiumStateReconciliationQueueService} from '../infrastructure/PremiumStateReconciliationQueueService';
import type {UserCacheService} from '../infrastructure/UserCacheService';
import type {InstanceConfigRepository} from '../instance/InstanceConfigRepository';
import type {IInviteRepository} from '../invite/IInviteRepository';
import {Logger} from '../Logger';
import type {LimitConfigService} from '../limits/LimitConfigService';
import {resolveLimitSafe} from '../limits/LimitConfigUtils';
import {createLimitMatchContext} from '../limits/LimitMatchContextBuilder';
import type {RequestCache} from '../middleware/RequestCacheMiddleware';
import type {AuthSession as AuthSessionModel} from '../models/AuthSession';
import type {Channel} from '../models/Channel';
import type {FavoriteMeme} from '../models/FavoriteMeme';
import type {Guild} from '../models/Guild';
import type {GuildMember} from '../models/GuildMember';
import type {GuildSticker} from '../models/GuildSticker';
import type {ReadState} from '../models/ReadState';
import type {Relationship} from '../models/Relationship';
import type {User} from '../models/User';
import type {UserGuildSettings} from '../models/UserGuildSettings';
import {UserSettings} from '../models/UserSettings';
import type {WebAuthnCredential} from '../models/WebAuthnCredential';
import type {BotAuthService} from '../oauth/BotAuthService';
import {sendApnsPush} from '../push/ApnsPushService';
import {encodeReadStatesResponseProto, mapReadStateResponse} from '../read_state/ReadStateResponseMapper';
import type {ReadStateService} from '../read_state/ReadStateService';
import type {IUserRepository} from '../user/IUserRepository';
import {PaymentRepository} from '../user/repositories/PaymentRepository';
import {CustomStatusValidator} from '../user/services/CustomStatusValidator';
import {getCachedUserPartialResponse} from '../user/UserCacheHelpers';
import {
	mapRelationshipToResponse,
	mapUserGuildSettingsToResponse,
	mapUserSettingsToResponse,
	mapUserToPrivateResponse,
} from '../user/UserMappers';
import {isUserAdult} from '../utils/AgeUtils';
import {deriveDominantAvatarColor} from '../utils/AvatarColorUtils';
import {calculateDistance, parseCoordinate} from '../utils/GeoUtils';
import {lookupGeoip} from '../utils/IpUtils';
import type {VoiceAccessContext, VoiceAvailabilityService} from '../voice/VoiceAvailabilityService';
import type {VoiceService} from '../voice/VoiceService';
import type {IWebhookRepository} from '../webhook/IWebhookRepository';
import type {WorkerTaskName} from '../worker/WorkerLaneConfig';
import {RpcSessionStartService} from './RpcSessionStartService';
import {
	createRpcTimingNode,
	RpcTimingRecorder,
	type RpcTimingSteps,
	startRpcTiming,
	timeRpcStep,
	timeRpcStepSync,
} from './RpcTimings';

interface HandleRpcRequestParams {
	request: RpcRequest;
	requestCache: RequestCache;
}

interface HandleSessionRequestParams {
	token: string;
	version: number;
	requestCache: RequestCache;
	ip?: string;
	latitude?: string;
	longitude?: string;
}

interface HandleGuildCollectionRequestParams {
	guildId: GuildID;
	collection: RpcGuildCollectionType;
	requestCache: RequestCache;
	afterUserId?: UserID;
	limit?: number;
}

interface GetUserDataParams {
	userId: UserID;
	includePrivateChannels?: boolean;
	timingSteps?: RpcTimingSteps;
}

interface UserData {
	user: User;
	settings: UserSettings | null;
	guildSettings: Array<UserGuildSettings>;
	notes: Map<UserID, string>;
	readStates: Array<ReadState>;
	guildIds: Array<GuildID>;
	privateChannels: Array<Channel>;
	relationships: Array<Relationship>;
	favoriteMemes: Array<FavoriteMeme>;
	pinnedDMs: Array<ChannelID>;
	webAuthnCredentials: Array<WebAuthnCredential>;
}

interface RpcVoiceParticipantSnapshot {
	identity: string;
	user_id: string;
	connection_id: string;
}

const GUILD_COLLECTION_DEFAULT_LIMIT = 250;
const GUILD_COLLECTION_MAX_LIMIT = 1000;
const RPC_RESPONSE_MAP_CONCURRENCY = 32;
const VOICE_STATE_KV_KEY_PREFIX = 'voice:guild:states:';
const PREMIUM_RECONCILE_ENQUEUE_COOLDOWN_SECONDS = 10 * 60;
const PREMIUM_RECONCILE_ENQUEUE_COOLDOWN_KEY_PREFIX = 'rpc:premium:reconcile:enqueue:';
const PAYMENT_RECONCILE_ENQUEUE_COOLDOWN_SECONDS = 30 * 60;
const PAYMENT_RECONCILE_ENQUEUE_COOLDOWN_KEY_PREFIX = 'rpc:payment:reconcile:enqueue:';

function serializeCoordinate(value: number | null | undefined): string | undefined {
	if (value === null || value === undefined || !Number.isFinite(value)) {
		return undefined;
	}
	return value.toString();
}

function parseVoiceParticipantIdentity(identity: string): RpcVoiceParticipantSnapshot | null {
	const match = /^user_(\d+)_(.+)$/.exec(identity);
	if (!match) {
		return null;
	}
	const [, userId, connectionId] = match;
	if (!userId || !connectionId) {
		return null;
	}
	return {
		identity,
		user_id: userId,
		connection_id: connectionId,
	};
}

function mapVoiceParticipantSnapshot(result: ListParticipantsResult): {
	status: 'ok' | 'error';
	participants: Array<RpcVoiceParticipantSnapshot>;
	error_code?: string;
	retryable?: boolean;
	server_missing?: boolean;
} {
	if (result.status === 'error') {
		const errorSnapshot: {
			status: 'error';
			participants: Array<RpcVoiceParticipantSnapshot>;
			error_code: string;
			retryable: boolean;
			server_missing?: boolean;
		} = {
			status: 'error',
			participants: [],
			error_code: result.errorCode,
			retryable: result.retryable,
		};
		if (result.serverMissing !== undefined) {
			errorSnapshot.server_missing = result.serverMissing;
		}
		return errorSnapshot;
	}
	return {
		status: 'ok',
		participants: result.participants.flatMap((participant) => {
			const parsed = parseVoiceParticipantIdentity(participant.identity);
			return parsed ? [parsed] : [];
		}),
	};
}

export class RpcService {
	private readonly customStatusValidator: CustomStatusValidator;
	private readonly sessionStartService: RpcSessionStartService;

	constructor(
		private userRepository: IUserRepository,
		private guildRepository: IGuildRepositoryAggregate,
		private channelRepository: IChannelRepository,
		private userCacheService: UserCacheService,
		private readStateService: ReadStateService,
		private apiContext: ApiContext,
		private gatewayService: IGatewayService,
		private discriminatorService: IDiscriminatorService,
		private favoriteMemeRepository: IFavoriteMemeRepository,
		private botAuthService: BotAuthService,
		private inviteRepository: IInviteRepository,
		private webhookRepository: IWebhookRepository,
		private storageService: IStorageService,
		private avatarService: AvatarService,
		private rateLimitService: IRateLimitService,
		private readonly limitConfigService: LimitConfigService,
		private readonly kvClient: IKVProvider,
		private readonly workerService: IWorkerService<WorkerTaskName>,
		private readonly premiumStateReconciliationQueueService: PremiumStateReconciliationQueueService,
		private readonly instanceConfigRepository: InstanceConfigRepository,
		private voiceService: VoiceService | null,
		private voiceAvailabilityService: VoiceAvailabilityService | null,
	) {
		this.customStatusValidator = new CustomStatusValidator(
			this.userRepository,
			this.guildRepository,
			this.limitConfigService,
		);
		this.sessionStartService = new RpcSessionStartService({
			userRepository: this.userRepository,
			guildRepository: this.guildRepository,
			userCacheService: this.userCacheService,
			gatewayService: this.gatewayService,
			discriminatorService: this.discriminatorService,
			paymentRepository: new PaymentRepository(),
		});
	}

	private async ensurePersonalNotesChannel(user: User): Promise<void> {
		const personalNotesChannelId = userIdToChannelId(user.id);
		const existingChannel = await this.channelRepository.findUnique(personalNotesChannelId);
		if (existingChannel) {
			if (existingChannel.type !== ChannelTypes.DM_PERSONAL_NOTES) {
				Logger.warn(
					{channelId: personalNotesChannelId.toString(), type: existingChannel.type},
					'Unexpected channel type already exists for personal notes channel',
				);
			}
			return;
		}
		await ensurePersonalNotesChannelExists({channelRepository: this.channelRepository, userId: user.id});
	}

	private async updateGuildMemberCount(guild: Guild, actualMemberCount: number): Promise<Guild> {
		if (guild.memberCount === actualMemberCount) {
			return guild;
		}
		return await this.guildRepository.upsertPartial(guild.id, {member_count: actualMemberCount}, guild.toRow());
	}

	private async migrateStickerAnimated(sticker: GuildSticker): Promise<GuildSticker> {
		if (sticker.animated !== null && sticker.animated !== undefined) {
			return sticker;
		}
		try {
			const animated = await this.avatarService.checkStickerAnimated(sticker.id);
			if (animated !== null) {
				const updatedSticker = await this.guildRepository.upsertSticker({
					guild_id: sticker.guildId,
					sticker_id: sticker.id,
					name: sticker.name,
					description: sticker.description,
					animated,
					nsfw: sticker.isNsfw,
					tags: sticker.tags,
					creator_id: sticker.creatorId,
					version: sticker.version,
				});
				Logger.debug({stickerId: sticker.id, animated}, 'Migrated sticker animated field');
				return updatedSticker;
			}
		} catch (error) {
			Logger.warn({stickerId: sticker.id, error}, 'Failed to migrate sticker animated field');
		}
		return sticker;
	}

	async handleRpcRequest({request, requestCache}: HandleRpcRequestParams): Promise<RpcResponse> {
		switch (request.type) {
			case 'session':
				return {
					type: 'session',
					data: await this.handleSessionRequest({
						token: request.token,
						version: request.version,
						requestCache,
						ip: request.ip,
						latitude: request.latitude,
						longitude: request.longitude,
					}),
				};
			case 'log_guild_crash': {
				Logger.error(
					{
						guildId: request.guild_id.toString(),
						stacktrace: request.stacktrace,
					},
					'Guild crash reported by gateway',
				);
				return {
					type: 'log_guild_crash',
					data: {success: true},
				};
			}
			case 'guild_collection':
				return {
					type: 'guild_collection',
					data: await this.handleGuildCollectionRequest({
						guildId: createGuildID(request.guild_id),
						collection: request.collection,
						requestCache,
						afterUserId: request.after_user_id ? createUserID(request.after_user_id) : undefined,
						limit: request.limit,
					}),
				};
			case 'voice_state_upsert':
				await this.persistGuildVoiceState({
					guildId: createGuildID(request.guild_id),
					voiceState: request.voice_state,
				});
				return {
					type: 'voice_state_upsert',
					data: {success: true},
				};
			case 'voice_state_remove':
				await this.removeGuildVoiceState({
					guildId: createGuildID(request.guild_id),
					connectionId: request.connection_id,
				});
				return {
					type: 'voice_state_remove',
					data: {success: true},
				};
			case 'get_user_guild_settings': {
				const result = await this.getUserGuildSettings({
					userIds: request.user_ids.map(createUserID),
					guildId: createGuildID(request.guild_id),
				});
				return {
					type: 'get_user_guild_settings',
					data: {
						user_guild_settings: result.user_guild_settings.map((settings) =>
							settings ? mapUserGuildSettingsToResponse(settings) : null,
						),
					},
				};
			}
			case 'get_push_subscriptions':
				return {
					type: 'get_push_subscriptions',
					data: await this.getPushSubscriptions({
						userIds: request.user_ids.map(createUserID),
					}),
				};
			case 'get_badge_counts': {
				const badgeCounts = await this.getBadgeCounts({
					userIds: request.user_ids.map(createUserID),
				});
				return {
					type: 'get_badge_counts',
					data: {
						badge_counts: badgeCounts,
					},
				};
			}
			case 'geoip_lookup': {
				const geoip = await lookupGeoip(request.ip);
				return {
					type: 'geoip_lookup',
					data: {
						country_code: geoip.countryCode ?? 'US',
						latitude: serializeCoordinate(geoip.latitude),
						longitude: serializeCoordinate(geoip.longitude),
					},
				};
			}
			case 'delete_push_subscriptions':
				return {
					type: 'delete_push_subscriptions',
					data: await this.deletePushSubscriptions({
						subscriptions: request.subscriptions.map((sub) => ({
							userId: createUserID(sub.user_id),
							subscriptionId: sub.subscription_id,
						})),
					}),
				};
			case 'send_apns_push': {
				const result = await sendApnsPush({
					userId: request.user_id.toString(),
					subscriptionId: request.subscription_id,
					deviceToken: request.device_token,
					appId: request.app_id,
					providerEnvironment: request.provider_environment,
					payload: request.payload,
				});
				return {
					type: 'send_apns_push',
					data: {
						success: result.success,
						should_delete: result.shouldDelete,
						reason: result.reason,
						status_code: result.statusCode,
					},
				};
			}
			case 'get_user_blocked_ids':
				return {
					type: 'get_user_blocked_ids',
					data: await this.getUserBlockedIds({
						userIds: request.user_ids.map(createUserID),
					}),
				};
			case 'voice_get_token': {
				Logger.debug(
					{type: 'voice_get_token', guildId: request.guild_id, channelId: request.channel_id, userId: request.user_id},
					'RPC voice_get_token received',
				);
				if (this.voiceService === null) {
					throw new Error('Voice is not enabled on this server');
				}
				const result = await this.voiceService.getVoiceToken({
					guildId: request.guild_id !== undefined ? createGuildID(request.guild_id) : undefined,
					channelId: createChannelID(request.channel_id),
					userId: createUserID(request.user_id),
					connectionId: request.connection_id,
					region: request.rtc_region,
					latitude: request.latitude,
					longitude: request.longitude,
					canSpeak: request.can_speak,
					canStream: request.can_stream,
					canVideo: request.can_video,
					tokenNonce: request.token_nonce,
				});
				return {
					type: 'voice_get_token',
					data: result,
				};
			}
			case 'voice_force_disconnect_participant': {
				if (this.voiceService === null) {
					throw new Error('Voice is not enabled on this server');
				}
				await this.voiceService.disconnectParticipant({
					guildId: request.guild_id !== undefined ? createGuildID(request.guild_id) : undefined,
					channelId: createChannelID(request.channel_id),
					userId: createUserID(request.user_id),
					connectionId: request.connection_id,
				});
				return {
					type: 'voice_force_disconnect_participant',
					data: {success: true},
				};
			}
			case 'voice_update_participant': {
				if (this.voiceService === null) {
					throw new Error('Voice is not enabled on this server');
				}
				await this.voiceService.updateParticipant({
					guildId: request.guild_id !== undefined ? createGuildID(request.guild_id) : undefined,
					channelId: createChannelID(request.channel_id),
					userId: createUserID(request.user_id),
					mute: request.mute,
					deaf: request.deaf,
					canSpeak: request.can_speak,
					canStream: request.can_stream,
					canVideo: request.can_video,
				});
				return {
					type: 'voice_update_participant',
					data: {success: true},
				};
			}
			case 'voice_force_disconnect_channel': {
				if (this.voiceService === null) {
					throw new Error('Voice is not enabled on this server');
				}
				const result = await this.voiceService.disconnectChannel({
					guildId: request.guild_id !== undefined ? createGuildID(request.guild_id) : undefined,
					channelId: createChannelID(request.channel_id),
				});
				return {
					type: 'voice_force_disconnect_channel',
					data: {
						success: result.success,
						disconnected_count: result.disconnectedCount,
					},
				};
			}
			case 'voice_list_participants': {
				if (this.voiceService === null) {
					return {
						type: 'voice_list_participants',
						data: {
							status: 'error',
							participants: [],
							error_code: 'voice_disabled',
							retryable: false,
						},
					};
				}
				const result = await this.voiceService.listParticipantsOnServer({
					guildId: request.guild_id !== undefined ? createGuildID(request.guild_id) : undefined,
					channelId: createChannelID(request.channel_id),
					regionId: request.region_id,
					serverId: request.server_id,
				});
				return {
					type: 'voice_list_participants',
					data: mapVoiceParticipantSnapshot(result),
				};
			}
			case 'voice_update_participant_permissions': {
				if (this.voiceService === null) {
					throw new Error('Voice is not enabled on this server');
				}
				await this.voiceService.updateParticipantPermissions({
					guildId: request.guild_id !== undefined ? createGuildID(request.guild_id) : undefined,
					channelId: createChannelID(request.channel_id),
					userId: createUserID(request.user_id),
					connectionId: request.connection_id,
					canSpeak: request.can_speak,
					canStream: request.can_stream,
					canVideo: request.can_video,
					deaf: request.deaf,
				});
				return {
					type: 'voice_update_participant_permissions',
					data: {success: true},
				};
			}
			case 'kick_temporary_member': {
				const success = await this.kickTemporaryMember({
					userId: createUserID(request.user_id),
					guildIds: request.guild_ids.map(createGuildID),
				});
				return {
					type: 'kick_temporary_member',
					data: {success},
				};
			}
			case 'call_ended': {
				await this.handleCallEnded({
					channelId: createChannelID(request.channel_id),
					messageId: createMessageID(request.message_id),
					participants: request.participants.map(createUserID),
					endedTimestamp: new Date(request.ended_timestamp),
					requestCache,
				});
				return {
					type: 'call_ended',
					data: {success: true},
				};
			}
			case 'validate_custom_status': {
				const userId = createUserID(request.user_id);
				const validatedCustomStatus =
					request.custom_status === null || request.custom_status === undefined
						? null
						: await this.customStatusValidator.validate(userId, request.custom_status);
				return {
					type: 'validate_custom_status',
					data: {
						custom_status: validatedCustomStatus
							? {
									text: validatedCustomStatus.text,
									expires_at: validatedCustomStatus.expiresAt?.toISOString() ?? null,
									emoji_id: validatedCustomStatus.emojiId?.toString() ?? null,
									emoji_name: validatedCustomStatus.emojiName,
									emoji_animated: validatedCustomStatus.emojiAnimated,
								}
							: null,
					},
				};
			}
			case 'get_dm_channel': {
				const channel = await this.getDmChannel({
					channelId: createChannelID(request.channel_id),
					userId: createUserID(request.user_id),
					requestCache,
				});
				return {
					type: 'get_dm_channel',
					data: {channel},
				};
			}
			case 'get_gateway_rollout_config': {
				const rolloutConfig = await this.instanceConfigRepository.getGatewayRolloutConfig();
				return {
					type: 'get_gateway_rollout_config',
					data: {config: rolloutConfig},
				};
			}
			default: {
				const exhaustiveCheck: never = request;
				throw new Error(
					`Unknown RPC request type: ${String(
						(
							exhaustiveCheck as {
								type?: string;
							}
						).type ?? 'unknown',
					)}`,
				);
			}
		}
	}

	private parseTokenType(token: string): 'user' | 'bot' | 'unknown' {
		if (token.startsWith('flx_')) {
			return 'user';
		}
		const dotIndex = token.indexOf('.');
		if (dotIndex > 0 && dotIndex < token.length - 1) {
			const beforeDot = token.slice(0, dotIndex);
			if (/^\d+$/.test(beforeDot)) {
				return 'bot';
			}
		}
		return 'unknown';
	}

	private normalizeSessionToken(token: string): string {
		if (token.startsWith('Bot ')) {
			return token.slice('Bot '.length);
		}
		return token;
	}

	private isUnknownUserError(error: unknown): error is UnknownUserError {
		return error instanceof UnknownUserError;
	}

	private async mapRpcSessionPrivateChannels(params: {
		channels: Array<Channel>;
		userId: UserID;
		requestCache: RequestCache;
	}): Promise<Array<ChannelResponse>> {
		const {channels, userId, requestCache} = params;
		const mappedChannels = await allSettledWithConcurrency(channels, RPC_RESPONSE_MAP_CONCURRENCY, (channel) =>
			mapChannelToResponse({
				channel,
				currentUserId: userId,
				userCacheService: this.userCacheService,
				requestCache,
			}),
		);
		const validChannels: Array<ChannelResponse> = [];
		for (const [index, mappedChannel] of mappedChannels.entries()) {
			if (mappedChannel.status === 'fulfilled') {
				validChannels.push(mappedChannel.value);
				continue;
			}
			const channel = channels[index];
			if (this.isUnknownUserError(mappedChannel.reason)) {
				Logger.warn(
					{
						userId: userId.toString(),
						channelId: channel?.id.toString(),
					},
					'Skipping RPC session private channel with unknown user reference',
				);
				continue;
			}
			throw mappedChannel.reason;
		}
		return validChannels;
	}

	private async mapRpcSessionRelationships(params: {
		relationships: Array<Relationship>;
		userId: UserID;
		requestCache: RequestCache;
	}): Promise<Array<RelationshipResponse>> {
		const {relationships, userId, requestCache} = params;
		const userPartialResolver = (targetUserId: UserID) =>
			getCachedUserPartialResponse({
				userId: targetUserId,
				userCacheService: this.userCacheService,
				requestCache,
			});
		const mappedRelationships = await allSettledWithConcurrency(
			relationships,
			RPC_RESPONSE_MAP_CONCURRENCY,
			(relationship) => mapRelationshipToResponse({relationship, userPartialResolver}),
		);
		const validRelationships: Array<RelationshipResponse> = [];
		for (const [index, mappedRelationship] of mappedRelationships.entries()) {
			if (mappedRelationship.status === 'fulfilled') {
				validRelationships.push(mappedRelationship.value);
				continue;
			}
			const relationship = relationships[index];
			if (this.isUnknownUserError(mappedRelationship.reason)) {
				Logger.warn(
					{
						userId: userId.toString(),
						targetUserId: relationship?.targetUserId.toString(),
						type: relationship?.type,
					},
					'Skipping RPC session relationship with unknown user reference',
				);
				continue;
			}
			throw mappedRelationship.reason;
		}
		return validRelationships;
	}

	private async preloadRpcSessionUserPartials(params: {
		channels: Array<Channel>;
		relationships: Array<Relationship>;
		userId: UserID;
		requestCache: RequestCache;
		steps?: RpcTimingSteps;
	}): Promise<void> {
		const {channels, relationships, userId, requestCache, steps} = params;
		const userIds = timeRpcStepSync(steps ?? {}, 'collect_user_partial_ids', () => {
			const collected = new Set<UserID>();
			for (const channel of channels) {
				if (
					channel.guildId != null ||
					channel.type === ChannelTypes.DM_PERSONAL_NOTES ||
					!channel.recipientIds ||
					channel.recipientIds.size === 0
				) {
					continue;
				}
				for (const recipientId of channel.recipientIds) {
					if (recipientId !== userId) {
						collected.add(recipientId);
					}
				}
			}
			for (const relationship of relationships) {
				collected.add(relationship.targetUserId);
			}
			return Array.from(collected);
		});
		if (userIds.length === 0) {
			return;
		}
		await this.userCacheService.getUserPartialResponses(userIds, requestCache, {
			timeStep: (name, operation) => timeRpcStep(steps ?? {}, name, operation),
		});
	}

	private async mapRpcGuildMembers(params: {
		guildId: GuildID;
		members: Array<GuildMember>;
		requestCache: RequestCache;
	}): Promise<Array<GuildMemberResponse>> {
		const {guildId, members, requestCache} = params;
		const mappedMembers = await allSettledWithConcurrency(members, RPC_RESPONSE_MAP_CONCURRENCY, (member) =>
			mapGuildMemberToResponse(member, this.userCacheService, requestCache),
		);
		const validMembers: Array<GuildMemberResponse> = [];
		for (const [index, mappedMember] of mappedMembers.entries()) {
			if (mappedMember.status === 'fulfilled') {
				validMembers.push(mappedMember.value);
				continue;
			}
			const member = members[index];
			if (this.isUnknownUserError(mappedMember.reason)) {
				Logger.warn(
					{
						guildId: guildId.toString(),
						userId: member?.userId.toString(),
					},
					'Skipping RPC guild member with unknown user reference',
				);
				continue;
			}
			throw mappedMember.reason;
		}
		return validMembers;
	}

	private queueStripePremiumStateReconciliation(user: User): void {
		if (user.isBot || user.premiumType === UserPremiumTypes.LIFETIME) {
			return;
		}
		if (!user.stripeSubscriptionId && !user.stripeCustomerId) {
			return;
		}
		const userIdString = user.id.toString();
		const cooldownKey = `${PREMIUM_RECONCILE_ENQUEUE_COOLDOWN_KEY_PREFIX}${userIdString}`;
		void this.kvClient
			.setnx(cooldownKey, new Date().toISOString(), PREMIUM_RECONCILE_ENQUEUE_COOLDOWN_SECONDS)
			.then(async (queueAllowed) => {
				if (!queueAllowed) {
					return;
				}
				await this.premiumStateReconciliationQueueService.enqueueUser(user.id);
			})
			.catch((error) => {
				Logger.warn(
					{userId: userIdString, error},
					'Failed to queue user for premium reconciliation during RPC session handling',
				);
			});
	}

	private queuePaymentReconciliation(user: User): void {
		if (user.isBot) {
			return;
		}
		if (!user.stripeCustomerId) {
			return;
		}
		const userIdString = user.id.toString();
		const cooldownKey = `${PAYMENT_RECONCILE_ENQUEUE_COOLDOWN_KEY_PREFIX}${userIdString}`;
		void this.kvClient
			.setnx(cooldownKey, new Date().toISOString(), PAYMENT_RECONCILE_ENQUEUE_COOLDOWN_SECONDS)
			.then(async (queueAllowed) => {
				if (!queueAllowed) {
					return;
				}
				await this.workerService.addJob('reconcileUserPayments', {userId: userIdString});
			})
			.catch((error) => {
				Logger.warn(
					{userId: userIdString, error},
					'Failed to queue user for payment reconciliation during RPC session handling',
				);
			});
	}

	private async handleSessionRequest({
		token,
		version,
		requestCache,
		ip,
		latitude,
		longitude,
	}: HandleSessionRequestParams): Promise<RpcResponseSessionData> {
		const timings = new RpcTimingRecorder();
		const normalizedToken = timings.timeSync('normalize_session_token', () => this.normalizeSessionToken(token));
		const tokenHash = timings.timeSync('hash_session_token', () =>
			createHash('sha256').update(normalizedToken).digest('hex'),
		);
		const tokenType = timings.timeSync('parse_token_type', () => this.parseTokenType(normalizedToken));
		const tokenHashPrefix = timings.timeSync('compute_token_hash_prefix', () => tokenHash.slice(0, 12));
		timings.timeSync('log_session_handling_started', () => {
			Logger.debug(
				{
					tokenType,
					tokenHashPrefix,
					version,
					hasIp: ip !== undefined,
					hasLatitude: latitude !== undefined,
					hasLongitude: longitude !== undefined,
				},
				'RPC session handling started',
			);
		});
		const bucketKey = timings.timeSync(
			'build_rate_limit_bucket_key',
			() => `gateway:rpc:session:${tokenType}:${tokenHash}`,
		);
		const rateLimitResult = await timings.time('check_session_rate_limit', async () =>
			this.rateLimitService.checkLimit({
				identifier: bucketKey,
				maxAttempts: 20,
				windowMs: ms('1 minute'),
			}),
		);
		if (!rateLimitResult.allowed) {
			Logger.warn(
				{
					tokenType,
					tokenHashPrefix,
					retryAfter: rateLimitResult.retryAfter,
					limit: rateLimitResult.limit,
					resetTime: rateLimitResult.resetTime,
				},
				'RPC session request rate limited',
			);
			throw new RateLimitError({
				retryAfter: rateLimitResult.retryAfter!,
				limit: rateLimitResult.limit,
				resetTime: rateLimitResult.resetTime,
			});
		}
		let userId: UserID | null = null;
		let authSession: AuthSessionModel | null = null;
		if (tokenType === 'user') {
			authSession = await timings.time('validate_user_session_token', async () =>
				AuthSession.getAuthSessionByToken(this.apiContext, normalizedToken),
			);
			if (authSession) {
				userId = authSession.userId;
			}
		} else if (tokenType === 'bot') {
			userId = await timings.time('validate_bot_token', async () =>
				this.botAuthService.validateBotToken(normalizedToken),
			);
		}
		if (!userId) {
			Logger.warn(
				{
					tokenType,
					tokenHashPrefix,
				},
				'RPC session token validation failed',
			);
			throw new UnauthorizedError();
		}
		const loadUserDataSteps: RpcTimingSteps = {};
		const loadUserDataStartedAtNs = startRpcTiming();
		const userData = await this.getUserData({userId, includePrivateChannels: true, timingSteps: loadUserDataSteps});
		timings.record('load_user_data', loadUserDataStartedAtNs, loadUserDataSteps);
		if (!userData || !userData.user) {
			Logger.warn(
				{
					tokenType,
					tokenHashPrefix,
					userId: userId.toString(),
				},
				'RPC session user lookup failed',
			);
			throw new UnauthorizedError();
		}
		let user = userData.user;
		if (user.avatarHash && user.avatarColor == null) {
			const avatarRepairSteps: RpcTimingSteps = {};
			const avatarRepairStartedAtNs = startRpcTiming();
			try {
				const avatarKey = timeRpcStepSync(
					avatarRepairSteps,
					'build_avatar_storage_key',
					() => `avatars/${user.id}/${this.stripAnimationPrefix(user.avatarHash!)}`,
				);
				const object = await timeRpcStep(avatarRepairSteps, 'read_avatar_object', async () =>
					this.storageService.readObject(Config.s3.buckets.cdn, avatarKey),
				);
				const avatarColor = await timeRpcStep(avatarRepairSteps, 'derive_avatar_color', async () =>
					deriveDominantAvatarColor(object),
				);
				const updatedUser = await timeRpcStep(avatarRepairSteps, 'persist_avatar_color', async () =>
					this.userRepository.patchUpsert(user.id, {avatar_color: avatarColor}, user.toRow()),
				);
				if (updatedUser) {
					user = updatedUser;
					this.userCacheService.setUserPartialResponseFromUserInBackground(updatedUser, requestCache);
				}
			} catch (error) {
				Logger.warn({userId: user.id, error}, 'Failed to repair user avatar color');
			} finally {
				timings.record('repair_avatar_color', avatarRepairStartedAtNs, avatarRepairSteps);
			}
		}
		if (user.bannerHash && user.bannerColor == null) {
			const bannerRepairSteps: RpcTimingSteps = {};
			const bannerRepairStartedAtNs = startRpcTiming();
			try {
				const bannerKey = timeRpcStepSync(
					bannerRepairSteps,
					'build_banner_storage_key',
					() => `banners/${user.id}/${this.stripAnimationPrefix(user.bannerHash!)}`,
				);
				const object = await timeRpcStep(bannerRepairSteps, 'read_banner_object', async () =>
					this.storageService.readObject(Config.s3.buckets.cdn, bannerKey),
				);
				const bannerColor = await timeRpcStep(bannerRepairSteps, 'derive_banner_color', async () =>
					deriveDominantAvatarColor(object),
				);
				const updatedUser = await timeRpcStep(bannerRepairSteps, 'persist_banner_color', async () =>
					this.userRepository.patchUpsert(user.id, {banner_color: bannerColor}, user.toRow()),
				);
				if (updatedUser) {
					user = updatedUser;
				}
			} catch (error) {
				Logger.warn({userId: user.id, error}, 'Failed to repair user banner color');
			} finally {
				timings.record('repair_banner_color', bannerRepairStartedAtNs, bannerRepairSteps);
			}
		}
		timings.timeSync('sync_repaired_user_data', () => {
			userData.user = user;
		});
		let countryCode = 'US';
		let geoipCountryIso: string | null = null;
		let geoipLatitude: string | undefined;
		let geoipLongitude: string | undefined;
		if (ip) {
			const geoip = await timings.time('lookup_geoip', async () => lookupGeoip(ip));
			timings.timeSync('map_geoip_result', () => {
				geoipCountryIso = geoip.countryCode ?? null;
				countryCode = geoip.countryCode ?? countryCode;
				geoipLatitude = serializeCoordinate(geoip.latitude);
				geoipLongitude = serializeCoordinate(geoip.longitude);
			});
		} else {
			timings.timeSync('log_missing_geoip_ip', () => {
				Logger.warn({context: 'rpc_geoip', reason: 'ip_missing'}, 'RPC session request missing IP for GeoIP');
			});
		}
		const sessionStartStartedAtNs = startRpcTiming();
		const sessionStartResult = await this.sessionStartService.processSessionStart({
			userData,
			requestCache,
			geoipCountryIso,
			clientIp: ip ?? null,
		});
		timings.record('process_session_start', sessionStartStartedAtNs, sessionStartResult.timings.steps);
		user = sessionStartResult.user;
		timings.timeSync('queue_stripe_premium_state_reconciliation', () =>
			this.queueStripePremiumStateReconciliation(user),
		);
		timings.timeSync('queue_payment_reconciliation', () => this.queuePaymentReconciliation(user));
		await timings.time('ensure_personal_notes_channel', async () => this.ensurePersonalNotesChannel(user));
		const privateChannelLimitSteps: RpcTimingSteps = {};
		const privateChannelLimitStartedAtNs = startRpcTiming();
		userData.privateChannels = await this.ensurePrivateChannelsWithinLimit({
			user,
			channels: userData.privateChannels,
			steps: privateChannelLimitSteps,
		});
		timings.record('ensure_private_channels_within_limit', privateChannelLimitStartedAtNs, privateChannelLimitSteps);
		const readyPayloadSteps: RpcTimingSteps = {};
		const readyPayloadStartedAtNs = startRpcTiming();
		const preloadUserPartialsSteps: RpcTimingSteps = {};
		const preloadUserPartialsStartedAtNs = startRpcTiming();
		await this.preloadRpcSessionUserPartials({
			channels: userData.privateChannels,
			relationships: userData.relationships,
			userId: user.id,
			requestCache,
			steps: preloadUserPartialsSteps,
		});
		readyPayloadSteps.preload_user_partials = createRpcTimingNode(
			preloadUserPartialsStartedAtNs,
			preloadUserPartialsSteps,
		);
		const [privateChannels, relationships, guilds] = await Promise.all([
			timeRpcStep(readyPayloadSteps, 'map_private_channels', async () =>
				this.mapRpcSessionPrivateChannels({
					channels: userData.privateChannels,
					userId: user.id,
					requestCache,
				}),
			),
			timeRpcStep(readyPayloadSteps, 'map_relationships', async () =>
				this.mapRpcSessionRelationships({
					relationships: userData.relationships,
					userId: user.id,
					requestCache,
				}),
			),
			timeRpcStepSync(readyPayloadSteps, 'map_guild_ids', () =>
				userData.guildIds.map((guildId) => ({id: guildId.toString()})),
			),
		]);
		timings.record('map_ready_payloads', readyPayloadStartedAtNs, readyPayloadSteps);
		const rtcRegions = timings.timeSync('build_rtc_regions', () => {
			const voiceAccessContext: VoiceAccessContext = {requestingUserId: user.id};
			const availableRegions =
				this.voiceAvailabilityService === null
					? []
					: this.voiceAvailabilityService.getAvailableRegions(voiceAccessContext);
			const accessibleRegions = availableRegions.filter((region) => region.isAccessible);
			const sortedRegions = accessibleRegions.slice();
			const userLatitude = parseCoordinate(latitude);
			const userLongitude = parseCoordinate(longitude);
			const hasLocation = userLatitude !== null && userLongitude !== null;
			if (hasLocation) {
				sortedRegions.sort((a, b) => {
					const distanceA = calculateDistance(userLatitude, userLongitude, a.latitude, a.longitude);
					const distanceB = calculateDistance(userLatitude, userLongitude, b.latitude, b.longitude);
					if (distanceA !== distanceB) {
						return distanceA - distanceB;
					}
					return a.id.localeCompare(b.id);
				});
			} else {
				sortedRegions.sort((a, b) => a.id.localeCompare(b.id));
			}
			return [
				{id: AUTOMATIC_VOICE_REGION_ID, name: 'Automatic', emoji: '🌐'},
				...sortedRegions.map((region) => ({
					id: region.id,
					name: region.name,
					emoji: region.emoji,
				})),
			];
		});
		timings.timeSync('log_session_handling_completed', () => {
			Logger.debug(
				{
					tokenType,
					tokenHashPrefix,
					userId: user.id.toString(),
					guildCount: guilds.length,
					privateChannelCount: privateChannels.length,
					relationshipCount: relationships.length,
					countryCode,
					rtcRegionCount: rtcRegions.length,
				},
				'RPC session handling completed',
			);
		});
		const responseBuildSteps: RpcTimingSteps = {};
		const responseBuildStartedAtNs = startRpcTiming();
		const responsePayload = {
			auth_session_id_hash: timeRpcStepSync(responseBuildSteps, 'map_auth_session_id_hash', () =>
				authSession ? uint8ArrayToBase64(authSession.sessionIdHash, {urlSafe: true}) : undefined,
			),
			user: timeRpcStepSync(responseBuildSteps, 'map_user', () => mapUserToPrivateResponse(user)),
			user_settings: timeRpcStepSync(responseBuildSteps, 'map_user_settings', () =>
				userData.settings
					? mapUserSettingsToResponse({
							settings: userData.settings,
						})
					: null,
			),
			user_guild_settings: timeRpcStepSync(responseBuildSteps, 'map_user_guild_settings', () =>
				userData.guildSettings.map(mapUserGuildSettingsToResponse),
			),
			notes: timeRpcStepSync(responseBuildSteps, 'map_notes', () =>
				Object.fromEntries(Array.from(userData.notes.entries()).map(([userId, note]) => [userId.toString(), note])),
			),
			read_states: timeRpcStepSync(responseBuildSteps, 'map_read_states', () =>
				userData.readStates.map(mapReadStateResponse),
			),
			read_state_proto: timeRpcStepSync(responseBuildSteps, 'encode_read_state_proto', () =>
				encodeReadStatesResponseProto(userData.readStates),
			),
			guilds,
			private_channels: privateChannels,
			relationships,
			favorite_memes: timeRpcStepSync(responseBuildSteps, 'map_favorite_memes', () =>
				userData.favoriteMemes.map(mapFavoriteMemeToResponse),
			),
			pinned_dms: timeRpcStepSync(responseBuildSteps, 'map_pinned_dms', () => userData.pinnedDMs?.map(String) ?? []),
			country_code: countryCode,
			latitude: geoipLatitude,
			longitude: geoipLongitude,
			rtc_regions: rtcRegions,
			webauthn_credentials: timeRpcStepSync(responseBuildSteps, 'map_webauthn_credentials', () =>
				userData.webAuthnCredentials.map((cred) => ({
					id: cred.credentialId,
					name: cred.name,
					created_at: cred.createdAt.toISOString(),
					last_used_at: cred.lastUsedAt?.toISOString() ?? null,
				})),
			),
			version,
		};
		timings.record('build_session_response_payload', responseBuildStartedAtNs, responseBuildSteps);
		return {
			_timings: timings.finalize(),
			...responsePayload,
		};
	}

	private async handleGuildCollectionRequest({
		guildId,
		collection,
		requestCache,
		afterUserId,
		limit,
	}: HandleGuildCollectionRequestParams): Promise<RpcResponseGuildCollectionData> {
		switch (collection) {
			case 'guild':
				return await this.handleGuildCollectionGuildRequest({guildId});
			case 'roles':
				return await this.handleGuildCollectionRolesRequest({guildId});
			case 'channels':
				return await this.handleGuildCollectionChannelsRequest({guildId, requestCache});
			case 'emojis':
				return await this.handleGuildCollectionEmojisRequest({guildId});
			case 'stickers':
				return await this.handleGuildCollectionStickersRequest({guildId});
			case 'members':
				return await this.handleGuildCollectionMembersRequest({guildId, requestCache, afterUserId, limit});
			case 'voice_states':
				return await this.handleGuildCollectionVoiceStatesRequest({guildId});
			default: {
				const exhaustiveCheck: never = collection;
				throw new Error(`Unknown guild collection: ${String(exhaustiveCheck)}`);
			}
		}
	}

	private createGuildCollectionResponse(collection: RpcGuildCollectionType): RpcResponseGuildCollectionData {
		return {
			collection,
			guild: undefined,
			roles: undefined,
			channels: undefined,
			emojis: undefined,
			stickers: undefined,
			members: undefined,
			voice_states: undefined,
			has_more: false,
			next_after_user_id: null,
		};
	}

	private async getGuildOrThrow(guildId: GuildID): Promise<Guild> {
		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) {
			throw new UnknownGuildError();
		}
		return guild;
	}

	private resolveGuildCollectionLimit(limit?: number): number {
		if (!limit || !Number.isInteger(limit) || limit < 1) {
			return GUILD_COLLECTION_DEFAULT_LIMIT;
		}
		return Math.min(limit, GUILD_COLLECTION_MAX_LIMIT);
	}

	private async handleGuildCollectionGuildRequest({
		guildId,
	}: {
		guildId: GuildID;
	}): Promise<RpcResponseGuildCollectionData> {
		const guild = await this.getGuildOrThrow(guildId);
		const memberCount = await this.guildRepository.countMembers(guildId);
		const repairedMemberCountGuild = await this.updateGuildMemberCount(guild, memberCount);
		const repairedBannerGuild = await this.repairGuildBannerHeight(repairedMemberCountGuild);
		const repairedSplashGuild = await this.repairGuildSplashDimensions(repairedBannerGuild);
		const repairedEmbedSplashGuild = await this.repairGuildEmbedSplashDimensions(repairedSplashGuild);
		const response = mapGuildToGuildResponse(repairedEmbedSplashGuild);
		return {
			...this.createGuildCollectionResponse('guild'),
			guild: response,
		};
	}

	private async handleGuildCollectionRolesRequest({
		guildId,
	}: {
		guildId: GuildID;
	}): Promise<RpcResponseGuildCollectionData> {
		await this.getGuildOrThrow(guildId);
		const roles = await this.guildRepository.listRoles(guildId);
		return {
			...this.createGuildCollectionResponse('roles'),
			roles: roles.map(mapGuildRoleToResponse),
		};
	}

	private async handleGuildCollectionChannelsRequest({
		guildId,
		requestCache,
	}: {
		guildId: GuildID;
		requestCache: RequestCache;
	}): Promise<RpcResponseGuildCollectionData> {
		const guild = await this.getGuildOrThrow(guildId);
		const channels = await this.channelRepository.listGuildChannels(guildId);
		const repairedGuild = await this.repairDanglingChannelReferences({guild, channels});
		this.repairOrphanedInvitesAndWebhooks({guild: repairedGuild, channels}).catch((error) => {
			Logger.warn({guildId: guildId.toString(), error}, 'Failed to repair orphaned invites/webhooks');
		});
		const mappedChannels = await Promise.all(
			channels.map((channel) =>
				mapChannelToResponse({
					channel,
					currentUserId: null,
					userCacheService: this.userCacheService,
					requestCache,
				}),
			),
		);
		return {
			...this.createGuildCollectionResponse('channels'),
			channels: mappedChannels,
		};
	}

	private async handleGuildCollectionEmojisRequest({
		guildId,
	}: {
		guildId: GuildID;
	}): Promise<RpcResponseGuildCollectionData> {
		await this.getGuildOrThrow(guildId);
		const emojis = await this.guildRepository.listEmojis(guildId);
		return {
			...this.createGuildCollectionResponse('emojis'),
			emojis: emojis.map(mapGuildEmojiToResponse),
		};
	}

	private async handleGuildCollectionStickersRequest({
		guildId,
	}: {
		guildId: GuildID;
	}): Promise<RpcResponseGuildCollectionData> {
		await this.getGuildOrThrow(guildId);
		const stickers = await this.guildRepository.listStickers(guildId);
		const migratedStickers = await this.migrateGuildStickersForRpc(guildId, stickers);
		return {
			...this.createGuildCollectionResponse('stickers'),
			stickers: migratedStickers.map(mapGuildStickerToResponse),
		};
	}

	private async handleGuildCollectionMembersRequest({
		guildId,
		requestCache,
		afterUserId,
		limit,
	}: {
		guildId: GuildID;
		requestCache: RequestCache;
		afterUserId?: UserID;
		limit?: number;
	}): Promise<RpcResponseGuildCollectionData> {
		await this.getGuildOrThrow(guildId);
		const chunkSize = this.resolveGuildCollectionLimit(limit);
		const members = await this.guildRepository.listMembersPaginated(guildId, chunkSize + 1, afterUserId);
		const hasMore = members.length > chunkSize;
		const pageMembers = hasMore ? members.slice(0, chunkSize) : members;
		const mappedMembers = await this.mapRpcGuildMembers({guildId, members: pageMembers, requestCache});
		let nextAfterUserId: string | null = null;
		if (hasMore) {
			const lastMember = pageMembers[pageMembers.length - 1];
			if (!lastMember) {
				throw new Error('Failed to build next member collection cursor');
			}
			nextAfterUserId = lastMember.userId.toString();
		}
		return {
			...this.createGuildCollectionResponse('members'),
			members: mappedMembers,
			has_more: hasMore,
			next_after_user_id: nextAfterUserId,
		};
	}

	private async handleGuildCollectionVoiceStatesRequest({
		guildId,
	}: {
		guildId: GuildID;
	}): Promise<RpcResponseGuildCollectionData> {
		await this.getGuildOrThrow(guildId);
		const persistedVoiceStates = await this.kvClient.hgetall(this.guildVoiceStatesKey(guildId));
		const voiceStates = this.parsePersistedGuildVoiceStates(guildId, persistedVoiceStates);
		return {
			...this.createGuildCollectionResponse('voice_states'),
			voice_states: voiceStates,
		};
	}

	private guildVoiceStatesKey(guildId: GuildID): string {
		return `${VOICE_STATE_KV_KEY_PREFIX}${guildId.toString()}`;
	}

	private isPersistedVoiceState(value: unknown): value is VoiceStateResponse {
		if (typeof value !== 'object' || value === null || Array.isArray(value)) {
			return false;
		}
		const candidate = value as Record<string, unknown>;
		const hasGuildId = typeof candidate.guild_id === 'string' || candidate.guild_id === null;
		const hasChannelId = typeof candidate.channel_id === 'string' || candidate.channel_id === null;
		return (
			hasGuildId &&
			hasChannelId &&
			typeof candidate.user_id === 'string' &&
			typeof candidate.mute === 'boolean' &&
			typeof candidate.deaf === 'boolean' &&
			typeof candidate.self_mute === 'boolean' &&
			typeof candidate.self_deaf === 'boolean'
		);
	}

	private parsePersistedGuildVoiceStates(
		guildId: GuildID,
		persistedVoiceStates: Record<string, string>,
	): Array<VoiceStateResponse> {
		const voiceStates: Array<VoiceStateResponse> = [];
		for (const [connectionId, serializedVoiceState] of Object.entries(persistedVoiceStates)) {
			try {
				const parsed = JSON.parse(serializedVoiceState);
				if (!this.isPersistedVoiceState(parsed)) {
					Logger.warn(
						{guildId: guildId.toString(), connectionId},
						'Dropping malformed persisted guild voice state payload',
					);
					continue;
				}
				voiceStates.push({
					...parsed,
					connection_id:
						typeof parsed.connection_id === 'string' && parsed.connection_id.length > 0
							? parsed.connection_id
							: connectionId,
					guild_id: guildId.toString(),
				});
			} catch (error) {
				Logger.warn(
					{guildId: guildId.toString(), connectionId, error},
					'Failed to decode persisted guild voice state payload',
				);
			}
		}
		return voiceStates;
	}

	private async persistGuildVoiceState(params: {guildId: GuildID; voiceState: VoiceStateResponse}): Promise<void> {
		const {guildId, voiceState} = params;
		const connectionId = voiceState.connection_id;
		if (!connectionId || connectionId.length === 0) {
			return;
		}
		const normalizedVoiceState: VoiceStateResponse = {
			...voiceState,
			guild_id: guildId.toString(),
			connection_id: connectionId,
		};
		await this.kvClient.hset(this.guildVoiceStatesKey(guildId), connectionId, JSON.stringify(normalizedVoiceState));
	}

	private async removeGuildVoiceState(params: {guildId: GuildID; connectionId: string}): Promise<void> {
		const {guildId, connectionId} = params;
		await this.kvClient.hdel(this.guildVoiceStatesKey(guildId), connectionId);
	}

	private async migrateGuildStickersForRpc(
		guildId: GuildID,
		stickers: Array<GuildSticker>,
	): Promise<Array<GuildSticker>> {
		const needsMigration = stickers.filter((sticker) => sticker.animated === null || sticker.animated === undefined);
		if (needsMigration.length === 0) {
			return stickers;
		}
		Logger.info({count: needsMigration.length, guildId}, 'Migrating sticker animated fields');
		const migrated = await Promise.all(needsMigration.map((sticker) => this.migrateStickerAnimated(sticker)));
		return stickers.map((sticker) => {
			const migratedSticker = migrated.find((candidate) => candidate.id === sticker.id);
			return migratedSticker ?? sticker;
		});
	}

	private async getUserData({
		userId,
		includePrivateChannels = true,
		timingSteps,
	}: GetUserDataParams): Promise<UserData | null> {
		const timeUserDataStep = <T>(name: string, operation: () => Promise<T>): Promise<T> =>
			timingSteps ? timeRpcStep(timingSteps, name, operation) : operation();
		const user = await timeUserDataStep('find_user', async () => this.userRepository.findUnique(userId));
		if (!user) return null;
		if (user.isBot) {
			const guildIds = await timeUserDataStep('get_user_guild_ids', async () =>
				this.userRepository.getUserGuildIds(userId),
			);
			return {
				user,
				settings: null,
				guildSettings: [],
				notes: new Map(),
				readStates: [],
				guildIds,
				privateChannels: [],
				relationships: [],
				favoriteMemes: [],
				pinnedDMs: [],
				webAuthnCredentials: [],
			};
		}
		const [
			settingsResult,
			notes,
			readStates,
			guildIds,
			relationships,
			favoriteMemes,
			pinnedDMs,
			webAuthnCredentials,
			guildSettings,
			privateChannels,
		] = await Promise.all([
			timeUserDataStep('find_settings', async () => this.userRepository.findSettings(userId)),
			timeUserDataStep('get_user_notes', async () => this.userRepository.getUserNotes(userId)),
			timeUserDataStep('get_read_states', async () => this.readStateService.getReadStates(userId)),
			timeUserDataStep('get_user_guild_ids', async () => this.userRepository.getUserGuildIds(userId)),
			timeUserDataStep('list_relationships', async () => this.userRepository.listRelationships(userId)),
			timeUserDataStep('list_favorite_memes', async () => this.favoriteMemeRepository.findByUserId(userId)),
			timeUserDataStep('get_pinned_dms', async () => this.userRepository.getPinnedDms(userId)),
			timeUserDataStep('list_webauthn_credentials', async () => this.userRepository.listWebAuthnCredentials(userId)),
			timeUserDataStep('find_all_guild_settings', async () => this.userRepository.findAllGuildSettings(userId)),
			includePrivateChannels
				? timeUserDataStep('list_private_channels', async () => this.userRepository.listPrivateChannels(userId))
				: Promise.resolve<Array<Channel>>([]),
		]);
		let settings = settingsResult;
		if (settings) {
			const needsIncomingCallRepair = settings.incomingCallFlags === 0;
			const needsGroupDmRepair = settings.groupDmAddPermissionFlags === 0;
			if (needsIncomingCallRepair || needsGroupDmRepair) {
				const isAdult = isUserAdult(user.dateOfBirth);
				const updatedRow = {
					...settings.toRow(),
					...(needsIncomingCallRepair && {
						incoming_call_flags: isAdult ? IncomingCallFlags.EVERYONE : IncomingCallFlags.FRIENDS_ONLY,
					}),
					...(needsGroupDmRepair && {group_dm_add_permission_flags: GroupDmAddPermissionFlags.FRIENDS_ONLY}),
				};
				await timeUserDataStep('repair_user_settings_defaults', async () =>
					this.userRepository.upsertSettings(updatedRow),
				);
				settings = new UserSettings(updatedRow);
			}
		}
		return {
			user,
			settings,
			guildSettings,
			notes,
			readStates,
			guildIds,
			privateChannels,
			relationships,
			favoriteMemes,
			pinnedDMs,
			webAuthnCredentials,
		};
	}

	private async ensurePrivateChannelsWithinLimit(params: {
		user: User;
		channels: Array<Channel>;
		steps?: RpcTimingSteps;
	}): Promise<Array<Channel>> {
		const {user, channels, steps} = params;
		if (user.isBot) {
			return [];
		}
		const totalPrivateChannels = channels.length;
		const limit = timeRpcStepSync(steps ?? {}, 'resolve_private_channel_limit', () =>
			this.resolveLimitForUser(user, 'max_private_channels_per_user', MAX_PRIVATE_CHANNELS_PER_USER),
		);
		if (totalPrivateChannels <= limit) {
			return channels;
		}
		const pinnedDmIds = new Set(
			await timeRpcStep(steps ?? {}, 'get_pinned_dms_for_limit', async () => this.userRepository.getPinnedDms(user.id)),
		);
		const toClose = totalPrivateChannels - limit;
		const channelsToClose = timeRpcStepSync(steps ?? {}, 'select_private_channels_to_close', () =>
			channels
				.filter((channel) => channel.type === ChannelTypes.DM && !pinnedDmIds.has(channel.id))
				.sort((a, b) => {
					const aValue = a.lastMessageId ?? 0n;
					const bValue = b.lastMessageId ?? 0n;
					if (aValue < bValue) return -1;
					if (aValue > bValue) return 1;
					return 0;
				})
				.slice(0, toClose),
		);
		await timeRpcStep(steps ?? {}, 'close_private_channels_over_limit', async () => {
			await Promise.all(channelsToClose.map((channel) => this.userRepository.closeDmForUser(user.id, channel.id)));
		});
		if (channelsToClose.length < toClose) {
			Logger.warn(
				{
					user_id: user.id.toString(),
					total_private_channels: totalPrivateChannels,
					required_closures: toClose,
					actual_closures: channelsToClose.length,
				},
				'Unable to close enough DMs to satisfy private channel limit',
			);
		}
		if (channelsToClose.length === 0) {
			return channels;
		}
		const closedChannelIds = new Set(channelsToClose.map((channel) => channel.id));
		return channels.filter((channel) => !closedChannelIds.has(channel.id));
	}

	private resolveLimitForUser(user: User | null, key: LimitKey, fallback: number): number {
		const ctx = createLimitMatchContext({user});
		return resolveLimitSafe(this.limitConfigService.getConfigSnapshot(), ctx, key, fallback);
	}

	private async getUserGuildSettings(params: {userIds: Array<UserID>; guildId: GuildID}): Promise<{
		user_guild_settings: Array<UserGuildSettings | null>;
	}> {
		const {userIds, guildId} = params;
		const actualGuildId = guildId === createGuildID(0n) ? null : guildId;
		const userGuildSettings = await Promise.all(
			userIds.map((userId) => this.userRepository.findGuildSettings(userId, actualGuildId)),
		);
		return {user_guild_settings: userGuildSettings};
	}

	private async getPushSubscriptions(params: {userIds: Array<UserID>}): Promise<
		Record<
			string,
			Array<{
				subscription_id: string;
				endpoint: string;
				p256dh_key: string | null;
				auth_key: string | null;
				platform: string;
				app_id: string | null;
				provider_environment: string | null;
			}>
		>
	> {
		const {userIds} = params;
		const subscriptionsMap = await this.userRepository.getBulkPushSubscriptions(userIds);
		const result: Record<
			string,
			Array<{
				subscription_id: string;
				endpoint: string;
				p256dh_key: string | null;
				auth_key: string | null;
				platform: string;
				app_id: string | null;
				provider_environment: string | null;
			}>
		> = {};
		for (const [userId, subscriptions] of subscriptionsMap.entries()) {
			result[userId.toString()] = subscriptions.map((sub) => ({
				subscription_id: sub.subscriptionId,
				endpoint: sub.endpoint,
				p256dh_key: sub.p256dhKey,
				auth_key: sub.authKey,
				platform: sub.platform,
				app_id: sub.appId,
				provider_environment: sub.providerEnvironment,
			}));
		}
		return result;
	}

	private async getBadgeCounts(params: {userIds: Array<UserID>}): Promise<Record<string, number>> {
		const {userIds} = params;
		const uniqueUserIds = Array.from(new Set(userIds)) as Array<UserID>;
		const badgeCounts: Record<string, number> = {};
		await Promise.all(
			uniqueUserIds.map(async (userId) => {
				const readStates = await this.readStateService.getReadStates(userId);
				const totalMentions = readStates.reduce((sum, state) => sum + state.mentionCount, 0);
				badgeCounts[userId.toString()] = totalMentions;
			}),
		);
		return badgeCounts;
	}

	private async deletePushSubscriptions(params: {
		subscriptions: Array<{
			userId: UserID;
			subscriptionId: string;
		}>;
	}): Promise<{
		success: boolean;
	}> {
		const {subscriptions} = params;
		await Promise.all(
			subscriptions.map((sub) => this.userRepository.deletePushSubscription(sub.userId, sub.subscriptionId)),
		);
		return {success: true};
	}

	private stripAnimationPrefix(hash: string): string {
		return hash.startsWith('a_') ? hash.slice(2) : hash;
	}

	private async repairDanglingChannelReferences(params: {guild: Guild; channels: Array<Channel>}): Promise<Guild> {
		const {guild, channels} = params;
		const channelIds = new Set(channels.map((channel) => channel.id));
		const danglingSystemChannel = guild.systemChannelId != null && !channelIds.has(guild.systemChannelId);
		const danglingRulesChannel = guild.rulesChannelId != null && !channelIds.has(guild.rulesChannelId);
		const danglingAfkChannel = guild.afkChannelId != null && !channelIds.has(guild.afkChannelId);
		if (!danglingSystemChannel && !danglingRulesChannel && !danglingAfkChannel) {
			return guild;
		}
		Logger.info(
			{
				guildId: guild.id.toString(),
				danglingSystemChannel,
				danglingRulesChannel,
				danglingAfkChannel,
			},
			'Repairing dangling guild channel references',
		);
		const patch: {
			system_channel_id?: null;
			rules_channel_id?: null;
			afk_channel_id?: null;
		} = {};
		if (danglingSystemChannel) patch.system_channel_id = null;
		if (danglingRulesChannel) patch.rules_channel_id = null;
		if (danglingAfkChannel) patch.afk_channel_id = null;
		return this.guildRepository.upsertPartial(guild.id, patch, guild.toRow());
	}

	private async repairGuildBannerHeight(guild: Guild): Promise<Guild> {
		if (!guild.bannerHash || (guild.bannerHeight != null && guild.bannerWidth != null)) {
			return guild;
		}
		const s3Key = `banners/${guild.id}/${this.stripAnimationPrefix(guild.bannerHash)}`;
		try {
			const object = await this.storageService.readObject(Config.s3.buckets.cdn, s3Key);
			const metadata = await sharp(object).metadata();
			const bannerHeight = metadata.height ?? null;
			const bannerWidth = metadata.width ?? null;
			if (bannerHeight == null || bannerWidth == null) {
				return guild;
			}
			const repairedGuild = await this.guildRepository.upsertPartial(
				guild.id,
				{banner_height: bannerHeight, banner_width: bannerWidth},
				guild.toRow(),
			);
			return repairedGuild;
		} catch (error) {
			Logger.warn({guildId: guild.id, error}, 'Failed to repair guild banner height');
			return guild;
		}
	}

	private async repairGuildSplashDimensions(guild: Guild): Promise<Guild> {
		if (!guild.splashHash || (guild.splashWidth != null && guild.splashHeight != null)) {
			return guild;
		}
		const s3Key = `splashes/${guild.id}/${this.stripAnimationPrefix(guild.splashHash)}`;
		try {
			const object = await this.storageService.readObject(Config.s3.buckets.cdn, s3Key);
			const metadata = await sharp(object).metadata();
			const splashHeight = metadata.height ?? null;
			const splashWidth = metadata.width ?? null;
			if (splashHeight == null || splashWidth == null) {
				return guild;
			}
			const repairedGuild = await this.guildRepository.upsertPartial(
				guild.id,
				{splash_height: splashHeight, splash_width: splashWidth},
				guild.toRow(),
			);
			return repairedGuild;
		} catch (error) {
			Logger.warn({guildId: guild.id, error}, 'Failed to repair guild splash dimensions');
			return guild;
		}
	}

	private async repairGuildEmbedSplashDimensions(guild: Guild): Promise<Guild> {
		if (!guild.embedSplashHash || (guild.embedSplashWidth != null && guild.embedSplashHeight != null)) {
			return guild;
		}
		const s3Key = `embed-splashes/${guild.id}/${this.stripAnimationPrefix(guild.embedSplashHash)}`;
		try {
			const object = await this.storageService.readObject(Config.s3.buckets.cdn, s3Key);
			const metadata = await sharp(object).metadata();
			const embedSplashHeight = metadata.height ?? null;
			const embedSplashWidth = metadata.width ?? null;
			if (embedSplashHeight == null || embedSplashWidth == null) {
				return guild;
			}
			const repairedGuild = await this.guildRepository.upsertPartial(
				guild.id,
				{embed_splash_height: embedSplashHeight, embed_splash_width: embedSplashWidth},
				guild.toRow(),
			);
			return repairedGuild;
		} catch (error) {
			Logger.warn({guildId: guild.id, error}, 'Failed to repair guild embed splash dimensions');
			return guild;
		}
	}

	private async repairOrphanedInvitesAndWebhooks(params: {guild: Guild; channels: Array<Channel>}): Promise<void> {
		const {guild, channels} = params;
		const channelIds = new Set(channels.map((channel) => channel.id));
		const vanityInviteCode = guild.vanityUrlCode ? vanityCodeToInviteCode(guild.vanityUrlCode) : null;
		const [invites, webhooks] = await Promise.all([
			this.inviteRepository.listGuildInvites(guild.id),
			this.webhookRepository.listByGuild(guild.id),
		]);
		const orphanedInvites = invites.filter((invite) => {
			if (!invite.channelId) {
				return false;
			}
			if (vanityInviteCode && invite.code === vanityInviteCode) {
				return false;
			}
			return !channelIds.has(invite.channelId);
		});
		const orphanedWebhooks = webhooks.filter((webhook) => {
			if (!webhook.channelId) {
				return false;
			}
			return !channelIds.has(webhook.channelId);
		});
		if (orphanedInvites.length > 0) {
			Logger.info(
				{
					guildId: guild.id.toString(),
					count: orphanedInvites.length,
					codes: orphanedInvites.map((i) => i.code),
				},
				'Repairing orphaned invites',
			);
			await Promise.all(orphanedInvites.map((invite) => this.inviteRepository.delete(invite.code)));
		}
		if (orphanedWebhooks.length > 0) {
			Logger.info(
				{
					guildId: guild.id.toString(),
					count: orphanedWebhooks.length,
					webhookIds: orphanedWebhooks.map((w) => w.id.toString()),
				},
				'Repairing orphaned webhooks',
			);
			await Promise.all(orphanedWebhooks.map((webhook) => this.webhookRepository.delete(webhook.id)));
		}
	}

	private async getUserBlockedIds(params: {userIds: Array<UserID>}): Promise<Record<string, Array<string>>> {
		const {userIds} = params;
		const result: Record<string, Array<string>> = {};
		const relationshipsPromises = userIds.map(async (userId) => {
			const relationships = await this.userRepository.listRelationships(userId);
			const blockedIds = relationships.filter((rel) => rel.type === 2).map((rel) => rel.targetUserId.toString());
			return {userId, blockedIds};
		});
		const results = await Promise.all(relationshipsPromises);
		for (const {userId, blockedIds} of results) {
			result[userId.toString()] = blockedIds;
		}
		return result;
	}

	private async kickTemporaryMember(params: {userId: UserID; guildIds: Array<GuildID>}): Promise<boolean> {
		const {userId, guildIds} = params;
		try {
			await Promise.all(
				guildIds.map(async (guildId) => {
					try {
						const [member, guild] = await Promise.all([
							this.guildRepository.getMember(guildId, userId),
							this.guildRepository.findUnique(guildId),
						]);
						if (member?.isTemporary && guild) {
							await this.guildRepository.deleteMember(guildId, userId);
							await this.updateGuildMemberCount(guild, Math.max(0, guild.memberCount - 1));
							await this.gatewayService.dispatchGuild({
								guildId,
								event: 'GUILD_MEMBER_REMOVE',
								data: {user: {id: userId.toString()}},
							});
							await this.gatewayService.leaveGuild({userId, guildId});
						}
					} catch (error) {
						Logger.error(
							{userId: userId.toString(), guildId: guildId.toString(), error},
							'Failed to kick temporary member from guild',
						);
						throw error;
					}
				}),
			);
			return true;
		} catch (error) {
			Logger.error(
				{userId: userId.toString(), guildIds: guildIds.map(String), error},
				'Failed to kick temporary member from multiple guilds',
			);
			return false;
		}
	}

	private async handleCallEnded(params: {
		channelId: ChannelID;
		messageId: bigint;
		participants: Array<UserID>;
		endedTimestamp: Date;
		requestCache: RequestCache;
	}): Promise<void> {
		const {channelId, messageId, participants, endedTimestamp} = params;
		const [message, channel] = await Promise.all([
			this.channelRepository.getMessage(channelId, createMessageID(messageId)),
			this.channelRepository.findUnique(channelId),
		]);
		if (!message || !channel) {
			return;
		}
		if (message.type !== MessageTypes.CALL) {
			return;
		}
		const messageRow = message.toRow();
		const updatedMessage = await this.channelRepository.upsertMessage({
			...messageRow,
			call: {
				participant_ids: new Set(participants),
				ended_timestamp: endedTimestamp,
			},
		});
		if (!updatedMessage) {
			return;
		}
		const participantIds = participants.filter((id) => channel.recipientIds.has(id));
		if (participantIds.length > 0) {
			await Promise.all(
				participantIds.map((participantId) =>
					this.readStateService
						.ackMessage({
							userId: participantId,
							channelId,
							messageId: createMessageID(messageId),
							mentionCount: 0,
							silent: true,
						})
						.catch((error) => {
							Logger.error(
								{
									userId: participantId.toString(),
									channelId: channelId.toString(),
									messageId: messageId.toString(),
									error,
								},
								'Failed to ack call message for participant on call end',
							);
							return null;
						}),
				),
			);
		}
		const messageResponse = await buildBroadcastMessageData({
			channel,
			message: updatedMessage,
		});
		for (const recipientId of channel.recipientIds) {
			await this.gatewayService.dispatchPresence({
				userId: recipientId,
				event: 'MESSAGE_UPDATE',
				data: messageResponse,
			});
		}
	}

	private async getDmChannel(params: {
		channelId: ChannelID;
		userId: UserID;
		requestCache: RequestCache;
	}): Promise<ChannelResponse | null> {
		const {channelId, userId, requestCache} = params;
		const channel = await this.channelRepository.findUnique(channelId);
		if (!channel) {
			return null;
		}
		if (!channel.recipientIds.has(userId)) {
			return null;
		}
		try {
			return await mapChannelToResponse({
				channel,
				currentUserId: userId,
				userCacheService: this.userCacheService,
				requestCache,
			});
		} catch (error) {
			if (this.isUnknownUserError(error)) {
				Logger.warn(
					{
						userId: userId.toString(),
						channelId: channelId.toString(),
					},
					'Skipping RPC get_dm_channel response with unknown user reference',
				);
				return null;
			}
			throw error;
		}
	}
}

async function allSettledWithConcurrency<T, TResult>(
	items: ReadonlyArray<T>,
	concurrency: number,
	mapper: (item: T, index: number) => Promise<TResult>,
): Promise<Array<PromiseSettledResult<TResult>>> {
	const results = new Array<PromiseSettledResult<TResult>>(items.length);
	let nextIndex = 0;
	async function worker(): Promise<void> {
		for (;;) {
			const index = nextIndex++;
			if (index >= items.length) return;
			try {
				results[index] = {
					status: 'fulfilled',
					value: await mapper(items[index]!, index),
				};
			} catch (reason) {
				results[index] = {
					status: 'rejected',
					reason,
				};
			}
		}
	}
	await Promise.all(Array.from({length: Math.min(concurrency, items.length)}, () => worker()));
	return results;
}
