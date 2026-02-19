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

import {createHash} from 'node:crypto';
import type {AlertService} from '@fluxer/api/src/alert/AlertService';
import type {AuthService} from '@fluxer/api/src/auth/AuthService';
import type {ChannelID, GuildID, UserID} from '@fluxer/api/src/BrandedTypes';
import {
	createChannelID,
	createGuildID,
	createMessageID,
	createUserID,
	userIdToChannelId,
	vanityCodeToInviteCode,
} from '@fluxer/api/src/BrandedTypes';
import {Config} from '@fluxer/api/src/Config';
import {mapChannelToResponse} from '@fluxer/api/src/channel/ChannelMappers';
import type {IChannelRepository} from '@fluxer/api/src/channel/IChannelRepository';
import {mapMessageToResponse} from '@fluxer/api/src/channel/MessageMappers';
import {mapFavoriteMemeToResponse} from '@fluxer/api/src/favorite_meme/FavoriteMemeModel';
import type {IFavoriteMemeRepository} from '@fluxer/api/src/favorite_meme/IFavoriteMemeRepository';
import {
	mapGuildEmojiToResponse,
	mapGuildMemberToResponse,
	mapGuildRoleToResponse,
	mapGuildStickerToResponse,
	mapGuildToGuildResponse,
} from '@fluxer/api/src/guild/GuildModel';
import type {IGuildRepositoryAggregate} from '@fluxer/api/src/guild/repositories/IGuildRepositoryAggregate';
import type {AvatarService} from '@fluxer/api/src/infrastructure/AvatarService';
import type {IDiscriminatorService} from '@fluxer/api/src/infrastructure/DiscriminatorService';
import type {IGatewayService} from '@fluxer/api/src/infrastructure/IGatewayService';
import type {IMediaService} from '@fluxer/api/src/infrastructure/IMediaService';
import type {IStorageService} from '@fluxer/api/src/infrastructure/IStorageService';
import type {UserCacheService} from '@fluxer/api/src/infrastructure/UserCacheService';
import type {IInviteRepository} from '@fluxer/api/src/invite/IInviteRepository';
import {Logger} from '@fluxer/api/src/Logger';
import type {LimitConfigService} from '@fluxer/api/src/limits/LimitConfigService';
import {resolveLimitSafe} from '@fluxer/api/src/limits/LimitConfigUtils';
import {createLimitMatchContext} from '@fluxer/api/src/limits/LimitMatchContextBuilder';
import type {RequestCache} from '@fluxer/api/src/middleware/RequestCacheMiddleware';
import type {AuthSession} from '@fluxer/api/src/models/AuthSession';
import type {Channel} from '@fluxer/api/src/models/Channel';
import type {FavoriteMeme} from '@fluxer/api/src/models/FavoriteMeme';
import type {Guild} from '@fluxer/api/src/models/Guild';
import type {GuildMember} from '@fluxer/api/src/models/GuildMember';
import type {GuildSticker} from '@fluxer/api/src/models/GuildSticker';
import type {ReadState} from '@fluxer/api/src/models/ReadState';
import type {Relationship} from '@fluxer/api/src/models/Relationship';
import type {User} from '@fluxer/api/src/models/User';
import type {UserGuildSettings} from '@fluxer/api/src/models/UserGuildSettings';
import {UserSettings} from '@fluxer/api/src/models/UserSettings';
import type {BotAuthService} from '@fluxer/api/src/oauth/BotAuthService';
import type {IApplicationRepository} from '@fluxer/api/src/oauth/repositories/IApplicationRepository';
import type {PackService} from '@fluxer/api/src/pack/PackService';
import type {ReadStateService} from '@fluxer/api/src/read_state/ReadStateService';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import {CustomStatusValidator} from '@fluxer/api/src/user/services/CustomStatusValidator';
import {getCachedUserPartialResponse} from '@fluxer/api/src/user/UserCacheHelpers';
import {createPremiumClearPatch, shouldStripExpiredPremium} from '@fluxer/api/src/user/UserHelpers';
import {
	mapRelationshipToResponse,
	mapUserGuildSettingsToResponse,
	mapUserSettingsToResponse,
	mapUserToPrivateResponse,
} from '@fluxer/api/src/user/UserMappers';
import {isUserAdult} from '@fluxer/api/src/utils/AgeUtils';
import {deriveDominantAvatarColor} from '@fluxer/api/src/utils/AvatarColorUtils';
import {calculateDistance, parseCoordinate} from '@fluxer/api/src/utils/GeoUtils';
import {lookupGeoip} from '@fluxer/api/src/utils/IpUtils';
import type {VoiceAccessContext, VoiceAvailabilityService} from '@fluxer/api/src/voice/VoiceAvailabilityService';
import type {VoiceService} from '@fluxer/api/src/voice/VoiceService';
import type {IWebhookRepository} from '@fluxer/api/src/webhook/IWebhookRepository';
import {ChannelTypes, MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {MAX_PRIVATE_CHANNELS_PER_USER} from '@fluxer/constants/src/LimitConstants';
import {
	GroupDmAddPermissionFlags,
	IncomingCallFlags,
	UserFlags,
	UserPremiumTypes,
} from '@fluxer/constants/src/UserConstants';
import {RateLimitError} from '@fluxer/errors/src/domains/core/RateLimitError';
import {UnauthorizedError} from '@fluxer/errors/src/domains/core/UnauthorizedError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {IRateLimitService} from '@fluxer/rate_limit/src/IRateLimitService';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {
	RpcGuildCollectionType,
	RpcRequest,
	RpcResponse,
	RpcResponseGuildCollectionData,
	RpcResponseSessionData,
} from '@fluxer/schema/src/domains/rpc/RpcSchemas';
import type {RelationshipResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {ms} from 'itty-time';
import sharp from 'sharp';
import {uint8ArrayToBase64} from 'uint8array-extras';

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
}

const GUILD_COLLECTION_DEFAULT_LIMIT = 250;
const GUILD_COLLECTION_MAX_LIMIT = 1000;

export class RpcService {
	private readonly customStatusValidator: CustomStatusValidator;

	constructor(
		private userRepository: IUserRepository,
		private applicationRepository: IApplicationRepository,
		private guildRepository: IGuildRepositoryAggregate,
		private channelRepository: IChannelRepository,
		private userCacheService: UserCacheService,
		private readStateService: ReadStateService,
		private authService: AuthService,
		private gatewayService: IGatewayService,
		private alertService: AlertService,
		private discriminatorService: IDiscriminatorService,
		private favoriteMemeRepository: IFavoriteMemeRepository,
		private packService: PackService,
		private botAuthService: BotAuthService,
		private inviteRepository: IInviteRepository,
		private webhookRepository: IWebhookRepository,
		private storageService: IStorageService,
		private avatarService: AvatarService,
		private rateLimitService: IRateLimitService,
		private mediaService: IMediaService,
		private readonly limitConfigService: LimitConfigService,
		private voiceService?: VoiceService,
		private voiceAvailabilityService?: VoiceAvailabilityService,
	) {
		this.customStatusValidator = new CustomStatusValidator(
			this.userRepository,
			this.guildRepository,
			this.packService,
			this.limitConfigService,
		);
	}

	private async sanitizeOwnedBotDiscriminators(owner: User): Promise<boolean> {
		// Only reroll bot discriminators that are invalid (#0000). This is a one-time migration guarded by UserFlags.BOT_SANITIZED.
		const applications = await this.applicationRepository.listApplicationsByOwner(owner.id);
		const botUserIds: Array<UserID> = [];

		for (const app of applications) {
			if (!app.hasBotUser()) continue;
			const botUserId = app.getBotUserId();
			if (botUserId) {
				botUserIds.push(botUserId);
			}
		}

		const uniqueBotUserIds = Array.from(new Set(botUserIds));
		let allSucceeded = true;
		let rerolledCount = 0;

		for (const botUserId of uniqueBotUserIds) {
			const botUser = await this.userRepository.findUnique(botUserId);
			if (!botUser) {
				Logger.warn(
					{ownerUserId: owner.id.toString(), botUserId: botUserId.toString()},
					'Owned bot user missing during discriminator sanitization',
				);
				allSucceeded = false;
				continue;
			}

			if (!botUser.isBot) {
				Logger.warn(
					{ownerUserId: owner.id.toString(), botUserId: botUserId.toString()},
					'Owned bot user record is not marked as bot during discriminator sanitization',
				);
				allSucceeded = false;
				continue;
			}

			const isDeleted = (botUser.flags & UserFlags.DELETED) === UserFlags.DELETED;
			if (isDeleted) {
				continue;
			}

			if (botUser.discriminator !== 0) {
				continue;
			}

			try {
				const discriminatorResult = await this.discriminatorService.generateDiscriminator({
					username: botUser.username,
				});

				if (
					!discriminatorResult.available ||
					discriminatorResult.discriminator === -1 ||
					discriminatorResult.discriminator === 0
				) {
					Logger.warn(
						{
							ownerUserId: owner.id.toString(),
							botUserId: botUserId.toString(),
							username: botUser.username,
							discriminatorResult,
						},
						'Failed to reroll invalid bot discriminator during sanitization',
					);
					allSucceeded = false;
					continue;
				}

				const updatedBotUser = await this.userRepository.patchUpsert(
					botUserId,
					{
						discriminator: discriminatorResult.discriminator,
					},
					botUser.toRow(),
				);
				await this.userCacheService.setUserPartialResponseFromUser(updatedBotUser);
				rerolledCount += 1;
			} catch (error) {
				Logger.warn(
					{ownerUserId: owner.id.toString(), botUserId: botUserId.toString(), error},
					'Failed to sanitize owned bot discriminator',
				);
				allSucceeded = false;
			}
		}

		Logger.info(
			{
				ownerUserId: owner.id.toString(),
				botCount: uniqueBotUserIds.length,
				rerolledCount,
				allSucceeded,
			},
			'Completed owned bot discriminator sanitization pass',
		);

		return allSucceeded;
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

		await this.channelRepository.upsert({
			channel_id: personalNotesChannelId,
			guild_id: null,
			type: ChannelTypes.DM_PERSONAL_NOTES,
			name: '',
			topic: null,
			icon_hash: null,
			url: null,
			parent_id: null,
			position: 0,
			owner_id: null,
			recipient_ids: new Set(),
			nsfw: false,
			rate_limit_per_user: 0,
			bitrate: null,
			user_limit: null,
			rtc_region: null,
			last_message_id: null,
			last_pin_timestamp: null,
			permission_overwrites: null,
			nicks: null,
			soft_deleted: false,
			indexed_at: null,
			version: 1,
		});
	}

	private async updateGuildMemberCount(guild: Guild, actualMemberCount: number): Promise<Guild> {
		if (guild.memberCount === actualMemberCount) {
			return guild;
		}
		const guildRow = guild.toRow();
		return await this.guildRepository.upsert({
			...guildRow,
			member_count: actualMemberCount,
		});
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
				await this.alertService.logGuildCrash({
					guildId: request.guild_id.toString(),
					stacktrace: request.stacktrace,
				});
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
				if (!this.voiceService) {
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
				if (!this.voiceService) {
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
				if (!this.voiceService) {
					throw new Error('Voice is not enabled on this server');
				}
				await this.voiceService.updateParticipant({
					guildId: request.guild_id !== undefined ? createGuildID(request.guild_id) : undefined,
					channelId: createChannelID(request.channel_id),
					userId: createUserID(request.user_id),
					mute: request.mute,
					deaf: request.deaf,
				});
				return {
					type: 'voice_update_participant',
					data: {success: true},
				};
			}
			case 'voice_force_disconnect_channel': {
				if (!this.voiceService) {
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
						message: result.message,
					},
				};
			}
			case 'voice_update_participant_permissions': {
				if (!this.voiceService) {
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
			default: {
				const exhaustiveCheck: never = request;
				throw new Error(`Unknown RPC request type: ${String((exhaustiveCheck as {type?: string}).type ?? 'unknown')}`);
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
		const mappedChannels = await Promise.allSettled(
			channels.map((channel) =>
				mapChannelToResponse({
					channel,
					currentUserId: userId,
					userCacheService: this.userCacheService,
					requestCache,
				}),
			),
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
		const mappedRelationships = await Promise.allSettled(
			relationships.map((relationship) => mapRelationshipToResponse({relationship, userPartialResolver})),
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

	private async mapRpcGuildMembers(params: {
		guildId: GuildID;
		members: Array<GuildMember>;
		requestCache: RequestCache;
	}): Promise<Array<GuildMemberResponse>> {
		const {guildId, members, requestCache} = params;
		const mappedMembers = await Promise.allSettled(
			members.map((member) => mapGuildMemberToResponse(member, this.userCacheService, requestCache)),
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

	private async handleSessionRequest({
		token,
		version,
		requestCache,
		ip,
		latitude,
		longitude,
	}: HandleSessionRequestParams): Promise<RpcResponseSessionData> {
		const normalizedToken = this.normalizeSessionToken(token);
		const tokenHash = createHash('sha256').update(normalizedToken).digest('hex');
		const tokenType = this.parseTokenType(normalizedToken);
		const tokenHashPrefix = tokenHash.slice(0, 12);
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
		const bucketKey = `gateway:rpc:session:${tokenType}:${tokenHash}`;
		const rateLimitResult = await this.rateLimitService.checkLimit({
			identifier: bucketKey,
			maxAttempts: 20,
			windowMs: ms('1 minute'),
		});
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
		let authSession: AuthSession | null = null;

		if (tokenType === 'user') {
			authSession = await this.authService.getAuthSessionByToken(normalizedToken);
			if (authSession) {
				userId = authSession.userId;
			}
		} else if (tokenType === 'bot') {
			userId = await this.botAuthService.validateBotToken(normalizedToken);
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
		const userData = await this.getUserData({userId, includePrivateChannels: false});

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
			try {
				const avatarKey = `avatars/${user.id}/${this.stripAnimationPrefix(user.avatarHash)}`;
				const object = await this.storageService.readObject(Config.s3.buckets.cdn, avatarKey);
				const avatarColor = await deriveDominantAvatarColor(object);
				const updatedUser = await this.userRepository.patchUpsert(user.id, {avatar_color: avatarColor}, user.toRow());
				if (updatedUser) {
					user = updatedUser;
					this.userCacheService.setUserPartialResponseFromUserInBackground(updatedUser, requestCache);
				}
			} catch (error) {
				Logger.warn({userId: user.id, error}, 'Failed to repair user avatar color');
			}
		}

		if (user.bannerHash && user.bannerColor == null) {
			try {
				const bannerKey = `banners/${user.id}/${this.stripAnimationPrefix(user.bannerHash)}`;
				const object = await this.storageService.readObject(Config.s3.buckets.cdn, bannerKey);
				const bannerColor = await deriveDominantAvatarColor(object);
				const updatedUser = await this.userRepository.patchUpsert(user.id, {banner_color: bannerColor}, user.toRow());
				if (updatedUser) {
					user = updatedUser;
				}
			} catch (error) {
				Logger.warn({userId: user.id, error}, 'Failed to repair user banner color');
			}
		}

		userData.user = user;

		let userSettings = userData.settings;
		if (userSettings?.customStatus?.isExpired()) {
			userSettings = Object.assign(Object.create(Object.getPrototypeOf(userSettings)), userSettings, {
				customStatus: null,
			});
			userData.settings = userSettings;
		}

		let flagsToUpdate: bigint | null = null;

		if (!(user.flags & UserFlags.HAS_SESSION_STARTED)) {
			flagsToUpdate = (flagsToUpdate ?? user.flags) | UserFlags.HAS_SESSION_STARTED;
		}

		const hadPremium = user.premiumType != null && user.premiumType > 0;
		const isPremium = user.isPremium();
		const needsPremiumStrip = shouldStripExpiredPremium(user);
		const hasBeenSanitized = !!(user.flags & UserFlags.PREMIUM_PERKS_SANITIZED);
		const hasInvalidNonLifetimeDiscriminator =
			user.discriminator === 0 && user.premiumType !== UserPremiumTypes.LIFETIME;

		if (needsPremiumStrip) {
			try {
				const strippedUser = await this.userRepository.patchUpsert(user.id, createPremiumClearPatch(), user.toRow());
				if (strippedUser) {
					user = strippedUser;
					userData.user = strippedUser;
					this.userCacheService.setUserPartialResponseFromUserInBackground(strippedUser, requestCache);
				}
			} catch (error) {
				Logger.warn({userId: user.id.toString(), error}, 'Failed to strip expired premium on RPC session start');
			}
		}

		if (hasInvalidNonLifetimeDiscriminator) {
			try {
				const discriminatorResult = await this.discriminatorService.generateDiscriminator({
					username: user.username,
					user,
				});

				if (discriminatorResult.available && discriminatorResult.discriminator !== -1) {
					const updatedUser = await this.userRepository.patchUpsert(
						user.id,
						{
							discriminator: discriminatorResult.discriminator,
						},
						user.toRow(),
					);
					if (updatedUser) {
						Object.assign(user, updatedUser);
						userData.user = user;
						this.userCacheService.setUserPartialResponseFromUserInBackground(user, requestCache);
					}
				}
			} catch (error) {
				Logger.error(
					{userId: user.id.toString(), error},
					'Failed to reset invalid non-lifetime discriminator on RPC session start',
				);
			}

			flagsToUpdate = (flagsToUpdate ?? user.flags) & ~UserFlags.PREMIUM_DISCRIMINATOR;
		}

		if (hadPremium && !isPremium && !hasBeenSanitized) {
			if (user.flags & UserFlags.PREMIUM_DISCRIMINATOR) {
				try {
					const discriminatorResult = await this.discriminatorService.generateDiscriminator({
						username: user.username,
						user,
					});

					if (discriminatorResult.available && discriminatorResult.discriminator !== -1) {
						const updatedUser = await this.userRepository.patchUpsert(
							user.id,
							{
								discriminator: discriminatorResult.discriminator,
							},
							user.toRow(),
						);
						if (updatedUser) {
							Object.assign(user, updatedUser);
							this.userCacheService.setUserPartialResponseFromUserInBackground(user, requestCache);
						}
					}
				} catch (error) {
					Logger.error({userId: user.id.toString(), error}, 'Failed to reset discriminator after premium expired');
				}

				flagsToUpdate = (flagsToUpdate ?? user.flags) & ~UserFlags.PREMIUM_DISCRIMINATOR;
			}

			const guildIdsToProcess = userData.guildIds;
			try {
				const members = await Promise.all(
					guildIdsToProcess.map(async (guildId) => {
						try {
							const member = await this.guildRepository.getMember(guildId, user.id);
							return {guildId, member, error: null};
						} catch (error) {
							Logger.error(
								{userId: user.id.toString(), guildId: guildId.toString(), error},
								'Failed to fetch guild member for premium sanitization',
							);
							return {guildId, member: null, error};
						}
					}),
				);

				const membersToSanitize = members.filter(
					({member, error}) =>
						!error &&
						member &&
						!member.isPremiumSanitized &&
						(member.avatarHash || member.bannerHash || member.bio || member.accentColor !== null),
				);

				if (membersToSanitize.length > 0) {
					const updatePromises = membersToSanitize.map(({guildId, member}) =>
						this.guildRepository
							.upsertMember({
								...member!.toRow(),
								is_premium_sanitized: true,
							})
							.then((updatedMember) => ({guildId, updatedMember, error: null})),
					);

					const updatedResults = await Promise.all(updatePromises);

					const dispatchPromises = updatedResults.map(async ({guildId, updatedMember, error}) => {
						if (error) return;
						try {
							await this.gatewayService.dispatchGuild({
								guildId,
								event: 'GUILD_MEMBER_UPDATE',
								data: await mapGuildMemberToResponse(updatedMember!, this.userCacheService, requestCache),
							});
						} catch (error) {
							Logger.error(
								{userId: user.id.toString(), guildId: guildId.toString(), error},
								'Failed to dispatch guild member update after premium sanitization',
							);
						}
					});

					await Promise.all(dispatchPromises);
				}
			} catch (error) {
				Logger.error(
					{userId: user.id.toString(), guildIds: guildIdsToProcess.map(String), error},
					'Failed to sanitize guild member premium perks for multiple guilds',
				);
			}

			flagsToUpdate = (flagsToUpdate ?? user.flags) | UserFlags.PREMIUM_PERKS_SANITIZED;

			await this.gatewayService.dispatchPresence({
				userId: user.id,
				event: 'USER_UPDATE',
				data: {user: mapUserToPrivateResponse(user)},
			});
		}

		if (!user.isBot && (user.flags & UserFlags.BOT_SANITIZED) !== UserFlags.BOT_SANITIZED) {
			try {
				const sanitized = await this.sanitizeOwnedBotDiscriminators(user);
				if (sanitized) {
					flagsToUpdate = (flagsToUpdate ?? user.flags) | UserFlags.BOT_SANITIZED;
				}
			} catch (error) {
				Logger.warn(
					{userId: user.id.toString(), error},
					'Failed to run owned bot discriminator sanitization on RPC session start',
				);
			}
		}

		if (!(user.flags & UserFlags.HAS_RELATIONSHIPS_INDEXED)) {
			try {
				const relationships = userData.relationships;
				await this.userRepository.backfillRelationshipsIndex(user.id, relationships);
				flagsToUpdate = (flagsToUpdate ?? user.flags) | UserFlags.HAS_RELATIONSHIPS_INDEXED;
			} catch (error) {
				Logger.warn({userId: user.id, error}, 'Failed to backfill relationships index');
			}
		}

		if (!(user.flags & UserFlags.MESSAGES_BY_AUTHOR_BACKFILLED)) {
			try {
				await this.channelRepository.backfillMessagesByAuthorIndex(user.id);
				flagsToUpdate = (flagsToUpdate ?? user.flags) | UserFlags.MESSAGES_BY_AUTHOR_BACKFILLED;
			} catch (error) {
				Logger.warn({userId: user.id, error}, 'Failed to backfill messages by author index');
			}
		}

		if (flagsToUpdate !== null && flagsToUpdate !== user.flags) {
			await this.userRepository.patchUpsert(
				user.id,
				{
					flags: flagsToUpdate,
				},
				user.toRow(),
			);
		}

		await this.ensurePersonalNotesChannel(user);
		const cachedChannels = await this.ensurePrivateChannelsWithinLimit(user);
		userData.privateChannels = cachedChannels ?? (await this.userRepository.listPrivateChannels(user.id));

		const mapReadState = (readState: ReadState) => ({
			id: readState.channelId.toString(),
			mention_count: readState.mentionCount,
			last_message_id: readState.lastMessageId?.toString() ?? null,
			last_pin_timestamp: readState.lastPinTimestamp?.toISOString() ?? null,
		});

		let countryCode = 'US';
		if (ip) {
			const geoip = await lookupGeoip(ip);
			countryCode = geoip.countryCode ?? countryCode;
		} else {
			Logger.warn({context: 'rpc_geoip', reason: 'ip_missing'}, 'RPC session request missing IP for GeoIP');
		}

		const [privateChannels, relationships] = await Promise.all([
			this.mapRpcSessionPrivateChannels({
				channels: userData.privateChannels,
				userId: user.id,
				requestCache,
			}),
			this.mapRpcSessionRelationships({
				relationships: userData.relationships,
				userId: user.id,
				requestCache,
			}),
		]);

		const voiceAccessContext: VoiceAccessContext = {requestingUserId: user.id};
		const availableRegions = this.voiceAvailabilityService?.getAvailableRegions(voiceAccessContext) ?? [];
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

		const rtcRegions = sortedRegions.map((region) => ({
			id: region.id,
			name: region.name,
			emoji: region.emoji,
		}));
		Logger.debug(
			{
				tokenType,
				tokenHashPrefix,
				userId: user.id.toString(),
				guildCount: userData.guildIds.length,
				privateChannelCount: privateChannels.length,
				relationshipCount: relationships.length,
				countryCode,
				rtcRegionCount: rtcRegions.length,
			},
			'RPC session handling completed',
		);

		return {
			auth_session_id_hash: authSession ? uint8ArrayToBase64(authSession.sessionIdHash, {urlSafe: true}) : undefined,
			user: mapUserToPrivateResponse(user),
			user_settings: userData.settings
				? mapUserSettingsToResponse({
						settings: userData.settings,
						memberGuildIds: userData.guildIds,
					})
				: null,
			user_guild_settings: userData.guildSettings.map(mapUserGuildSettingsToResponse),
			notes: Object.fromEntries(
				Array.from(userData.notes.entries()).map(([userId, note]) => [userId.toString(), note]),
			),
			read_states: userData.readStates.map(mapReadState),
			guild_ids: userData.guildIds.map(String),
			private_channels: privateChannels,
			relationships,
			favorite_memes: userData.favoriteMemes.map(mapFavoriteMemeToResponse),
			pinned_dms: userData.pinnedDMs?.map(String) ?? [],
			country_code: countryCode,
			rtc_regions: rtcRegions,
			version,
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
		return {
			...this.createGuildCollectionResponse('guild'),
			guild: mapGuildToGuildResponse(repairedEmbedSplashGuild),
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

	private async getUserData({userId, includePrivateChannels = true}: GetUserDataParams): Promise<UserData | null> {
		const user = await this.userRepository.findUnique(userId);
		if (!user) return null;

		if (user.isBot) {
			const guildIds = await this.userRepository.getUserGuildIds(userId);

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
			};
		}

		const [settingsResult, notes, readStates, guildIds, relationships, favoriteMemes, pinnedDMs] = await Promise.all([
			this.userRepository.findSettings(userId),
			this.userRepository.getUserNotes(userId),
			this.readStateService.getReadStates(userId),
			this.userRepository.getUserGuildIds(userId),
			this.userRepository.listRelationships(userId),
			this.favoriteMemeRepository.findByUserId(userId),
			this.userRepository.getPinnedDms(userId),
		]);

		const privateChannels = includePrivateChannels ? await this.userRepository.listPrivateChannels(userId) : [];

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
				await this.userRepository.upsertSettings(updatedRow);
				settings = new UserSettings(updatedRow);
			}
		}

		const guildSettings = await this.userRepository.findAllGuildSettings(userId);

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
		};
	}

	private async ensurePrivateChannelsWithinLimit(user: User): Promise<Array<Channel> | null> {
		if (user.isBot) {
			return [];
		}

		const channels = await this.userRepository.listPrivateChannels(user.id);
		const totalPrivateChannels = channels.length;
		const limit = this.resolveLimitForUser(
			user ?? null,
			'max_private_channels_per_user',
			MAX_PRIVATE_CHANNELS_PER_USER,
		);
		if (totalPrivateChannels <= limit) {
			return channels;
		}

		const closableDms = channels
			.filter((channel) => channel.type === ChannelTypes.DM)
			.sort((a, b) => {
				const aValue = a.lastMessageId ?? 0n;
				const bValue = b.lastMessageId ?? 0n;
				if (aValue < bValue) return -1;
				if (aValue > bValue) return 1;
				return 0;
			});

		const toClose = totalPrivateChannels - limit;
		let closed = 0;
		for (const channel of closableDms) {
			if (closed >= toClose) {
				break;
			}
			await this.userRepository.closeDmForUser(user.id, channel.id);
			closed += 1;
		}

		if (closed < toClose) {
			Logger.warn(
				{
					user_id: user.id.toString(),
					total_private_channels: totalPrivateChannels,
					required_closures: toClose,
					actual_closures: closed,
				},
				'Unable to close enough DMs to satisfy private channel limit',
			);
		}

		return null;
	}

	private resolveLimitForUser(user: User | null, key: LimitKey, fallback: number): number {
		const ctx = createLimitMatchContext({user});
		return resolveLimitSafe(this.limitConfigService.getConfigSnapshot(), ctx, key, fallback);
	}

	private async getUserGuildSettings(params: {
		userIds: Array<UserID>;
		guildId: GuildID;
	}): Promise<{user_guild_settings: Array<UserGuildSettings | null>}> {
		const {userIds, guildId} = params;
		const actualGuildId = guildId === createGuildID(0n) ? null : guildId;
		const userGuildSettings = await Promise.all(
			userIds.map((userId) => this.userRepository.findGuildSettings(userId, actualGuildId)),
		);
		return {user_guild_settings: userGuildSettings};
	}

	private async getPushSubscriptions(params: {
		userIds: Array<UserID>;
	}): Promise<
		Record<string, Array<{subscription_id: string; endpoint: string; p256dh_key: string; auth_key: string}>>
	> {
		const {userIds} = params;
		const subscriptionsMap = await this.userRepository.getBulkPushSubscriptions(userIds);

		const result: Record<
			string,
			Array<{subscription_id: string; endpoint: string; p256dh_key: string; auth_key: string}>
		> = {};

		for (const [userId, subscriptions] of subscriptionsMap.entries()) {
			result[userId.toString()] = subscriptions.map((sub) => ({
				subscription_id: sub.subscriptionId,
				endpoint: sub.endpoint,
				p256dh_key: sub.p256dhKey,
				auth_key: sub.authKey,
			}));
		}

		return result;
	}

	private async getBadgeCounts(params: {userIds: Array<UserID>}): Promise<Record<string, number>> {
		const {userIds} = params;
		const uniqueUserIds = Array.from(new Set(userIds)) as Array<UserID>;

		const allReadStates = await Promise.all(uniqueUserIds.map((userId) => this.readStateService.getReadStates(userId)));

		const badgeCounts: Record<string, number> = {};
		uniqueUserIds.forEach((userId, index) => {
			const readStates = allReadStates[index];
			const totalMentions = readStates.reduce((sum, state) => sum + state.mentionCount, 0);
			badgeCounts[userId.toString()] = totalMentions;
		});

		return badgeCounts;
	}

	private async deletePushSubscriptions(params: {
		subscriptions: Array<{userId: UserID; subscriptionId: string}>;
	}): Promise<{success: boolean}> {
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

		return this.guildRepository.upsert({
			...guild.toRow(),
			system_channel_id: danglingSystemChannel ? null : guild.systemChannelId,
			rules_channel_id: danglingRulesChannel ? null : guild.rulesChannelId,
			afk_channel_id: danglingAfkChannel ? null : guild.afkChannelId,
		});
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

			const repairedGuild = await this.guildRepository.upsert({
				...guild.toRow(),
				banner_height: bannerHeight,
				banner_width: bannerWidth,
			});
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

			const repairedGuild = await this.guildRepository.upsert({
				...guild.toRow(),
				splash_height: splashHeight,
				splash_width: splashWidth,
			});
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

			const repairedGuild = await this.guildRepository.upsert({
				...guild.toRow(),
				embed_splash_height: embedSplashHeight,
				embed_splash_width: embedSplashWidth,
			});
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
		const {channelId, messageId, participants, endedTimestamp, requestCache} = params;

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

		const messageResponse = await mapMessageToResponse({
			message: updatedMessage,
			userCacheService: this.userCacheService,
			requestCache,
			mediaService: this.mediaService,
			getReferencedMessage: (channelId, messageId) => this.channelRepository.getMessage(channelId, messageId),
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
