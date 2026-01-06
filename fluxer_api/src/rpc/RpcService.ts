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
import sharp from 'sharp';
import {uint8ArrayToBase64} from 'uint8array-extras';
import type {AuthService} from '~/auth/AuthService';
import {
	type ChannelID,
	createChannelID,
	createGuildID,
	createMessageID,
	createUserID,
	type GuildID,
	type MessageID,
	type UserID,
	userIdToChannelId,
	vanityCodeToInviteCode,
} from '~/BrandedTypes';
import {Config} from '~/Config';
import {
	ChannelTypes,
	GroupDmAddPermissionFlags,
	IncomingCallFlags,
	MAX_PRIVATE_CHANNELS_PER_USER,
	MessageTypes,
	UserFlags,
} from '~/Constants';
import {mapChannelToResponse, mapMessageToResponse} from '~/channel/ChannelModel';
import type {IChannelRepository} from '~/channel/IChannelRepository';
import {RateLimitError, UnauthorizedError, UnknownGuildError} from '~/Errors';
import {mapFavoriteMemeToResponse} from '~/favorite_meme/FavoriteMemeModel';
import type {IFavoriteMemeRepository} from '~/favorite_meme/IFavoriteMemeRepository';
import type {FeatureFlagService} from '~/feature_flag/FeatureFlagService';
import {
	mapGuildEmojiToResponse,
	mapGuildMemberToResponse,
	mapGuildRoleToResponse,
	mapGuildStickerToResponse,
	mapGuildToGuildResponse,
} from '~/guild/GuildModel';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {IDiscriminatorService} from '~/infrastructure/DiscriminatorService';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IMediaService} from '~/infrastructure/IMediaService';
import type {IRateLimitService} from '~/infrastructure/IRateLimitService';
import type {IStorageService} from '~/infrastructure/IStorageService';
import {StorageService} from '~/infrastructure/StorageService';
import type {UserCacheService} from '~/infrastructure/UserCacheService';
import type {IInviteRepository} from '~/invite/IInviteRepository';
import {Logger} from '~/Logger';
import {
	type AuthSession,
	type Channel,
	type FavoriteMeme,
	type Guild,
	type GuildEmoji,
	type GuildMember,
	type GuildRole,
	type GuildSticker,
	type ReadState,
	type Relationship,
	type User,
	type UserGuildSettings,
	UserSettings,
} from '~/Models';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import type {BotAuthService} from '~/oauth/BotAuthService';
import type {PackService} from '~/pack/PackService';
import type {ReadStateService} from '~/read_state/ReadStateService';
import type {RpcRequest, RpcResponse, RpcResponseGuildData, RpcResponseSessionData} from '~/rpc/RpcModel';
import type {IUserRepository} from '~/user/IUserRepository';
import {CustomStatusValidator} from '~/user/services/CustomStatusValidator';
import {getCachedUserPartialResponse} from '~/user/UserCacheHelpers';
import {createPremiumClearPatch, shouldStripExpiredPremium} from '~/user/UserHelpers';
import {
	mapRelationshipToResponse,
	mapUserGuildSettingsToResponse,
	mapUserSettingsToResponse,
	mapUserToPrivateResponse,
} from '~/user/UserModel';
import {isUserAdult} from '~/utils/AgeUtils';
import {deriveDominantAvatarColor} from '~/utils/AvatarColorUtils';
import {calculateDistance, parseCoordinate} from '~/utils/GeoUtils';
import {lookupGeoip} from '~/utils/IpUtils';
import type {VoiceAccessContext, VoiceAvailabilityService} from '~/voice/VoiceAvailabilityService';
import type {VoiceService} from '~/voice/VoiceService';
import type {IWebhookRepository} from '~/webhook/IWebhookRepository';

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

interface HandleGuildRequestParams {
	guildId: GuildID;
	requestCache: RequestCache;
}

interface GetGuildDataParams {
	guildId: GuildID;
}

interface GetUserDataParams {
	userId: UserID;
	includePrivateChannels?: boolean;
}

interface GuildData {
	guild: Guild;
	channels: Array<Channel>;
	emojis: Array<GuildEmoji>;
	stickers: Array<GuildSticker>;
	members: Array<GuildMember>;
	roles: Array<GuildRole>;
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

const DM_HISTORY_BATCH_SIZE = 1000;

export class RpcService {
	private readonly customStatusValidator: CustomStatusValidator;

	constructor(
		private userRepository: IUserRepository,
		private guildRepository: IGuildRepository,
		private channelRepository: IChannelRepository,
		private userCacheService: UserCacheService,
		private readStateService: ReadStateService,
		private authService: AuthService,
		private gatewayService: IGatewayService,
		private discriminatorService: IDiscriminatorService,
		private favoriteMemeRepository: IFavoriteMemeRepository,
		private packService: PackService,
		private botAuthService: BotAuthService,
		private inviteRepository: IInviteRepository,
		private webhookRepository: IWebhookRepository,
		private storageService: IStorageService = new StorageService(),
		private voiceService?: VoiceService,
		private voiceAvailabilityService?: VoiceAvailabilityService,
		private rateLimitService?: IRateLimitService,
		private mediaService?: IMediaService,
		private featureFlagService?: FeatureFlagService,
	) {
		this.customStatusValidator = new CustomStatusValidator(this.userRepository, this.guildRepository, this.packService);
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

	private async backfillHistoricalDmChannels(userId: UserID): Promise<void> {
		const processedChannels = new Set<string>();
		let lastChannelId: ChannelID | undefined;
		let lastMessageId: MessageID | undefined;

		while (true) {
			const messageRefs = await this.channelRepository.listMessagesByAuthor(
				userId,
				DM_HISTORY_BATCH_SIZE,
				lastChannelId,
				lastMessageId,
			);
			if (messageRefs.length === 0) {
				break;
			}

			for (const {channelId} of messageRefs) {
				const channelKey = channelId.toString();
				if (processedChannels.has(channelKey)) {
					continue;
				}
				processedChannels.add(channelKey);

				const channel = await this.channelRepository.channelData.findUnique(channelId);
				if (!channel || channel.guildId || channel.type !== ChannelTypes.DM) {
					continue;
				}

				await this.userRepository.recordHistoricalDmChannel(userId, channelId, false);
			}

			const lastRef = messageRefs[messageRefs.length - 1];
			lastChannelId = lastRef.channelId;
			lastMessageId = lastRef.messageId;

			if (messageRefs.length < DM_HISTORY_BATCH_SIZE) {
				break;
			}
		}
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
			case 'guild':
				return {
					type: 'guild',
					data: await this.handleGuildRequest({
						guildId: createGuildID(request.guild_id),
						requestCache,
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
			case 'get_visionary_slots':
				return {
					type: 'get_visionary_slots',
					data: await this.getVisionarySlotCounts(),
				};
			case 'voice_get_token': {
				if (!this.voiceService) {
					throw new Error('Voice is not enabled on this server');
				}
				const result = await this.voiceService.getVoiceToken({
					guildId: request.guild_id !== undefined ? createGuildID(request.guild_id) : undefined,
					channelId: createChannelID(request.channel_id),
					userId: createUserID(request.user_id),
					latitude: request.latitude,
					longitude: request.longitude,
					canSpeak: request.can_speak,
					canStream: request.can_stream,
					canVideo: request.can_video,
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

	private async handleSessionRequest({
		token,
		version,
		requestCache,
		ip,
		latitude,
		longitude,
	}: HandleSessionRequestParams): Promise<RpcResponseSessionData> {
		if (this.rateLimitService) {
			const tokenHash = createHash('sha256').update(token).digest('hex');
			const tokenType = this.parseTokenType(token);
			const bucketKey = `gateway:rpc:session:${tokenType}:${tokenHash}`;
			const result = await this.rateLimitService.checkLimit({
				identifier: bucketKey,
				maxAttempts: 20,
				windowMs: 60000,
			});
			if (!result.allowed) {
				throw new RateLimitError({
					retryAfter: result.retryAfter!,
					retryAfterDecimal: result.retryAfterDecimal,
					limit: result.limit,
					resetTime: result.resetTime,
					global: false,
				});
			}
		}

		let userId: UserID | null = null;
		let authSession: AuthSession | null = null;

		const tokenType = this.parseTokenType(token);

		if (tokenType === 'user') {
			authSession = await this.authService.getAuthSessionByToken(token);
			if (authSession) {
				userId = authSession.userId;
			}
		} else if (tokenType === 'bot') {
			userId = await this.botAuthService.validateBotToken(token);
		}

		if (!userId) {
			throw new UnauthorizedError();
		}
		const userData = await this.getUserData({userId, includePrivateChannels: false});

		if (!userData || !userData.user) {
			throw new UnauthorizedError();
		}

		let user = userData.user;

		if (user.avatarHash && user.avatarColor == null) {
			try {
				const avatarKey = `avatars/${user.id}/${this.stripAnimationPrefix(user.avatarHash)}`;
				const object = await this.storageService.readObject(Config.s3.buckets.cdn, avatarKey);
				const avatarColor = await deriveDominantAvatarColor(object);
				const updatedUser = await this.userRepository.patchUpsert(user.id, {avatar_color: avatarColor});
				if (updatedUser) {
					user = updatedUser;
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
				const updatedUser = await this.userRepository.patchUpsert(user.id, {banner_color: bannerColor});
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

		if (needsPremiumStrip) {
			try {
				const strippedUser = await this.userRepository.patchUpsert(user.id, createPremiumClearPatch());
				if (strippedUser) {
					user = strippedUser;
					userData.user = strippedUser;
				}
			} catch (error) {
				Logger.warn({userId: user.id.toString(), error}, 'Failed to strip expired premium on RPC session start');
			}
		}

		if (hadPremium && !isPremium && !hasBeenSanitized) {
			if (user.flags & UserFlags.PREMIUM_DISCRIMINATOR) {
				try {
					const discriminatorResult = await this.discriminatorService.generateDiscriminator({
						username: user.username,
						isPremium: false,
					});

					if (discriminatorResult.available && discriminatorResult.discriminator !== -1) {
						await this.userRepository.patchUpsert(user.id, {
							discriminator: discriminatorResult.discriminator,
						});
						const updatedUser = await this.userRepository.findUnique(user.id);
						if (updatedUser) {
							Object.assign(user, updatedUser);
						}
					}
				} catch (_error) {}

				flagsToUpdate = (flagsToUpdate ?? user.flags) & ~UserFlags.PREMIUM_DISCRIMINATOR;
			}

			const guildIdsToProcess = userData.guildIds;
			await Promise.all(
				guildIdsToProcess.map(async (guildId) => {
					try {
						const member = await this.guildRepository.getMember(guildId, user.id);
						if (
							member &&
							!member.isPremiumSanitized &&
							(member.avatarHash || member.bannerHash || member.bio || member.accentColor !== null)
						) {
							await this.guildRepository.upsertMember({
								...member.toRow(),
								is_premium_sanitized: true,
							});

							const updatedMember = await this.guildRepository.getMember(guildId, user.id);
							if (updatedMember) {
								await this.gatewayService.dispatchGuild({
									guildId,
									event: 'GUILD_MEMBER_UPDATE',
									data: await mapGuildMemberToResponse(updatedMember, this.userCacheService, requestCache),
								});
							}
						}
					} catch (_error) {}
				}),
			);

			flagsToUpdate = (flagsToUpdate ?? user.flags) | UserFlags.PREMIUM_PERKS_SANITIZED;

			await this.gatewayService.dispatchPresence({
				userId: user.id,
				event: 'USER_UPDATE',
				data: {user: mapUserToPrivateResponse(user)},
			});
		}

		if (!(user.flags & UserFlags.HAS_DM_HISTORY_BACKFILLED)) {
			try {
				await this.backfillHistoricalDmChannels(user.id);
				flagsToUpdate = (flagsToUpdate ?? user.flags) | UserFlags.HAS_DM_HISTORY_BACKFILLED;
			} catch (error) {
				Logger.warn({userId: user.id, error}, 'Failed to backfill DM history');
			}
		}

		if (flagsToUpdate !== null && flagsToUpdate !== user.flags) {
			await this.userRepository.patchUpsert(user.id, {
				flags: flagsToUpdate,
			});
		}

		await this.ensurePersonalNotesChannel(user);
		const cachedChannels = await this.ensurePrivateChannelsWithinLimit(user);
		userData.privateChannels = cachedChannels ?? (await this.userRepository.listPrivateChannels(user.id));

		const mapChannel = (channel: Channel) =>
			mapChannelToResponse({
				channel,
				currentUserId: user.id,
				userCacheService: this.userCacheService,
				requestCache,
			});

		const userPartialResolver = (userId: UserID) =>
			getCachedUserPartialResponse({
				userId,
				userCacheService: this.userCacheService,
				requestCache,
			});

		const mapRelationship = (relationship: Relationship) =>
			mapRelationshipToResponse({relationship, userPartialResolver});

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
			Promise.all(userData.privateChannels.map(mapChannel)),
			Promise.all(userData.relationships.map(mapRelationship)),
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

		const rtcRegions = sortedRegions.map((region) => region.id);

		return {
			auth_session_id_hash: authSession ? uint8ArrayToBase64(authSession.sessionIdHash, {urlSafe: true}) : undefined,
			user: mapUserToPrivateResponse(user),
			user_settings: userData.settings
				? mapUserSettingsToResponse({
						settings: userData.settings,
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
			feature_flags: this.featureFlagService?.getConfigForSession(),
		};
	}

	private async handleGuildRequest({guildId, requestCache}: HandleGuildRequestParams): Promise<RpcResponseGuildData> {
		const guildData = await this.getGuildData({guildId});

		if (!guildData) {
			throw new UnknownGuildError();
		}

		const mapChannel = (channel: Channel) =>
			mapChannelToResponse({
				channel,
				currentUserId: null,
				userCacheService: this.userCacheService,
				requestCache,
			});

		const mapMember = (member: GuildMember) => mapGuildMemberToResponse(member, this.userCacheService, requestCache);

		const [channels, members] = await Promise.all([
			Promise.all(guildData.channels.map(mapChannel)),
			Promise.all(guildData.members.map(mapMember)),
		]);

		return {
			guild: mapGuildToGuildResponse(guildData.guild),
			roles: guildData.roles.map(mapGuildRoleToResponse),
			channels,
			emojis: guildData.emojis.map(mapGuildEmojiToResponse),
			stickers: guildData.stickers.map(mapGuildStickerToResponse),
			members,
		};
	}

	private async getGuildData({guildId}: GetGuildDataParams): Promise<GuildData | null> {
		const [guildResult, channels, emojis, stickers, members, roles] = await Promise.all([
			this.guildRepository.findUnique(guildId),
			this.channelRepository.listGuildChannels(guildId),
			this.guildRepository.listEmojis(guildId),
			this.guildRepository.listStickers(guildId),
			this.guildRepository.listMembers(guildId),
			this.guildRepository.listRoles(guildId),
		]);
		if (!guildResult) return null;

		const repairedBannerGuild = await this.repairGuildBannerHeight(guildResult);
		const repairedSplashGuild = await this.repairGuildSplashDimensions(repairedBannerGuild);
		const repairedEmbedSplashGuild = await this.repairGuildEmbedSplashDimensions(repairedSplashGuild);
		const updatedGuild = await this.updateGuildMemberCount(repairedEmbedSplashGuild, members.length);

		this.repairOrphanedInvitesAndWebhooks({guild: updatedGuild, channels}).catch((error) => {
			Logger.warn({guildId: guildId.toString(), error}, 'Failed to repair orphaned invites/webhooks');
		});

		return {
			guild: updatedGuild,
			channels,
			emojis,
			stickers,
			members,
			roles,
		};
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
		if (totalPrivateChannels <= MAX_PRIVATE_CHANNELS_PER_USER) {
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

		const toClose = totalPrivateChannels - MAX_PRIVATE_CHANNELS_PER_USER;
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

	private async getVisionarySlotCounts(): Promise<{total: number; bought: number; remaining: number}> {
		const allSlots = await this.userRepository.listVisionarySlots();
		const bought = allSlots.filter((slot) => slot.isReserved()).length;
		const remaining = Math.max(allSlots.length - bought, 0);
		return {
			total: allSlots.length,
			bought,
			remaining,
		};
	}

	private async kickTemporaryMember(params: {userId: UserID; guildIds: Array<GuildID>}): Promise<boolean> {
		const {userId, guildIds} = params;

		try {
			await Promise.all(
				guildIds.map(async (guildId) => {
					const member = await this.guildRepository.getMember(guildId, userId);
					if (member?.isTemporary) {
						await this.guildRepository.deleteMember(guildId, userId);

						const guild = await this.guildRepository.findUnique(guildId);
						if (guild) {
							await this.updateGuildMemberCount(guild, Math.max(0, guild.memberCount - 1));
						}

						await this.gatewayService.dispatchGuild({
							guildId,
							event: 'GUILD_MEMBER_REMOVE',
							data: {user: {id: userId.toString()}},
						});

						await this.gatewayService.leaveGuild({userId, guildId});
					}
				}),
			);
			return true;
		} catch (_error) {
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

		if (!updatedMessage || !this.mediaService) {
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
	}): Promise<ReturnType<typeof mapChannelToResponse> | null> {
		const {channelId, userId, requestCache} = params;

		const channel = await this.channelRepository.findUnique(channelId);
		if (!channel) {
			return null;
		}

		if (!channel.recipientIds.has(userId)) {
			return null;
		}

		return mapChannelToResponse({
			channel,
			currentUserId: userId,
			userCacheService: this.userCacheService,
			requestCache,
		});
	}
}
