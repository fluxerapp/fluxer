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

import type {ChannelID, GuildID, UserID} from '@fluxer/api/src/BrandedTypes';
import {createChannelID, createGuildID, guildIdToRoleId} from '@fluxer/api/src/BrandedTypes';
import type {IChannelRepository} from '@fluxer/api/src/channel/IChannelRepository';
import type {ChannelService} from '@fluxer/api/src/channel/services/ChannelService';
import {BatchBuilder} from '@fluxer/api/src/database/Cassandra';
import type {GuildRow} from '@fluxer/api/src/database/types/GuildTypes';
import {mapGuildToGuildResponse, mapGuildToPartialResponse} from '@fluxer/api/src/guild/GuildModel';
import type {IGuildDiscoveryRepository} from '@fluxer/api/src/guild/repositories/GuildDiscoveryRepository';
import type {IGuildRepositoryAggregate} from '@fluxer/api/src/guild/repositories/IGuildRepositoryAggregate';
import type {GuildDataHelpers} from '@fluxer/api/src/guild/services/data/GuildDataHelpers';
import type {EntityAssetService, PreparedAssetUpload} from '@fluxer/api/src/infrastructure/EntityAssetService';
import type {IGatewayService} from '@fluxer/api/src/infrastructure/IGatewayService';
import {getMetricsService} from '@fluxer/api/src/infrastructure/MetricsService';
import type {SnowflakeService} from '@fluxer/api/src/infrastructure/SnowflakeService';
import type {InviteRepository} from '@fluxer/api/src/invite/InviteRepository';
import {Logger} from '@fluxer/api/src/Logger';
import type {LimitConfigService} from '@fluxer/api/src/limits/LimitConfigService';
import {createLimitMatchContext} from '@fluxer/api/src/limits/LimitMatchContextBuilder';
import type {RequestCache} from '@fluxer/api/src/middleware/RequestCacheMiddleware';
import {Guild} from '@fluxer/api/src/models/Guild';
import type {User} from '@fluxer/api/src/models/User';
import {getGuildSearchService} from '@fluxer/api/src/SearchFactory';
import type {GuildDiscoveryContext} from '@fluxer/api/src/search/guild/GuildSearchSerializer';
import {
	Channels,
	ChannelsByGuild,
	GuildMembers,
	GuildMembersByUserId,
	GuildRoles,
	Guilds,
} from '@fluxer/api/src/Tables';
import {withBusinessSpan} from '@fluxer/api/src/telemetry/BusinessSpans';
import {withSpan} from '@fluxer/api/src/telemetry/Tracing';
import type {IUserRepository} from '@fluxer/api/src/user/IUserRepository';
import {mapUserSettingsToResponse} from '@fluxer/api/src/user/UserMappers';
import {removeGuildFromUserFolders} from '@fluxer/api/src/user/utils/GuildFolderUtils';
import type {IWebhookRepository} from '@fluxer/api/src/webhook/IWebhookRepository';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ChannelTypes, DEFAULT_PERMISSIONS, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {
	GuildFeatures,
	GuildSplashCardAlignment,
	type GuildSplashCardAlignmentValue,
	JoinSourceTypes,
	SystemChannelFlags,
} from '@fluxer/constants/src/GuildConstants';
import {MAX_GUILDS_NON_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {DEFAULT_GUILD_FOLDER_ICON, UNCATEGORIZED_FOLDER_ID} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {AccessDeniedError} from '@fluxer/errors/src/domains/core/AccessDeniedError';
import {InputValidationError} from '@fluxer/errors/src/domains/core/InputValidationError';
import {MissingPermissionsError} from '@fluxer/errors/src/domains/core/MissingPermissionsError';
import {MaxGuildsError} from '@fluxer/errors/src/domains/guild/MaxGuildsError';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import {resolveLimit} from '@fluxer/limits/src/LimitResolver';
import type {GuildCreateRequest, GuildUpdateRequest} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import type {GuildPartialResponse, GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {extractTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';

interface PreparedGuildAssets {
	icon: PreparedAssetUpload | null;
	banner: PreparedAssetUpload | null;
	splash: PreparedAssetUpload | null;
	embed_splash: PreparedAssetUpload | null;
}

const BASE_GUILD_FEATURES: ReadonlyArray<string> = [
	GuildFeatures.ANIMATED_ICON,
	GuildFeatures.ANIMATED_BANNER,
	GuildFeatures.BANNER,
	GuildFeatures.INVITE_SPLASH,
];

export class GuildOperationsService {
	constructor(
		private readonly guildRepository: IGuildRepositoryAggregate,
		private readonly channelRepository: IChannelRepository,
		private readonly inviteRepository: InviteRepository,
		private readonly channelService: ChannelService,
		private readonly gatewayService: IGatewayService,
		private readonly entityAssetService: EntityAssetService,
		private readonly userRepository: IUserRepository,
		private readonly snowflakeService: SnowflakeService,
		private readonly webhookRepository: IWebhookRepository,
		private readonly helpers: GuildDataHelpers,
		private readonly limitConfigService: LimitConfigService,
		private readonly discoveryRepository: IGuildDiscoveryRepository,
	) {}

	async getGuild({userId, guildId}: {userId: UserID; guildId: GuildID}): Promise<GuildResponse> {
		try {
			const guild = await this.gatewayService.getGuildData({guildId, userId});
			if (!guild) throw new UnknownGuildError();
			return guild;
		} catch (error) {
			if (this.isGuildAccessError(error)) {
				if (await this.guildExists(guildId)) {
					throw new AccessDeniedError();
				}
				throw new UnknownGuildError();
			}
			throw error;
		}
	}

	async getUserGuilds(
		userId: UserID,
		options?: {
			before?: GuildID;
			after?: GuildID;
			limit?: number;
			withCounts?: boolean;
		},
	): Promise<Array<GuildResponse>> {
		let guilds = await this.guildRepository.listUserGuilds(userId);
		guilds.sort((a, b) => (a.id < b.id ? -1 : 1));

		if (options?.after) {
			const index = guilds.findIndex((g) => g.id === options.after);
			if (index !== -1) {
				guilds = guilds.slice(index + 1);
			}
		} else if (options?.before) {
			const index = guilds.findIndex((g) => g.id === options.before);
			if (index !== -1) {
				guilds = guilds.slice(0, index);
			}
		}

		const limit = options?.limit ?? 200;
		guilds = guilds.slice(0, limit);

		const guildIds = guilds.map((g) => g.id);
		let permissionsMap = new Map<GuildID, bigint>();
		try {
			permissionsMap = await this.gatewayService.getUserPermissionsBatch({guildIds, userId});
		} catch (error) {
			Logger.warn(
				{userId: userId.toString(), guildCount: guildIds.length, error},
				'[GuildOperationsService] Failed to fetch guild permissions batch for list_guilds; returning without permissions',
			);
		}

		const responses = guilds.map((guild) => {
			const permissions = permissionsMap.get(guild.id);
			if (permissions == null) {
				return mapGuildToGuildResponse(guild);
			}
			return mapGuildToGuildResponse(guild, {permissions});
		});

		if (!options?.withCounts) {
			return responses;
		}

		const guildsWithCounts: Array<GuildResponse> = [];
		const countBatchSize = 25;

		for (let index = 0; index < guilds.length; index += countBatchSize) {
			const guildChunk = guilds.slice(index, index + countBatchSize);
			const responseChunk = responses.slice(index, index + countBatchSize);

			const batchResults = await Promise.all(
				guildChunk.map(async (guild, chunkIndex) => {
					const baseResponse = responseChunk[chunkIndex] ?? mapGuildToGuildResponse(guild);
					try {
						const counts = await this.gatewayService.getGuildCounts(guild.id);
						return {
							...baseResponse,
							approximate_member_count: counts.memberCount,
							approximate_presence_count: counts.presenceCount,
						};
					} catch (error) {
						Logger.warn(
							{guildId: guild.id.toString(), userId: userId.toString(), error},
							'[GuildOperationsService] Failed to fetch guild counts for list_guilds; returning without counts for guild',
						);
						return baseResponse;
					}
				}),
			);

			guildsWithCounts.push(...batchResults);
		}

		return guildsWithCounts;
	}

	async getPublicGuildData(guildId: GuildID): Promise<GuildPartialResponse> {
		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) throw new UnknownGuildError();
		return mapGuildToPartialResponse(guild);
	}

	async getGuildSystem(guildId: GuildID): Promise<Guild> {
		const guild = await this.guildRepository.findUnique(guildId);
		if (!guild) throw new UnknownGuildError();
		return guild;
	}

	async createGuild(
		params: {user: User; data: GuildCreateRequest},
		_auditLogReason?: string | null,
	): Promise<GuildResponse> {
		return await withBusinessSpan('fluxer.guild.create', 'fluxer.guilds.created', {}, () =>
			this.performCreateGuild(params, _auditLogReason),
		);
	}

	private async performCreateGuild(
		params: {user: User; data: GuildCreateRequest},
		_auditLogReason?: string | null,
	): Promise<GuildResponse> {
		try {
			const {user, data} = params;
			const currentGuildCount = await this.guildRepository.countUserGuilds(user.id);
			let maxGuilds = MAX_GUILDS_NON_PREMIUM;
			const ctx = createLimitMatchContext({user});
			maxGuilds = resolveLimit(this.limitConfigService.getConfigSnapshot(), ctx, 'max_guilds');
			if (currentGuildCount >= maxGuilds) throw new MaxGuildsError(maxGuilds);

			const guildId = createGuildID(await this.snowflakeService.generate());
			const textCategoryId = createChannelID(await this.snowflakeService.generate());
			const voiceCategoryId = createChannelID(await this.snowflakeService.generate());
			const generalChannelId = createChannelID(await this.snowflakeService.generate());
			const generalVoiceId = createChannelID(await this.snowflakeService.generate());

			let preparedIcon: PreparedAssetUpload | null = null;
			if (data.icon) {
				preparedIcon = await this.entityAssetService.prepareAssetUpload({
					assetType: 'icon',
					entityType: 'guild',
					entityId: guildId,
					previousHash: null,
					base64Image: data.icon,
					errorPath: 'icon',
				});
			}
			const iconKey = preparedIcon?.newHash ?? null;
			const shouldUseEmptyFeatures = data.empty_features ?? false;
			const featuresSet = shouldUseEmptyFeatures ? new Set<string>() : new Set(BASE_GUILD_FEATURES);

			const isUnclaimedOwner = !user.passwordHash;
			if (isUnclaimedOwner) {
				featuresSet.add(GuildFeatures.INVITES_DISABLED);
			}

			const guildData: GuildRow = {
				guild_id: guildId,
				owner_id: user.id,
				name: data.name,
				vanity_url_code: null,
				icon_hash: iconKey,
				banner_hash: null,
				banner_width: null,
				banner_height: null,
				splash_hash: null,
				splash_width: null,
				splash_height: null,
				splash_card_alignment: GuildSplashCardAlignment.CENTER,
				embed_splash_hash: null,
				embed_splash_width: null,
				embed_splash_height: null,
				features: featuresSet,
				verification_level: 0,
				mfa_level: 0,
				nsfw_level: 0,
				explicit_content_filter: 0,
				default_message_notifications: 0,
				system_channel_id: generalChannelId,
				system_channel_flags: 0,
				rules_channel_id: null,
				afk_channel_id: null,
				afk_timeout: 300,
				disabled_operations: 0,
				member_count: 1,
				audit_logs_indexed_at: null,
				members_indexed_at: null,
				message_history_cutoff: null,
				version: 1,
			};

			const batch = new BatchBuilder();

			batch.addPrepared(Guilds.insert(guildData));

			const addChannel = (
				channelId: ChannelID,
				type: number,
				name: string,
				parentId: ChannelID | null,
				position: number,
				bitrate: number | null = null,
			) => {
				batch.addPrepared(
					Channels.insert({
						channel_id: channelId,
						guild_id: guildId,
						type,
						name,
						topic: null,
						icon_hash: null,
						url: null,
						parent_id: parentId,
						position,
						owner_id: null,
						recipient_ids: null,
						nsfw: false,
						rate_limit_per_user: 0,
						bitrate,
						user_limit: bitrate !== null ? 0 : null,
						rtc_region: null,
						last_message_id: null,
						last_pin_timestamp: null,
						permission_overwrites: null,
						nicks: null,
						soft_deleted: false,
						indexed_at: null,
						version: 1,
					}),
				);
				batch.addPrepared(
					ChannelsByGuild.upsertAll({
						guild_id: guildId,
						channel_id: channelId,
					}),
				);
			};

			addChannel(textCategoryId, ChannelTypes.GUILD_CATEGORY, 'Text Channels', null, 0);
			addChannel(voiceCategoryId, ChannelTypes.GUILD_CATEGORY, 'Voice Channels', null, 1);
			addChannel(generalChannelId, ChannelTypes.GUILD_TEXT, 'general', textCategoryId, 0);
			addChannel(generalVoiceId, ChannelTypes.GUILD_VOICE, 'General', voiceCategoryId, 0, 64000);

			batch.addPrepared(
				GuildRoles.insert({
					guild_id: guildId,
					role_id: guildIdToRoleId(guildId),
					name: '@everyone',
					permissions: DEFAULT_PERMISSIONS,
					position: 0,
					hoist_position: null,
					color: 0,
					icon_hash: null,
					unicode_emoji: null,
					hoist: false,
					mentionable: false,
					version: 1,
				}),
			);

			batch.addPrepared(
				GuildMembers.insert({
					guild_id: guildId,
					user_id: user.id,
					joined_at: new Date(),
					nick: null,
					avatar_hash: null,
					banner_hash: null,
					bio: null,
					pronouns: null,
					accent_color: null,
					join_source_type: JoinSourceTypes.CREATOR,
					source_invite_code: null,
					inviter_id: null,
					deaf: false,
					mute: false,
					communication_disabled_until: null,
					role_ids: null,
					is_premium_sanitized: null,
					temporary: false,
					profile_flags: null,
					version: 1,
				}),
			);
			batch.addPrepared(GuildMembersByUserId.insert({user_id: user.id, guild_id: guildId}));

			await batch.execute();

			const guild = new Guild(guildData);

			await this.gatewayService.startGuild(guildId);
			await this.gatewayService.joinGuild({userId: user.id, guildId});

			if (!user.isBot) {
				const userSettings = await this.userRepository.findSettings(user.id);
				if (userSettings) {
					const settingsRow = userSettings.toRow();
					const existingFolders = settingsRow.guild_folders ?? [];
					const uncategorizedIndex = existingFolders.findIndex(
						(folder) => folder.folder_id === UNCATEGORIZED_FOLDER_ID,
					);

					if (uncategorizedIndex !== -1) {
						const uncategorizedFolder = existingFolders[uncategorizedIndex];
						const updatedGuildIds = [guildId, ...(uncategorizedFolder.guild_ids ?? [])];
						existingFolders[uncategorizedIndex] = {
							...uncategorizedFolder,
							guild_ids: updatedGuildIds,
						};
					} else {
						existingFolders.push({
							folder_id: UNCATEGORIZED_FOLDER_ID,
							name: null,
							color: null,
							flags: 0,
							icon: DEFAULT_GUILD_FOLDER_ICON,
							guild_ids: [guildId],
						});
					}

					settingsRow.guild_folders = existingFolders;
					const updatedSettings = await this.userRepository.upsertSettings(settingsRow);
					const guildIds = await this.userRepository.getUserGuildIds(user.id);
					await this.gatewayService.dispatchPresence({
						userId: user.id,
						event: 'USER_SETTINGS_UPDATE',
						data: mapUserSettingsToResponse({settings: updatedSettings, memberGuildIds: guildIds}),
					});
				}
			}

			const guildSearchService = getGuildSearchService();
			if (guildSearchService) {
				await guildSearchService.indexGuild(guild).catch((error) => {
					Logger.error({guildId: guild.id, error}, 'Failed to index guild in search');
				});
			}

			getMetricsService().counter({
				name: 'fluxer.guilds.created',
				dimensions: {
					guild_id: guildId.toString(),
				},
			});

			return mapGuildToGuildResponse(guild);
		} catch (error) {
			getMetricsService().counter({name: 'guild.create.error'});
			throw error;
		}
	}

	async updateGuild(
		params: {userId: UserID; guildId: GuildID; data: GuildUpdateRequest; requestCache: RequestCache},
		auditLogReason?: string | null,
	): Promise<GuildResponse> {
		const {userId, guildId, data} = params;
		const {checkPermission, guildData} = await this.helpers.getGuildAuthenticated({userId, guildId});
		await checkPermission(Permissions.MANAGE_GUILD);

		const currentGuild = await this.guildRepository.findUnique(guildId);
		if (!currentGuild) throw new UnknownGuildError();

		const previousSnapshot = this.helpers.serializeGuildForAudit(currentGuild);

		if (data.mfa_level !== undefined) {
			const isOwner = guildData.owner_id === userId.toString();
			if (!isOwner) {
				throw new MissingPermissionsError();
			}

			const owner = await this.userRepository.findUniqueAssert(userId);
			if (owner.authenticatorTypes.size === 0) {
				throw InputValidationError.fromCode(
					'mfa_level',
					ValidationErrorCodes.MUST_ENABLE_2FA_BEFORE_REQUIRING_FOR_MODS,
				);
			}
		}

		const preparedAssets: PreparedGuildAssets = {icon: null, banner: null, splash: null, embed_splash: null};

		let iconHash = currentGuild.iconHash;
		if (data.icon !== undefined) {
			preparedAssets.icon = await this.entityAssetService.prepareAssetUpload({
				assetType: 'icon',
				entityType: 'guild',
				entityId: guildId,
				previousHash: currentGuild.iconHash,
				base64Image: data.icon,
				errorPath: 'icon',
			});
			iconHash = preparedAssets.icon.newHash;
		}

		let bannerHash = currentGuild.bannerHash;
		let bannerHeight = currentGuild.bannerHeight;
		let bannerWidth = currentGuild.bannerWidth;
		if (data.banner !== undefined) {
			if (data.banner && !currentGuild.features.has(GuildFeatures.BANNER)) {
				await this.rollbackPreparedAssets(preparedAssets);
				throw InputValidationError.fromCode('banner', ValidationErrorCodes.GUILD_BANNER_REQUIRES_FEATURE);
			}

			try {
				preparedAssets.banner = await this.entityAssetService.prepareAssetUpload({
					assetType: 'banner',
					entityType: 'guild',
					entityId: guildId,
					previousHash: currentGuild.bannerHash,
					base64Image: data.banner,
					errorPath: 'banner',
				});

				if (preparedAssets.banner.isAnimated && !currentGuild.features.has(GuildFeatures.ANIMATED_BANNER)) {
					await this.rollbackPreparedAssets(preparedAssets);
					throw InputValidationError.fromCode('banner', ValidationErrorCodes.ANIMATED_GUILD_BANNER_REQUIRES_FEATURE);
				}

				bannerHash = preparedAssets.banner.newHash;
				bannerHeight =
					preparedAssets.banner.newHash === currentGuild.bannerHash && bannerHeight != null
						? bannerHeight
						: (preparedAssets.banner.height ?? null);
				bannerWidth =
					preparedAssets.banner.newHash === currentGuild.bannerHash && bannerWidth != null
						? bannerWidth
						: (preparedAssets.banner.width ?? null);
			} catch (error) {
				await this.rollbackPreparedAssets(preparedAssets);
				throw error;
			}
		} else if (data.banner === null) {
			bannerHeight = null;
			bannerWidth = null;
		}

		let splashHash = currentGuild.splashHash;
		let splashWidth = currentGuild.splashWidth;
		let splashHeight = currentGuild.splashHeight;
		if (data.splash !== undefined) {
			if (data.splash && !currentGuild.features.has(GuildFeatures.INVITE_SPLASH)) {
				await this.rollbackPreparedAssets(preparedAssets);
				throw InputValidationError.fromCode('splash', ValidationErrorCodes.INVITE_SPLASH_REQUIRES_FEATURE);
			}

			try {
				preparedAssets.splash = await this.entityAssetService.prepareAssetUpload({
					assetType: 'splash',
					entityType: 'guild',
					entityId: guildId,
					previousHash: currentGuild.splashHash,
					base64Image: data.splash,
					errorPath: 'splash',
				});
				splashHash = preparedAssets.splash.newHash;
				splashHeight =
					preparedAssets.splash.newHash === currentGuild.splashHash && splashHeight != null
						? splashHeight
						: (preparedAssets.splash.height ?? null);
				splashWidth =
					preparedAssets.splash.newHash === currentGuild.splashHash && splashWidth != null
						? splashWidth
						: (preparedAssets.splash.width ?? null);
			} catch (error) {
				await this.rollbackPreparedAssets(preparedAssets);
				throw error;
			}
		} else if (data.splash === null) {
			splashHash = null;
			splashWidth = null;
			splashHeight = null;
		}

		let embedSplashHash = currentGuild.embedSplashHash;
		let embedSplashWidth = currentGuild.embedSplashWidth;
		let embedSplashHeight = currentGuild.embedSplashHeight;
		if (data.embed_splash !== undefined) {
			if (data.embed_splash && !currentGuild.features.has(GuildFeatures.INVITE_SPLASH)) {
				await this.rollbackPreparedAssets(preparedAssets);
				throw InputValidationError.fromCode('embed_splash', ValidationErrorCodes.EMBED_SPLASH_REQUIRES_FEATURE);
			}

			try {
				preparedAssets.embed_splash = await this.entityAssetService.prepareAssetUpload({
					assetType: 'embed_splash',
					entityType: 'guild',
					entityId: guildId,
					previousHash: currentGuild.embedSplashHash,
					base64Image: data.embed_splash,
					errorPath: 'embed_splash',
				});
				embedSplashHash = preparedAssets.embed_splash.newHash;
				embedSplashHeight =
					preparedAssets.embed_splash.newHash === currentGuild.embedSplashHash && embedSplashHeight != null
						? embedSplashHeight
						: (preparedAssets.embed_splash.height ?? null);
				embedSplashWidth =
					preparedAssets.embed_splash.newHash === currentGuild.embedSplashHash && embedSplashWidth != null
						? embedSplashWidth
						: (preparedAssets.embed_splash.width ?? null);
			} catch (error) {
				await this.rollbackPreparedAssets(preparedAssets);
				throw error;
			}
		} else if (data.embed_splash === null) {
			embedSplashHash = null;
			embedSplashWidth = null;
			embedSplashHeight = null;
		}

		let afkChannelId: ChannelID | null | undefined;
		if (data.afk_channel_id !== undefined) {
			if (data.afk_channel_id) {
				afkChannelId = createChannelID(data.afk_channel_id);
				const afkChannel = await this.channelRepository.findUnique(afkChannelId);
				if (!afkChannel || afkChannel.guildId !== guildId) {
					throw InputValidationError.fromCode('afk_channel_id', ValidationErrorCodes.AFK_CHANNEL_MUST_BE_IN_GUILD);
				}
				if (afkChannel.type !== ChannelTypes.GUILD_VOICE) {
					throw InputValidationError.fromCode('afk_channel_id', ValidationErrorCodes.AFK_CHANNEL_MUST_BE_VOICE);
				}
			} else {
				afkChannelId = null;
			}
		}

		let systemChannelId: ChannelID | null | undefined;
		if (data.system_channel_id !== undefined) {
			if (data.system_channel_id) {
				systemChannelId = createChannelID(data.system_channel_id);
				const systemChannel = await this.channelRepository.findUnique(systemChannelId);
				if (!systemChannel || systemChannel.guildId !== guildId) {
					throw InputValidationError.fromCode(
						'system_channel_id',
						ValidationErrorCodes.SYSTEM_CHANNEL_MUST_BE_IN_GUILD,
					);
				}
				if (systemChannel.type !== ChannelTypes.GUILD_TEXT) {
					throw InputValidationError.fromCode('system_channel_id', ValidationErrorCodes.SYSTEM_CHANNEL_MUST_BE_TEXT);
				}
			} else {
				systemChannelId = null;
			}
		}

		let sanitizedSystemChannelFlags = currentGuild.systemChannelFlags;
		if (data.system_channel_flags !== undefined) {
			const SUPPORTED_SYSTEM_CHANNEL_FLAGS = SystemChannelFlags.SUPPRESS_JOIN_NOTIFICATIONS;
			sanitizedSystemChannelFlags = data.system_channel_flags & SUPPORTED_SYSTEM_CHANNEL_FLAGS;
		}

		let updatedFeatures = currentGuild.features;
		if (data.features !== undefined) {
			const newFeatures = new Set(currentGuild.features);

			const owner = await this.userRepository.findUnique(currentGuild.ownerId);
			const isOwnerUnclaimed = owner && !owner.passwordHash;

			const toggleableFeatures = [
				GuildFeatures.INVITES_DISABLED,
				GuildFeatures.TEXT_CHANNEL_FLEXIBLE_NAMES,
				GuildFeatures.DETACHED_BANNER,
			];

			for (const feature of toggleableFeatures) {
				if (feature === GuildFeatures.INVITES_DISABLED && isOwnerUnclaimed) {
					newFeatures.add(feature);
					continue;
				}

				if (data.features.includes(feature)) {
					newFeatures.add(feature);
				} else {
					newFeatures.delete(feature);
				}
			}

			updatedFeatures = newFeatures;
		}

		let messageHistoryCutoff: Date | null | undefined;
		if (data.message_history_cutoff !== undefined) {
			if (data.message_history_cutoff === null) {
				messageHistoryCutoff = null;
			} else {
				const cutoffDate = new Date(data.message_history_cutoff);
				const guildCreationTimestamp = extractTimestamp(guildId.toString());
				if (cutoffDate.getTime() < guildCreationTimestamp) {
					throw InputValidationError.fromCode(
						'message_history_cutoff',
						ValidationErrorCodes.MESSAGE_HISTORY_CUTOFF_BEFORE_GUILD_CREATION,
					);
				}
				if (cutoffDate.getTime() > Date.now()) {
					throw InputValidationError.fromCode(
						'message_history_cutoff',
						ValidationErrorCodes.MESSAGE_HISTORY_CUTOFF_IN_FUTURE,
					);
				}
				messageHistoryCutoff = cutoffDate;
			}
		}

		const currentGuildRow = currentGuild.toRow();
		const splashCardAlignment: GuildSplashCardAlignmentValue =
			data.splash_card_alignment ?? currentGuildRow.splash_card_alignment ?? GuildSplashCardAlignment.CENTER;
		const upsertData = {
			...currentGuildRow,
			name: data.name ?? currentGuildRow.name,
			icon_hash: iconHash,
			banner_hash: bannerHash,
			banner_width: bannerWidth,
			banner_height: bannerHeight,
			splash_hash: splashHash,
			splash_width: splashWidth,
			splash_height: splashHeight,
			splash_card_alignment: splashCardAlignment,
			embed_splash_hash: embedSplashHash,
			embed_splash_width: embedSplashWidth,
			embed_splash_height: embedSplashHeight,
			features: updatedFeatures,
			system_channel_id: systemChannelId !== undefined ? systemChannelId : currentGuildRow.system_channel_id,
			system_channel_flags: sanitizedSystemChannelFlags,
			afk_channel_id: afkChannelId !== undefined ? afkChannelId : currentGuildRow.afk_channel_id,
			afk_timeout: data.afk_timeout ?? currentGuildRow.afk_timeout,
			default_message_notifications:
				data.default_message_notifications ?? currentGuildRow.default_message_notifications,
			verification_level: data.verification_level ?? currentGuildRow.verification_level,
			mfa_level: data.mfa_level ?? currentGuildRow.mfa_level,
			nsfw_level: data.nsfw_level ?? currentGuildRow.nsfw_level,
			explicit_content_filter: data.explicit_content_filter ?? currentGuildRow.explicit_content_filter,
			message_history_cutoff:
				messageHistoryCutoff !== undefined ? messageHistoryCutoff : currentGuildRow.message_history_cutoff,
		};
		let updatedGuild: Guild;
		try {
			updatedGuild = await this.guildRepository.upsert(upsertData);
		} catch (error) {
			await this.rollbackPreparedAssets(preparedAssets);
			Logger.error({error, guildId}, 'Guild update failed, rolled back asset uploads');
			throw error;
		}

		try {
			await this.commitPreparedAssets(preparedAssets);
		} catch (error) {
			Logger.error({error, guildId}, 'Failed to commit asset changes after successful guild update');
		}

		await this.helpers.dispatchGuildUpdate(updatedGuild);

		const guildSearchService = getGuildSearchService();
		if (guildSearchService) {
			let discoveryContext: GuildDiscoveryContext | undefined;
			if (updatedGuild.features.has(GuildFeatures.DISCOVERABLE)) {
				const discoveryRow = await this.discoveryRepository.findByGuildId(updatedGuild.id).catch(() => null);
				if (discoveryRow) {
					discoveryContext = {
						description: discoveryRow.description,
						categoryId: discoveryRow.category_type,
					};
				}
			}
			await guildSearchService.updateGuild(updatedGuild, discoveryContext).catch((error) => {
				Logger.error({guildId: updatedGuild.id, error}, 'Failed to update guild in search');
			});
		}

		await this.helpers.recordAuditLog({
			guildId,
			userId,
			action: AuditLogActionType.GUILD_UPDATE,
			targetId: guildId,
			auditLogReason: auditLogReason ?? null,
			metadata: {name: updatedGuild.name},
			changes: this.helpers.computeGuildChanges(previousSnapshot, updatedGuild),
		});

		if (data.name !== undefined && currentGuild.name !== updatedGuild.name) {
			getMetricsService().counter({
				name: 'fluxer.guilds.updated',
				dimensions: {
					guild_id: guildId.toString(),
					update_type: 'name',
				},
			});
		}
		if (data.icon !== undefined && currentGuild.iconHash !== updatedGuild.iconHash) {
			getMetricsService().counter({
				name: 'fluxer.guilds.updated',
				dimensions: {
					guild_id: guildId.toString(),
					update_type: 'icon',
				},
			});
		}
		if (data.banner !== undefined && currentGuild.bannerHash !== updatedGuild.bannerHash) {
			getMetricsService().counter({
				name: 'fluxer.guilds.updated',
				dimensions: {
					guild_id: guildId.toString(),
					update_type: 'banner',
				},
			});
		}

		return mapGuildToGuildResponse(updatedGuild);
	}

	private async rollbackPreparedAssets(assets: PreparedGuildAssets): Promise<void> {
		const rollbackPromises: Array<Promise<void>> = [];

		if (assets.icon) {
			rollbackPromises.push(this.entityAssetService.rollbackAssetUpload(assets.icon));
		}
		if (assets.banner) {
			rollbackPromises.push(this.entityAssetService.rollbackAssetUpload(assets.banner));
		}
		if (assets.splash) {
			rollbackPromises.push(this.entityAssetService.rollbackAssetUpload(assets.splash));
		}
		if (assets.embed_splash) {
			rollbackPromises.push(this.entityAssetService.rollbackAssetUpload(assets.embed_splash));
		}

		await Promise.all(rollbackPromises);
	}

	private async commitPreparedAssets(assets: PreparedGuildAssets): Promise<void> {
		const commitPromises: Array<Promise<void>> = [];

		if (assets.icon) {
			commitPromises.push(this.entityAssetService.commitAssetChange({prepared: assets.icon, deferDeletion: true}));
		}
		if (assets.banner) {
			commitPromises.push(this.entityAssetService.commitAssetChange({prepared: assets.banner, deferDeletion: true}));
		}
		if (assets.splash) {
			commitPromises.push(this.entityAssetService.commitAssetChange({prepared: assets.splash, deferDeletion: true}));
		}
		if (assets.embed_splash) {
			commitPromises.push(
				this.entityAssetService.commitAssetChange({prepared: assets.embed_splash, deferDeletion: true}),
			);
		}

		await Promise.all(commitPromises);
	}

	async deleteGuild(params: {user: User; guildId: GuildID}, _auditLogReason?: string | null): Promise<void> {
		const {user, guildId} = params;
		const {guildData} = await this.helpers.getGuildAuthenticated({userId: user.id, guildId});
		if (!guildData || guildData.owner_id !== user.id.toString()) {
			throw new MissingPermissionsError();
		}

		await this.performGuildDeletion(guildId);
	}

	async deleteGuildById(guildId: GuildID): Promise<void> {
		await this.performGuildDeletion(guildId);
	}

	private async performGuildDeletion(guildId: GuildID): Promise<void> {
		return await withSpan(
			{
				name: 'fluxer.guild.delete',
				attributes: {},
			},
			async () => {
				try {
					const guild = await this.guildRepository.findUnique(guildId);
					if (!guild) {
						throw new UnknownGuildError();
					}

					const members = await this.guildRepository.listMembers(guildId);

					await this.gatewayService.dispatchGuild({
						guildId,
						event: 'GUILD_DELETE',
						data: {id: guildId.toString()},
					});

					await Promise.all(
						members.map(async (member) => {
							await this.gatewayService.leaveGuild({userId: member.userId, guildId});
						}),
					);

					await Promise.all(members.map((member) => this.userRepository.deleteGuildSettings(member.userId, guildId)));

					await Promise.all(
						members.map(async (member) => {
							const user = await this.userRepository.findUnique(member.userId);
							if (user && !user.isBot) {
								await removeGuildFromUserFolders({
									userId: member.userId,
									guildId,
									userRepository: this.userRepository,
									gatewayService: this.gatewayService,
								});
							}
						}),
					);

					const invites = await this.inviteRepository.listGuildInvites(guildId);
					await Promise.all(invites.map((invite) => this.inviteRepository.delete(invite.code)));

					const webhooks = await this.webhookRepository.listByGuild(guildId);
					await Promise.all(webhooks.map((webhook) => this.webhookRepository.delete(webhook.id)));

					const channels = await this.channelRepository.listGuildChannels(guildId);

					await Promise.all(channels.map((channel) => this.channelRepository.deleteAllChannelMessages(channel.id)));
					await Promise.all(channels.map((channel) => this.channelService.purgeChannelAttachments(channel)));

					await this.guildRepository.delete(guildId, guild.ownerId);
					await this.gatewayService.stopGuild(guildId);

					const guildSearchService = getGuildSearchService();
					if (guildSearchService) {
						await guildSearchService.deleteGuild(guildId).catch((error) => {
							Logger.error({guildId, error}, 'Failed to delete guild from search');
						});
					}

					getMetricsService().counter({
						name: 'fluxer.guilds.deleted',
						dimensions: {
							guild_id: guildId.toString(),
						},
					});
				} catch (error) {
					getMetricsService().counter({name: 'guild.delete.error'});
					throw error;
				}
			},
		);
	}

	private isGuildAccessError(error: unknown): boolean {
		return error instanceof UnknownGuildError;
	}

	private async guildExists(guildId: GuildID): Promise<boolean> {
		const guild = await this.guildRepository.findUnique(guildId);
		return guild !== null;
	}
}
