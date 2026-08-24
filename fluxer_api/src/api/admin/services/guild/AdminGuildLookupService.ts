// SPDX-License-Identifier: AGPL-3.0-or-later

import {MEDIA_PROXY_ICON_SIZE_DEFAULT} from '@fluxer/constants/src/MediaProxyAssetSizes';
import type {MediaProxyImageSize} from '@fluxer/constants/src/MediaProxyImageSizes';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import type {
	ListGuildMembersRequest,
	ListUserGuildsRequest,
	LookupGuildRequest,
} from '@fluxer/schema/src/domains/admin/AdminGuildSchemas';
import type {ListGuildEmojisResponse, ListGuildStickersResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {GuildID} from '../../../BrandedTypes';
import {createGuildID, createUserID} from '../../../BrandedTypes';
import {Config} from '../../../Config';
import type {IChannelRepository} from '../../../channel/IChannelRepository';
import {mapGuildFeatures} from '../../../guild/GuildFeatureUtils';
import type {IGuildRepositoryAggregate} from '../../../guild/repositories/IGuildRepositoryAggregate';
import type {IGatewayService} from '../../../infrastructure/IGatewayService';
import type {IUserRepository} from '../../../user/IUserRepository';
import {mapGuildsToAdminResponse} from '../../models/GuildTypes';

const ADMIN_STICKER_MEDIA_RUNG: MediaProxyImageSize = 320;

interface AdminGuildLookupServiceDeps {
	guildRepository: IGuildRepositoryAggregate;
	userRepository: IUserRepository;
	channelRepository: IChannelRepository;
	gatewayService: IGatewayService;
}

export class AdminGuildLookupService {
	constructor(private readonly deps: AdminGuildLookupServiceDeps) {}

	async lookupGuild(data: LookupGuildRequest) {
		const {guildRepository, channelRepository, userRepository} = this.deps;
		const guildId = createGuildID(data.guild_id);
		const guild = await guildRepository.findUnique(guildId);
		if (!guild) {
			return {guild: null};
		}
		const [channels, roles, ownerUser] = await Promise.all([
			channelRepository.listGuildChannels(guildId),
			guildRepository.listRoles(guildId),
			userRepository.findUnique(guild.ownerId),
		]);
		return {
			guild: {
				id: guild.id.toString(),
				owner_id: guild.ownerId.toString(),
				owner_username: ownerUser?.username ?? null,
				owner_global_name: ownerUser?.globalName ?? null,
				owner_discriminator: ownerUser ? String(ownerUser.discriminator).padStart(4, '0') : null,
				name: guild.name,
				vanity_url_code: guild.vanityUrlCode,
				icon: guild.iconHash,
				banner: guild.bannerHash,
				splash: guild.splashHash,
				embed_splash: guild.embedSplashHash,
				features: mapGuildFeatures(guild.features),
				verification_level: guild.verificationLevel,
				mfa_level: guild.mfaLevel,
				nsfw_level: guild.nsfwLevel,
				explicit_content_filter: guild.explicitContentFilter,
				default_message_notifications: guild.defaultMessageNotifications,
				afk_channel_id: guild.afkChannelId?.toString() ?? null,
				afk_timeout: guild.afkTimeout,
				system_channel_id: guild.systemChannelId?.toString() ?? null,
				system_channel_flags: guild.systemChannelFlags,
				rules_channel_id: guild.rulesChannelId?.toString() ?? null,
				disabled_operations: guild.disabledOperations,
				member_count: guild.memberCount,
				channels: channels.map((c) => ({
					id: c.id.toString(),
					name: c.name,
					type: c.type,
					position: c.position,
					parent_id: c.parentId?.toString() ?? null,
					nsfw: c.isNsfw,
					url: c.url,
				})),
				roles: roles.map((r) => ({
					id: r.id.toString(),
					name: r.name,
					color: r.color,
					position: r.position,
					permissions: r.permissions.toString(),
					hoist: r.isHoisted,
					mentionable: r.isMentionable,
				})),
			},
		};
	}

	async listUserGuilds(data: ListUserGuildsRequest) {
		const {userRepository, guildRepository, gatewayService} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}
		let guildIds = await userRepository.getUserGuildIds(userId);
		guildIds.sort((a, b) => {
			if (a < b) return -1;
			if (a > b) return 1;
			return 0;
		});
		if (data.after != null) {
			const afterId = createGuildID(data.after);
			const afterIndex = guildIds.indexOf(afterId);
			if (afterIndex !== -1) {
				guildIds = guildIds.slice(afterIndex + 1);
			}
		}
		if (data.before != null) {
			const beforeId = createGuildID(data.before);
			const beforeIndex = guildIds.indexOf(beforeId);
			if (beforeIndex !== -1) {
				guildIds = guildIds.slice(0, beforeIndex);
			}
		}
		const limit = data.limit ?? 200;
		guildIds = guildIds.slice(0, limit);
		const guilds = await guildRepository.listGuilds(guildIds);
		const ownerIds = [...new Set(guilds.map((g) => g.ownerId))];
		const ownerUsers = await Promise.all(ownerIds.map((id) => userRepository.findUnique(id)));
		const ownerMap = new Map<string, NonNullable<(typeof ownerUsers)[number]>>();
		for (let i = 0; i < ownerIds.length; i++) {
			const owner = ownerUsers[i];
			if (owner) {
				ownerMap.set(ownerIds[i].toString(), owner);
			}
		}
		const result = mapGuildsToAdminResponse(guilds, ownerMap);
		if (data.with_counts) {
			const countsPromises = guilds.map((g) => gatewayService.getGuildCounts(g.id));
			const counts = await Promise.all(countsPromises);
			return {
				guilds: result.guilds.map((g, i) => ({
					...g,
					approximate_member_count: counts[i].memberCount,
					approximate_presence_count: counts[i].presenceCount,
				})),
			};
		}
		return result;
	}

	async listGuildMembers(data: ListGuildMembersRequest) {
		const {gatewayService} = this.deps;
		const guildId = createGuildID(data.guild_id);
		const limit = data.limit ?? 50;
		const offset = data.offset ?? 0;
		const result = await gatewayService.listGuildMembers({
			guildId,
			limit,
			offset,
		});
		return {
			members: result.members,
			total: result.total,
			limit,
			offset,
		};
	}

	async listGuildEmojis(guildId: GuildID): Promise<ListGuildEmojisResponse> {
		const {guildRepository} = this.deps;
		const emojis = await guildRepository.listEmojis(guildId);
		return {
			guild_id: guildId.toString(),
			emojis: emojis.map((emoji) => {
				const emojiId = emoji.id.toString();
				return {
					id: emojiId,
					name: emoji.name,
					animated: emoji.isAnimated,
					creator_id: emoji.creatorId.toString(),
					media_url: this.buildEmojiMediaUrl(emojiId, emoji.isAnimated),
				};
			}),
		};
	}

	async listGuildStickers(guildId: GuildID): Promise<ListGuildStickersResponse> {
		const {guildRepository} = this.deps;
		const stickers = await guildRepository.listStickers(guildId);
		return {
			guild_id: guildId.toString(),
			stickers: stickers.map((sticker) => {
				const stickerId = sticker.id.toString();
				return {
					id: stickerId,
					name: sticker.name,
					animated: sticker.animated,
					creator_id: sticker.creatorId.toString(),
					media_url: this.buildStickerMediaUrl(stickerId, sticker.animated),
				};
			}),
		};
	}

	private buildEmojiMediaUrl(id: string, animated: boolean): string {
		return `${Config.endpoints.media}/emojis/${id}.webp?size=${MEDIA_PROXY_ICON_SIZE_DEFAULT}${animated ? '&animated=true' : ''}`;
	}

	private buildStickerMediaUrl(id: string, animated: boolean): string {
		return `${Config.endpoints.media}/stickers/${id}.webp?size=${ADMIN_STICKER_MEDIA_RUNG}${animated ? '&animated=true' : ''}`;
	}
}
