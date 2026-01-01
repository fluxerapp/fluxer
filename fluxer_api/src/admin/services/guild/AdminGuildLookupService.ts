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

import type {GuildID} from '~/BrandedTypes';
import {createGuildID, createUserID} from '~/BrandedTypes';
import {Config} from '~/Config';
import type {IChannelRepository} from '~/channel/IChannelRepository';
import {StickerFormatTypes} from '~/constants/Guild';
import {UnknownUserError} from '~/Errors';
import type {IGuildRepository} from '~/guild/IGuildRepository';
import type {IGatewayService} from '~/infrastructure/IGatewayService';
import type {IUserRepository} from '~/user/IUserRepository';
import type {
	ListGuildEmojisResponse,
	ListGuildMembersRequest,
	ListGuildStickersResponse,
	ListUserGuildsRequest,
	LookupGuildRequest,
} from '../../AdminModel';
import {mapGuildsToAdminResponse} from '../../AdminModel';

interface AdminGuildLookupServiceDeps {
	guildRepository: IGuildRepository;
	userRepository: IUserRepository;
	channelRepository: IChannelRepository;
	gatewayService: IGatewayService;
}

export class AdminGuildLookupService {
	constructor(private readonly deps: AdminGuildLookupServiceDeps) {}

	async lookupGuild(data: LookupGuildRequest) {
		const {guildRepository, channelRepository} = this.deps;
		const guildId = createGuildID(data.guild_id);
		const guild = await guildRepository.findUnique(guildId);

		if (!guild) {
			return {guild: null};
		}

		const channels = await channelRepository.listGuildChannels(guildId);
		const roles = await guildRepository.listRoles(guildId);

		return {
			guild: {
				id: guild.id.toString(),
				owner_id: guild.ownerId.toString(),
				name: guild.name,
				vanity_url_code: guild.vanityUrlCode,
				icon: guild.iconHash,
				banner: guild.bannerHash,
				splash: guild.splashHash,
				features: Array.from(guild.features),
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
		const {userRepository, guildRepository} = this.deps;
		const userId = createUserID(data.user_id);
		const user = await userRepository.findUnique(userId);
		if (!user) {
			throw new UnknownUserError();
		}

		const guildIds = await userRepository.getUserGuildIds(userId);
		const guilds = await guildRepository.listGuilds(guildIds);

		return mapGuildsToAdminResponse(guilds);
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
					format_type: sticker.formatType,
					creator_id: sticker.creatorId.toString(),
					media_url: this.buildStickerMediaUrl(stickerId, sticker.formatType),
				};
			}),
		};
	}

	private buildEmojiMediaUrl(id: string, animated: boolean): string {
		const format = animated ? 'gif' : 'webp';
		return `${Config.endpoints.media}/emojis/${id}.${format}?size=160`;
	}

	private buildStickerMediaUrl(id: string, formatType: number): string {
		const ext = formatType === StickerFormatTypes.GIF ? 'gif' : 'webp';
		return `${Config.endpoints.media}/stickers/${id}.${ext}?size=160`;
	}
}
