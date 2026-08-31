// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelTypes, GUILD_TEXT_BASED_CHANNEL_TYPES} from '@fluxer/constants/src/ChannelConstants';
import {GuildExplicitContentFilterTypes, GuildFeatures, GuildNSFWLevel} from '@fluxer/constants/src/GuildConstants';
import {SensitiveMediaFilterLevel} from '@fluxer/constants/src/UserConstants';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import type {GuildID, UserID, WebhookID} from '../../../BrandedTypes';
import type {IGuildRepositoryAggregate} from '../../../guild/repositories/IGuildRepositoryAggregate';
import type {LimitConfigService} from '../../../limits/LimitConfigService';
import type {Channel} from '../../../models/Channel';
import type {IUserRepository} from '../../../user/IUserRepository';
import * as EmojiUtils from '../../../utils/EmojiUtils';

export interface DmNsfwContext {
	senderFilterLevel: number;
	recipientFilterLevel: number;
}

export class MessageContentService {
	constructor(
		private userRepository: IUserRepository,
		private guildRepository: IGuildRepositoryAggregate,
		private limitConfigService: LimitConfigService,
	) {}

	async sanitizeCustomEmojis(params: {
		content: string;
		userId: UserID | null;
		webhookId: WebhookID | null;
		guildId: GuildID | null;
		hasPermission?: (permission: bigint) => Promise<boolean>;
	}): Promise<string> {
		return await EmojiUtils.sanitizeCustomEmojis({
			...params,
			userRepository: this.userRepository,
			guildRepository: this.guildRepository,
			limitConfigService: this.limitConfigService,
		});
	}

	isNSFWContentAllowed(params: {
		channel?: Channel;
		guild?: GuildResponse | null;
		member?: GuildMemberResponse | null;
		isBot?: boolean;
		dmNsfwContext?: DmNsfwContext;
	}): boolean {
		const {channel, guild, member, isBot, dmNsfwContext} = params;
		if (isBot) {
			return true;
		}
		if (channel && GUILD_TEXT_BASED_CHANNEL_TYPES.has(channel.type) && channel.isNsfw) {
			return true;
		}
		if (channel?.type === ChannelTypes.DM_PERSONAL_NOTES) {
			return true;
		}
		if (!guild) {
			if (dmNsfwContext) {
				const eitherBlocks =
					dmNsfwContext.senderFilterLevel === SensitiveMediaFilterLevel.BLOCK ||
					dmNsfwContext.recipientFilterLevel === SensitiveMediaFilterLevel.BLOCK;
				return !eitherBlocks;
			}
			return false;
		}
		const guildMarkedNsfw = guild.nsfw_level === GuildNSFWLevel.AGE_RESTRICTED;
		if (guildMarkedNsfw) {
			return true;
		}
		const features = new Set(guild.features ?? []);
		if (features.has(GuildFeatures.DISCOVERABLE)) {
			return false;
		}
		const explicitContentFilter = guild.explicit_content_filter;
		if (explicitContentFilter === GuildExplicitContentFilterTypes.DISABLED) {
			return true;
		}
		if (explicitContentFilter === GuildExplicitContentFilterTypes.MEMBERS_WITHOUT_ROLES) {
			const hasRoles = member && member.roles.length > 0;
			return !!hasRoles;
		}
		return false;
	}
}
