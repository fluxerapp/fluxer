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

import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {
	ATTACHMENT_MAX_SIZE_NON_PREMIUM,
	ATTACHMENT_MAX_SIZE_PREMIUM,
	AVATAR_MAX_SIZE,
	EMOJI_MAX_SIZE,
	MAX_ATTACHMENTS_PER_MESSAGE,
	MAX_BIO_LENGTH,
	MAX_BOOKMARKS_NON_PREMIUM,
	MAX_BOOKMARKS_PREMIUM,
	MAX_CHANNELS_PER_CATEGORY,
	MAX_CREATED_PACKS_NON_PREMIUM,
	MAX_CREATED_PACKS_PREMIUM,
	MAX_EMBEDS_PER_MESSAGE,
	MAX_FAVORITE_MEME_TAGS,
	MAX_FAVORITE_MEMES_NON_PREMIUM,
	MAX_FAVORITE_MEMES_PREMIUM,
	MAX_GROUP_DM_RECIPIENTS,
	MAX_GROUP_DMS_PER_USER,
	MAX_GUILD_CHANNELS,
	MAX_GUILD_EMOJIS_ANIMATED,
	MAX_GUILD_EMOJIS_ANIMATED_MORE_EMOJI,
	MAX_GUILD_EMOJIS_STATIC,
	MAX_GUILD_EMOJIS_STATIC_MORE_EMOJI,
	MAX_GUILD_INVITES,
	MAX_GUILD_MEMBERS,
	MAX_GUILD_ROLES,
	MAX_GUILD_STICKERS,
	MAX_GUILD_STICKERS_MORE_STICKERS,
	MAX_INSTALLED_PACKS_NON_PREMIUM,
	MAX_INSTALLED_PACKS_PREMIUM,
	MAX_MESSAGE_LENGTH_NON_PREMIUM,
	MAX_MESSAGE_LENGTH_PREMIUM,
	MAX_PACK_EXPRESSIONS,
	MAX_PRIVATE_CHANNELS_PER_USER,
	MAX_REACTIONS_PER_MESSAGE,
	MAX_RELATIONSHIPS,
	MAX_USERS_PER_MESSAGE_REACTION,
	MAX_VOICE_MESSAGE_DURATION,
	MAX_WEBHOOKS_PER_CHANNEL,
	MAX_WEBHOOKS_PER_GUILD,
	STICKER_MAX_SIZE,
} from '@fluxer/constants/src/LimitConstants';

export const DEFAULT_FREE_LIMITS: Record<LimitKey, number> = {
	avatar_max_size: AVATAR_MAX_SIZE,
	emoji_max_size: EMOJI_MAX_SIZE,
	feature_animated_avatar: 0,
	feature_animated_banner: 0,
	feature_custom_discriminator: 0,
	feature_custom_notification_sounds: 1,
	feature_early_access: 0,
	feature_global_expressions: 0,
	feature_higher_video_quality: 0,
	feature_per_guild_profiles: 0,
	feature_voice_entrance_sounds: 0,
	max_attachment_file_size: ATTACHMENT_MAX_SIZE_NON_PREMIUM,
	max_attachments_per_message: MAX_ATTACHMENTS_PER_MESSAGE,
	max_bio_length: MAX_BIO_LENGTH,
	max_bookmarks: MAX_BOOKMARKS_NON_PREMIUM,
	max_channels_per_category: MAX_CHANNELS_PER_CATEGORY,
	max_created_packs: MAX_CREATED_PACKS_NON_PREMIUM,
	max_custom_backgrounds: 1,
	max_embeds_per_message: MAX_EMBEDS_PER_MESSAGE,
	max_favorite_meme_tags: MAX_FAVORITE_MEME_TAGS,
	max_favorite_memes: MAX_FAVORITE_MEMES_NON_PREMIUM,
	max_group_dm_recipients: MAX_GROUP_DM_RECIPIENTS,
	max_group_dms_per_user: MAX_GROUP_DMS_PER_USER,
	max_guild_channels: MAX_GUILD_CHANNELS,
	max_guild_emojis_animated_more: MAX_GUILD_EMOJIS_ANIMATED_MORE_EMOJI,
	max_guild_emojis_animated: MAX_GUILD_EMOJIS_ANIMATED,
	max_guild_emojis_static_more: MAX_GUILD_EMOJIS_STATIC_MORE_EMOJI,
	max_guild_emojis_static: MAX_GUILD_EMOJIS_STATIC,
	max_guild_invites: MAX_GUILD_INVITES,
	max_guild_members: MAX_GUILD_MEMBERS,
	max_guild_roles: MAX_GUILD_ROLES,
	max_guild_stickers_more: MAX_GUILD_STICKERS_MORE_STICKERS,
	max_guild_stickers: MAX_GUILD_STICKERS,
	max_guilds: 100,
	max_installed_packs: MAX_INSTALLED_PACKS_NON_PREMIUM,
	max_message_length: MAX_MESSAGE_LENGTH_NON_PREMIUM,
	max_pack_expressions: MAX_PACK_EXPRESSIONS,
	max_private_channels_per_user: MAX_PRIVATE_CHANNELS_PER_USER,
	max_reactions_per_message: MAX_REACTIONS_PER_MESSAGE,
	max_relationships: MAX_RELATIONSHIPS,
	max_users_per_message_reaction: MAX_USERS_PER_MESSAGE_REACTION,
	max_voice_message_duration: MAX_VOICE_MESSAGE_DURATION,
	max_webhooks_per_channel: MAX_WEBHOOKS_PER_CHANNEL,
	max_webhooks_per_guild: MAX_WEBHOOKS_PER_GUILD,
	sticker_max_size: STICKER_MAX_SIZE,
};

export const DEFAULT_PREMIUM_LIMITS: Record<LimitKey, number> = {
	...DEFAULT_FREE_LIMITS,
	feature_animated_avatar: 1,
	feature_animated_banner: 1,
	feature_custom_discriminator: 1,
	feature_custom_notification_sounds: 1,
	feature_early_access: 1,
	feature_global_expressions: 1,
	feature_higher_video_quality: 1,
	feature_per_guild_profiles: 1,
	feature_voice_entrance_sounds: 1,
	max_attachment_file_size: ATTACHMENT_MAX_SIZE_PREMIUM,
	max_bio_length: MAX_BIO_LENGTH,
	max_bookmarks: MAX_BOOKMARKS_PREMIUM,
	max_created_packs: MAX_CREATED_PACKS_PREMIUM,
	max_custom_backgrounds: 15,
	max_favorite_memes: MAX_FAVORITE_MEMES_PREMIUM,
	max_guilds: 200,
	max_installed_packs: MAX_INSTALLED_PACKS_PREMIUM,
	max_message_length: MAX_MESSAGE_LENGTH_PREMIUM,
};
