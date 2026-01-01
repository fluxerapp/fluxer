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

import {createUserID} from '~/BrandedTypes';

export const FLUXER_EPOCH = 1420070400000;
export const FLUXER_USER_AGENT = 'Mozilla/5.0 (compatible; Fluxerbot/1.0; +https://fluxer.app)';

export const MAX_GUILDS_PREMIUM = 200;
export const MAX_GUILDS_NON_PREMIUM = 100;
export const MAX_GUILD_CHANNELS = 500;
export const MAX_CHANNELS_PER_CATEGORY = 50;
export const MAX_GUILD_EMOJIS_ANIMATED = 50;
export const MAX_GUILD_EMOJIS_STATIC = 50;
export const MAX_GUILD_EMOJIS_ANIMATED_MORE_EMOJI = 250;
export const MAX_GUILD_EMOJIS_STATIC_MORE_EMOJI = 250;
export const MAX_GUILD_STICKERS = 50;
export const MAX_GUILD_STICKERS_MORE_STICKERS = 250;
export const MAX_GUILD_INVITES = 1000;
export const MAX_GUILD_ROLES = 250;

export const MAX_WEBHOOKS_PER_CHANNEL = 15;
export const MAX_WEBHOOKS_PER_GUILD = 1000;

export const MAX_MESSAGE_LENGTH_PREMIUM = 4000;
export const MAX_MESSAGE_LENGTH_NON_PREMIUM = 2000;
export const MAX_ATTACHMENTS_PER_MESSAGE = 10;
export const MAX_EMBEDS_PER_MESSAGE = 10;
export const MAX_REACTIONS_PER_MESSAGE = 30;
export const MAX_USERS_PER_MESSAGE_REACTION = 5000;

export const MAX_RELATIONSHIPS = 1000;
export const MAX_GROUP_DM_RECIPIENTS = 10;
export const MAX_PRIVATE_CHANNELS_PER_USER = 250;
export const MAX_GROUP_DMS_PER_USER = 150;
export const MAX_BOOKMARKS_PREMIUM = 300;
export const MAX_BOOKMARKS_NON_PREMIUM = 50;
export const MAX_FAVORITE_MEMES_PREMIUM = 500;
export const MAX_FAVORITE_MEMES_NON_PREMIUM = 50;
export const MAX_FAVORITE_MEME_TAGS = 10;
export const MAX_PACK_EXPRESSIONS = 200;
export const MAX_CREATED_PACKS_NON_PREMIUM = 0;
export const MAX_CREATED_PACKS_PREMIUM = 50;
export const MAX_INSTALLED_PACKS_NON_PREMIUM = 0;
export const MAX_INSTALLED_PACKS_PREMIUM = 50;

export const AVATAR_MAX_SIZE = 10 * 1024 * 1024;
export const AVATAR_EXTENSIONS = new Set(['jpeg', 'png', 'webp', 'gif']);

export const EMOJI_MAX_SIZE = 384 * 1024;
export const EMOJI_EXTENSIONS = new Set(['jpeg', 'png', 'webp', 'gif']);

export const STICKER_MAX_SIZE = 512 * 1024;
export const STICKER_EXTENSIONS = new Set(['png', 'gif']);

export const ATTACHMENT_MAX_SIZE_PREMIUM = 500 * 1024 * 1024;
export const ATTACHMENT_MAX_SIZE_NON_PREMIUM = 25 * 1024 * 1024;

export const USER_MENTION_REGEX = /<@!?(?<userId>\d+)>/g;
export const ROLE_MENTION_REGEX = /<@&(?<roleId>\d+)>/g;
export const URL_REGEX = /https?:\/\/[^\s/$.?#].[^\s]*/g;

export const VALID_TEMP_BAN_DURATIONS = new Set([
	60 * 60,
	60 * 60 * 12,
	60 * 60 * 24,
	60 * 60 * 24 * 3,
	60 * 60 * 24 * 5,
	60 * 60 * 24 * 7,
	60 * 60 * 24 * 14,
	60 * 60 * 24 * 30,
]);

export const DeletionReasons = {
	USER_REQUESTED: 1,
	OTHER: 2,
	SPAM: 3,
	HACKS_CHEATS: 4,
	RAIDS: 5,
	SELFBOT: 6,
	NONCONSENSUAL_PORNOGRAPHY: 7,
	SCAM: 8,
	LOLICON: 9,
	DOXXING: 10,
	HARASSMENT: 11,
	FRAUDULENT_CHARGE: 12,
	COPPA: 13,
	FRIENDLY_FRAUD: 14,
	UNSOLICITED_NSFW: 15,
	GORE: 16,
	BAN_EVASION: 17,
	TOKEN_SOLICITATION: 18,
	INACTIVITY: 19,
} as const;

export const SYSTEM_USER_ID = createUserID(0n);
