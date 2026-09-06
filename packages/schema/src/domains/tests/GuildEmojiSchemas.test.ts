// SPDX-License-Identifier: AGPL-3.0-or-later

import {MAX_GUILD_STICKER_TAGS} from '@fluxer/constants/src/LimitConstants';
import {GuildStickerResponse} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import {describe, expect, it} from 'vitest';

const buildSticker = (tagCount: number) => ({
	id: '1234567890123456789',
	name: 'sticker',
	description: 'A sticker',
	tags: Array.from({length: tagCount}, (_, index) => `tag${index}`),
	animated: false,
	nsfw: false,
});

describe('GuildStickerResponse', () => {
	it('accepts the maximum tag count', () => {
		expect(GuildStickerResponse.safeParse(buildSticker(MAX_GUILD_STICKER_TAGS)).success).toBe(true);
	});
	it('rejects one tag beyond the maximum', () => {
		expect(GuildStickerResponse.safeParse(buildSticker(MAX_GUILD_STICKER_TAGS + 1)).success).toBe(false);
	});
});
