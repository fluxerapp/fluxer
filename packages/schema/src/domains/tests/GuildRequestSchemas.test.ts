// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MAX_GUILD_ROLES,
	MAX_GUILD_STICKER_TAGS,
	MAX_TEMP_BAN_DURATION_SECONDS,
	MIN_TEMP_BAN_DURATION_SECONDS,
} from '@fluxer/constants/src/LimitConstants';
import {
	GuildBanCreateRequest,
	GuildMemberUpdateRequest,
	GuildStickerCreateRequest,
} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import {describe, expect, it} from 'vitest';

describe('GuildBanCreateRequest', () => {
	it('accepts permanent bans and arbitrary temporary durations within range', () => {
		expect(GuildBanCreateRequest.safeParse({ban_duration_seconds: 0}).success).toBe(true);
		expect(GuildBanCreateRequest.safeParse({ban_duration_seconds: MIN_TEMP_BAN_DURATION_SECONDS}).success).toBe(true);
		expect(GuildBanCreateRequest.safeParse({ban_duration_seconds: 3601}).success).toBe(true);
		expect(GuildBanCreateRequest.safeParse({ban_duration_seconds: MAX_TEMP_BAN_DURATION_SECONDS}).success).toBe(true);
	});
	it('rejects temporary ban durations outside the allowed range', () => {
		expect(GuildBanCreateRequest.safeParse({ban_duration_seconds: MIN_TEMP_BAN_DURATION_SECONDS - 1}).success).toBe(
			false,
		);
		expect(GuildBanCreateRequest.safeParse({ban_duration_seconds: MAX_TEMP_BAN_DURATION_SECONDS + 1}).success).toBe(
			false,
		);
	});
});

const buildRoleIds = (count: number) =>
	Array.from({length: count}, (_, index) => `${1234567890123456789n + BigInt(index)}`);

describe('GuildMemberUpdateRequest', () => {
	it('accepts every role a member can hold', () => {
		expect(GuildMemberUpdateRequest.safeParse({roles: buildRoleIds(MAX_GUILD_ROLES)}).success).toBe(true);
	});
	it('rejects one role beyond the guild role limit', () => {
		expect(GuildMemberUpdateRequest.safeParse({roles: buildRoleIds(MAX_GUILD_ROLES + 1)}).success).toBe(false);
	});
});

describe('GuildStickerCreateRequest', () => {
	const buildTags = (count: number) => Array.from({length: count}, (_, index) => `tag${index}`);
	const image = `data:image/png;base64,${Buffer.from('sticker').toString('base64')}`;
	it('accepts the maximum tag count', () => {
		expect(
			GuildStickerCreateRequest.safeParse({name: 'sticker', tags: buildTags(MAX_GUILD_STICKER_TAGS), image}).success,
		).toBe(true);
	});
	it('rejects one tag beyond the maximum', () => {
		expect(
			GuildStickerCreateRequest.safeParse({name: 'sticker', tags: buildTags(MAX_GUILD_STICKER_TAGS + 1), image})
				.success,
		).toBe(false);
	});
});
