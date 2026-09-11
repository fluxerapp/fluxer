// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	MAX_GUILD_ROLES,
	MAX_GUILD_STICKER_TAGS,
	MAX_TEMP_BAN_DURATION_SECONDS,
	MIN_TEMP_BAN_DURATION_SECONDS,
} from '@fluxer/constants/src/LimitConstants';
import {
	DiscoveryApplicationPatchRequest,
	DiscoveryApplicationRequest,
	DiscoverySearchQuery,
} from '@fluxer/schema/src/domains/guild/GuildDiscoverySchemas';
import {
	GuildBanCreateRequest,
	GuildMemberUpdateRequest,
	GuildStickerCreateRequest,
} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import {TemplateChannel} from '@fluxer/schema/src/domains/guild/GuildTemplateSchemas';
import {describe, expect, it} from 'vitest';

describe.each([
	{name: 'application', schema: DiscoveryApplicationRequest.shape.primary_language},
	{name: 'application patch', schema: DiscoveryApplicationPatchRequest.shape.primary_language},
	{name: 'search', schema: DiscoverySearchQuery.shape.language},
])('discovery $name language', ({schema}) => {
	it.each(['en-US', 'sv-SE', undefined])('preserves supported or omitted language %j', (language) => {
		expect(schema.parse(language)).toBe(language);
	});
	it.each(['unsupported', 'EN-US', ' en-US '])('rejects unsupported language %j without normalization', (language) => {
		expect(schema.safeParse(language).error?.issues).toEqual([
			{code: 'custom', message: 'Unsupported language code', path: []},
		]);
	});
});

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

describe('TemplateChannel permission overwrites', () => {
	it.each([
		['role', 0],
		['member', 1],
		['0', 0],
		[1, 1],
		[42, 42],
	])('normalizes the imported overwrite type %j', (type, expected) => {
		const channel = TemplateChannel.parse({
			id: '1',
			type: 0,
			position: 0,
			permission_overwrites: [{id: '2', type, allow: '8', deny: '0'}],
		});
		expect(channel.permission_overwrites).toEqual([{id: '2', type: expected, allow: '8', deny: '0'}]);
	});

	it.each([
		'invalid',
		'NaN',
		'Infinity',
		Number.NaN,
		Number.POSITIVE_INFINITY,
	])('rejects nonfinite overwrite type %j', (type) => {
		expect(
			TemplateChannel.safeParse({
				id: '1',
				type: 0,
				position: 0,
				permission_overwrites: [{id: '2', type, allow: '8', deny: '0'}],
			}).success,
		).toBe(false);
	});
});
