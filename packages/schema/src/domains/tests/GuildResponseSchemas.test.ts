// SPDX-License-Identifier: AGPL-3.0-or-later

import {GuildFeatures, GuildSplashCardAlignment} from '@fluxer/constants/src/GuildConstants';
import {
	GuildPartialResponse,
	GuildResponse,
	GuildVanityURLResponse,
} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {describe, expect, it} from 'vitest';

describe('GuildResponse', () => {
	const validGuild = {
		id: '123456789012345678',
		name: 'Test Guild',
		icon: 'icon_hash',
		banner: 'banner_hash',
		banner_width: 1920,
		banner_height: 1080,
		splash: 'splash_hash',
		splash_width: 1920,
		splash_height: 1080,
		splash_card_alignment: GuildSplashCardAlignment.CENTER,
		embed_splash: 'embed_splash_hash',
		embed_splash_width: 800,
		embed_splash_height: 600,
		vanity_url_code: 'myserver',
		owner_id: '987654321098765432',
		system_channel_id: '111111111111111111',
		system_channel_flags: 0,
		rules_channel_id: '222222222222222222',
		afk_channel_id: '333333333333333333',
		afk_timeout: 300,
		features: [GuildFeatures.VERIFIED, GuildFeatures.VANITY_URL],
		verification_level: 2,
		mfa_level: 1,
		nsfw_level: 0,
		nsfw: false,
		content_warning_level: 0,
		content_warning_text: null,
		explicit_content_filter: 2,
		default_message_notifications: 1,
		disabled_operations: 0,
		message_history_cutoff: null,
		permissions: '8',
	};
	it('accepts valid guild response', () => {
		const result = GuildResponse.parse(validGuild);
		expect(result.id).toBe('123456789012345678');
		expect(result.name).toBe('Test Guild');
	});
	it('accepts guild with null optional fields', () => {
		const guild = {
			...validGuild,
			icon: null,
			banner: null,
			splash: null,
			vanity_url_code: null,
			system_channel_id: null,
			rules_channel_id: null,
			afk_channel_id: null,
		};
		const result = GuildResponse.safeParse(guild);
		expect(result.success).toBe(true);
	});
	it('rejects null permissions because the field is omitted when unavailable', () => {
		const guild = {...validGuild, permissions: null};
		const result = GuildResponse.safeParse(guild);
		expect(result.success).toBe(false);
	});
	it('accepts all splash_card_alignment values', () => {
		for (const alignment of [
			GuildSplashCardAlignment.LEFT,
			GuildSplashCardAlignment.CENTER,
			GuildSplashCardAlignment.RIGHT,
		]) {
			const guild = {...validGuild, splash_card_alignment: alignment};
			const result = GuildResponse.safeParse(guild);
			expect(result.success).toBe(true);
		}
	});
	it('rejects invalid splash_card_alignment', () => {
		const guild = {...validGuild, splash_card_alignment: 'invalid'};
		const result = GuildResponse.safeParse(guild);
		expect(result.success).toBe(false);
	});
	it('rejects missing required fields', () => {
		const {id, ...guildWithoutId} = validGuild;
		const result = GuildResponse.safeParse(guildWithoutId);
		expect(result.success).toBe(false);
	});
	it('requires owner_id', () => {
		const {owner_id, ...guildWithoutOwner} = validGuild;
		const result = GuildResponse.safeParse(guildWithoutOwner);
		expect(result.success).toBe(false);
	});
	it('requires name', () => {
		const {name, ...guildWithoutName} = validGuild;
		const result = GuildResponse.safeParse(guildWithoutName);
		expect(result.success).toBe(false);
	});
	it('requires features array', () => {
		const {features, ...guildWithoutFeatures} = validGuild;
		const result = GuildResponse.safeParse(guildWithoutFeatures);
		expect(result.success).toBe(false);
	});
	it('accepts empty features array', () => {
		const guild = {...validGuild, features: []};
		const result = GuildResponse.safeParse(guild);
		expect(result.success).toBe(true);
	});
	it('accepts RAID_DETECTED as a known guild feature', () => {
		const guild = {
			...validGuild,
			features: [GuildFeatures.RAID_DETECTED, GuildFeatures.VERIFIED],
		};
		const result = GuildResponse.parse(guild);
		expect(result.features).toEqual([GuildFeatures.RAID_DETECTED, GuildFeatures.VERIFIED]);
	});
	it('preserves unknown features in guild response', () => {
		const guild = {
			...validGuild,
			features: [GuildFeatures.VERIFIED, 'DISALLOW_UNCLAIMED_ACCOUNTS'],
		};
		const result = GuildResponse.parse(guild);
		expect(result.features).toEqual(['DISALLOW_UNCLAIMED_ACCOUNTS', GuildFeatures.VERIFIED]);
	});
	it('deduplicates and sorts guild features while preserving values', () => {
		const guild = {
			...validGuild,
			features: [GuildFeatures.VERIFIED, 'DISALLOW_UNCLAIMED_ACCOUNTS', GuildFeatures.VERIFIED],
		};
		const result = GuildResponse.parse(guild);
		expect(result.features).toEqual(['DISALLOW_UNCLAIMED_ACCOUNTS', GuildFeatures.VERIFIED]);
	});
	it('rejects non-integer system_channel_flags', () => {
		const guild = {...validGuild, system_channel_flags: 1.5};
		const result = GuildResponse.safeParse(guild);
		expect(result.success).toBe(false);
	});
	it('rejects non-integer verification_level', () => {
		const guild = {...validGuild, verification_level: 'high'};
		const result = GuildResponse.safeParse(guild);
		expect(result.success).toBe(false);
	});
});

describe('GuildPartialResponse', () => {
	const validPartialGuild = {
		id: '123456789012345678',
		name: 'Test Guild',
		icon: 'icon_hash',
		banner: 'banner_hash',
		banner_width: 1920,
		banner_height: 1080,
		splash: 'splash_hash',
		splash_width: 1920,
		splash_height: 1080,
		splash_card_alignment: GuildSplashCardAlignment.CENTER,
		embed_splash: 'embed_splash_hash',
		embed_splash_width: 800,
		embed_splash_height: 600,
		features: [GuildFeatures.VERIFIED],
	};
	it('accepts valid partial guild response', () => {
		const result = GuildPartialResponse.parse(validPartialGuild);
		expect(result.id).toBe('123456789012345678');
		expect(result.name).toBe('Test Guild');
	});
	it('accepts partial guild with null optional fields', () => {
		const guild = {
			...validPartialGuild,
			icon: null,
			banner: null,
			splash: null,
			embed_splash: null,
		};
		const result = GuildPartialResponse.safeParse(guild);
		expect(result.success).toBe(true);
	});
	it('requires id', () => {
		const {id, ...guildWithoutId} = validPartialGuild;
		const result = GuildPartialResponse.safeParse(guildWithoutId);
		expect(result.success).toBe(false);
	});
	it('requires name', () => {
		const {name, ...guildWithoutName} = validPartialGuild;
		const result = GuildPartialResponse.safeParse(guildWithoutName);
		expect(result.success).toBe(false);
	});
	it('requires features array', () => {
		const {features, ...guildWithoutFeatures} = validPartialGuild;
		const result = GuildPartialResponse.safeParse(guildWithoutFeatures);
		expect(result.success).toBe(false);
	});
	it('preserves unknown features in partial guild response', () => {
		const guild = {
			...validPartialGuild,
			features: [GuildFeatures.VERIFIED, 'DISALLOW_UNCLAIMED_ACCOUNTS'],
		};
		const result = GuildPartialResponse.parse(guild);
		expect(result.features).toEqual(['DISALLOW_UNCLAIMED_ACCOUNTS', GuildFeatures.VERIFIED]);
	});
});

describe('GuildVanityURLResponse', () => {
	it('accepts valid vanity URL response', () => {
		const result = GuildVanityURLResponse.safeParse({
			code: 'myserver',
			uses: 42,
		});
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.code).toBe('myserver');
			expect(result.data.uses).toBe(42);
		}
	});
	it('accepts null code', () => {
		const result = GuildVanityURLResponse.safeParse({
			code: null,
			uses: 0,
		});
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.code).toBeNull();
		}
	});
	it('requires uses field', () => {
		const result = GuildVanityURLResponse.safeParse({
			code: 'myserver',
		});
		expect(result.success).toBe(false);
	});
	it('requires uses to be integer', () => {
		const result = GuildVanityURLResponse.safeParse({
			code: 'myserver',
			uses: 1.5,
		});
		expect(result.success).toBe(false);
	});
	it('accepts zero uses', () => {
		const result = GuildVanityURLResponse.safeParse({
			code: 'myserver',
			uses: 0,
		});
		expect(result.success).toBe(true);
	});
});
