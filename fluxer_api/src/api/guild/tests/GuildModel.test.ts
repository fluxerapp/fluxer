// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import type {GuildRow} from '@app/api/database/types/GuildTypes';
import {mapGuildToGuildResponse, mapGuildToPartialResponse} from '@app/api/guild/GuildModel';
import {Guild} from '@app/api/models/Guild';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {describe, expect, test} from 'vitest';

function createGuild(overrides: Partial<GuildRow> = {}): Guild {
	return new Guild({
		guild_id: createGuildID(100n),
		owner_id: createUserID(200n),
		name: 'Test Guild',
		vanity_url_code: null,
		icon_hash: null,
		banner_hash: null,
		banner_width: null,
		banner_height: null,
		splash_hash: null,
		splash_width: null,
		splash_height: null,
		splash_card_alignment: null,
		embed_splash_hash: null,
		embed_splash_width: null,
		embed_splash_height: null,
		features: null,
		verification_level: 0,
		mfa_level: 0,
		nsfw_level: 0,
		nsfw: false,
		content_warning_level: 0,
		content_warning_text: null,
		explicit_content_filter: 0,
		default_message_notifications: 0,
		system_channel_id: null,
		system_channel_flags: 0,
		rules_channel_id: null,
		afk_channel_id: null,
		afk_timeout: 0,
		disabled_operations: 0,
		member_count: 0,
		audit_logs_indexed_at: null,
		members_indexed_at: null,
		message_history_cutoff: null,
		version: 1,
		...overrides,
	});
}

describe('GuildModel asset entitlement mapping', () => {
	test('strips animated guild icon prefix when ANIMATED_ICON is absent', () => {
		const guild = createGuild({icon_hash: 'a_icon123', features: new Set()});
		expect(mapGuildToPartialResponse(guild).icon).toBe('icon123');
		expect(mapGuildToGuildResponse(guild).icon).toBe('icon123');
	});
	test('keeps animated guild icon prefix when ANIMATED_ICON is present', () => {
		const guild = createGuild({icon_hash: 'a_icon123', features: new Set([GuildFeatures.ANIMATED_ICON])});
		expect(mapGuildToPartialResponse(guild).icon).toBe('a_icon123');
		expect(mapGuildToGuildResponse(guild).icon).toBe('a_icon123');
	});
	test('hides banners and dimensions when BANNER is absent', () => {
		const guild = createGuild({
			banner_hash: 'banner123',
			banner_width: 1024,
			banner_height: 512,
			features: new Set(),
		});
		const partial = mapGuildToPartialResponse(guild);
		const full = mapGuildToGuildResponse(guild);
		expect(partial.banner).toBeNull();
		expect(partial.banner_width).toBeNull();
		expect(partial.banner_height).toBeNull();
		expect(full.banner).toBeNull();
		expect(full.banner_width).toBeNull();
		expect(full.banner_height).toBeNull();
	});
	test('strips animated guild banner prefix when ANIMATED_BANNER is absent', () => {
		const guild = createGuild({
			banner_hash: 'a_banner123',
			banner_width: 1024,
			banner_height: 512,
			features: new Set([GuildFeatures.BANNER]),
		});
		expect(mapGuildToPartialResponse(guild).banner).toBe('banner123');
		expect(mapGuildToGuildResponse(guild).banner).toBe('banner123');
	});
	test('hides invite splashes and dimensions when INVITE_SPLASH is absent', () => {
		const guild = createGuild({
			splash_hash: 'splash123',
			splash_width: 1024,
			splash_height: 512,
			embed_splash_hash: 'embed123',
			embed_splash_width: 1024,
			embed_splash_height: 512,
			features: new Set(),
		});
		const partial = mapGuildToPartialResponse(guild);
		const full = mapGuildToGuildResponse(guild);
		expect(partial.splash).toBeNull();
		expect(partial.splash_width).toBeNull();
		expect(partial.splash_height).toBeNull();
		expect(partial.embed_splash).toBeNull();
		expect(partial.embed_splash_width).toBeNull();
		expect(partial.embed_splash_height).toBeNull();
		expect(full.splash).toBeNull();
		expect(full.splash_width).toBeNull();
		expect(full.splash_height).toBeNull();
		expect(full.embed_splash).toBeNull();
		expect(full.embed_splash_width).toBeNull();
		expect(full.embed_splash_height).toBeNull();
	});
});
