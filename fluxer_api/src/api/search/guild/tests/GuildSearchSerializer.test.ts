// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createGuildID, createUserID} from '@app/api/BrandedTypes';
import type {GuildRow} from '@app/api/database/types/GuildTypes';
import {Guild} from '@app/api/models/Guild';
import {convertToSearchableGuild} from '@app/api/search/guild/GuildSearchSerializer';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {describe, expect, it} from 'vitest';

function guildRow(features: Set<string>): GuildRow {
	return {
		guild_id: createGuildID(1472623911696138261n),
		owner_id: createUserID(1472623911696138262n),
		name: 'FluxDiscover',
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
		features,
		verification_level: 0,
		mfa_level: 0,
		nsfw_level: 0,
		nsfw: false,
		content_warning_level: 0,
		content_warning_text: null,
		explicit_content_filter: 0,
		default_message_notifications: 0,
		system_channel_id: createChannelID(1472623911696138263n),
		system_channel_flags: 0,
		rules_channel_id: null,
		afk_channel_id: null,
		afk_timeout: 0,
		disabled_operations: 0,
		member_count: 42,
		audit_logs_indexed_at: null,
		members_indexed_at: null,
		message_history_cutoff: null,
		version: 1,
	};
}

describe('GuildSearchSerializer', () => {
	it('requires approved discovery context before indexing a guild as discoverable', () => {
		const guild = new Guild(guildRow(new Set([GuildFeatures.DISCOVERABLE])));
		const withoutContext = convertToSearchableGuild(guild);
		expect(withoutContext.isDiscoverable).toBe(false);
		expect(withoutContext.discoveryDescription).toBeNull();
		expect(withoutContext.discoveryTags).toEqual([]);
		const withContext = convertToSearchableGuild(guild, {
			description: 'A community for finding other communities.',
			categoryId: 8,
			primaryLanguage: 'en-US',
			tags: ['discover', 'community'],
			memberCount: 1928,
		});
		expect(withContext.isDiscoverable).toBe(true);
		expect(withContext.discoveryDescription).toBe('A community for finding other communities.');
		expect(withContext.discoveryTags).toEqual(['discover', 'community']);
		expect(withContext.memberCount).toBe(1928);
	});
	it('drops accidental discovery context when the guild feature is absent', () => {
		const guild = new Guild(guildRow(new Set()));
		const result = convertToSearchableGuild(guild, {
			description: 'Stale discovery row.',
			categoryId: 8,
			primaryLanguage: 'en-US',
			tags: ['stale'],
		});
		expect(result.isDiscoverable).toBe(false);
		expect(result.discoveryDescription).toBeNull();
		expect(result.discoveryTags).toEqual([]);
	});
});
