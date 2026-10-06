// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import type {GuildRow} from '@app/api/database/types/GuildTypes';
import {Guild} from '@app/api/models/Guild';
import {
	DISQUALIFIED_MUTUAL_GUILD_DM_ACCESS_GUILD_IDS,
	getMutualGuildsForDmAccess,
	guildQualifiesForMutualGuildDmAccess,
} from '@app/api/utils/MutualGuildDmAccess';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {describe, expect, it} from 'vitest';

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

describe('guildQualifiesForMutualGuildDmAccess', () => {
	it('disqualifies configured mutual guild ids', () => {
		for (const guildId of DISQUALIFIED_MUTUAL_GUILD_DM_ACCESS_GUILD_IDS) {
			const guild = createGuild({guild_id: createGuildID(guildId)});
			expect(guildQualifiesForMutualGuildDmAccess(guild)).toBe(false);
		}
	});

	it('does not disqualify guilds just because they are verified', () => {
		const guild = createGuild({features: new Set([GuildFeatures.VERIFIED])});
		expect(guildQualifiesForMutualGuildDmAccess(guild)).toBe(true);
	});

	it('returns only qualifying mutual guilds', () => {
		const allowedGuild = createGuild({guild_id: createGuildID(300n)});
		const disqualifiedGuild = createGuild({guild_id: createGuildID(DISQUALIFIED_MUTUAL_GUILD_DM_ACCESS_GUILD_IDS[0])});
		const otherGuild = createGuild({guild_id: createGuildID(400n)});
		const mutualGuilds = getMutualGuildsForDmAccess({
			userGuilds: [allowedGuild, disqualifiedGuild],
			targetGuilds: [allowedGuild, disqualifiedGuild, otherGuild],
		});
		expect(mutualGuilds).toEqual([allowedGuild]);
	});
});
