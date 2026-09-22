// SPDX-License-Identifier: AGPL-3.0-or-later

import {ListGuildEmojisResponse, LookupGuildResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {ListUserRelationshipsResponse} from '@fluxer/schema/src/domains/admin/AdminUserSchemas';
import {describe, expect, it} from 'vitest';

function snowflakeAt(index: number): string {
	return String(100000000000000000n + BigInt(index));
}

describe('LookupGuildResponse', () => {
	const validGuild = {
		id: '123456789012345678',
		owner_id: '111111111111111111',
		owner_username: 'owner',
		owner_global_name: null,
		owner_discriminator: '0001',
		name: 'Test Guild',
		vanity_url_code: null,
		icon: null,
		banner: null,
		splash: null,
		embed_splash: null,
		features: [],
		verification_level: 0,
		mfa_level: 0,
		nsfw_level: 0,
		explicit_content_filter: 0,
		default_message_notifications: 0,
		afk_channel_id: null,
		afk_timeout: 300,
		system_channel_id: null,
		system_channel_flags: 0,
		rules_channel_id: null,
		disabled_operations: 0,
		member_count: 1,
		channels: [],
		roles: [],
	};

	it('accepts more roles than the old response ceiling', () => {
		const result = LookupGuildResponse.safeParse({
			guild: {
				...validGuild,
				roles: Array.from({length: 251}, (_, index) => ({
					id: snowflakeAt(index),
					name: `role-${index}`,
					color: 0,
					position: index,
					permissions: '0',
					hoist: false,
					mentionable: false,
				})),
			},
		});
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.guild?.roles).toHaveLength(251);
		}
	});

	it('accepts more channels than the old response ceiling', () => {
		const result = LookupGuildResponse.safeParse({
			guild: {
				...validGuild,
				channels: Array.from({length: 501}, (_, index) => ({
					id: snowflakeAt(index),
					name: `channel-${index}`,
					type: 0,
					position: index,
					parent_id: null,
					nsfw: false,
					url: null,
				})),
			},
		});
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.guild?.channels).toHaveLength(501);
		}
	});
});

describe('ListGuildEmojisResponse', () => {
	it('accepts more emojis than the old response ceiling', () => {
		const result = ListGuildEmojisResponse.safeParse({
			guild_id: '123456789012345678',
			emojis: Array.from({length: 501}, (_, index) => ({
				id: snowflakeAt(index),
				name: `emoji_${index}`,
				animated: false,
				creator_id: '111111111111111111',
				media_url: 'https://example.com/emoji.png',
			})),
		});
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.emojis).toHaveLength(501);
		}
	});
});

describe('ListUserRelationshipsResponse', () => {
	it('accepts more friends than the old response ceiling', () => {
		const result = ListUserRelationshipsResponse.safeParse({
			friends: Array.from({length: 10001}, (_, index) => ({
				target_user_id: snowflakeAt(index),
				category: 'friend',
				nickname: null,
				since: null,
				target: null,
			})),
			incoming_requests: [],
			outgoing_requests: [],
			blocked: [],
		});
		expect(result.success).toBe(true);
		if (result.success) {
			expect(result.data.friends).toHaveLength(10001);
		}
	});
});
