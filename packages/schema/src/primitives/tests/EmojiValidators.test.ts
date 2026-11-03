// SPDX-License-Identifier: AGPL-3.0-or-later

import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {GuildRoleCreateRequest, GuildRoleUpdateRequest} from '@fluxer/schema/src/domains/guild/GuildRequestSchemas';
import {UnicodeRoleEmojiType} from '@fluxer/schema/src/primitives/EmojiValidators';
import {describe, expect, it} from 'vitest';

const PNG_DATA_URI = 'data:image/png;base64,iVBORw0KGgo=';

describe('UnicodeRoleEmojiType', () => {
	it('accepts a single emoji, a skin tone variant and a ZWJ sequence', () => {
		for (const value of ['🔥', '🛡️', '👍🏽', '👩‍💻', '🏳️‍🌈']) {
			expect(UnicodeRoleEmojiType.safeParse(value).success, value).toBe(true);
		}
	});

	it('rejects text, two emoji and long strings', () => {
		for (const value of ['abc', '🔥🔥', '', '🔥 ', '🔥'.repeat(40)]) {
			const result = UnicodeRoleEmojiType.safeParse(value);
			expect(result.success, value).toBe(false);
			expect(result.error?.issues[0]?.message).toBe(ValidationErrorCodes.NOT_A_VALID_UNICODE_EMOJI);
		}
	});
});

describe('role icon request fields', () => {
	for (const [name, schema, base] of [
		['create', GuildRoleCreateRequest, {name: 'Mods'}],
		['update', GuildRoleUpdateRequest, {}],
	] as const) {
		it(`${name} accepts null and absent icon fields`, () => {
			expect(schema.safeParse(base).success).toBe(true);
			expect(schema.safeParse({...base, icon: null, unicode_emoji: null}).success).toBe(true);
			expect(schema.safeParse({...base, icon: PNG_DATA_URI, unicode_emoji: null}).success).toBe(true);
			expect(schema.safeParse({...base, icon: null, unicode_emoji: '🔥'}).success).toBe(true);
		});

		it(`${name} rejects an image and an emoji together`, () => {
			const result = schema.safeParse({...base, icon: PNG_DATA_URI, unicode_emoji: '🔥'});
			expect(result.success).toBe(false);
			expect(result.error?.issues[0]).toMatchObject({
				message: ValidationErrorCodes.ROLE_ICON_AND_EMOJI_EXCLUSIVE,
				path: ['icon'],
			});
		});
	}
});
