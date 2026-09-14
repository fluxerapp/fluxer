// SPDX-License-Identifier: AGPL-3.0-or-later

import {Guild} from '@app/features/guild/models/Guild';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {Guild as WireGuild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {describe, expect, test, vi} from 'vitest';

vi.mock('@app/features/app/state/RuntimeConfig', () => ({default: {localInstanceDomain: 'fluxer.test'}}));

const createGuild = (features: Array<string>): Guild =>
	new Guild({
		id: '1',
		name: 'Test Community',
		icon: null,
		features,
		roles: [],
		owner_id: '2',
	} as unknown as WireGuild);

describe('Guild clone permissions', () => {
	test('no features means cloning is not allowed', () => {
		const guild = createGuild([]);
		expect(guild.cloneEmojiAllowed).toBe(false);
		expect(guild.cloneStickerAllowed).toBe(false);
	});

	test('the enabled features allow cloning', () => {
		const guild = createGuild([GuildFeatures.CLONE_EMOJI_ENABLED, GuildFeatures.CLONE_STICKER_ENABLED]);
		expect(guild.cloneEmojiAllowed).toBe(true);
		expect(guild.cloneStickerAllowed).toBe(true);
	});

	test('each enabled feature only allows its own kind', () => {
		const emojiOnly = createGuild([GuildFeatures.CLONE_EMOJI_ENABLED]);
		expect(emojiOnly.cloneEmojiAllowed).toBe(true);
		expect(emojiOnly.cloneStickerAllowed).toBe(false);
		const stickerOnly = createGuild([GuildFeatures.CLONE_STICKER_ENABLED]);
		expect(stickerOnly.cloneEmojiAllowed).toBe(false);
		expect(stickerOnly.cloneStickerAllowed).toBe(true);
	});

	test('the deprecated disabled features do not allow cloning', () => {
		const guild = createGuild([GuildFeatures.CLONE_EMOJI_DISABLED, GuildFeatures.CLONE_STICKER_DISABLED]);
		expect(guild.cloneEmojiAllowed).toBe(false);
		expect(guild.cloneStickerAllowed).toBe(false);
	});

	test('the deprecated disabled features do not override the enabled features', () => {
		const guild = createGuild([
			GuildFeatures.CLONE_EMOJI_DISABLED,
			GuildFeatures.CLONE_STICKER_DISABLED,
			GuildFeatures.CLONE_EMOJI_ENABLED,
			GuildFeatures.CLONE_STICKER_ENABLED,
		]);
		expect(guild.cloneEmojiAllowed).toBe(true);
		expect(guild.cloneStickerAllowed).toBe(true);
	});
});
