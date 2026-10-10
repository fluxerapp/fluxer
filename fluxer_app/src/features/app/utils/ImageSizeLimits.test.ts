// SPDX-License-Identifier: AGPL-3.0-or-later

import {AVATAR_MAX_SIZE, EMOJI_MAX_SIZE, STICKER_MAX_SIZE} from '@fluxer/constants/src/LimitConstants';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';
import type {Guild as WireGuild} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {beforeEach, describe, expect, it, vi} from 'vitest';

const RAISED = 5 * 1024 * 1024;
const state: {limits: LimitConfigSnapshot} = {limits: {traitDefinitions: [], rules: []}};

vi.mock('@app/features/app/state/RuntimeConfig', () => ({
	default: {
		localInstanceDomain: 'fluxer.test',
		get limits() {
			return state.limits;
		},
		getSnapshotOrNull: () => ({limits: state.limits}),
	},
}));
vi.mock('@app/features/app/state/InstanceSnapshotStore', () => ({default: {getLimitsForInstance: () => null}}));
vi.mock('@app/features/user/state/Users', () => ({default: {getCurrentUser: () => ({traits: ['premium']})}}));
vi.mock('@app/features/channel/state/Channels', () => ({default: {getChannel: () => null}}));
vi.mock('@app/features/guild/state/Guilds', () => ({default: {getGuild: () => null}}));
vi.mock('@app/features/navigation/state/SelectedChannel', () => ({default: {currentChannelId: null}}));
vi.mock('@app/features/navigation/state/SelectedGuild', () => ({default: {selectedGuildId: null}}));
vi.mock('@app/features/theme/fonts/ScriptFontLoader', () => ({noteText: () => {}}));

const {Guild} = await import('@app/features/guild/models/Guild');
const {GlobalLimits} = await import('@app/features/app/utils/GlobalLimits');

function guild(features: Array<string> = []) {
	return new Guild({id: '1', name: 'guild', owner_id: '2', features, roles: []} as unknown as WireGuild);
}

beforeEach(() => {
	state.limits = {traitDefinitions: ['premium'], rules: [{id: 'default', limits: {}}]};
});

describe('configured image size limits', () => {
	it('falls back to the stock sizes when nothing is configured', () => {
		expect(guild().maxEmojiSize).toBe(EMOJI_MAX_SIZE);
		expect(guild().maxStickerSize).toBe(STICKER_MAX_SIZE);
		expect(GlobalLimits.getAvatarMaxSize()).toBe(AVATAR_MAX_SIZE);
	});

	it('follows a raised default rule', () => {
		state.limits.rules[0].limits = {emoji_max_size: RAISED, sticker_max_size: RAISED, avatar_max_size: RAISED * 6};
		expect(guild().maxEmojiSize).toBe(RAISED);
		expect(guild().maxStickerSize).toBe(RAISED);
		expect(GlobalLimits.getAvatarMaxSize()).toBe(RAISED * 6);
	});

	it('applies guild feature rules to emoji and sticker sizes', () => {
		state.limits.rules.push({
			id: 'boosted',
			filters: {guildFeatures: ['MORE_EMOJI']},
			limits: {emoji_max_size: RAISED, sticker_max_size: RAISED},
		});
		expect(guild().maxEmojiSize).toBe(EMOJI_MAX_SIZE);
		expect(guild(['MORE_EMOJI']).maxEmojiSize).toBe(RAISED);
		expect(guild(['MORE_EMOJI']).maxStickerSize).toBe(RAISED);
	});

	it('ignores trait rules the server does not apply', () => {
		state.limits.rules.push({
			id: 'premium',
			filters: {traits: ['premium']},
			limits: {emoji_max_size: RAISED, sticker_max_size: RAISED, avatar_max_size: RAISED * 6},
		});
		expect(guild().maxEmojiSize).toBe(EMOJI_MAX_SIZE);
		expect(guild().maxStickerSize).toBe(STICKER_MAX_SIZE);
		expect(GlobalLimits.getAvatarMaxSize()).toBe(AVATAR_MAX_SIZE);
	});
});
