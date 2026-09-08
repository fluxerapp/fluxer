// SPDX-License-Identifier: AGPL-3.0-or-later

import {FAVORITES_GUILD_ID} from '@fluxer/constants/src/AppConstants';
import {describe, expect, it} from 'vitest';
import {selectGuildActivationTarget} from './GuildActivationTarget';

describe('selectGuildActivationTarget', () => {
	it('returns the selected guild on a guild route', () => {
		expect(selectGuildActivationTarget({selectedGuildId: '1', openChannelGuildId: '1'})).toBe('1');
	});

	it('returns the selected guild when no channel is open', () => {
		expect(selectGuildActivationTarget({selectedGuildId: '1', openChannelGuildId: null})).toBe('1');
	});

	it('returns the owning guild of a channel opened from the favorites route', () => {
		expect(selectGuildActivationTarget({selectedGuildId: null, openChannelGuildId: '1'})).toBe('1');
	});

	it('ignores the favorites pseudo guild and falls through to the open channel', () => {
		expect(selectGuildActivationTarget({selectedGuildId: FAVORITES_GUILD_ID, openChannelGuildId: '1'})).toBe('1');
	});

	it('returns null on the favorites route while no channel is open', () => {
		expect(selectGuildActivationTarget({selectedGuildId: FAVORITES_GUILD_ID, openChannelGuildId: null})).toBeNull();
	});

	it('returns null for a private channel', () => {
		expect(selectGuildActivationTarget({selectedGuildId: null, openChannelGuildId: null})).toBeNull();
	});
});
