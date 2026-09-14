// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	ForwardChannelIndex,
	type ForwardChannelObservation,
} from '@app/features/app/components/dialogs/shared/ForwardChannelIndex';
import type {Channel} from '@app/features/channel/models/Channel';
import type {User} from '@app/features/user/models/User';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {setupI18n} from '@lingui/core';
import {describe, expect, it, vi} from 'vitest';

vi.mock('@lingui/core/macro', () => ({msg: (descriptor: unknown) => descriptor}));
vi.mock('@app/features/channel/components/SlowmodeIndicator', () => ({formatSlowmodeTime: () => ''}));
vi.mock('@app/features/permissions/utils/PermissionUtils', () => ({formatPermissionLabel: () => ''}));

const i18n = setupI18n({locale: 'en', messages: {en: {}}});

function observation(overrides: Partial<ForwardChannelObservation>): ForwardChannelObservation {
	return {
		canAttachFiles: true,
		canEmbedLinks: true,
		canSendMessages: true,
		categoryName: null,
		channel: null,
		displayName: '',
		guildMessagesDisabled: false,
		guildName: null,
		key: '',
		memberTimedOut: false,
		recipient: null,
		searchAliases: [],
		slowmodeEnabled: false,
		slowmodeRemainingMs: 0,
		...overrides,
	};
}

const general = observation({
	channel: {id: 'channel-1', name: 'general', type: ChannelTypes.GUILD_TEXT} as unknown as Channel,
	displayName: 'general',
	key: 'channel-1',
});
const closedFriendDM = observation({
	displayName: 'Ada Lovelace',
	key: 'user:user-1',
	recipient: {id: 'user-1'} as unknown as User,
	searchAliases: ['ada_dev'],
});

function buildIndex(observations: ReadonlyArray<ForwardChannelObservation>): ForwardChannelIndex {
	return new ForwardChannelIndex({
		excludedChannelId: 'channel-1',
		i18n,
		mediaSelection: {hasAttachments: true, hasEmbeds: true},
		observations,
		recentChannelIds: [],
	});
}

describe('forward channel index', () => {
	it('lists a friend whose DM is closed ahead of the source channel', () => {
		const keys = buildIndex([general, closedFriendDM])
			.filter('')
			.map((option) => option.key);
		expect(keys).toEqual(['user:user-1', 'channel-1']);
	});

	it('finds a friend whose DM is closed by username', () => {
		const matches = buildIndex([general, closedFriendDM]).filter('ada_dev');
		expect(matches.map((option) => option.key)).toEqual(['user:user-1']);
		expect(matches[0]?.channel).toBeNull();
		expect(matches[0]?.disableReason).toBeNull();
	});

	it('selects destinations by key', () => {
		const selected = buildIndex([general, closedFriendDM]).select(new Set(['user:user-1', 'channel-1', 'missing']));
		expect(selected.map((option) => option.key)).toEqual(['user:user-1', 'channel-1']);
	});

	it('drops an observation with neither a channel nor a recipient', () => {
		const keys = buildIndex([observation({displayName: 'ghost', key: 'user:ghost'})])
			.filter('')
			.map((option) => option.key);
		expect(keys).toEqual([]);
	});
});
