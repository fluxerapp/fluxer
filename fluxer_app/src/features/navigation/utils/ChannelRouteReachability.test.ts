// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, test, vi} from 'vitest';

const mocks = vi.hoisted(() => ({
	channels: new Set<string>(),
	guilds: new Set<string>(),
	unavailableGuilds: new Set<string>(),
	lastLocation: null as string | null,
	currentPath: '',
	replaceWith: vi.fn<(path: string) => void>(),
	clearLastLocation: vi.fn<() => void>(),
}));

vi.mock('@app/features/channel/state/Channels', () => ({
	default: {getChannel: (id: string) => (mocks.channels.has(id) ? {id} : undefined)},
}));

vi.mock('@app/features/guild/state/Guilds', () => ({
	default: {getGuild: (id: string) => (mocks.guilds.has(id) ? {id} : undefined)},
}));

vi.mock('@app/features/guild/state/GuildAvailability', () => ({
	default: {unavailableGuilds: mocks.unavailableGuilds},
}));

vi.mock('@app/features/ui/state/Location', () => ({
	default: {getLastLocation: () => mocks.lastLocation, clearLastLocation: mocks.clearLastLocation},
}));

vi.mock('@app/features/navigation/utils/RouterUtils', () => ({
	getCurrentPath: () => mocks.currentPath,
	replaceWith: mocks.replaceWith,
}));

import {
	abandonUnreachableLastLocation,
	isUnreachableChannelRoute,
} from '@app/features/navigation/utils/ChannelRouteReachability';

const OWN_DM = '/channels/@me/333';
const FOREIGN_DM = '/channels/@me/444';

beforeEach(() => {
	mocks.channels.clear();
	mocks.guilds.clear();
	mocks.unavailableGuilds.clear();
	mocks.channels.add('333');
	mocks.guilds.add('111');
	mocks.lastLocation = null;
	mocks.currentPath = '';
	mocks.replaceWith.mockReset();
	mocks.clearLastLocation.mockReset();
});

describe('channel route reachability', () => {
	test('a direct message the account has is reachable and one it lacks is not', () => {
		expect(isUnreachableChannelRoute(OWN_DM)).toBe(false);
		expect(isUnreachableChannelRoute(FOREIGN_DM)).toBe(true);
		expect(isUnreachableChannelRoute(`${FOREIGN_DM}/999`)).toBe(true);
	});

	test('a guild route is unreachable only when the guild is neither joined nor unavailable', () => {
		mocks.unavailableGuilds.add('555');

		expect(isUnreachableChannelRoute('/channels/111/222')).toBe(false);
		expect(isUnreachableChannelRoute('/channels/111')).toBe(false);
		expect(isUnreachableChannelRoute('/channels/555/222')).toBe(false);
		expect(isUnreachableChannelRoute('/channels/666/222')).toBe(true);
	});

	test('routes that name no channel are always reachable', () => {
		for (const pathname of ['/channels/@me', '/channels/@me/', '/channels/@favorites/444', '/bookmarks', '/']) {
			expect(isUnreachableChannelRoute(pathname), pathname).toBe(false);
		}
	});
});

describe('abandoning an unreachable last location', () => {
	test('a reachable last location is kept', () => {
		mocks.lastLocation = OWN_DM;
		mocks.currentPath = OWN_DM;

		abandonUnreachableLastLocation();

		expect(mocks.clearLastLocation).not.toHaveBeenCalled();
		expect(mocks.replaceWith).not.toHaveBeenCalled();
	});

	test('an unreachable last location is forgotten and left when it is on screen', () => {
		mocks.lastLocation = FOREIGN_DM;
		mocks.currentPath = FOREIGN_DM;

		abandonUnreachableLastLocation();

		expect(mocks.clearLastLocation).toHaveBeenCalledTimes(1);
		expect(mocks.replaceWith).toHaveBeenCalledWith('/channels/@me');
	});

	test('an unreachable last location is forgotten without moving a user who is elsewhere', () => {
		mocks.lastLocation = FOREIGN_DM;
		mocks.currentPath = '/channels/111/222';

		abandonUnreachableLastLocation();

		expect(mocks.clearLastLocation).toHaveBeenCalledTimes(1);
		expect(mocks.replaceWith).not.toHaveBeenCalled();
	});
});
