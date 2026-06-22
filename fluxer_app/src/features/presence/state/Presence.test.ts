// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeEach, describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/auth/state/Authentication', () => ({
	default: {
		currentUserId: 'current-user',
	},
}));

vi.mock('@app/features/channel/state/Channels', () => ({
	default: {
		getPrivateChannels: () => [],
	},
}));

vi.mock('@app/features/guild/state/Guilds', () => ({
	default: {
		getGuild: () => null,
	},
}));

vi.mock('@app/features/member/state/GuildMembers', () => ({
	default: {
		getMember: () => null,
	},
}));

vi.mock('@app/features/member/state/MemberSidebar', () => ({
	default: {
		handleLocalPresenceUpdate: vi.fn(),
	},
}));

vi.mock('@app/features/platform/utils/AppLogger', () => ({
	Logger: class {
		error = vi.fn();
	},
}));

vi.mock('@app/features/platform/utils/DeferUntilModulesLoaded', () => ({
	deferUntilModulesLoaded: vi.fn(),
}));

vi.mock('@app/features/presence/state/LocalPresence', () => ({
	default: {
		customStatus: null,
		getStatus: () => 'online',
	},
}));

vi.mock('@app/features/presence/state/TransientPresence', () => ({
	default: {
		clear: vi.fn(),
		clearPresence: vi.fn(),
	},
}));

vi.mock('@app/features/relationship/state/Relationships', () => ({
	default: {
		getRelationships: () => [],
	},
}));

vi.mock('@app/features/ui/state/MobileLayout', () => ({
	default: {
		isMobileLayout: () => false,
	},
}));

vi.mock('@app/features/user/state/CustomStatus', () => ({
	fromGatewayCustomStatus: vi.fn(() => null),
}));

vi.mock('@app/features/user/state/CustomStatusEmitter', () => ({
	CustomStatusEmitter: {
		emitPresenceChange: vi.fn(),
	},
}));

const richPresenceActivity = {
	type: 0,
	name: 'Fluxer Desktop',
	state: 'Testing RPC',
};

describe('Presence activity records', () => {
	beforeEach(() => {
		vi.resetModules();
	});

	it('stores activity records from gateway presence updates', async () => {
		const {default: Presence} = await import('@app/features/presence/state/Presence');

		Presence.handlePresenceUpdate({
			guild_id: 'guild-1',
			user: {id: 'friend-1'},
			status: 'online',
			activities: [richPresenceActivity],
		});

		expect(Presence.getActivities('friend-1')).toEqual([richPresenceActivity]);
	});

	it('stores activity records from the ready presence snapshot', async () => {
		const {default: Presence} = await import('@app/features/presence/state/Presence');

		Presence.handleConnectionOpen(
			{id: 'current-user'} as never,
			[
				{
					id: 'guild-1',
					unavailable: false,
					members: [{user: {id: 'friend-1'}}],
				},
			] as never,
			[
				{
					user: {id: 'friend-1'},
					status: 'online',
					activities: [richPresenceActivity],
				},
			],
		);

		expect(Presence.getActivities('friend-1')).toEqual([richPresenceActivity]);
	});
});
