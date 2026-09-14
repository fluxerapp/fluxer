// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Profile} from '@app/features/user/models/Profile';
import {afterEach, describe, expect, it, vi} from 'vitest';

vi.mock('@app/features/auth/state/Authentication', () => ({default: {currentUserId: null}}));

const {default: UserProfile} = await import('@app/features/user/state/UserProfile');

const GUILD_ID = '1000';
const TARGET_ID = '2000';

function cache(userId: string = TARGET_ID, guildId?: string): void {
	UserProfile.handleProfileCreate({
		userId,
		guildId: guildId ?? null,
		mutualGuilds: [{id: GUILD_ID, nick: null}],
	} as unknown as Profile);
}

describe('UserProfile cache invalidation', () => {
	afterEach(() => {
		UserProfile.handleGatewayReady();
	});

	it('drops every cached scope for a member removed from a community', () => {
		cache();
		cache(TARGET_ID, GUILD_ID);
		expect(UserProfile.getProfile(TARGET_ID)?.mutualGuilds).toHaveLength(1);
		UserProfile.handleGuildMemberRemove(TARGET_ID);
		expect(UserProfile.getProfile(TARGET_ID)).toBeNull();
		expect(UserProfile.getProfile(TARGET_ID, GUILD_ID)).toBeNull();
	});

	it('drops every cached scope for a member who joined a community', () => {
		cache();
		UserProfile.handleGuildMemberAdd(TARGET_ID);
		expect(UserProfile.getProfile(TARGET_ID)).toBeNull();
	});

	it('keeps other users cached when one member is removed', () => {
		cache();
		cache('3000');
		UserProfile.handleGuildMemberRemove(TARGET_ID);
		expect(UserProfile.getProfile(TARGET_ID)).toBeNull();
		expect(UserProfile.getProfile('3000')).not.toBeNull();
	});

	it('drops cached profiles when the viewer leaves a community', () => {
		cache();
		UserProfile.handleGuildDelete(false);
		expect(UserProfile.getProfile(TARGET_ID)).toBeNull();
	});

	it('keeps cached profiles when a community only goes unavailable', () => {
		cache();
		UserProfile.handleGuildDelete(true);
		expect(UserProfile.getProfile(TARGET_ID)).not.toBeNull();
	});

	it('drops cached profiles when the viewer joins a community', () => {
		cache();
		UserProfile.handleGuildCreate();
		expect(UserProfile.getProfile(TARGET_ID)).toBeNull();
	});
});
