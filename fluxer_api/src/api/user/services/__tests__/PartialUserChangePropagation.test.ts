// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID} from '@app/api/BrandedTypes';
import type {IGuildRepositoryAggregate} from '@app/api/guild/repositories/IGuildRepositoryAggregate';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {UserCacheService} from '@app/api/infrastructure/UserCacheService';
import type {GuildMember} from '@app/api/models/GuildMember';
import type {User} from '@app/api/models/User';
import {
	type PartialUserChangePropagationDeps,
	propagatePartialUserChange,
} from '@app/api/user/services/PartialUserChangePropagation';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {describe, expect, test, vi} from 'vitest';

const USER_ID = createUserID(1n);
const GUILD_IDS = [createGuildID(10n), createGuildID(11n)];
const USER = {id: USER_ID} as unknown as User;

const USER_PARTIAL: UserPartialResponse = {
	id: USER_ID.toString(),
	username: 'ada',
	discriminator: '0001',
	global_name: null,
	avatar: 'newhash',
	avatar_color: null,
	flags: 0,
};

function createMember(): GuildMember {
	return {
		userId: USER_ID,
		nickname: null,
		avatarHash: null,
		bannerHash: null,
		accentColor: null,
		roleIds: new Set(),
		joinedAt: new Date(0),
		isMute: false,
		isDeaf: false,
		communicationDisabledUntil: null,
		profileFlags: 0,
		mentionFlags: 0,
		isPremiumSanitized: false,
	} as unknown as GuildMember;
}

function createDeps(guildIds: Array<(typeof GUILD_IDS)[number]> = GUILD_IDS) {
	const dispatchGuild = vi.fn().mockResolvedValue(undefined);
	const setUserPartialResponseFromUser = vi.fn().mockResolvedValue(USER_PARTIAL);
	const getUserGuildIds = vi.fn().mockResolvedValue(guildIds);
	const getMember = vi.fn().mockResolvedValue(createMember());
	const deps: PartialUserChangePropagationDeps = {
		userCacheService: {
			setUserPartialResponseFromUser,
			getUserPartialResponse: async () => USER_PARTIAL,
		} as unknown as UserCacheService,
		gatewayService: {dispatchGuild} as unknown as IGatewayService,
		userRepository: {getUserGuildIds},
		guildRepository: {getMember} as unknown as IGuildRepositoryAggregate,
	};
	return {deps, dispatchGuild, setUserPartialResponseFromUser, getUserGuildIds, getMember};
}

describe('propagatePartialUserChange', () => {
	test('invalidates the users service cache and pushes the new partial to every guild', async () => {
		const {deps, dispatchGuild, setUserPartialResponseFromUser} = createDeps();

		await propagatePartialUserChange(deps, USER);

		expect(setUserPartialResponseFromUser).toHaveBeenCalledWith(USER);
		expect(dispatchGuild).toHaveBeenCalledTimes(2);
		for (const [index, guildId] of GUILD_IDS.entries()) {
			const call = dispatchGuild.mock.calls[index]![0];
			expect(call.guildId).toBe(guildId);
			expect(call.event).toBe('GUILD_MEMBER_UPDATE');
			expect((call.data as {user: UserPartialResponse}).user).toEqual(USER_PARTIAL);
		}
	});

	test('dispatches nothing when the user is in no guilds', async () => {
		const {deps, dispatchGuild, getMember} = createDeps([]);

		await propagatePartialUserChange(deps, USER);

		expect(getMember).not.toHaveBeenCalled();
		expect(dispatchGuild).not.toHaveBeenCalled();
	});

	test('skips guilds the user is no longer a member of', async () => {
		const {deps, dispatchGuild, getMember} = createDeps();
		getMember.mockResolvedValueOnce(null);

		await propagatePartialUserChange(deps, USER);

		expect(dispatchGuild).toHaveBeenCalledTimes(1);
		expect(dispatchGuild.mock.calls[0]![0].guildId).toBe(GUILD_IDS[1]);
	});
});
