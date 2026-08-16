// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {describe, expect, it} from 'vitest';
import type {User} from '../../models/User';
import {GuildOperationsService} from '../services/data/GuildOperationsService';

const REACHED_GUILD_COUNT = new Error('reached guild count lookup');

function createServiceStoppingAtGuildCount(): GuildOperationsService {
	const guildRepository = {
		countUserGuilds: (): never => {
			throw REACHED_GUILD_COUNT;
		},
	};
	const unused = null as never;
	return new GuildOperationsService(
		guildRepository as never,
		unused,
		unused,
		unused,
		unused,
		unused,
		unused,
		unused,
		unused,
		unused,
		unused,
		unused,
	);
}

function createUser(overrides: {emailVerified: boolean; isBot?: boolean}): User {
	return {
		id: 1n,
		isBot: overrides.isBot ?? false,
		emailVerified: overrides.emailVerified,
		isUnclaimedAccount: () => false,
	} as unknown as User;
}

describe('guild creation email verification', () => {
	it('does not gate guild creation on email verification inside the domain operation', async () => {
		const service = createServiceStoppingAtGuildCount();
		await expect(
			service.createGuild({user: createUser({emailVerified: false}), data: {name: 'Stock Community'} as never}),
		).rejects.toBe(REACHED_GUILD_COUNT);
	});

	it('reaches the same point for a verified user', async () => {
		const service = createServiceStoppingAtGuildCount();
		await expect(
			service.createGuild({user: createUser({emailVerified: true}), data: {name: 'Stock Community'} as never}),
		).rejects.toBe(REACHED_GUILD_COUNT);
	});

	it('still rejects bots before any guild count lookup', async () => {
		const service = createServiceStoppingAtGuildCount();
		await expect(
			service.createGuild({
				user: createUser({emailVerified: true, isBot: true}),
				data: {name: 'Stock Community'} as never,
			}),
		).rejects.toMatchObject({code: APIErrorCodes.BOTS_CANNOT_CREATE_GUILDS});
	});
});
