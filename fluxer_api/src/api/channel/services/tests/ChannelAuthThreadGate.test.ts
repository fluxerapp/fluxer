// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createGuildID, createUserID} from '@app/api/BrandedTypes';
import type {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {ChannelAuthService} from '@app/api/channel/services/channel_data/ChannelAuthService';
import {SYSTEM_THREAD_VIEWER, type ThreadViewer} from '@app/api/experiment/ChannelThreadsGate';
import type {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import type {IGatewayService} from '@app/api/infrastructure/IGatewayService';
import type {Channel} from '@app/api/models/Channel';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {UnknownChannelError} from '@fluxer/errors/src/domains/channel/UnknownChannelError';
import {describe, expect, it} from 'vitest';

const GUILD_ID = createGuildID(100n);
const CHANNEL_ID = createChannelID(300n);
const USER_ID = createUserID(10n);
const REACHED_GATEWAY = new Error('reached the gateway permission lookup');

function createService(type: number): ChannelAuthService {
	const channel = {id: CHANNEL_ID, type, guildId: GUILD_ID, parentId: createChannelID(200n)} as unknown as Channel;
	const channelRepository = {
		channelData: {findUnique: async () => channel},
		threads: {getState: async () => null, getMember: async () => null},
	} as unknown as ChannelRepository;
	const gatewayService = {
		getGuildAuthContext: async () => {
			throw REACHED_GATEWAY;
		},
		getGuildMember: async () => {
			throw REACHED_GATEWAY;
		},
	} as unknown as IGatewayService;
	return new ChannelAuthService(channelRepository, {} as UserRepository, {} as GuildRepository, gatewayService);
}

const capableViewer: ThreadViewer = {kind: 'user', userId: USER_ID, bot: false, capable: true};

describe('getChannelAuthenticated thread gate', () => {
	it('never consults the gate for control channel types', async () => {
		await expect(
			createService(ChannelTypes.GUILD_TEXT).getChannelAuthenticated({
				userId: USER_ID,
				channelId: CHANNEL_ID,
				viewer: {...capableViewer, capable: false},
			}),
		).rejects.toBe(REACHED_GATEWAY);
	});

	it.each([
		ChannelTypes.PUBLIC_THREAD,
		ChannelTypes.PRIVATE_THREAD,
		ChannelTypes.GUILD_FORUM,
		ChannelTypes.GUILD_MEDIA,
	])('hides channel type %i as unknown from incapable viewers', async (type) => {
		await expect(
			createService(type).getChannelAuthenticated({
				userId: USER_ID,
				channelId: CHANNEL_ID,
				viewer: {...capableViewer, capable: false},
			}),
		).rejects.toBeInstanceOf(UnknownChannelError);
	});

	it('hides a thread when the client lacks the capability', async () => {
		await expect(
			createService(ChannelTypes.PUBLIC_THREAD).getChannelAuthenticated({
				userId: USER_ID,
				channelId: CHANNEL_ID,
				viewer: {...capableViewer, capable: false},
			}),
		).rejects.toBeInstanceOf(UnknownChannelError);
	});

	it('lets an active viewer and the system through to the permission checks', async () => {
		const service = createService(ChannelTypes.PUBLIC_THREAD);
		await expect(
			service.getChannelAuthenticated({userId: USER_ID, channelId: CHANNEL_ID, viewer: capableViewer}),
		).rejects.toBe(REACHED_GATEWAY);
		await expect(
			service.getChannelAuthenticated({userId: USER_ID, channelId: CHANNEL_ID, viewer: SYSTEM_THREAD_VIEWER}),
		).rejects.toBe(REACHED_GATEWAY);
	});
});
