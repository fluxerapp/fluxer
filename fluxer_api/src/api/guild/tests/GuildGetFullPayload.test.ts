// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	createChannel,
	createChannelInvite,
	createGuild,
	createPermissionOverwrite,
	getGuild,
} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('GET /guilds/:guild_id full payload', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('includes gateway state collections and filters channels for the requesting user', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Full Guild Payload');
		const publicChannel = await createChannel(harness, owner.token, guild.id, 'public-room');
		const hiddenChannel = await createChannel(harness, owner.token, guild.id, 'owner-room');
		await createPermissionOverwrite(harness, owner.token, hiddenChannel.id, guild.id, {
			type: 0,
			allow: '0',
			deny: Permissions.VIEW_CHANNEL.toString(),
		});
		const invite = await createChannelInvite(harness, owner.token, publicChannel.id);
		await acceptInvite(harness, member.token, invite.code);
		const ownerGuild = await getGuild(harness, owner.token, guild.id);
		expect(Array.isArray(ownerGuild.roles)).toBe(true);
		expect(Array.isArray(ownerGuild.emojis)).toBe(true);
		expect(Array.isArray(ownerGuild.stickers)).toBe(true);
		expect(Array.isArray(ownerGuild.channels)).toBe(true);
		expect(ownerGuild.member_count).toBe(2);
		expect(ownerGuild.online_count).toBe(0);
		const ownerChannelIds = new Set(ownerGuild.channels?.map((channel) => channel.id) ?? []);
		expect(ownerChannelIds.has(publicChannel.id)).toBe(true);
		expect(ownerChannelIds.has(hiddenChannel.id)).toBe(true);
		const memberGuild = await getGuild(harness, member.token, guild.id);
		const memberChannelIds = new Set(memberGuild.channels?.map((channel) => channel.id) ?? []);
		expect(memberChannelIds.has(publicChannel.id)).toBe(true);
		expect(memberChannelIds.has(hiddenChannel.id)).toBe(false);
	});
});
