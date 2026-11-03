// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID} from '@app/api/BrandedTypes';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {createChannel, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {THREADS_FEATURE, THREADS_FEATURE_HEADER, threadsRequest} from '@app/api/channel/tests/ThreadTestUtils';
import {clearChannelThreadsTaintCacheForTesting, insertGuildThreadMarker} from '@app/api/experiment/ChannelThreadsGate';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ChannelOverrideFlags} from '@fluxer/constants/src/ThreadConstants';
import type {UserGuildSettingsResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const repository = new ChannelRepository();

function override(extra: Record<string, unknown> = {}) {
	return {collapsed: false, message_notifications: 1, muted: false, mute_config: null, ...extra};
}

describe('thread user settings', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		clearChannelThreadsTaintCacheForTesting();
	});

	afterAll(async () => {
		clearChannelThreadsTaintCacheForTesting();
		await harness.shutdown();
	});

	async function setup() {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'settings');
		const channel = await createChannel(harness, account.token, guild.id, 'general');

		return {account, guildId: guild.id, channelId: channel.id};
	}

	async function createForum(guildId: string, textChannelId: string): Promise<string> {
		const text = await repository.channelData.findUnique(createChannelID(BigInt(textChannelId)));
		const forumId = (BigInt(textChannelId) + 7n).toString();
		await repository.channelData.upsert({
			...text!.toRow(),
			channel_id: createChannelID(BigInt(forumId)),
			type: ChannelTypes.GUILD_FORUM,
			name: 'forum',
		});
		await insertGuildThreadMarker(createGuildID(BigInt(guildId)), null);
		return forumId;
	}

	function patchSettings(account: TestAccount, guildId: string, body: Record<string, unknown>, capable: boolean) {
		return threadsRequest<UserGuildSettingsResponse>(harness, account.token, {capable})
			.patch(`/users/@me/guilds/${guildId}/settings`)
			.body(body)
			.expect(200)
			.execute();
	}

	describe('override flags', () => {
		it('stores NEW_FORUM_THREADS flags for viewers and masks unknown bits', async () => {
			const s = await setup();
			const response = await patchSettings(
				s.account,
				s.guildId,
				{channel_overrides: {[s.channelId]: override({flags: ChannelOverrideFlags.NEW_FORUM_THREADS_ON | 1})}},
				true,
			);
			expect(response.channel_overrides?.[s.channelId]?.flags).toBe(ChannelOverrideFlags.NEW_FORUM_THREADS_ON);
		});

		it('strips flags from non-viewers, carries stored flags forward and hides them on read', async () => {
			const s = await setup();
			await patchSettings(
				s.account,
				s.guildId,
				{channel_overrides: {[s.channelId]: override({flags: ChannelOverrideFlags.NEW_FORUM_THREADS_OFF})}},
				true,
			);
			const control = await patchSettings(
				s.account,
				s.guildId,
				{channel_overrides: {[s.channelId]: override({muted: true, flags: ChannelOverrideFlags.NEW_FORUM_THREADS_ON})}},
				false,
			);
			expect(control.channel_overrides?.[s.channelId]).toEqual(expect.not.objectContaining({flags: expect.anything()}));
			expect(control.channel_overrides?.[s.channelId]?.muted).toBe(true);
			const viewer = await createBuilder<UserGuildSettingsResponse>(harness, s.account.token)
				.header(THREADS_FEATURE_HEADER, THREADS_FEATURE)
				.patch(`/users/@me/guilds/${s.guildId}/settings`)
				.body({muted: false})
				.expect(200)
				.execute();
			expect(viewer.channel_overrides?.[s.channelId]?.flags).toBe(ChannelOverrideFlags.NEW_FORUM_THREADS_OFF);
		});

		it('ignores malformed flags from non-viewers and rejects them from viewers', async () => {
			const s = await setup();
			const body = {channel_overrides: {[s.channelId]: override({flags: 'bogus'})}};
			await patchSettings(s.account, s.guildId, body, false);
			await createBuilder(harness, s.account.token)
				.patch('/users/@me/guilds/@me/settings')
				.body(body)
				.expect(200)
				.execute();
			await threadsRequest(harness, s.account.token)
				.patch(`/users/@me/guilds/${s.guildId}/settings`)
				.body(body)
				.expect(400)
				.execute();
		});
	});

	describe('forum override omission', () => {
		it('hides forum-keyed overrides from non-viewers in a tainted guild and preserves them on write', async () => {
			const s = await setup();
			const forumId = await createForum(s.guildId, s.channelId);
			await patchSettings(
				s.account,
				s.guildId,
				{channel_overrides: {[s.channelId]: override(), [forumId]: override({muted: true})}},
				true,
			);
			const control = await patchSettings(
				s.account,
				s.guildId,
				{channel_overrides: {[s.channelId]: override({collapsed: true})}},
				false,
			);
			expect(Object.keys(control.channel_overrides ?? {})).toEqual([s.channelId]);
			const viewer = await patchSettings(s.account, s.guildId, {muted: true}, true);
			expect(viewer.channel_overrides?.[forumId]?.muted).toBe(true);
			expect(viewer.channel_overrides?.[s.channelId]?.collapsed).toBe(true);
		});
	});
});
