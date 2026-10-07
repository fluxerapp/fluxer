// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID} from '@app/api/BrandedTypes';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {createChannel, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	THREADS_FEATURE,
	THREADS_FEATURE_HEADER,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {insertGuildThreadMarker} from '@app/api/experiment/ChannelThreadsGate';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ChannelOverrideFlags} from '@fluxer/constants/src/ThreadConstants';
import type {UserGuildSettingsResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

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
		resetChannelThreadsConfig();
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		resetChannelThreadsConfig();
		await harness.shutdown();
	});

	async function setup() {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'settings');
		const channel = await createChannel(harness, account.token, guild.id, 'general');
		await setChannelThreadsConfig({
			enabled: true,
			enabled_guild_ids: [guild.id],
			included_user_ids: [account.userId],
		});
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
			const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchPresence');
			const control = await patchSettings(
				s.account,
				s.guildId,
				{channel_overrides: {[s.channelId]: override({muted: true, flags: ChannelOverrideFlags.NEW_FORUM_THREADS_ON})}},
				false,
			);
			expect(control.channel_overrides?.[s.channelId]).toEqual(expect.not.objectContaining({flags: expect.anything()}));
			expect(control.channel_overrides?.[s.channelId]?.muted).toBe(true);
			const updates = dispatch.mock.calls
				.map(([params]) => params)
				.filter((params) => params.event === 'USER_GUILD_SETTINGS_UPDATE');
			const viewer = await createBuilder<UserGuildSettingsResponse>(harness, s.account.token)
				.header(THREADS_FEATURE_HEADER, THREADS_FEATURE)
				.patch(`/users/@me/guilds/${s.guildId}/settings`)
				.body({muted: false})
				.expect(200)
				.execute();
			expect(viewer.channel_overrides?.[s.channelId]?.flags).toBe(ChannelOverrideFlags.NEW_FORUM_THREADS_OFF);
			expect(updates).toHaveLength(2);
			expect(updates[0]?.data).not.toHaveProperty('__thread_scoped');
			expect(updates[0]?.data).toMatchObject({__thread_unscoped: s.guildId});
			expect(JSON.stringify(updates[0]?.data)).not.toContain('"flags"');
			expect(updates[1]?.data).toMatchObject({__thread_scoped: s.guildId});
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

		it('sends a single settings update when no override carries thread data', async () => {
			const s = await setup();
			const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchPresence');
			await patchSettings(s.account, s.guildId, {channel_overrides: {[s.channelId]: override()}}, true);
			const updates = dispatch.mock.calls.filter(([params]) => params.event === 'USER_GUILD_SETTINGS_UPDATE');
			expect(updates).toHaveLength(1);
			expect(updates[0]?.[0].data).not.toHaveProperty('__thread_scoped');
			expect(updates[0]?.[0].data).not.toHaveProperty('__thread_unscoped');
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
			await setChannelThreadsConfig({enabled: false});
			const killed = await patchSettings(s.account, s.guildId, {muted: false}, true);
			expect(Object.keys(killed.channel_overrides ?? {})).toEqual([s.channelId]);
		});
	});

	describe('device capability', () => {
		interface PushSubscriptionsRpc {
			data: Record<string, Array<{subscription_id: string; thread_channels?: boolean}>>;
		}

		async function subscriptions(account: TestAccount) {
			const response = await createBuilder<PushSubscriptionsRpc>(harness, '')
				.post('/test/rpc-session-init')
				.body({type: 'get_push_subscriptions', user_ids: [account.userId]})
				.expect(200)
				.execute();
			return response.data[account.userId] ?? [];
		}

		it('records the thread capability of a mobile device only when the features header carries it', async () => {
			const account = await createTestAccount(harness);
			const capable = await threadsRequest<{device_id: string}>(harness, account.token)
				.post('/users/@me/mobile-devices')
				.body({platform: 'android_fcm', token: 'capable-device-token'})
				.execute();
			const legacy = await threadsRequest<{device_id: string}>(harness, account.token, {capable: false})
				.post('/users/@me/mobile-devices')
				.body({platform: 'android_fcm', token: 'legacy-device-token'})
				.execute();
			const byId = new Map((await subscriptions(account)).map((sub) => [sub.subscription_id, sub]));
			expect(byId.get(capable.device_id)?.thread_channels).toBe(true);
			expect(byId.get(legacy.device_id)).toBeDefined();
			expect(byId.get(legacy.device_id)).not.toHaveProperty('thread_channels');
		});

		it('clears the thread capability on a header-less re-registration only once the experiment was ever enabled', async () => {
			const account = await createTestAccount(harness);
			const register = (capable: boolean) =>
				threadsRequest<{device_id: string}>(harness, account.token, {capable})
					.post('/users/@me/mobile-devices')
					.body({platform: 'android_fcm', token: 'downgraded-device-token'})
					.execute();
			const capability = async (deviceId: string) =>
				(await subscriptions(account)).find((sub) => sub.subscription_id === deviceId)?.thread_channels;

			const {device_id} = await register(true);
			await register(false);
			expect(await capability(device_id)).toBe(true);

			await setChannelThreadsConfig({enabled: true, included_user_ids: [account.userId]});
			await register(true);
			expect(await capability(device_id)).toBe(true);
			await register(false);
			expect(await capability(device_id)).toBeUndefined();
		});
	});
});
