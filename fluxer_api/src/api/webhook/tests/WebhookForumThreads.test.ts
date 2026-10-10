// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {ChannelDataRepository} from '@app/api/channel/repositories/ChannelDataRepository';
import {MessageSendService} from '@app/api/channel/services/message/MessageSendService';
import {createChannel, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {GuildAuditLogListResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {WebhookResponse} from '@fluxer/schema/src/domains/webhook/WebhookSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

interface Setup {
	owner: TestAccount;
	guildId: string;
	forum: ChannelResponse;
	webhook: WebhookResponse;
}

describe('webhooks on forum and media channels', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		resetChannelThreadsConfig();
	});

	afterAll(async () => {
		resetChannelThreadsConfig();
		await harness.shutdown();
	});

	async function setup(type: number = ChannelTypes.GUILD_FORUM): Promise<Setup> {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
		const owner = await createTestAccount(harness);
		await ensureSessionStarted(harness, owner.token);
		const guild = await createGuild(harness, owner.token, 'webhook forums');
		const forum = await threadsRequest<ChannelResponse>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({name: 'forum', type, available_tags: [{name: 'open'}, {name: 'staff', moderated: true}]})
			.execute();
		const webhook = await threadsRequest<WebhookResponse>(harness, owner.token)
			.post(`/channels/${forum.id}/webhooks`)
			.body({name: 'poster'})
			.execute();
		return {owner, guildId: guild.id, forum, webhook};
	}

	function execute(s: Setup, body: Record<string, unknown>, query = '?wait=true') {
		return createBuilderWithoutAuth<MessageResponse>(harness)
			.post(`/webhooks/${s.webhook.id}/${s.webhook.token}${query}`)
			.body(body);
	}

	for (const type of [ChannelTypes.GUILD_FORUM, ChannelTypes.GUILD_MEDIA]) {
		it(`creates a post from thread_name in channel type ${type}`, async () => {
			const s = await setup(type);
			const [open] = s.forum.available_tags!;
			const message = await execute(s, {content: 'hi', thread_name: 'from hook', applied_tags: [open!.id]}).execute();
			const thread = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
				.get(`/channels/${message.channel_id}`)
				.execute();
			expect(message.id).toBe(thread.id);
			expect(thread).toMatchObject({
				type: ChannelTypes.PUBLIC_THREAD,
				parent_id: s.forum.id,
				name: 'from hook',
				owner_id: s.webhook.id,
				applied_tags: [open!.id],
			});
			const reply = await execute(s, {content: 'again'}, `?wait=true&thread_id=${thread.id}`).execute();
			expect(reply.channel_id).toBe(thread.id);
		});
	}

	it('rejects an invalid starter before creating the post', async () => {
		const s = await setup();
		const dispatched: Array<string> = [];
		const spy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild').mockImplementation(async (params) => {
			dispatched.push(params.event);
		});
		await execute(s, {thread_name: 'empty'}).expect(400, APIErrorCodes.CANNOT_SEND_EMPTY_MESSAGE).execute();
		spy.mockRestore();
		expect(dispatched).not.toContain('THREAD_CREATE');
		const forum = await threadsRequest<ChannelResponse>(harness, s.owner.token)
			.get(`/channels/${s.forum.id}`)
			.execute();
		expect(forum.last_message_id ?? null).toBeNull();
	});

	it('rolls clients back to the previous forum last_message_id when the starter fails', async () => {
		const s = await setup();
		const first = await execute(s, {content: 'first', thread_name: 'first'}).execute();
		const dispatched: Array<{event: string; data: unknown}> = [];
		const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild').mockImplementation(async (params) => {
			dispatched.push({event: params.event, data: params.data});
		});
		const send = vi
			.spyOn(MessageSendService.prototype, 'sendWebhookMessage')
			.mockRejectedValueOnce(new Error('starter failed'));
		await execute(s, {content: 'second', thread_name: 'second'}).expect(500).execute();
		send.mockRestore();
		dispatch.mockRestore();
		const events = dispatched.map((entry) => entry.event);
		expect(events).toEqual(['THREAD_CREATE', 'CHANNEL_UPDATE', 'THREAD_DELETE']);
		expect(dispatched[1]!.data).toMatchObject({id: s.forum.id, last_message_id: first.channel_id});
		const forum = await threadsRequest<ChannelResponse>(harness, s.owner.token)
			.get(`/channels/${s.forum.id}`)
			.execute();
		expect(forum.last_message_id).toBe(first.channel_id);
	});

	it('enforces the forum target rules', async () => {
		const s = await setup();
		const [, staff] = s.forum.available_tags!;
		await execute(s, {content: 'x'}).expect(400, APIErrorCodes.WEBHOOK_FORUM_TARGET_REQUIRED).execute();
		const message = await execute(s, {content: 'x', thread_name: 'post'}).execute();
		await execute(s, {content: 'x', thread_name: 'post'}, `?thread_id=${message.channel_id}`)
			.expect(400, APIErrorCodes.WEBHOOK_FORUM_TARGET_CONFLICT)
			.execute();
		await execute(s, {content: 'x', thread_name: 'post', applied_tags: [staff!.id]})
			.expect(403)
			.execute();
		await createBuilderWithoutAuth(harness)
			.post(`/webhooks/${s.webhook.id}/${s.webhook.token}/slack`)
			.body({text: 'hi'})
			.expect(400, APIErrorCodes.WEBHOOK_SERVICE_FORUM_UNSUPPORTED)
			.execute();
		const text = await createChannel(harness, s.owner.token, s.guildId, 'text');
		const textHook = await threadsRequest<WebhookResponse>(harness, s.owner.token)
			.post(`/channels/${text.id}/webhooks`)
			.body({name: 'text'})
			.execute();
		await createBuilderWithoutAuth(harness)
			.post(`/webhooks/${textHook.id}/${textHook.token}?wait=true`)
			.body({content: 'x', thread_name: 'nope'})
			.expect(400, APIErrorCodes.WEBHOOK_THREAD_NAME_REQUIRES_FORUM)
			.execute();
	});

	it('omits a webhook moved onto a forum from the audit log webhooks for non-viewers', async () => {
		const s = await setup();
		const text = await createChannel(harness, s.owner.token, s.guildId, 'text');
		const hook = await threadsRequest<WebhookResponse>(harness, s.owner.token)
			.post(`/channels/${text.id}/webhooks`)
			.body({name: 'mover'})
			.execute();
		await threadsRequest(harness, s.owner.token).patch(`/webhooks/${hook.id}`).body({channel_id: s.forum.id}).execute();
		const controlLog = await threadsRequest<GuildAuditLogListResponse>(harness, s.owner.token, {capable: false})
			.get(`/guilds/${s.guildId}/audit-logs`)
			.execute();
		expect(controlLog.audit_log_entries.some((entry) => entry.target_id === hook.id)).toBe(true);
		expect(controlLog.webhooks.map((webhook) => webhook.id)).not.toContain(hook.id);
		expect(controlLog.webhooks.some((webhook) => webhook.channel_id === s.forum.id)).toBe(false);
	});

	it('hides forum webhooks once the guild leaves the experiment and ignores forum fields outside it', async () => {
		const s = await setup();
		await setChannelThreadsConfig({enabled: false});
		await createBuilderWithoutAuth(harness)
			.get(`/webhooks/${s.webhook.id}/${s.webhook.token}`)
			.expect(404, APIErrorCodes.UNKNOWN_WEBHOOK)
			.execute();
		for (const capable of [true, false]) {
			await threadsRequest(harness, s.owner.token, {capable})
				.get(`/webhooks/${s.webhook.id}`)
				.expect(404, APIErrorCodes.UNKNOWN_WEBHOOK)
				.execute();
			await threadsRequest(harness, s.owner.token, {capable})
				.patch(`/webhooks/${s.webhook.id}`)
				.body({name: 'renamed'})
				.expect(404, APIErrorCodes.UNKNOWN_WEBHOOK)
				.execute();
			await threadsRequest(harness, s.owner.token, {capable})
				.delete(`/webhooks/${s.webhook.id}`)
				.expect(404, APIErrorCodes.UNKNOWN_WEBHOOK)
				.execute();
		}
		await execute(s, {content: 'x', thread_name: 'post'}).expect(404, APIErrorCodes.UNKNOWN_WEBHOOK).execute();
		await createBuilderWithoutAuth(harness)
			.post(`/webhooks/${s.webhook.id}/${s.webhook.token}/slack`)
			.body({text: 'hi'})
			.expect(404, APIErrorCodes.UNKNOWN_WEBHOOK)
			.execute();
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'plain');
		const hook = await threadsRequest<WebhookResponse>(harness, owner.token)
			.post(`/channels/${guild.system_channel_id}/webhooks`)
			.body({name: 'plain'})
			.execute();
		const message = await createBuilderWithoutAuth<MessageResponse>(harness)
			.post(`/webhooks/${hook.id}/${hook.token}?wait=true`)
			.body({content: 'x', thread_name: 'ignored', applied_tags: ['1']})
			.execute();
		expect(message.channel_id).toBe(guild.system_channel_id);
	});

	it('reads no channels for webhook lookups in an untainted guild after the experiment was enabled', async () => {
		await setChannelThreadsConfig({enabled: true, guild_basis_points: 0, user_basis_points: 10000});
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'control');
		const hook = await threadsRequest<WebhookResponse>(harness, owner.token)
			.post(`/channels/${guild.system_channel_id}/webhooks`)
			.body({name: 'control'})
			.execute();
		const readChannelIds: Array<string> = [];
		const readChannel = ChannelDataRepository.prototype.findUnique;
		const findUnique = vi.spyOn(ChannelDataRepository.prototype, 'findUnique').mockImplementation(function (
			this: ChannelDataRepository,
			channelId,
		) {
			if (!new Error().stack?.includes('NoopGatewayService')) readChannelIds.push(channelId.toString());
			return readChannel.call(this, channelId);
		});
		const listChannels = vi.spyOn(ChannelDataRepository.prototype, 'listChannels');
		await createBuilderWithoutAuth(harness).get(`/webhooks/${hook.id}/${hook.token}`).execute();
		await threadsRequest(harness, owner.token).get(`/guilds/${guild.id}/webhooks`).execute();
		await threadsRequest(harness, owner.token).get(`/webhooks/${hook.id}`).execute();
		const listCalls = listChannels.mock.calls.length;
		findUnique.mockRestore();
		listChannels.mockRestore();
		expect(readChannelIds).not.toContain(guild.system_channel_id);
		expect(listCalls).toBe(0);
	});
});
