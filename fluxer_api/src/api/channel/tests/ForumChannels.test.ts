// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	addMemberRole,
	createChannel,
	createChannelInvite,
	createGuild,
	createRole,
} from '@app/api/channel/tests/ChannelTestUtils';
import {threadsRequest} from '@app/api/channel/tests/ThreadTestUtils';
import {clearChannelThreadsTaintCacheForTesting} from '@app/api/experiment/ChannelThreadsGate';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {StartForumThreadResponse} from '@fluxer/schema/src/domains/channel/ForumRequestSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface Setup {
	owner: TestAccount;
	member: TestAccount;
	guildId: string;
	forumId: string;
}

describe('forum and media channels', () => {
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

	async function createForum(
		token: string,
		guildId: string,
		body: Record<string, unknown> = {},
		type: number = ChannelTypes.GUILD_FORUM,
	): Promise<ChannelResponse> {
		return threadsRequest<ChannelResponse>(harness, token)
			.post(`/guilds/${guildId}/channels`)
			.body({name: 'forum', type, ...body})
			.execute();
	}

	async function setup(forumBody: Record<string, unknown> = {}): Promise<Setup> {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		await ensureSessionStarted(harness, owner.token);
		await ensureSessionStarted(harness, member.token);
		const guild = await createGuild(harness, owner.token, 'forums');
		const general = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, general.id);
		await acceptInvite(harness, member.token, invite.code);
		const forum = await createForum(owner.token, guild.id, forumBody);
		return {owner, member, guildId: guild.id, forumId: forum.id};
	}

	async function post(
		token: string,
		forumId: string,
		body: Record<string, unknown> = {},
		status = 201,
	): Promise<StartForumThreadResponse> {
		return threadsRequest<StartForumThreadResponse>(harness, token)
			.post(`/channels/${forumId}/threads`)
			.body({name: 'post', message: {content: 'hello'}, ...body})
			.expect(status)
			.execute();
	}

	describe('control arm', () => {
		it('rejects creating forums without the client capability', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'control');
			const body = {name: 'forum', type: ChannelTypes.GUILD_FORUM, available_tags: [{name: 'a'}]};
			const control = await harness.requestJson({
				path: `/guilds/${guild.id}/channels`,
				method: 'POST',
				headers: {Authorization: owner.token},
				body,
			});
			expect(control.status).toBe(400);
		});
	});

	describe('settings', () => {
		it('keeps every tag when tag creates race', async () => {
			const s = await setup();
			const names = ['a', 'b', 'c', 'd', 'e'];
			await Promise.all(
				names.map((name) =>
					threadsRequest<ChannelResponse>(harness, s.owner.token)
						.post(`/channels/${s.forumId}/tags`)
						.body({name})
						.execute(),
				),
			);
			const forum = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.get(`/channels/${s.forumId}`)
				.execute();
			expect(forum.available_tags!.map((tag) => tag.name).sort()).toEqual(names);
		});
	});

	describe('posts', () => {
		it('refuses a limited account before creating the post', async () => {
			const s = await setup();
			await createBuilder(harness, '')
				.post(`/test/users/${s.member.userId}/security-flags`)
				.body({set_flags: ['ACCOUNT_LIMITED']})
				.execute();
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.forumId}/threads`)
				.body({name: 'limited', message: {content: 'hello'}})
				.expect(403, APIErrorCodes.ACCOUNT_LIMITED)
				.execute();
			const forum = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.get(`/channels/${s.forumId}`)
				.execute();
			expect(forum.last_message_id ?? null).toBeNull();
		});

		it('rolls back a post whose starter fails after the thread exists', async () => {
			const s = await setup({rate_limit_per_user: 60});
			const first = await post(s.owner.token, s.forumId);
			await post(s.member.token, s.forumId, {message: {content: 'x', sticker_ids: ['1']}}, 400);
			const forum = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.get(`/channels/${s.forumId}`)
				.execute();
			expect(forum.last_message_id).toBe(first.id);
			await post(s.member.token, s.forumId);
		});

		it('requires send messages on the forum and refuses private posts', async () => {
			const s = await setup();
			await post(s.member.token, s.forumId, {type: ChannelTypes.PRIVATE_THREAD}, 400);
			const role = await createRole(harness, s.owner.token, s.guildId, {name: 'muted'});
			await addMemberRole(harness, s.owner.token, s.guildId, s.member.userId, role.id);
			await threadsRequest(harness, s.owner.token)
				.put(`/channels/${s.forumId}/permissions/${role.id}`)
				.body({type: 0, allow: '0', deny: Permissions.SEND_MESSAGES.toString()})
				.expect(204)
				.execute();
			await post(s.member.token, s.forumId, {}, 403);
		});
	});
});
