// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createMultipartFormData} from '@app/api/channel/tests/AttachmentTestUtils';
import {
	acceptInvite,
	addMemberRole,
	createChannel,
	createChannelInvite,
	createGuild,
	createRole,
} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	THREADS_FEATURE,
	THREADS_FEATURE_HEADER,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {createEmoji, getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {ChannelFlags} from '@fluxer/constants/src/ThreadConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import type {
	StartForumThreadResponse,
	ThreadPostDataResponse,
} from '@fluxer/schema/src/domains/channel/ForumRequestSchemas';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

interface Setup {
	owner: TestAccount;
	member: TestAccount;
	guildId: string;
	forumId: string;
}

const PNG_1X1 = Buffer.from(
	'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==',
	'base64',
);

describe('forum and media channels', () => {
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
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
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

	async function tags(token: string, forumId: string, names: Array<{name: string; moderated?: boolean}>) {
		return threadsRequest<ChannelResponse>(harness, token)
			.patch(`/channels/${forumId}`)
			.body({available_tags: names})
			.execute();
	}

	describe('control arm', () => {
		it('rejects a forum create body exactly as before for callers outside the experiment', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'control');
			const body = {name: 'forum', type: ChannelTypes.GUILD_FORUM, available_tags: [{name: 'a'}]};
			const control = await harness.requestJson({
				path: `/guilds/${guild.id}/channels`,
				method: 'POST',
				headers: {Authorization: owner.token},
				body,
			});
			const capable = await harness.requestJson({
				path: `/guilds/${guild.id}/channels`,
				method: 'POST',
				headers: {Authorization: owner.token, [THREADS_FEATURE_HEADER]: THREADS_FEATURE},
				body,
			});
			expect(control.status).toBe(400);
			expect(capable.status).toBe(400);
			expect(await capable.text()).toBe(await control.text());
		});

		it('strips the thread defaults of a text channel outside the experiment', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'control');
			const channel = await threadsRequest<ChannelResponse>(harness, owner.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'text', type: ChannelTypes.GUILD_TEXT, default_auto_archive_duration: 60})
				.execute();
			expect(channel).not.toHaveProperty('default_auto_archive_duration');
			const patched = await threadsRequest<ChannelResponse>(harness, owner.token)
				.patch(`/channels/${channel.id}`)
				.body({default_thread_rate_limit_per_user: 30})
				.execute();
			expect(patched).not.toHaveProperty('default_thread_rate_limit_per_user');
		});

		it('hides forums from members who are not in the experiment', async () => {
			const s = await setup();
			await setChannelThreadsConfig({...ALL_THREADS_ACTIVE, excluded_user_ids: [s.member.userId]});
			await threadsRequest(harness, s.member.token).get(`/channels/${s.forumId}`).expect(404).execute();
			const channels = await threadsRequest<Array<ChannelResponse>>(harness, s.member.token)
				.get(`/guilds/${s.guildId}/channels`)
				.execute();
			expect(channels.some((channel) => channel.id === s.forumId)).toBe(false);
			await createBuilder(harness, s.member.token)
				.post(`/channels/${s.forumId}/tags`)
				.body({name: 'x'})
				.expect(404)
				.execute();
		});
	});

	describe('settings', () => {
		it('creates a forum with its settings and serves them on reads', async () => {
			const s = await setup({
				topic: 'x'.repeat(2000),
				available_tags: [
					{name: 'bug', emoji_name: '🐛'},
					{name: 'staff', moderated: true},
				],
				default_sort_order: 1,
				default_forum_layout: 2,
				default_tag_setting: 'match_all',
				default_thread_rate_limit_per_user: 10,
				default_auto_archive_duration: 1440,
				default_reaction_emoji: {emoji_name: '👍'},
				nsfw: true,
				rate_limit_per_user: 15,
			});
			const forum = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.get(`/channels/${s.forumId}`)
				.execute();
			expect(forum.type).toBe(ChannelTypes.GUILD_FORUM);
			expect(forum.topic).toHaveLength(2000);
			expect(forum.available_tags?.map((tag) => [tag.name, tag.moderated, tag.emoji_name])).toEqual([
				['bug', false, '🐛'],
				['staff', true, null],
			]);
			expect(forum).toMatchObject({
				flags: 0,
				default_sort_order: 1,
				default_forum_layout: 2,
				default_tag_setting: 'match_all',
				default_thread_rate_limit_per_user: 10,
				default_auto_archive_duration: 1440,
				default_reaction_emoji: {emoji_id: null, emoji_name: '👍'},
				nsfw: true,
				rate_limit_per_user: 15,
			});
			expect(forum).not.toHaveProperty('icon');
			expect(forum).not.toHaveProperty('owner_id');
			const listed = await threadsRequest<Array<ChannelResponse>>(harness, s.owner.token)
				.get(`/guilds/${s.guildId}/channels`)
				.execute();
			expect(listed.find((channel) => channel.id === s.forumId)?.available_tags).toHaveLength(2);
		});

		it('resets the layout and thread slowmode defaults on explicit null', async () => {
			const s = await setup({default_forum_layout: null, default_thread_rate_limit_per_user: null});
			const set = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.patch(`/channels/${s.forumId}`)
				.body({default_forum_layout: 2, default_thread_rate_limit_per_user: 10})
				.execute();
			expect(set).toMatchObject({default_forum_layout: 2, default_thread_rate_limit_per_user: 10});
			const cleared = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.patch(`/channels/${s.forumId}`)
				.body({default_forum_layout: null, default_thread_rate_limit_per_user: null})
				.execute();
			expect(cleared).toMatchObject({default_forum_layout: 0, default_thread_rate_limit_per_user: 0});
		});

		it('validates tags and flags', async () => {
			const s = await setup();
			const tooMany = Array.from({length: 21}, (_, index) => ({name: `t${index}`}));
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${s.forumId}`)
				.body({available_tags: tooMany})
				.expect(400, APIErrorCodes.MAX_FORUM_TAGS)
				.execute();
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${s.forumId}`)
				.body({available_tags: [{name: 'a'}, {name: 'a'}]})
				.expect(400, APIErrorCodes.FORUM_TAG_NAMES_MUST_BE_UNIQUE)
				.execute();
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${s.forumId}`)
				.body({available_tags: [{id: '123', name: 'a'}]})
				.expect(404, APIErrorCodes.UNKNOWN_FORUM_TAG)
				.execute();
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${s.forumId}`)
				.body({available_tags: [{name: 'a', emoji_name: 'hello'}]})
				.expect(400, APIErrorCodes.INVALID_FORM_BODY)
				.execute();
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${s.forumId}`)
				.body({default_reaction_emoji: {emoji_name: 'hello'}})
				.expect(400, APIErrorCodes.INVALID_FORM_BODY)
				.execute();
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${s.forumId}`)
				.body({available_tags: [{name: 'mods', moderated: true}], flags: ChannelFlags.REQUIRE_TAG})
				.expect(400, APIErrorCodes.NO_TAGS_AVAILABLE_TO_NON_MODERATORS)
				.execute();
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${s.forumId}`)
				.body({flags: ChannelFlags.HIDE_MEDIA_DOWNLOAD_OPTIONS})
				.expect(400, APIErrorCodes.HIDE_MEDIA_DOWNLOAD_OPTION_MEDIA_ONLY)
				.execute();
			const media = await createForum(s.owner.token, s.guildId, {flags: ChannelFlags.HIDE_MEDIA_DOWNLOAD_OPTIONS}, 16);
			expect(media.flags).toBe(ChannelFlags.HIDE_MEDIA_DOWNLOAD_OPTIONS);
			expect(media).not.toHaveProperty('default_forum_layout');
		});

		it('keeps tag ids across edits and exposes the tag routes', async () => {
			const s = await setup();
			const first = await tags(s.owner.token, s.forumId, [{name: 'a'}, {name: 'b'}]);
			const [a, b] = first.available_tags!;
			const kept = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.patch(`/channels/${s.forumId}`)
				.body({available_tags: [{id: b!.id, name: 'b2'}, {name: 'c'}]})
				.execute();
			expect(kept.available_tags![0]).toMatchObject({id: b!.id, name: 'b2'});
			expect(kept.available_tags!.some((tag) => tag.id === a!.id)).toBe(false);
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${s.forumId}`)
				.body({
					available_tags: [
						{id: b!.id, name: 'x'},
						{id: b!.id, name: 'y'},
					],
				})
				.expect(404, APIErrorCodes.UNKNOWN_FORUM_TAG)
				.execute();
			const created = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.post(`/channels/${s.forumId}/tags`)
				.body({name: 'd', moderated: true})
				.execute();
			const d = created.available_tags!.find((tag) => tag.name === 'd')!;
			const updated = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.put(`/channels/${s.forumId}/tags/${d.id}`)
				.body({name: 'e'})
				.execute();
			expect(updated.available_tags!.find((tag) => tag.id === d.id)).toMatchObject({name: 'e', moderated: false});
			const deleted = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.delete(`/channels/${s.forumId}/tags/${d.id}`)
				.execute();
			expect(deleted.available_tags!.some((tag) => tag.id === d.id)).toBe(false);
			await threadsRequest(harness, s.owner.token)
				.delete(`/channels/${s.forumId}/tags/${d.id}`)
				.expect(404, APIErrorCodes.UNKNOWN_FORUM_TAG)
				.execute();
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.forumId}/tags`)
				.body({name: 'x'})
				.expect(403)
				.execute();
		});

		it('keeps tag edits working after a tag emoji is deleted', async () => {
			const s = await setup();
			const emoji = await createEmoji(harness, s.owner.token, s.guildId, {name: 'bug', image: getPngDataUrl()});
			const first = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.patch(`/channels/${s.forumId}`)
				.body({available_tags: [{name: 'bug', emoji_id: emoji.id}, {name: 'other'}]})
				.execute();
			const [bug, other] = first.available_tags!;
			await createBuilder(harness, s.owner.token)
				.delete(`/guilds/${s.guildId}/emojis/${emoji.id}`)
				.expect(204)
				.execute();
			await threadsRequest(harness, s.owner.token)
				.post(`/channels/${s.forumId}/tags`)
				.body({name: 'new'})
				.expect(200)
				.execute();
			await threadsRequest(harness, s.owner.token)
				.delete(`/channels/${s.forumId}/tags/${other!.id}`)
				.expect(200)
				.execute();
			const kept = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.patch(`/channels/${s.forumId}`)
				.body({available_tags: [{id: bug!.id, name: 'bug2', emoji_id: emoji.id}]})
				.execute();
			expect(kept.available_tags).toEqual([expect.objectContaining({id: bug!.id, name: 'bug2', emoji_id: emoji.id})]);
			await threadsRequest(harness, s.owner.token)
				.post(`/channels/${s.forumId}/tags`)
				.body({name: 'again', emoji_id: emoji.id})
				.expect(404, APIErrorCodes.UNKNOWN_EMOJI)
				.execute();
		});

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

		it('stores thread defaults on text channels and copies only the rate limit onto new threads', async () => {
			const s = await setup();
			const text = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.post(`/guilds/${s.guildId}/channels`)
				.body({name: 'text', type: ChannelTypes.GUILD_TEXT, default_auto_archive_duration: 60})
				.execute();
			expect(text.default_auto_archive_duration).toBe(60);
			const patched = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.patch(`/channels/${text.id}`)
				.body({default_thread_rate_limit_per_user: 30})
				.execute();
			expect(patched).toMatchObject({default_auto_archive_duration: 60, default_thread_rate_limit_per_user: 30});
			const thread = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
				.post(`/channels/${text.id}/threads`)
				.body({name: 't', type: ChannelTypes.PUBLIC_THREAD})
				.expect(201)
				.execute();
			expect(thread.rate_limit_per_user).toBe(30);
			expect(thread.thread_metadata?.auto_archive_duration).toBe(4320);
		});
	});

	describe('posts', () => {
		it('creates a post whose first message shares the thread id', async () => {
			const s = await setup();
			const created = await post(s.member.token, s.forumId);
			expect(created.type).toBe(ChannelTypes.PUBLIC_THREAD);
			expect(created.parent_id).toBe(s.forumId);
			expect(created.message?.id).toBe(created.id);
			expect(created.message?.channel_id).toBe(created.id);
			expect(created.applied_tags).toEqual([]);
			expect(created.message_count).toBe(0);
			const forum = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.get(`/channels/${s.forumId}`)
				.execute();
			expect(forum.last_message_id).toBe(created.id);
			const messages = await threadsRequest<Array<{id: string}>>(harness, s.member.token)
				.get(`/channels/${created.id}/messages`)
				.execute();
			expect(messages.map((message) => message.id)).toEqual([created.id]);
			const media = await createForum(s.owner.token, s.guildId, {}, ChannelTypes.GUILD_MEDIA);
			const mediaPost = await post(s.member.token, media.id);
			expect(mediaPost).toMatchObject({type: ChannelTypes.PUBLIC_THREAD, parent_id: media.id, applied_tags: []});
			expect(mediaPost.message?.id).toBe(mediaPost.id);
		});

		it('applies tag rules on create', async () => {
			const s = await setup();
			const forum = await tags(s.owner.token, s.forumId, [{name: 'open'}, {name: 'staff', moderated: true}]);
			const [open, staff] = forum.available_tags!;
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${s.forumId}`)
				.body({flags: ChannelFlags.REQUIRE_TAG})
				.execute();
			await post(s.member.token, s.forumId, {}, 400);
			await post(s.member.token, s.forumId, {applied_tags: ['1']}, 404);
			await post(s.member.token, s.forumId, {applied_tags: [staff!.id]}, 403);
			const tagged = await post(s.member.token, s.forumId, {applied_tags: [open!.id]});
			expect(tagged.applied_tags).toEqual([open!.id]);
			const moderated = await post(s.owner.token, s.forumId, {applied_tags: [staff!.id]});
			expect(moderated.applied_tags).toEqual([staff!.id]);
			await threadsRequest(harness, s.member.token)
				.patch(`/channels/${tagged.id}`)
				.body({applied_tags: [open!.id, staff!.id]})
				.expect(403)
				.execute();
			const retagged = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
				.patch(`/channels/${tagged.id}`)
				.body({applied_tags: [staff!.id]})
				.execute();
			expect(retagged.applied_tags).toEqual([staff!.id]);
		});

		it('drops a deleted tag when the post tags are sent back unchanged', async () => {
			const s = await setup();
			const forum = await tags(s.owner.token, s.forumId, [{name: 'keep'}, {name: 'gone'}]);
			const [keep, gone] = forum.available_tags!;
			const created = await post(s.member.token, s.forumId, {applied_tags: [keep!.id, gone!.id]});
			await threadsRequest(harness, s.owner.token).delete(`/channels/${s.forumId}/tags/${gone!.id}`).execute();
			const read = await threadsRequest<ThreadChannelResponse>(harness, s.member.token)
				.get(`/channels/${created.id}`)
				.execute();
			expect(read.applied_tags).toEqual([keep!.id]);
			const edited = await threadsRequest<ThreadChannelResponse>(harness, s.member.token)
				.patch(`/channels/${created.id}`)
				.body({applied_tags: [keep!.id, gone!.id]})
				.execute();
			expect(edited.applied_tags).toEqual([keep!.id]);
			await threadsRequest(harness, s.member.token)
				.patch(`/channels/${created.id}`)
				.body({applied_tags: [gone!.id]})
				.expect(404, APIErrorCodes.UNKNOWN_FORUM_TAG)
				.execute();
		});

		it('refuses a limited account before creating the post', async () => {
			const s = await setup();
			await createBuilder(harness, '')
				.post(`/test/users/${s.member.userId}/security-flags`)
				.body({set_flags: ['ACCOUNT_LIMITED']})
				.execute();
			const dispatched: Array<string> = [];
			const spy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild').mockImplementation(async (params) => {
				dispatched.push(params.event);
			});
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.forumId}/threads`)
				.body({name: 'limited', message: {content: 'hello'}})
				.expect(403, APIErrorCodes.ACCOUNT_LIMITED)
				.execute();
			spy.mockRestore();
			expect(dispatched.filter((event) => event.startsWith('THREAD_'))).toEqual([]);
			const forum = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.get(`/channels/${s.forumId}`)
				.execute();
			expect(forum.last_message_id ?? null).toBeNull();
		});

		it('rejects an invalid starter before creating the post', async () => {
			const s = await setup({rate_limit_per_user: 60});
			const dispatched: Array<string> = [];
			const spy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild').mockImplementation(async (params) => {
				dispatched.push(params.event);
			});
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.forumId}/threads`)
				.body({name: 'empty', message: {}})
				.expect(400, APIErrorCodes.CANNOT_SEND_EMPTY_MESSAGE)
				.execute();
			spy.mockRestore();
			expect(dispatched).not.toContain('THREAD_CREATE');
			const forum = await threadsRequest<ChannelResponse>(harness, s.owner.token)
				.get(`/channels/${s.forumId}`)
				.execute();
			expect(forum.last_message_id ?? null).toBeNull();
			await post(s.member.token, s.forumId);
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

		it('requires a first message on a JSON post', async () => {
			const s = await setup();
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.forumId}/threads`)
				.body({name: 'post'})
				.expect(400, APIErrorCodes.INVALID_FORM_BODY)
				.execute();
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

		it('accepts a multipart post with a file', async () => {
			const s = await setup();
			const {body, contentType} = createMultipartFormData(
				{name: 'files', message: {content: 'with file', attachments: [{id: 0, filename: 'a.png'}]}},
				[{index: 0, filename: 'a.png', data: PNG_1X1}],
			);
			const response = await harness.app.request(`/channels/${s.forumId}/threads`, {
				method: 'POST',
				headers: {
					Authorization: s.member.token,
					[THREADS_FEATURE_HEADER]: THREADS_FEATURE,
					'Content-Type': contentType,
					'x-forwarded-for': '127.0.0.1',
				},
				body,
			});
			expect(response.status).toBe(201);
			const created = (await response.json()) as StartForumThreadResponse;
			expect(created.message?.id).toBe(created.id);
			expect(created.message?.attachments).toHaveLength(1);
		});

		it('allows one pinned post and clears the pin on archive', async () => {
			const s = await setup();
			const first = await post(s.owner.token, s.forumId);
			const second = await post(s.owner.token, s.forumId);
			const pinned = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
				.patch(`/channels/${first.id}`)
				.body({flags: ChannelFlags.PINNED})
				.execute();
			expect(pinned.flags).toBe(ChannelFlags.PINNED);
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${second.id}`)
				.body({flags: ChannelFlags.PINNED})
				.expect(400, APIErrorCodes.MAX_PINNED_THREADS_IN_FORUM)
				.execute();
			const archived = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
				.patch(`/channels/${first.id}`)
				.body({archived: true})
				.execute();
			expect(archived.flags).toBe(0);
			await threadsRequest(harness, s.owner.token)
				.patch(`/channels/${second.id}`)
				.body({flags: ChannelFlags.PINNED})
				.execute();
			await threadsRequest(harness, s.member.token)
				.patch(`/channels/${second.id}`)
				.body({flags: 0})
				.expect(403)
				.execute();
		});

		it('returns post data', async () => {
			const s = await setup();
			const created = await post(s.member.token, s.forumId);
			const data = await threadsRequest<ThreadPostDataResponse>(harness, s.owner.token)
				.post(`/channels/${s.forumId}/post-data`)
				.body({thread_ids: [created.id, '1']})
				.execute();
			expect(Object.keys(data.threads).sort()).toEqual([created.id, '1'].sort());
			expect(data.threads['1']).toEqual({owner: null, first_message: null});
			expect(data.threads[created.id]?.owner?.user.id).toBe(s.member.userId);
			expect(data.threads[created.id]?.first_message?.id).toBe(created.id);
		});

		it('serves the member preview on posts only, newest first', async () => {
			const s = await setup();
			const created = await post(s.member.token, s.forumId);
			expect(created.member_ids_preview).toEqual([s.member.userId]);
			await threadsRequest(harness, s.owner.token)
				.put(`/channels/${created.id}/thread-members/@me`)
				.expect(204)
				.execute();
			const fetched = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
				.get(`/channels/${created.id}`)
				.execute();
			expect(fetched.member_ids_preview).toEqual([s.owner.userId, s.member.userId]);
			await threadsRequest(harness, s.owner.token).patch(`/channels/${created.id}`).body({archived: true}).execute();
			const archived = await threadsRequest<{threads: Array<ThreadChannelResponse>}>(harness, s.owner.token)
				.get(`/channels/${s.forumId}/threads/archived/public`)
				.execute();
			expect(archived.threads.map((thread) => thread.member_ids_preview)).toEqual([[s.owner.userId, s.member.userId]]);
			const text = await createChannel(harness, s.owner.token, s.guildId, 'text');
			const textThread = await threadsRequest<ThreadChannelResponse>(harness, s.owner.token)
				.post(`/channels/${text.id}/threads`)
				.body({name: 'thread', type: ChannelTypes.PUBLIC_THREAD})
				.expect(201)
				.execute();
			expect(textThread).not.toHaveProperty('member_ids_preview');
		});

		it('applies the forum slowmode to post creation', async () => {
			const s = await setup({rate_limit_per_user: 60});
			await post(s.member.token, s.forumId);
			const state = await threadsRequest<{retry_after_ms: number}>(harness, s.member.token)
				.get(`/channels/${s.forumId}/slowmode`)
				.execute();
			expect(state.retry_after_ms).toBeGreaterThan(0);
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.forumId}/threads`)
				.body({name: 'again', message: {content: 'x'}})
				.expect(400, APIErrorCodes.SLOWMODE_RATE_LIMITED)
				.execute();
		});
	});

	describe('gateway payloads', () => {
		it('carries forum settings on CHANNEL_CREATE and CHANNEL_UPDATE', async () => {
			const s = await setup();
			const dispatches: Array<{event: string; data: unknown}> = [];
			const spy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild').mockImplementation(async (params) => {
				dispatches.push({event: params.event, data: params.data});
			});
			await createForum(s.owner.token, s.guildId, {available_tags: [{name: 'x'}]});
			await threadsRequest(harness, s.owner.token).post(`/channels/${s.forumId}/tags`).body({name: 'y'}).execute();
			spy.mockRestore();
			const created = dispatches.find((entry) => entry.event === 'CHANNEL_CREATE')?.data as ChannelResponse;
			const updated = dispatches.find((entry) => entry.event === 'CHANNEL_UPDATE')?.data as ChannelResponse;
			expect(created.available_tags?.map((tag) => tag.name)).toEqual(['x']);
			expect(updated.available_tags?.map((tag) => tag.name)).toEqual(['y']);
			expect(updated.default_forum_layout).toBe(0);
		});
	});

	describe('existing routes on forums', () => {
		it('reports thread search as unavailable without a search provider', async () => {
			const s = await setup();
			await threadsRequest(harness, s.member.token)
				.get(`/channels/${s.forumId}/threads/search`)
				.expect(403, APIErrorCodes.FEATURE_TEMPORARILY_DISABLED)
				.execute();
		});

		it('answers with the right errors', async () => {
			const s = await setup();
			await threadsRequest(harness, s.member.token)
				.get(`/channels/${s.forumId}/messages`)
				.expect(400, APIErrorCodes.INVALID_CHANNEL_TYPE)
				.execute();
			await threadsRequest(harness, s.member.token)
				.get(`/channels/${s.forumId}/messages/pins`)
				.expect(400, APIErrorCodes.INVALID_CHANNEL_TYPE)
				.execute();
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.forumId}/typing`)
				.expect(400, APIErrorCodes.INVALID_CHANNEL_TYPE)
				.execute();
			await threadsRequest(harness, s.member.token)
				.post(`/channels/${s.forumId}/messages`)
				.body({content: 'x'})
				.expect(400, APIErrorCodes.CANNOT_SEND_MESSAGES_IN_NON_TEXT_CHANNEL)
				.execute();
		});
	});
});
