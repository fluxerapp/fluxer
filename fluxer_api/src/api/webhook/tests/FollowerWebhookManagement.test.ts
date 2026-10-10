// SPDX-License-Identifier: AGPL-3.0-or-later

import {setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID} from '@app/api/BrandedTypes';
import {
	type AnnouncementWorld,
	addGuildMember,
	announcementWorld,
	createGuildChannel,
	disableCrosspostWorker,
	findWebhookRow,
	follow,
	grantGuildRole,
} from '@app/api/channel/tests/AnnouncementTestUtils';
import {createPermissionOverwrite, deleteChannel, leaveGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	copiesOf,
	type FanoutWorld,
	followInto,
	postAndPublish,
	publishAndDrain,
	readRow,
	sendWithImage,
	setupFanoutWorld,
} from '@app/api/channel/tests/CrosspostWorkerTestUtils';
import {getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {createWebhook, getChannelWebhooks, getGuildWebhooks} from '@app/api/webhook/tests/WebhookTestUtils';
import {WebhookRepository} from '@app/api/webhook/WebhookRepository';
import {CROSSPOST_SOURCE_DELETED_CONTENT} from '@fluxer/constants/src/AnnouncementConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ChannelTypes, MessageFlags, Permissions, WebhookTypes} from '@fluxer/constants/src/ChannelConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {GuildAuditLogListResponse} from '@fluxer/schema/src/domains/guild/GuildAuditLogSchemas';
import {WebhookListResponse, WebhookResponse} from '@fluxer/schema/src/domains/webhook/WebhookSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test, vi} from 'vitest';

function webhooksUpdateChannels(spy: {
	mock: {calls: Array<Parameters<NoopGatewayService['dispatchGuild']>>};
}): Array<string> {
	return spy.mock.calls
		.map(([params]) => params)
		.filter((params) => params.event === 'WEBHOOKS_UPDATE')
		.map((params) => (params.data as {channel_id: string}).channel_id);
}

async function sourceIndex(sourceChannelId: string) {
	return new WebhookRepository().listIdsBySourceChannel(createChannelID(BigInt(sourceChannelId)), {limit: 100});
}

describe('Follower webhook management', () => {
	let harness: ApiTestHarness;
	let world: AnnouncementWorld;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		world = await announcementWorld(harness);
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	test('rejects every token route for a follower webhook', async () => {
		const {a, b} = world;
		const followed = await follow(harness, b.owner.token, a.ann.id, b.t1.id);
		const row = await findWebhookRow(followed.webhook_id);
		expect(row?.type).toBe(WebhookTypes.CHANNEL_FOLLOWER);
		const base = `/webhooks/${followed.webhook_id}/${row!.token}`;
		const messages = await createBuilder<Array<{id: string}>>(harness, b.owner.token)
			.get(`/channels/${b.t1.id}/messages`)
			.execute();
		const messageId = messages[0]!.id;
		const attempts: Array<{method: 'get' | 'post' | 'patch' | 'delete'; path: string; body?: unknown}> = [
			{method: 'get', path: base},
			{method: 'patch', path: base, body: {name: 'Hijacked'}},
			{method: 'delete', path: base},
			{method: 'post', path: base, body: {content: 'hello'}},
			{method: 'post', path: `${base}?wait=true`, body: {content: 'hello'}},
			{method: 'post', path: `${base}/slack`, body: {text: 'hello'}},
			{method: 'post', path: `${base}/instatus`, body: {meta: {unsubscribe: '', documentation: ''}, page: {id: 'x'}}},
			{method: 'get', path: `${base}/messages/${messageId}`},
			{method: 'patch', path: `${base}/messages/${messageId}`, body: {content: 'edited'}},
			{method: 'delete', path: `${base}/messages/${messageId}`},
		];
		for (const attempt of attempts) {
			const builder = createBuilderWithoutAuth(harness)[attempt.method](attempt.path);
			if (attempt.body !== undefined) builder.body(attempt.body);
			await builder.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_WEBHOOK).execute();
		}
		const github = await harness.requestJson({
			path: `${base}/github`,
			method: 'POST',
			body: {
				repository: {id: 789, full_name: 'test/repo', name: 'repo', html_url: 'https://github.com/test/repo'},
				sender: {
					id: 123456,
					login: 'testauthor',
					html_url: 'https://github.com/testauthor',
					avatar_url: 'https://avatars.githubusercontent.com/u/123456',
				},
			},
			headers: {'X-GitHub-Event': 'ping', 'X-GitHub-Delivery': 'delivery-1'},
		});
		expect(github.status).toBe(HTTP_STATUS.NOT_FOUND);
		expect(((await github.json()) as {code: string}).code).toBe(APIErrorCodes.UNKNOWN_WEBHOOK);
		expect(await findWebhookRow(followed.webhook_id)).not.toBeNull();
	});

	test('allows renaming a follower webhook', async () => {
		const {a, b} = world;
		const followed = await follow(harness, b.owner.token, a.ann.id, b.t1.id);
		const updated = await createBuilder<WebhookResponse>(harness, b.owner.token)
			.patch(`/webhooks/${followed.webhook_id}`)
			.body({name: 'Release feed'})
			.execute();
		WebhookResponse.parse(updated);
		expect(updated.name).toBe('Release feed');
		expect(updated.type).toBe(WebhookTypes.CHANNEL_FOLLOWER);
		expect(updated).not.toHaveProperty('token');
		expect(updated.source_channel).toEqual({id: a.ann.id, name: 'news'});
		const row = await findWebhookRow(followed.webhook_id);
		expect(row?.name).toBe('Release feed');
		expect(row?.sourceChannelId?.toString()).toBe(a.ann.id);
		expect(row?.sourceGuildId?.toString()).toBe(a.guild.id);
	});

	test.each([
		['an image', () => getPngDataUrl()],
		['null', () => null],
	])('rejects %s as a follower webhook avatar', async (_label, avatar) => {
		const {a, b} = world;
		const followed = await follow(harness, b.owner.token, a.ann.id, b.t1.id);
		const before = await findWebhookRow(followed.webhook_id);
		const response = await createBuilder<{code: string; errors: Array<{path: string; code: string}>}>(
			harness,
			b.owner.token,
		)
			.patch(`/webhooks/${followed.webhook_id}`)
			.body({name: 'Release feed', channel_id: b.t2.id, avatar: avatar()})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
		expect(response.errors[0]?.path).toBe('avatar');
		expect(response.errors[0]?.code).toBe(ValidationErrorCodes.INVALID_FORMAT);
		const after = await findWebhookRow(followed.webhook_id);
		expect(after?.name).toBe(before?.name);
		expect(after?.avatarHash).toBe(before?.avatarHash);
		expect(after?.channelId?.toString()).toBe(b.t1.id);
	});

	test('moves a follower webhook to another text channel and notifies both channels', async () => {
		const {a, b} = world;
		const followed = await follow(harness, b.owner.token, a.ann.id, b.t1.id);
		const dispatchSpy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		const moved = await createBuilder<WebhookResponse>(harness, b.owner.token)
			.patch(`/webhooks/${followed.webhook_id}`)
			.body({channel_id: b.t2.id})
			.execute();
		expect(moved.channel_id).toBe(b.t2.id);
		expect(moved.type).toBe(WebhookTypes.CHANNEL_FOLLOWER);
		expect(moved.source_guild?.id).toBe(a.guild.id);
		expect(moved.source_channel?.id).toBe(a.ann.id);
		expect(webhooksUpdateChannels(dispatchSpy).sort()).toEqual([b.t1.id, b.t2.id].sort());
		expect(await sourceIndex(a.ann.id)).toEqual([
			{webhookId: BigInt(followed.webhook_id), guildId: createGuildID(BigInt(b.guild.id))},
		]);
		expect(await getChannelWebhooks(harness, b.t1.id, b.owner.token)).toHaveLength(0);
	});

	test('validates follower webhook moves', async () => {
		const {a, b} = world;
		const followed = await follow(harness, b.owner.token, a.ann.id, b.t1.id);
		const announcement = await createGuildChannel(harness, b.owner.token, b.guild.id, {
			name: 'b-news',
			type: ChannelTypes.GUILD_ANNOUNCEMENT,
		});
		for (const target of [b.voice.id, announcement.id]) {
			await createBuilder(harness, b.owner.token)
				.patch(`/webhooks/${followed.webhook_id}`)
				.body({channel_id: target})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FOLLOW_TARGET_CHANNEL)
				.execute();
		}
		await follow(harness, b.owner.token, a.ann.id, b.t2.id);
		await createBuilder(harness, b.owner.token)
			.patch(`/webhooks/${followed.webhook_id}`)
			.body({channel_id: b.t2.id})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CHANNEL_ALREADY_FOLLOWED)
			.execute();
		const restricted = await createGuildChannel(harness, a.owner.token, a.guild.id, {
			name: 'adult-news',
			type: ChannelTypes.GUILD_ANNOUNCEMENT,
			nsfw_override: true,
		});
		const restrictedFollow = await follow(harness, b.owner.token, restricted.id, b.ageRestricted.id);
		await createBuilder(harness, b.owner.token)
			.patch(`/webhooks/${restrictedFollow.webhook_id}`)
			.body({channel_id: b.t1.id})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.FOLLOW_TARGET_NOT_AGE_RESTRICTED)
			.execute();
		await grantGuildRole(harness, b.owner, b.guild.id, b.webhookManager, Permissions.MANAGE_WEBHOOKS, 'Hooks');
		await createPermissionOverwrite(harness, b.owner.token, b.contentWarning.id, b.webhookManager.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.MANAGE_WEBHOOKS.toString(),
		});
		const third = await createGuildChannel(harness, b.owner.token, b.guild.id, {
			name: 'third',
			type: ChannelTypes.GUILD_TEXT,
		});
		const managed = await follow(harness, b.owner.token, a.ann.id, third.id);
		await createBuilder(harness, b.webhookManager.token)
			.patch(`/webhooks/${managed.webhook_id}`)
			.body({channel_id: b.contentWarning.id})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
		expect((await findWebhookRow(followed.webhook_id))?.channelId?.toString()).toBe(b.t1.id);
	});

	test('a follower webhook move loses to a concurrent conversion of the target to announcement', async () => {
		const {a, b} = world;
		const followed = await follow(harness, b.owner.token, a.ann.id, b.t1.id);
		const originalCount = WebhookRepository.prototype.countByChannel;
		let converted = false;
		vi.spyOn(WebhookRepository.prototype, 'countByChannel').mockImplementation(async function (
			this: WebhookRepository,
			channelId,
		) {
			const result = await originalCount.call(this, channelId);
			if (!converted && channelId.toString() === b.t2.id) {
				converted = true;
				await createBuilder(harness, b.owner.token)
					.patch(`/channels/${b.t2.id}`)
					.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
					.expect(HTTP_STATUS.OK)
					.execute();
			}
			return result;
		});
		await createBuilder(harness, b.owner.token)
			.patch(`/webhooks/${followed.webhook_id}`)
			.body({channel_id: b.t2.id})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FOLLOW_TARGET_CHANNEL)
			.execute();
		vi.restoreAllMocks();
		expect(converted).toBe(true);
		expect((await findWebhookRow(followed.webhook_id))?.channelId?.toString()).toBe(b.t1.id);
		const t2Webhooks = await new WebhookRepository().listByChannel(createChannelID(BigInt(b.t2.id)));
		expect(t2Webhooks.filter((webhook) => webhook.type === WebhookTypes.CHANNEL_FOLLOWER)).toHaveLength(0);
	});

	test('unfollows by deleting the follower webhook', async () => {
		const {a, b} = world;
		const followed = await follow(harness, b.owner.token, a.ann.id, b.t1.id);
		const dispatchSpy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		await createBuilder(harness, b.owner.token)
			.delete(`/webhooks/${followed.webhook_id}`)
			.header('X-Audit-Log-Reason', 'no longer needed')
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect(webhooksUpdateChannels(dispatchSpy)).toEqual([b.t1.id]);
		expect(await findWebhookRow(followed.webhook_id)).toBeNull();
		expect(await sourceIndex(a.ann.id)).toEqual([]);
		const auditLog = await createBuilder<GuildAuditLogListResponse>(harness, b.owner.token)
			.get(`/guilds/${b.guild.id}/audit-logs?action_type=${AuditLogActionType.WEBHOOK_DELETE}`)
			.execute();
		const entry = auditLog.audit_log_entries.find((candidate) => candidate.target_id === followed.webhook_id);
		expect(entry?.reason).toBe('no longer needed');
		expect(entry?.options).toEqual({channel_id: b.t1.id, type: WebhookTypes.CHANNEL_FOLLOWER});
		await follow(harness, b.owner.token, a.ann.id, b.t1.id);
	});

	test('deleting the target channel removes the follower webhook and its index row', async () => {
		const {a, b} = world;
		const followed = await follow(harness, b.owner.token, a.ann.id, b.t1.id);
		await deleteChannel(harness, b.owner.token, b.t1.id);
		expect(await findWebhookRow(followed.webhook_id)).toBeNull();
		expect(await sourceIndex(a.ann.id)).toEqual([]);
	});

	test('deleting the target guild removes all of its follower index rows', async () => {
		const {a, b} = world;
		await follow(harness, b.owner.token, a.ann.id, b.t1.id);
		await follow(harness, b.owner.token, a.ann.id, b.t2.id);
		expect(await sourceIndex(a.ann.id)).toHaveLength(2);
		await createBuilder(harness, b.owner.token)
			.post(`/guilds/${b.guild.id}/delete`)
			.body({password: b.owner.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect(await sourceIndex(a.ann.id)).toEqual([]);
	});

	test('lists both webhook types, with a token only on incoming webhooks', async () => {
		const {a, b} = world;
		const incoming = await createWebhook(harness, b.t1.id, b.owner.token, 'Incoming');
		const followed = await follow(harness, b.owner.token, a.ann.id, b.t1.id);
		const guildList = WebhookListResponse.parse(await getGuildWebhooks(harness, b.guild.id, b.owner.token));
		const channelList = WebhookListResponse.parse(await getChannelWebhooks(harness, b.t1.id, b.owner.token));
		for (const list of [guildList, channelList]) {
			const incomingEntry = list.find((webhook) => webhook.id === incoming.id);
			const followerEntry = list.find((webhook) => webhook.id === followed.webhook_id);
			expect(incomingEntry?.type).toBe(WebhookTypes.INCOMING);
			expect(incomingEntry?.token).toBe(incoming.token);
			expect(incomingEntry).not.toHaveProperty('source_guild');
			expect(followerEntry?.type).toBe(WebhookTypes.CHANNEL_FOLLOWER);
			expect(followerEntry).not.toHaveProperty('token');
			expect(followerEntry?.source_guild?.id).toBe(a.guild.id);
		}
		const single = WebhookResponse.parse(
			await createBuilder<WebhookResponse>(harness, b.owner.token).get(`/webhooks/${followed.webhook_id}`).execute(),
		);
		expect(single).not.toHaveProperty('token');
		expect(single.source_channel?.id).toBe(a.ann.id);
	});

	test('hides the source while the creator cannot view it and shows it again after they rejoin', async () => {
		const {a, b} = world;
		const followed = await follow(harness, b.owner.token, a.ann.id, b.t1.id);
		await leaveGuild(harness, b.owner.token, a.guild.id);
		const paused = (await getChannelWebhooks(harness, b.t1.id, b.owner.token)).find(
			(webhook) => webhook.id === followed.webhook_id,
		);
		expect(paused).toBeDefined();
		expect(paused).not.toHaveProperty('source_guild');
		expect(paused).not.toHaveProperty('source_channel');
		await addGuildMember(harness, a.owner, a.guild, b.owner);
		const resumed = (await getChannelWebhooks(harness, b.t1.id, b.owner.token)).find(
			(webhook) => webhook.id === followed.webhook_id,
		);
		expect(resumed?.source_guild?.id).toBe(a.guild.id);
		expect(resumed?.source_channel?.id).toBe(a.ann.id);
		await createPermissionOverwrite(harness, a.owner.token, a.ann.id, b.owner.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.VIEW_CHANNEL.toString(),
		});
		const hidden = (await getChannelWebhooks(harness, b.t1.id, b.owner.token)).find(
			(webhook) => webhook.id === followed.webhook_id,
		);
		expect(hidden).not.toHaveProperty('source_guild');
	});
});

describe('Follower webhook delivery', () => {
	let harness: ApiTestHarness;
	let world: FanoutWorld;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		harness.storageService.reset();
		world = await setupFanoutWorld(harness);
	});

	afterEach(() => {
		disableCrosspostWorker();
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	const DELETED_COPY_FLAGS = MessageFlags.IS_CROSSPOST | MessageFlags.SOURCE_MESSAGE_DELETED;

	async function publishedCopyWithImage(channelId: string) {
		const source = await sendWithImage(harness, world.a.owner.token, world.a.ann.id, {content: 'with image'});
		await publishAndDrain(harness, world, source.id);
		const [copy] = await copiesOf(harness, world.b.owner.token, channelId, source.id);
		const [sourceAttachment] = (await readRow(world.a.ann.id, source.id))!.attachments;
		const [copyAttachment] = (await readRow(channelId, copy!.id))!.attachments;
		expect(copyAttachment!.id).toBe(sourceAttachment!.id);
		return {
			source,
			copy: copy!,
			sourceKey: `attachments/${world.a.ann.id}/${sourceAttachment!.id}/${sourceAttachment!.filename}`,
		};
	}

	function expectNoTargetObjectsDeleted(): void {
		for (const channelId of [world.b.t1.id, world.b.t2.id]) {
			expect(
				harness.storageService.getDeletedObjects().some((entry) => entry.key.startsWith(`attachments/${channelId}/`)),
			).toBe(false);
		}
	}

	test('a moved follower webhook delivers the next publish into its new channel', async () => {
		const webhookId = await followInto(harness, world, world.b.t1.id);
		await createBuilder(harness, world.b.owner.token)
			.patch(`/webhooks/${webhookId}`)
			.body({channel_id: world.b.t2.id})
			.expect(HTTP_STATUS.OK)
			.execute();
		const source = await postAndPublish(harness, world, {content: 'after the move'});
		expect(await copiesOf(harness, world.b.owner.token, world.b.t2.id, source.id)).toHaveLength(1);
		expect(await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id)).toHaveLength(0);
	});

	test('after an unfollow the next publish makes no copy there', async () => {
		const webhookId = await followInto(harness, world, world.b.t1.id);
		await createBuilder(harness, world.b.owner.token)
			.delete(`/webhooks/${webhookId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const source = await postAndPublish(harness, world, {content: 'nobody listening'});
		expect(await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id)).toHaveLength(0);
		expect(world.worker.deadLetters).toHaveLength(0);
	});

	test('deleting the source channel removes followers and marks earlier copies deleted', async () => {
		await followInto(harness, world, world.b.t1.id);
		const {copy, sourceKey} = await publishedCopyWithImage(world.b.t1.id);
		await deleteChannel(harness, world.a.owner.token, world.a.ann.id);
		const [removal] = world.worker.byTask('removeChannelFollowers');
		expect(removal!.payload).toEqual({sourceChannelId: world.a.ann.id, reason: 'deleted', copyMode: 'source_deleted'});
		await world.worker.drain();
		expect(world.worker.deadLetters).toHaveLength(0);
		const webhooks = await getChannelWebhooks(harness, world.b.t1.id, world.b.owner.token);
		expect(webhooks.filter((webhook) => webhook.type === WebhookTypes.CHANNEL_FOLLOWER)).toHaveLength(0);
		const marked = (await readRow(world.b.t1.id, copy.id))!;
		expect(marked.flags).toBe(DELETED_COPY_FLAGS);
		expect(marked.content).toBe(CROSSPOST_SOURCE_DELETED_CONTENT);
		expect(marked.attachments).toHaveLength(0);
		expect(harness.storageService.getDeletedObjects().filter((entry) => entry.key === sourceKey)).toHaveLength(1);
		expectNoTargetObjectsDeleted();
	});

	test('a user deleting the source guild marks copies deleted', async () => {
		await followInto(harness, world, world.b.t1.id);
		const {copy} = await publishedCopyWithImage(world.b.t1.id);
		await createBuilder(harness, world.a.owner.token)
			.post(`/guilds/${world.a.guild.id}/delete`)
			.body({password: world.a.owner.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.executeWithResponse();
		await world.worker.drain();
		expect(world.worker.deadLetters).toHaveLength(0);
		expect((await readRow(world.b.t1.id, copy.id))!.flags).toBe(DELETED_COPY_FLAGS);
	});

	test('an admin deleting the source guild deletes the copies', async () => {
		await followInto(harness, world, world.b.t1.id);
		const {copy} = await publishedCopyWithImage(world.b.t1.id);
		const admin = await setUserACLs(harness, world.a.member, ['admin:authenticate', 'guild:lookup', 'guild:delete']);
		const dispatchSpy = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		await createBuilder(harness, admin.token)
			.delete(`/admin/guilds/${world.a.guild.id}`)
			.body(null)
			.expect(HTTP_STATUS.OK)
			.execute();
		await world.worker.drain();
		expect(world.worker.deadLetters).toHaveLength(0);
		expect(await readRow(world.b.t1.id, copy.id)).toBeNull();
		expectNoTargetObjectsDeleted();
		expect(
			dispatchSpy.mock.calls.some(
				([params]) =>
					params.event === 'MESSAGE_DELETE' &&
					params.guildId.toString() === world.b.guild.id &&
					(params.data as {id: string}).id === copy.id,
			),
		).toBe(true);
	});

	test('delivery pauses while the creator cannot view the source and resumes when they rejoin', async () => {
		await followInto(harness, world, world.b.t1.id);
		await leaveGuild(harness, world.b.owner.token, world.a.guild.id);
		const paused = await postAndPublish(harness, world, {content: 'while away'});
		expect(await copiesOf(harness, world.b.owner.token, world.b.t1.id, paused.id)).toHaveLength(0);
		await addGuildMember(harness, world.a.owner, world.a.guild, world.b.owner);
		const resumed = await postAndPublish(harness, world, {content: 'welcome back'});
		expect(await copiesOf(harness, world.b.owner.token, world.b.t1.id, resumed.id)).toHaveLength(1);
		expect(await copiesOf(harness, world.b.owner.token, world.b.t1.id, paused.id)).toHaveLength(0);
	});
});
