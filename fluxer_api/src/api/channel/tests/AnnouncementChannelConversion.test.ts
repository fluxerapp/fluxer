// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createGuildID, createUserID, createWebhookID, createWebhookToken} from '@app/api/BrandedTypes';
import {disableCrosspostWorker, followRequest, publishRequest} from '@app/api/channel/tests/AnnouncementTestUtils';
import {
	createChannel,
	createGuild,
	deleteChannel,
	getChannel,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {
	copiesOf,
	editRequest,
	type FanoutWorld,
	followInto,
	postAndPublish,
	sendMessage,
	setupFanoutWorld,
} from '@app/api/channel/tests/CrosspostWorkerTestUtils';
import {processRecordedGuildDeletions} from '@app/api/guild/tests/GuildDeletionTestUtils';
import {setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {RecordingWorkerService} from '@app/api/test/RecordingWorkerService';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {createWebhook, getChannelWebhooks} from '@app/api/webhook/tests/WebhookTestUtils';
import {WebhookRepository} from '@app/api/webhook/WebhookRepository';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {AuditLogActionType} from '@fluxer/constants/src/AuditLogActionType';
import {ChannelTypes, WebhookTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelResponse} from '@fluxer/schema/src/domains/channel/ChannelSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface AuditLogResponse {
	audit_log_entries: Array<{
		action_type: number;
		target_id: string | null;
		changes?: Array<{key: string; old_value?: unknown; new_value?: unknown}>;
	}>;
}

async function insertFollowerWebhook(params: {guildId: string; channelId: string; creatorId: string}): Promise<string> {
	const webhookId = createWebhookID(BigInt(Date.now()) * 1000n + BigInt(Math.floor(Math.random() * 1000)));
	await new WebhookRepository().create({
		webhookId,
		token: createWebhookToken('f'.repeat(64)),
		type: WebhookTypes.CHANNEL_FOLLOWER,
		guildId: createGuildID(BigInt(params.guildId)),
		channelId: createChannelID(BigInt(params.channelId)),
		creatorId: createUserID(BigInt(params.creatorId)),
		name: 'Source Guild #news',
		avatarHash: null,
	});
	return webhookId.toString();
}

describe('Announcement channel conversion', () => {
	let harness: ApiTestHarness;
	let worker: RecordingWorkerService;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		worker = new RecordingWorkerService();
		setInjectedWorkerService(worker);
	});

	afterEach(() => {
		setInjectedWorkerService(new NoopWorkerService());
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	test('requires MANAGE_CHANNELS', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const text = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_TEXT);
		const announcement = await createChannel(harness, owner.token, guild.id, 'ann', ChannelTypes.GUILD_ANNOUNCEMENT);
		await createBuilder(harness, member.token)
			.patch(`/channels/${text.id}`)
			.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
		await createBuilder(harness, member.token)
			.patch(`/channels/${announcement.id}`)
			.body({type: ChannelTypes.GUILD_TEXT})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
		expect((await getChannel(harness, owner.token, text.id)).type).toBe(ChannelTypes.GUILD_TEXT);
		expect((await getChannel(harness, owner.token, announcement.id)).type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
		expect(worker.byTask('removeChannelFollowers')).toHaveLength(0);
	});

	test('refuses to convert a channel that receives follows until the follow is removed', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Followed Guild');
		const text = await createChannel(harness, owner.token, guild.id, 'feed', ChannelTypes.GUILD_TEXT);
		await createWebhook(harness, text.id, owner.token, 'Incoming Bot');
		const followerWebhookId = await insertFollowerWebhook({
			guildId: guild.id,
			channelId: text.id,
			creatorId: owner.userId,
		});
		const channelWebhooks = await getChannelWebhooks(harness, text.id, owner.token);
		expect(channelWebhooks.some((webhook) => webhook.id === followerWebhookId)).toBe(true);
		await createBuilder(harness, owner.token)
			.patch(`/channels/${text.id}`)
			.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.CHANNEL_HAS_FOLLOWED_CHANNELS)
			.execute();
		expect((await getChannel(harness, owner.token, text.id)).type).toBe(ChannelTypes.GUILD_TEXT);
		await createBuilder(harness, owner.token)
			.delete(`/webhooks/${followerWebhookId}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const converted = await createBuilder<ChannelResponse>(harness, owner.token)
			.patch(`/channels/${text.id}`)
			.body({type: ChannelTypes.GUILD_ANNOUNCEMENT})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(converted.type).toBe(ChannelTypes.GUILD_ANNOUNCEMENT);
	});

	test('deleting an announcement channel schedules follower removal with source-deleted copies', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Delete Guild');
		const announcement = await createChannel(harness, owner.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		const text = await createChannel(harness, owner.token, guild.id, 'text', ChannelTypes.GUILD_TEXT);
		await deleteChannel(harness, owner.token, text.id);
		expect(worker.byTask('removeChannelFollowers')).toHaveLength(0);
		await deleteChannel(harness, owner.token, announcement.id);
		const removals = worker.byTask('removeChannelFollowers');
		expect(removals).toHaveLength(1);
		expect(removals[0]?.payload).toEqual({
			sourceChannelId: announcement.id,
			reason: 'deleted',
			copyMode: 'source_deleted',
		});
		expect(removals[0]?.options?.jobKey).toMatch(new RegExp(`^remove-followers:${announcement.id}:deleted:\\d+$`));
	});

	test('admin guild deletion purges copies', async () => {
		const admin = await createTestAccount(harness);
		await setUserACLs(harness, admin, ['admin:authenticate', 'guild:lookup', 'guild:delete']);
		const guild = await createGuild(harness, admin.token, 'Admin Delete Guild');
		const announcement = await createChannel(harness, admin.token, guild.id, 'news', ChannelTypes.GUILD_ANNOUNCEMENT);
		await createBuilder(harness, admin.token)
			.delete(`/admin/guilds/${guild.id}`)
			.body({basis: {type: 'holder_request'}})
			.expect(HTTP_STATUS.OK)
			.execute();
		await processRecordedGuildDeletions(worker);
		const removals = worker.byTask('removeChannelFollowers');
		expect(removals).toHaveLength(1);
		expect(removals[0]?.payload).toEqual({
			sourceChannelId: announcement.id,
			reason: 'deleted',
			copyMode: 'purge',
		});
	});
});

describe('Announcement channel conversion with the crosspost worker', () => {
	let harness: ApiTestHarness;
	let world: FanoutWorld;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		world = await setupFanoutWorld(harness);
	});

	afterEach(() => {
		disableCrosspostWorker();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function convert(type: number): Promise<void> {
		await createBuilder<ChannelResponse>(harness, world.a.owner.token)
			.patch(`/channels/${world.a.ann.id}`)
			.body({type})
			.expect(HTTP_STATUS.OK)
			.execute();
	}

	async function followerWebhooks(channelId: string) {
		const webhooks = await new WebhookRepository().listByChannel(createChannelID(BigInt(channelId)));
		return webhooks.filter((webhook) => webhook.type === WebhookTypes.CHANNEL_FOLLOWER);
	}

	test('converting to text removes every follower after the drain and leaves copies propagating', async () => {
		await followInto(harness, world, world.b.t1.id);
		await followInto(harness, world, world.b.t2.id);
		const source = await postAndPublish(harness, world, {content: 'before conversion'});
		await convert(ChannelTypes.GUILD_TEXT);
		expect(world.worker.byTask('removeChannelFollowers')).toHaveLength(1);
		expect(await followerWebhooks(world.b.t1.id)).toHaveLength(1);
		await world.worker.drain();
		expect(world.worker.deadLetters).toHaveLength(0);
		expect(await followerWebhooks(world.b.t1.id)).toHaveLength(0);
		expect(await followerWebhooks(world.b.t2.id)).toHaveLength(0);
		const audit = await createBuilder<AuditLogResponse>(harness, world.b.owner.token)
			.get(`/guilds/${world.b.guild.id}/audit-logs?action_type=${AuditLogActionType.WEBHOOK_DELETE}`)
			.execute();
		expect(audit.audit_log_entries).toHaveLength(0);
		const [copy] = await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id);
		expect(copy!.content).toBe('before conversion');
		await editRequest(harness, world.a.owner.token, world.a.ann.id, source.id, {content: 'after conversion'})
			.expect(HTTP_STATUS.OK)
			.execute();
		await world.worker.drain();
		for (const channelId of [world.b.t1.id, world.b.t2.id]) {
			const [updated] = await copiesOf(harness, world.b.owner.token, channelId, source.id);
			expect(updated!.content).toBe('after conversion');
		}
	});

	test('converting back before the drain keeps the followers', async () => {
		await followInto(harness, world, world.b.t1.id);
		await convert(ChannelTypes.GUILD_TEXT);
		await convert(ChannelTypes.GUILD_ANNOUNCEMENT);
		await world.worker.drain();
		expect(await followerWebhooks(world.b.t1.id)).toHaveLength(1);
		const source = await postAndPublish(harness, world, {content: 'still followed'});
		expect(await copiesOf(harness, world.b.owner.token, world.b.t1.id, source.id)).toHaveLength(1);
	});

	test('flipping twice records two removals and ends with no followers', async () => {
		await followInto(harness, world, world.b.t1.id);
		await convert(ChannelTypes.GUILD_TEXT);
		await convert(ChannelTypes.GUILD_ANNOUNCEMENT);
		await convert(ChannelTypes.GUILD_TEXT);
		const removals = world.worker.byTask('removeChannelFollowers');
		expect(new Set(removals.map((job) => job.jobKey)).size).toBe(2);
		await world.worker.drain();
		expect(await followerWebhooks(world.b.t1.id)).toHaveLength(0);
	});

	test('publishing or following after converting to text is refused', async () => {
		const message = await sendMessage(harness, world.a.owner.token, world.a.ann.id, {content: 'late'});
		await convert(ChannelTypes.GUILD_TEXT);
		await publishRequest(harness, world.a.owner.token, world.a.ann.id, message.id)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.ANNOUNCEMENT_CHANNEL_REQUIRED)
			.execute();
		await followRequest(harness, world.b.owner.token, world.a.ann.id, world.b.t1.id)
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.ANNOUNCEMENT_CHANNEL_REQUIRED)
			.execute();
	});
});
