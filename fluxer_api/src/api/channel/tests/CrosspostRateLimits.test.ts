// SPDX-License-Identifier: AGPL-3.0-or-later

import {loadFixture, sendMessageWithAttachments} from '@app/api/channel/tests/AttachmentTestUtils';
import {
	type AnnouncementWorld,
	channelBudgetRemaining,
	createAnnouncementChannel,
	createAnnouncementWorld,
	crosspostRequest,
	editMessage,
	postMessage,
	publish,
	publishedEditBudgetRemaining,
	RecordingWorkerService,
} from '@app/api/channel/tests/CrosspostTestUtils';
import {setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {RateLimitConfigs} from '@app/api/RateLimitConfig';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {createWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {
	CROSSPOST_CHANNEL_RATE_LIMIT,
	PUBLISHED_MESSAGE_EDIT_RATE_LIMIT,
} from '@fluxer/constants/src/AnnouncementConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {MessageFlags} from '@fluxer/constants/src/ChannelConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {ms} from 'itty-time';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface RateLimitBody {
	code: string;
	retry_after: number;
	global: boolean;
}

describe('Publishing and published-edit limits', () => {
	let harness: ApiTestHarness;
	let worker: RecordingWorkerService;
	let world: AnnouncementWorld;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		worker = new RecordingWorkerService();
		setInjectedWorkerService(worker);
		world = await createAnnouncementWorld(harness);
	});

	afterEach(() => {
		setInjectedWorkerService(new NoopWorkerService());
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	async function publishMany(channelId: string, count: number, token = world.author.token): Promise<void> {
		for (let index = 0; index < count; index++) {
			const message = await postMessage(harness, token, channelId, `Update ${channelId} ${index}`);
			await publish(harness, token, channelId, message.id);
		}
	}

	test('a channel publishes 10 in a row, then the 11th is limited with a shared scope', async () => {
		await publishMany(world.announcement.id, CROSSPOST_CHANNEL_RATE_LIMIT.maxAttempts);
		const extra = await postMessage(harness, world.author.token, world.announcement.id, 'One too many');
		const {response, json} = await crosspostRequest(harness, world.author.token, world.announcement.id, extra.id)
			.expect(429, APIErrorCodes.MESSAGE_CROSSPOST_RATE_LIMITED)
			.executeWithResponse();
		const body = json as unknown as RateLimitBody;
		expect(body.retry_after).toBeGreaterThan(300);
		expect(body.retry_after).toBeLessThanOrEqual(360);
		expect(body.global).toBe(false);
		expect(response.headers.get('X-RateLimit-Scope')).toBe('shared');
		expect(Number(response.headers.get('Retry-After'))).toBeGreaterThan(300);
		expect(worker.byTask('crosspostMessage')).toHaveLength(CROSSPOST_CHANNEL_RATE_LIMIT.maxAttempts);
	});

	test('the channel limit is per channel and shared across members', async () => {
		await publishMany(world.announcement.id, CROSSPOST_CHANNEL_RATE_LIMIT.maxAttempts);
		const byMod = await postMessage(harness, world.mod.token, world.announcement.id, 'Moderator news');
		await crosspostRequest(harness, world.mod.token, world.announcement.id, byMod.id)
			.expect(429, APIErrorCodes.MESSAGE_CROSSPOST_RATE_LIMITED)
			.execute();
		const second = await createAnnouncementChannel(harness, world.owner.token, world.guild.id, 'news-two');
		const elsewhere = await postMessage(harness, world.author.token, second.id, 'Different channel');
		await publish(harness, world.author.token, second.id, elsewhere.id);
	});

	test('a community has no shared publish allowance across its announcement channels', async () => {
		const channels = [
			world.announcement,
			await createAnnouncementChannel(harness, world.owner.token, world.guild.id, 'news-b'),
			await createAnnouncementChannel(harness, world.owner.token, world.guild.id, 'news-c'),
			await createAnnouncementChannel(harness, world.owner.token, world.guild.id, 'news-d'),
		];
		for (const channel of channels) {
			await publishMany(channel.id, CROSSPOST_CHANNEL_RATE_LIMIT.maxAttempts);
			expect(await channelBudgetRemaining(channel.id)).toBe(0);
		}
		expect(worker.byTask('crosspostMessage')).toHaveLength(channels.length * CROSSPOST_CHANNEL_RATE_LIMIT.maxAttempts);
		const last = channels[3]!;
		const extra = await postMessage(harness, world.author.token, last.id, 'One past the channel allowance');
		const {response} = await crosspostRequest(harness, world.author.token, last.id, extra.id)
			.expect(429, APIErrorCodes.MESSAGE_CROSSPOST_RATE_LIMITED)
			.executeWithResponse();
		expect(response.headers.get('X-RateLimit-Scope')).toBe('shared');
	});

	test('failed publishes do not consume budget', async () => {
		const published = await postMessage(harness, world.author.token, world.announcement.id, 'Published once');
		await publish(harness, world.author.token, world.announcement.id, published.id);
		const reply = await createBuilder<MessageResponse>(harness, world.author.token)
			.post(`/channels/${world.announcement.id}/messages`)
			.body({content: 'Reply', message_reference: {message_id: published.id}})
			.expect(200)
			.execute();
		await crosspostRequest(harness, world.author.token, world.announcement.id, published.id)
			.expect(400, APIErrorCodes.MESSAGE_ALREADY_CROSSPOSTED)
			.execute();
		await crosspostRequest(harness, world.author.token, world.announcement.id, reply.id)
			.expect(400, APIErrorCodes.MESSAGE_NOT_CROSSPOSTABLE)
			.execute();
		await crosspostRequest(harness, world.member.token, world.announcement.id, reply.id)
			.expect(403, APIErrorCodes.MISSING_PERMISSIONS)
			.execute();
		await crosspostRequest(harness, world.author.token, world.announcement.id, '123456789012345678')
			.expect(404, APIErrorCodes.UNKNOWN_MESSAGE)
			.execute();
		expect(await channelBudgetRemaining(world.announcement.id)).toBe(CROSSPOST_CHANNEL_RATE_LIMIT.maxAttempts - 1);
	});

	test('a published message can be edited 3 times in a row, then the 4th is limited', async () => {
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'Edit me');
		const other = await postMessage(harness, world.author.token, world.announcement.id, 'Edit me too');
		await publish(harness, world.author.token, world.announcement.id, message.id);
		await publish(harness, world.author.token, world.announcement.id, other.id);
		for (let index = 0; index < PUBLISHED_MESSAGE_EDIT_RATE_LIMIT.maxAttempts; index++) {
			await editMessage(harness, world.author.token, world.announcement.id, message.id, {content: `Edit ${index}`})
				.expect(200)
				.execute();
		}
		const {response, json} = await editMessage(harness, world.author.token, world.announcement.id, message.id, {
			content: 'Edit 4',
		})
			.expect(429, APIErrorCodes.PUBLISHED_MESSAGE_EDIT_RATE_LIMITED)
			.executeWithResponse();
		const body = json as unknown as RateLimitBody;
		expect(body.retry_after).toBeGreaterThan(1100);
		expect(body.retry_after).toBeLessThanOrEqual(1200);
		expect(response.headers.get('X-RateLimit-Scope')).toBe('shared');
		expect(worker.syncJobsFor(message.id)).toHaveLength(PUBLISHED_MESSAGE_EDIT_RATE_LIMIT.maxAttempts);
		await editMessage(harness, world.author.token, world.announcement.id, other.id, {content: 'Other edit'})
			.expect(200)
			.execute();
	});

	test('edits before publishing do not count toward the published-edit cap', async () => {
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'Draft');
		for (let index = 0; index < 3; index++) {
			await editMessage(harness, world.author.token, world.announcement.id, message.id, {content: `Draft ${index}`})
				.expect(200)
				.execute();
		}
		expect(worker.syncJobsFor(message.id)).toHaveLength(0);
		await publish(harness, world.author.token, world.announcement.id, message.id);
		expect(await publishedEditBudgetRemaining(message.id)).toBe(PUBLISHED_MESSAGE_EDIT_RATE_LIMIT.maxAttempts);
		for (let index = 0; index < PUBLISHED_MESSAGE_EDIT_RATE_LIMIT.maxAttempts; index++) {
			await editMessage(harness, world.author.token, world.announcement.id, message.id, {content: `Final ${index}`})
				.expect(200)
				.execute();
		}
	});

	test('unpublished messages are never limited by the published-edit cap', async () => {
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'Never published');
		for (let index = 0; index < 6; index++) {
			await editMessage(harness, world.author.token, world.announcement.id, message.id, {content: `Version ${index}`})
				.expect(200)
				.execute();
		}
		expect(await publishedEditBudgetRemaining(message.id)).toBe(PUBLISHED_MESSAGE_EDIT_RATE_LIMIT.maxAttempts);
		expect(worker.byTask('syncCrosspostedMessage')).toHaveLength(0);
	});

	test('a moderator suppress-embeds edit is not limited and still propagates', async () => {
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'Link https://example.com');
		await publish(harness, world.author.token, world.announcement.id, message.id);
		for (let index = 0; index < PUBLISHED_MESSAGE_EDIT_RATE_LIMIT.maxAttempts; index++) {
			await editMessage(harness, world.author.token, world.announcement.id, message.id, {content: `Link ${index}`})
				.expect(200)
				.execute();
		}
		await editMessage(harness, world.author.token, world.announcement.id, message.id, {content: 'Blocked'})
			.expect(429, APIErrorCodes.PUBLISHED_MESSAGE_EDIT_RATE_LIMITED)
			.execute();
		const syncsBefore = worker.syncJobsFor(message.id).length;
		const suppressed = await editMessage(harness, world.mod.token, world.announcement.id, message.id, {
			flags: MessageFlags.SUPPRESS_EMBEDS,
		})
			.expect(200)
			.execute();
		expect(suppressed.flags).toBe(MessageFlags.CROSSPOSTED | MessageFlags.SUPPRESS_EMBEDS);
		const restored = await editMessage(harness, world.mod.token, world.announcement.id, message.id, {flags: 0})
			.expect(200)
			.execute();
		expect(restored.flags).toBe(MessageFlags.CROSSPOSTED);
		expect(worker.syncJobsFor(message.id).length).toBe(syncsBefore + 2);
	});

	test('webhook token edits count toward the published-edit cap', async () => {
		const webhook = await createWebhook(harness, world.announcement.id, world.owner.token, 'Changelog');
		const executed = await createBuilderWithoutAuth<MessageResponse>(harness)
			.post(`/webhooks/${webhook.id}/${webhook.token}?wait=true`)
			.body({content: 'v1'})
			.expect(200)
			.execute();
		await publish(harness, world.mod.token, world.announcement.id, executed.id);
		const editPath = `/webhooks/${webhook.id}/${webhook.token}/messages/${executed.id}`;
		for (let index = 0; index < PUBLISHED_MESSAGE_EDIT_RATE_LIMIT.maxAttempts; index++) {
			await createBuilderWithoutAuth(harness)
				.patch(editPath)
				.body({content: `v1.${index}`})
				.expect(200)
				.execute();
		}
		await createBuilderWithoutAuth(harness)
			.patch(editPath)
			.body({content: 'v1.9'})
			.expect(429, APIErrorCodes.PUBLISHED_MESSAGE_EDIT_RATE_LIMITED)
			.execute();
		expect(worker.syncJobsFor(executed.id)).toHaveLength(PUBLISHED_MESSAGE_EDIT_RATE_LIMIT.maxAttempts);
	});

	test('published edits that fail validation do not use the edit budget', async () => {
		const missingUpload = {
			attachments: [
				{
					id: 0,
					filename: 'upload.png',
					upload_filename: 'missing-upload-key',
					file_size: 2048,
					content_type: 'image/png',
				},
			],
		};
		const message = await postMessage(harness, world.author.token, world.announcement.id, 'Edit me');
		await publish(harness, world.author.token, world.announcement.id, message.id);
		for (let index = 0; index < PUBLISHED_MESSAGE_EDIT_RATE_LIMIT.maxAttempts; index++) {
			const {json} = await editMessage(harness, world.author.token, world.announcement.id, message.id, {
				content: `Broken ${index}`,
				...missingUpload,
			})
				.expect(400)
				.executeWithResponse();
			expect(JSON.stringify(json)).toContain('FILE_NOT_FOUND');
		}
		expect(await publishedEditBudgetRemaining(message.id)).toBe(PUBLISHED_MESSAGE_EDIT_RATE_LIMIT.maxAttempts);
		await editMessage(harness, world.author.token, world.announcement.id, message.id, {content: 'Valid edit'})
			.expect(200)
			.execute();
		expect(await publishedEditBudgetRemaining(message.id)).toBe(PUBLISHED_MESSAGE_EDIT_RATE_LIMIT.maxAttempts - 1);
	});

	test('removing an attachment from a published message counts toward the cap and propagates', async () => {
		const {json} = await sendMessageWithAttachments(
			harness,
			world.author.token,
			world.announcement.id,
			{
				content: 'Two pictures',
				attachments: [
					{id: 0, filename: 'first.png'},
					{id: 1, filename: 'second.png'},
				],
			},
			[
				{index: 0, filename: 'first.png', data: loadFixture('yeah.png')},
				{index: 1, filename: 'second.png', data: loadFixture('yeah.png')},
			],
		);
		expect(json.attachments?.length).toBe(2);
		await publish(harness, world.author.token, world.announcement.id, json.id);
		await createBuilder(harness, world.author.token)
			.delete(`/channels/${world.announcement.id}/messages/${json.id}/attachments/${json.attachments![0]!.id}`)
			.expect(204)
			.execute();
		expect(await publishedEditBudgetRemaining(json.id)).toBe(PUBLISHED_MESSAGE_EDIT_RATE_LIMIT.maxAttempts - 1);
		expect(worker.syncJobsFor(json.id)).toHaveLength(1);
		for (let index = 0; index < PUBLISHED_MESSAGE_EDIT_RATE_LIMIT.maxAttempts - 1; index++) {
			await editMessage(harness, world.author.token, world.announcement.id, json.id, {content: `Caption ${index}`})
				.expect(200)
				.execute();
		}
		await createBuilder(harness, world.author.token)
			.delete(`/channels/${world.announcement.id}/messages/${json.id}/attachments/${json.attachments![1]!.id}`)
			.expect(429, APIErrorCodes.PUBLISHED_MESSAGE_EDIT_RATE_LIMITED)
			.execute();
	});

	test('the publish and follow route buckets are enforced when route limits are on', async () => {
		expect(RateLimitConfigs.CHANNEL_MESSAGE_CROSSPOST).toEqual({
			bucket: 'channel:message:crosspost::channel_id',
			config: {limit: 5, windowMs: ms('5 seconds')},
		});
		expect(RateLimitConfigs.CHANNEL_FOLLOW).toEqual({
			bucket: 'channel:follow::channel_id',
			config: {limit: 5, windowMs: ms('10 seconds')},
		});
		expect(RateLimitConfigs.CHANNEL_FOLLOWER_STATS).toEqual({
			bucket: 'channel:follower_stats::channel_id',
			config: {limit: 10, windowMs: ms('10 seconds')},
		});
		const statuses: Array<number> = [];
		for (let index = 0; index < 6; index++) {
			const {response} = await crosspostRequest(
				harness,
				world.author.token,
				world.announcement.id,
				`12345678901234567${index}`,
			)
				.header('x-fluxer-test-enable-rate-limits', 'true')
				.executeRaw();
			statuses.push(response.status);
		}
		expect(statuses.slice(0, 5)).toEqual([404, 404, 404, 404, 404]);
		expect(statuses[5]).toBe(429);
	});
});
