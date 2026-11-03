// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type AnnouncementWorld,
	channelBudgetRemaining,
	createAnnouncementWorld,
	crosspostRequest,
	editMessage,
	postMessage,
	publish,
	RecordingWorkerService,
} from '@app/api/channel/tests/CrosspostTestUtils';
import {setInjectedWorkerService} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopWorkerService} from '@app/api/test/NoopWorkerService';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {createWebhook} from '@app/api/webhook/tests/WebhookTestUtils';
import {
	CROSSPOST_CHANNEL_RATE_LIMIT,
	PUBLISHED_MESSAGE_EDIT_RATE_LIMIT,
} from '@fluxer/constants/src/AnnouncementConstants';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
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
});
