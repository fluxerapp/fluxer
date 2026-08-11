// SPDX-License-Identifier: AGPL-3.0-or-later

import {nanos} from 'nats';
import {describe, expect, it, vi} from 'vitest';
import {JetStreamWorkerQueue, WORKER_QUEUE_MAX_AGE_MS} from '../JetStreamWorkerQueue';

describe('JetStreamWorkerQueue dead-letter handling', () => {
	it('retains permitted 30-day schedules plus the queue recovery window', async () => {
		const add = vi.fn().mockResolvedValue(undefined);
		const queue = new JetStreamWorkerQueue({
			getJetStreamManager: async () => ({
				streams: {info: vi.fn().mockRejectedValue(new Error('missing')), add},
			}),
		} as unknown as ConstructorParameters<typeof JetStreamWorkerQueue>[0]);

		await queue.ensureStream();

		expect(WORKER_QUEUE_MAX_AGE_MS).toBe(37 * 24 * 60 * 60 * 1000);
		expect(add).toHaveBeenCalledWith(expect.objectContaining({max_age: nanos(WORKER_QUEUE_MAX_AGE_MS)}));
	});

	it('updates an existing source stream to the schedule-safe retained lifetime', async () => {
		const update = vi.fn().mockResolvedValue(undefined);
		const info = vi.fn().mockResolvedValue({
			config: {name: 'JOBS', subjects: ['jobs.>'], max_age: nanos(7 * 24 * 60 * 60 * 1000)},
		});
		const queue = new JetStreamWorkerQueue({
			getJetStreamManager: async () => ({streams: {info, update}}),
		} as unknown as ConstructorParameters<typeof JetStreamWorkerQueue>[0]);

		await queue.ensureStream();

		expect(update).toHaveBeenCalledWith('JOBS', expect.objectContaining({max_age: nanos(WORKER_QUEUE_MAX_AGE_MS)}));
	});

	it('lets the application retain recovery deliveries while stream max age bounds message lifetime', async () => {
		const add = vi.fn().mockResolvedValue(undefined);
		const queue = new JetStreamWorkerQueue({
			getJetStreamManager: async () => ({consumers: {add}}),
		} as unknown as ConstructorParameters<typeof JetStreamWorkerQueue>[0]);

		await queue.ensureConsumers([
			{
				name: 'realtime',
				consumerName: 'test-consumer',
				taskTypes: ['handleMentions'],
				concurrency: 1,
				maxAckPending: 1,
				ackWaitMs: 60_000,
				maxDeliver: 5,
			},
		]);

		expect(add.mock.calls[0]?.[1]).toEqual(expect.objectContaining({max_deliver: -1}));
	});

	it('keeps DLQ message IDs deduplicated for the complete retained lifetime', async () => {
		const add = vi.fn().mockResolvedValue(undefined);
		const queue = new JetStreamWorkerQueue({
			getJetStreamManager: async () => ({
				streams: {info: vi.fn().mockRejectedValue(new Error('missing')), add},
			}),
		} as unknown as ConstructorParameters<typeof JetStreamWorkerQueue>[0]);

		await queue.ensureDlqStream();

		expect(add).toHaveBeenCalledWith(
			expect.objectContaining({
				duplicate_window: nanos(30 * 24 * 60 * 60 * 1000),
				max_age: nanos(30 * 24 * 60 * 60 * 1000),
			}),
		);
	});

	it('updates an existing DLQ stream to use the retained-lifetime duplicate window', async () => {
		const update = vi.fn().mockResolvedValue(undefined);
		const info = vi.fn().mockResolvedValue({
			config: {
				name: 'JOBS_DLQ',
				subjects: ['dlq.>'],
				retention: 'limits',
				storage: 'file',
				max_age: nanos(30 * 24 * 60 * 60 * 1000),
			},
		});
		const queue = new JetStreamWorkerQueue({
			getJetStreamManager: async () => ({streams: {info, update}}),
		} as unknown as ConstructorParameters<typeof JetStreamWorkerQueue>[0]);

		await queue.ensureDlqStream();

		expect(update).toHaveBeenCalledWith(
			'JOBS_DLQ',
			expect.objectContaining({duplicate_window: nanos(30 * 24 * 60 * 60 * 1000)}),
		);
	});

	it('uses a stable message ID so retry publication is idempotent', async () => {
		const publish = vi.fn().mockResolvedValue({seq: 1});
		const queue = new JetStreamWorkerQueue({getJetStreamClient: () => ({publish})} as unknown as ConstructorParameters<
			typeof JetStreamWorkerQueue
		>[0]);
		const meta = {
			originalSeq: 123,
			errorMessage: 'failed',
			deliveryCount: 5,
			lane: 'batch',
		};

		await queue.publishToDlq('testTask', {id: 'a'}, meta);
		await queue.publishToDlq('testTask', {id: 'a'}, {...meta, deliveryCount: 6});

		expect(publish).toHaveBeenCalledTimes(2);
		expect(publish.mock.calls[0]?.[2]).toEqual({msgID: 'dlq:batch:testTask:123'});
		expect(publish.mock.calls[1]?.[2]).toEqual({msgID: 'dlq:batch:testTask:123'});
	});
});
