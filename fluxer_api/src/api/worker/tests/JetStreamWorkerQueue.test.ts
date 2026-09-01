// SPDX-License-Identifier: AGPL-3.0-or-later

import type {JetStreamConnectionManager} from '@pkgs/nats/src/JetStreamConnectionManager';
import {DiscardPolicy, NatsError, RetentionPolicy, StorageType, type StreamConfig} from 'nats';
import {describe, expect, it} from 'vitest';
import {JetStreamWorkerQueue} from '../JetStreamWorkerQueue';
import {WORKER_LANES} from '../WorkerLaneConfig';
import {WorkerQueueOverflowError} from '../WorkerQueueOverflowError';

const EXPECTED_LIMITS = {
	max_msgs: 2_000_000,
	max_bytes: 8 * 1024 * 1024 * 1024,
	max_msgs_per_subject: 250_000,
	discard: DiscardPolicy.New,
	discard_new_per_subject: true,
};

const LEGACY_CONFIG = {
	name: 'JOBS',
	subjects: ['jobs.>'],
	retention: RetentionPolicy.Workqueue,
	storage: StorageType.File,
	max_msgs: -1,
	max_bytes: -1,
	max_msgs_per_subject: -1,
	discard: DiscardPolicy.Old,
	discard_new_per_subject: false,
} as unknown as StreamConfig;

interface ConsumerAddConfig {
	durable_name: string;
	filter_subjects: Array<string>;
}

function streamLimitError(description: string): NatsError {
	const error = new NatsError('503', '503');
	error.api_error = {code: 503, err_code: 10077, description};
	return error;
}

function boundedPublisher(maxPerSubject: number): (subject: string) => {seq: number} {
	const counts = new Map<string, number>();
	let seq = 0;
	return (subject) => {
		const stored = counts.get(subject) ?? 0;
		if (stored >= maxPerSubject) {
			throw streamLimitError('maximum messages per subject exceeded');
		}
		counts.set(subject, stored + 1);
		seq += 1;
		return {seq};
	};
}

function createQueue(params: {
	existing?: StreamConfig | null;
	updateError?: Error;
	publish?: (subject: string) => {seq: number};
}): {
	queue: JetStreamWorkerQueue;
	added: Array<Partial<StreamConfig>>;
	updated: Array<Partial<StreamConfig>>;
	consumerAdds: Array<ConsumerAddConfig>;
} {
	const added: Array<Partial<StreamConfig>> = [];
	const updated: Array<Partial<StreamConfig>> = [];
	const consumerAdds: Array<ConsumerAddConfig> = [];
	const connectionManager = {
		getJetStreamManager: () =>
			Promise.resolve({
				consumers: {
					add: (_stream: string, config: ConsumerAddConfig) => {
						consumerAdds.push(config);
						return Promise.resolve({});
					},
					delete: () => Promise.resolve(true),
					info: () => Promise.reject(new Error('consumer not found')),
				},
				streams: {
					info: () => {
						if (!params.existing) {
							return Promise.reject(new Error('stream not found'));
						}
						return Promise.resolve({config: params.existing});
					},
					add: (config: Partial<StreamConfig>) => {
						added.push(config);
						return Promise.resolve({config});
					},
					update: (_name: string, config: Partial<StreamConfig>) => {
						if (params.updateError) {
							return Promise.reject(params.updateError);
						}
						updated.push(config);
						return Promise.resolve({config});
					},
				},
			}),
		getJetStreamClient: () => ({
			publish: (subject: string) => {
				const publish = params.publish ?? (() => ({seq: 1}));
				return Promise.resolve(publish(subject));
			},
		}),
	} as unknown as JetStreamConnectionManager;
	return {queue: new JetStreamWorkerQueue(connectionManager), added, updated, consumerAdds};
}

describe('jobs stream limits', () => {
	it('creates the stream bounded and discarding new messages', async () => {
		const {queue, added, updated} = createQueue({existing: null});
		await queue.ensureStream();
		expect(updated).toHaveLength(0);
		expect(added).toHaveLength(1);
		expect(added[0]).toMatchObject(EXPECTED_LIMITS);
		expect(added[0]).toMatchObject({retention: RetentionPolicy.Workqueue, storage: StorageType.File});
	});

	it('applies the limits to an existing unbounded stream', async () => {
		const {queue, added, updated} = createQueue({existing: LEGACY_CONFIG});
		await queue.ensureStream();
		expect(added).toHaveLength(0);
		expect(updated).toHaveLength(1);
		expect(updated[0]).toEqual(EXPECTED_LIMITS);
	});

	it('leaves an already bounded stream alone', async () => {
		const {queue, added, updated} = createQueue({existing: {...LEGACY_CONFIG, ...EXPECTED_LIMITS}});
		await queue.ensureStream();
		expect(added).toHaveLength(0);
		expect(updated).toHaveLength(0);
	});

	it('keeps startup alive when the limit update is rejected', async () => {
		const {queue} = createQueue({existing: LEGACY_CONFIG, updateError: new Error('stream update rejected')});
		await expect(queue.ensureStream()).resolves.toBeUndefined();
	});
});

describe('jobs stream enqueue shedding', () => {
	it('rejects enqueues once the stream is at its cap', async () => {
		const {queue} = createQueue({existing: LEGACY_CONFIG, publish: boundedPublisher(2)});
		await expect(queue.enqueue('extractEmbeds', {})).resolves.toBe('1');
		await expect(queue.enqueue('extractEmbeds', {})).resolves.toBe('2');
		await expect(queue.enqueue('extractEmbeds', {})).rejects.toBeInstanceOf(WorkerQueueOverflowError);
	});

	it('caps each task type independently', async () => {
		const {queue} = createQueue({existing: LEGACY_CONFIG, publish: boundedPublisher(1)});
		await expect(queue.enqueue('extractEmbeds', {})).resolves.toBe('1');
		await expect(queue.enqueue('extractEmbeds', {})).rejects.toBeInstanceOf(WorkerQueueOverflowError);
		await expect(queue.enqueue('handleMentions', {})).resolves.toBe('2');
	});

	it('rethrows publish failures that are not stream limits', async () => {
		const failure = new Error('no responders');
		const {queue} = createQueue({
			existing: LEGACY_CONFIG,
			publish: () => {
				throw failure;
			},
		});
		await expect(queue.enqueue('extractEmbeds', {})).rejects.toBe(failure);
	});
});

describe('lane consumer filters', () => {
	it('keeps consuming retired subjects so legacy jobs are drained instead of orphaned', async () => {
		const {queue, consumerAdds} = createQueue({existing: LEGACY_CONFIG});
		await queue.ensureConsumers(WORKER_LANES);
		const lifecycle = consumerAdds.find((config) => config.durable_name === 'workers_lifecycle');
		expect(lifecycle?.filter_subjects).toContain('jobs.sendSystemDm');
		expect(lifecycle?.filter_subjects).toContain('jobs.sendScheduledMessage');
	});

	it('leaves lanes without retired tasks filtering only their own subjects', async () => {
		const {queue, consumerAdds} = createQueue({existing: LEGACY_CONFIG});
		await queue.ensureConsumers(WORKER_LANES);
		const unfurl = consumerAdds.find((config) => config.durable_name === 'workers_unfurl');
		expect(unfurl?.filter_subjects).toEqual(['jobs.extractEmbeds']);
	});

	it('never claims the same subject from two lane consumers', async () => {
		const {queue, consumerAdds} = createQueue({existing: LEGACY_CONFIG});
		await queue.ensureConsumers(WORKER_LANES);
		const allSubjects = consumerAdds.flatMap((config) => config.filter_subjects);
		expect(new Set(allSubjects).size).toBe(allSubjects.length);
	});
});
