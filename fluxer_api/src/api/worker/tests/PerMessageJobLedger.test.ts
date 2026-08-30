// SPDX-License-Identifier: AGPL-3.0-or-later

import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {IWorkerService} from '@pkgs/worker/src/contracts/IWorkerService';
import type {WorkerJobOptions, WorkerJobPayload} from '@pkgs/worker/src/contracts/WorkerTypes';
import {describe, expect, it} from 'vitest';
import {createChannelID, createMessageID, createRoleID, createUserID} from '../../BrandedTypes';
import {MessageMentionService} from '../../channel/services/message/MessageMentionService';
import {EmbedService} from '../../infrastructure/EmbedService';
import type {Message} from '../../models/Message';
import type {WorkerTaskName} from '../WorkerLaneConfig';
import {WorkerQueueOverflowError} from '../WorkerQueueOverflowError';

class RecordingWorkerService implements IWorkerService<WorkerTaskName> {
	readonly jobs: Array<{taskType: WorkerTaskName; options: WorkerJobOptions | undefined}> = [];

	async addJob<TPayload extends WorkerJobPayload = WorkerJobPayload>(
		taskType: WorkerTaskName,
		_payload: TPayload,
		options?: WorkerJobOptions,
	): Promise<bigint> {
		this.jobs.push({taskType, options});
		return 1n;
	}

	async cancelJob(): Promise<boolean> {
		return false;
	}

	async retryDeadLetterJob(): Promise<boolean> {
		return false;
	}
}

class OverflowingWorkerService implements IWorkerService<WorkerTaskName> {
	async addJob<TPayload extends WorkerJobPayload = WorkerJobPayload>(
		taskType: WorkerTaskName,
		_payload: TPayload,
		_options?: WorkerJobOptions,
	): Promise<bigint> {
		throw new WorkerQueueOverflowError(taskType, 'maximum messages per subject exceeded');
	}

	async cancelJob(): Promise<boolean> {
		return false;
	}

	async retryDeadLetterJob(): Promise<boolean> {
		return false;
	}
}

function makeMentionMessage(): Message {
	return {
		id: createMessageID(2n),
		channelId: createChannelID(3n),
		type: MessageTypes.DEFAULT,
		reference: null,
		mentionEveryone: false,
		mentionedUserIds: new Set([createUserID(4n)]),
		mentionedRoleIds: new Set([createRoleID(5n)]),
	} as never;
}

describe('per-message worker jobs', () => {
	it('enqueues handleMentions without a ledger row', async () => {
		const workerService = new RecordingWorkerService();
		const mentionService = new MessageMentionService(
			null as never,
			null as never,
			null as never,
			workerService,
			null as never,
		);
		await mentionService.handleMentionTasks({
			guildId: null,
			message: makeMentionMessage(),
			authorId: createUserID(1n),
		});
		expect(workerService.jobs).toHaveLength(1);
		expect(workerService.jobs[0]!.taskType).toBe('handleMentions');
		expect(workerService.jobs[0]!.options?.skipLedger).toBe(true);
	});

	it('enqueues extractEmbeds without a ledger row', async () => {
		const workerService = new RecordingWorkerService();
		const embedService = new EmbedService(null as never, null as never, null as never, workerService);
		await embedService.enqueueUrlEmbedExtraction(createChannelID(3n), createMessageID(2n), null, 'block');
		expect(workerService.jobs).toHaveLength(1);
		expect(workerService.jobs[0]!.taskType).toBe('extractEmbeds');
		expect(workerService.jobs[0]!.options?.skipLedger).toBe(true);
	});

	it('drops mention fanout instead of failing the send when the jobs stream is full', async () => {
		const mentionService = new MessageMentionService(
			null as never,
			null as never,
			null as never,
			new OverflowingWorkerService(),
			null as never,
		);
		await expect(
			mentionService.handleMentionTasks({
				guildId: null,
				message: makeMentionMessage(),
				authorId: createUserID(1n),
			}),
		).resolves.toBeUndefined();
	});

	it('drops embed extraction instead of failing the send when the jobs stream is full', async () => {
		const embedService = new EmbedService(null as never, null as never, null as never, new OverflowingWorkerService());
		await expect(
			embedService.enqueueUrlEmbedExtraction(createChannelID(3n), createMessageID(2n), null, 'block'),
		).resolves.toBeUndefined();
	});
});
