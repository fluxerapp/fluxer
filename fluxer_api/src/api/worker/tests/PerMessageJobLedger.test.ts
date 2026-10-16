// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannelID, createMessageID, createRoleID, createUserID} from '@app/api/BrandedTypes';
import {MessageMentionService} from '@app/api/channel/services/message/MessageMentionService';
import {EmbedService} from '@app/api/infrastructure/EmbedService';
import type {Message} from '@app/api/models/Message';
import {RecordingWorkerService} from '@app/api/test/RecordingWorkerService';
import {WorkerQueueOverflowError} from '@app/api/worker/WorkerQueueOverflowError';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import {describe, expect, it} from 'vitest';

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

function createOverflowingWorkerService(): RecordingWorkerService {
	const service = new RecordingWorkerService();
	service.failure = (taskType) => new WorkerQueueOverflowError(taskType, 'maximum messages per subject exceeded');
	return service;
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
		const embedService = new EmbedService(null as never, null as never, workerService);
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
			createOverflowingWorkerService(),
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
		const embedService = new EmbedService(null as never, null as never, createOverflowingWorkerService());
		await expect(
			embedService.enqueueUrlEmbedExtraction(createChannelID(3n), createMessageID(2n), null, 'block'),
		).resolves.toBeUndefined();
	});
});
