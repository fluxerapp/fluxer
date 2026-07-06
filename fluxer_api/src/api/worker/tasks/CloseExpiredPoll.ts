// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskHandler, WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';
import {createChannelID, createMessageID} from '../../BrandedTypes';
import {dispatchMessageUpdateBroadcast} from '../../channel/services/message/MessageGatewayDispatch';
import {Logger} from '../../Logger';
import {getWorkerDependencies} from '../WorkerContext';
import type {WorkerDependencies} from '../WorkerDependencies';

const PayloadSchema = z.object({
	channelId: z.string(),
	messageId: z.string(),
	pollId: z.string(),
});

interface CloseExpiredPollCoreOptions {
	now?: Date;
	dispatchMessageUpdate?: typeof dispatchMessageUpdateBroadcast;
	logger?: Pick<typeof Logger, 'debug'>;
}

export async function closeExpiredPollCore(
	payload: unknown,
	helpers: Pick<WorkerTaskHelpers, 'addJob'>,
	dependencies: Pick<WorkerDependencies, 'channelRepository' | 'gatewayService'>,
	options: CloseExpiredPollCoreOptions = {},
): Promise<void> {
	const validated = PayloadSchema.parse(payload);
	const channelId = createChannelID(BigInt(validated.channelId));
	const messageId = createMessageID(BigInt(validated.messageId));
	const pollId = createMessageID(BigInt(validated.pollId));
	const {channelRepository, gatewayService} = dependencies;
	const dispatchMessageUpdate = options.dispatchMessageUpdate ?? dispatchMessageUpdateBroadcast;
	const logger = options.logger ?? Logger;
	const message = await channelRepository.messages.getMessage(channelId, messageId);
	if (!message?.poll) {
		logger.debug(
			{channelId: validated.channelId, messageId: validated.messageId},
			'Poll close skipped: message missing',
		);
		return;
	}
	if (message.poll.poll_id !== pollId) {
		logger.debug(
			{channelId: validated.channelId, messageId: validated.messageId, pollId: validated.pollId},
			'Poll close skipped: stale poll id',
		);
		return;
	}
	if (message.poll.closed_at) {
		return;
	}
	const now = options.now ?? new Date();
	if (message.poll.expires_at.getTime() > now.getTime()) {
		await helpers.addJob('closeExpiredPoll', validated, {
			jobKey: `close-poll:${validated.messageId}:${validated.pollId}`,
			runAt: message.poll.expires_at,
		});
		return;
	}
	const channel = await channelRepository.channelData.findUnique(channelId);
	if (!channel) {
		logger.debug(
			{channelId: validated.channelId, messageId: validated.messageId},
			'Poll close skipped: channel missing',
		);
		return;
	}
	const updatedMessage = await channelRepository.messages.upsertMessage(
		{
			...message.toRow(),
			poll: {
				...message.poll,
				closed_at: now,
			},
		},
		message.toRow(),
	);
	await dispatchMessageUpdate({
		gatewayService,
		channel,
		message: updatedMessage,
	});
}

const closeExpiredPoll: WorkerTaskHandler = async (payload, helpers) => {
	const {channelRepository, gatewayService} = getWorkerDependencies();
	await closeExpiredPollCore(payload, helpers, {channelRepository, gatewayService});
};

export default closeExpiredPoll;
