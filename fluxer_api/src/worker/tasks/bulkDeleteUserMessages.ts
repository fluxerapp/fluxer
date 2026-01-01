/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {Task} from 'graphile-worker';
import {createUserID} from '~/BrandedTypes';
import {Logger} from '~/Logger';
import {MessageDeletionService} from '../services/MessageDeletionService';
import {CommonFields, validatePayload} from '../utils/TaskPayloadValidator';
import {getWorkerDependencies} from '../WorkerContext';

interface BulkDeleteUserMessagesPayload {
	userId: string;
	scheduledAt?: number;
}

const payloadSchema = {
	userId: CommonFields.userId(),
	scheduledAt: CommonFields.timestamp(),
};

const bulkDeleteUserMessages: Task = async (payload, helpers) => {
	const validated = validatePayload<BulkDeleteUserMessagesPayload>(payload, payloadSchema);
	helpers.logger.debug('Processing bulkDeleteUserMessages task', {payload: validated});

	const userId = createUserID(BigInt(validated.userId));
	const scheduledAtMs = validated.scheduledAt ?? Number.POSITIVE_INFINITY;

	const {channelRepository, gatewayService, userRepository, storageService, cloudflarePurgeQueue} =
		getWorkerDependencies();

	const user = await userRepository.findUnique(userId);
	if (!user) {
		Logger.debug({userId}, 'User not found, skipping bulk message deletion');
		return;
	}

	if (!user.pendingBulkMessageDeletionAt) {
		Logger.debug({userId}, 'User has no pending bulk message deletion, skipping (already completed)');
		return;
	}

	const deletionService = new MessageDeletionService({
		channelRepository,
		gatewayService,
		storageService,
		cloudflarePurgeQueue,
	});

	const totalDeleted = await deletionService.deleteUserMessagesBulk(userId, {
		beforeTimestamp: scheduledAtMs,
		onProgress: (deleted) => helpers.logger.debug(`Deleted ${deleted} messages so far`),
	});

	await userRepository.patchUpsert(userId, {
		pending_bulk_message_deletion_at: null,
		pending_bulk_message_deletion_channel_count: null,
		pending_bulk_message_deletion_message_count: null,
	});

	Logger.debug({userId, totalDeleted}, 'Bulk message deletion completed');
};

export default bulkDeleteUserMessages;
