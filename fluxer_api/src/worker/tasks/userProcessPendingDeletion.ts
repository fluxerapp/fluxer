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
import {processUserDeletion} from '~/user/services/UserDeletionService';
import {CommonFields, validatePayload} from '../utils/TaskPayloadValidator';
import {getWorkerDependencies} from '../WorkerContext';

interface UserProcessPendingDeletionPayload {
	userId: string;
	deletionReasonCode: number;
}

const payloadSchema = {
	userId: CommonFields.userId(),
	deletionReasonCode: CommonFields.deletionReasonCode(),
};

const userProcessPendingDeletion: Task = async (payload, helpers) => {
	const validated = validatePayload<UserProcessPendingDeletionPayload>(payload, payloadSchema);
	helpers.logger.debug('Processing userProcessPendingDeletion task', {payload: validated});

	const userId = createUserID(BigInt(validated.userId));

	try {
		const deps = getWorkerDependencies();
		await processUserDeletion(userId, validated.deletionReasonCode, deps);
	} catch (error) {
		Logger.error({error, userId}, 'Failed to delete user account');
		throw error;
	}
};

export default userProcessPendingDeletion;
