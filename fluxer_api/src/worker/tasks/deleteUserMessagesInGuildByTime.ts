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
import {createGuildID, createUserID} from '~/BrandedTypes';
import {Logger} from '~/Logger';
import {CommonFields, validatePayload} from '../utils/TaskPayloadValidator';
import {getWorkerDependencies} from '../WorkerContext';

interface DeleteUserMessagesInGuildByTimePayload {
	guildId: string;
	userId: string;
	days: number;
}

const payloadSchema = {
	guildId: CommonFields.guildId(),
	userId: CommonFields.userId(),
	days: CommonFields.days(),
};

const deleteUserMessagesInGuildByTime: Task = async (payload, helpers) => {
	const validated = validatePayload<DeleteUserMessagesInGuildByTimePayload>(payload, payloadSchema);
	helpers.logger.debug('Processing deleteUserMessagesInGuildByTime task', {payload: validated});

	const guildId = createGuildID(BigInt(validated.guildId));
	const userId = createUserID(BigInt(validated.userId));
	const {days} = validated;

	Logger.debug(
		{guildId: guildId.toString(), userId: userId.toString(), days},
		'Starting time-based message deletion for guild ban',
	);

	try {
		const {channelService} = getWorkerDependencies();
		await channelService.deleteUserMessagesInGuild({guildId, userId, days});

		Logger.debug(
			{guildId: guildId.toString(), userId: userId.toString(), days},
			'Time-based message deletion completed successfully',
		);
	} catch (error) {
		Logger.error(
			{guildId: guildId.toString(), userId: userId.toString(), days, error},
			'Failed to delete user messages in guild',
		);
		throw error;
	}
};

export default deleteUserMessagesInGuildByTime;
