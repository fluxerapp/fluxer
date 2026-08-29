// SPDX-License-Identifier: AGPL-3.0-or-later

import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';
import {createGuildID, createUserID} from '../../BrandedTypes';
import {Logger} from '../../Logger';
import {getWorkerDependencies} from '../WorkerContext';

const PayloadSchema = z.object({
	guildId: z.string(),
	userId: z.string(),
	seconds: z.number().min(0).max(604800),
});
const deleteUserMessagesInGuildByTime: WorkerTaskHandler = async (payload, helpers) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload: validated}, 'Processing deleteUserMessagesInGuildByTime task');
	const guildId = createGuildID(BigInt(validated.guildId));
	const userId = createUserID(BigInt(validated.userId));
	const {seconds} = validated;
	Logger.debug(
		{guildId: guildId.toString(), userId: userId.toString(), seconds},
		'Starting time-based message deletion for guild ban',
	);
	try {
		const {channelService} = getWorkerDependencies();
		await channelService.messages.deletion.deleteUserMessagesInGuild({guildId, userId, seconds});
		Logger.debug(
			{guildId: guildId.toString(), userId: userId.toString(), seconds},
			'Time-based message deletion completed successfully',
		);
	} catch (error) {
		Logger.error(
			{guildId: guildId.toString(), userId: userId.toString(), seconds, error},
			'Failed to delete user messages in guild',
		);
		throw error;
	}
};

export default deleteUserMessagesInGuildByTime;
