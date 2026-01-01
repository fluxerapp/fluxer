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
import {applicationIdToUserId, createApplicationID} from '~/BrandedTypes';
import {UserFlags} from '~/Constants';
import {mapGuildMemberToResponse} from '~/guild/GuildModel';
import {Logger} from '~/Logger';
import type {RequestCache} from '~/middleware/RequestCacheMiddleware';
import {randomString} from '~/utils/RandomUtils';
import {validatePayload} from '../utils/TaskPayloadValidator';
import {getWorkerDependencies} from '../WorkerContext';
import {chunkArray} from './utils/messageDeletion';

interface ApplicationProcessDeletionPayload {
	applicationId: string;
}

const payloadSchema = {
	applicationId: {type: 'string' as const, requirement: 'required' as const},
};

function createRequestCache(): RequestCache {
	return {
		userPartials: new Map(),
		clear: () => {},
	};
}

const CHUNK_SIZE = 50;
const DELAY_MS = 100;

const applicationProcessDeletion: Task = async (payload, helpers) => {
	const validated = validatePayload<ApplicationProcessDeletionPayload>(payload, payloadSchema);
	helpers.logger.debug('Processing applicationProcessDeletion task', {payload: validated});

	const applicationId = createApplicationID(BigInt(validated.applicationId));
	const botUserId = applicationIdToUserId(applicationId);

	const {
		userRepository,
		guildRepository,
		applicationRepository,
		userCacheService,
		gatewayService,
		discriminatorService,
	} = getWorkerDependencies();

	Logger.debug({applicationId, botUserId}, 'Starting application deletion');

	try {
		const application = await applicationRepository.getApplication(applicationId);
		if (!application) {
			Logger.warn({applicationId}, 'Application not found, skipping deletion (already deleted)');
			return;
		}

		const botUser = await userRepository.findUnique(botUserId);
		if (!botUser) {
			Logger.warn({applicationId, botUserId}, 'Bot user not found, skipping deletion');
			return;
		}

		if (botUser.flags & UserFlags.DELETED) {
			Logger.info({applicationId, botUserId}, 'Bot user already marked as deleted, skipping');
			await applicationRepository.deleteApplication(applicationId);
			return;
		}

		let foundUsername: string;
		let foundDiscriminator: number;
		while (true) {
			foundUsername = `DeletedBot${randomString(8)}`;
			const discriminatorResult = await discriminatorService.generateDiscriminator({
				username: foundUsername,
				isPremium: false,
			});
			if (!discriminatorResult.available || discriminatorResult.discriminator === -1) {
				continue;
			}
			foundDiscriminator = discriminatorResult.discriminator;
			break;
		}

		Logger.debug(
			{applicationId, botUserId, newUsername: foundUsername, newDiscriminator: foundDiscriminator},
			'Generated deleted bot username',
		);

		await userRepository.patchUpsert(botUserId, {
			username: foundUsername,
			discriminator: foundDiscriminator,
			flags: botUser.flags | UserFlags.DELETED,
		});

		Logger.debug({applicationId, botUserId}, 'Updated bot user to deleted state');

		const guildIds = await userRepository.getUserGuildIds(botUserId);
		Logger.debug({applicationId, botUserId, guildCount: guildIds.length}, 'Found guilds bot is member of');

		const chunks = chunkArray(guildIds, CHUNK_SIZE);
		let processedGuilds = 0;

		for (const chunk of chunks) {
			await Promise.all(
				chunk.map(async (guildId) => {
					try {
						const member = await guildRepository.getMember(guildId, botUserId);
						if (!member) {
							Logger.debug({botUserId, guildId}, 'Member not found in guild, skipping');
							return;
						}

						const requestCache = createRequestCache();
						const botMemberResponse = await mapGuildMemberToResponse(member, userCacheService, requestCache);

						await gatewayService.dispatchGuild({
							guildId,
							event: 'GUILD_MEMBER_UPDATE',
							data: {
								guild_id: guildId.toString(),
								...botMemberResponse,
							},
						});

						Logger.debug({botUserId, guildId}, 'Dispatched GUILD_MEMBER_UPDATE for bot');
					} catch (error) {
						Logger.error({error, botUserId, guildId}, 'Failed to dispatch guild member update');
					}
				}),
			);

			processedGuilds += chunk.length;

			if (processedGuilds < guildIds.length) {
				await new Promise((resolve) => setTimeout(resolve, DELAY_MS));
			}

			Logger.info(
				{applicationId, botUserId, processedGuilds, totalGuilds: guildIds.length},
				'Application deletion: dispatched guild updates',
			);
		}

		Logger.debug({applicationId, botUserId, totalGuilds: guildIds.length}, 'Completed guild member updates');

		Logger.debug({applicationId}, 'Deleting application from database');
		await applicationRepository.deleteApplication(applicationId);

		Logger.info({applicationId, botUserId, guildCount: guildIds.length}, 'Application deletion completed successfully');
	} catch (error) {
		Logger.error({error, applicationId, botUserId}, 'Failed to delete application');
		throw error;
	}
};

export default applicationProcessDeletion;
