// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DELETED_USER_DISCRIMINATOR,
	DELETED_USER_GLOBAL_NAME,
	DELETED_USER_USERNAME,
	UserFlags,
} from '@fluxer/constants/src/UserConstants';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';
import {applicationIdToUserId, createApplicationID, type GuildID} from '../../BrandedTypes';
import {mapGuildMemberToResponse} from '../../guild/GuildModel';
import {Logger} from '../../Logger';
import {createRequestCache} from '../../middleware/RequestCacheMiddleware';
import {remapAuthorMessagesToDeletedUser} from '../../oauth/ApplicationMessageAuthorAnonymization';
import {chunkArray} from '../../utils/ArrayUtils';
import {getWorkerDependencies} from '../WorkerContext';

const PayloadSchema = z.object({
	applicationId: z.string(),
});
const CHUNK_SIZE = 50;
const applicationProcessDeletion: WorkerTaskHandler = async (payload, helpers) => {
	const validated = PayloadSchema.parse(payload);
	helpers.logger.debug({payload: validated}, 'Processing applicationProcessDeletion task');
	const applicationId = createApplicationID(BigInt(validated.applicationId));
	const botUserId = applicationIdToUserId(applicationId);
	const {
		userRepository,
		guildRepository,
		channelRepository,
		applicationRepository,
		userCacheService,
		gatewayService,
		snowflakeService,
	} = getWorkerDependencies();
	Logger.debug({applicationId, botUserId}, 'Starting application deletion');
	try {
		const application = await applicationRepository.getApplication(applicationId);
		if (!application) {
			Logger.warn({applicationId}, 'Application not found, skipping deletion (already deleted)');
			return;
		}
		const botUser = await userRepository.findUniqueAssert(botUserId);
		const replacementAuthorId = await remapAuthorMessagesToDeletedUser({
			originalAuthorId: botUserId,
			channelRepository,
			userRepository,
			snowflakeService,
		});
		if (botUser.flags & UserFlags.DELETED) {
			Logger.info(
				{
					applicationId,
					botUserId,
					replacementAuthorId: replacementAuthorId?.toString() ?? null,
				},
				'Bot user already marked as deleted, skipping profile update',
			);
			await applicationRepository.deleteApplication(applicationId);
			return;
		}
		const updatedBotUser = await userRepository.patchUpsert(
			botUserId,
			{
				username: DELETED_USER_USERNAME,
				global_name: DELETED_USER_GLOBAL_NAME,
				discriminator: DELETED_USER_DISCRIMINATOR,
				flags: botUser.flags | UserFlags.DELETED,
			},
			botUser.toRow(),
		);
		await userCacheService.setUserPartialResponseFromUser(updatedBotUser);
		Logger.debug({applicationId, botUserId}, 'Updated bot user to deleted state');
		const guildIds = await userRepository.getUserGuildIds(botUserId);
		Logger.debug({applicationId, botUserId, guildCount: guildIds.length}, 'Found guilds bot is member of');
		const chunks = chunkArray(guildIds, CHUNK_SIZE);
		let processedGuilds = 0;
		for (const chunk of chunks) {
			await Promise.all(
				chunk.map(async (guildId: GuildID) => {
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
				await new Promise((resolve) => setTimeout(resolve, 100));
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
