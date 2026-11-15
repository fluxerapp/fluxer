// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, type UserID} from '@app/api/BrandedTypes';
import {
	endGuildVisitsForCode,
	endUnassignedGuildVisits,
	pruneGuildVisits,
} from '@app/api/guild/visits/GuildVisitLifecycle';
import {getInstanceConfigRepository, getUserRepository} from '@app/api/middleware/ServiceSingletons';
import {canUserAccessNsfwContent} from '@app/api/utils/AgeUtils';
import type {WorkerTaskHandler} from '@pkgs/worker/src/contracts/WorkerTask';
import {z} from 'zod';

const EndForCodePayload = z.object({
	guildId: z.string().regex(/^\d{1,20}$/u),
	code: z.string().min(1).max(64),
});

const PrunePayload = z.object({
	guildId: z.string().regex(/^\d{1,20}$/u),
	mode: z.enum(['disabled', 'age_restricted', 'deleted']),
});

async function adultContentAllowed(userId: UserID): Promise<boolean> {
	const user = await getUserRepository().findUnique(userId);
	return user !== null && canUserAccessNsfwContent(user);
}

export const endGuildVisitsForCodeTask: WorkerTaskHandler = async (payload, helpers) => {
	const {guildId, code} = EndForCodePayload.parse(payload);
	const ended = await endGuildVisitsForCode(createGuildID(BigInt(guildId)), code);
	helpers.logger.debug({guildId, ended}, 'Ended visits for a removed invite');
};

export const pruneGuildVisitsTask: WorkerTaskHandler = async (payload, helpers) => {
	const {guildId, mode} = PrunePayload.parse(payload);
	const ended = await pruneGuildVisits(createGuildID(BigInt(guildId)), mode, adultContentAllowed);
	helpers.logger.debug({guildId, mode, ended}, 'Pruned guild visits');
};

export const endUnassignedGuildVisitsTask: WorkerTaskHandler = async (_payload, helpers) => {
	await getInstanceConfigRepository().getGuildVisitsConfig();
	const ended = await endUnassignedGuildVisits();
	helpers.logger.debug({ended}, 'Ended visits outside the guild visits experiment');
};
