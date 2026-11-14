// SPDX-License-Identifier: AGPL-3.0-or-later

import {EXPERIMENT_BUCKET_RESOLUTION, experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {z} from 'zod';

const GUILD_VISITS_MAX_TARGETED_IDS = 1000;
const DEFAULT_GUILD_VISITS_USER_SALT = 'guild-visits-user-v1';

const GuildVisitsUserIdsSchema = z.array(z.string().regex(/^\d{1,20}$/u)).max(GUILD_VISITS_MAX_TARGETED_IDS);

const guildVisitsConfigFields = {
	enabled: z.boolean().describe('Kill switch for visiting communities without joining'),
	user_basis_points: z
		.number()
		.int()
		.min(0)
		.max(EXPERIMENT_BUCKET_RESOLUTION)
		.describe('Share of people assigned, in basis points'),
	user_salt: z
		.string()
		.trim()
		.min(1)
		.max(64)
		.regex(/^[\x20-\x7e]+$/u)
		.describe('Salt for the assignment bucket'),
	included_user_ids: GuildVisitsUserIdsSchema.describe('People always assigned'),
	excluded_user_ids: GuildVisitsUserIdsSchema.describe('People never assigned, checked before the included list'),
};

export const GuildVisitsConfigSchema = z.object({
	enabled: guildVisitsConfigFields.enabled.default(false),
	user_basis_points: guildVisitsConfigFields.user_basis_points.default(0),
	user_salt: guildVisitsConfigFields.user_salt.default(DEFAULT_GUILD_VISITS_USER_SALT),
	included_user_ids: guildVisitsConfigFields.included_user_ids.default([]),
	excluded_user_ids: guildVisitsConfigFields.excluded_user_ids.default([]),
});

export type GuildVisitsConfig = z.infer<typeof GuildVisitsConfigSchema>;

export const GuildVisitsConfigResponse = GuildVisitsConfigSchema;

export const GuildVisitsConfigUpdateRequest = z.object(guildVisitsConfigFields).partial();

export type GuildVisitsConfigUpdateRequest = z.infer<typeof GuildVisitsConfigUpdateRequest>;

export const GuildVisitsAssignmentResponse = z.object({
	enabled: z.boolean().describe('Whether this person may visit communities without joining'),
});

export type GuildVisitsAssignmentResponse = z.infer<typeof GuildVisitsAssignmentResponse>;

export function guildVisitsUserActive(config: GuildVisitsConfig, userId: string): boolean {
	if (!config.enabled) return false;
	if (config.excluded_user_ids.includes(userId)) return false;
	if (config.included_user_ids.includes(userId)) return true;
	return config.user_basis_points > 0 && experimentBucket(userId, config.user_salt) < config.user_basis_points;
}
