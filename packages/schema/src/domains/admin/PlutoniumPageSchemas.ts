// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	EXPERIMENT_BUCKET_RESOLUTION,
	type ExperimentTargeting,
	experimentAudienceIncludes,
	experimentBucket,
} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {z} from 'zod';

const PLUTONIUM_PAGE_ROLLOUT_BASIS_POINTS_MAX = EXPERIMENT_BUCKET_RESOLUTION;
const PLUTONIUM_PAGE_MAX_TARGETED_USERS = 1000;
const DEFAULT_PLUTONIUM_PAGE_SALT = 'plutonium-page-v1';

const PLUTONIUM_PAGE_SALT_PATTERN = /^[\x20-\x7e]+$/u;

const PlutoniumPageTargetIdSchema = z.string().regex(/^\d{1,20}$/u);
const PlutoniumPageTargetedUserIdsSchema = z.array(PlutoniumPageTargetIdSchema).max(PLUTONIUM_PAGE_MAX_TARGETED_USERS);

const plutoniumPageConfigFields = {
	enabled: z.boolean(),
	config_version: z.number().int().min(0),
	rollout_basis_points: z.number().int().min(0).max(PLUTONIUM_PAGE_ROLLOUT_BASIS_POINTS_MAX),
	rollout_salt: z.string().trim().min(1).max(64).regex(PLUTONIUM_PAGE_SALT_PATTERN),
	included_user_ids: PlutoniumPageTargetedUserIdsSchema,
	included_guild_ids: PlutoniumPageTargetedUserIdsSchema,
	include_premium_users: z.boolean(),
	excluded_user_ids: PlutoniumPageTargetedUserIdsSchema,
};

export const PlutoniumPageConfigSchema = z.object({
	enabled: plutoniumPageConfigFields.enabled.default(false),
	config_version: plutoniumPageConfigFields.config_version.default(0),
	rollout_basis_points: plutoniumPageConfigFields.rollout_basis_points.default(0),
	rollout_salt: plutoniumPageConfigFields.rollout_salt.default(DEFAULT_PLUTONIUM_PAGE_SALT),
	included_user_ids: plutoniumPageConfigFields.included_user_ids.default([]),
	included_guild_ids: plutoniumPageConfigFields.included_guild_ids.default([]),
	include_premium_users: plutoniumPageConfigFields.include_premium_users.default(false),
	excluded_user_ids: plutoniumPageConfigFields.excluded_user_ids.default([]),
});

export type PlutoniumPageConfig = z.infer<typeof PlutoniumPageConfigSchema>;

export const DEFAULT_PLUTONIUM_PAGE_CONFIG: PlutoniumPageConfig = PlutoniumPageConfigSchema.parse({});

export const PlutoniumPageConfigUpdateRequest = z
	.object(plutoniumPageConfigFields)
	.omit({config_version: true})
	.partial();

export type PlutoniumPageConfigUpdateRequest = z.infer<typeof PlutoniumPageConfigUpdateRequest>;

export const PlutoniumPageConfigResponse = PlutoniumPageConfigSchema;

export type PlutoniumPageConfigResponse = z.infer<typeof PlutoniumPageConfigResponse>;

export const PlutoniumPageAssignmentResponse = z.object({
	enabled: plutoniumPageConfigFields.enabled,
});

export type PlutoniumPageAssignmentResponse = z.infer<typeof PlutoniumPageAssignmentResponse>;

export const INERT_PLUTONIUM_PAGE_ASSIGNMENT: PlutoniumPageAssignmentResponse = {
	enabled: false,
};

export function resolvePlutoniumPageAssignment(
	config: PlutoniumPageConfig,
	userId: string,
	targeting: ExperimentTargeting,
): PlutoniumPageAssignmentResponse {
	if (!config.enabled) return {...INERT_PLUTONIUM_PAGE_ASSIGNMENT};
	if (config.excluded_user_ids.includes(userId)) return {...INERT_PLUTONIUM_PAGE_ASSIGNMENT};
	if (config.included_user_ids.includes(userId)) return {enabled: true};
	if (experimentAudienceIncludes(config, targeting)) return {enabled: true};
	return {enabled: experimentBucket(userId, config.rollout_salt) < config.rollout_basis_points};
}
