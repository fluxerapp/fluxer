// SPDX-License-Identifier: AGPL-3.0-or-later

import {EXPERIMENT_BUCKET_RESOLUTION, experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {z} from 'zod';

const PROFILE_TIMEZONE_ROLLOUT_BASIS_POINTS_MAX = EXPERIMENT_BUCKET_RESOLUTION;
const PROFILE_TIMEZONE_MAX_TARGETED_USERS = 1000;
const DEFAULT_PROFILE_TIMEZONE_SALT = 'profile-timezone-v1';

const PROFILE_TIMEZONE_SALT_PATTERN = /^[\x20-\x7e]+$/u;

const ProfileTimezoneTargetIdSchema = z.string().regex(/^\d{1,20}$/u);
const ProfileTimezoneTargetedUserIdsSchema = z
	.array(ProfileTimezoneTargetIdSchema)
	.max(PROFILE_TIMEZONE_MAX_TARGETED_USERS);

const profileTimezoneConfigFields = {
	enabled: z.boolean(),
	config_version: z.number().int().min(0),
	rollout_basis_points: z.number().int().min(0).max(PROFILE_TIMEZONE_ROLLOUT_BASIS_POINTS_MAX),
	rollout_salt: z.string().trim().min(1).max(64).regex(PROFILE_TIMEZONE_SALT_PATTERN),
	included_user_ids: ProfileTimezoneTargetedUserIdsSchema,
	excluded_user_ids: ProfileTimezoneTargetedUserIdsSchema,
};

export const ProfileTimezoneConfigSchema = z.object({
	enabled: profileTimezoneConfigFields.enabled.default(false),
	config_version: profileTimezoneConfigFields.config_version.default(0),
	rollout_basis_points: profileTimezoneConfigFields.rollout_basis_points.default(0),
	rollout_salt: profileTimezoneConfigFields.rollout_salt.default(DEFAULT_PROFILE_TIMEZONE_SALT),
	included_user_ids: profileTimezoneConfigFields.included_user_ids.default([]),
	excluded_user_ids: profileTimezoneConfigFields.excluded_user_ids.default([]),
});

export type ProfileTimezoneConfig = z.infer<typeof ProfileTimezoneConfigSchema>;

export const DEFAULT_PROFILE_TIMEZONE_CONFIG: ProfileTimezoneConfig = ProfileTimezoneConfigSchema.parse({});

export const ProfileTimezoneConfigUpdateRequest = z
	.object(profileTimezoneConfigFields)
	.omit({config_version: true})
	.partial();

export type ProfileTimezoneConfigUpdateRequest = z.infer<typeof ProfileTimezoneConfigUpdateRequest>;

export const ProfileTimezoneConfigResponse = ProfileTimezoneConfigSchema;

export type ProfileTimezoneConfigResponse = z.infer<typeof ProfileTimezoneConfigResponse>;

export const ProfileTimezoneAssignmentResponse = z.object({
	enabled: profileTimezoneConfigFields.enabled,
});

export type ProfileTimezoneAssignmentResponse = z.infer<typeof ProfileTimezoneAssignmentResponse>;

export const INERT_PROFILE_TIMEZONE_ASSIGNMENT: ProfileTimezoneAssignmentResponse = {
	enabled: false,
};

export function resolveProfileTimezoneAssignment(
	config: ProfileTimezoneConfig,
	userId: string,
): ProfileTimezoneAssignmentResponse {
	if (!config.enabled) return {...INERT_PROFILE_TIMEZONE_ASSIGNMENT};
	if (config.excluded_user_ids.includes(userId)) return {...INERT_PROFILE_TIMEZONE_ASSIGNMENT};
	if (config.included_user_ids.includes(userId)) return {enabled: true};
	return {enabled: experimentBucket(userId, config.rollout_salt) < config.rollout_basis_points};
}
