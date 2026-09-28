// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	EXPERIMENT_BUCKET_RESOLUTION,
	type ExperimentTargeting,
	experimentAudienceIncludes,
	experimentBucket,
} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {z} from 'zod';

const ALTCHA_CAPTCHA_ROLLOUT_BASIS_POINTS_MAX = EXPERIMENT_BUCKET_RESOLUTION;
const ALTCHA_CAPTCHA_MAX_TARGETED_USERS = 1000;
const DEFAULT_ALTCHA_CAPTCHA_SALT = 'altcha-captcha-v1';

export const ALTCHA_CAPTCHA_MIN_COST = 1000;
export const ALTCHA_CAPTCHA_MAX_COST = 100000;
export const ALTCHA_CAPTCHA_MIN_MAX_COUNTER = 100;
export const ALTCHA_CAPTCHA_MAX_MAX_COUNTER = 1000000;

const ALTCHA_CAPTCHA_SALT_PATTERN = /^[\x20-\x7e]+$/u;

const AltchaCaptchaTargetIdSchema = z.string().regex(/^\d{1,20}$/u);
const AltchaCaptchaTargetedUserIdsSchema = z.array(AltchaCaptchaTargetIdSchema).max(ALTCHA_CAPTCHA_MAX_TARGETED_USERS);

const altchaCaptchaConfigFields = {
	enabled: z.boolean(),
	config_version: z.number().int().min(0),
	rollout_basis_points: z.number().int().min(0).max(ALTCHA_CAPTCHA_ROLLOUT_BASIS_POINTS_MAX),
	rollout_salt: z.string().trim().min(1).max(64).regex(ALTCHA_CAPTCHA_SALT_PATTERN),
	included_user_ids: AltchaCaptchaTargetedUserIdsSchema,
	included_guild_ids: AltchaCaptchaTargetedUserIdsSchema,
	include_premium_users: z.boolean(),
	excluded_user_ids: AltchaCaptchaTargetedUserIdsSchema,
	anonymous_enabled: z.boolean(),
	cost: z.number().int().min(ALTCHA_CAPTCHA_MIN_COST).max(ALTCHA_CAPTCHA_MAX_COST),
	max_counter: z.number().int().min(ALTCHA_CAPTCHA_MIN_MAX_COUNTER).max(ALTCHA_CAPTCHA_MAX_MAX_COUNTER),
};

export const AltchaCaptchaConfigSchema = z.object({
	enabled: altchaCaptchaConfigFields.enabled.default(false),
	config_version: altchaCaptchaConfigFields.config_version.default(0),
	rollout_basis_points: altchaCaptchaConfigFields.rollout_basis_points.default(0),
	rollout_salt: altchaCaptchaConfigFields.rollout_salt.default(DEFAULT_ALTCHA_CAPTCHA_SALT),
	included_user_ids: altchaCaptchaConfigFields.included_user_ids.default([]),
	included_guild_ids: altchaCaptchaConfigFields.included_guild_ids.default([]),
	include_premium_users: altchaCaptchaConfigFields.include_premium_users.default(false),
	excluded_user_ids: altchaCaptchaConfigFields.excluded_user_ids.default([]),
	anonymous_enabled: altchaCaptchaConfigFields.anonymous_enabled.default(false),
	cost: altchaCaptchaConfigFields.cost.default(5000),
	max_counter: altchaCaptchaConfigFields.max_counter.default(10000),
});

export type AltchaCaptchaConfig = z.infer<typeof AltchaCaptchaConfigSchema>;

export const DEFAULT_ALTCHA_CAPTCHA_CONFIG: AltchaCaptchaConfig = AltchaCaptchaConfigSchema.parse({});

export const AltchaCaptchaConfigUpdateRequest = z
	.object(altchaCaptchaConfigFields)
	.omit({config_version: true})
	.partial();

export type AltchaCaptchaConfigUpdateRequest = z.infer<typeof AltchaCaptchaConfigUpdateRequest>;

export const AltchaCaptchaConfigResponse = AltchaCaptchaConfigSchema;

export type AltchaCaptchaConfigResponse = z.infer<typeof AltchaCaptchaConfigResponse>;

export const AltchaCaptchaAssignmentResponse = z.object({
	enabled: altchaCaptchaConfigFields.enabled,
});

export type AltchaCaptchaAssignmentResponse = z.infer<typeof AltchaCaptchaAssignmentResponse>;

export const INERT_ALTCHA_CAPTCHA_ASSIGNMENT: AltchaCaptchaAssignmentResponse = {
	enabled: false,
};

export function resolveAltchaCaptchaAssignment(
	config: AltchaCaptchaConfig,
	userId: string,
	targeting: ExperimentTargeting,
): AltchaCaptchaAssignmentResponse {
	if (!config.enabled) return {...INERT_ALTCHA_CAPTCHA_ASSIGNMENT};
	if (config.excluded_user_ids.includes(userId)) return {...INERT_ALTCHA_CAPTCHA_ASSIGNMENT};
	if (config.included_user_ids.includes(userId)) return {enabled: true};
	if (experimentAudienceIncludes(config, targeting)) return {enabled: true};
	return {enabled: experimentBucket(userId, config.rollout_salt) < config.rollout_basis_points};
}

export function altchaCaptchaAppliesTo(
	config: AltchaCaptchaConfig,
	userId: string | null,
	targeting: ExperimentTargeting,
): boolean {
	if (userId === null) return config.enabled && config.anonymous_enabled;
	return resolveAltchaCaptchaAssignment(config, userId, targeting).enabled;
}
