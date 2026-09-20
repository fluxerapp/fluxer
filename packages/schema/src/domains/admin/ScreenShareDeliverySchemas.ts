// SPDX-License-Identifier: AGPL-3.0-or-later

import {EXPERIMENT_BUCKET_RESOLUTION, experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {z} from 'zod';

const SCREEN_SHARE_DELIVERY_ROLLOUT_BASIS_POINTS_MAX = EXPERIMENT_BUCKET_RESOLUTION;
const SCREEN_SHARE_DELIVERY_MAX_TARGETED_USERS = 1000;
const DEFAULT_SCREEN_SHARE_DELIVERY_SALT = 'screen-share-delivery-v1';

const ScreenShareDeliveryTargetIdSchema = z.string().regex(/^\d{1,20}$/u);
const ScreenShareDeliveryTargetedUserIdsSchema = z
	.array(ScreenShareDeliveryTargetIdSchema)
	.max(SCREEN_SHARE_DELIVERY_MAX_TARGETED_USERS);

const screenShareDeliveryConfigFields = {
	enabled: z.boolean(),
	config_version: z.number().int().min(0),
	rollout_basis_points: z.number().int().min(0).max(SCREEN_SHARE_DELIVERY_ROLLOUT_BASIS_POINTS_MAX),
	rollout_salt: z.string().trim().min(1).max(64),
	included_user_ids: ScreenShareDeliveryTargetedUserIdsSchema,
	excluded_user_ids: ScreenShareDeliveryTargetedUserIdsSchema,
};

export const ScreenShareDeliveryConfigSchema = z.object({
	enabled: screenShareDeliveryConfigFields.enabled.default(false),
	config_version: screenShareDeliveryConfigFields.config_version.default(0),
	rollout_basis_points: screenShareDeliveryConfigFields.rollout_basis_points.default(0),
	rollout_salt: screenShareDeliveryConfigFields.rollout_salt.default(DEFAULT_SCREEN_SHARE_DELIVERY_SALT),
	included_user_ids: screenShareDeliveryConfigFields.included_user_ids.default([]),
	excluded_user_ids: screenShareDeliveryConfigFields.excluded_user_ids.default([]),
});

export type ScreenShareDeliveryConfig = z.infer<typeof ScreenShareDeliveryConfigSchema>;

export const DEFAULT_SCREEN_SHARE_DELIVERY_CONFIG: ScreenShareDeliveryConfig = ScreenShareDeliveryConfigSchema.parse(
	{},
);

export const ScreenShareDeliveryConfigUpdateRequest = z
	.object(screenShareDeliveryConfigFields)
	.omit({config_version: true})
	.partial();

export type ScreenShareDeliveryConfigUpdateRequest = z.infer<typeof ScreenShareDeliveryConfigUpdateRequest>;

export const ScreenShareDeliveryConfigResponse = ScreenShareDeliveryConfigSchema;

export type ScreenShareDeliveryConfigResponse = z.infer<typeof ScreenShareDeliveryConfigResponse>;

export const ScreenShareDeliveryAssignmentResponse = z.object({
	enabled: screenShareDeliveryConfigFields.enabled,
});

export type ScreenShareDeliveryAssignmentResponse = z.infer<typeof ScreenShareDeliveryAssignmentResponse>;

export const INERT_SCREEN_SHARE_DELIVERY_ASSIGNMENT: ScreenShareDeliveryAssignmentResponse = {
	enabled: false,
};

export function resolveScreenShareDeliveryAssignment(
	config: ScreenShareDeliveryConfig,
	userId: string,
): ScreenShareDeliveryAssignmentResponse {
	if (!config.enabled) return {...INERT_SCREEN_SHARE_DELIVERY_ASSIGNMENT};
	if (config.excluded_user_ids.includes(userId)) return {...INERT_SCREEN_SHARE_DELIVERY_ASSIGNMENT};
	if (config.included_user_ids.includes(userId)) return {enabled: true};
	return {enabled: experimentBucket(userId, config.rollout_salt) < config.rollout_basis_points};
}
