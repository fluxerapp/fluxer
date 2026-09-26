// SPDX-License-Identifier: AGPL-3.0-or-later

import {EXPERIMENT_BUCKET_RESOLUTION, experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {z} from 'zod';

const PUSH_SERVICE_DELIVERY_ROLLOUT_BASIS_POINTS_MAX = EXPERIMENT_BUCKET_RESOLUTION;
const PUSH_SERVICE_DELIVERY_MAX_TARGETED_USERS = 1000;
const DEFAULT_PUSH_SERVICE_DELIVERY_SALT = 'push-service-delivery-v1';

const PUSH_SERVICE_DELIVERY_SALT_PATTERN = /^[\x20-\x7e]+$/u;

const PushServiceDeliveryTargetIdSchema = z.string().regex(/^\d{1,20}$/u);
const PushServiceDeliveryTargetedUserIdsSchema = z
	.array(PushServiceDeliveryTargetIdSchema)
	.max(PUSH_SERVICE_DELIVERY_MAX_TARGETED_USERS);

const pushServiceDeliveryConfigFields = {
	enabled: z.boolean(),
	config_version: z.number().int().min(0),
	rollout_basis_points: z.number().int().min(0).max(PUSH_SERVICE_DELIVERY_ROLLOUT_BASIS_POINTS_MAX),
	rollout_salt: z.string().trim().min(1).max(64).regex(PUSH_SERVICE_DELIVERY_SALT_PATTERN),
	included_user_ids: PushServiceDeliveryTargetedUserIdsSchema,
	excluded_user_ids: PushServiceDeliveryTargetedUserIdsSchema,
};

export const PushServiceDeliveryConfigSchema = z.object({
	enabled: pushServiceDeliveryConfigFields.enabled.default(false),
	config_version: pushServiceDeliveryConfigFields.config_version.default(0),
	rollout_basis_points: pushServiceDeliveryConfigFields.rollout_basis_points.default(0),
	rollout_salt: pushServiceDeliveryConfigFields.rollout_salt.default(DEFAULT_PUSH_SERVICE_DELIVERY_SALT),
	included_user_ids: pushServiceDeliveryConfigFields.included_user_ids.default([]),
	excluded_user_ids: pushServiceDeliveryConfigFields.excluded_user_ids.default([]),
});

export type PushServiceDeliveryConfig = z.infer<typeof PushServiceDeliveryConfigSchema>;

export const DEFAULT_PUSH_SERVICE_DELIVERY_CONFIG: PushServiceDeliveryConfig = PushServiceDeliveryConfigSchema.parse(
	{},
);

export const PushServiceDeliveryConfigUpdateRequest = z
	.object(pushServiceDeliveryConfigFields)
	.omit({config_version: true})
	.partial();

export type PushServiceDeliveryConfigUpdateRequest = z.infer<typeof PushServiceDeliveryConfigUpdateRequest>;

export const PushServiceDeliveryConfigResponse = PushServiceDeliveryConfigSchema;

export type PushServiceDeliveryConfigResponse = z.infer<typeof PushServiceDeliveryConfigResponse>;

export function pushServiceDeliveryEnrols(config: PushServiceDeliveryConfig, userId: string): boolean {
	if (!config.enabled) return false;
	if (config.excluded_user_ids.includes(userId)) return false;
	if (config.included_user_ids.includes(userId)) return true;
	return experimentBucket(userId, config.rollout_salt) < config.rollout_basis_points;
}
