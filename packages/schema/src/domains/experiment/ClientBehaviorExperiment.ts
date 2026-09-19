// SPDX-License-Identifier: AGPL-3.0-or-later

import {EXPERIMENT_BUCKET_RESOLUTION, experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {z} from 'zod';

const CLIENT_BEHAVIOR_MAX_TARGETED_USERS = 1000;
const CLIENT_BEHAVIOR_ASSIGNMENT_SOURCES = ['user_rule', 'canary'] as const;

export function createClientBehaviorExperiment({defaultRolloutSalt}: {defaultRolloutSalt: string}) {
	const targetIdSchema = z.string().regex(/^\d{1,20}$/u);
	const targetedUserIdsSchema = z.array(targetIdSchema).max(CLIENT_BEHAVIOR_MAX_TARGETED_USERS);
	const configFields = {
		enabled: z.boolean(),
		config_version: z.number().int().min(0),
		rollout_basis_points: z.number().int().min(0).max(EXPERIMENT_BUCKET_RESOLUTION),
		rollout_salt: z.string().trim().min(1).max(64),
		included_user_ids: targetedUserIdsSchema,
		excluded_user_ids: targetedUserIdsSchema,
	};
	const ConfigSchema = z.object({
		enabled: configFields.enabled.default(false),
		config_version: configFields.config_version.default(0),
		rollout_basis_points: configFields.rollout_basis_points.default(0),
		rollout_salt: configFields.rollout_salt.default(defaultRolloutSalt),
		included_user_ids: configFields.included_user_ids.default([]),
		excluded_user_ids: configFields.excluded_user_ids.default([]),
	});
	const ConfigUpdateRequest = z.object(configFields).omit({config_version: true}).partial();
	const AssignmentResponse = z.object({
		enabled: configFields.enabled,
		config_version: z.number().int(),
		user_targeted: z.boolean(),
		source: z.enum(CLIENT_BEHAVIOR_ASSIGNMENT_SOURCES).nullable(),
	});
	type Config = z.infer<typeof ConfigSchema>;
	type Assignment = z.infer<typeof AssignmentResponse>;
	const INERT_ASSIGNMENT: Assignment = {
		enabled: false,
		config_version: 0,
		user_targeted: false,
		source: null,
	};
	const resolveAssignment = (config: Config, userId: string): Assignment => {
		if (!config.enabled) {
			return {...INERT_ASSIGNMENT, config_version: config.config_version};
		}
		const shared = {enabled: true, config_version: config.config_version};
		if (config.excluded_user_ids.includes(userId)) {
			return {...shared, user_targeted: false, source: null};
		}
		if (config.included_user_ids.includes(userId)) {
			return {...shared, user_targeted: true, source: 'user_rule'};
		}
		const inCanary = experimentBucket(userId, config.rollout_salt) < config.rollout_basis_points;
		return {...shared, user_targeted: inCanary, source: inCanary ? 'canary' : null};
	};
	return {ConfigSchema, ConfigUpdateRequest, AssignmentResponse, INERT_ASSIGNMENT, resolveAssignment};
}
