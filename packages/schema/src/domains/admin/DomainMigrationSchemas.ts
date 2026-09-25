// SPDX-License-Identifier: AGPL-3.0-or-later

import {EXPERIMENT_BUCKET_RESOLUTION, experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {z} from 'zod';

const DOMAIN_MIGRATION_ROLLOUT_BASIS_POINTS_MAX = EXPERIMENT_BUCKET_RESOLUTION;
const DOMAIN_MIGRATION_MAX_TARGETED_USERS = 1000;
const DEFAULT_DOMAIN_MIGRATION_SALT = 'domain-migration-v1';

const DOMAIN_MIGRATION_SALT_PATTERN = /^[\x20-\x7e]+$/u;

const DomainMigrationTargetIdSchema = z.string().regex(/^\d{1,20}$/u);
const DomainMigrationTargetedUserIdsSchema = z
	.array(DomainMigrationTargetIdSchema)
	.max(DOMAIN_MIGRATION_MAX_TARGETED_USERS);

const domainMigrationConfigFields = {
	enabled: z.boolean(),
	config_version: z.number().int().min(0),
	rollout_basis_points: z.number().int().min(0).max(DOMAIN_MIGRATION_ROLLOUT_BASIS_POINTS_MAX),
	rollout_salt: z.string().trim().min(1).max(64).regex(DOMAIN_MIGRATION_SALT_PATTERN),
	included_user_ids: DomainMigrationTargetedUserIdsSchema,
	excluded_user_ids: DomainMigrationTargetedUserIdsSchema,
	anonymous_rollout_basis_points: z.number().int().min(0).max(DOMAIN_MIGRATION_ROLLOUT_BASIS_POINTS_MAX),
	standalone_forwarding: z.boolean(),
};

export const DomainMigrationConfigSchema = z.object({
	enabled: domainMigrationConfigFields.enabled.default(false),
	config_version: domainMigrationConfigFields.config_version.default(0),
	rollout_basis_points: domainMigrationConfigFields.rollout_basis_points.default(0),
	rollout_salt: domainMigrationConfigFields.rollout_salt.default(DEFAULT_DOMAIN_MIGRATION_SALT),
	included_user_ids: domainMigrationConfigFields.included_user_ids.default([]),
	excluded_user_ids: domainMigrationConfigFields.excluded_user_ids.default([]),
	anonymous_rollout_basis_points: domainMigrationConfigFields.anonymous_rollout_basis_points.default(0),
	standalone_forwarding: domainMigrationConfigFields.standalone_forwarding.default(false),
});

export type DomainMigrationConfig = z.infer<typeof DomainMigrationConfigSchema>;

export const DEFAULT_DOMAIN_MIGRATION_CONFIG: DomainMigrationConfig = DomainMigrationConfigSchema.parse({});

export const DomainMigrationConfigUpdateRequest = z
	.object(domainMigrationConfigFields)
	.omit({config_version: true})
	.partial();

export type DomainMigrationConfigUpdateRequest = z.infer<typeof DomainMigrationConfigUpdateRequest>;

export const DomainMigrationConfigResponse = DomainMigrationConfigSchema;

export type DomainMigrationConfigResponse = z.infer<typeof DomainMigrationConfigResponse>;

export const DomainMigrationAssignmentResponse = z.object({
	enabled: domainMigrationConfigFields.enabled,
});

export type DomainMigrationAssignmentResponse = z.infer<typeof DomainMigrationAssignmentResponse>;

export const INERT_DOMAIN_MIGRATION_ASSIGNMENT: DomainMigrationAssignmentResponse = {
	enabled: false,
};

export function resolveDomainMigrationAssignment(
	config: DomainMigrationConfig,
	userId: string,
): DomainMigrationAssignmentResponse {
	if (!config.enabled) return {...INERT_DOMAIN_MIGRATION_ASSIGNMENT};
	if (config.excluded_user_ids.includes(userId)) return {...INERT_DOMAIN_MIGRATION_ASSIGNMENT};
	if (config.included_user_ids.includes(userId)) return {enabled: true};
	return {enabled: experimentBucket(userId, config.rollout_salt) < config.rollout_basis_points};
}

export const DomainMigrationDiscoveryResponse = z.object({
	enabled: domainMigrationConfigFields.enabled.describe('Whether the domain migration is switched on'),
	anonymous_rollout_basis_points: domainMigrationConfigFields.anonymous_rollout_basis_points.describe(
		'Share of logged-out devices, in basis points, that move to the new domain',
	),
	rollout_salt: domainMigrationConfigFields.rollout_salt.describe('Salt used to bucket devices and users'),
	standalone_forwarding: domainMigrationConfigFields.standalone_forwarding.describe(
		'Whether installed desktop web apps forward to the new domain after moving their session',
	),
});

export type DomainMigrationDiscoveryResponse = z.infer<typeof DomainMigrationDiscoveryResponse>;

export function toDomainMigrationDiscovery(config: DomainMigrationConfig): DomainMigrationDiscoveryResponse {
	return {
		enabled: config.enabled,
		anonymous_rollout_basis_points: config.anonymous_rollout_basis_points,
		rollout_salt: config.rollout_salt,
		standalone_forwarding: config.standalone_forwarding,
	};
}
