// SPDX-License-Identifier: AGPL-3.0-or-later

import {EXPERIMENT_BUCKET_RESOLUTION} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {z} from 'zod';

const CHANNEL_THREADS_BASIS_POINTS_MAX = EXPERIMENT_BUCKET_RESOLUTION;
const CHANNEL_THREADS_MAX_TARGETED_IDS = 1000;
const DEFAULT_CHANNEL_THREADS_GUILD_SALT = 'channel-threads-guild-v1';
const DEFAULT_CHANNEL_THREADS_USER_SALT = 'channel-threads-user-v1';

const CHANNEL_THREADS_SALT_PATTERN = /^[\x20-\x7e]+$/u;

const ChannelThreadsTargetIdSchema = z.string().regex(/^\d{1,20}$/u);
const ChannelThreadsTargetIdsSchema = z.array(ChannelThreadsTargetIdSchema).max(CHANNEL_THREADS_MAX_TARGETED_IDS);
const ChannelThreadsSaltSchema = z.string().trim().min(1).max(64).regex(CHANNEL_THREADS_SALT_PATTERN);
const ChannelThreadsBasisPointsSchema = z.number().int().min(0).max(CHANNEL_THREADS_BASIS_POINTS_MAX);

const channelThreadsConfigFields = {
	enabled: z.boolean(),
	config_version: z.number().int().min(0),
	ever_enabled: z.boolean(),
	guild_basis_points: ChannelThreadsBasisPointsSchema,
	guild_salt: ChannelThreadsSaltSchema,
	enabled_guild_ids: ChannelThreadsTargetIdsSchema,
	disabled_guild_ids: ChannelThreadsTargetIdsSchema,
	user_basis_points: ChannelThreadsBasisPointsSchema,
	user_salt: ChannelThreadsSaltSchema,
	included_user_ids: ChannelThreadsTargetIdsSchema,
	excluded_user_ids: ChannelThreadsTargetIdsSchema,
};

export const ChannelThreadsConfigSchema = z.object({
	enabled: channelThreadsConfigFields.enabled.default(false),
	config_version: channelThreadsConfigFields.config_version.default(0),
	ever_enabled: channelThreadsConfigFields.ever_enabled.default(false),
	guild_basis_points: channelThreadsConfigFields.guild_basis_points.default(0),
	guild_salt: channelThreadsConfigFields.guild_salt.default(DEFAULT_CHANNEL_THREADS_GUILD_SALT),
	enabled_guild_ids: channelThreadsConfigFields.enabled_guild_ids.default([]),
	disabled_guild_ids: channelThreadsConfigFields.disabled_guild_ids.default([]),
	user_basis_points: channelThreadsConfigFields.user_basis_points.default(0),
	user_salt: channelThreadsConfigFields.user_salt.default(DEFAULT_CHANNEL_THREADS_USER_SALT),
	included_user_ids: channelThreadsConfigFields.included_user_ids.default([]),
	excluded_user_ids: channelThreadsConfigFields.excluded_user_ids.default([]),
});

export type ChannelThreadsConfig = z.infer<typeof ChannelThreadsConfigSchema>;

export const DEFAULT_CHANNEL_THREADS_CONFIG: ChannelThreadsConfig = ChannelThreadsConfigSchema.parse({});

export const ChannelThreadsConfigResponse = ChannelThreadsConfigSchema;

export type ChannelThreadsConfigResponse = z.infer<typeof ChannelThreadsConfigResponse>;

export function everyoneChannelThreadsConfig(configVersion: number): ChannelThreadsConfig {
	return {
		...DEFAULT_CHANNEL_THREADS_CONFIG,
		enabled: true,
		config_version: configVersion,
		ever_enabled: true,
		guild_basis_points: CHANNEL_THREADS_BASIS_POINTS_MAX,
		user_basis_points: CHANNEL_THREADS_BASIS_POINTS_MAX,
		enabled_guild_ids: [],
		disabled_guild_ids: [],
		included_user_ids: [],
		excluded_user_ids: [],
	};
}

export const ChannelThreadsAssignmentResponse = z.object({
	active: z.literal(true),
	config_version: z.number().int().min(0),
});

export type ChannelThreadsAssignmentResponse = z.infer<typeof ChannelThreadsAssignmentResponse>;
