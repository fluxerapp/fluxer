// SPDX-License-Identifier: AGPL-3.0-or-later

import {EXPERIMENT_BUCKET_RESOLUTION, experimentBucket} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
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

export const ChannelThreadsConfigUpdateRequest = z
	.object(channelThreadsConfigFields)
	.omit({config_version: true, ever_enabled: true})
	.partial();

export type ChannelThreadsConfigUpdateRequest = z.infer<typeof ChannelThreadsConfigUpdateRequest>;

export const ChannelThreadsConfigResponse = ChannelThreadsConfigSchema;

export type ChannelThreadsConfigResponse = z.infer<typeof ChannelThreadsConfigResponse>;

export function applyChannelThreadsConfigUpdate(
	current: ChannelThreadsConfig,
	update: ChannelThreadsConfigUpdateRequest,
): ChannelThreadsConfig {
	const next = ChannelThreadsConfigSchema.parse({
		...current,
		...update,
		config_version: current.config_version + 1,
	});
	return {...next, ever_enabled: current.ever_enabled || next.enabled};
}

export interface CompiledChannelThreadsConfig {
	config: ChannelThreadsConfig;
	enabledGuildIds: ReadonlySet<string>;
	disabledGuildIds: ReadonlySet<string>;
	includedUserIds: ReadonlySet<string>;
	excludedUserIds: ReadonlySet<string>;
}

export function compileChannelThreadsConfig(config: ChannelThreadsConfig): CompiledChannelThreadsConfig {
	return {
		config,
		enabledGuildIds: new Set(config.enabled_guild_ids),
		disabledGuildIds: new Set(config.disabled_guild_ids),
		includedUserIds: new Set(config.included_user_ids),
		excludedUserIds: new Set(config.excluded_user_ids),
	};
}

export const DEFAULT_COMPILED_CHANNEL_THREADS_CONFIG: CompiledChannelThreadsConfig =
	compileChannelThreadsConfig(DEFAULT_CHANNEL_THREADS_CONFIG);

export function channelThreadsGuildActive(compiled: CompiledChannelThreadsConfig, guildId: string): boolean {
	const {config} = compiled;
	if (!config.enabled) return false;
	if (compiled.disabledGuildIds.has(guildId)) return false;
	if (compiled.enabledGuildIds.has(guildId)) return true;
	return config.guild_basis_points > 0 && experimentBucket(guildId, config.guild_salt) < config.guild_basis_points;
}

export function channelThreadsUserExcluded(compiled: CompiledChannelThreadsConfig, userId: string): boolean {
	return compiled.excludedUserIds.has(userId);
}

export function channelThreadsUserActive(compiled: CompiledChannelThreadsConfig, userId: string): boolean {
	const {config} = compiled;
	if (!config.enabled) return false;
	if (compiled.excludedUserIds.has(userId)) return false;
	if (compiled.includedUserIds.has(userId)) return true;
	return config.user_basis_points > 0 && experimentBucket(userId, config.user_salt) < config.user_basis_points;
}

function sameIds(a: ReadonlyArray<string>, b: ReadonlyArray<string>): boolean {
	const left = new Set(a);
	const right = new Set(b);
	return left.size === right.size && [...right].every((id) => left.has(id));
}

export function channelThreadsGuildFieldsChanged(a: ChannelThreadsConfig, b: ChannelThreadsConfig): boolean {
	return (
		a.enabled !== b.enabled ||
		a.guild_basis_points !== b.guild_basis_points ||
		a.guild_salt !== b.guild_salt ||
		!sameIds(a.enabled_guild_ids, b.enabled_guild_ids) ||
		!sameIds(a.disabled_guild_ids, b.disabled_guild_ids)
	);
}

export function channelThreadsUserFieldsChanged(a: ChannelThreadsConfig, b: ChannelThreadsConfig): boolean {
	return (
		a.enabled !== b.enabled ||
		a.user_basis_points !== b.user_basis_points ||
		a.user_salt !== b.user_salt ||
		!sameIds(a.included_user_ids, b.included_user_ids) ||
		!sameIds(a.excluded_user_ids, b.excluded_user_ids)
	);
}

export const ChannelThreadsAssignmentResponse = z.object({
	active: z.literal(true),
	config_version: z.number().int().min(0),
});

export type ChannelThreadsAssignmentResponse = z.infer<typeof ChannelThreadsAssignmentResponse>;

export function resolveChannelThreadsAssignment(
	compiled: CompiledChannelThreadsConfig,
	userId: string,
): ChannelThreadsAssignmentResponse | undefined {
	if (!channelThreadsUserActive(compiled, userId)) return undefined;
	return {active: true, config_version: compiled.config.config_version};
}
