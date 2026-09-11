// SPDX-License-Identifier: AGPL-3.0-or-later

import {z} from 'zod';
import {EXPERIMENT_BUCKET_RESOLUTION, experimentBucket} from '../experiment/ExperimentBucket';

export const VOICE_NOISE_SUPPRESSION_BACKENDS = [
	'none',
	'standard',
	'gate',
	'speex',
	'rnnoise',
	'gtcrn',
	'deep_filter',
] as const;

export type VoiceNoiseSuppressionBackend = (typeof VOICE_NOISE_SUPPRESSION_BACKENDS)[number];

const VoiceNoiseSuppressionBackendSchema = z.enum(VOICE_NOISE_SUPPRESSION_BACKENDS);

const VOICE_NOISE_SUPPRESSION_ROLLOUT_BASIS_POINTS_MAX = EXPERIMENT_BUCKET_RESOLUTION;
const VOICE_NOISE_SUPPRESSION_MAX_TARGETED_USERS = 1000;
const VOICE_NOISE_SUPPRESSION_MAX_GUILD_OVERRIDES = 200;
const DEFAULT_VOICE_NOISE_SUPPRESSION_SALT = 'voice-ns-v1';

const DEFAULT_VOICE_NOISE_SUPPRESSION_ENABLED_BACKENDS: ReadonlyArray<VoiceNoiseSuppressionBackend> = [
	'none',
	'standard',
	'gate',
	'speex',
	'rnnoise',
	'gtcrn',
	'deep_filter',
];

const SnowflakeListSchema = (max: number) =>
	z
		.array(z.string().regex(/^\d{1,20}$/u))
		.max(max)
		.default([]);

const VoiceNoiseSuppressionGuildOverrideSchema = z.object({
	guild_id: z.string().regex(/^\d{1,20}$/u),
	backend: VoiceNoiseSuppressionBackendSchema,
});

export const VoiceNoiseSuppressionConfigSchema = z.object({
	enabled: z.boolean().default(false),
	config_version: z.number().int().min(0).max(Number.MAX_SAFE_INTEGER).default(0),
	default_backend: VoiceNoiseSuppressionBackendSchema.default('standard'),
	enabled_backends: z
		.array(VoiceNoiseSuppressionBackendSchema)
		.max(VOICE_NOISE_SUPPRESSION_BACKENDS.length)
		.default([...DEFAULT_VOICE_NOISE_SUPPRESSION_ENABLED_BACKENDS]),
	allow_user_override: z.boolean().default(true),
	rollout_basis_points: z.number().int().min(0).max(VOICE_NOISE_SUPPRESSION_ROLLOUT_BASIS_POINTS_MAX).default(0),
	rollout_salt: z.string().trim().min(1).max(64).default(DEFAULT_VOICE_NOISE_SUPPRESSION_SALT),
	included_user_ids: SnowflakeListSchema(VOICE_NOISE_SUPPRESSION_MAX_TARGETED_USERS),
	excluded_user_ids: SnowflakeListSchema(VOICE_NOISE_SUPPRESSION_MAX_TARGETED_USERS),
	guild_overrides: z
		.array(VoiceNoiseSuppressionGuildOverrideSchema)
		.max(VOICE_NOISE_SUPPRESSION_MAX_GUILD_OVERRIDES)
		.default([]),
	stereo_enabled: z.boolean().default(false),
	suppression_strength: z.number().int().min(0).max(100).default(80),
});

export type VoiceNoiseSuppressionConfig = z.infer<typeof VoiceNoiseSuppressionConfigSchema>;

export const DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG: VoiceNoiseSuppressionConfig = {
	enabled: false,
	config_version: 0,
	default_backend: 'standard',
	enabled_backends: [...DEFAULT_VOICE_NOISE_SUPPRESSION_ENABLED_BACKENDS],
	allow_user_override: true,
	rollout_basis_points: 0,
	rollout_salt: DEFAULT_VOICE_NOISE_SUPPRESSION_SALT,
	included_user_ids: [],
	excluded_user_ids: [],
	guild_overrides: [],
	stereo_enabled: false,
	suppression_strength: 80,
};

export const VoiceNoiseSuppressionConfigUpdateRequest = z.object({
	enabled: z.boolean().optional(),
	default_backend: VoiceNoiseSuppressionBackendSchema.optional(),
	enabled_backends: z.array(VoiceNoiseSuppressionBackendSchema).max(VOICE_NOISE_SUPPRESSION_BACKENDS.length).optional(),
	allow_user_override: z.boolean().optional(),
	rollout_basis_points: z.number().int().min(0).max(VOICE_NOISE_SUPPRESSION_ROLLOUT_BASIS_POINTS_MAX).optional(),
	rollout_salt: z.string().trim().min(1).max(64).optional(),
	included_user_ids: z
		.array(z.string().regex(/^\d{1,20}$/u))
		.max(VOICE_NOISE_SUPPRESSION_MAX_TARGETED_USERS)
		.optional(),
	excluded_user_ids: z
		.array(z.string().regex(/^\d{1,20}$/u))
		.max(VOICE_NOISE_SUPPRESSION_MAX_TARGETED_USERS)
		.optional(),
	guild_overrides: z
		.array(VoiceNoiseSuppressionGuildOverrideSchema)
		.max(VOICE_NOISE_SUPPRESSION_MAX_GUILD_OVERRIDES)
		.optional(),
	stereo_enabled: z.boolean().optional(),
	suppression_strength: z.number().int().min(0).max(100).optional(),
});

export type VoiceNoiseSuppressionConfigUpdateRequest = z.infer<typeof VoiceNoiseSuppressionConfigUpdateRequest>;

export const VoiceNoiseSuppressionConfigResponse = VoiceNoiseSuppressionConfigSchema;

export type VoiceNoiseSuppressionConfigResponse = z.infer<typeof VoiceNoiseSuppressionConfigResponse>;

const VOICE_NOISE_SUPPRESSION_ASSIGNMENT_SOURCES = ['user_rule', 'canary'] as const;

const VoiceNoiseSuppressionAssignmentSourceSchema = z.enum(VOICE_NOISE_SUPPRESSION_ASSIGNMENT_SOURCES);

export const VoiceNoiseSuppressionAssignmentResponse = z.object({
	enabled: z.boolean(),
	config_version: z.number().int(),
	user_targeted: z.boolean(),
	backend: VoiceNoiseSuppressionBackendSchema.nullable(),
	source: VoiceNoiseSuppressionAssignmentSourceSchema.nullable(),
	guild_overrides: z.array(VoiceNoiseSuppressionGuildOverrideSchema),
	enabled_backends: z.array(VoiceNoiseSuppressionBackendSchema),
	allow_user_override: z.boolean(),
	stereo_enabled: z.boolean(),
	suppression_strength: z.number().int().min(0).max(100),
});

export type VoiceNoiseSuppressionAssignmentResponse = z.infer<typeof VoiceNoiseSuppressionAssignmentResponse>;

export const INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT: VoiceNoiseSuppressionAssignmentResponse = {
	enabled: false,
	config_version: 0,
	user_targeted: false,
	backend: null,
	source: null,
	guild_overrides: [],
	enabled_backends: [],
	allow_user_override: false,
	stereo_enabled: false,
	suppression_strength: DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG.suppression_strength,
};

export function resolveVoiceNoiseSuppressionAssignment(
	config: VoiceNoiseSuppressionConfig,
	userId: string,
): VoiceNoiseSuppressionAssignmentResponse {
	if (!config.enabled) {
		return {
			...INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT,
			config_version: config.config_version,
		};
	}
	const shared = {
		enabled: true,
		config_version: config.config_version,
		enabled_backends: [...config.enabled_backends],
		allow_user_override: config.allow_user_override,
		stereo_enabled: config.stereo_enabled,
		suppression_strength: config.suppression_strength,
	};
	if (config.excluded_user_ids.includes(userId)) {
		return {
			...shared,
			enabled_backends: [],
			allow_user_override: false,
			stereo_enabled: false,
			user_targeted: false,
			backend: null,
			source: null,
			guild_overrides: [],
		};
	}
	const backendIsUsable = config.enabled_backends.includes(config.default_backend);
	const guildOverrides = config.guild_overrides.filter((override) =>
		config.enabled_backends.includes(override.backend),
	);
	if (config.included_user_ids.includes(userId)) {
		return {
			...shared,
			user_targeted: backendIsUsable,
			backend: backendIsUsable ? config.default_backend : null,
			source: backendIsUsable ? 'user_rule' : null,
			guild_overrides: guildOverrides,
		};
	}
	const inCanary = experimentBucket(userId, config.rollout_salt) < config.rollout_basis_points;
	return {
		...shared,
		user_targeted: inCanary && backendIsUsable,
		backend: inCanary && backendIsUsable ? config.default_backend : null,
		source: inCanary && backendIsUsable ? 'canary' : null,
		guild_overrides: guildOverrides,
	};
}

export const VOICE_NOISE_SUPPRESSION_RESOLUTION_SOURCES = [
	'user_rule',
	'guild_rule',
	'canary',
	'user_override',
] as const;

export type VoiceNoiseSuppressionResolutionSource = (typeof VOICE_NOISE_SUPPRESSION_RESOLUTION_SOURCES)[number];

export interface VoiceNoiseSuppressionResolution {
	backend: VoiceNoiseSuppressionBackend;
	source: VoiceNoiseSuppressionResolutionSource;
	stereoEnabled: boolean;
	suppressionStrength: number;
	configVersion: number;
}

export function resolveVoiceNoiseSuppressionForCall(
	assignment: VoiceNoiseSuppressionAssignmentResponse,
	guildId: string | null,
	userPreference: VoiceNoiseSuppressionBackend | null,
): VoiceNoiseSuppressionResolution | null {
	if (!assignment.enabled) return null;
	const guildOverride =
		guildId == null ? undefined : assignment.guild_overrides.find((override) => override.guild_id === guildId);
	const targeted =
		assignment.source === 'user_rule' && assignment.backend != null
			? {backend: assignment.backend, source: 'user_rule' as const}
			: guildOverride != null
				? {backend: guildOverride.backend, source: 'guild_rule' as const}
				: assignment.user_targeted && assignment.backend != null && assignment.source != null
					? {backend: assignment.backend, source: assignment.source}
					: null;
	if (targeted == null) return null;
	const shared = {
		stereoEnabled: assignment.stereo_enabled,
		suppressionStrength: assignment.suppression_strength,
		configVersion: assignment.config_version,
	};
	if (
		assignment.allow_user_override &&
		userPreference != null &&
		assignment.enabled_backends.includes(userPreference)
	) {
		return {...shared, backend: userPreference, source: 'user_override'};
	}
	return {...shared, backend: targeted.backend, source: targeted.source};
}
