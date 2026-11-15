// SPDX-License-Identifier: AGPL-3.0-or-later

import {VOICE_P2P_MAX_PARTICIPANTS} from '@fluxer/constants/src/LimitConstants';
import {
	EXPERIMENT_BUCKET_RESOLUTION,
	ExperimentRolloutCountryCodesSchema,
	type ExperimentTargeting,
	experimentAudienceIncludes,
	experimentBucket,
	experimentRolloutCountryIncludes,
} from '@fluxer/schema/src/domains/experiment/ExperimentBucket';
import {z} from 'zod';

const VOICE_P2P_ROLLOUT_BASIS_POINTS_MAX = EXPERIMENT_BUCKET_RESOLUTION;
const VOICE_P2P_MAX_TARGETED_USERS = 1000;
const DEFAULT_VOICE_P2P_SALT = 'voice-p2p-v1';
const VOICE_P2P_MIN_PARTICIPANTS = 2;

const VOICE_P2P_SALT_PATTERN = /^[\x20-\x7e]+$/u;

const VoiceP2pTargetIdSchema = z.string().regex(/^\d{1,20}$/u);
const VoiceP2pTargetedUserIdsSchema = z.array(VoiceP2pTargetIdSchema).max(VOICE_P2P_MAX_TARGETED_USERS);

const voiceP2pConfigFields = {
	enabled: z.boolean(),
	config_version: z.number().int().min(0),
	rollout_basis_points: z.number().int().min(0).max(VOICE_P2P_ROLLOUT_BASIS_POINTS_MAX),
	rollout_country_codes: ExperimentRolloutCountryCodesSchema,
	rollout_salt: z.string().trim().min(1).max(64).regex(VOICE_P2P_SALT_PATTERN),
	included_user_ids: VoiceP2pTargetedUserIdsSchema,
	included_guild_ids: VoiceP2pTargetedUserIdsSchema,
	include_premium_users: z.boolean(),
	excluded_user_ids: VoiceP2pTargetedUserIdsSchema,
	max_participants: z.number().int().min(VOICE_P2P_MIN_PARTICIPANTS).max(VOICE_P2P_MAX_PARTICIPANTS),
};

export const VoiceP2pConfigSchema = z.object({
	enabled: voiceP2pConfigFields.enabled.default(false),
	config_version: voiceP2pConfigFields.config_version.default(0),
	rollout_basis_points: voiceP2pConfigFields.rollout_basis_points.default(0),
	rollout_country_codes: voiceP2pConfigFields.rollout_country_codes.default([]),
	rollout_salt: voiceP2pConfigFields.rollout_salt.default(DEFAULT_VOICE_P2P_SALT),
	included_user_ids: voiceP2pConfigFields.included_user_ids.default([]),
	included_guild_ids: voiceP2pConfigFields.included_guild_ids.default([]),
	include_premium_users: voiceP2pConfigFields.include_premium_users.default(false),
	excluded_user_ids: voiceP2pConfigFields.excluded_user_ids.default([]),
	max_participants: voiceP2pConfigFields.max_participants.default(VOICE_P2P_MIN_PARTICIPANTS),
});

export type VoiceP2pConfig = z.infer<typeof VoiceP2pConfigSchema>;

export const DEFAULT_VOICE_P2P_CONFIG: VoiceP2pConfig = VoiceP2pConfigSchema.parse({});

export const VoiceP2pConfigUpdateRequest = z.object(voiceP2pConfigFields).omit({config_version: true}).partial();

export type VoiceP2pConfigUpdateRequest = z.infer<typeof VoiceP2pConfigUpdateRequest>;

export const VoiceP2pConfigResponse = VoiceP2pConfigSchema;

export type VoiceP2pConfigResponse = z.infer<typeof VoiceP2pConfigResponse>;

export const VoiceP2pAssignmentResponse = z.object({
	enabled: voiceP2pConfigFields.enabled,
	max_participants: voiceP2pConfigFields.max_participants,
});

export type VoiceP2pAssignmentResponse = z.infer<typeof VoiceP2pAssignmentResponse>;

export const INERT_VOICE_P2P_ASSIGNMENT: VoiceP2pAssignmentResponse = {
	enabled: false,
	max_participants: DEFAULT_VOICE_P2P_CONFIG.max_participants,
};

export function resolveVoiceP2pAssignment(
	config: VoiceP2pConfig,
	userId: string,
	targeting: ExperimentTargeting,
): VoiceP2pAssignmentResponse {
	return {enabled: voiceP2pSelects(config, userId, targeting), max_participants: config.max_participants};
}

function voiceP2pSelects(config: VoiceP2pConfig, userId: string, targeting: ExperimentTargeting): boolean {
	if (!config.enabled) return false;
	if (config.excluded_user_ids.includes(userId)) return false;
	if (config.included_user_ids.includes(userId)) return true;
	if (experimentAudienceIncludes(config, targeting)) return true;
	if (!experimentRolloutCountryIncludes(config, targeting)) return false;
	return experimentBucket(userId, config.rollout_salt) < config.rollout_basis_points;
}
