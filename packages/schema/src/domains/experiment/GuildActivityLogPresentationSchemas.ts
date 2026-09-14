// SPDX-License-Identifier: AGPL-3.0-or-later

import {createClientBehaviorExperiment} from '@fluxer/schema/src/domains/experiment/ClientBehaviorExperiment';
import type {z} from 'zod';

const guildActivityLogPresentation = createClientBehaviorExperiment({
	defaultRolloutSalt: 'guild-activity-log-presentation-v1',
});

export const GuildActivityLogPresentationConfigSchema = guildActivityLogPresentation.ConfigSchema;

export type GuildActivityLogPresentationConfig = z.infer<typeof GuildActivityLogPresentationConfigSchema>;

export const DEFAULT_GUILD_ACTIVITY_LOG_PRESENTATION_CONFIG: GuildActivityLogPresentationConfig =
	guildActivityLogPresentation.DEFAULT_CONFIG;

export const GuildActivityLogPresentationConfigUpdateRequest = guildActivityLogPresentation.ConfigUpdateRequest;

export type GuildActivityLogPresentationConfigUpdateRequest = z.infer<
	typeof GuildActivityLogPresentationConfigUpdateRequest
>;

export const GuildActivityLogPresentationConfigResponse = guildActivityLogPresentation.ConfigSchema;

export type GuildActivityLogPresentationConfigResponse = GuildActivityLogPresentationConfig;

export const GuildActivityLogPresentationAssignmentResponse = guildActivityLogPresentation.AssignmentResponse;

export type GuildActivityLogPresentationAssignmentResponse = z.infer<
	typeof GuildActivityLogPresentationAssignmentResponse
>;

export const INERT_GUILD_ACTIVITY_LOG_PRESENTATION_ASSIGNMENT: GuildActivityLogPresentationAssignmentResponse =
	guildActivityLogPresentation.INERT_ASSIGNMENT;

export function resolveGuildActivityLogPresentationAssignment(
	config: GuildActivityLogPresentationConfig,
	userId: string,
): GuildActivityLogPresentationAssignmentResponse {
	return guildActivityLogPresentation.resolveAssignment(config, userId);
}
