// SPDX-License-Identifier: AGPL-3.0-or-later

import {createClientBehaviorExperiment} from '@fluxer/schema/src/domains/experiment/ClientBehaviorExperiment';
import type {z} from 'zod';

const guildHeaderCollapse = createClientBehaviorExperiment({defaultRolloutSalt: 'guild-header-collapse-v1'});

export const GuildHeaderCollapseConfigSchema = guildHeaderCollapse.ConfigSchema;

export type GuildHeaderCollapseConfig = z.infer<typeof GuildHeaderCollapseConfigSchema>;

export const DEFAULT_GUILD_HEADER_COLLAPSE_CONFIG: GuildHeaderCollapseConfig = guildHeaderCollapse.DEFAULT_CONFIG;

export const GuildHeaderCollapseConfigUpdateRequest = guildHeaderCollapse.ConfigUpdateRequest;

export type GuildHeaderCollapseConfigUpdateRequest = z.infer<typeof GuildHeaderCollapseConfigUpdateRequest>;

export const GuildHeaderCollapseConfigResponse = guildHeaderCollapse.ConfigSchema;

export type GuildHeaderCollapseConfigResponse = GuildHeaderCollapseConfig;

export const GuildHeaderCollapseAssignmentResponse = guildHeaderCollapse.AssignmentResponse;

export type GuildHeaderCollapseAssignmentResponse = z.infer<typeof GuildHeaderCollapseAssignmentResponse>;

export const INERT_GUILD_HEADER_COLLAPSE_ASSIGNMENT: GuildHeaderCollapseAssignmentResponse =
	guildHeaderCollapse.INERT_ASSIGNMENT;

export function resolveGuildHeaderCollapseAssignment(
	config: GuildHeaderCollapseConfig,
	userId: string,
): GuildHeaderCollapseAssignmentResponse {
	return guildHeaderCollapse.resolveAssignment(config, userId);
}

export function isGuildHeaderCollapseTargeted(assignment: GuildHeaderCollapseAssignmentResponse): boolean {
	return assignment.enabled && assignment.user_targeted;
}
