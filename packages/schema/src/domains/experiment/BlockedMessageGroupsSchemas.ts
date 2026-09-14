// SPDX-License-Identifier: AGPL-3.0-or-later

import {createClientBehaviorExperiment} from '@fluxer/schema/src/domains/experiment/ClientBehaviorExperiment';
import type {z} from 'zod';

const blockedMessageGroups = createClientBehaviorExperiment({defaultRolloutSalt: 'blocked-message-groups-v1'});

export const BlockedMessageGroupsConfigSchema = blockedMessageGroups.ConfigSchema;

export type BlockedMessageGroupsConfig = z.infer<typeof BlockedMessageGroupsConfigSchema>;

export const DEFAULT_BLOCKED_MESSAGE_GROUPS_CONFIG: BlockedMessageGroupsConfig = blockedMessageGroups.DEFAULT_CONFIG;

export const BlockedMessageGroupsConfigUpdateRequest = blockedMessageGroups.ConfigUpdateRequest;

export type BlockedMessageGroupsConfigUpdateRequest = z.infer<typeof BlockedMessageGroupsConfigUpdateRequest>;

export const BlockedMessageGroupsConfigResponse = blockedMessageGroups.ConfigSchema;

export type BlockedMessageGroupsConfigResponse = BlockedMessageGroupsConfig;

export const BlockedMessageGroupsAssignmentResponse = blockedMessageGroups.AssignmentResponse;

export type BlockedMessageGroupsAssignmentResponse = z.infer<typeof BlockedMessageGroupsAssignmentResponse>;

export const INERT_BLOCKED_MESSAGE_GROUPS_ASSIGNMENT: BlockedMessageGroupsAssignmentResponse =
	blockedMessageGroups.INERT_ASSIGNMENT;

export function resolveBlockedMessageGroupsAssignment(
	config: BlockedMessageGroupsConfig,
	userId: string,
): BlockedMessageGroupsAssignmentResponse {
	return blockedMessageGroups.resolveAssignment(config, userId);
}
