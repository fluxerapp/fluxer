// SPDX-License-Identifier: AGPL-3.0-or-later

import {createClientBehaviorExperiment} from '@fluxer/schema/src/domains/experiment/ClientBehaviorExperiment';
import type {z} from 'zod';

const messageHoverTracking = createClientBehaviorExperiment({defaultRolloutSalt: 'message-hover-tracking-v1'});

export const MessageHoverTrackingConfigSchema = messageHoverTracking.ConfigSchema;

export type MessageHoverTrackingConfig = z.infer<typeof MessageHoverTrackingConfigSchema>;

export const DEFAULT_MESSAGE_HOVER_TRACKING_CONFIG: MessageHoverTrackingConfig = messageHoverTracking.DEFAULT_CONFIG;

export const MessageHoverTrackingConfigUpdateRequest = messageHoverTracking.ConfigUpdateRequest;

export type MessageHoverTrackingConfigUpdateRequest = z.infer<typeof MessageHoverTrackingConfigUpdateRequest>;

export const MessageHoverTrackingConfigResponse = messageHoverTracking.ConfigSchema;

export type MessageHoverTrackingConfigResponse = MessageHoverTrackingConfig;

export const MessageHoverTrackingAssignmentResponse = messageHoverTracking.AssignmentResponse;

export type MessageHoverTrackingAssignmentResponse = z.infer<typeof MessageHoverTrackingAssignmentResponse>;

export const INERT_MESSAGE_HOVER_TRACKING_ASSIGNMENT: MessageHoverTrackingAssignmentResponse =
	messageHoverTracking.INERT_ASSIGNMENT;

export function resolveMessageHoverTrackingAssignment(
	config: MessageHoverTrackingConfig,
	userId: string,
): MessageHoverTrackingAssignmentResponse {
	return messageHoverTracking.resolveAssignment(config, userId);
}
