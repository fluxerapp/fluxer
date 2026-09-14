// SPDX-License-Identifier: AGPL-3.0-or-later

import {createClientBehaviorExperiment} from '@fluxer/schema/src/domains/experiment/ClientBehaviorExperiment';
import type {z} from 'zod';

const typingIndicatorRework = createClientBehaviorExperiment({defaultRolloutSalt: 'typing-indicator-rework-v1'});

export const TypingIndicatorReworkConfigSchema = typingIndicatorRework.ConfigSchema;

export type TypingIndicatorReworkConfig = z.infer<typeof TypingIndicatorReworkConfigSchema>;

export const DEFAULT_TYPING_INDICATOR_REWORK_CONFIG: TypingIndicatorReworkConfig = typingIndicatorRework.DEFAULT_CONFIG;

export const TypingIndicatorReworkConfigUpdateRequest = typingIndicatorRework.ConfigUpdateRequest;

export type TypingIndicatorReworkConfigUpdateRequest = z.infer<typeof TypingIndicatorReworkConfigUpdateRequest>;

export const TypingIndicatorReworkConfigResponse = typingIndicatorRework.ConfigSchema;

export type TypingIndicatorReworkConfigResponse = TypingIndicatorReworkConfig;

export const TypingIndicatorReworkAssignmentResponse = typingIndicatorRework.AssignmentResponse;

export type TypingIndicatorReworkAssignmentResponse = z.infer<typeof TypingIndicatorReworkAssignmentResponse>;

export const INERT_TYPING_INDICATOR_REWORK_ASSIGNMENT: TypingIndicatorReworkAssignmentResponse =
	typingIndicatorRework.INERT_ASSIGNMENT;

export function resolveTypingIndicatorReworkAssignment(
	config: TypingIndicatorReworkConfig,
	userId: string,
): TypingIndicatorReworkAssignmentResponse {
	return typingIndicatorRework.resolveAssignment(config, userId);
}

export function isTypingIndicatorReworkTargeted(assignment: TypingIndicatorReworkAssignmentResponse): boolean {
	return assignment.enabled && assignment.user_targeted;
}
