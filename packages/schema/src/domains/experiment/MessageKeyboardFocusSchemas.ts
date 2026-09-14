// SPDX-License-Identifier: AGPL-3.0-or-later

import {createClientBehaviorExperiment} from '@fluxer/schema/src/domains/experiment/ClientBehaviorExperiment';
import type {z} from 'zod';

const messageKeyboardFocus = createClientBehaviorExperiment({
	defaultRolloutSalt: 'message-keyboard-focus-v1',
});

export const MessageKeyboardFocusConfigSchema = messageKeyboardFocus.ConfigSchema;

export type MessageKeyboardFocusConfig = z.infer<typeof MessageKeyboardFocusConfigSchema>;

export const DEFAULT_MESSAGE_KEYBOARD_FOCUS_CONFIG: MessageKeyboardFocusConfig = messageKeyboardFocus.DEFAULT_CONFIG;

export const MessageKeyboardFocusConfigUpdateRequest = messageKeyboardFocus.ConfigUpdateRequest;

export type MessageKeyboardFocusConfigUpdateRequest = z.infer<typeof MessageKeyboardFocusConfigUpdateRequest>;

export const MessageKeyboardFocusConfigResponse = messageKeyboardFocus.ConfigSchema;

export type MessageKeyboardFocusConfigResponse = MessageKeyboardFocusConfig;

export const MessageKeyboardFocusAssignmentResponse = messageKeyboardFocus.AssignmentResponse;

export type MessageKeyboardFocusAssignmentResponse = z.infer<typeof MessageKeyboardFocusAssignmentResponse>;

export const INERT_MESSAGE_KEYBOARD_FOCUS_ASSIGNMENT: MessageKeyboardFocusAssignmentResponse =
	messageKeyboardFocus.INERT_ASSIGNMENT;

export function resolveMessageKeyboardFocusAssignment(
	config: MessageKeyboardFocusConfig,
	userId: string,
): MessageKeyboardFocusAssignmentResponse {
	return messageKeyboardFocus.resolveAssignment(config, userId);
}
