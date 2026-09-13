// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT,
	VoiceNoiseSuppressionAssignmentResponse,
} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';
import {
	BlockedMessageGroupsAssignmentResponse,
	INERT_BLOCKED_MESSAGE_GROUPS_ASSIGNMENT,
} from '@fluxer/schema/src/domains/experiment/BlockedMessageGroupsSchemas';
import {
	GuildActivityLogPresentationAssignmentResponse,
	INERT_GUILD_ACTIVITY_LOG_PRESENTATION_ASSIGNMENT,
} from '@fluxer/schema/src/domains/experiment/GuildActivityLogPresentationSchemas';
import {
	INERT_MESSAGE_HOVER_TRACKING_ASSIGNMENT,
	MessageHoverTrackingAssignmentResponse,
} from '@fluxer/schema/src/domains/experiment/MessageHoverTrackingSchemas';
import {
	INERT_MESSAGE_KEYBOARD_FOCUS_ASSIGNMENT,
	MessageKeyboardFocusAssignmentResponse,
} from '@fluxer/schema/src/domains/experiment/MessageKeyboardFocusSchemas';
import {z} from 'zod';

export const EXPERIMENT_MIN_POLL_INTERVAL_SECONDS = 60;
export const EXPERIMENT_MAX_POLL_INTERVAL_SECONDS = 86400;
export const EXPERIMENT_MAX_POLL_JITTER_PERCENT = 50;
export const DEFAULT_EXPERIMENT_POLL_INTERVAL_SECONDS = 300;
export const DEFAULT_EXPERIMENT_POLL_JITTER_PERCENT = 15;

const experimentDeliveryFields = {
	poll_interval_seconds: z
		.number()
		.int()
		.min(EXPERIMENT_MIN_POLL_INTERVAL_SECONDS)
		.max(EXPERIMENT_MAX_POLL_INTERVAL_SECONDS),
	poll_jitter_percent: z.number().int().min(0).max(EXPERIMENT_MAX_POLL_JITTER_PERCENT),
};

export const ExperimentDeliveryConfigSchema = z.object({
	poll_interval_seconds: experimentDeliveryFields.poll_interval_seconds.default(
		DEFAULT_EXPERIMENT_POLL_INTERVAL_SECONDS,
	),
	poll_jitter_percent: experimentDeliveryFields.poll_jitter_percent.default(DEFAULT_EXPERIMENT_POLL_JITTER_PERCENT),
});

export type ExperimentDeliveryConfig = z.infer<typeof ExperimentDeliveryConfigSchema>;

export const DEFAULT_EXPERIMENT_DELIVERY_CONFIG: ExperimentDeliveryConfig = ExperimentDeliveryConfigSchema.parse({});

export const ExperimentDeliveryConfigUpdateRequest = z.object(experimentDeliveryFields).partial();

export type ExperimentDeliveryConfigUpdateRequest = z.infer<typeof ExperimentDeliveryConfigUpdateRequest>;

export const ExperimentDeliveryConfigResponse = ExperimentDeliveryConfigSchema;

export type ExperimentDeliveryConfigResponse = z.infer<typeof ExperimentDeliveryConfigResponse>;

const ExperimentAssignmentsSchema = z.object({
	voice_noise_suppression: VoiceNoiseSuppressionAssignmentResponse.optional(),
	message_hover_tracking: MessageHoverTrackingAssignmentResponse.optional(),
	message_keyboard_focus: MessageKeyboardFocusAssignmentResponse.optional(),
	blocked_message_groups: BlockedMessageGroupsAssignmentResponse.optional(),
	guild_activity_log_presentation: GuildActivityLogPresentationAssignmentResponse.optional(),
});

export const ExperimentAssignmentsResponse = z.object({
	poll_interval_seconds: z.number().int(),
	poll_jitter_percent: experimentDeliveryFields.poll_jitter_percent,
	assignments: ExperimentAssignmentsSchema,
});

export type ExperimentAssignmentsResponse = z.infer<typeof ExperimentAssignmentsResponse>;

export const INERT_EXPERIMENT_ASSIGNMENTS_RESPONSE: ExperimentAssignmentsResponse = {
	poll_interval_seconds: DEFAULT_EXPERIMENT_POLL_INTERVAL_SECONDS,
	poll_jitter_percent: DEFAULT_EXPERIMENT_POLL_JITTER_PERCENT,
	assignments: {},
};

export function readVoiceNoiseSuppressionAssignment(
	response: ExperimentAssignmentsResponse,
): VoiceNoiseSuppressionAssignmentResponse {
	return response.assignments.voice_noise_suppression ?? INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT;
}

export function readMessageHoverTrackingAssignment(
	response: ExperimentAssignmentsResponse,
): MessageHoverTrackingAssignmentResponse {
	return response.assignments.message_hover_tracking ?? INERT_MESSAGE_HOVER_TRACKING_ASSIGNMENT;
}

export function readMessageKeyboardFocusAssignment(
	response: ExperimentAssignmentsResponse,
): MessageKeyboardFocusAssignmentResponse {
	return response.assignments.message_keyboard_focus ?? INERT_MESSAGE_KEYBOARD_FOCUS_ASSIGNMENT;
}

export function readBlockedMessageGroupsAssignment(
	response: ExperimentAssignmentsResponse,
): BlockedMessageGroupsAssignmentResponse {
	return response.assignments.blocked_message_groups ?? INERT_BLOCKED_MESSAGE_GROUPS_ASSIGNMENT;
}

export function readGuildActivityLogPresentationAssignment(
	response: ExperimentAssignmentsResponse,
): GuildActivityLogPresentationAssignmentResponse {
	return response.assignments.guild_activity_log_presentation ?? INERT_GUILD_ACTIVITY_LOG_PRESENTATION_ASSIGNMENT;
}
