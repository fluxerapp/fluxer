// SPDX-License-Identifier: AGPL-3.0-or-later

import {z} from 'zod';
import {
	INERT_VOICE_NOISE_SUPPRESSION_ASSIGNMENT,
	VoiceNoiseSuppressionAssignmentResponse,
} from '../admin/VoiceNoiseSuppressionSchemas';

export const EXPERIMENT_MIN_POLL_INTERVAL_SECONDS = 60;
export const EXPERIMENT_MAX_POLL_INTERVAL_SECONDS = 86400;
export const EXPERIMENT_MAX_POLL_JITTER_PERCENT = 50;
export const DEFAULT_EXPERIMENT_POLL_INTERVAL_SECONDS = 300;
export const DEFAULT_EXPERIMENT_POLL_JITTER_PERCENT = 15;

export const ExperimentDeliveryConfigSchema = z.object({
	poll_interval_seconds: z
		.number()
		.int()
		.min(EXPERIMENT_MIN_POLL_INTERVAL_SECONDS)
		.max(EXPERIMENT_MAX_POLL_INTERVAL_SECONDS)
		.default(DEFAULT_EXPERIMENT_POLL_INTERVAL_SECONDS),
	poll_jitter_percent: z
		.number()
		.int()
		.min(0)
		.max(EXPERIMENT_MAX_POLL_JITTER_PERCENT)
		.default(DEFAULT_EXPERIMENT_POLL_JITTER_PERCENT),
});

export type ExperimentDeliveryConfig = z.infer<typeof ExperimentDeliveryConfigSchema>;

export const DEFAULT_EXPERIMENT_DELIVERY_CONFIG: ExperimentDeliveryConfig = {
	poll_interval_seconds: DEFAULT_EXPERIMENT_POLL_INTERVAL_SECONDS,
	poll_jitter_percent: DEFAULT_EXPERIMENT_POLL_JITTER_PERCENT,
};

export const ExperimentDeliveryConfigUpdateRequest = z.object({
	poll_interval_seconds: z
		.number()
		.int()
		.min(EXPERIMENT_MIN_POLL_INTERVAL_SECONDS)
		.max(EXPERIMENT_MAX_POLL_INTERVAL_SECONDS)
		.optional(),
	poll_jitter_percent: z.number().int().min(0).max(EXPERIMENT_MAX_POLL_JITTER_PERCENT).optional(),
});

export type ExperimentDeliveryConfigUpdateRequest = z.infer<typeof ExperimentDeliveryConfigUpdateRequest>;

export const ExperimentDeliveryConfigResponse = ExperimentDeliveryConfigSchema;

export type ExperimentDeliveryConfigResponse = z.infer<typeof ExperimentDeliveryConfigResponse>;

const ExperimentAssignmentsSchema = z.object({
	voice_noise_suppression: VoiceNoiseSuppressionAssignmentResponse.optional(),
});

export const ExperimentAssignmentsResponse = z.object({
	poll_interval_seconds: z.number().int(),
	poll_jitter_percent: z.number().int().min(0).max(EXPERIMENT_MAX_POLL_JITTER_PERCENT),
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
