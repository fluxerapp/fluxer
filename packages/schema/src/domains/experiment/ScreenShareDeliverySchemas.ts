// SPDX-License-Identifier: AGPL-3.0-or-later

import {createClientBehaviorExperiment} from '@fluxer/schema/src/domains/experiment/ClientBehaviorExperiment';
import type {z} from 'zod';

const screenShareDelivery = createClientBehaviorExperiment({defaultRolloutSalt: 'screen-share-delivery-v1'});

export const ScreenShareDeliveryConfigSchema = screenShareDelivery.ConfigSchema;

export type ScreenShareDeliveryConfig = z.infer<typeof ScreenShareDeliveryConfigSchema>;

export const ScreenShareDeliveryConfigUpdateRequest = screenShareDelivery.ConfigUpdateRequest;

export type ScreenShareDeliveryConfigUpdateRequest = z.infer<typeof ScreenShareDeliveryConfigUpdateRequest>;

export const ScreenShareDeliveryConfigResponse = screenShareDelivery.ConfigSchema;

export const ScreenShareDeliveryAssignmentResponse = screenShareDelivery.AssignmentResponse;

export type ScreenShareDeliveryAssignmentResponse = z.infer<typeof ScreenShareDeliveryAssignmentResponse>;

export const INERT_SCREEN_SHARE_DELIVERY_ASSIGNMENT: ScreenShareDeliveryAssignmentResponse =
	screenShareDelivery.INERT_ASSIGNMENT;

export function resolveScreenShareDeliveryAssignment(
	config: ScreenShareDeliveryConfig,
	userId: string,
): ScreenShareDeliveryAssignmentResponse {
	return screenShareDelivery.resolveAssignment(config, userId);
}
