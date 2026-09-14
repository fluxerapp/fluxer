// SPDX-License-Identifier: AGPL-3.0-or-later

import {createClientBehaviorExperiment} from '@fluxer/schema/src/domains/experiment/ClientBehaviorExperiment';
import type {z} from 'zod';

const expressionInfoCard = createClientBehaviorExperiment({defaultRolloutSalt: 'expression-info-card-v1'});

export const ExpressionInfoCardConfigSchema = expressionInfoCard.ConfigSchema;

export type ExpressionInfoCardConfig = z.infer<typeof ExpressionInfoCardConfigSchema>;

export const DEFAULT_EXPRESSION_INFO_CARD_CONFIG: ExpressionInfoCardConfig = expressionInfoCard.DEFAULT_CONFIG;

export const ExpressionInfoCardConfigUpdateRequest = expressionInfoCard.ConfigUpdateRequest;

export type ExpressionInfoCardConfigUpdateRequest = z.infer<typeof ExpressionInfoCardConfigUpdateRequest>;

export const ExpressionInfoCardConfigResponse = expressionInfoCard.ConfigSchema;

export type ExpressionInfoCardConfigResponse = ExpressionInfoCardConfig;

export const ExpressionInfoCardAssignmentResponse = expressionInfoCard.AssignmentResponse;

export type ExpressionInfoCardAssignmentResponse = z.infer<typeof ExpressionInfoCardAssignmentResponse>;

export const INERT_EXPRESSION_INFO_CARD_ASSIGNMENT: ExpressionInfoCardAssignmentResponse =
	expressionInfoCard.INERT_ASSIGNMENT;

export function resolveExpressionInfoCardAssignment(
	config: ExpressionInfoCardConfig,
	userId: string,
): ExpressionInfoCardAssignmentResponse {
	return expressionInfoCard.resolveAssignment(config, userId);
}
