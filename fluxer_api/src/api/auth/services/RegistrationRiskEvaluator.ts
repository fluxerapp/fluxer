// SPDX-License-Identifier: AGPL-3.0-or-later

import type {
	IRegistrationRiskEvaluator,
	RegistrationRiskParams,
	RegistrationRiskResult,
} from '@app/api/auth/services/IRegistrationRiskEvaluator';
import {Logger} from '@app/api/Logger';
import type {DeterministicRiskEngine} from '@app/api/risk/DeterministicRiskEngine';
import {RecommendedAction, RiskConfidence, RiskDecisionMethod, RiskLevel} from '@app/api/risk/RiskTypes';

export class RegistrationRiskEvaluator implements IRegistrationRiskEvaluator {
	constructor(private readonly riskEngine: Pick<DeterministicRiskEngine, 'classify'>) {}

	async evaluate(params: RegistrationRiskParams): Promise<RegistrationRiskResult> {
		const assessment = await this.riskEngine.classify({
			email: params.email,
			ip: params.clientIp,
			locale: params.locale,
			timezone: params.timezone,
			userAgent: params.userAgent,
			username: params.username,
			globalName: params.globalName,
			usernameIsUserChosen: params.usernameIsUserChosen,
			isUnclaimed: params.isUnclaimed,
		});
		if (assessment.suspicious) {
			Logger.info(
				{
					clientIp: params.clientIp,
					email: params.email,
					level: assessment.level,
					score: assessment.riskScore,
					method: assessment.method,
					elapsedMs: assessment.elapsedMs,
					recommendedAction: assessment.recommendedAction,
				},
				'Registration risk evaluation flagged as suspicious',
			);
		}
		return {
			assessment,
			level: assessment.level,
			recommendedAction: assessment.recommendedAction,
		};
	}
}

export const noopRegistrationRiskEvaluator: IRegistrationRiskEvaluator = {
	async evaluate(): Promise<RegistrationRiskResult> {
		return {
			assessment: {
				suspicious: false,
				level: RiskLevel.Low,
				confidence: RiskConfidence.Low,
				riskScore: 0,
				reasoning: 'risk engine not configured — defaulted to allow',
				recommendedAction: RecommendedAction.Allow,
				method: RiskDecisionMethod.Noop,
				modelUsed: 'none',
				rounds: 0,
				elapsedMs: 0,
				signals: {},
			},
			level: RiskLevel.Low,
			recommendedAction: RecommendedAction.Allow,
		};
	},
};
